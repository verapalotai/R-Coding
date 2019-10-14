***************************************************
* ch03 fottball managers change
* using the football data
*
* creates work data by merging games and manager data
* 
* v0.9 2019-09-12
* (has two versions of joinby, needs cutting)
**********************************************************


cap cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
global data_in "cases_studies_public\football\raw"
global data_out "cases_studies_public\football\clean"


* IN: data_in/ 
	* fdata_pl_t.csv, t=2009-2018
	* managers_epl.xlsx
 
* OUT: data_out/
	* epl_games.dta
	* epl_teams_games.dta
	* football_managers.dta
	* football_managers_workfile

***********************************************************
* TIDY DATA: game (home, away pair) & gameweek
* load in many csv and append them

clear 
import delimited "$data_in/fdata_pl_2008.csv", varnames(1) 
save "$data_out/epl_games.dta", replace
forval j=2009/2018 {
	clear
	import delimited "$data_in/fdata_pl_`j'.csv", varnames(1) 
	append using "$data_out/epl_games.dta"
	save "$data_out/epl_games.dta", replace
}

* keep variables we'll use
keep div date hometeam awayteam  fthg ftag

* Missing data (empty rows)
lis if date==""
drop if date==""

* rename variables
* naming: variable_j where j is home or away

rename fthg goals_home
rename ftag goals_away
rename hometeam team_home
rename awayteam team_away

* date, season
* date format, year, month
rename date datestring
gen date = date(datestring, "DMY", 2019)
format date %td
gen y = year(date)
gen m = month(date)
* season
gen season = y-1 if m<=6
 replace season=y if m>=8
* check if date variables are fine
sort season date
by season date: gen temp=_n
*lis season y m date datestring if (m==9 | m==1) & temp==1
drop datestring y m

* create points variables
* naming: variable_j where j is home or away

gen points_home=3 if goals_home > goals_away
 replace points_home=1 if goals_home == goals_away
 replace points_home=0 if goals_home <goals_away
gen points_away=3 if goals_home < goals_away
replace points_away=1 if goals_home == goals_away
replace points_away=0 if goals_home  > goals_away

lab var team_home ""
lab var team_away ""
lab var goals_home ""
lab var goals_away ""
cap drop temp*
compress
order div season date team* points* goals*
save "$data_out/epl_games.dta", replace


***********************************************************
* TIDY DATA: team & game
* from tidy game data

clear
use "$data_out/epl_games.dta", replace
 gen home=0
 foreach x in team points goals {
	rename `x'_away `x'
	rename `x'_home `x'_opponent
 }
 save temp,replace
use "$data_out/epl_games.dta", replace
 gen home=1
 foreach x in team points goals {
	rename `x'_home `x'
	rename `x'_away `x'_opponent
 }
append using temp

* gameno: game number team played in season
sort team season date
by team season: gen gameno = _n

compress
order div season date team gameno home points goals
save "$data_out/epl_teams_games.dta", replace



***********************************************************
* TIDY DATA: managers

clear
import excel "$data_in\managers_epl.xlsx", sheet("epl") firstrow case(lower)

* identity resolution

* double-dagger sign means caretaker manager
gen caretaker = substr(name,-13,.)=="double-dagger"
* remove signs from name
replace name = subinstr(name,"double-dagger", "",.)
replace name = subinstr(name,"dagger", "",.)
replace name = subinstr(name,"§", "",.)

* removes space at beginning and end 
replace name = ustrtrim(name) 

* create date variables
* remove unnecessary characters representign footnotes in Wikipedia
forvalue i=1/14 {
	replace from = subinstr(from, "[N `i']", "",.)
	replace until = subinstr(until, "[N `i']", "",.)
}

gen flag_inoffice= (until=="Present*" | until=="" | until==".")
replace until="1 July 2019" if until=="Present*"

gen date_from = date(from, "DMY", 2019)
gen date_until = date(until, "DMY", 2019)
format date_from %td
format date_until %td
* check if date conversion is fine
codebook date_from date_until
tab until if date_until==.,mis
order name club date_from from date_until until
drop from until

* removes duplicates
sort name club date_from date_until
drop if name==name[_n-1] & club==club[_n-1] & date_from==date_from[_n-1] & date_until==date_until[_n-1]

* numerical id for managers
sort club date_from
encode name, gen(manager_id) 
rename name manager_name

* drop unnecessary variables, save data
drop yearsin duration
compress
order manager_id manager_name club caretaker date* nat
save "$data_out\football_managers.dta", replace



***********************************************************
* create WORKFILE: 
* merge team & game with managers

* prepare: team names should match
use "$data_out\football_managers.dta", replace
gen str14 team=club
replace team = ustrword(team,1)
lis team club
replace team = "Aston Villa" if team=="Aston"
replace team = "Crystal Palace" if team=="Crystal"
replace team = "Man United" if club=="Manchester United"
replace team = "Man City" if club=="Manchester City"
replace team = "West Brom" if club=="West Bromwich Albion"
replace team = "West Ham" if club=="West Ham United"
replace team = "QPR" if club=="Queens Park Rangers"
replace team = "Wolves" if club=="Wolverhampton Wanderers"


* drop managers whose tenure ended before 2008
drop if year(date_until)<2008

* create "wide" file: one observation one team
keep team manager_id manager_name date* caretaker
sort team date_from
save "$data_out\football_managers_temp.dta", replace

* merger
* this approach is a bit cumbersome but works well

by team: gen manager_no=_n
reshape wide manager_id manager_name date_from date_until caretaker, i(team) j(manager_no)

qui compress
save temp,replace

* merge all managers to club
use "$data_out/epl_teams_games.dta", replace
merge m:m team using temp, keep(1 3) nogen

* find manager in charge at game time
gen str20 manager_name =""
foreach x in manager_id manager_name caretaker date_from date_until {
	cap gen `x'=.
	forvalue j=1/13 {
		replace `x'=`x'`j' if date>=date_from`j' & date<=date_until`j'
	}
}
format date_from date_until %td
order div-goals_opponent manager_id manager_name caretaker date_from date_until
drop manager_id1-date_until13
compress

* missing data: replace by hand
lis if manager_id==.
sum manager_id
replace manager_id = 262 if team=="Reading" & gameno==30
replace manager_name = "Eamonn Dolan" if team=="Reading" & gameno==30
replace caretaker = 1 if team=="Reading" & gameno==30
replace date_from = date("11March2013","DMY",2019) if team=="Reading" & gameno==30
replace date_until = date("26March2013","DMY",2019) if team=="Reading" & gameno==30

save "$data_out\football_managers_workfile",replace

erase "$data_out\football_managers_temp.dta"

end

*************************************************
*************************************************
*************************************************
* simpler way or joining
clear
use "$data_out/epl_teams_games.dta" 
joinby team using "$data_out\football_managers_temp.dta", unmatched(both)


* missing data: replace by hand
lis if manager_id==.
sum manager_id
replace manager_id = 999 if team=="Reading" & gameno==30
replace manager_name = "Eamonn Dolan" if team=="Reading" & gameno==30
replace caretaker = 1 if team=="Reading" & gameno==30
replace date_from = date("11March2013","DMY",2019) if team=="Reading" & gameno==30
replace date_until = date("26March2013","DMY",2019) if team=="Reading" & gameno==30

keep if date<= date_until & date>=date_from


* treat that managers overlap sometimes when caretakers are used
bys season team gameno: gen z=_N
drop if z>1 & caretaker==0

* there is some minor problem, still-.-.


format date_from date_until %td
order div-goals_opponent manager_id manager_name caretaker date_from date_until
compress

save "$data_out\football_managers_workfile_v2",replace
erase "$data_out\football_managers_temp.dta"
