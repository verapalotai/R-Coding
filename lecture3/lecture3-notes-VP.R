# Work with given dataset

install.packages("nycflights13")
library(nycflights13)
flights
?flights

# see the whole dataset, opens a new tab, looks like an excel spreadsheet
View(flights)

library(tidyverse)

#we will cover: filter(), arrange(), select()
# tibble = data frame

filter(flights, month == 1) # using = would produce an error

filter(flights, month == 1, day == 1)

filter(flights, dep_time == 517)       

flights # python chages the original dataset, r throws filtered datasets away unless u save it

jan1 <- filter(flights, month == 1, day == 1)
jan1

# to assign and print: put () around the statement and it gets printed

(feb1 <- filter(flights, month == 2, day == 1))

# comparison

1/49*49 == 1 #false
1/(7^9)*7^9 == 1 #true

# near, use it for float comparisons

near(sqrt(2)^2, 2) # exercise: how near do you have to be? can u change that?

# multiple constraints

(jan_feb <- filter(flights, month == 1 | month == 2)) # | is the or operator

# all months except jan

(not_jan <- filter(flights, !(month == 1)))

# check that it worked

filter(not_jan, month == 1)
View(jan_feb)
unique(not_jan$month) # gives the values in month column, it indeed does not contain 1
jan <- filter(flights, month == 1)
nrow(flights) == nrow(jan) + nrow(not_jan)

(jan_to_june <- filter(flights, month <= 6))
(jan_to_june_2 <- filter(flights, month %in% c(1,2,3,4,5,6)))

#check
nrow(jan_to_june) == nrow(jan_to_june_2)

(mystery_filter <- filter(flights, !(arr_delay > 120 | dep_delay > 120)))
# nr 4. is the correct answer

(mystery_filter2 <- filter(flights, (arr_delay <= 120 | dep_delay <= 120)))
# arr_delay > 120 OR dep_delay > 120

# true if:
# - (120, 120)
# - (0, 120)
# - (120, 0)
# false if:
# - (0, 0)

#flights that departed less than 120 mins late or arrived less than 120 mins late
(practice_filter <- filter(flights, (dep_delay < 120) | (arr_delay < 120)))

# filter for flights that lost time in the air

dep_ok_arr_not <- filter(flights, dep_delay <= 120, arr_delay > 120)

ggplot(data = dep_ok_arr_not, 
       mapping = aes(x = dep_delay)) + 
  geom_histogram()

# the whole population of flights, how late they started -> how preticted if you start late you arrive late

ggplot(data = flights, 
       mapping = aes(x = dep_delay)) + 
  geom_histogram()

# all the flights that landed ok

dep_ok <- filter(flights, dep_delay <= 120)
ggplot(data = dep_ok, 
       mapping = aes(x = dep_delay)) + 
  geom_histogram()

# NA: not available

NA > 5
10 == NA
NA == NA
FALSE & NA # false and sth is false
TRUE & NA # output depends on what NA is

# Mary's age
x <- NA # this is also an observation

#John's age
y <- NA

# Are they the same?
x == y # it's NA bc we do not know

NA^0 # will give 1
0 * NA # NA -> ?? wtf is going on

is.na(x) # spits out TRUE if a variable is NA
df <- tibble(x = c(1, NA, 3))
df
filter(df, x > 1)
filter(df, x > 1 | is.na(x))
arrange(df,x)
arrange(df, desc(x))

# all in all NA is weird

# arrange()

arrange(flights, year, month, day)
arrange(flights, dep_delay)
arrange(flights, desc(dep_delay))

# HW: Class exercise: How can we get the missing values at the top?

# fastest flight
arrange(flights, air_time)

# select()

select(flights, year, month, day)
select(flights, air_time)
select(arrange(flights, air_time), air_time, origin, dest)
# arrange(flights, air_time) returns a data frame

# let's use pipes: %>%

flights %>%
  arrange(air_time) %>%
  select(air_time, origin, dest)

# some helpful functions

select(flights, year:day)
flights %>% select(year:day) # same as above

colnames(flights)

# dropping columns

select(flights, -(year:day))

# start_with, end_with, contains

select(flights, starts_with("arr"))
select(flights, -starts_with("arr"))
select(flights, ends_with("hour"))
select(flights, -contains("time"))

# for more do
?select

#renaming columns
rename(flights, destination = dest)

# if it is difficult to see
flights %>% rename(destination = dest) %>% select(year:day, destination)

# moving columns to the start
select(flights, origin, dest, everything()) # takes origin and dest to the beginning, before everything

# try to get ch03_hotels-vienna-explore.R
# change the beginning to wherever your R repository is
