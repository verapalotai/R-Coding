######################################################################
# Chapter 04
#
# wms-management-survey
# v1.3

# using WMS data 2004-2015
#
######################################################################


######################################################################

# Clear memory
rm(list=ls())

# Import libraries
require(tidyverse)
require(plyr)
library(gridExtra)
library(cowplot)
library(viridis)
library(haven)
library(Hmisc)
library(binsreg)
library(xtable)

# Set the path
dir <-  "..."

# Location folders
data_in <- paste0(dir,"da_data_repo/wms-management-survey/clean/")
data_out <- paste0(dir,"da_case_studies/ch04-management-firm-size/")
output <- paste0(dir,"da_case_studies/ch04-management-firm-size/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))

########################################################################

# Import data
df <- read_csv(paste0(data_in,"wms_data-textbook_2004_2015.csv"))

# Sample selection
df <- df %>%
  filter(country=="Mexico" & wave==2013 & emp_firm>=100  & emp_firm<=5000)

# Summary
summary(df$emp_firm)
describe(df$emp_firm)

# Save workfile
write.csv(data, paste0(data_out, "ch04-wms-work.csv"), row.names = F)

########################################################################

# Summary
df %>%
  select(management, emp_firm) %>% 
  summarise_all(funs(min, max, mean, median, sd, n()))

# Histogram
ggplot(data = df, aes (x = management, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.25, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(x = "Quality of management, average score", y = "Percent") +
  #scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_x_continuous(limits = c(1,5))+
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bg() +
  background_grid(major = "xy", minor = "y")
ggsave(paste0(output, "wms_Mex_management_hist_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)

ggplot(data = df, aes (x = emp_firm, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 200, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(x = "Firm size (# employees)", y = "Percent") +
  scale_x_continuous(breaks = seq(0, 5000, by = 1000)) +
  scale_y_continuous(limits=c(0, 0.5), breaks = seq(0, 1, by = 0.1), labels = scales::percent_format()) +
  theme_bg() +
  background_grid(major = "xy", minor = "y")
ggsave(paste0(output, "wms_Mex_emp_hist_R.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

# Generate variable
df$lnemp = log(df$emp_firm)

# Histogram
ggplot(data = df, aes (x = lnemp, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.3, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(x = "Firm size (# employees), log values", y = "Percent") +
  scale_x_continuous(limits = c(4,9)) +
  scale_y_continuous(limits=c(0, 0.2), breaks = seq(0, 0.2, by = 0.05), labels = scales::percent_format()) +
  theme_bg() +
  background_grid(major = "xy", minor = "y")
ggsave(paste0(output, "wms_Mex_lnemp_hist_R.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

########################################################################


# Stack bar charts some management items by emp bins

# Generate employment bins
df$emp3bins <- ifelse(df$emp_firm<200, 1, 
                      ifelse(df$emp_firm>=200 & df$emp_firm<1000, 2,
                             ifelse(df$emp_firm>=1000, 3,100)
                          )
                      )
describe(df$emp3bins)

# Create pivot
df$emp3bins <- as.factor(df$emp3bins)

df1 <- df %>% 
  select(emp3bins,lean1) %>% 
  group_by (emp3bins,lean1) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(Percent= round(Count / sum(Count),digits = 5)) %>% ungroup()

# Stacked bar
ggplot(data=df1, aes(x=emp3bins, y=Percent, fill = factor(lean1, levels = rev(unique(lean1))))) +
  geom_bar(stat = "identity", position = "fill",width = 0.6,  color = "white",  size = 0.5, alpha = 0.8) +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::percent_format()) +
  scale_x_discrete(labels=c("1" = "Small", "2" = "Medium", "3" = "Large")) +
  scale_fill_manual(values = viridis(5, begin=0, end=0.8), name = NULL) +
  labs(x = "Firm size bins (by # employees)", y = "Percent") +
  theme_bg() +
  background_grid(major = "y", minor = "y") +
  theme(legend.position = "bottom")
ggsave(paste0(output, "wms_Mex_lean1_emp3bins_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)

# Create pivot
df1 <- df %>% 
  select(emp3bins,perf2) %>% 
  group_by (emp3bins,perf2) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(Percent= round(Count / sum(Count),digits = 5)) %>% ungroup()
                # use %>% ungroup() when do multiple times group_by


ggplot(data=df1, aes(x=emp3bins, y=Percent, fill = factor(perf2, levels = rev(unique(perf2))))) +
  geom_bar(stat = "identity", position = "fill",width = 0.6,  color = "white",  size = 0.5, alpha = 0.8) +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::percent_format()) +
  scale_x_discrete(labels=c("1" = "Small", "2" = "Medium", "3" = "Large")) +
  scale_fill_manual(values = viridis(5, begin=0, end=0.8), name = NULL) +
  labs(x = "Firm size bins (# employees)", y = "Percent") +
  theme_bg() +
  background_grid(major = "y", minor = "y") +
  theme(legend.position = "bottom")
ggsave(paste0(output, "wms_Mex_perf2_emp3bins_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)


##############################################################################

# Bin scatters avg score by employment bins

# Option 1: create 3 bins as defined by thresholds

# Summary
df %>%
  select(emp_firm, emp3bins) %>% 
  group_by(emp3bins) %>% 
  dplyr::summarise_all(funs(min, max, mean, median, sd, n()))

# Recode employee bins
df$emp3bins <- ifelse(df$emp3bins == 1 , 150, 
                      ifelse(df$emp3bins == 2, 600,
                             ifelse(df$emp3bins == 3, 3000, NA)))
# Summary
df %>%
  select(emp_firm, emp3bins) %>% 
  group_by(emp3bins) %>%
  summarise_all(funs(min, max, mean, median, sd, n()))

# Generate variables by mean
df1<-df %>% group_by(emp3bins) %>%
  dplyr::summarize(management_emp3bins=mean(management))


# Bin scatters
ggplot(data = df1, aes(x = emp3bins, y = management_emp3bins)) +
  geom_point(size = 2, color = "black", fill= color[1], shape = 21, alpha = 0.8, na.rm=T) +
  #geom_text(aes(label = round(management_emp3bins, 1)), hjust = 0.5, vjust = -1, color = "black", size = 3) +
  scale_y_continuous(limits = c(2.4, 3.4), breaks = seq(2.4, 3.4, by=0.2)) +
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0,3000, by=500)) +
  labs(x = "Firm size (# employees), 3 bins", y = "Average management quality score") +
  theme_bg() +
  background_grid(major = "xy", minor = "xy", size.major = 0.2) 
ggsave(paste0(output, "wms_Mex_management_emp3bins_R.png"), width=mywidth_large, height=myheight_large,  units = "cm", dpi = 1200)


# Option 2: create 10 bins as defined by equal cutoffs

df$emp10bins <- df$emp_firm %>% cut_number(10)

    # another way would be to make sure we exactly 30 units/bin
    # df <- df %>%   dplyr::arrange(emp_firm) %>%   dplyr::mutate(id = row_number()) 
    # df$emp10bins <- as.factor(cut(df$id, 10))
    # levels(df$emp10bins) <- c('0','1', '2', '3', '4', '5', '6', '7', '8', '9')

# Summary
df_summary<-df %>%
  select(emp_firm, emp10bins) %>% 
  group_by(emp10bins) %>%
  summarise_all(funs(min, max, mean, median, sd, n()))
df_summary

# Recode
levels(df$emp10bins) <-  df_summary %>% pull(mean) %>% round()
df$emp10bins<-as.numeric(levels(df$emp10bins))[df$emp10bins]

# Summary
df %>%
  select(emp_firm, emp10bins) %>% 
  group_by(emp10bins) %>%
  dplyr::summarise_all(funs(min, max, mean, median, sd, n()))

# Generate variables by mean
df1 <- df %>% group_by(emp10bins) %>% 
              dplyr::summarize(management_emp10bins=mean(management))

# Bin scatters
ggplot(data = df1, aes(x = emp10bins, y = management_emp10bins)) +
  geom_point(size = 2, color = "black", fill= color[1], shape = 21, alpha = 0.8, na.rm=T) +
  #geom_text(aes(label = round(management_emp10bins, 1)), hjust = 0.5, vjust = -1, color = "black", size = 3) +
  scale_y_continuous(limits = c(2.5, 3.5), breaks = seq(2.5, 3.5, by=0.25)) +
  scale_x_continuous(limits = c(0, 3500), breaks = seq(0,3500, by=500)) +
  labs(x = "Firm size (# employees), 10 bins", y = "Average management quality score") +
  theme_bg() +
  background_grid(major = "xy", minor = "xy", size.major = 0.2) 
ggsave(paste0(output, "wms_Mex_management_emp10bins_R.png"), width=mywidth_large, height=myheight_large,  units = "cm", dpi = 1200)

# This is a simpler solution, similar looking graph:
binsreg(df$management, df$emp_firm, nbins = 10)



##############################################################################

# Scatterplot avg score by employment

ggplot(data = df, aes(x = emp_firm, y = management)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  scale_x_continuous(limits=c(0, 5000), breaks=seq(0, 5000, by=1000)) + 
  scale_y_continuous(limits = c(1, 5), breaks = seq(1, 5,1)) +
  labs(x = "Firm size (# employees)",y = "Quality of management: average score")+
  theme_bg() +
  background_grid(major = "xy", minor="none")
ggsave(paste0(output, "wms_Mex_management_emp_scatter_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)


df$lnemp = log(df$emp_firm)

ggplot(data = df, aes(x = lnemp, y = management)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  scale_x_continuous(limits=c(4, 9), breaks=seq(4, 9, by=1)) + 
  scale_y_continuous(limits = c(1, 5), breaks = seq(1, 5,1)) +
  labs(x = "Firm size (# employees), log values",y = "Quality of management: average score")+
  theme_bg() +
  background_grid(major = "xy", minor="none")
ggsave(paste0(output, "wms_Mex_management_lnemp_scatter_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)


# Box plots by emp bins
df$emp3bins <- as.factor(df$emp3bins)
levels(df$emp3bins) <- c('Small','Medium', 'Large')

# Boxplot
ggplot(data = df, aes(x = emp3bins, y = management)) +
  stat_boxplot(aes(group = emp3bins), geom = "errorbar", width = 0.5, color = viridis(3, begin=0, end=0.7), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = emp3bins),  color = viridis(3, begin=0, end=0.7), fill = viridis(3, begin = 0, end=0.7), size = 0.5, width = 0.5, alpha = 0.3, na.rm=T) +
#  geom_jitter(aes(color = emp3bins), position=position_jitter(0.1), size = 0.5, show.legend=F,  na.rm=TRUE) +
  labs(x = "Firm size (# employees), 3 bins",y = "Average management quality score")+
  scale_y_continuous(limits = c(1,5), breaks = seq(1,5,1)) +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0, end=0.7)+
  theme_bg() +
  background_grid(major = "xy", minor="none")
ggsave(paste0(output, "wms_Mex_boxplot_management_emp3bins_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)

# Violin plot
ggplot(data = df, aes(x = emp3bins, y = management, color=emp3bins, fill=emp3bins)) +
  geom_violin(aes(group = emp3bins),   size=0.3,  alpha=0.3, trim = F, show.legend=F, na.rm =TRUE) +
  geom_boxplot(aes(group = emp3bins),  color = c(color[2], color[1], color[3]), fill  = c(color[2], color[1], color[3]), size = 0.5, width = 0.2, alpha = 0.3, na.rm=T) +
#  geom_jitter(aes(color = emp3bins), position=position_jitter(0.1), size = 0.5, show.legend=F,  na.rm=TRUE, alpha = 0.8) +  labs(x = "Number of Employees, 3 bins",y = "Average management quality score")+
  labs(x = "Firm size (# employees), 3 bins",y = "Average management quality score")+
  scale_y_continuous(limits = c(0,6), breaks = seq(0,6,1)) +
  scale_color_manual(name="", 
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="", 
                    values=c(color[2],color[1], color[3])) +
  theme_bg() +
  background_grid(major = "xy", minor="none")
ggsave(paste0(output, "wms_Mex_violin_management_emp3bins_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)





##############################################################################
# Correlation
cor(df$management, df$emp_firm, use = "complete.obs")

# by industry
df$industry_broad[df$sic<220] <- 'food_drinks_tobacco'
df$industry_broad[df$sic>=220 & df$sic<240 | df$sic>=310 & df$sic<320 | df$sic>=390 & df$sic<400] <- 'textile_apparel_leather_etc'
df$industry_broad[df$sic>=240& df$sic<269] <- 'wood_furniture_paper'
df$industry_broad[df$sic>=280 & df$sic<310] <- 'chemicals_etc'
df$industry_broad[df$sic>=320 & df$sic<350] <- 'materials_metals'
df$industry_broad[df$sic>=357 & df$sic<370] <- 'electronics'
df$industry_broad[df$sic>=370 & df$sic<380] <- 'auto'

# Correlation
df %>%
  group_by(industry_broad) %>%
  dplyr::summarize(COR=cor(management, emp_firm))

# Summary
df %>%
  select(management, industry_broad) %>% 
  filter(!is.na(industry_broad)) %>% 
  group_by(industry_broad) %>%
  dplyr::summarise(Min = min(management), 
                   Max= max(management),
                   SD = sd(management),
                   Median = median(management),
                   n())

df %>%
  select(emp_firm, industry_broad) %>% 
  filter(!is.na(industry_broad)) %>% 
  group_by(industry_broad) %>%
  dplyr::summarise(Min = min(emp_firm), 
                   Max= max(emp_firm),
                   SD = sd(emp_firm),
                   Median = median(emp_firm),
                   n())

# todo create Table 4.1. using this
# N/A -->other

cor<-df %>%
  group_by(industry_broad) %>%
  dplyr::summarize(COR=cor(management, emp_firm))



table41 <-df %>%
  select(emp_firm, industry_broad,management) %>% 
  # filter(!is.na(industry_broad)) %>% 
  group_by(industry_broad) %>%
  dplyr::summarise(Mean = mean(management),Obs=n())

table41$cor<-cor$COR


table41<-table41 %>% replace_na(list(industry_broad = "other"))
table41$industry_broad<-table41$industry_broad %>% dplyr::recode(auto='Auto',
                                        chemicals_etc='Chemicals',
                                        electronics='Electronics',
                                        food_drinks_tobacco='Food, drinks, tobacco',
                                        materials_metals='Materials, metals',
                                        textile_apparel_leather_etc='Textile, apparel',
                                        wood_furniture_paper='Wood, furniture, paper',
                                        other = 'Other'
                                        )
last_row<-table41 %>% summarise(Mean=mean(Mean),Obs=sum(Obs),cor=mean(cor))
last_row$industry_broad<-'All'

table41<-table41 %>% add_row(industry_broad=last_row$industry_broad,
                   Mean=last_row$Mean,
                   cor=last_row$cor,
                   Obs=last_row$Obs
                   )


table41<-table41 %>% select(industry_broad,cor,Mean,Obs)
table41
xt<-xtable(table41,align='llccc', digits = c(0,0,2,1,0))
names(xt) <- c('Industry','Management - employment correlation','Avg. management score','Observations' )
print(xt, type = "latex",include.rownames = FALSE,
      file = paste0(output,"ch04.tex"))

