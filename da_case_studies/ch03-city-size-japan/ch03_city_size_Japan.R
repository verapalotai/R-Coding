###############################################
# Chapter 03

# city_size_japan : pop & lnpop
###############################################

# WHAT THIS CODES DOES:
## creates desrciptive stats

###############################################


# CLEAR MEMORY
rm(list=ls())

# Import libraries

library(ggplot2)
library(tidyverse)
library(scales)


#----------------------------------------------------------------------------------------------------
# Set your directory here
#dir <-  ""  # set your dir
#dir <- "C:/Users/user/Documents/eszter/other/bekeskezdi_2/ch03"
dir<- "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"

#location folders
data_in <- paste0(dir,"da_data_repo/city-size-japan/clean/")
data_out <-  paste0(dir,"da_case_studies/ch03-city-size-japan/")
output <- paste0(dir,"da_case_studies/ch03-city-size-japan/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


# load ggplot theme function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))

#-----------------------------------------------------------------------------------------
# import data
city_size <- read_csv(paste0(data_in, "city-size-japan.csv"))
describe(city_size)
# create variables

city_size <-  city_size %>%
  mutate(
    pop = (pop_2015/1000),
    lnpop = log(pop)) %>%
  arrange(-pop)

city_size <- city_size %>%
  mutate (rank = seq( from = 1, to = nrow(.), by = 1))

#------------------------------------------------------------
# ln(rank) vs ln(x)

city_size <-  city_size %>%
  mutate(lnrank = log(rank))


R_03_lnrank <- ggplot(data = city_size, aes(x=lnpop, y=lnrank)) +
  geom_point(size=2, colour=color[4], shape=16)+
  labs(x="ln(population)",y="ln(rank)")+
  geom_smooth(method="lm", colour=color[3], se=FALSE)+
  theme_bg()
R_03_lnrank
ggsave(paste0(output, "ch03_citysize-japan-logrank_R.png"), width=12, height=9, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "ch03_citysize-japan-logrank_R.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(R_03_lnrank)
dev.off()

#------------------------------------------------------------
## ln P(X>x) vs ln(x) figure
## should be the same s with ln(rank) except for constant shift


# city_size <-  city_size %>%
#   mutate(P = (rank / max(nrow(.))),
#          lnP = log(P))
# 
# R_03_lnP <- ggplot(data = city_size, aes(x=lnpop, y=lnP)) +
#   geom_point(size=3, colour=color[3], shape=16)+
#   labs(x="ln(population)",y="")+
#   geom_smooth(method="lm", colour=color[1], se=FALSE)+
#   theme_bg()
# R_03_lnP

#---------------------------------------------------------------
# scale invariance

x1 <-  200
x2 <- 300
bound <-  0.2

print(paste0(x1, " ", x2))

city_size %>%
  filter(pop >= x1*(1-bound) & pop <= x1*(1+bound)) %>%
  count()
  
city_size %>%
  filter(pop >= x2*(1-bound) & pop <= x2*(1+bound)) %>%
  count()  

shift <-  3  
x3 <-  x1*shift
x4 <-  x2*shift

print(paste0(x3, " ", x4))

city_size %>%
  filter(pop >= x3*(1-bound) & pop <= x3*(1+bound)) %>%
  count() 

city_size %>%
  filter(pop >= x4*(1-bound) & pop <= x4*(1+bound)) %>%
  count() 






  

