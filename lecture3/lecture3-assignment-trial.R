# CLEAR MEMORY
rm(list=ls())

# Import libraries
install.packages("scales")
library(ggplot2)
library(tidyverse)
library(scales)

# set the path
dir <-  "D:/Egyetem/CEU/Coding_1/R-Coding/"


#location folders
data_in <- paste0(dir,"da_data_repo/hotels-vienna/clean/")
data_out <-  paste0(dir,"da_case_studies/ch03-hotels-vienna-explore/")
output <- paste0(dir,"da_case_studies/ch03-hotels-vienna-explore/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


# load vienna
vienna <- read_csv(paste0(data_in,"hotels-vienna.csv"))

View(vienna)

vienna

filter(vienna, stars == 4.0)
