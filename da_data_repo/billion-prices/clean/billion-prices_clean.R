############################################################
#
# DATA ANALYSIS TEXTBOOK
# Billion prices project
#
############################################################  
# WHAT THIS CODES DOES:

# Import stata file and save to csv

# Clear memory
rm(list=ls())

dir <- "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"

#location folders
data_in <- paste0(dir,"/cases_studies_public/billion-prices/raw/")
data_out <- paste0(dir,"/cases_studies_public/billion-prices/clean/")


library(haven)

# LOAD DATA
pd <- read_dta(paste(data_in,"online_offline_ALL_clean.dta",sep=""))

write.csv(pd, paste0(data_out,"online_offline_ALL_clean.csv"), row.names = F)

