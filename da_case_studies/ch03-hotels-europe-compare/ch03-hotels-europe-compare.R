###############################################
# Chapter 03

# DATA ANALYSIS TEXTBOOK
# CH03
# Describe hotels-vienna
# 

# WHAT THIS CODES DOES:
# Focus on histograms

###############################################



# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(rlang)
library(xtable)



# set the path
dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"

#location folders
data_in <- paste0(dir,"da_data_repo/hotels-europe/clean/")
data_out <-  paste0(dir,"da_case_studies/ch03-hotels-europe-compare/")
output <- paste0(dir,"da_case_studies/ch03-hotels-europe-compare/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))


# load in clean and tidy data and create workfile
hotels_europe_price <- read_csv(paste0(data_in,"hotels-europe_price.csv"))
hotels_europe_features <- read_csv(paste0(data_in,"hotels-europe_features.csv"))

hotels_europe <- left_join(hotels_europe_price, hotels_europe_features, by = "hotel_id")
rm(hotels_europe_price)
rm(hotels_europe_features)

# filter for same Vienna data we used + London same date
hotels_europe_cut <- hotels_europe %>% filter(year == 2017 & month == 11 & weekend == 0) %>%
                     filter(city %in% c("Vienna", "London")) %>%
                     filter(accommodation_type == "Hotel") %>%
                     filter(stars>=3 & stars<=4) %>%
                     filter(!is.na(stars)) %>%
                     filter(city_actual %in% c("Vienna", "London")) %>%
                     filter(price <=600)


hotels_europe_cut %>%
  group_by(city) %>%
  summarise(mean_price = mean(price), max=max(price), n = n())

write_csv(hotels_europe_cut, paste0(data_out,"hotels-vienna-london.csv"))



# Vienna vs London
# have same range on x axis

# Figure 3.4 a) and b)
ggplot(data = filter(hotels_europe_cut, city=="Vienna"), aes (x = price, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 20, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(x = "Price", y = "Percentage") +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bg() +
  background_grid(major = "y", minor = "y")
ggsave(paste0(output, "histprice_Vienna5_R.png"), width=12, height=9, units = "cm", dpi = 1200)

ggplot(data = filter(hotels_europe_cut, city=="London"), aes (x = price, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 20, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(x = "Price", y = "Percentage") +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  scale_y_continuous(limits = c(0, 0.3), labels = scales::percent_format()) +
  theme_bg() +
  background_grid(major = "y", minor = "y")
ggsave(paste0(output, "histprice_London_R.png"), width=12, height=9, units = "cm", dpi = 1200)


# kernel density plots
# Figure 3.5

ggplot(data = hotels_europe_cut, aes(x=price, y = stat(density), color = city)) +
  geom_line(stat="density",  show.legend=F, na.rm =TRUE) +
  labs(x="Price", y="Density", color = "") +
  scale_color_manual(name="", 
                     values=c(color[2],color[1]),
                     labels=c("London","Vienna")) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  geom_text(aes(x = 340, y = 0.003, label = "London"), color = color[2], size=4) +
  geom_text(aes(x = 200, y = 0.007, label = "Vienna"), color = color[1], size=4) +
  theme_bg() +
  background_grid(major = "y", minor = "y")
ggsave(paste0(output, "kdens_ViennaLondon_R.png"), width=12, height=9, units = "cm", dpi = 1200)


# Table 3.6
table_3_6 <- 
  hotels_europe_cut %>%
  group_by(city) %>%
  summarise(n = length(price), mean=mean(price), median=median(price), min = min(price), max = max(price), 
            sd = sd(price), skew= ((mean(price)-median(price))/sd(price)))
table_3_6
# print out nicely
xt<-xtable(table_3_6,align='llccccccc', digits = c(0, 0,0,2,0,0,0,2,3))
names(xt) <- c('City','Observations','Mean','Median','Min','Max','Std.dev.','Skewness' )
print(xt, type = "latex",include.rownames = FALSE, 
      file = paste0(output,"ch03_vienna-london-compare.tex"))


