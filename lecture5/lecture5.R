# Script for lecture 5

#Today: group_by(), summarise()
# Next week: read_*, parse_*, spread(), gather(), tidy the data

# Summary functions

(x <- 1:100)

median(x)
mean(x)
sd(x)
IQR(x)
mad(x)
?mad

min(x)
max(x)

#when using mean, consider median

mean(x)
median(x)
x_with_outlier <- c(x, 10000000)

mean(x_with_outlier)
median(x_with_outlier) # median is much more robust

# What is a quantile?
?quantile

quantile(x, 0.20) # what is below 20 percent
quantile(x, 0.80)

z <- c(0,0,0,0,0,1,2,100,100)
quantile(z,0.50)
quantile(z,0.90)
quantile(z,0.40)

# Counts

library(nycflights13)
library(tidyverse)

not_missing <- flights %>%
  filter(!is.na(arr_time), !is.na(dep_time)) %>%
  filter(!is.na(arr_delay), !is.na(dep_delay))

# Count the nr of flights to each destination

not_missing %>%
  group_by(dest) %>%
  summarise(
    count = n()
  )

# Shortcut

not_missing %>%
  count(dest)

# Count the nr of distinct carriers to each location

not_missing %>%
  group_by(dest) %>%
  summarise(
    carriers = n_distinct(carrier)
  ) %>%
  arrange(desc(carriers))

# You can weight the counts
# Count airmiles a given airplane does from NYC

not_missing %>%
  count(tailnum, wt = distance) %>% # how many flights each plane has done to and from NYC
  arrange(desc(n))

# observation: for adding weight 'wt' must be used!!

# Number of flights each day before 5am
not_missing %>%
  group_by(year, month, day) %>%
  summarise(before_5am = sum(dep_time < 500))
  #filter(dep_time < 500) %>% # another possibility but here the days with 0 flights before 5am disappear from the dataset
  #summarise(count=n())

# getting rid of group_by: ungroup

daily <- flights %>%
  group_by(year, month, day)

summarise(daily, n())

daily %>%
  ungroup() %>%
  summarise(n())

# 5.7 Grouped Mutates

# Get the worst 10 arrivers for every day
# Worst: arr_delay is highest

flights_subset <- flights %>%
  select(year:day, starts_with("arr"), starts_with("dep"))

flights_subset %>%
  group_by(year, month, day) %>%
  mutate(delay_rank = rank(desc(arr_delay))) %>% # rank is a summarising function
  filter(delay_rank <= 2) #%>%
  #arrange(delay_rank)

# Find destinations that have more than one flight arriving every day
(popular_destinations <- flights %>%
  group_by(dest) %>%
  #summarise(n = n()) %>% # nr of flights going to and from a dest in each year
  filter(n() > 365))

# Chapter 7

# cut_width() # take your data and puts it into bins

diamonds %>%
  ggplot(mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.5)

# histogram uses cut_width() to create these bins

diamonds %>%
  mutate(interval = cut_width(carat, 0.5)) %>%
  group_by(interval) %>%
  summarise(n = n())

#ifelse()

x <- 1:10
ifelse(x %% 2 == 0, 'even', 'odd')

#case when
?case_when
case_when(
  x %% 3 == 0 ~ "divisible by 3",
  x %% 3 == 0 ~ "+1",
  x %% 3 == 0 ~ "-1"
)

diamonds2 <- diamonds %>%
  mutate(y = ifelse(y<3|y>20,NA,y))


