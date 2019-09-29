# Group assignment
# Track how learning is going in class (goal 1)
# deal with issues others had (go deeper into it), take over some of the learning from the others, goal 2
# get some data from classmates (like how many languages u know) goal 3

#first team presents in 2 weeks!!!

library(tidyverse)

# 1. you have to tell it some data first
# 2. mapping = aes(...) aes is aesthetic -> map variables that ggplot understands
# 3. geometric object (geom) 'geom_poit()' for scatter plot (part of the work for next week is to find out which does what)

library(ggplot2)
library(gapminder)
gapminder

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p # it does not yet know how to plot it

p + geom_point()

str(p) # gives the structure of argument, make sure the output does not pop up in the markdown!!!

p + geom_point()
p + geom_smooth() # gray area: region of confidence

p + geom_point() + geom_smooth() # when adding them up think of them as layers (the order matters), this is where ggplot starts to get better than plot

# Can we transform data inside ggplot?

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp * 2)) # do not do data transformation within ggplot!!
p + geom_point()

# Which other methods does geom_smooth() have?

?geom_smooth()
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + 
  geom_smooth() + 
  geom_smooth(method = "lm", color = "green") + #lm = linear
  geom_smooth(method = "loess", color = "red")

# Let's see it on a log plot

p + geom_point() + geom_smooth()
p + geom_point() + geom_smooth() + scale_x_log10()
p + geom_point() + geom_smooth() + scale_x_log10(labels = scales::dollar)
p + geom_point() + geom_smooth(method = "lm") + scale_x_log10(labels = scales::comma)

# Colour settings of plot, make the data points yellow
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(color = "yellow") + scale_x_log10()

# Setting attributes in aes(), determine the color based on continent
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent))
p + geom_point() + scale_x_log10()

p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 2, method = "lm") + scale_x_log10()
# se stands for standard error

p + geom_point() + geom_smooth(color = "orange", size = 2, method = "lm") # to see se u have to get rid of log scaling

# Fitting to data by continent, property fill sets the color of confidence interval 
p_continent <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p_continent + geom_point() + geom_smooth() + scale_x_log10()

# Setting labels
p_continent + geom_point() + geom_smooth() + scale_x_log10(labels = scales::dollar) + labs(x = "GDP Per Capita", y = "Life Expectancy", title = "Economic Growth and Life Expectancy", subtitle = "Data Points are country-years", caption = "Source: Gapminder")

# geom_smooth automatically inherits mapping's grouping
#say we want to do it differently, we want one line

p_continent <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p_continent + geom_point(mapping = aes(color = continent)) + 
  geom_smooth(color = 'yellow') + scale_x_log10(labels = scales::dollar) + 
  labs(x = "GDP Per Capita", 
       y = "Life Expectancy", 
       title = "Economic Growth and Life Expectancy", 
       subtitle = "Data Points are country-years", 
       caption = "Source: Gapminder")
