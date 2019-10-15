#############################################################################
#### Bio201 - Relashionship between human dody length and Digitus medius ####
#############################################################################


# first install the following packages in the console using the command: install.packages(...)

# then load library
library("gsheet")
library("ggplot2")
library("tidyverse")

# now you can read in data
# you have to go to your google sheet and copy the url and past it here
dat <- gsheet2tbl("https://docs.google.com/spreadsheets/d/125h8IGB4yPBBy7wUOBUPzvDUl29TED2aS7OF4XLaMP4/edit?usp=sharing")

# remove NAs
dat <- dat %>% 
  filter(!is.na(BodyLength_cm))

# Make a plot using ggplot
ggplot(dat, aes(x = BodyLength_cm, y = DigitusMediusLength_cm)) +
  geom_point() +
  geom_smooth(method='lm')

# Test correlation
cor(dat$BodyLength_cm, dat$DigitusMediusLength_cm)
