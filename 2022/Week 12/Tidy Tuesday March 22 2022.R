# Load in required packages
library(tidytuesdayR)
library(tidyverse)

# Read in all the datasets for the week of March 22, 2022.
tuesdata <- tidytuesdayR::tt_load('2022-03-22')

# Read in the baby names dataset.
babynames <- tuesdata$babynames

# I want to visualize the number of babies that were given names starting with the letter "D" over the years.
# To do that, some data cleaning is necessary.

# First, create a new clean babynames dataset, separate of the original dataset.
babynames_clean <- babynames

# Second, capitalize all names so that in the next step, the filter doesn't miss any names.
babynames_clean$name <- toupper(babynames_clean$name)

# Third, filter out names so that only names that begin with the letter "D" remain.
babynames_clean <-
  babynames_clean %>%
  filter(stringr::str_detect(name, "^D"))

# Fourth, group the names by year, and count the total number of times a baby was given a name starting with the letter "D".
babynames_clean <-
  babynames_clean %>%
  group_by(year) %>%
  summarise(total = sum(n))

# Fifth, this next line makes sure scientific notation is not present in the plot.
options(scipen = 999)

# Sixth, create a histogram to visually display the results.
ggplot(data = babynames_clean, aes(x = year, y = total)) +
  labs(title = "Number of Babies In The U.S. Given A Name Beginning With The Letter 'D'",
       caption = "Data Source: babynames R package from Hadley Wickham.
       Graph created by David Acosta.",
       x = "Year",
       y = "Number of Names Given Per Year") +
  geom_point(colour = "#6699FF") +
  geom_line()
