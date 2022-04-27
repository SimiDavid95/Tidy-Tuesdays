###############################
# Tidy Tuesday - April 26, 2022
###############################

# Load in necessary libraries.
library(tidytuesdayR)
library(tidyverse)
library(ggwordcloud)

# Set seed for reproducible word cloud.
set.seed(314159)

# Get the data.
tuesdata <- tidytuesdayR::tt_load("2022-04-26")

hidden_gems <- tuesdata$hidden_gems

# Create a version of the dataset that will be the clean dataset.
clean_hidden_gems <- hidden_gems

# We are only interested in the title variable, so remove all other variables.
clean_hidden_gems <-
  clean_hidden_gems %>%
  select(title)

# Remove all non-alphanumeric characters from each title, and convert to lowercase.
for (i in 1:length(clean_hidden_gems$title)) {
  clean_hidden_gems$title[i] <- str_replace_all(clean_hidden_gems$title[i], "[^[:alnum:]]", " ")
  clean_hidden_gems$title[i] <- str_to_lower(clean_hidden_gems$title[i])
}

# Collect each individual word from each title and add it to a vector called words_list.
words_list <- c()

for (i in 1:length(clean_hidden_gems$title)) {
  words_list <- c(words_list, unlist(str_split(clean_hidden_gems$title[i], " "), recursive = TRUE))
}

# Create a tibble using the words_list vector, which will have only one column with each word as a separate entry.
words_list_tibble <- tibble(words_list)

# Rename column in tibble as "word", remove any blank entries and any entries with only one letter, 
# group by common words to remove duplicates, and count the number of occurrences for each word.
words_list_tibble <-
  words_list_tibble %>%
  rename(word = words_list) %>%
  filter(str_length(word) > 1) %>%
  group_by(word) %>%
  count()

# Remove any words that don't add significance to our results ("and", "of", "to", etc.)
words_list_tibble <-
  words_list_tibble %>%
  filter(word != "and") %>%
  filter(word != "of") %>%
  filter(word != "in") %>%
  filter(word != "the") %>%
  filter(word != "to") %>%
  filter(word != "who") %>%
  filter(word != "what") %>%
  filter(word != "when") %>%
  filter(word != "where") %>%
  filter(word != "why") %>%
  filter(word != "how") %>%
  filter(word != "or") %>%
  filter(word != "for") %>%
  filter(word != "using") %>%
  filter(word != "an") %>%
  filter(word != "at") %>%
  filter(word != "be") %>%
  filter(word != "do") %>%
  filter(word != "does") %>%
  filter(word != "is") %>%
  filter(word != "here") %>%
  filter(word != "on") %>%
  filter(word != "with") %>%
  filter(word != "you") %>%
  filter(word != "your") %>%
  filter(word != "we") %>%
  filter(word != "get") %>%
  filter(word != "getting") %>%
  filter(word != "from")

# Remove words that only occurred once or twice.
words_list_tibble <-
  words_list_tibble %>%
  filter(n > 2)

# Create word cloud.
word_cloud_plot <-
  words_list_tibble %>%
  ggplot(aes(label = word, size = n, colour = n)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 24) +
    labs(title = "Most Common Topics From Hidden Gems Kaggle Notebooks",
         caption = "Data: Kaggle | Viz: Juan David Acosta") +
    theme_minimal() +
    scale_colour_gradient(low = "#2D82B5", high = "#015C92", space = "Lab", aesthetics = "colour")
word_cloud_plot
