########################################################################
      ### CHAPTER VII: Case Study - Comparing Twitter Archives ###
########################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
          # Getting the Data and Distribution of Tweets #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

# DESCARGAMOS LOS DATOS
# Tweets of Julia
url <- "https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/tweets_julia.csv"
destino <- "tweets_julia.csv"
download.file(url, destino)

tweets_julia <- read.csv("tweets_julia.csv")

# Tweets of Dave
url1 <- "https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/tweets_dave.csv"
destino1 <- "tweets_dave.csv"
download.file(url1, destino1)

tweets_dave <- read.csv("tweets_dave.csv")

# UNIMOS BASE DE DATOS
tweets <- bind_rows(tweets_julia %>%
                      mutate(person = "Julia"),
                    tweets_dave %>%
                      mutate(person = "David")) %>%
  mutate(timestamp = ymd_hms(timestamp))

# Figure - All tweets from our accounts
ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
                          # Word Frequencies #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

library(tidytext)
library(stringr)

replace_reg1 <- "https://t.co/[A-Za-z\\d]+|"
replace_reg2 <- "http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
replace_reg <- paste0(replace_reg1, replace_reg2)
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <-tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

frequency <- tidy_tweets %>%
  group_by(person) %>%
  count(word, sort = TRUE) %>%
  left_join(tidy_tweets %>%
              group_by(person) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

# This is a nice and tidy data frame, but we would actually like to plot those frequencies
# on the x- and y-axes of a plot, so we will need to use spread() from tidyr to make a
# differently shaped data frame.
library(tidyr)

frequency <- frequency %>%
  select(person, word, freq) %>%
  spread(person, freq) %>%
  arrange(Julia, David)

frequency

# Now this is ready for us to plot. Let's use geom_jitter() so that we don't see the discreteness
# at the low end of frequency as much, and check_overlap = TRUE so the
# text labels don't all print out on top of each other (only some will print)

library(scales)

# Figure - Comparing the frequency of words used by Julia and David
ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
                        # Comparing Word Usage #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# We just made a plot comparing raw word frequencies over our whole Twitter histories;
# now let's find which words are more or less likely to come from each person's
# account using the log odds ratio. First, let's restrict the analysis moving forward to
# tweets from David and Julia sent during 2016. David was consistently active on Twitter
# for all of 2016, and this was about when Julia transitioned into data science as a
# career.
tidy_tweets <- tidy_tweets %>%
  filter(timestamp >= as.Date("2016-01-01"),
         timestamp < as.Date("2017-01-01"))

# Next, let's use str_detect() to remove Twitter usernames from the word column,
# because otherwise, the results here are dominated only by people who Julia or David
# know and the other does not. After removing these, we count how many times each
# person uses each word and keep only the words used more than 10 times. After a
# spread() operation, we can calculate the log odds ratio for each word,
# where n is the number of times the word in question is used by each person, and the
# total indicates the total words for each person.
word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill=0) %>%
  mutate_if(is.numeric, funs((. + 1) /sum(. + 1))) %>%
  mutate(logratio = log(David / Julia)) %>%
  arrange(desc(logratio))

# What are some words that have been about equally likely to come from David's or
# Julia's account during 2016?
word_ratios %>%
  arrange(abs(logratio))

# We are about equally likely to tweet about maps, email, APIs, and functions.
# Which words are most likely to be from Julia's account or from David's account? Let's
# just take the top 15 most distinctive words for each account and plot them
  
# Figure - Comparing the odds ratios of words from our accounts
word_ratios %>%
    group_by(logratio < 0) %>%
    top_n(15, abs(logratio)) %>%
    ungroup() %>%
    mutate(word = reorder(word, logratio)) %>%
    ggplot(aes(word, logratio, fill = logratio < 0)) +
    geom_col(show.legend = TRUE) +
    coord_flip() +
    ylab("log odds ratio (David/Julia)") +
    scale_fill_discrete(name = "", labels = c("David", "Julia"))
  

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
                           # Changes in Word Use #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# The section above looked at overall word use, but now let's ask a different question.
# Which words' frequencies have changed the fastest in our Twitter feeds? Or to state
# this another way, which words have we tweeted about at a higher or lower rate as time
# has passed? To do this, we will define a new time variable in the data frame that
# defines which unit of time each tweet was posted in. We can use floor_date() from
# lubridate to do this, with a unit of our choosing; using 1 month seems to work well
# for this year of tweets from both of us.
# After we have the time bins defined, we count how many times each of us used each
# word in each time bin. After that, we add columns to the data frame for the total
# number of words used in each time bin by each person and the total number of times
# each word was used by each person. We can then filter() to only keep words used
# at least some minimum number of times (30, in this case).
words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
  count(time_floor, person, word) %>%
  ungroup() %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_by_time

# Each row in this data frame corresponds to one person using one word in a given
# time bin. The count column tells us how many times that person used that word in
# that time bin, the time_total column tells us how many words that person used during
# that time bin, and the word_total column tells us how many times that person
# used that word over the whole year. This is the data set we can use for modeling.
# We can use nest() from tidyr to make a data frame with a list column that contains
# little miniature data frames for each word. Let's do that now and take a look at the
# resulting structure.
nested_data <- words_by_time %>%
  nest(-word, -person)

nested_data

# This data frame has one row for each person-word combination; the data column is a
# list column that contains data frames, one for each combination of person and word.
# Let's use map() from the purrr library to apply our modeling procedure to each of
# those little data frames inside our big data frame. This is count data, so let's use glm()
# with family = "binomial" for modeling.
library(purrr)

nested_models <- nested_data %>%
    mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, .,
                                    family = "binomial")))

nested_models

#  We can think about this modeling procedure answering questions
#  like, "Was a given word mentioned in a given time bin? Yes or no?
#  How does the count of word mentions depend on time?"

# Now notice that we have a new column for the modeling results; it is another list column
# and contains glm objects. The next step is to use map() and tidy() from the
# broom package to pull out the slopes for each of these models and find the important
# ones. We are comparing many slopes here, and some of them are not statistically significant,
# so let's apply an adjustment to the p-values for multiple comparisons.
library(broom)

slopes <- nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

# Now let's find the most important slopes. Which words have changed
# in frequency at a moderately significant level in our tweets?
top_slopes <- slopes %>%
  filter(adjusted.p.value < 0.1) %>%
  select(-statistic, -p.value)

top_slopes

# To visualize our results, we can plot the use of these words for both David and Julia
# over this year of tweets

# Figure - trending words in David's tweets
words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "Julia") %>%
  ggplot(aes(time_floor, count/time_total, color=word, lty = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
                     # Favorites and Retweets #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# datos de tweets Julia
url2 <- "https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/juliasilge_tweets.csv"
destino2 <- "tweets_julia2.csv"
download.file(url2, destino2)

tweets_julia <- read.csv("tweets_julia2.csv")

# datos de tweets Dave
url3 <- "https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/drob_tweets.csv"
destino3 <- "tweets_dave.csv"
download.file(url3, destino3)

tweets_dave <- read.csv("tweets_dave.csv")

# Unimos los dos datos
tweets <- bind_rows(tweets_julia %>%
                      mutate(person = "Julia"),
                    tweets_dave %>%
                      mutate(person = "David")) %>%
  mutate(created_at = ymd_hms(created_at))

# Now that we have this second, smaller set of only recent tweets, let's use unn
# est_tokens() to transform these tweets to a tidy data set. Let's remove all retweets
# and replies from this data set so we only look at regular tweets that David and Julia
# have posted directly.

tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"))

tidy_tweets

# To start with, let's look at the number of times each of our tweets 
# was retweeted. Let's find the total number of retweets for each person

total <- tidy_tweets %>%
  group_by(person, id) %>%
  summarise(rts = sum(retweets)) %>%
  group_by(person) %>%
  summarise(total_rts = sum(rts))

total

# Now let's find the median number of retweets for each word and person. We probably
# want to count each tweet/word combination only once, so we will use group_by()
# and summarise() twice, one right after the other. The first summarise() statement
# counts how many times each word was retweeted, for each tweet and person. In the
# second summarise() statement, we can find the median retweets for each person and
# word, count the number of times each word was used by each person, and keep that
# in uses. Next, we can join this to the data frame of retweet totals. Let's filter() to
# only keep words mentioned at least five times.

word_by_rts <- tidy_tweets %>%
  group_by(id, word, person) %>%
  summarise(rts = first(retweets)) %>%
  group_by(person, word) %>%
  summarise(retweets = median(rts), uses = n()) %>%
  left_join(total) %>%
  filter(retweets !=0) %>%
  ungroup()

word_by_rts %>%
  filter(uses >= 5) %>%
  arrange(desc(retweets))

# At the top of this sorted data frame, we see tweets from Julia and David about packages
# that they work on, like gutenbergr, gganimate, and tidytext. Let's plot the words
# that have the highest median retweets for each of our accounts

word_by_rts %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL,
       y = "Median # of retweets for tweets containing each word")

# We see lots of words about R packages, including tidytext, a package about which you
# are reading right now! The "0" for David comes from tweets where he mentions version
# numbers of packages, like "broom 0.4.0" or similar.

# We can follow a similar procedure to see which words led to more favorites. Are they
# different than the words that lead to more retweets?

totals <- tidy_tweets %>%
  group_by(person, id) %>%
  summarise(favs = sum(favorites)) %>%
  group_by(person) %>%
  summarise(tota_favs = sum(favs))

word_by_favs <- tidy_tweets %>%
  group_by(id, word, person) %>%
  summarise(favs = first(favorites)) %>%
  group_by(person, word) %>%
  summarise(favorites = median(favs), uses = n()) %>%
  left_join(totals) %>%
  filter(favorites !=0) %>%
  ungroup()

# We have built the data frame we need. Now let's make our 
# visualization in 

# Figure - Words with highest median favorites
word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorites, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")

# We see some minor differences between Figures 7-6 and 7-7, especially near the bottom
# of the top 10 list, but these are largely the same words as for retweets. In general,
# the same words that lead to retweets lead to favorites. A prominent word for Julia in
# both plots is the hashtag for the NASA Datanauts program that she has participated
# in; read on to Chapter 8 to learn more about NASA data and what we can learn from
# text analysis of NASA datasets.