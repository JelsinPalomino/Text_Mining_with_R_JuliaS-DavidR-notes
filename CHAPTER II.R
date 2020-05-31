#####################################################
### CHAPTER II: SENTIMENT ANALYSIS WITH TIDY DATA ###
#####################################################

# The Sentiment Dataset
install.packages("textdata") # esto es necesario para descargar los diccionarios
                             # de AFINN y NRC, porque tidytext no los tiene por 
                             # defecto. 
library(tidytext)
sentiments

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# - - - - - - - - - - - - - - - - - - - - - - - #
     # Sentiment Analysis with Inner Join #
# - - - - - - - - - - - - - - - - - - - - - - - #

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# Small sections of text many not have enough words in them to get a good estimate
# of sentiment, while really large sections can wash out narrative structure. For
# these books, using 80 lines works well, but this can vary depending on individual
# texts, how long the lines were to start with, etc. We then use spread() so that
# we have negative and positive sentiment in separate columns, and lastly calculate
# a net sentiment (positive - negative).
library(tidyr)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Now we can plot these sentiment scores across the plot trajectory of each novel.
# Notice that we are plotting against the index on the x-axis that keeps track of 
# narrative time in sections of text
library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# - - - - - - - - - - - - - - - - - - - - - - - #
 # Comparing the Three Sentiment Dictionaries #
# - - - - - - - - - - - - - - - - - - - - - - - #

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice

# Let's again use integer division(%/%) to define larger sections of text
# that span multiple lines, and we can use the same pattern with count(),
# spread(), and mutate() to find the net sentiment in each of these section
# of text.

# - - - - - - - - - - - - - - - - - - - - - - # Cambiamos "score" por
# - - - - - - - - - - - - - - - - - - - - - - # "value" y luego transformarlo
# - - - - - - - - - - - - - - - - - - - - - - # en dataframe, con aes() o 
# - - - - - - - - - - - - - - - - - - - - - - # fortify() a afinn
# - - - - - - - - - - - - - - - - - - - - - - #
afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")
# - - - - - - - - - - - - - - - - - - - - - - #

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
    count(method, index = linenumber %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

# We now have an estimate of the net sentiment (positive - negative)
# in each chunk of the novel text for each sentiment lexicon. Let's 
# bind them together and visualize
bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Why is, for example, the result for the NRC lexicon biased so high
# in sentiment compared to the Bing et al. result? Let's look briefly
# at how many positive and negative words are in these lexicons.
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

# Both lexicons have more negative than positive words, but the ratio of negative to
# positive words is higher in the Bing lexicon than the NRC lexicon. This will contribute
# to the effect we see in the plot above, as will any systematic difference in word
# matches, for example, if the negative words in the NRC lexicon do not match very
# well with the words that Jane Austen uses. Whatever the source of these differences,
# we see similar relative trajectories across the narrative arc, with similar changes in
# slope, but marked differences in absolute sentiment from lexicon to lexicon. This is
# important context to keep in mind when choosing a sentiment lexicon for analysis.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
        # Most Common Positive and Negative Words #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# This can be shown visually, and we can pipe straight into ggplot2,
# if we like, because of the way we are consistently using tools built
# for handling tidy data frames.
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
# Words that contribute to positive and negative sentiment in
# Jane Austen's novels

# If it were appropriate for our purposes, we could easily add "miss" to
# a custom stop-words list using bind_rows(). We could implement that with
# a strategy such as this:
custom_stop_words <- bind_rows(tibble(word = c("miss"),           # - - - - - -# Cambiamos data_frame()
                               lexicon = c("custom")),stop_words) # - - - - - -# por tibble()
custom_stop_words

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
                     # Wordclouds #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
library(wordcloud)      # activamos tambien la libreria RColorBrewer
library(RColorBrewer)

# Figure of the most common words in Jane Austen's novels
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# In other function, suchs as comparison.cloud(), you may need to turn
# the data frame into a matrix with reshape2's acast(). Let's do the
# sentiment analysis to tag positive and negative words using an
# inner join, then find the most common positive and negative words.
# Until the step where we need to send the data to comparison.cloud(),
# this can all be done with joins, piping, and dplyr because our data
# is in tidy format.

# figure. Most common positive and negative words in Jane Austen's novels
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
         # Looking at Units Beyond Just Words #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# tokenizacion in sentences for score that sentiment algorithm
# the libraries is coreNLP(Arnold and Tilton 2016), cleanNLP
# (Arnold 2016), and sentimentr(Rinker 2017)
PandP_sentences <- tibble(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")

# Let's look at just one
PandP_sentences$sentence[2]

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

# We can use tidy text analysis to ask questions such as what are the most negative
# chapters in each of Jane Austen's novels? 
# Fist: Let's get the list of negative words from the Bing lexicon
# Second: Let's make a data frame of how many words are in each chapter
#         so we can normalize for chapter length.
# Third: Let's find the number of negative words in each chapter and
#        divide by the total words in each chapter.
# For each book, which chapter has the highest proportion of negative words?

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter !=0) %>%
  top_n(1) %>%
  ungroup()
