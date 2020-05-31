########################################################################
### CHAPTER IV: RELATIONSHIPS BETWEEN WORDS: N-grams and Correlations###
########################################################################

# introduce two new packages: ggraph, by Thomas Pedersen, which extend ggplot2
# to construct network plots, and widyr, which calculates pairwise correlations
# and distances within a tidy data frame. Together these expand our toolbox
# for exploring text within the tidy data framework.

# - - - - - - - - - - - - - - - - - - - - - - - #
            # Tokenizing by N-gram #
# - - - - - - - - - - - - - - - - - - - - - - - #

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

austen_bigrams

# - - - - - - - - - - - - - - - - - - - - - - - #
       # Counting and Filtering N-grams #
# - - - - - - - - - - - - - - - - - - - - - - - #

austen_bigrams %>%
  count(bigram, sort = TRUE)

# This is useful time to use tidyr's separate()
library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

# In other analyses, we may want to work with the recombined words. tidyr's unite()
# function is the inverse of separate(), and lets us recombine the columns into one.
# Thus, "separate/filter/count/unite" let us find the most common bigrams not
# containing stop words
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# in other analyses you may be interested in the most common trigrams,
# which are consecutive sequences of three words. We can find this by
# setting n = 3
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - #
              # Analyzing Bigrams #
# - - - - - - - - - - - - - - - - - - - - - - - #
# This one-bigram-per-row format is helpful for exploratory analyses of 
# the text. As a simple example, we might be interested in the most common
# "streets" mentioned in each book.
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# A bigram can also be treated as a term in a document in the same way 
# that we treated individual words.
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_if_idf

bigram_if_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol =2, scales = "free") +
  coord_flip()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
   # Using Bigrams to Provide Context in Sentiment Analysis #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# Now that we have the data organized into bigrams, it's easy to tell
# how often words are preceded by a word like "not"
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# By performing sentiment analysis on the bigram data, we can examine 
# how often sentiment-associated words are preceded by "not" or other
# negating words. We could use this to ignore or even reverse their 
# contribution to the sentiment score.
# Let's use the AFINN lexicon for sentiment analysis, which you may 
# recall gives a numeric sentiment score for each word, with positive
# or negative numbers indicating the direction of the sentiment.

AFINN <- get_sentiments("afinn")
AFINN

# We can then examine the most frequent words that were preceded by "not"
# and were associated with a sentiment.
not_words <- bigrams_separated %>%               # - - - - - -# Cambiamos "score" del 
  filter(word1 == "not") %>%                     # - - - - - -# libro por "value" que
  inner_join(AFINN, by = c(word2 = "word")) %>%  # - - - - - -# es el nombre del valor
  count(word2, value, sort = TRUE) %>%           # - - - - - -# del sentimiento asignado
  ungroup()                                      # - - - - - -# por AFINN

not_words

# For example, the most common sentiment-associated word to follow "not"
# was "like", which would normally have a (positive) score of 2.
# It's worth asking which words contributed the most in the "wrong" direction.
# To compute that, we can multiply their score by the number of times
# they appear (so that a word with a score of +3 ocurring 10 times has a 
# much impact as a word with a sentiment score of +1 ocurring 30 times).
# We visualize the result with a bar plot 

# Figure: The 20 words followed by "not" that had the greatest contribution
#         to sentiment scores, in either a positive or negative direction
not_words %>%
  mutate(contribution = n*value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n*value, fill=n*value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Word preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

# "Not" isn't the only term that provides some context
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by=c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup()

# Me falta hacer el grafico de las cuatro palabras sobre este tema.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
        # Visualizing a Network of Bigrams with ggraph #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# Here we'll be referring to a graph not in the sense of a visualization,
# but as a combination of connected nodes. A graph can be constructed
# from a tidy object since it has three variables:
# from   - the node an edge is coming from
# to     - the node an edge is going toward
# weight - A numeric value associated with each edge
library(igraph)

#original counts
bigram_counts

#filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

# igraph has plotting function built in, but they're not what the package
# is designed to do, so many other packages have developed visualization
# methods for graph objects.

# We recomended ggraph. For example, for a basic graph we need to add
# three layers: nodes, edges, and text
# install.packages("ggraph") - ya lo tengo ahora
library(ggplot2)
library(ggraph)
set.seed(2017)

# Figure: Common bigrams in Pride and Prejudice, showing those that 
# ocurred more than 20 times and where neither word was a stop word
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)

# We conclude with a few polishing operations to make a better-looking
# graph:
# - We add the "edge_alpha" aesthetic to the link layer to make links
#   transparent based on how common or rare the bigram is.
# - We add directionality with an arrow, constructed using "grid::arrow()",
#   including an "end_cap" option that tells the arrow to end before 
#   touching the node.
# - We tinker with the options to the node layer to make the nodes more
#   attractive.
#   We add a theme that's useful for plotting networks, "theme_void()"

#figure: Common bigrams in Pride and Prejudice, with some polishing
set.seed(2016)

a <- grid::arrow(type = "closed", length=unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Note that this is a visualization of a Markov chain, a common
# model in text processing. In a Markov chain, each choice of word
# depends only on the previous word. In this case, a random generator
# following this model might spit out "dear," then "sir," then "william/
#   walter/thomas/thomas's" by following each word to the most
# common words that follow it. To make the visualization interpretable,
# we chose to show only the most common word-to-word connections,
# but one could imagine an enormous graph representing
# all connections that occur in the text.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
           # Visualizing Bigrams in Other Texts #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# We went to a good amount of work in cleaning and visualizing bigrams
# on a text dataset, so let's collect it into a function so that we can
# easily perform it on other text datasets.

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token="ngrams", n=2) %>%
    separate(bigram, c("word1", "word2"), sep=" ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, 
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lighblue", size = 5) + 
    geom_node_text(aes(label=name), vjust=1, hjust=1) +
    theme_void()
}

# At this point, we could visualize bigrams in other works, such as 
# the King James Bible:
# The king James version is book 10 on project Gutenberg:
library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)
kjv_bigrams <- kjv %>%
  count_bigrams() 

# filter out rare combinations, as well as digits 
kjv_bigrams %>%
  filter(n > 40, 
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    # COUNTING AND CORRELATING PAIRS OF WORDS WITH THE WIDYR PACKAGE #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
                 # Counting and Correlating Among Sectinos #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# One useful function from widyr is the "pairwise_count()" function.
# The prefix "pairwise_" means it will result in one row for each pair
# of words in the "word" variable. This lets us count common pairs 
# of words co-appearing within the same section. 
install.packages("widyr")
library(widyr)

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort=TRUE)

word_pairs

# Notice that while the input had one row for each pair of a document (a 10-line section)
# and a word, the output has one row for each pair of words. This is also a tidy
# format, but of a very different structure that we can use to answer new questions.
# For example, we can see that the most common pair of words in a section is "Elizabeth"
# and "Darcy" (the two main characters). We can easily find the words that most
# often occur with Darcy.
word_pairs %>%
  filter(item1 == "darcy")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
                # Examining Pairwise Correlation #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# The phi coefficient is equivalent to the Pearson correlation, which
# you may have heard of elsewhere, when it is applied to binary data.
# The pairwise_cor() function in widyr lets us find the phi coefficient between words
# based on how often they appear in the same section. Its syntax is similar to pair
# wise_count().

# we need to filter for at leat relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort=TRUE)

word_cors

# This output format is helpful for exploration. For example, we could
# find the words most correlated with a word like "pounds" using a "filter"
# operation
word_cors %>%
  filter(item1 == "pounds")

# This lets us pick particular interesting words and find the other 
# words most associated with them

# Words from Pride and Prejudice that were most correlated with "elizabeth",
# "pounds", "married" and "pride".
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Just as we used ggraph to visualize bigrams, we can use it to 
# visualize the correlations and clusters of words that were found
# by the widyr package

# Figure - ¨Pairs of words in Pride and Prejudice that show at least a 
# 0.15 correlation of appearing within the same 10-line section
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Note that unlike the bigram analysis, the relationships here are symmetrical, rather
# than directional (there are no arrows). We can also see that while pairings of names
# and titles that dominated bigram pairings are common, such as "colonel/fitzwilliam,"
# we can also see pairings of words that appear close to each other, such as "walk" and
# "park," or "dance" and "ball."