##################################################################
### CHAPTER III: ANALYZING WORD AND DOCUMENT FREQUENCY: tf-idf ###
##################################################################

# - - - - - - - - - - - - - - - - - - - - - - - #
   # Term Frequency in Jane Austen's Novels #
# - - - - - - - - - - - - - - - - - - - - - - - #

# What are the most commonly used words in Jane Austen's novels?
# (Let's also calculate the total words in each novel here, for later use)
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)
book_words

# Term Frequency distribution in Jane Austen's novels
library(ggplot2)

ggplot(book_words, aes(n/total, fill=book)) + 
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# - - - - - - - - - - - - - - - - - - - - - - - #
                  # Zipf's Law #
# - - - - - - - - - - - - - - - - - - - - - - - #
# Zipf's law states that the frequency that a word appears is 
# inversely proportional to its rank.

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank

# Plotting this way, an inversely proportional relationship will have
# a constant, negative slope
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + 
  scale_y_log10()

# Let's see what the exponent of the power law is for the middle section
# of the rank range.
rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# The Zipf's law with ls
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, color="gray50", linetype=2) +
  geom_line(size = 1.1, alpha=0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# - - - - - - - - - - - - - - - - - - - - - - - #
          # The bind_tf_idf Function #
# - - - - - - - - - - - - - - - - - - - - - - - #

# The idea of tf-idf is to find the important words for the content of
# each document by decreasing the weight for commonly used words an 
# increasing the weight for words that are not used very much in a collection
# or corpus of documents, in this case, the group of Jane Austen's novels
# as a whole. Calculating tf-idf attempts to find the words that are
# important (i.e. common) in a text, but not too common. Let's do that now.

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

# Let's look at terms with high tf-idf in Jane Austen's works
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Let's look at a visualization for these high tf-idf words in
# Highest tf-idf words in each of Jane Austen's novels
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol =2, scales = "free") +
  coord_flip()

# NOTE: This is the point of tf-idf; it identifies
# words that are important to one document within a collection of documents.

# - - - - - - - - - - - - - - - - - - - - - - - #
          # A Corpus of Physics Text #
# - - - - - - - - - - - - - - - - - - - - - - - #

# Let's download "Discourse on Floating Bodies by Galileo Galilei",
# "Treatise on Light by Christiaan Huygens", Experiments with Alternate
# Current of High Potential and High Frequency by Nikola Tesla, and
# "Relativity: the Special and General Theory by Albert Einstein.

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author")

# Now that we have the texts, let's use unnest_tokens() and count()
# to find out how many times each word is used in each text.
physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words
colnames(physics_words)
# Here we see just the raw counts; we need to remember that these 
# documents are all different lengths. Let's go ahead and calculate
# tf-idf, then visualize the high tf-idf words in 
# figure: Highest tf-idf words in each physics text
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# Very interesting indeed. One thing we see here is "eq" in the 
# Einstein text?!
library(stringr)

physics %>%
  filter(str_detect(text, "eq\\.")) %>%
  select(text)

physics %>%
  filter(str_detect(text, "refraction")) %>%
  select(text)

# Let's remove some of these less meaningful words to make a better,
# more meaningful plot. Notice that we make a custom list of stop words
# and use anti_join() to remove them; this is a flexible approach that
# can be used in many situations.

mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak",
                                   "bn", "fig", "file", "cg", "cb",
                                   "cm"))

physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

# fig 3.6 - Highest tf-idf words in classic physics texts
ggplot(plot_physics, aes(word, tf_idf, fill=author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol=2, scales = "free") +
  coord_flip()

