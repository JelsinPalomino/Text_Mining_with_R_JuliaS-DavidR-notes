########################################################################
                   ### CHAPTER V: TOPIC MODELING ###
########################################################################

library(topicmodels)

data("AssociatedPress")
AssociatedPress

# This function returns an object containing the full details of the model
# fit, such as how words are associated with topics and how topics are
# associated with documents.

# Set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k=2, control = list(seed = 1234))
ap_lda

# - - - - - - - - - - - - - - - - - - - - - - - #
          # Word-Topic Probabilities #
# - - - - - - - - - - - - - - - - - - - - - - - #

# In Chapter 5 we introduced the tidy() method, originally from the broom package
# (Robinson 2017), for tidying model objects. The tidytext package provides this
# method for extracting the per-topic-per-word probabilities, called ?? ("beta"), from
# the model.

library(tidytext)

ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics

# We could use dplyr's top_n() to find the 10 terms that are most common
# within each topic. As a tidy data frame, this lends itself well to a
# ggplot2 visualization

# Figure - The terms that are most common within each topic
library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# As an alternative, we could consider the terms that had the greatest difference in ??
# between topic 1 and topic 2. This can be estimated based on the log ratio of the two:
#   log(??2/??1)

# A log ratio is useful because it makes the difference symmetrical: ??2
# being twice as large leads to a log ratio of 1, while ??1 being twice as
# large results in -1.

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))

beta_spread

# graphic
beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()
# - - - - - - - - - - - - - - - - - - - - - - - #
           # Document-Topic Probabilities #
# - - - - - - - - - - - - - - - - - - - - - - - #
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# Each of these values is an estimated proportion of words from that document that are
# generated from that topic. For example, the model estimates that only about 24.8% of
# the words in document 1 are generated from topic 1.
# We can see that many of these documents are drawn from a mix of the two topics, but
# that document 6 is drawn almost entirely from topic 2, having a ?? from topic 1 close
# to zero.

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

# - - - - - - - - - - - - - - - - - - - - - - - #
     # Example: The Great Library Heist #
# - - - - - - - - - - - - - - - - - - - - - - - #

titles <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds",
            "Pride and Prejudice",
            "Great Expectations")
library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# As preprocessing, we divide these into chapters, use tidytext's unnest_tokens() to
# separate them into words, then remove stop_words. We're treating every chapter as a
# separate "document," each with a name like Great Expectations_1 or Pride and
# Prejudice_11. (In other applications, each document might be one newspaper article,
# or one blog post).

library(stringr)

# divide into documents, each representing one chapter
reg <- regex("^chapter ", ignore_case = TRUE)
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, reg))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# - - - - - - - - - - - - - - - - - - - - - - - #
                # LDA on Chapters #
# - - - - - - - - - - - - - - - - - - - - - - - #

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

# We can then use the LDA() function to create a four-topic model. In this case we
# know we're looking for four topics because there are four books; in other problems
# we may need to try a few different values of k.

chapters_lda <- LDA(chapters_dtm, k=4, control = list(seed = 1234))
chapters_lda

# Much as we did on the Associated Press data, we can examine
# per-topic-per-word probabilities

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# Notice that this has turned the model into a one-topic-per-term-per-row format. For
# each combination, the model computes the probability of that term being generated
# from that topic. For example, the term "joe" has an almost zero probability of being
# generated from topics 1, 2, or 3, but it makes up 1.45% of topic 4.

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# This tidy output lends itself well to ggplot2 visualization

# Figure - The terms that are most common within each topic
library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# - - - - - - - - - - - - - - - - - - - - - - - #
         # Per-Document Classification #
# - - - - - - - - - - - - - - - - - - - - - - - #

# Each document in this analysis represented a single chapter. Thus, we may want to
# know which topics are associated with each document. Can we put the chapters back
# together in the correct books? We can find this by examining the per-document-pertopic
# probabilities, ?? ("gamma").

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

# Each of these values is an estimated proportion of words from that document that are
# generated from that topic. For example, the model estimates that each word in the
# Great Expectations_57 document has only a 0.00135% probability of coming from
# topic 1 (Pride and Prejudice).
# Now that we have these topic probabilities, we can see how well our unsupervised
# learning did at distinguishing the four books. We'd expect that chapters within a book
# would be found to be mostly (or entirely) generated from the corresponding topic.
# First we re-separate the document name into title and chapter, after which we can visualize
# the per-document-per-topic probability for each

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

# Reorder titles in order of topic 1, topic 2, etc. before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

# We notice that almost all of the chapters from Pride and Prejudice, The War of the
# Worlds, and Twenty Thousand Leagues Under the Sea were uniquely identified as a
# single topic each.
# It does look like some chapters from Great Expectations (which should be topic 4)
# were somewhat associated with other topics. Are there any cases where the topic most
# associated with a chapter belonged to another book? First we'd find the topic that was
# most associated with each chapter using top_n(), which is effectively the "classification"
# of that chapter.

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()
chapter_classifications

# We can then compare each to the "consensus" topic for each book (the most common
# topic among its chapters), and see which were most often misidentified.
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by="topic") %>%
  filter(title != consensus)

# We see that only two chapters from Great Expectations were misclassified, as LDA
# described one as coming from the Pride and Prejudice topic (topic 1) and one from
# The War of the Worlds (topic 3). That's not bad for unsupervised clustering!

# - - - - - - - - - - - - - - - - - - - - - - - #
        # By-Word Assignments: augment #
# - - - - - - - - - - - - - - - - - - - - - - - #

# One step of the LDA algorithm is assigning each word in each document to a topic.
# The more words in a document are assigned to that topic, generally, the more weight
# (gamma) will go on that document-topic classification.
# We may want to take the original document-word pairs and find which words in each
# document were assigned to which topic. This is the job of the augment() function,
# which also originated in the broom package as a way of tidying model output. While
# tidy() retrieves the statistical components of the model, augment() uses a model to
# add information to each observation in the original data.

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

# This returns a tidy data frame of book-term counts, but adds an extra column,
# .topic, with the topic each term was assigned to within each document. (Extra                             columns added by augment always start with . to prevent overwriting existing columns.)
# We can combine this assignments table with the consensus book titles to find
# which words were incorrectly classified.
assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

# This combination of the true book (title) and the book assigned to it (consensus) is
# useful for further exploration. We can, for example, visualize a confusion matrix,
# showing how often words from one book were assigned to another, using dplyr's
# count() and ggplot2's geom_tile

# Figure - Confusion matrix showing where LDA assigned the words from
# each book. Each row of this table represents the true book each word came
# from, and each column represent what book it was assigned to.
library(scales)
assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high="red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

# What were the most commonly mistaken words?
wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

# On the other hand, there are a few wrongly classified words that never appeared in
# the novel they were misassigned to. For example, we can confirm "flopson" appears
# only in Great Expectations, even though it's assigned to the Pride and Prejudice cluster.
word_counts %>%
  filter(word == "flopson")

# The LDA algorithm is stochastic, and it can accidentally land on a topic that spans
# multiple books.

# - - - - - - - - - - - - - - - - - - - - - - - #
      # Alternative LDA Implementations #
# - - - - - - - - - - - - - - - - - - - - - - - #

# The LDA() function in the topicmodels package is only one implementation of the
# latent Dirichlet allocation algorithm. For example, the mallet package (Mimno 2013)
# implements a wrapper around the MALLET Java package for text classification tools,
# and the tidytext package provides tidiers for this model output as well.
install.packages("mallet")
library(rJava)
library(mallet)

# Create a vector with one string per chapter
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# Create an empty file of "stop_words"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

# word-topic pairs
tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)
