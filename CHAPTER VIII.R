########################################################################
### CHAPTER VIII: Case Study - Mining NASA Metadata ###
########################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# How Data is Organized at NASA #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
library(jsonlite)
# metadata <- fromJSON("https://data.nasa.gov/data.json")

# write_json(metadata, "NASA.json")

load("metadata.rda")
names(metadata$dataset)

class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Wrangling and Tidying the Data #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

library(dplyr)

nasa_title <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                     title = metadata$dataset$title)
nasa_title

nasa_desc <- data_frame(id = metadata$dataset$`_id`$`$oid`,
                        desc = metadata$dataset$description)
nasa_desc %>%
  select(desc) %>%
  sample_n(5)

# Here we see the first part of several selected description fields from the metadata
# Now we can build the tidy data frame for the keywords. For this one,
# we need to use "unnest()" from tidyr, because they are in a list-column
library(tidyr)

nasa_keyword <- data_frame(id = metadata$dataset$`_id`$`$oid`,
                           keyword = metadata$dataset$keyword) %>%
  unnest(keyword)

nasa_keyword

# This is a tidy data frame because we have one row for each keyword; this means we
# will have multiple rows for each dataset because a dataset can have more than one
# keyword.
# Now it is time to use tidytext's unnest_tokens() for the title and description fields so
# we can do the text analysis. Let's also remove stop words from the titles and descriptions.
# We will not remove stop words from the keywords, because those are short,
# human-assigned keywords like "RADIATION" or "CLIMATE INDICATORS."
library(tidytext)

nasa_title <- nasa_title %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words)

nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)

# These are now in the tidy text format that we have been working with throughout this
# book, with one token (word, in this case) per row; let's take a look before we move on
# in our analysis.
nasa_title

nasa_desc

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Some Initial Simple Exploration #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# What are the most common words in tha NASA dataset titles? We can use "count()"
# from dplyr to check this out
nasa_title %>%
  count(word, sort = TRUE)

# What about the description?
nasa_desc %>%
  count(word, sort = TRUE)

# Words like "data" and "global" are used very often in NASA titles and descriptions.
# We may want to remove digits and some "words" like "v1" from these data frames for
# many types of analyses; they are not too meaningful for most audiences.
my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "v1", "v03", "l2", "l3", "l4", "v5.2.0",
                                    "v003", "v004", "v005", "v006", "v7"))
nasa_title <- nasa_title %>%
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>%
  anti_join(my_stopwords)

# What are the most common keywords?
nasa_keyword %>%
  group_by(keyword) %>%
  count(sort = TRUE)

# We likely want to change all of the keywords to either lower- or uppercase
# to get rid of duplicates like "OCEANS" and "Oceans". Let's do that here
nasa_keyword <- nasa_keyword %>%
  mutate(keyword = toupper(keyword))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Word Co-ocurrences and Correlations #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# - - - - - - - - - - - - - - - - - - - - - -#
# Networks of Description and Title Words #
# - - - - - - - - - - - - - - - - - - - - - -#

# We can use pairwise_count() from the widyr package to count how many times
# each pair of words occurs together in a title or description field.
library(widyr)

title_word_pairs <- nasa_title %>%
  pairwise_count(word, id, sort=TRUE, upper = FALSE)

title_word_pairs

# These are the pairs of words that occur together most often in title fields. Some of
# these words are obviously acronyms used within NASA, and we see how often words
# like "project" and "system" are used.

desc_word_pairs <- nasa_desc %>%
  pairwise_count(word, id, sort=TRUE, upper = FALSE)

desc_word_pairs

# These are the pairs of words that occur together most often in descripton fields.
# "Data" is a very common word in description fields; there is no shortage of data in the
# datasets at NASA!
# Let's plot networks of these co-occurring words so we can see these relationships better
# in Figure 8-1. We will again use the ggraph package for visualizing our networks.

library(ggplot2)
library(igraph)
library(ggraph)

# Figure - word network in NASA dataset titles
set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# We see some clear clustering in this network of title words; words in NASA dataset
# titles are largely organized into several families of words that tend to go together.

# What about the words from the description fields?

# Figure - Word network in NASA dataset descriptions
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# shows such strong connections between the top dozen or so words (words
# like "data," "global," "resolution," and "instrument") that we do not see a clear clustering
# structure in the network. We may want to use tf-idf (as described in detail in
# Chapter 3) as a metric to find characteristic words for each description field, instead
# of looking at counts of words.

# - - - - - - - - - - - - - - - - - - - - - -#
# Networks of Keywords #
# - - - - - - - - - - - - - - - - - - - - - -#
keywords_pairs <- nasa_keyword %>%
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

keywords_pairs

# Graph for network
set.seed(1234)
keywords_pairs %>%
  filter(n >= 700) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n),
                 edge_colour = "royalblue")+
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# To examine the relationships among keywords in a different way, we can find the correlation
# among the keywords as described in Chapter 4. This looks for those keywords
# that are more likely to occur together than with other keywords in a
# description field.

keyword_cors <- nasa_keyword %>%
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

keyword_cors

# Notice that these keywords at the top of this sorted data frame have correlation coefficients
# equal to 1; they always occur together. This means these are redundant keywords.
# It may not make sense to continue to use both of the keywords in these sets of
# pairs; instead, just one keyword could be used.
# Let's visualize the network of keyword correlations, just as we did for keyword cooccurences
set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
                 edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) + 
  theme_void()

# This network in Figure 8-4 appears much different than the co-occurence network.
# The difference is that the co-occurrence network asks a question about which keyword
# pairs occur most often, and the correlation network asks a question about
# which keywords occur more often together than with other keywords. Notice here
# the high number of small clusters of keywords; the network structure can be extracted
# (for further analysis) from the graph_from_data_frame() function above.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Calculating tf-idf for the Description Fields #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# The network graph in Figure 8-2 showed us that the description fields are dominated
# by a few common words like "data," "global," and "resolution"; this would be an excellent
# opportunity to use tf-idf as a statistic to find characteristic words for individual
# description fields. As discussed in Chapter 3, we can use tf-idf, the term frequency
# times inverse document frequency, to identify words that are especially important to a
# document within a collection of documents. Let's apply that approach to the description
# fields of these NASA datasets.

# - - - - - - - - - - - - - - - - - - - - - - - -#
# What is tf-idf for the description Field Words #
# - - - - - - - - - - - - - - - - - - - - - - - -#

# We will consider each description field a document, and the whole set of description
# fields the collection or corpus of documents. We have already used unnest_tokens()
# earlier in this chapter to make a tidy data frame of the words in the description fields,
# so now we can use bind_tf_idf() to calculate tf-idf for each word.

desc_tf_idf <- nasa_desc %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

# What are the Highest tf-idf words in the NASA description fields?

desc_tf_idf %>%
  arrange(-tf_idf) %>%
  select(-id)

# These are the most important words in the description fields as measured by tf-idf,
# meaning they are common but not too common.
# Notice we have run into an issue here; both n and term frequency
# are equal to 1 for these terms, meaning that these were description
# fields that only had a single word in them. If a description field
# only contains one word, the tf-idf algorithm will think that is a very
# important word.
# Depending on our analytic goals, it might be a good idea to throw out all description
# fields that have very few words.

# - - - - - - - - - - - - - - - - - - - - - - - -#
# Connecting Description Fields to Keywords #
# - - - - - - - - - - - - - - - - - - - - - - - -#

# We now know which words in the descriptions have high tf-idf, and we also have
# labels for these descriptions in the keywords. Let's do a full join of the keyword data
# frame and the data frame of description words with tf-idf, and then find the highest
# tf-idf words for a given keyword.

desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")

# Let's plot some of the most important words, as measured by tf-idf, for a few example
# keywords used on NASA datasets. First, let's use dplyr operations to filter for the keywords
# we want to examine and take just the top 15 words for each keyword. Then,
# let's plot those words in

# Figure - Distribution of tf-idf for words from datasets labeled with select keywords
desc_tf_idf %>%
  filter(!near(tf, 1)) %>%
  filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS",
                        "SEISMOLOGY", "ASTROPHYSICS",
                        "HUMAN HEALTH", "BUDGET")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")

# Using tf-idf has allowed us to identify important description words for each of these
# keywords. Datasets labeled with the keyword "SEISMOLOGY" have words like
# "earthquake," "risk," and "hazard" in their description, while those labeled with
# "HUMAN HEALTH" have descriptions characterized by words like "wellbeing," "vulnerability,"
# and "children." Most of the combinations of letters that are not English
# words are certainly acronyms (like OMB for the Office of Management and Budget),
# and the examples of years and numbers are important for these topics. The tf-idf statistic
# has identified the kinds of words it is intended to-important words for individual
# documents within a collection of documents.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Topic Modeling #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# Using tf-idf as a statistic has already given us insight into the content of NASA
# description fields, but let's try an additional approach to the question of what the
# NASA descriptions fields are about. We can use topic modeling, as described in
# Chapter 6, to model each document (description field) as a mixture of topics, and
# each topic as a mixture of words. As in earlier chapters, we will use latent Dirichlet
# allocation (LDA) for our topic modeling; there are other possible approaches for
# topic modeling.

# - - - - - - - - - - - - - - - - - - - - - - - -#
# What is tf-idf for the description Field Words #
# - - - - - - - - - - - - - - - - - - - - - - - -#

# To do the topic modeling as implemented here, we need to make a DocumentTermMa
# trix, a special kind of matrix from the tm package (of course, this is just a specific
# implementation of the general concept of a "document-term matrix"). Rows correspond
# to documents (description texts in our case), and columns correspond to
# terms (i.e., words); it is a sparse matrix and the values are word counts.
# Let's clean up the text a bit using stop words to remove some of the nonsense "words"
# left over from HTML or other character encoding. We can use bind_rows() to add
# our custom stop words to the list of default stop words from the tidytext package, and
# then use anti_join() to remove them all at once from our data frame.

my_stop_words <- bind_rows(stop_words,
                           data_frame(word = c("nbsp", "amp", "gt", "lt",
                                               "timesnewromanpsmt", "font",
                                               "td", "li", "br", "tr", "quot",
                                               "st", "img", "src", "strong", 
                                               "http", "file", "files",
                                               as.character(1:12)),
                                      lexicon = rep("custom", 30)))

word_counts <- nasa_desc %>%
  anti_join(my_stop_words) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

word_counts

# This is the information we need, the number of times each word is used in each document,
# to make a DocumentTermMatrix. We can cast() from our tidy text format to
# this nontidy format, as described in detail in Chapter 5

desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

desc_dtm

# We see that this dataset contains documents (each of them a NASA description field)
# and terms (words). Notice that this example document-term matrix is (very close to)
# 100% sparse, meaning that almost all of the entries in this matrix are zero. Each nonzero
# entry corresponds to a certain word appearing in a certain document.

# - - - - - - - - - - - - - - - - - - - - - - - -#
# Ready for Topic Modeling #
# - - - - - - - - - - - - - - - - - - - - - - - -#

# Now let's use the topicmodels package to create an LDA model. How many topics will
# we tell the algorithm to make? This is a question much like in k-means clustering; we
# don't really know ahead of time. We tried the following modeling procedure using 8,
# 16, 24, 32, and 64 topics; we found that at 24 topics, documents are still getting sorted
# into topics cleanly, but going much beyond that caused the distributions of ??, the
# probability that each document belongs in each topic, to look worrisome. We will
# show more details on this later.

library(topicmodels)

# be aware that running this model is time intensive
desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
desc_lda

# This is a stochastic algorithm that could have different results depending on where
# the algorithm starts, so we need to specify a seed for reproducibility as shown here.

# - - - - - - - - - - - - - - - - - - - - - - - -#
# Interpreting the Topic Model #
# - - - - - - - - - - - - - - - - - - - - - - - -#

# Now that we have built the model, let's tidy() the results of the model, i.e., construct
# a tidy data frame that summarizes the results of the model. The tidytext package
# includes a tidying method for LDA models from the topicmodels package.

tidy_lda <- tidy(desc_lda)
tidy_lda

# The column ?? tells us the probability of that term being generated from that topic for
# that document. It is the probability of that term (word) belonging to that topic.
# Notice that some of the values for ?? are very, very low, and some are not so low.
# What is each topic about? Let's examine the top 10 terms for each topic.

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# It is not very easy to interpret what the topics are about from a data frame like this, so
# let's look at this information visually in Figures 8-6 and 8-7.

# Figure - Top terms in topic modeling of NASA metadata description field texts
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"),
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")

# We can see what a dominant word "data" is in these description texts. In addition,
# there are meaningful differences between these collections of terms, from terms
# about soil, forests, and biomass in topic 12 to terms about design, systems, and technology
# in topic 21. The topic modeling process has identified groupings of terms that
# we can understand as human readers of these description fields.
# We just explored which words are associated with which topics. Next, let's examine
# which topics are associated with which description fields (i.e., documents). We will
# look at a different probability for this, ??, the probability that each document belongs
# in each topic, again using the tidy verb.

lda_gamma <- tidy(desc_lda, matrix = "gamma")
lda_gamma

# Notice that some of the probabilities visible at the top of the data frame are low and
# some are higher. Our model has assigned a probability to each description belonging
# to each of the topics we constructed from the sets of words. How are the probabilities
# distributed? Let's visualize them (Figure 8-8).

ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

# First notice that the y-axis is plotted on a log scale; otherwise it is difficult to make
# out any detail in the plot. Next, notice that ?? runs from 0 to 1; remember that this is
# the probability that a given document belongs in a given topic. There are many values
# near zero, which means there are many documents that do not belong in each topic.
# Also, there are many values near ?? = 1; these are the documents that do belong in
# those topics. This distribution shows that documents are being well discriminated as
# belonging to a topic or not. We can also look at how the probabilities are distributed
# within each topic, as shown in Figure 8-9.

# Figure - probability distribution for each topic in topic modeling of NASA metadata description field texts
ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

# Let's look specifically at topic 18 in Figure 8-9, a topic that had documents cleanly
# sorted in and out of it. There are many documents with ?? close to 1; these are the
# documents that do belong to topic 18 according to the model. There are also many
# documents with ?? close to 0; these are the documents that do not belong to topic 18.
# Each document appears in each panel in this plot, and its ?? for that topic tells us that
# document's probability of belonging in that topic.
# This plot displays the type of information we used to choose the number of topics for
# our topic modeling procedure. When we tried options higher than 24 (such as 32 or
# 64), the distributions for ?? started to look very flat toward ?? = 1; documents were not
# getting sorted into topics very well.

# - - - - - - - - - - - - - - - - - - - - - - - -#
# Connecting Topic Modeling with Keywords #
# - - - - - - - - - - - - - - - - - - - - - - - -#

# Let's connect these topic models with the keywords and see what relationships we can
# find. We can full_join() this to the human-tagged keywords and discover which
# keywords are associated with which topic.

lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c("document", "id"))
lda_gamma

# Now we can use filter() to keep only the document-topic entries that have probabilities
# (??) greater than some cutoff value; let's use 0.9.

top_keywords <- lda_gamma %>%
  filter(gamma >= 0.9) %>%
  count(topic, keyword, sort = TRUE)

top_keywords

# What are the top keywords for each topic (Figure 8-10)?
top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, keyword) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(keyword = factor(paste(keyword, topic, sep = "__"),
                          levels = rev(paste(keyword, topic, sep = "__")))) %>%
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~topic, ncol = 3, scales = "free")

# Let's take a step back and remind ourselves what Figure 8-10 is telling us. NASA datasets
# are tagged with keywords by human beings, and we have built an LDA topic
# model (with 24 topics) for the description fields of the NASA datasets. This plot
# answers the question, "For the datasets with description fields that have a high probability
# of belonging to a given topic, what are the most common human-assigned keywords?"
# It's interesting that the keywords for topics 13, 16, and 18 are essentially duplicates of
# each other ("OCEAN COLOR," "OCEAN OPTICS," "OCEANS"), because the top
# words in those topics do exhibit meaningful differences, as shown in Figures 8-6 and
# 8-7. Also note that by number of documents, the combination of 13, 16, and 18 is
# quite a large percentage of the total number of datasets represented in this plot, and
# even more if we were to include topic 11. By number, there are many datasets at
# NASA that deal with oceans, ocean color, and ocean optics. We see "PROJECT COMPLETED"
# in topics 9, 10, and 21, along with the names of NASA laboratories and
# research centers. Other important subject areas that stand out are groups of keywords
# about atmospheric science, budget/finance, and population/human dimensions. We
# can go back to Figures 8-6 and 8-7 on terms and topics to see which words in the
# description fields are driving datasets being assigned to these topics. For example,
# topic 4 is associated with keywords about population and human dimensions, and
# some of the top terms for that topic are "population," "international," "center," and
# "university."