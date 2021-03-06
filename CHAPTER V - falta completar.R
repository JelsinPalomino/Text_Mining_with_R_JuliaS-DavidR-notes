########################################################################
        ### CHAPTER V: CONVERTING TO AND FROM NOTIDY FORMATS ###
########################################################################

# - - - - - - - - - - - - - - - - - - - - - - - #
     # Tidying DocumentTermMatrix Objetcs #
# - - - - - - - - - - - - - - - - - - - - - - - #

# install.packages("tm")
library(tm)
# install.packages("topicmodels")

data("AssociatedPress", package = "topicmodels")

AssociatedPress

terms <- Terms(AssociatedPress)
head(terms)

# 
library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
ap_td

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt=count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill=sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

# - - - - - - - - - - - - - - - - - - - - - - - #
             # Tidyng dfm Objects #
# - - - - - - - - - - - - - - - - - - - - - - - #

library(methods)

data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm


inaug_td <- tidy(inaug_dfm)
inaug_td


inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf


library(tidyr)

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill=list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))


year_term_counts %>%
  filter(term %in% c("god", "america", "foreign",
                     "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")

# - - - - - - - - - - - - - - - - - - - - - - - #
    # Casting Tidy Text Data into a Matrix #
# - - - - - - - - - - - - - - - - - - - - - - - #
ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(term, document, count)

library(Matrix)
# cast into a matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
dim(m)

library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm

# - - - - - - - - - - - - - - - - - - - - - - - #
    # Tidying Corpus Objects with Metadata #
# - - - - - - - - - - - - - - - - - - - - - - - #
data("acq")
acq

# First document
acq[[1]]

acq_td <- tidy(acq)
acq_td


acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)

# tf_idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

# - - - - - - - - - - - - - - - - - - - - - - - #
     # EXAMPLE: Mining Financial Articles #
# - - - - - - - - - - - - - - - - - - - - - - - #
# install.packages("tm.plugin.webmining")
library(tm.plugin.webmining)
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", 
            "YHOO", "NFLX")

download_articles <- function(symbol){
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))

stock_articles
head(stock_articles)

stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading)


stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading )

stock_tokens

library(rlang)
rlang::last_error()
