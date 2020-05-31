##################################################
     ### CHAPTER I: THE TIDY TEXT FORMAT ###
##################################################

text = c("Because I could not stop for Death -",
         "He kindly stopped for me-",
         "The Carriage held but just Ourselves -",
         "and Immortality")
text

# Creamos un archivo dataframe
library(dplyr)
text_df <- data_frame(line=1:4, text = text)
text_df

# Tokenizamos
library(tidytext)
text_df %>% unnest_tokens(word, text)


# - - - - - - - - - - - - - - - - - - - - - - - - #
      # Tidying the Works of Jane Austen #
# - - - - - - - - - - - - - - - - - - - - - - - - #
install.packages("janeaustenr")
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>% group_by(book) %>%
                  mutate(linenumber = row_number(),
                         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                                 ignore_case = TRUE)))) %>%
                  ungroup()
original_books

# one-token-per-row
library(tidytext)
tidy_books <- original_books %>% unnest_tokens(word, text)

tidy_books

# primero quitaremos los stopwords
data(stop_words)
tidy_books <- tidy_books %>% anti_join(stop_words)

# We can also use dplyr's count() to find the most common words in all the books
# as a whole
tidy_books %>% count(word, sort = TRUE)

# Because we've been using tidy tools, our word counts are stored in a tidy data
# frame. This allows us to pipe directly to the ggplo2 package, for example to create
# a visualization of the most common words
library(ggplot2)

tidy_books %>% 
  count(word, sort=TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# - - - - - - - - - - - - - - - - - - - - - - - - #
             # The Gutemberg Package #
# - - - - - - - - - - - - - - - - - - - - - - - - #

# WORD FREQUENCIES
# descargaremos la obras de H.G. Wells los siguientes: The time machine, The War of
# the Worlds, The Invisible Man, and The Island of Doctor Moreau. We can access these
# works using gutenberg_download() and the Project Gutenberg ID numbers for each novel.
install.packages("gutenbergr")
library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# just for kicks, what are the most common words in these novels of H.G. Wells?
tidy_hgwells %>%
  count(word, sort = TRUE)

# Download the works of Bronte sisters. Let's get Jane Eyre, Wuthering Heights,
# The Tenant of Wildfell Hall, Villette and Agnes Grey.
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# What are the most common words in these novels of the Bronte sisters?
tidy_bronte %>%
  count(word, sort = TRUE)

# Now, let's calculate the frequency for each word in the works of Jane Austen, the
# Bronte sisters, and H.G. Wells by binding the data frames together. We can use 
# spread and gather from tidyr to reshape our data frame so that it is just what
# we need for plotting and comparing the three sets of novels. 
library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, 'Brontë Sisters':'H.G. Wells')

# NOW LET'S PLOT 
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") + 
  labs(y = "Jane Austen", x = NULL)

# Let's quantify how similar and different these sets of word frequencies are
# using a correlation test. How correlated are the word frequencies between Austen
# and the Bronte sisters, and between Austen and Wells?
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)
