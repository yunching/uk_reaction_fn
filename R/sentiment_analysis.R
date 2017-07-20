library(tidytext)
library(tidyverse)
library(janeaustenr)
library(dplyr)
library(stringr)
library(pdftools)
library(magrittr)

data(stop_words)

tmp <- pdf_text(file.path(".","data","20170620_mark_carney.pdf"))
text_df <- data_frame(page = 1:length(tmp), text = tmp)
text_df %<>%
  unnest_tokens(word, text)

text_df %<>%
  anti_join(stop_words)
text_df %>%
  count(word, sort = TRUE) 

text_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

mark_carney_sentiment <- 
text_df %>%
  inner_join(get_sentiments("bing")) 
%>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
