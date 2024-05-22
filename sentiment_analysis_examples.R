install.packages("tidyverse")
install.packages("syuzhet")
install.packages("tidytext")

library(tidyverse)
library(syuzhet)
library(tidytext)

# import text dataset
df <- read.csv("reviews.csv")
text.df <- tibble(text = str_to_lower(df$Review))

# analyze sentiments with syuzhet package based on "NRC" sentiment dictionary
# NRC lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust
emotions <- get_nrc_sentiment(text.df$text)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))

# create barplot showing counts for each of eight different emotions and positive/negative rating
ggplot(emo_sum, aes(x = reorder(emotion, count), y = count)) + geom_bar(stat = 'identity')

# sentiment analysis with tidytext package using "bing" 
# bing lexicon categorizes words in a binary fashion into positive and negative categories
bing_word_counts <- text.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE)

# viewing top ten words, filtered for positive
bing_top_ten <- bing_word_counts %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive") %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

# bar plotting bing analysis
bing_top_ten %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend=FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to Sentiment", x = NULL) + 
  coord_flip()
