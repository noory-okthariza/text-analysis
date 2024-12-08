# load libraries
library(readtext)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(textclean)
library(ggplot2)

democrat <- readtext("democrat.docx")


# Clean and split text into paragraphs
democrat_paragraphs <- democrat %>%
  unnest_tokens(paragraph, text, token = "regex", pattern = "\n") %>% # Split by newlines
  filter(paragraph != "") %>% # Remove empty paragraphs
  mutate(doc_id = row_number()) # Assign new doc IDs


# Create a DTM
dtm <- democrat_paragraphs %>%
  unnest_tokens(word, paragraph) %>% # Tokenize words
  anti_join(stop_words, by = "word") %>% # Remove stop words
  count(doc_id, word, sort = TRUE) %>% # Count words per document
  cast_dtm(document = doc_id, term = word, value = n) # Create DTM


# Fit LDA model
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Get the top terms for each topic
top_terms <- terms(lda_model, 10) # Top 10 terms per topic
print(top_terms)

# Get the topic distribution for each document
doc_topics <- posterior(lda_model)$topics
print(doc_topics)

# Find the dominant topic for each document
dominant_topic <- apply(doc_topics, 1, which.max)

# Count the documents associated with each topic
topic_counts <- table(dominant_topic)
print(topic_counts)




# Extract the word-topic probabilities
topic_word_probs <- tidy(lda_model, matrix = "beta")

# Identify the top 10 words for each topic
top_words <- topic_word_probs %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Generate the bar plot
top_words %>%
  ggplot(aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Words for Each Topic",
    x = "Words",
    y = "Probability"
  ) +
  theme_minimal()

