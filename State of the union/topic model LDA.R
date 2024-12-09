# Load libraries
library(readtext)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)

# Read and clean text
democrat <- readtext("democrat.docx")

# Clean and split text into paragraphs
democrat_paragraphs <- democrat %>%
  unnest_tokens(paragraph, text, token = "regex", pattern = "\n") %>%
  filter(paragraph != "")

# Create a Document-Term Matrix (DTM)
dtm <- democrat_paragraphs %>%
  unnest_tokens(word, paragraph) %>%
  anti_join(stop_words, by = "word") %>%
  count(row_number(), word) %>% # Use row_number() directly for doc_id
  cast_dtm(document = row_number(), term = word, value = n)

# Fit LDA model
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Get the top terms for each topic
top_terms <- terms(lda_model, 10)
print(top_terms)

# Extract the word-topic probabilities and plot top words
tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
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






#2. Dominant topic
# Find the dominant topic for each paragraph
doc_topics <- posterior(lda_model)$topics
dominant_topic <- apply(doc_topics, 1, which.max)

# Count the number of paragraphs associated with each topic
topic_counts <- table(dominant_topic)

# Convert the table to a data frame
topic_counts_df <- as.data.frame(topic_counts)
colnames(topic_counts_df) <- c("Topic", "Frequency") # Rename columns

# Visualize the topic dominance
topic_counts_df %>%
  ggplot(aes(x = as.factor(Topic), y = Frequency, fill = as.factor(Topic))) + # Convert Topic to factor
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Dominant Topics Across Documents",
    x = "Topic",
    y = "Number of Documents"
  ) +
  theme_minimal()



# 3. Strenght of association by topic 
# Calculate maximum association score for each paragraph
max_topic_association <- apply(doc_topics, 1, max)

# Add document-level information
association_df <- data.frame(
  doc_id = 1:nrow(doc_topics),
  dominant_topic = dominant_topic,
  max_association = max_topic_association
)

# Visualize the association strength
association_df %>%
  ggplot(aes(x = factor(dominant_topic), y = max_association, fill = factor(dominant_topic))) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Strength of Topic Association by Dominant Topic",
    x = "Dominant Topic",
    y = "Maximum Association Score"
  ) +
  theme_minimal()




#4. Topic co-occurance
# Calculate pairwise topic co-occurrence probabilities
co_occurrence <- cor(doc_topics)

# Visualize the co-occurrence matrix
library(reshape2)
melt(co_occurrence) %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(
    title = "Topic Co-occurrence Matrix",
    x = "Topic",
    y = "Topic",
    fill = "Correlation"
  ) +
  theme_minimal()

