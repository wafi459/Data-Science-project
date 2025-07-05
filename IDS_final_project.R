# 📦 Load libraries
install.packages("tm")
install.packages("topicmodels")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("reshape2")

library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(readr)

lemma_df <- read_csv("D:/Datascience/npr_stem_lemma_separate.csv")

clean_column <- function(text) {
  text <- gsub("\\[|\\]|'", "", text)
  tolower(text)
}

lemma_df <- lemma_df %>%
  mutate(clean_description_lemma = sapply(description_lemma_only, clean_column))

write_csv(lemma_df, "D:/Datascience/npr_lemma_cleaned_final.csv")
View(lemma_df)

lemma_df <- read_csv("D:/Datascience/npr_lemma_cleaned_final.csv")

corpus <- Corpus(VectorSource(lemma_df$clean_description_lemma))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

dtm <- DocumentTermMatrix(corpus)
dtm_sparse <- removeSparseTerms(dtm, 0.99)

# Save DTM + original data (optional)
dtm_matrix <- as.matrix(dtm_sparse)
dtm_df <- as.data.frame(dtm_matrix)
lemma_dtm_combined <- bind_cols(lemma_df, dtm_df)
write_csv(lemma_dtm_combined, "D:/Datascience/lemma_dtm_combined_final.csv")
View(lemma_dtm_combined)

set.seed(1234)
num_topics <- 5
lda_model <- LDA(dtm_sparse, k = num_topics, control = list(seed = 1234))

# 🧠 Step 5: Top 10 terms per topic (tidytext + ggplot)
topic_terms <- tidy(lda_model, matrix = "beta")

top_terms <- topic_terms %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

write.csv(top_terms, "D:/Datascience/lda_top_terms.csv", row.names = FALSE)
View(top_terms)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms in LDA Topics",
    x = "Term", y = "Probability"
  )

# 🧾 Step 7: Matrix-style top 10 words per topic
top_words_matrix <- terms(lda_model, 10)
top_words_df <- as.data.frame(top_words_matrix)
colnames(top_words_df) <- paste0("Topic ", 1:ncol(top_words_df))
top_words_df <- cbind(Rank = 1:10, top_words_df)

write.csv(top_words_df, "D:/Datascience/lda_top_words_matrix.csv", row.names = FALSE)
View(top_words_df)

doc_topics <- tidy(lda_model, matrix = "gamma")
doc_topics$document <- as.integer(doc_topics$document)

doc_topic_matrix <- doc_topics %>%
  pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "Topic_"
  ) %>%
  arrange(document) %>%
  rename(Document_ID = document)

lemma_df_with_id <- lemma_df %>%
  mutate(Document_ID = row_number())

doc_topic_matrix_full <- doc_topic_matrix %>%
  left_join(lemma_df_with_id, by = "Document_ID")

write_csv(doc_topic_matrix, "D:/Datascience/lda_document_topic_distribution.csv")
View(doc_topic_matrix)
