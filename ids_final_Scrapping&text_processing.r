install.packages("rvest")
install.packages("httr")
install.packages("dplyr")
install.packages("stringr")
install.packages("xml2")
install.packages("tokenizers")
install.packages("stopwords")
install.packages("SnowballC")
install.packages("textstem")
install.packages("readr")
install.packages("hunspell")
install.packages("tidytext")

library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(xml2)
library(tokenizers)
library(stopwords)
library(SnowballC)
library(textstem)
library(readr)
library(hunspell)
library(tidytext)

categories <- c("technology", "science", "world", "business", "health")
all_news <- data.frame()

for (category in categories) {
  base_url <- paste0("https://www.npr.org/sections/", category, "/")
  titles_cat <- c(); descriptions_cat <- c(); dates_cat <- c()
  
  for (i in 1:5) {
    full_url <- paste0(base_url, "?page=", i)
    page <- read_html(GET(full_url))
    
    titles <- page %>% html_nodes(".item-info h2.title a") %>% html_text(trim = TRUE)
    descriptions <- page %>% html_nodes(".item-info p.teaser a") %>% html_text(trim = TRUE)
    dates <- page %>% html_nodes(".item-info .teaser time") %>% html_attr("datetime")
    
    titles_cat <- c(titles_cat, titles)
    descriptions_cat <- c(descriptions_cat, descriptions)
    dates_cat <- c(dates_cat, dates)
    Sys.sleep(1)
  }
  
  category_data <- data.frame(
    Title = titles_cat,
    Description = descriptions_cat,
    Date = dates_cat,
    Category = rep(category, length(titles_cat)),
    stringsAsFactors = FALSE
  )
  
  all_news <- bind_rows(all_news, head(category_data, 100))
  cat("Scraped 100 articles for:", category, "\n")
}
write.csv(all_news, "D:/Wafi(Spring-25)/Data Science/final/npr_Updated_Dataset.csv", row.names = FALSE)
View(all_news)





contractions <- c(
  "don't" = "do not", "doesn't" = "does not", "didn't" = "did not",
  "can't" = "cannot", "won't" = "will not", "wouldn't" = "would not",
  "isn't" = "is not", "aren't" = "are not", "wasn't" = "was not",
  "weren't" = "were not", "haven't" = "have not", "hasn't" = "has not",
  "hadn't" = "had not", "i'm" = "i am", "we're" = "we are", "they're" = "they are",
  "you're" = "you are", "it's" = "it is", "that's" = "that is", "there's" = "there is",
  "what's" = "what is", "who's" = "who is", "she's" = "she is", "he's" = "he is",
  "i've" = "i have", "you've" = "you have", "they've" = "they have",
  "we've" = "we have", "i'll" = "i will", "you'll" = "you will", "they'll" = "they will",
  "we'll" = "we will", "shouldn't" = "should not", "couldn't" = "could not",
  "mustn't" = "must not", "needn't" = "need not", "mightn't" = "might not",
  "shan't" = "shall not", "let's" = "let us", "who'll" = "who will", "how's" = "how is"
)

expand_contractions <- function(text) {
  for (con in names(contractions)) {
    text <- gsub(paste0("\\b", con, "\\b"), contractions[con], text, ignore.case = TRUE)
  }
  return(text)
}

handle_emojis_emoticons <- function(text) {
  emoji_pattern <- "[\\U0001F600-\\U0001F64F\\U0001F300-\\U0001F5FF\\U0001F680-\\U0001F6FF\\U0001F700-\\U0001F77F\\U0001F900-\\U0001F9FF]+"
  emoticon_pattern <- ":\\)|:-\\)|:\\(|:-\\(|;\\)|;-\\)|:D|:-D|XD|<3"
  text <- str_replace_all(text, emoji_pattern, " emoji ")
  text <- str_replace_all(text, emoticon_pattern, " emoticon ")
  return(text)
}


clean_text <- function(text) {
  text <- handle_emojis_emoticons(text)    
  text <- tolower(text)
  text <- expand_contractions(text)
  text <- gsub("[0-9]+", "", text)
  text <- gsub("[[:punct:]]", "", text)
  text <- gsub("[^a-zA-Z\\s]", " ", text)
  text <- read_html(paste0("<body>", text, "</body>")) %>% html_text(trim = TRUE)
  text <- str_squish(text)
  return(text)
}

df <- read.csv("D:/Wafi(Spring-25)/Data Science/final/npr_Updated_Dataset.csv", stringsAsFactors = FALSE)
df$Cleaned_Title <- sapply(df$Title, clean_text)
df$Cleaned_Description <- sapply(df$Description, clean_text)






spell_check_text <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) return(NA)
  tokens <- unlist(strsplit(text, "\\s+"))
  corrected <- sapply(tokens, function(word) {
    if (!hunspell_check(word)) {
      suggestions <- hunspell_suggest(word)[[1]]
      valid <- suggestions[!grepl("[-\\s]", suggestions)]
      if (length(valid) > 0) return(tolower(valid[1]))
      return("")
    }
    return(tolower(word))
  })
  corrected <- corrected[corrected != ""]
  return(paste(corrected, collapse = " "))
}

df$Spellchecked_Title <- sapply(df$Cleaned_Title, spell_check_text)
df$Spellchecked_Description <- sapply(df$Cleaned_Description, spell_check_text)
write.csv(df, "D:/Wafi(Spring-25)/Data Science/final/npr_cleaned_spellchecked.csv", row.names = FALSE)
View(df)






format_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) return(NA)
  sentence_tokens <- tokenize_sentences(text)[[1]]
  formatted_sentences <- sapply(sentence_tokens, function(sentence) {
    word_tokens <- tokenize_words(sentence)[[1]]
    paste0("[", paste0("'", word_tokens, "'", collapse = ", "), "]")
  })
  return(paste(formatted_sentences, collapse = " "))
}

df <- read_csv("D:/Wafi(Spring-25)/Data Science/final/npr_cleaned_spellchecked.csv")
df_tokenized <- df %>%
  mutate(
    tokenized_title = sapply(Spellchecked_Title, format_tokens),
    tokenized_description = sapply(Spellchecked_Description, format_tokens)
  )
write_csv(df_tokenized, "D:/Wafi(Spring-25)/Data Science/final/npr_tokenized_fancy_format.csv")
View(df_tokenized)





process_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) return(NA)
  text <- gsub("\\[|\\]|'", "", text)
  tokens <- unlist(strsplit(text, ",\\s*"))
  filtered_tokens <- tokens[!tolower(tokens) %in% stopwords("en")]
  paste0("[", paste0("'", filtered_tokens, "'", collapse = ", "), "]")
}

data <- read_csv("D:/Wafi(Spring-25)/Data Science/final/npr_tokenized_fancy_format.csv")
data_filtered <- data %>%
  mutate(
    title_no_stopwords = sapply(tokenized_title, process_tokens),
    description_no_stopwords = sapply(tokenized_description, process_tokens)
  )
write_csv(data_filtered, "/Users/mithizaman/Documents/12/Introduction to data science/code/final/npr_stopwords_removed_fancy_format.csv")

View(data_filtered)







data <- read_csv("/Users/mithizaman/Documents/12/Introduction to data science/code/final/npr_stopwords_removed_fancy_format.csv")

clean_and_split <- function(text) {
  cleaned <- gsub("\\[|\\]|'", "", text)
  tokens <- unlist(strsplit(cleaned, ",\\s*"))
  tokens <- tokens[!tolower(tokens) %in% stopwords("en")]
  return(tokens)
}

data_stemmed <- data %>%
  mutate(
    title_stem_only = sapply(title_no_stopwords, function(text) {
      tokens <- clean_and_split(text)
      stemmed <- wordStem(tokens, language = "en")
      paste0("[", paste0("'", stemmed, "'", collapse = ", "), "]")
    }),
    description_stem_only = sapply(description_no_stopwords, function(text) {
      tokens <- clean_and_split(text)
      stemmed <- wordStem(tokens, language = "en")
      paste0("[", paste0("'", stemmed, "'", collapse = ", "), "]")
    })
  )



data_lemma_only <- data_stemmed %>%
  mutate(
    title_lemma_only = sapply(title_no_stopwords, function(text) {
      tokens <- clean_and_split(text)
      lemmatized <- lemmatize_words(tokens)
      paste0("[", paste0("'", lemmatized, "'", collapse = ", "), "]")
    }),
    description_lemma_only = sapply(description_no_stopwords, function(text) {
      tokens <- clean_and_split(text)
      lemmatized <- lemmatize_words(tokens)
      paste0("[", paste0("'", lemmatized, "'", collapse = ", "), "]")
    })
  )

write_csv(data_lemma_only, "/Users/mithizaman/Documents/12/Introduction to data science/code/final/npr_stem_lemma_separate.csv")

View(data_lemma_only)


