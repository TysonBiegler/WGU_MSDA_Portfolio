#####################################
# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 2
#####################################

################################
#INITIAL SETUP
################################

install_tensorflow(envname = "r-tensorflow")

#installing libraries
library(tidyverse)
library(tokenizers)
library(tm)
library(stringi)
library(keras)
library(tensorflow)
library(tidytext)
library(word2vec)

#set working dir
setwd("~/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 2/Raw/data")

yelp_df <- data.frame()
imdb_df <- data.frame()
amazon_df <- data.frame()
yelp_data <- readLines("yelp_labelled.txt", encoding = "UTF-8")
imdb_data <- readLines("imdb_labelled.txt", encoding = "UTF-8")
amazon_data <- readLines("amazon_cells_labelled.txt", encoding = "UTF-8")

for (line in yelp_data) {
  split_data <- unlist(strsplit(line, "\t"))
  yelp_df <- rbind(yelp_df, data.frame(split_data[1], as.integer(split_data[2])))
}
# Clean up for next run
line=""
split_data=""

for (line in imdb_data) {
  split_data <- unlist(strsplit(line, "\t"))
  imdb_df <- rbind(imdb_df, data.frame(split_data[1], as.integer(split_data[2])))
}
# Clean up for next run
line=""
split_data=""

for (line in amazon_data) {
  split_data <- unlist(strsplit(line, "\t"))
  amazon_df <- rbind(amazon_df, data.frame(split_data[1], as.integer(split_data[2])))
}

# Clean up as no longer needed
rm(line)
rm(split_data)

merged_df <- rbind(amazon_df, imdb_df, yelp_df)

glimpse(merged_df)

colnames(merged_df) <- c("text", "sentiment")

################################
#EDA
################################
#sum(duplicated(merged_df)) #checking for duplicates
#merged_df <- merged_df[!duplicated(merged_df), ] #removing duplicates
#sum(duplicated(merged_df)) #checking for duplicates

colSums(is.na(merged_df)) #checking for missing values

str(merged_df)

head(merged_df)

################################
#CLEANING
################################ 
#checking for non english characters  SOURCE: https://ss64.com/ascii.html
count_non_english_chars <- function(text) {
  chars <- utf8ToInt(text)
  non_english_chars <- chars[chars < 32 | chars > 126]  
  return(length(non_english_chars))
}

non_english <- sum(sapply(merged_df$text, count_non_english_chars))
cat("Total non-English characters:", non_english, "\n")

clean_text <- function(text) {
  text <- tolower(text) # converting to lowercase
  text <- removeWords(text, stopwords("en")) # removing English stopwords
  text <- gsub("[[:punct:]]", "", text) # removing punctuation
  text <- stri_trans_general(text, "Latin-ASCII") # normalizing to ASCII
  text <- iconv(text, "latin1", "ASCII", sub = "") # removing non-ASCII characters like emojis
  text <- gsub("[^\x01-\x7F]", "", text) # removing non-ASCII characters
  text <- gsub("\\b\\d+\\b", "", text) # remove numbers
  text <- stripWhitespace(text) # removing extra whitespace
  text <- trimws(text) # trimming leading and trailing whitespace
  return(text)
}

# Apply the function to the "text" column
merged_df$text <- sapply(merged_df$text, clean_text)

head(merged_df$text)

#confirming that non english characters are gone
non_english <- sum(sapply(merged_df$text, count_non_english_chars))
cat("Total non-English characters:", non_english, "\n")

################################
#Tokenization
################################
tokenized <- merged_df %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text, to_lower = FALSE)

tokenized <- tokenized %>%
  filter(word != "", !is.na(word))

vocabulary_size <- tokenized %>%
  distinct(word) %>%
  nrow()

cat("Vocabulary Size:", vocabulary_size, "\n") #should be 256 accorind to sewell, whatever that means.... 

#top 20 tokens
top_tokens <- merged_df %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text, to_lower = FALSE) %>%
  filter(word != "", !is.na(word)) %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20)

#plot of the top tokens
ggplot(top_tokens, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Token Frequencies",
       x = "Token", y = "Frequency") +
  theme_minimal()

################################
#Sentence length, and Embedding length
################################
sentence_lengths <- sapply(strsplit(merged_df$text, "\\s+"), length)
summary(sentence_lengths)

hist(sentence_lengths,
     main = "Distribution of sentence lengths",
     xlab = "Sentence length",
     ylab = "Frequency")

index <- which.max(sentence_lengths)
longest_sentence <- merged_df$text[index]
token_length <- sentence_lengths[index]
token_length

cat("Longest sentence:",longest_sentence, '\nLength:', token_length, "tokens.")

#embedding length
embedding <- as.integer(round(sqrt((vocabulary_size)),0))

input_length <- max(sentence_lengths)

cat("Estimated word embeding length:", embedding, "\nProposed word embedding lenght:", input_length)

################################
#Tokenize and pad
################################

tokenizer <- text_tokenizer(num_words = vocabulary_size)
tokenizer$fit_on_texts(merged_df$text)
sequences <- texts_to_sequences(tokenizer, merged_df$text)
word_index <- tokenizer$word_index

#pad
padded_sequence <- padded_sequences <- pad_sequences(sequences, maxlen = sentence_lengths[index], padding = "post", truncating = "post")



