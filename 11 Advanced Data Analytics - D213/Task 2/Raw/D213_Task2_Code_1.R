#####################################
# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 2
#####################################
# https://www.youtube.com/watch?v=G3y34PmbrHg&list=PLVp79EU78TtweD-Si75OSXi51pxp26rlF&index=2
# https://www.youtube.com/watch?v=otoXeVPhT7Q&list=LL&index=3&t=1051s

#####################################
# Initial setup 
#####################################

library(tensorflow)
library(tidytext)
library(tidyverse)



#set working dir
setwd("~/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 2/Raw")

# Read the text files
#amazon_data <- read.table("amazon_cells_labelled.txt", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
#imdb_data <- read.table("imdb_labelled.txt", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
#yelp_data <- read.table("yelp_labelled.txt", sep = "\t", header = FALSE, stringsAsFactors = FALSE)


#corpus <- rbind(amazon_data, imdb_data, yelp_data)


#Using base R and tm
library(tm)

data <- Corpus(DirSource(directory = "data",
                         pattern = "*.txt")) |>
  DocumentTermMatrix(control = list(tolower = TRUE,
                                    removePunctuation = TRUE,
                                    stopwords = TRUE,
                                    removeNumbers = TRUE)) |>
  removeSparseTerms(sparse = .1) |> 
  as.matrix() |>
  colSums() |>
  sort(decreasing = TRUE) |>
  head(n=10)

data

#Using tidytext
tidydata <- list.files(path = "data", pattern = "*.txt", #getting text files from the data directory
           full.names = TRUE) %>% 
  map_df(~ data_frame(txt = read_file(.x)))%>% #reading in all the files in the data directory
  unnest_tokens(word, txt) %>% #break documents into individual words (1 word per row), and removes punctuation
  filter(is.na(as.numeric(word))) %>% #remove all numbers
  anti_join(stop_words) %>% #removes all stopworkds
  count(word, sort = TRUE) #gets the count of all unique words

tidydata


lemma_dictionary <- make_lemma_dictionary(tidydata, engine = 'hunspell')

lemma_dictionary

lemmatize_strings(tidydata, dictionary = lemma_dictionary)

tidydata






#use stemming or lemmatization
  #stemming returns the root of the word where lemmatization returns the complete word. 
  #stemming is less readable by humans but speeds up the process

#tokenize


#vectorize
  #converting the words into numeric values

#add padding whereever
  #sentences need to be the same length

#split the dataset
  #

#save the dataset

#build, complie,summary(), fit model
# ONLY NEED 2 LAYERS. INPUT AND OPTPUT

#generate the history graphs showing model accuracy and loss
#

#evaluate the model
#

#model.save, make recomnendations, include sources. 
#I RECOMEND THIS MODEL FOR PRODUCTINO BECAUSE THIS MODEL ACCURACY IS GREATER THAN 70%

#NOTES 

#sentences are rows

#hyper parameters
  #sets epochs to 20
  #patience should be set between 1 and 5
  #validation split is usually same size as test size
  #history, model.fit is the name of the model for ex., model.history.history to plot the accuracy and loss curves

#graphing ephocs (train test and validation) 70% modele accuracy or better is good enough to turn in

#CNN - convolutional neural network
  #
#RNN - recurrent network
  #
#ANN - artificial neural network ***Maybe use this one***
  #


#scalar -> vector -> matrix -> tensor 

data %>% 
  unnest_tokens(output = "word",
                token = "words",
                input = text_column) %>% 
  count(word, sort = TRUE)


