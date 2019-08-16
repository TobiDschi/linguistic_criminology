#calculation of emotive measures

#get data
filepath <- "" #indicate .csv file containing material to be analyzed
comparison_data <- read.csv(filepath)

#clean the text
library(stringr)
library(purrr)

#Nice and Varcelona (manually handeled encoding problems between datasets)
comparison_data$text <- str_replace_all(comparison_data$text, "¡", "í")
comparison_data$text <- str_replace_all(comparison_data$text, "¢", "ó")
comparison_data$text <- str_replace_all(comparison_data$text, "£", "ú")
comparison_data$text <- str_replace_all(comparison_data$text, "¤", "ñ")
comparison_data$text <- str_replace_all(comparison_data$text, "é", "Ú")
comparison_data$text <- str_replace_all(comparison_data$text, ",", "é")
comparison_data$text <- str_replace_all(comparison_data$text, "¥", "Ñ")
comparison_data$text <- str_replace_all(comparison_data$text, "à", "Ó")

#Munich
comparison_data$text <- str_replace_all(comparison_data$text, "Ã©", "é") 
comparison_data$text <- str_replace_all(comparison_data$text, "Ãº", "ú") 
comparison_data$text <- str_replace_all(comparison_data$text, "Ã³", "ó")
comparison_data$text <- str_replace_all(comparison_data$text, "Ã¡", "á")
comparison_data$text <- str_replace_all(comparison_data$text, "Ã±", "ñ")
comparison_data$text <- str_replace_all(comparison_data$text, "Ã", "í")

library(tm)

comparison_data$text <- comparison_data$text %>%
  tolower() %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  removePunctuation() %>%
  removeWords(stopwords("spanish")) %>%
  stemDocument(language = "spanish")

# extract emotional value for analysis

library(purrr)
library(stringr)

cube <- function(x = NULL){
  x^3
}

cube_root <- function(x = NULL){ 
  x^(1/3)
}

emotion_values <- function(emotion){
  
  apply_emotion_index <- function(x = NULL, column = emotion){
    emotional_norms[,column][x == TRUE]
  }
  
  override_emotion <- function(value, position, column = emotion){
    comparison_data[,column][position] <<- value 
  }
  
  for(i in 1:nrow(comparison_data)){
    map(comparison_data$text[i], str_detect, as.character(emotional_norms$Word)) %>%
      unlist() %>%
      apply_emotion_index() %>%
      cube() %>% 
      mean() %>%
      cube_root() %>% 
      override_emotion(i)
  }
}


normspath = "" #indicate the file containing the emotive norms of reference

emotional_norms <- read.csv("")
emotional_norms$Word <- as.character(emotional_norms$Word)
emotional_norms$Word <- stemDocument(emotional_norms$Word, language = "spanish")
comparison_data$Valence_Mean <- 0
comparison_data$Arousal_Mean <- 0


# some more cleanup of the dataframe

library(dplyr)
library(tidyr)

comparison_data$hashtag <- as.character(comparison_data$hashtag)

comparison_data <- comparison_data %>%
  filter(lang == "es") %>%
  distinct(text, .keep_all = TRUE) %>%
  gather(key = "Emotion", value = "Emotive_Value", 16:17)

Sys.setlocale("LC_TIME", "English")
comparison_data$created_at <- as.POSIXct(comparison_data$created_at, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")

emotion_values("Valence_Mean")
emotion_values("Arousal_Mean")

writepath = ""
write.csv(comparison_data, writepath) #indicate the destination file
