#Setup with Libraries
install.packages("pacman")
pacman::p_load(pacman,httr,tidyr,tidyverse,readr,stringr,dplyr,rlang,magrittr)

#API to extract CSV 
# token <- token_path
# url <- url_path
# formData <- list("token"=token,
#                  content='record',
#                  action='export',
#                  format='csv',
#                  type='flat',
#                  csvDelimiter='',
#                  'forms[0]'='data',
#                  rawOrLabel='label',
#                  rawOrLabelHeaders='label',
#                  exportCheckboxLabel='false',
#                  exportSurveyFields='false',
#                  exportDataAccessGroups='false',
#                  returnFormat='csv'
# )
# response <- httr::POST(url, body = formData, encode = "form")
# dataframe <- httr::content(response)

#CSV File
dataframe  <- read_csv("https://raw.githubusercontent.com/MattKowalczyk/API-AND-Regex-for-Processing-Questionnaires/main/Questionnaire_headers_data.csv")

#Cleaning the data to make the code below generalizable on any data import as long as column name is the same (questionnaire 
#header needs to be provided)

#extract column names and make all columns lowercase
all_lower <- tolower(colnames(dataframe))

#removes all special characters
all_lower <- str_replace_all(all_lower, "[^a-zA-Z0-9']", " ") 
#takes multiple spaces and converts to one space
all_lower <- gsub("\\s+", " ", all_lower)

#Assigning Questions to Assessments Based on regular expression

#Big5
big5 <- str_match(all_lower, pattern = "i am someone who")
all_na<- is.na(big5)
big5 <- which(all_na==FALSE)
big5 <- dataframe[,big5]

#All sections below will assign the first and last question in each questionnaire as the range
#NOTE: The first and last question of each questionnaire must be included, else script will return 
#an error

#MIST
mist.first <-grep("morocco",all_lower)
mist.last <- grep("international relations experts",all_lower)
Mist <- dataframe[, mist.first:mist.last]

##Scoring##

#Big5
big5_regular <- big5 %>% 
  mutate(
    across(c(1:2, 6:7, 10, 13:15, 18:21, 27, 32:35, 38:41, 43, 46, 52:54, 56:57, 59:60),
           ~ recode(.x, 
                    "1 (disagree strongly)" = 1, 
                    "2 (disagree a little)" = 2, 
                    "3 (neutral, no opinion)" = 3, 
                    "4 (agree a little)" = 4, 
                    "5 (agree strongly)" = 5)
    ))

#Reverse Scoring for Big 5
big5_reverse<- c(1:2, 6:7, 10, 13:15, 18:21, 27, 32:35, 38:41, 43, 46, 52:54, 56:57, 59:60)
big5<-big5_regular %>% 
  mutate(
    across(-big5_reverse,
           ~ recode(.x, 
                    "1 (disagree strongly)" = 5, 
                    "2 (disagree a little)" = 4, 
                    "3 (neutral, no opinion)" = 3, 
                    "4 (agree a little)" = 2, 
                    "5 (agree strongly)" = 1)

    ))

#MIST-20
#1:10 are all fake whereas the rest are real 
MIST20_Columns_Fake <- Mist[,1:10]
MIST20_Columns_Real <- Mist[,11:20]

#Real columns (converting logical statements to binary 0s and 1s)
Mist_real <- data.frame(sapply(Mist[11:20], \(x) +as.logical(x)))

#fake (47:56 columns) only need to be recoded where fake = 1 and real = 0
Mist_recode <- ifelse(Mist[,1:10] == "FALSE",1,0)
Mist <- cbind(data.frame(Mist_recode),Mist_real)

##Score Calculations##

#Big 5 Calculations
big5 %<>%
  mutate(extraversion = rowMeans(big5[ , c(1, 6, 11, 16, 21, 26, 31, 36)], na.rm=TRUE))%>%
  mutate(agreeableness = rowMeans(big5[ , c(2, 7, 12, 17, 22, 27, 32, 37, 42)], na.rm=TRUE))%>%
  mutate(conscientiousness = rowMeans(big5[ , c(3, 8, 13, 18, 23, 28, 33, 38, 43)], na.rm=TRUE))%>%
  mutate(neuroticism = rowMeans(big5[ , c(4, 9, 14, 19, 24, 29, 34, 39)], na.rm=TRUE))%>%       
  mutate(openness = rowMeans(big5[ , c(5, 10, 15, 20, 25, 30, 35, 40, 41, 44)], na.rm=TRUE))                

##Mist Calculation##
# V | Veracity Discernment
Mist$MIST20_V <- rowSums(Mist[,1:20],na.rm=TRUE)

# r | Real News Detection Ability
Mist$Mist20_r <- rowSums(Mist[,11:20],na.rm=TRUE)

# f | Fake News Detection Ability
Mist$Mist20_f <- rowSums(Mist[,1:10], na.rm=TRUE)

# d | Distrust (Negative Response Bias)
Mist$MIST20_d <- rowSums(abs(1-Mist[,11:20]))+rowSums(Mist[,1:10])-10
Mist$MIST20_d[which(Mist$MIST20_d < 0)] <- 0

# n | Naïvité (Positive Response Bias)
Mist$MIST20_n <- rowSums(abs(1-Mist[,1:10]))+rowSums(Mist[,11:20])-10
Mist$MIST20_n[which(Mist$MIST20_n < 0)] <- 0

# Category
Mist$MIST20_Category <- Mist$MIST20_V
Mist$MIST20_Category[Mist$MIST20_Category > summary(Mist$MIST20_V)[3]] <- "High"
Mist$MIST20_Category[Mist$MIST20_Category != "High"] <- "Low"
Mist$MIST20_Category <- factor(Mist$MIST20_Category, levels = c("Low", "High"))



