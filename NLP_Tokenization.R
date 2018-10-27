#to find out if there's a relationship between how often different children use
#disfluencies (words like "um" or "uh") and how long they've been exposed to English.

# loadlibraries 
library(tidyverse) 
library(tidytext) #package for tidy text analysis 
library(glue) #for pasting strings
library(data.table) #for rbindlist, a faster version of rbind

# read the data & put it in a tibble
file_info <- as_data_frame(read.csv("guide_to_files.csv"))
head(file_info)


#  function to clean up file names.
prepFileName <- function(name){
  # get the filename  stick together the path to the file & 1st file name from the information file
  fileName <- glue("C:/Users/REVANTH/Desktop/data/tokenisation_files/", as.character(name), sep = "")
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # can't forget to return our filename!
  return(fileName)
}

#  a function that takes in a file & returns the frequency of words in child speech
fileToTokens <- function(filename){
  # read in data
  fileText <- paste(readLines(filename))
  # get child's speech
  
  # "grep" finds the elements in the vector that contain the exact string *CHI:.
  # and then select those indxes from the vector "fileText".
  childsSpeech <- as_data_frame(fileText[grep("\\*CHI:",fileText)])
  # tokens sorted by frequency 
 
   sortedTokens <- childsSpeech%>%#take child speech
    unnest_tokens(word, value) %>% #preprocessing using unnest
    anti_join(data_frame(word = "chi"))%>% #remove the word chi from data as it is an identifier 
    #rather than a speech sentence
    count(word, sort = T)#sorted word frequencies
   
  # and return that to the user
  return(sortedTokens)
}

# make an empty dataset to store the results 
tokenFreqByChild <- NULL

for(name in file_info$file_name){
  
  # get the name of a specific child
  child <- name
  
  # use our custom functions we just made!
  tokens <- prepFileName(child) %>% fileToTokens()
  
  # and add the name of the current child
  tokensCurrentChild <- cbind(tokens, child)
  
  # add the current child's data to the rest of it
  tokenFreqByChild <- rbindlist(list(tokensCurrentChild,tokenFreqByChild))
}

summary(tokenFreqByChild)
head(tokenFreqByChild)

#  plot how many words get used each number of times 
ggplot(tokenFreqByChild, aes(n)) + geom_histogram()

#This visualiation tells us that most words are only used once, and that there are fewer words 
#that are used more. This is a veryrobust pattern in human language (it's known as "Zipf's Law"),

# rows in the dataframe where the word is "um"
ums <- tokenFreqByChild[tokenFreqByChild$word == "um",]

# rows in the dataframe where the word is "uh"
uhs <- tokenFreqByChild[tokenFreqByChild$word == "uh",]


# merge the ums,uhs,er dataframe with our information file
umsWithInfo <- merge(ums, file_info, by.y = "file_name", by.x = "child")
umsWithInfo <- merge(uhs,umsWithInfo , by.y = "child", by.x = "child")
#umsWithInfo <- merge(ers,umsWithInfo , by.y = "child", by.x = "child")

n<-as.data.frame(umsWithInfo$n.x+umsWithInfo$n.y)
colnames(n)<-"n"
umsWithInfo<-cbind(umsWithInfo,n)
umsWithInfo<-umsWithInfo[,c(1:5,12,6:11)]

head(umsWithInfo)

#now lets see if there is a relation between no of ums and period of english training exposure

# see if there's a significant correlation
cor.test(umsWithInfo$n, umsWithInfo$months_of_english)

# and check the plot
ggplot(umsWithInfo, aes(x = n, y = months_of_english)) + geom_point() + 
  geom_smooth(method = "lm")

#conclusion 
#That's a "no"; there is absolutely no relation between the number of months a 
#child had been exposed to English and the number of times they said "um" during data elicitation.
