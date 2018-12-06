library(tm)
library(stringr)
library(wordcloud)
setwd("C:/Users/Chris Cooper/Desktop/Trump_Analysis/2018_UN/Trump_text_mining")


###Cleaning text file###

#readLines("Speech_Politico_25_09_2018.txt") #reads the text
#paste(readLines("Speech_Politico_25_09_2018.txt"), collapse = " ")
	#collapses the text into a single vector, collpase variable can be 
	# iven a delimiter in this case a space between the previous vectors

text <- paste(readLines("Speech_Politico_25_09_2018.txt"), collapse = " ")
text_punct <- gsub(pattern = "\\W", replace = " ", text)	
	#gsub() takes a pattern and looks for a given set of chars that are not useful to the analysis 
	#e.g. W looks for spaces and punctuation , then gsub asks for a replacement for these removed chars
	# the final arg is the variable we are applying it to

text_num <- gsub(pattern = "\\d",replace = " ", text_punct) 	
	# d removes digits 0-9

text_case <- tolower(text_num)	
	#make the text uniform by formatting case

text_stopWords <- removeWords(text_case, stopwords()) 
	#removes stopwords (and, the, or, etc.) from a pre-defined list, can use array with c("and") as the second arg

text_firstParse <- gsub(pattern  = "\\b[A-z]\\b[1]", replace = " ",  text_stopWords)
	#this removes any rogue single letters not picked up by stopWords b = string starts with (in this case letters A-z)
	# the second b specifies  what the string ends with, [1] changes the speicfication to length of 1

#reduce whitespace to single spacing
text <- stripWhitespace(text_firstParse)

##Converting text to a character vector text_bag##

#split the text string into seperate elements in the vector using str_split(arg1,pattern=) , arg1 = string to split, pattern= what determines the split
text_bag <- str_split(text, pattern = "\\s+") 

#in this case s+ means spaces limits with an extra space, in case any double spaces got through stripWhitespace
# a text bag is a "bag of words"

#convert the class of text_bag from a list to a character vector
text_bag <- unlist(text_bag)


###Matching Words in text_bag to lexicons###
#Clean lexicons and format to character vectors#
poswords <-  paste(readLines("Positive_Opinion_Lexicon.txt"), collapse = " ")
negwords <-  paste(readLines("Negative_Opinion_Lexicon.txt"), collapse = " ")

#Split string etc.#
poswords <- str_split(poswords, pattern="\\s+")
poswords <- unlist(poswords)

negwords <- str_split(negwords, pattern="\\s+")
negwords <- unlist(negwords)

#is not the field NA?, if NA then FALSE as it is NA statement is ... !is.na = IS NOT NA
#!is.na(match(textbag,poswords)) 


#First piece of analysis#
#Sum of negative words used and positive words used#
Sum_Neg_Words <- sum(!is.na(match(text_bag,negwords)))
Sum_Pos_Words <- sum(!is.na(match(text_bag,poswords)))


##Word Cloud##
#par(mfrow=c(1,3)) ##creates an array to present all 3 word clouds in
png("Macron_Speech.png", width=1280,height=800)
wordcloud(text_bag, min_freq = 4,random.order = FALSE, scale = c(4,0.5), color = rainbow(10), rot.per = 0)


