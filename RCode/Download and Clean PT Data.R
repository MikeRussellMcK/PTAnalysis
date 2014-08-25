# Loads libraries used in analysis:
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("mosaic", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("knitr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("foreign", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
# library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("grid", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("gridExtra", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

# Downloads data fresh from Google - 10-15 MIN DELAY FROM GOOGLE SERVERS
Private <- fetchGoogle("https://docs.google.com/spreadsheet/pub?key=0ApPsU6wycRKhdC14cnZXb3k0T0pKek5pbFpibFJUekE&output=csv")
#Private <- read.csv("~/Documents/R/PTAnalysis2/Source/Private%20Tutoring%20Coding%20Database%20-%20Form%20Responses.csv")

# Deletes unused columns from the database
Private$Author.s.. <- NULL
Private$Reviewer.Name <- NULL

# Converts blanks to NA
Private[Private==""]  <- NA 

#Renames columns to make processing easier
names(Private) <- c("Time", "ID", "ResearchType", "Publisher", "PubYear", "PubLang", "Country", "Region","EdLevel","ResearchApproach","ResearchMethods","Theory","Framed","WhatTaught","WhereOccur","DoesCharge","ClassSize","OperationSize","WhyTaking","Reviewer","PubType","WhoProvides","HowViewed","AddDetails","WhatData","Stakeholders","EquityDefine","WasData")

# Makes Asian Y/N column
Private$AsiaYN <- grepl("Asia", Private$Region)

# Make new data frame based Asia region
AllAsia <- filter(Private, AsiaYN == "TRUE")

# Make new data frame for English Language
AllEnglish <- filter(Private, PubLang == "English")

# Mane new data frame for Chinese Language
AllChinese <- filter(Private, PubLang == "Chinese")

PrivateName1 <- "All Entries"
AllAsiaName1 <- "Asia Regions"
AllChineseName1 <- "Chinese-language"
AllEnglishName1 <- "English-language"