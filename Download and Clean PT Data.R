# Loads libraries used in analysis:
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("mosaic", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("knitr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("foreign", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

# Downloads data fresh from Google - 10-15 MIN DELAY FROM GOOGLE SERVERS
Private <- fetchGoogle("https://docs.google.com/spreadsheet/pub?key=0ApPsU6wycRKhdC14cnZXb3k0T0pKek5pbFpibFJUekE&output=csv")

# Deletes unused columns from the database
Private$Author.s.. <- NULL
Private$Reviewer.Name <- NULL

# Converts blanks to NA
Private[Private==""]  <- NA 

#Renames columns to make processing easier
names(Private) <- c("Time", "ID", "ResearchType", "Publisher", "PubYear", "PubLang", "Country", "Region","EdLevel","ResearchApproach","ResearchMethods","Theory","Framed","WhatTaught","WhereOccur","DoesCharge","ClassSize","OperationSize","WhyTaking","Reviewer","PubType","WhoProvides","HowViewed","AddDetails","WhatData","Stakeholders","EquityDefine","WasData")
