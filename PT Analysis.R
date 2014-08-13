library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("mosaic", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
Private <- fetchGoogle("https://docs.google.com/spreadsheet/pub?key=0ApPsU6wycRKhdC14cnZXb3k0T0pKek5pbFpibFJUekE&output=csv")

# Delete unused columns
Private$Author.s.. <- NULL
Private$Reviewer.Name <- NULL

# Convert blanks to NA
Private[Private==""]  <- NA 

#Rename to make easier
names(Private) <- c("Time", "ID", "ResearchType", "Publisher", "PubYear", "PubLang", "Country", "Region","EdLevel","ResearchApproach","ResearchMethods","Theory","Framed","WhatTaught","WhereOccur","DoesCharge","ClassSize","OperationSize","WhyTaking","Reviewer","PubType","WhoProvides","HowViewed","AddDetails","WhatData","Stakeholders","EquityDefine","WasData")

# =================
# Type of Research
  
ResearchTypeGrid <- data.frame(Private$ID, grepl("Original", Private$ResearchType), grepl("Review", Private$ResearchType), grepl("Policy", Private$ResearchType))
names(ResearchTypeGrid) <- c("ID", "Original", "Review", "Policy")

# Others
ResearchTypeOther <- subset(Private, !grepl("Original", Private$ResearchType) & !grepl("Review", Private$ResearchType) & !grepl("Policy", Private$ResearchType), select=c(ID, ResearchType))
ResearchTypeOther <- na.omit(ResearchTypeOther)