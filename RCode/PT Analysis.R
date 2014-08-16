# Delete unused columns
#Private$Author.s.. <- NULL
#Private$Reviewer.Name <- NULL

# Convert blanks to NA
#Private[Private==""]  <- NA 

#Rename to make easier
#names(Private) <- c("Time", "ID", "ResearchType", "Publisher", "PubYear", "PubLang", "Country", "Region","EdLevel","ResearchApproach","ResearchMethods","Theory","Framed","WhatTaught","WhereOccur","DoesCharge","ClassSize","OperationSize","WhyTaking","Reviewer","PubType","WhoProvides","HowViewed","AddDetails","WhatData","Stakeholders","EquityDefine","WasData")

# =================
# Publication Language

PubLangGrid <- data.frame(grepl("Chinese", Private$PubLang), grepl("English", Private$PubLang), (!grepl("Chinese", Private$PubLang) & !grepl("English", Private$PubLang)))
names(PubLangGrid) <- c("Chinese", "English", "Other")

# =================
# Type of Research

Private$ResearchTypeOriginal <- grepl("Original", Private$ResearchType)
Private$ResearchTypeReview <- grepl("Review", Private$ResearchType)
Private$ResearchTypePolicy <- grepl("Policy", Private$ResearchType)

ResearchTypeGrid <- data.frame(Private$ResearchTypeOriginal, Private$ResearchTypeReview, Private$ResearchTypePolicy)

# Others
ResearchTypeOther <- subset(Private, !grepl("Original", Private$ResearchType) & !grepl("Review", Private$ResearchType) & !grepl("Policy", Private$ResearchType), select=c(ID, ResearchType, PubLang, Reviewer))
ResearchTypeOther <- na.omit(ResearchTypeOther)

# Counts
ResearchTypeCounts <- c(sum(ResearchTypeGrid$Original), sum(ResearchTypeGrid$Review), sum(ResearchTypeGrid$Policy))

# =================
# 

summary(Private$PubLang)
