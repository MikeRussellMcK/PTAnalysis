# Delete unused columns
#Private$Author.s.. <- NULL
#Private$Reviewer.Name <- NULL

# Convert blanks to NA
#Private[Private==""]  <- NA 

#Rename to make easier
#names(Private) <- c("Time", "ID", "ResearchType", "Publisher", "PubYear", "PubLang", "Country", "Region","EdLevel","ResearchApproach","ResearchMethods","Theory","Framed","WhatTaught","WhereOccur","DoesCharge","ClassSize","OperationSize","WhyTaking","Reviewer","PubType","WhoProvides","HowViewed","AddDetails","WhatData","Stakeholders","EquityDefine","WasData")

# =================
# Type of Publication

# Private$

# =================
# Type of Research

Private$ResearchTypeOriginal <- grepl("Original", Private$ResearchType)
Private$ResearchTypeReview <- grepl("Review", Private$ResearchType)
Private$ResearchTypePolicy <- grepl("Policy", Private$ResearchType)

ResearchTypeGrid <- data.frame(Private$ResearchTypeOriginal, Private$ResearchTypeReview, Private$ResearchTypePolicy)

ResearchTypeGrid$AsianRegionOriginal <- grepl("Original", Private$ResearchType) & grepl("TRUE", Private$AsiaYN)
ResearchTypeGrid$AsianRegionReview <- grepl("Review", Private$ResearchType) & grepl("TRUE", Private$AsiaYN)
ResearchTypeGrid$AsianRegionPolicy <- grepl("Policy", Private$ResearchType) & grepl("TRUE", Private$AsiaYN)

ResearchTypeGrid$EngLangOriginal <- grepl("Original", Private$ResearchType) & grepl("English", Private$PubLang)
ResearchTypeGrid$EngLangReview <- grepl("Review", Private$ResearchType) & grepl("English", Private$PubLang)
ResearchTypeGrid$EngLangPolicy <- grepl("Policy", Private$ResearchType) & grepl("English", Private$PubLang)

ResearchTypeGrid$ChLangOriginal <- grepl("Original", Private$ResearchType) & grepl("Chinese", Private$PubLang)
ResearchTypeGrid$ChLangReview <- grepl("Review", Private$ResearchType) & grepl("Chinese", Private$PubLang)
ResearchTypeGrid$ChLangPolicy <- grepl("Policy", Private$ResearchType) & grepl("Chinese", Private$PubLang)


# Others
ResearchTypeOther <- subset(Private, !grepl("Original", Private$ResearchType) & !grepl("Review", Private$ResearchType) & !grepl("Policy", Private$ResearchType), select=c(ID, ResearchType, PubLang))
ResearchTypeOther <- na.omit(ResearchTypeOther)

# Counts
ResearchTypeCounts <- c(sum(ResearchTypeGrid$Original), sum(ResearchTypeGrid$Review), sum(ResearchTypeGrid$Policy))

# =================
# 

summary(Private$PubLang)
