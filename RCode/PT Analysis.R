# Delete unused columns
Private$Author.s.. <- NULL
Private$Reviewer.Name <- NULL

# Convert blanks to NA
Private[Private==""]  <- NA 

#Rename to make easier
names(Private) <- c("Time", "ID", "ResearchType", "Publisher", "PubYear", "PubLang", "Country", "Region","EdLevel","ResearchApproach","ResearchMethods","Theory","Framed","WhatTaught","WhereOccur","DoesCharge","ClassSize","OperationSize","WhyTaking","Reviewer","PubType","WhoProvides","HowViewed","AddDetails","WhatData","Stakeholders","EquityDefine","WasData")

# =================
# Type of Research
  
ResearchTypeGrid <- data.frame(Private$ID, grepl("Original", Private$ResearchType), grepl("Review", Private$ResearchType), grepl("Policy", Private$ResearchType), Private$PubLang)
names(ResearchTypeGrid) <- c("ID", "Original", "Review", "Policy")

# Others
ResearchTypeOther <- subset(Private, !grepl("Original", Private$ResearchType) & !grepl("Review", Private$ResearchType) & !grepl("Policy", Private$ResearchType), select=c(ID, ResearchType, PubLang))
ResearchTypeOther <- na.omit(ResearchTypeOther)

# Counts
ResearchTypeCounts <- c(sum(ResearchTypeGrid$Original), sum(ResearchTypeGrid$Review), sum(ResearchTypeGrid$Policy))

# =================
# 

summary(Private$PubLang)
