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
items <- c("Review of other research", 
           "Original empirical research", 
           "Policy analysis")
ResearchTypeOther <- data.frame(Private$ID, (others <- gsub(sprintf("(,\\s)?(%s)(,\\s)?", paste(items, collapse = "|")), "", 
                                                sub(".*\\|\\s(.*)", "\\1", Private$ResearchType))), Private$Reviewer)
ResearchTypeOther[ResearchTypeOther==""] <- NA
ResearchTypeOther <- na.omit(ResearchTypeOther)

# Counts
#ResearchTypeCounts <- c(sum(ResearchTypeGrid$Original), sum(ResearchTypeGrid$Review), sum(ResearchTypeGrid$Policy))

# =================
#  Regions

Private$RegionMENA <- grepl("Middle East", Private$Region)
Private$RegionSSA <- grepl("Sub-Saharan", Private$Region)
Private$RegionCentralAsia <- grepl("Central", Private$Region)
Private$RegionEastAsia <- grepl("East Asia", Private$Region)
Private$RegionSouthAsia <- grepl("South Asia", Private$Region)
Private$RegionSoutheastAsia <- grepl("Southeast Asia", Private$Region)
Private$RegionSouthANZ <- grepl("Australia", Private$Region)
Private$RegionWesternEurope <- grepl("Western", Private$Region)
Private$RegionEasternEurope <- grepl("Eastern", Private$Region)
Private$RegionUSCanada <- grepl("U.S.", Private$Region)
Private$RegionLAC <- grepl("Latin", Private$Region)
Private$RegionGlobal <- grepl("Global", Private$Region)

RegionGrid <- data.frame(Private$RegionMENA, Private$RegionSSA, Private$RegionCentralAsia, Private$RegionEastAsia, Private$RegionSouthAsia, Private$RegionSoutheastAsia, Private$RegionSouthANZ, Private$RegionWesternEurope, Private$RegionEasternEurope, Private$RegionUSCanada, Private$RegionLAC, Private$RegionGlobal)

# Others

items <- c("Middle East & North Africa", 
           "Sub-Saharan Africa",
           "Central Asia", 
           "East Asia",
           "South Asia",
           "Southeast Asia",
           "Australia & New Zealand",
           "Western Europe",
           "Eastern Europe",
           "U.S. & Canada",
           "Latin America & Caribbean",
           "Global / Worldwide")
RegionOther <- data.frame(Private$ID, (others <- gsub(sprintf("(,\\s)?(%s)(,\\s)?", paste(items, collapse = "|")), "", 
                                                            sub(".*\\|\\s(.*)", "\\1", Private$Region))), Private$Reviewer)
RegionOther[RegionOther==""] <- NA
RegionOther <- na.omit(RegionOther)