# Delete unused columns
#Private$Author.s.. <- NULL
#Private$Reviewer.Name <- NULL

# Convert blanks to NA
#Private[Private==""]  <- NA 

#Rename to make easier
#names(Private) <- c("Time", "ID", "ResearchType", "Publisher", "PubYear", "PubLang", "Country", "Region","EdLevel","ResearchApproach","ResearchMethods","Theory","Framed","WhatTaught","WhereOccur","DoesCharge","ClassSize","OperationSize","WhyTaking","Reviewer","PubType","WhoProvides","HowViewed","AddDetails","WhatData","Stakeholders","EquityDefine","WasData")

# Graphing Function
AllDataPlot <- function(a,b,c,d,e) {
  
  Label <- as.data.frame(table(c))
  colnames(Label)[1] <- "x"
  Label$lab <- as.character(round(100 * Label$Freq / sum(Label$Freq)))
  Label$lab <- paste(Label$Freq,paste("(",Label$lab,"%)",sep=""),sep=" ")
  
  ggplot(a, aes_string(x=b)) + geom_bar() + ggtitle(d) + 
    geom_text(size=3, data=Label,aes(x=x,y=Freq,label=lab),hjust=-.1) + 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.background=element_blank(), plot.title=element_text(size=10)) + 
    scale_y_continuous(limits=c(0,e)) + coord_flip()
}

CategoricalPlot <- function(a,b,c) {
  Label <- as.data.frame(colSums(a))
  Label <- setNames(cbind(rownames(Label), Label, row.names = NULL), c("x", "Freq"))
  Label$lab <- as.character(round(100 * Label$Freq / sum(Label$Freq)))
  Label$lab <- paste(Label$Freq,paste("(",Label$lab,"%)",sep=""),sep=" ")
  
  ggplot(data=Label, aes(x=x, y=Freq)) + geom_bar(stat="identity") + ggtitle(b) +
    geom_text(size=3, data=Label,aes(x=x,y=Freq,label=lab),hjust=-.1) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.background=element_blank(), plot.title=element_text(size=10)) + 
    scale_y_continuous(limits = c(0, c)) + coord_flip()
}



Title1 = "All Entries"
Title2 = "Asia Region"
Title3 = "All Chinese"
Title4 = "All English"

# Publication Language
# ===================

# Create the TRUE/FALSE grid
PrivatePubLangGrid <- data.frame(grepl("Chinese", Private$PubLang), grepl("English", Private$PubLang), (!grepl("Chinese", Private$PubLang) & !grepl("English", Private$PubLang)))
names(PrivatePubLangGrid) <- c("Chinese", "English", "Other")

AllAsiaPubLangGrid <- data.frame(grepl("Chinese", AllAsia$PubLang), grepl("English", AllAsia$PubLang), (!grepl("Chinese", AllAsia$PubLang) & !grepl("English", AllAsia$PubLang)))
names(AllAsiaPubLangGrid) <- c("Chinese", "English", "Other")

# Type of Research
# ================= 
# Type of Research

#Make the Private T/F Grid
Private$ResearchTypeOriginal <- grepl("Original", Private$ResearchType)
Private$ResearchTypeReview <- grepl("Review", Private$ResearchType)
Private$ResearchTypePolicy <- grepl("Policy", Private$ResearchType)

PrivateResearchTypeGrid <- data.frame(Private$ResearchTypeOriginal, Private$ResearchTypeReview, Private$ResearchTypePolicy)
names(PrivateResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

#Make the AllAsia T/F Grid
AllAsia$ResearchTypeOriginal <- grepl("Original", AllAsia$ResearchType)
AllAsia$ResearchTypeReview <- grepl("Review", AllAsia$ResearchType)
AllAsia$ResearchTypePolicy <- grepl("Policy", AllAsia$ResearchType)

AllAsiaResearchTypeGrid <- data.frame(AllAsia$ResearchTypeOriginal, AllAsia$ResearchTypeReview, AllAsia$ResearchTypePolicy)
names(AllAsiaResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

#Make the AllChinese T/F Grid
AllChinese$ResearchTypeOriginal <- grepl("Original", AllChinese$ResearchType)
AllChinese$ResearchTypeReview <- grepl("Review", AllChinese$ResearchType)
AllChinese$ResearchTypePolicy <- grepl("Policy", AllChinese$ResearchType)

AllChineseResearchTypeGrid <- data.frame(AllChinese$ResearchTypeOriginal, AllChinese$ResearchTypeReview, AllChinese$ResearchTypePolicy)
names(AllChineseResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

#Make the AllEnglish T/F Grid
AllEnglish$ResearchTypeOriginal <- grepl("Original", AllEnglish$ResearchType)
AllEnglish$ResearchTypeReview <- grepl("Review", AllEnglish$ResearchType)
AllEnglish$ResearchTypePolicy <- grepl("Policy", AllEnglish$ResearchType)

AllEnglishResearchTypeGrid <- data.frame(AllEnglish$ResearchTypeOriginal, AllEnglish$ResearchTypeReview, AllEnglish$ResearchTypePolicy)
names(AllEnglishResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

## Others
items <- c("Review of other research", 
           "Original empirical research", 
           "Policy analysis")
ResearchTypeOther <- data.frame(Private$ID, (others <- gsub(sprintf("(,\\s)?(%s)(,\\s)?", paste(items, collapse = "|")), "", 
                                                sub(".*\\|\\s(.*)", "\\1", Private$ResearchType))), Private$Reviewer)
ResearchTypeOther[ResearchTypeOther==""] <- NA
ResearchTypeOther <- na.omit(ResearchTypeOther)

#  Regions
# =================
# Region

RegionNames <- c("MENA", "Sub-Saharan", "Cent. Asia", "East Asia", "S. Asia", "S.E. Asia", "Aus. & NZ", "W. Europe", "E. Europe", "US & Can", "LAC", "Global")

## Private
## ======
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

PrivateRegionGrid <- data.frame(Private$RegionMENA, Private$RegionSSA, Private$RegionCentralAsia, Private$RegionEastAsia, Private$RegionSouthAsia, Private$RegionSoutheastAsia, Private$RegionSouthANZ, Private$RegionWesternEurope, Private$RegionEasternEurope, Private$RegionUSCanada, Private$RegionLAC, Private$RegionGlobal)
names(PrivateRegionGrid) <- RegionNames

## AllAsia
## =====

AllAsia$RegionMENA <- grepl("Middle East", AllAsia$Region)
AllAsia$RegionSSA <- grepl("Sub-Saharan", AllAsia$Region)
AllAsia$RegionCentralAsia <- grepl("Central", AllAsia$Region)
AllAsia$RegionEastAsia <- grepl("East Asia", AllAsia$Region)
AllAsia$RegionSouthAsia <- grepl("South Asia", AllAsia$Region)
AllAsia$RegionSoutheastAsia <- grepl("Southeast Asia", AllAsia$Region)
AllAsia$RegionSouthANZ <- grepl("Australia", AllAsia$Region)
AllAsia$RegionWesternEurope <- grepl("Western", AllAsia$Region)
AllAsia$RegionEasternEurope <- grepl("Eastern", AllAsia$Region)
AllAsia$RegionUSCanada <- grepl("U.S.", AllAsia$Region)
AllAsia$RegionLAC <- grepl("Latin", AllAsia$Region)
AllAsia$RegionGlobal <- grepl("Global", AllAsia$Region)

AllAsiaRegionGrid <- data.frame(AllAsia$RegionMENA, AllAsia$RegionSSA, AllAsia$RegionCentralAsia, AllAsia$RegionEastAsia, AllAsia$RegionSouthAsia, AllAsia$RegionSoutheastAsia, AllAsia$RegionSouthANZ, AllAsia$RegionWesternEurope, AllAsia$RegionEasternEurope, AllAsia$RegionUSCanada, AllAsia$RegionLAC, AllAsia$RegionGlobal)
names(AllAsiaRegionGrid) <- RegionNames

## AllChinese
## =====

AllChinese$RegionMENA <- grepl("Middle East", AllChinese$Region)
AllChinese$RegionSSA <- grepl("Sub-Saharan", AllChinese$Region)
AllChinese$RegionCentralAsia <- grepl("Central", AllChinese$Region)
AllChinese$RegionEastAsia <- grepl("East Asia", AllChinese$Region)
AllChinese$RegionSouthAsia <- grepl("South Asia", AllChinese$Region)
AllChinese$RegionSoutheastAsia <- grepl("Southeast Asia", AllChinese$Region)
AllChinese$RegionSouthANZ <- grepl("Australia", AllChinese$Region)
AllChinese$RegionWesternEurope <- grepl("Western", AllChinese$Region)
AllChinese$RegionEasternEurope <- grepl("Eastern", AllChinese$Region)
AllChinese$RegionUSCanada <- grepl("U.S.", AllChinese$Region)
AllChinese$RegionLAC <- grepl("Latin", AllChinese$Region)
AllChinese$RegionGlobal <- grepl("Global", AllChinese$Region)

AllChineseRegionGrid <- data.frame(AllChinese$RegionMENA, AllChinese$RegionSSA, AllChinese$RegionCentralAsia, AllChinese$RegionEastAsia, AllChinese$RegionSouthAsia, AllChinese$RegionSoutheastAsia, AllChinese$RegionSouthANZ, AllChinese$RegionWesternEurope, AllChinese$RegionEasternEurope, AllChinese$RegionUSCanada, AllChinese$RegionLAC, AllChinese$RegionGlobal)
names(AllChineseRegionGrid) <- RegionNames

## AllEnglish
## =====

AllEnglish$RegionMENA <- grepl("Middle East", AllEnglish$Region)
AllEnglish$RegionSSA <- grepl("Sub-Saharan", AllEnglish$Region)
AllEnglish$RegionCentralAsia <- grepl("Central", AllEnglish$Region)
AllEnglish$RegionEastAsia <- grepl("East Asia", AllEnglish$Region)
AllEnglish$RegionSouthAsia <- grepl("South Asia", AllEnglish$Region)
AllEnglish$RegionSoutheastAsia <- grepl("Southeast Asia", AllEnglish$Region)
AllEnglish$RegionSouthANZ <- grepl("Australia", AllEnglish$Region)
AllEnglish$RegionWesternEurope <- grepl("Western", AllEnglish$Region)
AllEnglish$RegionEasternEurope <- grepl("Eastern", AllEnglish$Region)
AllEnglish$RegionUSCanada <- grepl("U.S.", AllEnglish$Region)
AllEnglish$RegionLAC <- grepl("Latin", AllEnglish$Region)
AllEnglish$RegionGlobal <- grepl("Global", AllEnglish$Region)

AllEnglishRegionGrid <- data.frame(AllEnglish$RegionMENA, AllEnglish$RegionSSA, AllEnglish$RegionCentralAsia, AllEnglish$RegionEastAsia, AllEnglish$RegionSouthAsia, AllEnglish$RegionSoutheastAsia, AllEnglish$RegionSouthANZ, AllEnglish$RegionWesternEurope, AllEnglish$RegionEasternEurope, AllEnglish$RegionUSCanada, AllEnglish$RegionLAC, AllEnglish$RegionGlobal)
names(AllEnglishRegionGrid) <- RegionNames

## Others
## ======

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

# Level of education discussed
# ============================

EdLevelNames <- c("Pre-primary", "Primary", "Secondary", "Tertiary")

## Private
## ======

Private$EdLevelPrePrimary <- grepl("Pre-primary", Private$EdLevel)
Private$EdLevelPrimary <- grepl("Primary", Private$EdLevel)
Private$EdLevelSecondary <- grepl("Secondary", Private$EdLevel)
Private$EdLevelTertiary <- grepl("Tertiary", Private$EdLevel)

PrivateEdLevelGrid <- data.frame(Private$EdLevelPrePrimary, Private$EdLevelPrimary, Private$EdLevelSecondary, Private$EdLevelTertiary)
names(PrivateEdLevelGrid) <- EdLevelNames

## AllAsia
## ======

AllAsia$EdLevelPrePrimary <- grepl("Pre-primary", AllAsia$EdLevel)
AllAsia$EdLevelPrimary <- grepl("Primary", AllAsia$EdLevel)
AllAsia$EdLevelSecondary <- grepl("Secondary", AllAsia$EdLevel)
AllAsia$EdLevelTertiary <- grepl("Tertiary", AllAsia$EdLevel)

AllAsiaEdLevelGrid <- data.frame(AllAsia$EdLevelPrePrimary, AllAsia$EdLevelPrimary, AllAsia$EdLevelSecondary, AllAsia$EdLevelTertiary)
names(AllAsiaEdLevelGrid) <- EdLevelNames

## AllChinese
## ======

AllChinese$EdLevelPrePrimary <- grepl("Pre-primary", AllChinese$EdLevel)
AllChinese$EdLevelPrimary <- grepl("Primary", AllChinese$EdLevel)
AllChinese$EdLevelSecondary <- grepl("Secondary", AllChinese$EdLevel)
AllChinese$EdLevelTertiary <- grepl("Tertiary", AllChinese$EdLevel)

AllChineseEdLevelGrid <- data.frame(AllChinese$EdLevelPrePrimary, AllChinese$EdLevelPrimary, AllChinese$EdLevelSecondary, AllChinese$EdLevelTertiary)
names(AllChineseEdLevelGrid) <- EdLevelNames

## AllEnglish
## ======

AllEnglish$EdLevelPrePrimary <- grepl("Pre-primary", AllEnglish$EdLevel)
AllEnglish$EdLevelPrimary <- grepl("Primary", AllEnglish$EdLevel)
AllEnglish$EdLevelSecondary <- grepl("Secondary", AllEnglish$EdLevel)
AllEnglish$EdLevelTertiary <- grepl("Tertiary", AllEnglish$EdLevel)

AllEnglishEdLevelGrid <- data.frame(AllEnglish$EdLevelPrePrimary, AllEnglish$EdLevelPrimary, AllEnglish$EdLevelSecondary, AllEnglish$EdLevelTertiary)
names(AllEnglishEdLevelGrid) <- EdLevelNames


## Others
## ======

Other <- data.frame(Private$ID, (others <- gsub(sprintf("(,\\s)?(%s)(,\\s)?", paste(EdLevelNames, collapse = "|")), "", 
                                                      sub(".*\\|\\s(.*)", "\\1", Private$EdLevel))), Private$Reviewer)
Other[Other==""] <- NA
EdLevelOther <- na.omit(Other)

# Research Approach
# ============================

ResearchApproachNames <- c("Single-country / case", "Comparative")

## Private
## ======

Private$ResearchApproachSingle <- grepl("Single-country", Private$ResearchApproach)
Private$ResearchApproachComparative <- grepl("Comparative", Private$ResearchApproach)

PrivateResearchApproachGrid <- data.frame(Private$ResearchApproachSingle, Private$ResearchApproachComparative)
names(PrivateResearchApproachGrid) <- ResearchApproachNames

## AllAsia
## ======

AllAsia$ResearchApproachSingle <- grepl("Single-country", AllAsia$ResearchApproach)
AllAsia$ResearchApproachComparative <- grepl("Comparative", AllAsia$ResearchApproach)

AllAsiaResearchApproachGrid <- data.frame(AllAsia$ResearchApproachSingle, AllAsia$ResearchApproachComparative)
names(AllAsiaResearchApproachGrid) <- ResearchApproachNames

## AllChinese
## ======

AllChinese$ResearchApproachSingle <- grepl("Single-country", AllChinese$ResearchApproach)
AllChinese$ResearchApproachComparative <- grepl("Comparative", AllChinese$ResearchApproach)

AllChineseResearchApproachGrid <- data.frame(AllChinese$ResearchApproachSingle, AllChinese$ResearchApproachComparative)
names(AllChineseResearchApproachGrid) <- ResearchApproachNames

## AllEnglish
## ======

AllEnglish$ResearchApproachSingle <- grepl("Single-country", AllEnglish$ResearchApproach)
AllEnglish$ResearchApproachComparative <- grepl("Comparative", AllEnglish$ResearchApproach)

AllEnglishResearchApproachGrid <- data.frame(AllEnglish$ResearchApproachSingle, AllEnglish$ResearchApproachComparative)
names(AllEnglishResearchApproachGrid) <- ResearchApproachNames

## Others
## ======

Other <- data.frame(Private$ID, (others <- gsub(sprintf("(,\\s)?(%s)(,\\s)?", paste(ResearchApproachNames, collapse = "|")), "", 
                                                sub(".*\\|\\s(.*)", "\\1", Private$ResearchApproach))), Private$Reviewer)
Other[Other==""] <- NA
ResearchApproachOther <- na.omit(Other)

# Research Methods
# ============================

ResearchMethodsNames <- c("Quantitative", "Qualitative", "Mixed", "None or unclear")

## Private
## ======

Private$ResearchMethodsQuant <- grepl("Quantitative", Private$ResearchMethods)
Private$ResearchMethodsQual <- grepl("Qualitative", Private$ResearchMethods)
Private$ResearchMethodsMixed <- grepl("Mixed", Private$ResearchMethods)
Private$ResearchMethodsNone <- grepl("None", Private$ResearchMethods)

PrivateResearchMethodsGrid <- data.frame(Private$ResearchMethodsQuant, Private$ResearchMethodsQual, Private$ResearchMethodsMixed, Private$ResearchMethodsNone)
names(PrivateResearchMethodsGrid) <- ResearchMethodsNames

## AllAsia
## ======

AllAsia$ResearchMethodsQuant <- grepl("Quantitative", AllAsia$ResearchMethods)
AllAsia$ResearchMethodsQual <- grepl("Qualitative", AllAsia$ResearchMethods)
AllAsia$ResearchMethodsMixed <- grepl("Mixed", AllAsia$ResearchMethods)
AllAsia$ResearchMethodsNone <- grepl("None", AllAsia$ResearchMethods)

AllAsiaResearchMethodsGrid <- data.frame(AllAsia$ResearchMethodsQuant, AllAsia$ResearchMethodsQual, AllAsia$ResearchMethodsMixed, AllAsia$ResearchMethodsNone)
names(AllAsiaResearchMethodsGrid) <- ResearchMethodsNames

## AllChinese
## ======

AllChinese$ResearchMethodsQuant <- grepl("Quantitative", AllChinese$ResearchMethods)
AllChinese$ResearchMethodsQual <- grepl("Qualitative", AllChinese$ResearchMethods)
AllChinese$ResearchMethodsMixed <- grepl("Mixed", AllChinese$ResearchMethods)
AllChinese$ResearchMethodsNone <- grepl("None", AllChinese$ResearchMethods)

AllChineseResearchMethodsGrid <- data.frame(AllChinese$ResearchMethodsQuant, AllChinese$ResearchMethodsQual, AllChinese$ResearchMethodsMixed, AllChinese$ResearchMethodsNone)
names(AllChineseResearchMethodsGrid) <- ResearchMethodsNames

## AllEnglish
## ======

AllEnglish$ResearchMethodsQuant <- grepl("Quantitative", AllEnglish$ResearchMethods)
AllEnglish$ResearchMethodsQual <- grepl("Qualitative", AllEnglish$ResearchMethods)
AllEnglish$ResearchMethodsMixed <- grepl("Mixed", AllEnglish$ResearchMethods)
AllEnglish$ResearchMethodsNone <- grepl("None", AllEnglish$ResearchMethods)

AllEnglishResearchMethodsGrid <- data.frame(AllEnglish$ResearchMethodsQuant, AllEnglish$ResearchMethodsQual, AllEnglish$ResearchMethodsMixed, AllEnglish$ResearchMethodsNone)
names(AllEnglishResearchMethodsGrid) <- ResearchMethodsNames

# Was data set used?
# ============================

WasDataNames <- c("Yes", "No")

## Private
## ======

Private$WasDataYes <- grepl("Yes", Private$WasData)
Private$WasDataNo <- grepl("No", Private$WasData)

PrivateWasDataGrid <- data.frame(Private$WasDataYes, Private$WasDataNo)
names(PrivateWasDataGrid) <- WasDataNames

## AllAsia
## ======

AllAsia$WasDataYes <- grepl("Yes", AllAsia$WasData)
AllAsia$WasDataNo <- grepl("No", AllAsia$WasData)

AllAsiaWasDataGrid <- data.frame(AllAsia$WasDataYes, AllAsia$WasDataNo)
names(AllAsiaWasDataGrid) <- WasDataNames

## AllChinese
## ======

AllChinese$WasDataYes <- grepl("Yes", AllChinese$WasData)
AllChinese$WasDataNo <- grepl("No", AllChinese$WasData)

AllChineseWasDataGrid <- data.frame(AllChinese$WasDataYes, AllChinese$WasDataNo)
names(AllChineseWasDataGrid) <- WasDataNames

## AllEnglish
## ======

AllEnglish$WasDataYes <- grepl("Yes", AllEnglish$WasData)
AllEnglish$WasDataNo <- grepl("No", AllEnglish$WasData)

AllEnglishWasDataGrid <- data.frame(AllEnglish$WasDataYes, AllEnglish$WasDataNo)
names(AllEnglishWasDataGrid) <- WasDataNames

# Theory

TheoryNames <- c("Social Capital", "Human Capital", "Cultural Capital", "World Culture", "Human Rights", "Critical Theory", "Political Theory", "None or Unclear to us")

# Theoretical Frameworks
# ============================

## Private
## ======
Private$TheorySocialCap <- grepl("Social", Private$Theory)
Private$TheoryHumanCap <- grepl("Human Capital", Private$Theory)
Private$TheoryCulturalCap <- grepl("Cultural", Private$Theory)
Private$TheoryWorldCult <- grepl("World", Private$Theory)
Private$TheoryHumanRights <- grepl("Human Rights", Private$Theory)
Private$TheoryCritical <- grepl("Critical Theory", Private$Theory)
Private$TheoryPolitical <- grepl("Political", Private$Theory)
Private$TheoryNone <- grepl("None", Private$Theory)

PrivateTheoryGrid <- data.frame(Private$TheorySocialCap, Private$TheoryHumanCap, Private$TheoryCulturalCap, Private$TheoryWorldCult, Private$TheoryHumanRights, Private$TheoryCritical, Private$TheoryPolitical, Private$TheoryNone)
names(PrivateTheoryGrid) <- TheoryNames

## AllAsia
## ======
AllAsia$TheorySocialCap <- grepl("Social", AllAsia$Theory)
AllAsia$TheoryHumanCap <- grepl("Human Capital", AllAsia$Theory)
AllAsia$TheoryCulturalCap <- grepl("Cultural", AllAsia$Theory)
AllAsia$TheoryWorldCult <- grepl("World", AllAsia$Theory)
AllAsia$TheoryHumanRights <- grepl("Human Rights", AllAsia$Theory)
AllAsia$TheoryCritical <- grepl("Critical Theory", AllAsia$Theory)
AllAsia$TheoryPolitical <- grepl("Political", AllAsia$Theory)
AllAsia$TheoryNone <- grepl("None", AllAsia$Theory)

AllAsiaTheoryGrid <- data.frame(AllAsia$TheorySocialCap, AllAsia$TheoryHumanCap, AllAsia$TheoryCulturalCap, AllAsia$TheoryWorldCult, AllAsia$TheoryHumanRights, AllAsia$TheoryCritical, AllAsia$TheoryPolitical, AllAsia$TheoryNone)
names(AllAsiaTheoryGrid) <- TheoryNames

## AllChinese
## ======
AllChinese$TheorySocialCap <- grepl("Social", AllChinese$Theory)
AllChinese$TheoryHumanCap <- grepl("Human Capital", AllChinese$Theory)
AllChinese$TheoryCulturalCap <- grepl("Cultural", AllChinese$Theory)
AllChinese$TheoryWorldCult <- grepl("World", AllChinese$Theory)
AllChinese$TheoryHumanRights <- grepl("Human Rights", AllChinese$Theory)
AllChinese$TheoryCritical <- grepl("Critical Theory", AllChinese$Theory)
AllChinese$TheoryPolitical <- grepl("Political", AllChinese$Theory)
AllChinese$TheoryNone <- grepl("None", AllChinese$Theory)

AllChineseTheoryGrid <- data.frame(AllChinese$TheorySocialCap, AllChinese$TheoryHumanCap, AllChinese$TheoryCulturalCap, AllChinese$TheoryWorldCult, AllChinese$TheoryHumanRights, AllChinese$TheoryCritical, AllChinese$TheoryPolitical, AllChinese$TheoryNone)
names(AllChineseTheoryGrid) <- TheoryNames

## AllEnglish
## ======
AllEnglish$TheorySocialCap <- grepl("Social", AllEnglish$Theory)
AllEnglish$TheoryHumanCap <- grepl("Human Capital", AllEnglish$Theory)
AllEnglish$TheoryCulturalCap <- grepl("Cultural", AllEnglish$Theory)
AllEnglish$TheoryWorldCult <- grepl("World", AllEnglish$Theory)
AllEnglish$TheoryHumanRights <- grepl("Human Rights", AllEnglish$Theory)
AllEnglish$TheoryCritical <- grepl("Critical Theory", AllEnglish$Theory)
AllEnglish$TheoryPolitical <- grepl("Political", AllEnglish$Theory)
AllEnglish$TheoryNone <- grepl("None", AllEnglish$Theory)

AllEnglishTheoryGrid <- data.frame(AllEnglish$TheorySocialCap, AllEnglish$TheoryHumanCap, AllEnglish$TheoryCulturalCap, AllEnglish$TheoryWorldCult, AllEnglish$TheoryHumanRights, AllEnglish$TheoryCritical, AllEnglish$TheoryPolitical, AllEnglish$TheoryNone)
names(AllEnglishTheoryGrid) <- TheoryNames

## Others
## ======

Other <- data.frame(Private$ID, (others <- gsub(sprintf("(,\\s)?(%s)(,\\s)?", paste(TheoryNames, collapse = "|")), "", 
                                                sub(".*\\|\\s(.*)", "\\1", Private$Theory))), Private$Reviewer)
Other[Other==""] <- NA
TheoryOther <- na.omit(Other)


# Framing
# ============================

FramedNames <- c("Related to curriculum coverage / ed. quality / exam prep", "Corruption", "Equity", "Supply & Demand / Economic Efficiency", " Social Cohesion", "Global Trend", "Household Expenditures")

## Private
## ======
Private$FramedCurric <- grepl("curriculum", Private$Framed)
Private$FramedCorruption <- grepl("Corruption", Private$Framed)
Private$FramedEquity <- grepl("Equity", Private$Framed)
Private$FramedEcon <- grepl("Economic", Private$Framed)
Private$FramedCohesion <- grepl("Social", Private$Framed)
Private$FramedTrend <- grepl("Global", Private$Framed)
Private$FramedHousehold <- grepl("Household", Private$Framed)

PrivateFramedGrid <- data.frame(Private$FramedCurric, Private$FramedCorruption, Private$FramedEquity, Private$FramedEcon, Private$FramedCohesion, Private$FramedTrend, Private$FramedHousehold)
names(PrivateFramedGrid) <- FramedNames

## AllAsia
## ======
AllAsia$FramedCurric <- grepl("curriculum", AllAsia$Framed)
AllAsia$FramedCorruption <- grepl("Corruption", AllAsia$Framed)
AllAsia$FramedEquity <- grepl("Equity", AllAsia$Framed)
AllAsia$FramedEcon <- grepl("Economic", AllAsia$Framed)
AllAsia$FramedCohesion <- grepl("Social", AllAsia$Framed)
AllAsia$FramedTrend <- grepl("Global", AllAsia$Framed)
AllAsia$FramedHousehold <- grepl("Household", AllAsia$Framed)

AllAsiaFramedGrid <- data.frame(AllAsia$FramedCurric, AllAsia$FramedCorruption, AllAsia$FramedEquity, AllAsia$FramedEcon, AllAsia$FramedCohesion, AllAsia$FramedTrend, AllAsia$FramedHousehold)
names(AllAsiaFramedGrid) <- FramedNames

## AllChinese
## ======
AllChinese$FramedCurric <- grepl("curriculum", AllChinese$Framed)
AllChinese$FramedCorruption <- grepl("Corruption", AllChinese$Framed)
AllChinese$FramedEquity <- grepl("Equity", AllChinese$Framed)
AllChinese$FramedEcon <- grepl("Economic", AllChinese$Framed)
AllChinese$FramedCohesion <- grepl("Social", AllChinese$Framed)
AllChinese$FramedTrend <- grepl("Global", AllChinese$Framed)
AllChinese$FramedHousehold <- grepl("Household", AllChinese$Framed)

AllChineseFramedGrid <- data.frame(AllChinese$FramedCurric, AllChinese$FramedCorruption, AllChinese$FramedEquity, AllChinese$FramedEcon, AllChinese$FramedCohesion, AllChinese$FramedTrend, AllChinese$FramedHousehold)
names(AllChineseFramedGrid) <- FramedNames

## AllEnglish
## ======
AllEnglish$FramedCurric <- grepl("curriculum", AllEnglish$Framed)
AllEnglish$FramedCorruption <- grepl("Corruption", AllEnglish$Framed)
AllEnglish$FramedEquity <- grepl("Equity", AllEnglish$Framed)
AllEnglish$FramedEcon <- grepl("Economic", AllEnglish$Framed)
AllEnglish$FramedCohesion <- grepl("Social", AllEnglish$Framed)
AllEnglish$FramedTrend <- grepl("Global", AllEnglish$Framed)
AllEnglish$FramedHousehold <- grepl("Household", AllEnglish$Framed)

AllEnglishFramedGrid <- data.frame(AllEnglish$FramedCurric, AllEnglish$FramedCorruption, AllEnglish$FramedEquity, AllEnglish$FramedEcon, AllEnglish$FramedCohesion, AllEnglish$FramedTrend, AllEnglish$FramedHousehold)
names(AllEnglishFramedGrid) <- FramedNames

## Others
## ======

Other <- data.frame(Private$ID, (others <- gsub(sprintf("(,\\s)?(%s)(,\\s)?", paste(FramedNames, collapse = "|")), "", 
                                                sub(".*\\|\\s(.*)", "\\1", Private$Framed))), Private$Reviewer)
Other[Other==""] <- NA
FramedOther <- na.omit(Other)

## Equity Convern

EquityConcernGrid <- data.frame(Private$ID, Private$EquityDefine, Private$Reviewer)
EquityConcernGrid <- na.omit(EquityConcernGrid)

# How is private tutoring viewed
# ============================

HowViewedNames <- c("Raises concerns", "Discusses benefits", "Both", "Neither")

## Private
## ======
Private$HowViewedConcerns <- grepl("Raises concerns", Private$HowViewed)
Private$HowViewedBenefits <- grepl("Discusses benefits", Private$HowViewed)
Private$HowViewedBoth <- grepl("Addresses both benefits and concerns", Private$HowViewed)
Private$HowViewedNeither <- grepl("Does not address benefits or concerns", Private$HowViewed)

PrivateHowViewedGrid <- data.frame(Private$HowViewedConcerns, Private$HowViewedBenefits, Private$HowViewedBoth, Private$HowViewedNeither)
names(PrivateHowViewedGrid) <- HowViewedNames

## AllAsia
## ======
AllAsia$HowViewedConcerns <- grepl("Raises concerns", AllAsia$HowViewed)
AllAsia$HowViewedBenefits <- grepl("Discusses benefits", AllAsia$HowViewed)
AllAsia$HowViewedBoth <- grepl("Addresses both benefits and concerns", AllAsia$HowViewed)
AllAsia$HowViewedNeither <- grepl("Does not address benefits or concerns", AllAsia$HowViewed)

AllAsiaHowViewedGrid <- data.frame(AllAsia$HowViewedConcerns, AllAsia$HowViewedBenefits, AllAsia$HowViewedBoth, AllAsia$HowViewedNeither)
names(AllAsiaHowViewedGrid) <- HowViewedNames

## AllChinese
## ======
AllChinese$HowViewedConcerns <- grepl("Raises concerns", AllChinese$HowViewed)
AllChinese$HowViewedBenefits <- grepl("Discusses benefits", AllChinese$HowViewed)
AllChinese$HowViewedBoth <- grepl("Addresses both benefits and concerns", AllChinese$HowViewed)
AllChinese$HowViewedNeither <- grepl("Does not address benefits or concerns", AllChinese$HowViewed)

AllChineseHowViewedGrid <- data.frame(AllChinese$HowViewedConcerns, AllChinese$HowViewedBenefits, AllChinese$HowViewedBoth, AllChinese$HowViewedNeither)
names(AllChineseHowViewedGrid) <- HowViewedNames

## AllEnglish
## ======
AllEnglish$HowViewedConcerns <- grepl("Raises concerns", AllEnglish$HowViewed)
AllEnglish$HowViewedBenefits <- grepl("Discusses benefits", AllEnglish$HowViewed)
AllEnglish$HowViewedBoth <- grepl("Addresses both benefits and concerns", AllEnglish$HowViewed)
AllEnglish$HowViewedNeither <- grepl("Does not address benefits or concerns", AllEnglish$HowViewed)

AllEnglishHowViewedGrid <- data.frame(AllEnglish$HowViewedConcerns, AllEnglish$HowViewedBenefits, AllEnglish$HowViewedBoth, AllEnglish$HowViewedNeither)
names(AllEnglishHowViewedGrid) <- HowViewedNames