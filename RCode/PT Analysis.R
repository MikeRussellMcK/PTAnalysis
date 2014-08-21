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

# Sets all entries for the graphing
PrivatePubLangLabel <- as.data.frame(colSums(PrivatePubLangGrid))
PrivatePubLangLabel <- setNames(cbind(rownames(PrivatePubLangLabel), PrivatePubLangLabel, row.names = NULL), c("x", "Freq"))
PrivatePubLangLabel$lab <- as.character(round(100 * PrivatePubLangLabel$Freq / sum(PrivatePubLangLabel$Freq)))
PrivatePubLangLabel$lab <- paste(PrivatePubLangLabel$Freq,paste("(",PrivatePubLangLabel$lab,"%)",sep=""),sep=" ")

AllAsiaPubLangGrid <- data.frame(grepl("Chinese", AllAsia$PubLang), grepl("English", AllAsia$PubLang), (!grepl("Chinese", AllAsia$PubLang) & !grepl("English", AllAsia$PubLang)))
names(AllAsiaPubLangGrid) <- c("Chinese", "English", "Other")

# Sets Asian Region for the graphing
AllAsiaPubLangLabel <- as.data.frame(colSums(AllAsiaPubLangGrid))
AllAsiaPubLangLabel <- setNames(cbind(rownames(AllAsiaPubLangLabel), AllAsiaPubLangLabel, row.names = NULL), c("x", "Freq"))
AllAsiaPubLangLabel$lab <- as.character(round(100 * AllAsiaPubLangLabel$Freq / sum(AllAsiaPubLangLabel$Freq)))
AllAsiaPubLangLabel$lab <- paste(AllAsiaPubLangLabel$Freq,paste("(",AllAsiaPubLangLabel$lab,"%)",sep=""),sep=" ")

# Type of Research
# =================

#Make the Private T/F Grid
Private$ResearchTypeOriginal <- grepl("Original", Private$ResearchType)
Private$ResearchTypeReview <- grepl("Review", Private$ResearchType)
Private$ResearchTypePolicy <- grepl("Policy", Private$ResearchType)

PrivateResearchTypeGrid <- data.frame(Private$ResearchTypeOriginal, Private$ResearchTypeReview, Private$ResearchTypePolicy)
names(PrivateResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

# Sets up the Private Graph
PrivateResearchTypeLabel <- as.data.frame(colSums(PrivateResearchTypeGrid))
PrivateResearchTypeLabel <- setNames(cbind(rownames(PrivateResearchTypeLabel), PrivateResearchTypeLabel, row.names = NULL), c("x", "Freq"))
PrivateResearchTypeLabel$lab <- as.character(round(100 * PrivateResearchTypeLabel$Freq / sum(PrivateResearchTypeLabel$Freq)))
PrivateResearchTypeLabel$lab <- paste(PrivateResearchTypeLabel$Freq,paste("(",PrivateResearchTypeLabel$lab,"%)",sep=""),sep=" ")

#Make the AllAsia T/F Grid
AllAsia$ResearchTypeOriginal <- grepl("Original", AllAsia$ResearchType)
AllAsia$ResearchTypeReview <- grepl("Review", AllAsia$ResearchType)
AllAsia$ResearchTypePolicy <- grepl("Policy", AllAsia$ResearchType)

AllAsiaResearchTypeGrid <- data.frame(AllAsia$ResearchTypeOriginal, AllAsia$ResearchTypeReview, AllAsia$ResearchTypePolicy)
names(AllAsiaResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

# Sets up the AllAsia Graph
AllAsiaResearchTypeLabel <- as.data.frame(colSums(AllAsiaResearchTypeGrid))
AllAsiaResearchTypeLabel <- setNames(cbind(rownames(AllAsiaResearchTypeLabel), AllAsiaResearchTypeLabel, row.names = NULL), c("x", "Freq"))
AllAsiaResearchTypeLabel$lab <- as.character(round(100 * AllAsiaResearchTypeLabel$Freq / sum(AllAsiaResearchTypeLabel$Freq)))
AllAsiaResearchTypeLabel$lab <- paste(AllAsiaResearchTypeLabel$Freq,paste("(",AllAsiaResearchTypeLabel$lab,"%)",sep=""),sep=" ")

#Make the AllChinese T/F Grid
AllChinese$ResearchTypeOriginal <- grepl("Original", AllChinese$ResearchType)
AllChinese$ResearchTypeReview <- grepl("Review", AllChinese$ResearchType)
AllChinese$ResearchTypePolicy <- grepl("Policy", AllChinese$ResearchType)

AllChineseResearchTypeGrid <- data.frame(AllChinese$ResearchTypeOriginal, AllChinese$ResearchTypeReview, AllChinese$ResearchTypePolicy)
names(AllChineseResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

# Sets up the AllChinese Graph
AllChineseResearchTypeLabel <- as.data.frame(colSums(AllChineseResearchTypeGrid))
AllChineseResearchTypeLabel <- setNames(cbind(rownames(AllChineseResearchTypeLabel), AllChineseResearchTypeLabel, row.names = NULL), c("x", "Freq"))
AllChineseResearchTypeLabel$lab <- as.character(round(100 * AllChineseResearchTypeLabel$Freq / sum(AllChineseResearchTypeLabel$Freq)))
AllChineseResearchTypeLabel$lab <- paste(AllChineseResearchTypeLabel$Freq,paste("(",AllChineseResearchTypeLabel$lab,"%)",sep=""),sep=" ")

#Make the AllEnglish T/F Grid
AllEnglish$ResearchTypeOriginal <- grepl("Original", AllEnglish$ResearchType)
AllEnglish$ResearchTypeReview <- grepl("Review", AllEnglish$ResearchType)
AllEnglish$ResearchTypePolicy <- grepl("Policy", AllEnglish$ResearchType)

AllEnglishResearchTypeGrid <- data.frame(AllEnglish$ResearchTypeOriginal, AllEnglish$ResearchTypeReview, AllEnglish$ResearchTypePolicy)
names(AllEnglishResearchTypeGrid) <- c("Original emperical research", "Review of other research", "Policy analysis")

# Sets up the AllEnglish Graph
AllEnglishResearchTypeLabel <- as.data.frame(colSums(AllEnglishResearchTypeGrid))
AllEnglishResearchTypeLabel <- setNames(cbind(rownames(AllEnglishResearchTypeLabel), AllEnglishResearchTypeLabel, row.names = NULL), c("x", "Freq"))
AllEnglishResearchTypeLabel$lab <- as.character(round(100 * AllEnglishResearchTypeLabel$Freq / sum(AllEnglishResearchTypeLabel$Freq)))
AllEnglishResearchTypeLabel$lab <- paste(AllEnglishResearchTypeLabel$Freq,paste("(",AllEnglishResearchTypeLabel$lab,"%)",sep=""),sep=" ")

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

# Publication Year
PrivatePubYearLabel <- as.data.frame(table(Private$PubYear))
colnames(PrivatePubYearLabel)[1] <- "x"
PrivatePubYearLabel$lab <- as.character(round(100 * PrivatePubYearLabel$Freq / sum(PrivatePubYearLabel$Freq)))
PrivatePubYearLabel$lab <- paste(PrivatePubYearLabel$Freq,paste("(",PrivatePubYearLabel$lab,"%)",sep=""),sep=" ")

AllAsiaPubYearLabel <- as.data.frame(table(AllAsia$PubYear))
colnames(AllAsiaPubYearLabel)[1] <- "x"
AllAsiaPubYearLabel$lab <- as.character(round(100 * AllAsiaPubYearLabel$Freq / sum(AllAsiaPubYearLabel$Freq)))
AllAsiaPubYearLabel$lab <- paste(AllAsiaPubYearLabel$Freq,paste("(",AllAsiaPubYearLabel$lab,"%)",sep=""),sep=" ")

AllChinesePubYearLabel <- as.data.frame(table(AllChinese$PubYear))
colnames(AllChinesePubYearLabel)[1] <- "x"
AllChinesePubYearLabel$lab <- as.character(round(100 * AllChinesePubYearLabel$Freq / sum(AllChinesePubYearLabel$Freq)))
AllChinesePubYearLabel$lab <- paste(AllChinesePubYearLabel$Freq,paste("(",AllChinesePubYearLabel$lab,"%)",sep=""),sep=" ")

AllEnglishPubYearLabel <- as.data.frame(table(AllEnglish$PubYear))
colnames(AllEnglishPubYearLabel)[1] <- "x"
AllEnglishPubYearLabel$lab <- as.character(round(100 * AllEnglishPubYearLabel$Freq / sum(AllEnglishPubYearLabel$Freq)))
AllEnglishPubYearLabel$lab <- paste(AllEnglishPubYearLabel$Freq,paste("(",AllEnglishPubYearLabel$lab,"%)",sep=""),sep=" ")


# Publication type
# =================

PrivatePubTypeLabel <- as.data.frame(table(Private$PubType))
colnames(PrivatePubTypeLabel)[1] <- "x"
PrivatePubTypeLabel$lab <- as.character(round(100 * PrivatePubTypeLabel$Freq / sum(PrivatePubTypeLabel$Freq)))
PrivatePubTypeLabel$lab <- paste(PrivatePubTypeLabel$Freq,paste("(",PrivatePubTypeLabel$lab,"%)",sep=""),sep=" ")

AllAsiaPubTypeLabel <- as.data.frame(table(AllAsia$PubType))
colnames(AllAsiaPubTypeLabel)[1] <- "x"
AllAsiaPubTypeLabel$lab <- as.character(round(100 * AllAsiaPubTypeLabel$Freq / sum(AllAsiaPubTypeLabel$Freq)))
AllAsiaPubTypeLabel$lab <- paste(AllAsiaPubTypeLabel$Freq,paste("(",AllAsiaPubTypeLabel$lab,"%)",sep=""),sep=" ")

AllChinesePubTypeLabel <- as.data.frame(table(AllChinese$PubType))
colnames(AllChinesePubTypeLabel)[1] <- "x"
AllChinesePubTypeLabel$lab <- as.character(round(100 * AllChinesePubTypeLabel$Freq / sum(AllChinesePubTypeLabel$Freq)))
AllChinesePubTypeLabel$lab <- paste(AllChinesePubTypeLabel$Freq,paste("(",AllChinesePubTypeLabel$lab,"%)",sep=""),sep=" ")

AllEnglishPubTypeLabel <- as.data.frame(table(AllEnglish$PubType))
colnames(AllEnglishPubTypeLabel)[1] <- "x"
AllEnglishPubTypeLabel$lab <- as.character(round(100 * AllEnglishPubTypeLabel$Freq / sum(AllEnglishPubTypeLabel$Freq)))
AllEnglishPubTypeLabel$lab <- paste(AllEnglishPubTypeLabel$Freq,paste("(",AllEnglishPubTypeLabel$lab,"%)",sep=""),sep=" ")

#  Regions
# =================

RegionNames <- c("MENA", "Sub-Saharan", "Cent. Asia", "East Asia", "S. Asia", "S.E. Asia", "Aus. & NZ", "W. Europe", "E. Europe", "US & Can", "LAC", "Global")

# Private
# ======
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

# AllAsia
# =====

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

# AllChinese
# =====

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

# AllEnglish
# =====

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

# Others
# ======

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