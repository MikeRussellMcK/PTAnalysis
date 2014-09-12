#Find Dupes

Dupes <- subset(Private, duplicated(Private$ID)|duplicated(Private$ID, fromLast=TRUE), select=c(ID, Reviewer, Publisher, PubLang)) # New DF with only duplicates
Dupes <- na.omit(Dupes) # Deletes NAs
Dupes <- Dupes[order(Dupes$ID),] # Sorts rows
#write.xlsx(Dupes, file="../Excel Output/Duplicates.xlsx")

EngDupes <- subset(AllEnglish, duplicated(AllEnglish$ID)|duplicated(AllEnglish$ID, fromLast=TRUE), select=c(ID, Reviewer, Publisher, PubLang)) # New DF with only duplicates
EngDupes <- na.omit(EngDupes) # Deletes NAs
EngDupes <- EngDupes[order(EngDupes$ID),] # Sorts rows

ChDupes <- subset(AllChinese, duplicated(AllChinese$ID)|duplicated(AllChinese$ID, fromLast=TRUE), select=c(ID, Reviewer, Publisher, PubLang)) # New DF with only duplicates
ChDupes <- na.omit(ChDupes) # Deletes NAs
ChDupes <- ChDupes[order(ChDupes$ID),] # Sorts rows