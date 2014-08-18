#Find Dupes

Dupes <- subset(Private, duplicated(Private$ID)|duplicated(Private$ID, fromLast=TRUE), select=c(ID, Reviewer, Publisher, PubLang)) # New DF with only duplicates
Dupes <- na.omit(Dupes) # Deletes NAs
Dupes <- Dupes[order(Dupes$ID),] # Sorts rows
#write.xlsx(Dupes, file="../Excel Output/Duplicates.xlsx")