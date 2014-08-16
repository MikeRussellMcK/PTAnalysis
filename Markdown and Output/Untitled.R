ResearchTypeGrid$AsianRegionOriginal <- grepl("Original", Private$ResearchType) & grepl("TRUE", Private$AsiaYN)
ResearchTypeGrid$AsianRegionReview <- grepl("Review", Private$ResearchType) & grepl("TRUE", Private$AsiaYN)
ResearchTypeGrid$AsianRegionPolicy <- grepl("Policy", Private$ResearchType) & grepl("TRUE", Private$AsiaYN)

ResearchTypeGrid$EnglishLangOriginal <- grepl("Original", Private$ResearchType) & grepl("English", Private$PubLang)
ResearchTypeGrid$EnglishLangReview <- grepl("Review", Private$ResearchType) & grepl("English", Private$PubLang)
ResearchTypeGrid$EnglishLangPolicy <- grepl("Policy", Private$ResearchType) & grepl("English", Private$PubLang)

ResearchTypeGrid$ChineseLangOriginal <- grepl("Original", Private$ResearchType) & grepl("Chinese", Private$PubLang)
ResearchTypeGrid$ChineseLangReview <- grepl("Review", Private$ResearchType) & grepl("Chinese", Private$PubLang)
ResearchTypeGrid$ChineseLangPolicy <- grepl("Policy", Private$ResearchType) & grepl("Chinese", Private$PubLang)