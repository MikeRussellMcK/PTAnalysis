ggplot(data=AllAsiaPubLangLabel, aes(x=x, y=Freq)) + geom_bar(stat="identity") + ggtitle("Asia Region") +
  geom_text(size=3, data=DataSet1,aes(x=x,y=Freq,label=lab),hjust=-.1) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.background=element_blank(), plot.title=element_text(size=10)) + 
  scale_y_continuous(limits = c(0, 350)) + coord_flip()