#   Analyse data for COmputer Labs

load("c:/labdatahistorical.RData")
if(!require(lubridate))install.packages("lubridate")
colnames(labdata)[2] <- "inuse"
if(!require(ggplot2))install.packages("ggplot2")
if(!require(plyr))install.packages("plyr")
if(!require(dplyr))install.packages("dplyr")


#   Select only HUM
labdata$room<-toupper(labdata$room)
labdata<-labdata[grep("HUM",labdata$room),]
rownames(labdata)<-NULL
labdata$room<-factor(labdata$room)



#   Convert to long format
library(reshape2)
labdata <- melt(labdata,id=c("room","year","month","week","day","hour","minute","weekday"))



#   By most recent day
filen<-paste("d:/Dropbox/Labdata/",today(),".pdf")
pdf(paste(filen),width=10,height=7)

labdataper<-labdata %>%
  filter(day==day(now()) & month==month(now(),label=T) & year==year(now()))


labdataper2<-labdataper[grep("vacantp",labdataper$variable),]
tempdat<-ddply(labdataper2, .(hour,variable,room), summarize,
               mean = round(mean(value), 2),
               sd = round(sd(value), 2),
               min=round(min(value),2))
tempdat2<-tempdat[(tempdat$min < .051),]
maxcount<-nrow(tempdat2[tempdat2$variable==c('vacantp'),])

pastemax<-paste("95% capacity exceeded ",maxcount," times")

labdataper<-labdataper[grep("p",labdataper$variable),]
x<-group_by(labdataper,room,hour,variable) %>%
summarise(value=mean(value))



ggplot(x, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity",color="black",lwd=.5)+
    coord_flip()+
    facet_wrap(~hour)+
    ggtitle(paste("Daily report, HUM lab usage per hour"," ",today()))+
    theme(plot.title = element_text(lineheight=2, face="bold",size=18,colour="#990000"))+
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=10),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=5))+
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=10),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=4),plot.title=element_text(vjust=2.12))+
    theme(legend.text = element_text(colour="grey50", size = 10, face = "bold"))+
    ylab("Mean usage per hour (% of capacity)")+xlab("Laboratory")+
    scale_fill_discrete(name  =paste(pastemax),
                              breaks=c("inusep", "vacantp","notconnectedp"),
                              labels=c("In use", "Not in use", "Not connected"))
    #scale_y_continuous(labels = c("0","25","50","75","100"))

    dev.off()

