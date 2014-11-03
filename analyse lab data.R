#   Analyse data for COmputer Labs

load("c:/labdatahistorical.RData")
if(!require(lubridate))install.packages("lubridate")
colnames(labdata)[2] <- "inuse"


#   Select only HUM

labdata<-labdata[grep("HUM",labdata$room),]
rownames(labdata)<-NULL
labdata$room<-factor(labdata$room)



#   Convert to long format

library(reshape2)
labdata <- melt(labdata,id=c("room","year","month","week","day","hour","minute","weekday"))


#   Select percentages

labdataper<-labdata[grep("p",labdata$variable),]
rownames(labdata)<-NULL




#   Stacked bar chart


#   Detailed

ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(year~month~week~day~hour~minute)+
    guides(fill=guide_legend(title=NULL))


#   Total usage
ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    guides(fill=guide_legend(title=NULL))


#   By year
ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(~year)+
    guides(fill=guide_legend(title=NULL))

#   By month
ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(~month)+
    guides(fill=guide_legend(title=NULL))

#   By week
ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(~week)+
    guides(fill=guide_legend(title=NULL))

#   By weekday
ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(~weekday)+
    guides(fill=guide_legend(title=NULL))

#   By hour
ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(~hour)+
    guides(fill=guide_legend(title=NULL))

#   By most recent day
labdataper<-subset(labdataper,day==day(now()))
ggplot(labdataper, aes(x=room,y=value/3, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(~hour)+
    guides(fill=guide_legend(title=NULL))+
    ggtitle(paste("Daily report, HUM lab usage per hour"," ",today()))+
    theme(plot.title = element_text(lineheight=2, face="bold",size=20))+
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=16))+
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=16))+
    theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))

#   By most recent week
labdataper<-labdataper["week"==week(now()-dweeks(1)),]
ggplot(labdataper, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(hour~day)+
    guides(fill=guide_legend(title=NULL))

#   By today
labdata2per<-subset(labdataper,day==day(now()))
ggplot(labdata2per, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    facet_wrap(~hour)+
    guides(fill=guide_legend(title=NULL))


#   Now
labdata2per<-subset(labdataper,(day==day(now())))
labdata2per<-subset(labdata2per,(hour==hour(now())))
ggplot(labdata2per, aes(x=room,y=value, fill=variable)) + 
    geom_bar(stat="identity")+
    coord_flip()+
    guides(fill=guide_legend(title=NULL))

