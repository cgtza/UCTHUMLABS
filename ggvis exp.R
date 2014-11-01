#   Analyse data for HUM Computer Labs and put daily graph out

# Load data and packages
load("labdatahistorical.RData")
if(!require(lubridate))install.packages("lubridate")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(dplyr))install.packages("dplyr")
if(!require(tidyr))install.packages("tidyr")

#   Select only HUM labs
labdata$room<-toupper(labdata$room)
labdata<-filter(labdata,grepl('HUM',room))
rownames(labdata)<-NULL
labdata$room<-factor(labdata$room)

#   By most recent day
filen<-paste(today(),".pdf")
pdf(paste(filen),width=10,height=7) # INitialise pdf file device
labdataper<-labdata %>%
  filter(day==day(now()) & month==month(now(),label=T) & year==year(now()))

# Compute times usage exceeded 95%
labdataper2<-select(labdataper,vacantp)
maxcount<-dim(filter(labdataper2,vacantp <.0501))[1]
pastemax<-paste("95% capacity exceeded ",maxcount," times")

# Gather usage data into column to enable stacked barplot
labdataper<-gather(labdataper,usagecat,value,inusep:notconnectedp)

# Compute means of usage for use by ggplot
labdataper<-group_by(labdataper,room,hour,usagecat) %>%
summarise(value=mean(value))


# Draw the graph
ggplot(labdataper, aes(x=room,y=value, fill=usagecat)) + 
    geom_bar(stat="identity",color="black",lwd=.5)+ coord_flip()+ facet_wrap(~hour)+
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

# Stop writing to pdf file
    dev.off()

