if(!require(XML))install.packages("XML")
if(!require(lubridate))install.packages("lubridate")

load("labdatahistorical.RData")


theurl <- "http://labs.uct.ac.za:8080/veralab/StudentPortalAction.do"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
tables[9]
labdatanew<-as.data.frame(tables[9])
colnames(labdatanew)<-c("room","total","vacant","notconnected")
labdatanew$total<-as.numeric(as.character(labdatanew$total))
labdatanew$vacant<-as.numeric(as.character(labdatanew$vacant))
labdatanew$notconnected<-as.numeric(as.character(labdatanew$notconnected))


labdatanew$year<-year(now())
labdatanew$month<-month(now(),label=T)
labdatanew$week<-week(now())
labdatanew$day<-day(now())
labdatanew$weekday<-wday(now(),label=T)
labdatanew$hour<-hour(now())
labdatanew$minute<-minute(now())
labdatanew$inusep<-(labdatanew$total-labdatanew$vacant)/(labdatanew$total+labdatanew$notconnected)
labdatanew$vacantp<-(labdatanew$vacant)/(labdatanew$total+labdatanew$notconnected)
labdatanew$notconnectedp<-(labdatanew$notconnected)/(labdatanew$total+labdatanew$notconnected)

labdata<-rbind(labdata,labdatanew)

#labdata<-labdatanew
save(labdata,file="labdatahistorical.RData")
