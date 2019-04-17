
# Processing monitor data

#Take raw counts which is in different monitors and conditions and combine them. 


## Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)
library(chron)

# Loading Monitor data

m1<-read.table("C:/Users/dtussey/Desktop/Monitor1.txt")
m1$monitor<-rep(1,length(m1$V1))
m7<-read.table("C:/Users/dtussey/Desktop/Monitor7.txt")
m7$monitor<-rep(7,length(m7$V1))

allm<-rbind(m1,m7) # combining all of the monitor data
allm$V3<-match(allm$V3,month.abb) # converint month to numeric

allm$V2<-paste("20",allm$V4,"-",allm$V3,"-",formatC(allm$V2,width=2,flag="0"),sep="") # reformatting the date

allm<-allm[,-3:-4]# excluding time we dont need
allm<-allm[,-5:-9]# excluding time we dont need
head(allm)

allm<-allm%>%
  filter(V6==1)

names(allm)<-c("time_step","date","time","trik_status","light_status",paste("t",seq(1,32),sep=""),"monitor")
names(allm)

allm$fulltime<-ymd_hms(paste(allm$date,allm$time,sep=""),tz="US/Eastern")   #Line 103

fwrite(allm,"C:/Users/dtussey/Desktop/monitorscombined.csv")###wide format; gitignore because file is too large (> 100mb)



monitors.long<-gather(allm,position,counts,t1:t32)
monitors.long$position<-as.numeric(substr(monitors.long$position,2,4))
head(monitors.long)
#monitors.long$fulltime<-ymd_hms(head(monitors.long$fulltime),tz="US/Eastern")
monitors.long$fulltime<-chron(dates=monitors.long$date,times=monitors.long$time,format=c('y-m-d','h:m:s'))
# write out dataset, long format
fwrite(monitors.long,"C:/Users/dtussey/Desktop/monitorscombined_long.csv")     #Line 125

monitors.long.2<-monitors.long[which(monitors.long$date<"2018-8-14"&monitors.long$monitor==1),]
fwrite(monitors.long.2,"C:/Users/dtussey/Desktop/Tri_1_long.csv")

#REP A
monitors.long.2<-monitors.long[which(monitors.long$date<"2018-8-14"&monitors.long$monitor==1),]
monitors.long.3<-monitors.long.2[which(monitors.long.2$time_step<37232),]
monitors.long.3<-monitors.long.3[which(monitors.long.3$time_step>22831),]


#REP B
monitors.long.4<-monitors.long[which(monitors.long$date>"2018-8-27"&monitors.long$monitor==1),]
monitors.long.4<-monitors.long.4[which(monitors.long.4$date<"2018-9-11"),]
monitors.long.5<-monitors.long.4[which(monitors.long.4$time_step<37232),]
monitors.long.5<-monitors.long.5[which(monitors.long.5$time_step>22831),]


fwrite(monitors.long.4,"C:/Users/dtussey/Desktop/Tri_2_long.csv")
monitors.long.5<-read.csv("C:/Users/dtussey/Desktop/Tri_2_long.csv")
monitors.long.5<-monitors.long.5[which(monitors.long.5$time_step>15361),]
monitors.long.5<-monitors.long.5[which(monitors.long.5$time_step<388481),]

aggregate(monitors.long.5$counts, by=list(Category=monitors.long.5$position), FUN=sum)


#Rep C

monitors.long.7<-monitors.long[which(monitors.long$monitor==7),]
monitors.long.7<-monitors.long.7[which(monitors.long.7$time_step>"27705"),]
monitors.long.7<-monitors.long.7[which(monitors.long.7$time_step<"42107"),]

aggregate(monitors.long.7$counts, by=list(Category=monitors.long.7$position), FUN=sum)



head(allm)

myfunc <- function(x,y){allm[allm$date >= x & allm$date <= y,]}

DATE1 <- as.Date("2018-8-1")
DATE2 <- as.Date("2018-8-11")
DATE3 <- as.Date("2018-8-28")
DATE4 <- as.Date("2018-9-8")
DATE5 <- as.Date("2018-9-24")
DATE6 <- as.Date("2018-10-04")

Rep.A<- myfunc(DATE1,DATE2)
Rep.A$uniqueID<-rep("Rep.A",nrow(Rep.A))
Rep.B<- myfunc(DATE3,DATE4)
Rep.B$uniqueID<-rep("Rep.B",nrow(Rep.B))
Rep.C<- myfunc(DATE5,DATE6)
Rep.C$uniqueID<-rep("Rep.C",nrow(Rep.C))






All.reps<-rbind(Rep.A,Rep.B,Rep.C)
All.reps$uniqueID<-as.factor(All.reps$uniqueID)
All.reps$date<-as.factor(All.reps$date)
names(All.reps)
head(All.reps)
dim(All.reps)
ggplot(All.reps,aes(x=date,y=t13))+geom_line()+facet_grid(uniqueID~.,scale="free")

All.reps$time2<-hour(hms(All.reps$fulltime))+minute(hms(all.data$fulltime))/60

bins60=c(paste0(rep(c(paste0(0,0:9),10:23), each=1),":", c("00"))[-1],"24:00")
All.reps$time<-as.numeric(All.reps$time)
All.reps$bins60=cut(All.reps$time,breaks=seq(0,24,1),labels=bins60)

All.reps

Rep.A2<-data.frame(t(amove[-1]))  
colnames(Rep.A2) <- amove[, 1]



amove <- Rep.A %>% 
  group_by(date) %>%
  summarise(t1= sum(t1),t2= sum(t2),t3= sum(t3),t4= sum(t4),t5= sum(t5),t6= sum(t6),t7= sum(t7),
            t8= sum(t8),t9= sum(t9),t10= sum(t10),t11= sum(t11),t12= sum(t12),t13= sum(t13),t14= sum(t14),t15= sum(t15),
            t16= sum(t16),t17= sum(t17),t18= sum(t18),t19= sum(t19),t20= sum(t20),t21= sum(t21),t22= sum(t22),t23= sum(t23),
            t24= sum(t24),t25= sum(t25),t26= sum(t26),t27= sum(t27),t28= sum(t28),t29= sum(t29),t30= sum(t30),t31= sum(t31),t32= sum(t32))

bmove <- Rep.B %>% 
  group_by(date) %>%
  summarise(t1= sum(t1),t2= sum(t2),t3= sum(t3),t4= sum(t4),t5= sum(t5),t6= sum(t6),t7= sum(t7),
            t8= sum(t8),t9= sum(t9),t10= sum(t10),t11= sum(t11),t12= sum(t12),t13= sum(t13),t14= sum(t14),t15= sum(t15),
            t16= sum(t16),t17= sum(t17),t18= sum(t18),t19= sum(t19),t20= sum(t20),t21= sum(t21),t22= sum(t22),t23= sum(t23),
            t24= sum(t24),t25= sum(t25),t26= sum(t26),t27= sum(t27),t28= sum(t28),t29= sum(t29),t30= sum(t30),t31= sum(t31),t32= sum(t32))


cmove <- Rep.C %>% 
  group_by(date) %>%
  summarise(t1= sum(t1),t2= sum(t2),t3= sum(t3),t4= sum(t4),t5= sum(t5),t6= sum(t6),t7= sum(t7),
            t8= sum(t8),t9= sum(t9),t10= sum(t10),t11= sum(t11),t12= sum(t12),t13= sum(t13),t14= sum(t14),t15= sum(t15),
            t16= sum(t16),t17= sum(t17),t18= sum(t18),t19= sum(t19),t20= sum(t20),t21= sum(t21),t22= sum(t22),t23= sum(t23),
            t24= sum(t24),t25= sum(t25),t26= sum(t26),t27= sum(t27),t28= sum(t28),t29= sum(t29),t30= sum(t30),t31= sum(t31),t32= sum(t32))


fwrite(amove,"C:/Users/dtussey/Desktop/RepA_tri.csv")
fwrite(bmove,"C:/Users/dtussey/Desktop/RepB_tri.csv")
fwrite(cmove,"C:/Users/dtussey/Desktop/RepC_tri.csv")

###convert time to hours   
all.data$time2<-hour(hms(all.data$time))+minute(hms(all.data$time))/60
```

### 1 hour bin 
```{r}
bins60=c(paste0(rep(c(paste0(0,0:9),10:23), each=1),":", c("00"))[-1],"24:00")
all.data$bins60=cut(all.data$time2,breaks=seq(0,24,1),labels=bins60)
head(all.data)
#all.data2<-ddply(all.data,.(date,uniqueID,experiment,bins60),transform,time=seq(1,length(bins60),1))
nall.data<-ddply(all.data,.(uniqueID,date,experiment,bins60),summarize,counts=sum(counts))
nall.data2<-na.omit(ddply(nall.data,.(uniqueID,experiment),transform,order(date)))
nall.data3<-ddply(nall.data2,.(uniqueID),transform,time=seq(1,length(bins60),1))
#counts15[order(as.Date(counts15$date,format ="%Y-%m-%d")),]
dim(nall.data3)
head(nall.data3)
ggplot(nall.data3,aes(x=time,y=counts))+geom_line()+facet_grid(uniqueID~.,scales="free")#+geom_vline(xintercept=seq(0,900,24))#+geom_vline(xintercept=192)