setwd("~/Desktop/NTU商业分析项目 预习材料")
AMZN_1<- read.csv("AMZN.1.csv",stringsAsFactors = F)
plot(AMZN_1$Open,type = 'l',ylab = 'Open',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Low,type = 'l',ylab = 'low',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Volume,type = 'l',ylab = 'Volume',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$High,type = 'l',ylab = 'High',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Close_today,type = 'l',ylab = 'Close_today',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Daily_increase,type = 'l',ylab = 'Daily_increase',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Confirmed_increase,type = 'l',ylab = 'Confirmed_increase',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Death_increase,type = 'l',ylab = 'Death_increase',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Incident_Rate,type = 'l',ylab = 'Incident_Rate',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Total_Test_Results,type = 'l',ylab = 'Total_Test_Results',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Case_Fatality_Ratio,type = 'l',ylab = 'Case_Fatality_Ratio',xlab = '',col='red',pch=16,las=T)
plot(AMZN_1$Testing_Rate,type = 'l',ylab = 'Testing_Rate',xlab = '',col='red',pch=16,las=T)

library(stringr)
setwd('~/Desktop/csse_covid_19_daily_reports_us')

files=list.files('~/Desktop/csse_covid_19_daily_reports_us')
file_total=c()

for (i in 1:length(files)) {
  file_i=read.csv(files[i])
  file_i$date=str_sub(files[i],1,10)
  file_total=rbind(file_total,file_i)
  
}
year=str_sub(file_total$date,7,10)
day=str_sub(file_total$date,4,5)
month=str_sub(file_total$date,1,2)

file_total$date=paste0(year,'-',month,'-',day)

daily_confirm=aggregate(Confirmed~date,data=file_total,sum)
write.csv(daily_confirm,"~/Desktop/daily_confirm.csv",row.names = F)

Deaths=aggregate(Deaths~date,data=file_total,sum)
write.csv(Deaths,"~/Desktop/Deaths.csv",row.names = F)

Incident_Rate=aggregate(Incident_Rate~date,data=file_total,sum)
write.csv(Incident_Rate,"~/Desktop/Incident_Rate.csv",row.names = F)

Total_Test_Results=aggregate(Total_Test_Results~date,data=file_total,sum)
write.csv(Total_Test_Results,"~/Desktop/Total_Test_Results.csv",row.names = F)

Testing_Rate=aggregate(Testing_Rate~date,data=file_total,sum)
write.csv(Testing_Rate,"~/Desktop/Testing_Rate.csv",row.names = F)

Case_Fatality_Ratio=aggregate(Case_Fatality_Ratio~date,data=file_total,mean)
write.csv(Case_Fatality_Ratio,"~/Desktop/Case_Fatality_Ratio.csv",row.names = F)

setwd("~/Desktop/NTU商业分析项目 预习材料")
Total_data<- read.csv("e-commerce_data.csv",stringsAsFactors = F)
x=(Total_data$Confirmed_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest confirmed increase","lower confirmed increase","higher confirmed increase","highest confirmed increase")
        ,space = c(1),col='red',ylim=c(-0.1,0.4),axes=FALSE,main='confirmed increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Average7_Confirmed_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Average7_confirmed increase","lower Average7_confirmed increase","higher Average7_confirmed increase","highest Average7_confirmed increase")
        ,space = c(1),col='red',ylim=c(-0.1,0.5),axes=FALSE,main='Average7_confirmed increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Max7_Confirmed_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Max7_confirmed increase","lower Max7_confirmed increase","higher Max7_confirmed increase","highest Max7_confirmed increase")
        ,space = c(1),col='red',ylim=c(-0.1,0.4),axes=FALSE,main='Max7_confirmed increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Min7_Confirmed_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Min7_confirmed increase","lower Min7_confirmed increase","higher Min7_confirmed increase","highest Min7_confirmed increase")
        ,space = c(1),col='red',ylim=c(0,0.3),axes=FALSE,main='Min7_confirmed increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Varp7_Confirmed_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Varp7_confirmed increase","lower Varp7_confirmed increase","higher Varp7_confirmed increase","highest Varp7_confirmed increase")
        ,space = c(1),col='red',ylim=c(0,0.3),axes=FALSE,main='Varp7_confirmed increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Deaths_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Death increase","lower Death increase","higher Death increase","highest Death increase")
        ,space = c(1),col='red',ylim=c(-0.2,0.6),axes=FALSE,main='Death increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Average7_Deaths_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Average7_Death increase","lower Average7_Death increase","higher Average7_Death increase","highest Average7_Death increase")
        ,space = c(1),col='red',ylim=c(-0.2,0.4),axes=FALSE,main='Death Average7_increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Max7_Deaths_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Max7_Death increase","lower Max7_Death increase","higher Max7_Death increase","highest Max7_Death increase")
        ,space = c(1),col='red',ylim=c(-0.2,0.4),axes=FALSE,main='Max7_Death increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Min7_Deaths_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Min7_Death increase","lower Min7_Death increase","higher Min7_Death increase","highest Min7_Death increase")
        ,space = c(1),col='red',ylim=c(-0.1,0.2),axes=FALSE,main='Min7_Death increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Varp7_Deaths_increase)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Varp7_Death increase","lower Varp7_Death increase","higher Varp7_Death increase","highest Varp7_Death increase")
        ,space = c(1),col='red',ylim=c(-0.1,0.3),axes=FALSE,main='Varp7_Death increase and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Volume)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Volume","lower Volume","higher Volume","highest Volume")
        ,space = c(1),col='red',ylim=c(0,0.2),axes=FALSE,main='Volume and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Average7_Volume)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Average7_Volume","lower Average7_Volume","higher Average7_Volume","highest Average7_Volume")
        ,space = c(1),col='red',ylim=c(0,0.2),axes=FALSE,main='Average7_Volume and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Max7_Volume)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Max7_Volume","lower Max7_Volume","higher Max7_Volume","highest Max7_Volume")
        ,space = c(1),col='red',ylim=c(-0.05,0.2),axes=FALSE,main='Max7_Volume and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Min7_Volume)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Min7_Volume","lower Min7_Volume","higher Min7_Volume","highest Min7_Volume")
        ,space = c(1),col='red',ylim=c(0,0.2),axes=FALSE,main='Min7_Volume and Daily_increase')
Axis(side = 2,las=T)

x=(Total_data$Varp7_Volume)
y=(Total_data$Daily_increase)
quantile(x,c(0.25,0.50,0.75))
mean(y[x<=quantile(x,0.25) ])
mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)])
mean(y[x>quantile(x,0.75)])
X<-c(1,2,3,4)
Y<-c(mean(y[x<=quantile(x,0.25) ]),mean(y[x>quantile(x,0.25) & x<=quantile(x,0.50)])
     ,mean(y[x>quantile(x,0.50) &x<=quantile(x,0.75)]),mean(y[x>quantile(x,0.75)]))
barplot(Y,ylab='Daily_increase', names.arg=c("lowest Varp7_Volume","lower Varp7_Volume","higher Varp7_Volume","highest Varp7_Volume")
        ,space = c(1),col='red',ylim=c(-0.1,0.3),axes=FALSE,main='Varp7_Volume and Daily_increase')
Axis(side = 2,las=T)


setwd("~/Desktop/NTU商业分析项目 预习材料")
total_data <- read.csv("E-commerce_total_data.csv",stringsAsFactors = F)
total_data$Date=as.Date(total_data$Date)
train_data = total_data[total_data$Date<="2021-07-06",]
validation_data = total_data[total_data$Date>"2021-07-06",]

#reg_rate=lm(Daily_increase~Low+Average7_Low+Max7_Low+Min7_Low+Varp7_Low+Average30_Low+Max30_Low
            #+Min30_Low+Varp30_Low+Open+Average7_Open+Max7_Open+Min7_Open+Varp7_Open+Average30_Open
            #+Max30_Open+Min30_Open+Varp30_Open+Volume+Average7_Volume+Max7_Volume+Min7_Volume
            #+Varp7_Volume+Average30_Volume+Max30_Volume+Min30_Volume+Varp30_Volume+High +Average7_High
            #+Max7_High+Min7_High+Varp7_High+Average30_High+Max30_High+Min30_High+Varp30_High+Close+Average7_Close
            #+Max7_Close+Min7_Close+Varp7_Close+Average30_Close+Max30_Close+Min30_Close+Varp30_Close+Adjusted.Close
            #+Average7_Adjusted.Close+Max7_Adjusted.Close+Min7_Adjusted.Close+Varp7_Adjusted.Close+Average30_Adjusted.Close
            #+Max30_Adjusted.Close+Min30_Adjusted.Close+Varp30_Adjusted.Close+Confirmed+Average7_Confirmed+Max7_Confirmed
            #+Min7_Confirmed+Varp7_Confirmed+Average30_Confirmed+Max30_Confirmed+Min30_Confirmed+Varp30_Confirmed
            #+Confirmed_increase+Average7_Confirmed_increase+Max7_Confirmed_increase+Min7_Confirmed_increase
            #+Varp7_Confirmed_increase+Average30_Confirmed_increase+Max30_Confirmed_increase+Min30_Confirmed_increase
            #+Varp30_Confirmed_increase+Deaths_increase+Average7_Deaths_increase+Max7_Deaths_increase
            #+Min7_Deaths_increase+Varp7_Deaths_increase+Average30_Deaths_increase+Max30_Deaths_increase
            #+Min30_Deaths_increase+Varp30_Deaths_increase+Deaths+Average7_Deaths+Max7_Deaths+Min7_Deaths
            #+Varp7_Deaths+Average30_Deaths+Max30_Deaths+Min30_Deaths+Varp30_Deaths+Incident_Rate+Average7_Incident_Rate
            #+Min30_Incident_Rate+Varp30_Incident_Rate+Total_Test_Results+Average7_Total_Test_Results
            #+Max7_Total_Test_Results+Min7_Total_Test_Results+Varp7_Total_Test_Results+Average30_Total_Test_Results
            #+ Max30_Total_Test_Results+Min30_Total_Test_Results+Varp30_Total_Test_Results+Case_Fatality_Ratio
            #+Average7_Case_Fatality_Ratio+Max7_Case_Fatality_Ratio+Min7_Case_Fatality_Ratio+Varp7_Case_Fatality_Ratio
            #+Average30_Case_Fatality_Ratio+Max30_Case_Fatality_Ratio+Min30_Case_Fatality_Ratio
            #+Varp30_Case_Fatality_Ratio+Testing_Rate+Average7_Testing_Rate+Max7_Testing_Rate+Min7_Testing_Rate
            #+Varp7_Testing_Rate+Average30_Testing_Rate+Max30_Testing_Rate+Min30_Testing_Rate+Varp30_Testing_Rate
            #,data=train_data)
reg_rate=lm(Daily_increase~Low+Average7_Low+Max7_Low+Min7_Low+Varp7_Low+Average30_Low+Max30_Low
            +Min30_Low+Varp30_Low+Open+Average7_Open+Max7_Open+Min7_Open+Varp7_Open+Average30_Open
            +Max30_Open+Min30_Open+Varp30_Open+Volume+Average7_Volume+Max7_Volume+Min7_Volume
            +Varp7_Volume+Average30_Volume+Max30_Volume+Min30_Volume+Varp30_Volume+High +Average7_High
            +Max7_High+Min7_High+Varp7_High+Average30_High+Max30_High+Min30_High+Varp30_High+Close+Average7_Close
            +Max7_Close+Min7_Close+Varp7_Close+Average30_Close+Max30_Close+Min30_Close+Varp30_Close+Adjusted.Close
            +Average7_Adjusted.Close+Max7_Adjusted.Close+Min7_Adjusted.Close+Varp7_Adjusted.Close+Average30_Adjusted.Close
            +Max30_Adjusted.Close+Min30_Adjusted.Close+Varp30_Adjusted.Close+Confirmed+Average7_Confirmed +Min7_Confirmed
            +Varp7_Confirmed+Average30_Confirmed+Min30_Confirmed+Varp30_Confirmed+Confirmed_increase
            +Average7_Confirmed_increase+Max7_Confirmed_increase+Min7_Confirmed_increase+Varp7_Confirmed_increase
            +Average30_Confirmed_increase+Max30_Confirmed_increase+Min30_Confirmed_increase
            +Varp30_Confirmed_increase+Deaths_increase+Average7_Deaths_increase+Max7_Deaths_increase
            +Min7_Deaths_increase+Varp7_Deaths_increase+Average30_Deaths_increase+Max30_Deaths_increase
            +Min30_Deaths_increase+Varp30_Deaths_increase+Deaths+Average7_Deaths++Min7_Deaths
            +Varp7_Deaths+Average30_Deaths+Min30_Deaths+Varp30_Deaths+Incident_Rate+Average7_Incident_Rate
            +Min7_Incident_Rate+Varp7_Incident_Rate+Average30_Incident_Rate+Min30_Incident_Rate+Varp30_Incident_Rate
            +Total_Test_Results+Average7_Total_Test_Results+Min7_Total_Test_Results+Varp7_Total_Test_Results
            +Average30_Total_Test_Results+Min30_Total_Test_Results+Varp30_Total_Test_Results+Case_Fatality_Ratio
            +Average7_Case_Fatality_Ratio+Min7_Case_Fatality_Ratio+Varp7_Case_Fatality_Ratio
            +Average30_Case_Fatality_Ratio+Min30_Case_Fatality_Ratio
            +Varp30_Case_Fatality_Ratio+Testing_Rate+Average7_Testing_Rate+Min7_Testing_Rate
            +Varp7_Testing_Rate+Average30_Testing_Rate+Min30_Testing_Rate+Varp30_Testing_Rate+Max7_Incident_Rate
            +Max7_Case_Fatality_Ratio+Max30_Case_Fatality_Ratio+Max7_Testing_Rate
            ,data=train_data)
reg_rate=lm(Daily_increase~Low+Min7_Low+Varp7_Low+Open+Average7_Open+Varp7_Open+Volume+High+Close+Average7_Close
            +Max7_Close+Varp7_Close+Adjusted.Close+Varp7_Adjusted.Close+Max30_Adjusted.Close+Confirmed+Average7_Confirmed +Min7_Confirmed
            +Average30_Confirmed+Average7_Confirmed_increase
            +Varp30_Confirmed_increase+Deaths_increase+Average7_Deaths_increase+Average7_Deaths
            +Average30_Deaths+Incident_Rate+Average30_Incident_Rate
            +Average30_Total_Test_Results+Varp30_Total_Test_Results
            +Average7_Case_Fatality_Ratio
            ,data=train_data)

######################################################################################################
setwd("~/Desktop/NTU商业分析科研")
total_data <- read.csv("E-commerce_total_data.csv",stringsAsFactors = F)
total_data$Date=as.Date(total_data$Date)
train_data = total_data[total_data$Date<="2021-07-06",]
validation_data = total_data[total_data$Date>"2021-07-06",]

######################################################################################################
reg_rate=lm(Daily_increase~Low+Open+Average7_Open+Volume+High+Average7_Close
            +Max7_Close+Varp7_Close+Adjusted.Close+Confirmed+Average7_Confirmed_increase
            +Average7_Deaths_increase
            ,data=train_data)
summary(reg_rate)
rate_pred=predict(reg_rate, validation_data)
mean((rate_pred-validation_data$Daily_increase)^2)
mean(abs(rate_pred-validation_data$Daily_increase))
write.csv(rate_pred,"~/Desktop/rate_pred.csv",row.names = F)

t_rate_pred1=predict(reg_rate, train_data)
mse1= mean((t_rate_pred1-train_data$Daily_increase)^2)
mse1

my_data=train_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close',
                 'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase',
                 'Average7_Deaths_increase')]
cor(my_data)
#install.packages('corrplot')

library('corrplot')
res=cor(my_data)
corrplot(res, type = "upper", order = "hclust", tl.col = "black ", tl.srt = 45 )
vif(reg_rate)

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(my_data[,1:5], histogram=TRUE, pch=19)

######################################################################################################
library('rpart')
tree_def = rpart(Daily_increase~Low+Open+Average7_Open+Volume+High+Average7_Close
                 +Max7_Close+Varp7_Close+Adjusted.Close+Confirmed+Average7_Confirmed_increase
                 +Average7_Deaths_increase
                 ,data=train_data,method="anova",cp=0.014)
#plot(tree_def)
#text(tree_def, pretty=1)
 
library('rpart.plot')
rpart.plot(tree_def, box.palette="RdBu", shadow.col="gray", nn=TRUE)
validation1_data=validation_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close'
                                    ,'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase'
                                    ,'Average7_Deaths_increase')]

pre_dt = predict(tree_def, validation1_data)
mean((pre_dt-validation_data$Daily_increase)^2)
mean(abs(pre_dt-validation_data$Daily_increase))
write.csv(pre_dt,"~/Desktop/pre_dt.csv",row.names = F)

t_rate_pred2=predict(tree_def, train_data)
mse2= mean((t_rate_pred2-train_data$Daily_increase)^2)
mse2

######################################################################################################
#install.packages('randomForest')
library(randomForest)
set.seed(123)
my_forest1 = randomForest(Daily_increase~Low+Open+Average7_Open+Volume+High+Average7_Close
                          +Max7_Close+Varp7_Close+Adjusted.Close+Confirmed+Average7_Confirmed_increase
                          +Average7_Deaths_increase
                          ,data=train_data,importance=T)
importance(my_forest1,type=1)
write.csv(importance(my_forest1,type=1),"~/Desktop/importance(my_forest1,type=2).csv",row.names = F)
validation1_dt=validation_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close'
                              ,'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase'
                              ,'Average7_Deaths_increase')]
p_forest=predict(my_forest1,validation1_dt)
p_forest=as.data.frame(p_forest)
mean((p_forest$p_forest-validation_data$Daily_increase)^2)


set.seed(123)
my_forest1 = randomForest(Daily_increase~Low+Open+Average7_Open+Volume+High+Average7_Close
                          +Max7_Close+Varp7_Close+Adjusted.Close+Confirmed+Average7_Confirmed_increase
                          +Average7_Deaths_increase
                          ,data=train_data,
                          ntree=600,mtry=4,importance=T)
validation1_dt=validation_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close'
                              ,'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase'
                              ,'Average7_Deaths_increase')]
p_forest=predict(my_forest1,validation1_dt)
p_forest=as.data.frame(p_forest)
mean((p_forest$p_forest-validation_data$Daily_increase)^2)

ntree_v=c(200,300,400,500,600)
mtry_v=c(2,3,4,5,6)
tree_accuracy=c()

for(i in ntree_v){
  for(j in mtry_v){
    set.seed(123)
    my_forest1 = randomForest(Daily_increase~Low+Open+Average7_Open+Volume+High+Average7_Close
                              +Max7_Close+Varp7_Close+Adjusted.Close+Confirmed+Average7_Confirmed_increase
                              +Average7_Deaths_increase
                              ,data=train_data,ntree=i,mtry=j,importance=T)
    p_forest=predict(my_forest1,validation1_dt)
    p_forest=as.data.frame(p_forest)
    x=mean((p_forest$p_forest-validation_data$Daily_increase)^2)
    tree_accuracy=c(tree_accuracy,x)
  }
}

which.min(tree_accuracy)

set.seed(123)
my_forest1 = randomForest(Daily_increase~Low+Open+Average7_Open+Volume+High+Average7_Close
                          +Max7_Close+Varp7_Close+Adjusted.Close+Confirmed+Average7_Confirmed_increase
                          +Average7_Deaths_increase
                          ,data=train_data,
                          ntree=300,mtry=5,importance=T)
validation1_dt=validation_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close'
                                  ,'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase'
                                  ,'Average7_Deaths_increase')]
p_forest=predict(my_forest1,validation1_dt)
p_forest=as.data.frame(p_forest)
mean((p_forest$p_forest-validation_data$Daily_increase)^2)
mean(abs(p_forest$p_forest-validation_data$Daily_increase))
write.csv(p_forest,"~/Desktop/pre_forest.csv",row.names = F)

t_rate_pred3=predict(my_forest1, train_data)
mse3= mean((t_rate_pred3-train_data$Daily_increase)^2)
mse3

######################################################################################################
#install.packages('xgboost')
library(xgboost)
Train=train_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close'
                ,'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase'
                ,'Average7_Deaths_increase')]
set.seed(123)
Daily_increase_xg = xgboost(data = data.matrix(Train), label = train_data$Daily_increase, 
                            max.depth = 3, eta = 0.35,  nrounds = 45, objective = "reg:linear")
validation1_dt=validation_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close'
                              ,'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase'
                              ,'Average7_Deaths_increase')]
pred_xgb = predict(Daily_increase_xg, data.matrix(validation1_dt))
mean((pred_xgb-validation_data$Daily_increase)^2)
mean(abs(pred_xgb-validation_data$Daily_increase))
write.csv(pred_xgb ,"~/Desktop/pred_xgb .csv",row.names = F)

t_rate_pred4=predict(Daily_increase_xg, data.matrix(Train))
mse4= mean((t_rate_pred4-train_data$Daily_increase)^2)
mse4

######################################################################################################
W1=mse1^-0.5
W2=mse2^-0.5
W3=mse3^-0.5
W4=mse4^-0.5
W_total=W1+W2+W3+W4
W1=W1/W_total
W2=W2/W_total
W3=W3/W_total
W4=W4/W_total
W1
W2
W3
W4
predict_weight=W1*rate_pred+W2*pre_dt+W3*p_forest+W4*pred_xgb
mse_total=mean((predict_weight-validation_data$Daily_increase)^2)
mse_total
write.csv(predict_weight,"~/Desktop/pred_Total .csv",row.names = F)

W1=mse1^-0.5
W3=mse3^-0.5
W_total=W1+W3
W1=W1/W_total
W3=W3/W_total
W1
W3
setwd("~/Desktop/NTU商业分析项目 预习材料")
pre_Total<- read.csv("pred_Total .csv",stringsAsFactors = F)
setwd("~/Desktop/NTU商业分析项目 预习材料")
total_data <- read.csv("E-commerce_total_data.csv",stringsAsFactors = F)
total_data$Date=as.Date(total_data$Date)
train_data = total_data[total_data$Date<="2021-07-06",]
validation_data = total_data[total_data$Date>"2021-07-06",]
mse_total=mean((pre_Total$daily_increase-validation_data$Daily_increase)^2)
mse_total
setwd("~/Desktop/NTU商业分析项目 预习材料")
library(ggplot2)
library(reshape2)
result_data<- read.csv("results.csv",stringsAsFactors = F)
resilt_data<-melt(result_data,id="month")
colnames(result_data)<-c("month","sample","value")
ggplot(data=result_data,aes(x=month,y=value,group=sample,color=sample,shape=sample))+
  geom_point()+
  geom_line()+
  xlab("month")+
  ylab("Daily_increase"))


#c("Low","Average7_Low","Max7_Low","Min7_Low","Varp7_Low","Average30_Low",
#"Max30_Low","Min30_Low","Varp30_Low","Open","Average7_Open","Max7_Open", 
#"Min7_Open","Varp7_Open","Average30_Open","Max30_Open","Min30_Open", 
#"Varp30_Open","Volume","Average7_Volume","Max7_Volume","Min7_Volume", 
#"Varp7_Volume","Average30_Volume","Max30_Volume","Min30_Volume","Varp30_Volume",   
#"High","Average7_High","Max7_High","Min7_High","Varp7_High","Average30_High", 
#"Max30_High","Min30_High","Varp30_High","Close","Average7_Close","Max7_Close", 
#"Min7_Close","Varp7_Close","Average30_Close","Max30_Close","Min30_Close", 
#"Varp30_Close","Adjusted.Close","Average7_Adjusted.Close","Max7_Adjusted.Close", 
#"Min7_Adjusted.Close","Varp7_Adjusted.Close","Average30_Adjusted.Close", 
#"Max30_Adjusted.Close","Min30_Adjusted.Close","Varp30_Adjusted.Close", 
#"Confirmed","Average7_Confirmed","Max7_Confirmed","Min7_Confirmed",
#"Varp7_Confirmed","Average30_Confirmed","Max30_Confirmed", "Min30_Confirmed","Varp30_Confirmed",
#"Confirmed_increase","Average7_Confirmed_increase","Max7_Confirmed_increase", 
#"Min7_Confirmed_increase","Varp7_Confirmed_increase","Average30_Confirmed_increase", 
#"Max30_Confirmed_increase","Min30_Confirmed_increase","Varp30_Confirmed_increase",
#"Deaths_increase","Average7_Deaths_increase","Max7_Deaths_increase","Min7_Deaths_increase",
#"Varp7_Deaths_increase","Average30_Deaths_increase","Max30_Deaths_increase","Min30_Deaths_increase",
#"Varp30_Deaths_increase","Deaths","Average7_Deaths","Max7_Deaths","Min7_Deaths", 
#"Varp7_Deaths","Average30_Deaths","Max30_Deaths","Min30_Deaths","Varp30_Deaths", 
#"Incident_Rate","Average7_Incident_Rate","Max7_Incident_Rate",
#"Min7_Incident_Rate","Varp7_Incident_Rate","Average30_Incident_Rate",
#"Max30_Incident_Rate","Min30_Incident_Rate","Varp30_Incident_Rate", 
#"Total_Test_Results","Average7_Total_Test_Results","Max7_Total_Test_Results", 
#"Min7_Total_Test_Results","Varp7_Total_Test_Results","Average30_Total_Test_Results", 
#"Max30_Total_Test_Results""Min30_Total_Test_Results","Varp30_Total_Test_Results",
#"Case_Fatality_Ratio","Average7_Case_Fatality_Ratio","Max7_Case_Fatality_Ratio",
#"Min7_Case_Fatality_Ratio","Varp7_Case_Fatality_Ratio","Average30_Case_Fatality_Ratio", 
#"Max30_Case_Fatality_Ratio","Min30_Case_Fatality_Ratio","Varp30_Case_Fatality_Ratio",
#"Testing_Rate","Average7_Testing_Rate","Max7_Testing_Rate","Min7_Testing_Rate",
#"Varp7_Testing_Rate","Average30_Testing_Rate","Max30_Testing_Rate","Min30_Testing_Rate",
#"Varp30_Testing_Rate")]）

#install.packages('neuralnet')
library('neuralnet')
set.seed(123)
nn=neuralnet(Daily_increase~Low+Open+Average7_Open+Volume+High+Average7_Close
             +Max7_Close+Varp7_Close+Adjusted.Close+Confirmed+Average7_Confirmed_increase
             +Average7_Deaths_increase
             ,data=train_data, hidden=c(3,2),linear.output = T,stepmax=1e7)
plot(nn)
validation1_dt=validation_data[,c('Low','Open','Average7_Open','Volume','High','Average7_Close'
                                  ,'Max7_Close','Varp7_Close','Adjusted.Close','Confirmed','Average7_Confirmed_increase'
                                  ,'Average7_Deaths_increase')]
Predict=compute(nn,validation1_dt)
Predict$net.result
write.csv(Predict$net.result,"~/Desktop/Predict$net.result.csv",row.names = F)
mean((Predict$net.result-validation_data$Daily_increase)^2)

