# We have used the following code to do both descriptive analysis and forecasting
# on the consumption of Energy.

# Set the packages and the work directory----
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
pacman::p_load(plotly, corrplot, lubridate, hms, Hmisc, imputeTS,rstudioapi,kknn,ggplot2, randomForest, dplyr, shiny, tidyr, ggplot2, caret, forecast)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))

#Preprocessing----
data <- read.delim("household_power_consumption.txt",header = TRUE, sep=";")
summary(data)

#change the type

data$Global_reactive_power = as.numeric(as.character(data$Global_reactive_power))
data$Global_active_power = as.numeric(as.character(data$Global_active_power))
data$Sub_metering_1 = as.numeric(as.character(data$Sub_metering_1))
data$Sub_metering_2= as.numeric(as.character(data$Sub_metering_2))
data$Global_intensity = as.numeric(as.character(data$Global_intensity))
data$Voltage = as.numeric(as.character(data$Voltage))
#data$Time = as.hms(as.character(data$Time))
#data$Date <- as.Date(data$Date, "%d/%m/%Y", tz="GMT")

#Change the unite measure
data$Global_active_power = data$Global_active_power * 1000 / 60
data$Global_reactive_power = data$Global_reactive_power * 1000 / 60

#create a column in the format strptime

data<-cbind(data,paste(data$Date,data$Time), stringsAsFactors=FALSE)
colnames(data)[10] <-"DateTime"
data<- data[,c(ncol(data), 1:(ncol(data)-1))]
data$DateTime <- strptime(data$DateTime, "%d/%m/%Y %H:%M:%S", tz="GMT")
#solar and legal hour

a = which(data$DateTime == as.POSIXct("2007-03-25 02:00:00", tz = "GMT") | data$DateTime ==as.POSIXct( "2007-10-28 01:59:00", tz = "GMT")) 
data[a[1]:a[2],1] = data[a[1]:a[2],1]+3600

a = which(data$DateTime == as.POSIXct("2008-03-30 02:00:00", tz = "GMT") | data$DateTime ==as.POSIXct( "2008-10-26 01:59:00", tz = "GMT")) 
data[a[1]:a[2],1] = data[a[1]:a[2],1]+3600

a = which(data$DateTime == as.POSIXct("2009-03-29 02:00:00", tz = "GMT") | data$DateTime ==as.POSIXct( "2009-10-25 01:59:00", tz = "GMT")) 
data[a[1]:a[2],1] = data[a[1]:a[2],1]+3600

a = which(data$DateTime == as.POSIXct("2010-03-28 02:00:00", tz = "GMT") | data$DateTime ==as.POSIXct( "2010-10-31 01:59:00", tz = "GMT")) 
data[a[1]:a[2],1] = data[a[1]:a[2],1]+3600

#Delate the two columns that are wrong  
data$Date <- NULL
data$Time <-NULL
data$Date <- as.Date(data$DateTime)
data=data[,c(1,9,2:8)]
#create a table with the missing values 
DATA=data[which(is.na(data$Global_reactive_power) == TRUE),]
t = table(DATA$Date)

#data$other = data$Global_active_power - data$Sub_metering_1- data$Sub_metering_2-data$Sub_metering_3
#data$weekdays= weekdays(data$DateTime)

##Descriptive analysis
#Substitute the NA-----

# Check if the missing values are consecutives
# for (i in 1:length(row.names(t)))
# { dataset = subset(DATA, DATA$Date == row.names(t)[i])
# rowsnumber = nrow(dataset)
# rowsname = as.integer(row.names(dataset))
# difference = rowsname[length(rowsname)]-rowsname[1]+1
# if ( (difference == rowsnumber)== FALSE)
# { print(row.names(t)[i])
#   print(i)
# }}
#
#Substitute the NA 
mydata = data
#create a list with the date that I want to consider. When I want the value 0
l = c()
for (i in 1:length(row.names(t)))
     {
       if (t[i]>=178 & t[i] != 891)
       {l = c(l, row.names(t)[i])}
}


a = which( (as.character(mydata$Date) %in% l) & is.na(mydata$Global_active_power) )
mydata[a,3:9] = c(0,0,0,0,0,0,0)
summary(mydata)


#Substitute the other NA with the mean 
# for ( i in 3:9)
# {
#   mydata[i]=impute(mydata[i],mean)
# }


#Substitute the other NA with the closest value 

for (i in 3:9)
{
mydata[,i] = na.locf(mydata[,i], option='locf')
}

#create a column with energy not used in submetering 1,2,3
mydata$other = as.numeric(mydata$Global_active_power-mydata$Sub_metering_1-mydata$Sub_metering_2-mydata$Sub_metering_3)

#Use of energy per hours ------

mydatah= mydata[,2:ncol(mydata)]
mydatah$hour = format( mydata$DateTime, format="%H") 
mydatahours = data.frame(mydatah %>% group_by(Date, hour ) %>% summarise_all(sum))

#Taking some day and plot the energy consumption per hours

for (i in c(5:11))
{
  dayconsidered = subset( mydatahours, day(Date)==i & month(Date)==3 & year(Date) == 2010)
  g =ggplot(dayconsidered,aes(x = c(1:24), y = Global_active_power))+ geom_point()+geom_line()+
    ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))
  print(g) }


#Take a day and consider the different type of energy 
par(mfrow=c(3,2))

for (i in c(5:11))
{
  dayconsidered = subset( mydatahours, day(Date)==i & month(Date)==10 & year(Date) == 2010)
  g =ggplot(dayconsidered,aes(x = c(1:24), y = Global_active_power))+ geom_point()+geom_line()+
    ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
    ylab(' Global active energy')
  print(g) }


#Show where the energy was used during that day
dayconsidered = subset( mydatahours, day(Date)==8 & month(Date)==10 & year(Date) == 2010)
g =ggplot(dayconsidered,aes(x = c(1:24), y = Sub_metering_3))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab(' Sub metering 1')

# It is better per minutes 

dayconsidered = subset( mydata, day(Date)==8 & month(Date)==10 & year(Date) == 2010)
g =ggplot(dayconsidered,aes(x = c(1:1440), y = Sub_metering_1))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab(' Sub metering 1')

# Other graphs
dayconsidered = subset( mydata, day(Date)==8 & month(Date)==10 & year(Date) == 2010)
g =ggplot(dayconsidered,aes(x = c(1:1440), y = other))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab('Other energy')

dayconsidered = subset( mydata, day(Date)==8 & month(Date)==10 & year(Date) == 2010)
g =ggplot(dayconsidered,aes(x = c(1:1440), y = other))+ geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab('Other energy')


dayconsidered = subset( mydata, day(Date)==8 & month(Date)==12 & year(Date) == 2009)
g =ggplot(dayconsidered,aes(x = c(1:1440), y = other))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab('Other energy')

for (i in c(5:11))
{
dayconsidered = subset( mydata, day(Date)==i & month(Date)== 7 & year(Date) == 2010)
g =ggplot(dayconsidered,aes(x = c(1:1440), y = other))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab('Other energy') 
print(g)
}

for ( i in c(4:18))

{dayconsidered = subset( mydata, day(Date)== i & month(Date)==10 & year(Date) == 2009)
g =ggplot(dayconsidered,aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab(' Sub metering 2')
print(g)
}


#zoom in sub metering-2
dayconsidered = subset( mydata, day(Date)==11 & month(Date)==10 & year(Date) == 2009)[400:500,]
g =ggplot(dayconsidered,aes(x = c(400:500), y = Sub_metering_2))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab(' Sub metering 2')

sum(dayconsidered$Sub_metering_2)
print(g)
jpeg('i')
dev.off()
#Try to understand which device is it

dayconsidered = subset( mydata, day(Date)==8 & month(Date)==10 & year(Date) == 2010)[1230:1370,]
g =ggplot(dayconsidered,aes(x = c(1230:1370), y = Sub_metering_1))+ geom_point()+geom_line()+
  ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))+xlab('hours')+
  ylab(' Sub metering 1')

sum(dayconsidered$Sub_metering_1)
summary(dayconsidered)

for (i in c(5:11))
{
  dayconsidered = subset( mydatahours, day(Date)==i & month(Date)==3 & year(Date) == 2010)
  g =ggplot(dayconsidered,aes(x = c(1:24), y = Global_active_power))+ geom_point()+geom_line()+
    ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))
  print(g) }


#second week considered

for (i in c(5:11))
{
  dayconsidered = subset( mydatahours, day(Date)==i & month(Date)== 10 & year(Date) == 2010)
  g =ggplot(dayconsidered,aes(x = c(1:24), y = Global_active_power))+ geom_point()+geom_line()+
    ggtitle(paste('Date:', weekdays(dayconsidered$Date[1]), dayconsidered$Date[1]))
  print(g) }

##Analysing the data per month------
# data per month  

mydatamonth = mydata
mydatamonth$DateTime = NULL
mydatamonths = mydatamonth %>% group_by(year(Date),month(Date) ) %>% summarize_all(sum)
mydatamonths$other = as.numeric(mydatamonths$Global_active_power-mydatamonths$Sub_metering_1-mydatamonths$Sub_metering_2-mydatamonths$Sub_metering_3)

#graphic in every year 
mydatam2007= mydatamonths[2:13,]
ggplot(data= mydatam2007, aes(x = factor(c(1:12)),y = Global_active_power,group=1)) +  
  geom_point() + geom_line()

mydatam2008= mydatamonths[14:25,]
ggplot(data= mydatam2008, aes(x = factor(c(1:12)),y = Global_active_power,group=1)) +  
  geom_point() + geom_line()

colnames(mydatamonths)

ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= Global_active_power, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")+labs(y= "Global active energy", x = "Months", color=' Years')


ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= Global_reactive_power, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")

ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= Sub_metering_1, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")

ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= Sub_metering_2, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")

ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= Sub_metering_3, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")

ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= Voltage, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")

ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= other, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")

#Some barplot to compere the type of energy used 

names <- as.factor(c(1:12))
prova = data.frame(c(1:12),mydatamonths[c(26:37),])

p <- plot_ly(prova, x = c(1:12), y = ~Sub_metering_1, type = 'bar', name = 'Sub metering 1') %>%
  add_trace(y = ~Sub_metering_2, name = 'Sub metering 2') %>%
  add_trace(y = ~Sub_metering_3, name = 'Sub metering 3') %>%
  add_trace(y =~other, name = 'Other energy used') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

prova[1]
print(p)
prova = data.frame(c(1:12),mydatamonths[c(2:13),])
p <- plot_ly(prova, x =~c.2.13., y = ~Sub_metering_1, type = 'bar', name = 'Sub metering 1') %>%
  add_trace(y = ~Sub_metering_2, name = 'Sub metering 2') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

colnames(prova)

plot(p)
chart_link = api_create(p, filename="bar-stacked")


#An easy plot with the energy used per year

mydatayears = data.frame(mydata[,2:ncol(mydata)] %>% group_by(year(Date)) %>% summarize_all(sum))
mydatayears$Global_active_power


# pie charts!
ds <- data.frame(labels = c("Sub-metering 1", "Sub-metering 2", "Submetering 3", 'Other energy used'), values = unlist(mydatayears[5,c(7,8,9,10)]))
plot_ly(ds, labels = ~labels, values = ~values) %>%
  add_pie() %>%
  layout(title = "2010")


ds <- data.frame(labels = c("Sub-metering 1", "Sub-metering 2", "Submetering 3", 'Other energy used'), values = unlist(mydatayears[5,c(7,8,9,10)]))
plot_ly(ds, labels = ~labels, values = ~values) %>%
  add_pie() %>%
  layout(title = "2011")

# Analysing the data per day, i.e search for patterns during the months or week---------

mydataday= mydata[2:3]

mydatadays = data.frame(mydataday %>% group_by(Date) %>% summarize_all(sum))

#Distribution of the global energy per weekdays
p <- plot_ly(y = (subset(mydatadays, weekdays(Date)=='sabato'))[,2], type = "box")
for (i in unique( weekdays(mydatadays$Date))[2:7])
{
  p = p%>%add_trace(y =  (subset(mydatadays, weekdays(Date)==i))[,2] )

}

# Distribution of the oder type of energy per weekdays
mydataday2= mydata[2:10]
mydataday2 = data.frame(mydataday2 %>% group_by(Date) %>% summarize_all(sum))


p <- plot_ly(y = (subset(mydataday2, weekdays(Date)=='sabato'))[,9], type = "box")
for (i in unique( weekdays(mydatadays$Date))[2:8])
{
  p = p%>% add_trace(y =(subset(mydataday2, weekdays(Date)==i))[,9])
}
print(p)

# Other energy and the submetering 3 have similar distribution during the week 

#graphich with all the day 
ggplot(data= mydatadays, aes(x = factor(c(1:1442)),y = Global_active_power,group=1)) +  
  geom_point() + geom_line()

#Try to plot the trend in the weekend (in the correct order)

#mydatadays$weekdays = as.factor(weekdays(mydatadays$Date))
mydatadays = mydatadays[17:nrow(mydatadays),]
#mydatadays$weekdays = factor( mydatadays$weekdays, unique(mydatadays$weekdays) )


# The analysis of the week should be done in more accurate way 
ggplot(data = mydatadays[8:48,], aes(x = factor(as.factor(weekdays(Date)), unique(weekdays(Date))),y = Global_active_power, group= as.factor(week(Date)))) +  
     geom_line(aes(color= as.factor(week(Date))))+geom_point()

ggplot(data = mydatadays[78:148,], aes(x = factor(as.factor(weekdays(Date)), unique(weekdays(Date))),y = Global_active_power, group= as.factor(week(Date)))) +  
  geom_line(aes(color= as.factor(week(Date))))+geom_point()


# I try to remove the seasonality and check the trend

mydata_no_seasonality = mydatadays
newseries = diff( mydata_no_seasonality$Global_active_power ,lag =365)

ggplot(data = data.frame(newseries[1:70]), aes(x = c(1:70), y = newseries[1:70])) +  
  geom_line()+geom_point()


#analysis again considering the last year of observation we have 

March2010= subset(mydata, year(Date)== 2010 & month(Date)==3)
March2010$weekdays = weekdays(March2010$Date)
March2010$week = week(March2010$Date)

#I consider 3 weeks of March going from Monday to Sundays

March2010_3weeks = subset(March2010, week %in% c(10,11,12) )

March2010_3_byday = data.frame( March2010_3weeks[,2:(ncol(March2010_3weeks)-2)] %>% group_by(Date) %>% summarize_all(sum))

ggplot(data = March2010_3_byday, aes(x = factor(as.factor(weekdays(Date)), unique(weekdays(Date))),y = Global_active_power, group= as.factor(week(Date)))) +  
geom_line(aes(color= as.factor(week(Date))))+geom_point()




#I consider 2009

March2009= subset(mydata, year(Date)== 2009 & month(Date)==3)
March2009$weekdays = weekdays(March2009$Date)
March2009$week = week(March2009$Date)

#I consider 3 weeks of March going from Monday to Sundays

March2010_3weeks = subset(March2009, week %in% c(10,11,12) )

March2010_3_byday = data.frame( March2010_3weeks[,2:(ncol(March2010_3weeks)-2)] %>% group_by(Date) %>% summarize_all(sum))

ggplot(data = March2010_3_byday, aes(x = factor(as.factor(weekdays(Date)), unique(weekdays(Date))),y = Global_active_power, group= as.factor(week(Date)))) +  
  geom_line(aes(color= as.factor(week(Date))))+geom_point()

#Some box plot per weekday

weekday= mydata
weekday$DateTime <-NULL
weekday = weekday %>% group_by(Date) %>% summarize_all(sum)
weekday$weekdays= weekdays(weekday$Date)
boxplot(weekday$Global_active_power)

#graphic per day

ggplot(data=mydatamonths[c(2:48),], aes(x=as.factor(`month(Date)`), y= Global_active_power, group=as.factor( `year(Date)`))) +
  geom_line(aes(color= as.factor(`year(Date)`)))+
  geom_point()+
  theme(legend.position="top")

# A box plot and a density plot
boxplot(mydata$Global_active_power)
d = density(mydata$Global_active_power, na.rm = TRUE)
plot(d)


# Analysing the data during the day-----

March2010= subset(mydata, year(Date)== 2010 & month(Date)==3)
March2010$weekdays = weekdays(March2010$Date)
March2010$week = week(March2010$Date)

#I consider 3 weeks of March going from Monday to Sundays

March2010_3weeks = subset(March2010, week %in% c(10,11,12) )

Venerdi = subset(March2010_3weeks, day(Date)==5)
ggplot(Venerdi,aes(x = c(1:1440), y = Global_active_power))+ geom_point()+geom_line()
ggplot(Venerdi[1000:1100,],aes(x = c(1000:1100), y = Global_active_power))+ geom_point()+geom_line()


# Isolate the behaviour of the submetering 2  

ggplot(Venerdi,aes(x = c(1:1440), y = Global_active_power))+ geom_point()+geom_line()
ggplot(Venerdi,aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()
ggplot(Venerdi[150:350,],aes(x = c(150:350), y = Sub_metering_2))+ geom_point()+geom_line()


ggplot(Venerdi[150:350,],aes(x = c(150:350), y = Sub_metering_2))+ geom_point()+geom_line()

#Sabato
ggplot(subset(March2010_3weeks, day(Date)==6),aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()

#Domenica (lavatrice e asciugatrice)
ggplot(subset(March2010_3weeks, day(Date)==7),aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()

#Lunedi(lavatruce e asciugatrice)
ggplot(subset(March2010_3weeks, day(Date)==8),aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()

#Martedi (lampadina)
ggplot(subset(March2010_3weeks, day(Date)==9),aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()

# Mercoledi(solo frigorifero)
ggplot(subset(March2010_3weeks, day(Date)==10),aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()

# Giovedi(solo frigorifero)
ggplot(subset(March2010_3weeks, day(Date)==11),aes(x = c(1:1440), y = Sub_metering_2))+ geom_point()+geom_line()


#Graphic of the consum of energy of the water-heater in March
l=list()
for (i in c(5:11))
{
  g =ggplot(subset(March2010_3weeks, day(Date)==i),aes(x = c(1:1440), y = Sub_metering_3))+ geom_point()+geom_line()
  print(g)
  l = append(l,g)
}

#Graphic of the summetering 3 in the summer (that peaks are the air conditionair? strangeee)
summer=subset(mydata, month(Date)==8 & year(Date)==2009 )
l=list()
for (i in c(5:11))
{
  g =ggplot( subset(summer, day(Date)==i) ,aes(x = c(1:1440), y = Sub_metering_3))+ geom_point()+geom_line()
  print(g)}


# Let examinate the microwave, the oven and the dish-washer adding the day of the week 
summer=subset(mydata, month(Date)==4 & year(Date)==2009 )
summer$other = summer$Global_active_power- summer$Sub_metering_1 - summer$Sub_metering_2 - summer$Sub_metering_3
l=list()
for (i in c(5:11))
{
  dataset =subset(summer, day(Date)==i)
  g =ggplot( dataset ,aes(x = c(1:1440), y = other))+ geom_point()+geom_line()+
    ggtitle(paste('Date:', weekdays(dataset$Date[1]), dataset$Date[1]))
  print(g)
  }


# ò Substitute for predicting-------------
energy = data
energy$Voltage <-NULL
energy$Global_intensity <-NULL
# In the small time frame with missing values I put the value of the last observations 

mv= c()
for (i in 1:length(t))
{
  if (t[i] < 178 | t[i] == 891)
  {mv = c(mv, row.names(t)[i])}
}
b = which( (as.character(energy$Date) %in% mv) )

for(i in 3:7)
{
  energy[b,i] = na.locf(energy[b,i], option='locf')
  
}

#Creation of the column season
energy$season = 'winter'
energy$season[month(energy$DateTime)>2 & month(energy$DateTime) <6 ]='spring'
energy$season[month(energy$DateTime)>5 & month(energy$DateTime) <9 ]='summer'
energy$season[month(energy$DateTime)>8 & month(energy$DateTime) <12 ]='autumn'
energy$season = as.character(energy$season)
energy$weekdays = weekdays(energy$DateTime)
head(energy)
ll= unique(energy$season)
l = unique(energy$weekdays)

for (i in ll){
  for (j in 0:23){
    for (k in 1:length(l))
    {
      index = which( hour(energy$DateTime) == j & as.character(energy$weekdays)== l[k] & as.character(energy$season)==i  )
      for (m in 3:7)
      {energy[index,m] = as.numeric(impute(energy[index,m],'random'))}
}}}



#Group by day and forecasting
energydays = energy[,2:7] %>% group_by(Date) %>% summarize_all(sum) #Dataset organised by date
december2010= subset(energydays, year(Date)== 2010 & month(Date)== 11)
summary(december2010)
Value = 28589 * 4
tsdays <- ts( data.frame(energydays)[2], start = c(2006, 350),frequency = 365.25)
plot(tsdays)
dec =decompose(tsdays)
plot(dec$seasonal)
plot(dec$trend)# I don't know how to interpret it  
#Use the Hold-Winters method


trainingset = tsdays[1:round(nrow(tsdays)*0.8),]
hw(tsdays,h =1)
hw(data.frame(energydays)[2],h = 1)

vector = data.frame(energydays)[2]
plot(forecast(auto.arima(vector), h = 30))
autoplot(window(diff(unlist(vector, use.names=FALSE),365)))



ggplot(diff(unlist(vector, use.names=FALSE),365)))
vector2=diff(unlist(vector, use.names=FALSE),365)
auto.arima(vector2)
plot(diff(vector, 365))
forecast3 = forecast(auto.arima(vector2), h = 365)
plot( forecast3)
#Plot the prediction using of the last 20% of the data

#Considere the errors and the residuals

#Use the cross validation for time series

summary(energy)


#Predict using the month

energymonth=data.frame(energy[,2:7] %>% group_by(year(Date),month(Date)) %>% summarize_all(sum)) #Dataset organised by date
energymonths=energymonth[c(2:47),4]
energymonths= ts(energymonths, start=2007, frequency = 12)

#detecting OUTLIERS
ouliers<- tsoutliers(energymonths) # I obtain two values I substitute boths
energymonths <- tsclean( energymonths, replace.missing = TRUE, lambda = NULL)
autoplot(energymonths)
training = subset(energymonths, start = 1 ,end = 36)
testing= subset(energymonths, start= 37, end= 46)
test=energymonths[37:46]

autoplot(testing)
autoplot(training)
autoplot(energymonths)

# First I used the menthod from Holt-Winter(M,N,M)

fit.ets= ets(training)
summary(fit.ets)
checkresiduals(fit.ets)
Box.test(fit.ets$residuals, lag=20, type="Ljung-Box")
a1= accuracy(as.numeric(test), forecast(fit.ets,h = 10)$mean)
f1 = forecast(fit.ets,h = 10)
autoplot(training)+autolayer(forecast(fit.ets,h = 10)$mean)+autolayer(testing)
ets(energymonths)
forecast1=forecast(ets(energymonths),h =12)
autoplot( energymonths)+autolayer(forecast1, PI = F)

# I use the method Arima 

fit.arima = auto.arima(training) #again it says that the only important thing is the seasonality
summary(fit.arima)
checkresiduals(fit.ets)
a1= accuracy(as.numeric(test), forecast(fit.arima,h = 10)$mean)
f2 = forecast( fit.arima, h = 10)
autoplot( energymonths)+autolayer(forecast2, PI = F)#plot sul testing
forecast2 = forecast( auto.arima(energymonths), h = 12)
autoplot(energymonths)+ autolayer(forecast2, PI = F)
summary(auto.arima(energymonths))

# I use the linear model 


fit.lm = tslm( training ~ trend + season  )
summary(fit.lm)
f3 = forecast( fit.lm, h = 10)
a1 = accuracy(as.numeric(test), f3)
autoplot( energymonths)+autolayer(f3, PI = F)#plot sul testing

forecast3 = forecast( tslm (energymonths ~ trend + season ), h = 12)
autoplot(energymonths)+ autolayer(forecast3, PI = F)

#I obtain the plot with all the forecasting for the test together 
autoplot( energymonths)+autolayer(f1, PI = F)+autolayer(f2, PI = F)+autolayer(f3, PI=F)
autoplot(   )+autolayer(f1, PI = F)+autolayer(f2, PI = F)+autolayer(f3, PI=F)

#I obtain the final plot with all the three model togeteher with the forecasting

autoplot(energymonths)+ autolayer(forecast1, PI = F)+autolayer(forecast2, PI = F)+autolayer(forecast3, PI=F)


# I want to consider the energy used on everage per day and plot it

energymonths_day=data.frame(energy[,2:7] %>% group_by(year(Date),month(Date)) %>% summarize_all(mean)) 
energymonths_day=energymonths_day[c(2:47),4]
energymonths_day= ts(energymonths_day, start=2007, frequency = 12)
autoplot(energymonths_day)


# I studied the time series per days 
summary(energymonths)
myts2 = ts( energymonths,
            start = 2007,
            frequency = 12)
HW=hw(myts2,h = 12)
autoplot(myts2)+autolayer(HW)
#Graphic without the confident interval 
HW=hw(myts2,h = 12)
summary(HW)
#the automatic default parameters chosen for the model are
#alpha = 8e-04 
#beta  = 8e-04 
#gamma = 1e-04 
autoplot(myts2)+autolayer(HW, PI=F)
#ets(myts2,model='MAA')
#ets(myts2,model='AAA') by default take this 
ets(myts2, model='ZZZ')
#I obtain the model with no trend as one expects!
forecast2 = ets(myts2, model='ANA')
#alpha = 1e-04 
#gamma = 1e-04 
f2= forecast(forecast2,h = 10)
summary(f2)
autoplot(myts2)+autolayer(f2)

#Timeseries cross validation. I can use this to evaluate my models
#tsCV(myts2, etc(model='ANA'), h = 12)
#tsCV(myts2, holt, h = 1, k = 1)

cbind('initial time series'= myts2, 'log of the time series'= log(myts2))%>% 
autoplot(facets=TRUE)

##forecast-------
Dayserie=data.frame(energydays)
head(Dayserie)
Dayserie2=Dayserie[17:nrow(Dayserie),]
myts <- ts( Dayserie2, 
            start = 2007,
            frequency = 365.25)

dec =decompose(myts[,2])
plot(dec$seasonal)
plot(dec$trend)#start after 


decc = decompose(myts)
plot(decc$seasonal)#impazzisce
plot(decc$trend)#???
#first try

myts2 <- ts( Dayserie, 
           start = c(2006, 350),
           frequency = 365)

head(myts2[1,2])
#get some inside of my time serie
dec2=decompose(myts2[,2])
plot(dec2$seasonal)
14plot(myts2[,2])
#create a model
fit <- auto.arima(myts[,2])
summary(Dayserie)

## forecast for next 60 time points
fore <- forecast(fit, h = 365) 
plot(fore)
fore2 <- forecast(myts2[,2],h = 300)
plot(fore2)


?forecast
#forecast by days-----
##Try ------------
Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)




#energy$season = quarter(energy$DateTime, with_year = FALSE, fiscal_start = 3)

#data = energy
#l = energy$weekdays
#for (i in 1:4){
  #for (j in 0:23){
   # for (k in 1:length(l))
  #  {
   #  index = which(energy$season ==i & hour(energy$DateTime) == j & as.character(energy$weekdays)== l[k] )
    #  for (m in 4:7)
     # {energy[index,m] = impute(energy[index,m], mean)
    #  }}}}


#summary(energy)

##grupping by day 

energy$Date= as.Date(date(energy$DateTime))
energy$DateTime <-NULL
energy = energy[,c(8,1:7)]
energyday=energy[,c(3:8)]
str(energyday)
energydays = energyday %>% group_by(Date) %>% summarize_all(sum)
energymonth= energy[,c(2:8)]
as.Date(date(energy$DateTime))
energyseason = group_by(energy, season )
summary(energy)

#v = c(NA,5,6,7)
#s =impute(v,'random')
#class(s[1])
#class(as.integer(s[1]))
#t = as.numeric(s)
#class(t)
#unique(energy$season)

#v[2]=3
#6840-6841
#ex= data[6839:6842,]
#na.locf(ex[4], option='locf')
#na.locf(ex[4], option='locf', na.remaining='mean')



#Final forecast------
#Predict using the month
summary(energymonth)
energymonth=data.frame(energy[,2:7] %>% group_by(year(Date),month(Date)) %>% summarize_all(sum)) #Dataset organised by date
# I add the days missing in November 2010
energymonth[48,4] = energymonth[48,4] + Value
energymonths=energymonth[c(2:48),4]

energymonths= ts(energymonths, start=2007, frequency = 12)
autoplot(energymonths)
#detecting OUTLIERS
ouliers<- tsoutliers(energymonths) # I obtain two values I substitute boths
#energymonths <- tsclean( energymonths, replace.missing = TRUE, lambda = NULL)
(459225.4 + 487404.8 +567496.2)/3
energymonths[20]=504708.8
energymonths[8]
autoplot(energymonths)
training = subset(energymonths, start = 1 ,end = 36)
testing= subset(energymonths, start= 37, end= 47)
test=energymonths[37:47]

autoplot(testing)
autoplot(training)
autoplot(energymonths)

# First I used the menthod from Holt-Winter(M,N,M)

fit.ets= ets(training)
summary(fit.ets)
checkresiduals(fit.ets)
Box.test(fit.ets$residuals, lag=20, type="Ljung-Box")
a1= accuracy(as.numeric(test), forecast(fit.ets,h = 11)$mean)
f1 = forecast(fit.ets,h = 11)
autoplot(training)+autolayer(forecast(fit.ets,h = 11)$mean)+autolayer(testing)
ets(energymonths)
forecast1=forecast(ets(energymonths),h =12)
autoplot( energymonths)+autolayer(forecast1, PI = T)

final = data.frame(forecast1$mean)
colnames(final)
ggplot(final,aes(x = c(1:12), y =forecast1.mean ))+ geom_point()+ geom_point()
# I use the method Arima 

fit.arima = auto.arima(training) #again it says that the only important thing is the seasonality
summary(fit.arima)
checkresiduals(fit.arima)
a1= accuracy(as.numeric(test), forecast(fit.arima,h = 11)$mean)
f2 = forecast( fit.arima, h = 11)
autoplot( energymonths)+autolayer(forecast2, PI = F)#plot sul testing
forecast2 = forecast( auto.arima(energymonths), h = 12)
autoplot(energymonths)+ autolayer(forecast2, PI = F)
summary(auto.arima(energymonths))

# I use the linear model 


fit.lm = tslm( training ~ trend + season  )
checkresiduals(fit.lm)
summary(fit.lm)
accuracy(fit.lm)
f3 = forecast( fit.lm, h = 11)
a1 = accuracy(as.numeric(test), f3$mean)
autoplot( energymonths)+autolayer(f3, PI = F)#plot sul testing

forecast3 = forecast( tslm (energymonths ~ trend + season ), h = 12)
autoplot(energymonths)+ autolayer(forecast3, PI = T)

#I obtain the plot with all the forecasting for the test together 
autoplot( energymonths)+autolayer(f1, PI = F)+autolayer(f2, PI = F)+autolayer(f3, PI=F)
autoplot(   )+autolayer(f1, PI = F)+autolayer(f2, PI = F)+autolayer(f3, PI=F)

#I obtain the final plot with all the three model togeteher with the forecasting

autoplot(energymonths)+ autolayer(forecast1, PI = F)+autolayer(forecast2, PI = F)+autolayer(forecast3, PI=F)
