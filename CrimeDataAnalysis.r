library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plotrix)

#read the crime dataset
//crime_data<- read.csv("/Users/nikhilsethi/Desktop/Crimes_LA_2012-2015.csv", head=TRUE,stringsAsFactors = FALSE)

head(crime_data)
#order by date
crime_data<- crime_data[order(crime_data$Date.Rptd),]

#understanding the dataset
str(crime_data)
#dimensions
dim(crime_data)
#check for missing values
sum(is.na(crime_data))
which(is.na(crime_data))
#removing duplicates
crime_data = subset(crime_data, !duplicated(crime_data$DR.NO) )

#date manipulation
crime_data$DATE.OCC = as.POSIXct(crime_data$DATE.OCC, format="%m/%d/%Y")
crime_data$Year = year(crime_data$DATE.OCC) 
crime_data$Month = month(crime_data$DATE.OCC)
crime_data$Day = day(crime_data$DATE.OCC)
#time manipulation
crime_data$min  <-  substr(crime_data$TIME.OCC, nchar(crime_data$TIME.OCC)-1, nchar(crime_data$TIME.OCC))
crime_data$hour <- substr(crime_data$TIME.OCC, nchar(crime_data$TIME.OCC)-3, nchar(crime_data$TIME.OCC)-2)

#identify crime categories
table(crime_data$CrmCd.Desc)
unique(crime_data$CrmCd.Desc)
length(unique(crime_data$CrmCd.Desc))

#there are 158 categories of crime, we are grouping them into 9 categories for easier analysis
crime_data$crime_category <- as.character(crime_data$CrmCd.Desc)
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("BUNCO, PETTY THEFT", "BIKE - STOLEN","BIKE - ATTEMPTED STOLEN", "BUNCO, GRAND THEFT", "DISHONEST EMPLOYEE - GRAND THEFT","DISHONEST EMPLOYEE - PETTY THEFT ", "DISHONEST EMPLOYEE ATTEMPTED THEFT", "EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)", "BOAT - STOLEN", "EMBEZZLEMENT, PETTY THEFT ($950 & UNDER)","GRAND THEFT / AUTO REPAIR", "PROPERTY MISSING RVD VEH - GRAND THEFT ($950.01 & OVER)",
                                                                     "SHOPLIFTING-GRAND THEFT ($950.01 & OVER)","PETTY THEFT - AUTO REPAIR", "SHOPLIFTING-GRAND THEFT (OVER $401)", "SHOPLIFTING - PETTY THEFT","SHOPLIFTING - PETTY THEFT ($950 & UNDER)","THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD", "THEFT-GRAND (OVER $400 OR $100 IF FOWL)", "THEFT FROM MOTOR VEHICLE - GRAND ($400 AND OVER)", 
                                                                     "THEFT FROM MOTOR VEHICLE - PETTY ($950.01 & OVER)", "THEFT FROM MOTOR VEHICLE - PETTY (UNDER $400)", "THEFT FROM PERSON - ATTEMPT", "THEFT OF IDENTITY", "THEFT PLAIN - ATTEMPT", "THEFT PLAIN - PETTY ($950 & UNDER)", "DISHONEST EMPLOYEE - PETTY THEFT", "THEFT, COIN MACHINE - ATTEMPT", "PETTY THEFT - AUTO REPAIR", "THEFT FROM MOTOR VEHICLE - ATTEMPT", 
                                                                     "THEFT PLAIN - PETTY (UNDER $400)","SHOPLIFTING - ATTEMPT", "THEFT, COIN MACHINE - GRAND", "THEFT, COIN MACHINE - GRAND ($950.01 & OVER)", "THEFT, COIN MACHINE - PETTY",
                                                                     "THEFT, COIN MACHINE - PETTY ($950 & UNDER)", "THEFT, PERSON", "VEHICLE - STOLEN","TILL TAP - GRAND THEFT ($950.01 & OVER)", "TILL TAP - PETTY", "TILL TAP - PETTY ($950 & UNDER)","VEHICLE - ATTEMPT STOLEN"), 'THEFT', crime_data$crime_category)             
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("ATTEMPTED ROBBERY","BURGLARY", "BURGLARY FROM VEHICLE", "BURGLARY FROM VEHICLE, ATTEMPTED", "BURGLARY, ATTEMPTED", "ROBBERY"), 'BURGLARY', crime_data$crime_category)  
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("CREDIT CARDS, FRAUD USE ($950 & UNDER","CREDIT CARDS, FRAUD USE ($950.01 & OVER)", "CREDIT CD > $200", "CREDIT DD < $200", "DOCUMENT FORGERY / STOLEN FELONY", "DOCUMENT WORTHLESS ($200 & UNDER)", "DOCUMENT WORTHLESS ($200.01 & OVER)","DOCUMENT WORTHLESS > $100","DEFRAUDING INNKEEPER/THEFT OF SERVICES, OVER $400", "DEFRAUDING INNKEEPER/THEFT OF SERVICES, $400 & UNDER"), 'FRAUD', crime_data$crime_category)  
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("RAPE, FORCIBLE","SEX OFFENDER REGISTRANT INCIDENT", "SEX, UNLAWFUL", "SEXUAL PENTRATION WITH A FOREIGN OBJECT", "SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH","BATTERY WITH SEXUAL CONTACT","PIMPING","INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)","RAPE, ATTEMPTED"), 'SEX', crime_data$crime_category)  
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT","ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER", "BATTERY - SIMPLE ASSAULT", "SPOUSAL (COHAB) ABUSE - AGGRAVATED ASSAULT", "SPOUSAL(COHAB) ABUSE - SIMPLE ASSAULT", "OTHER ASSAULT"), 'ASSAULT', crime_data$crime_category)  
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("CHILD ABANDONMENT","CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT", "CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT", "CHILD ANNOYING (17YRS & UNDER, DID NOT TOUCH VICTIM)", "CHILD ENDANGERMENT/NEG.","CHILD NEGLECT (SEE 300 W.I.C.)", "CHILD STEALING", "CHILD, CRIME AGAINST"), 'CHILD ABUSE', crime_data$crime_category)  
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("VANDALISM - MISDEAMEANOR","ARSON","VANDALISM - MISDEAMEANOR ($399 OR UNDER)","VANDALISM - FELONY", "VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)", "VANDALISM"), 'VANDALISM', crime_data$crime_category)  
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("TRAFFIC DR #"), 'TRAFFIC', crime_data$crime_category)  
crime_data$crime_category <- ifelse(crime_data$crime_category %in% c("ABORTION/ILLEGAL","BATTERY FIREMAN", "BATTERY ON A FIREFIGHTER", "BATTERY POLICE", "BEASTIALITY, CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM","BIGAMY","BOMB SCARE", "BRANDISH WEAPON", "BRIBERY", "BUNCO, ATTEMPT", "CONSPIRACY", "CONTEMPT OF COURT", "CONTRIBUTING", "COUNTERFEIT", "CRIMINAL HOMICIDE","CRIMINAL THREATS - NO WEAPON DISPLAYED", "CRUELTY TO ANIMALS", "DISCHARGE FIREARMS/SHOTS FIRED", "DISRUPT SCHOOL", "DISTURBING THE PEACE", "DRIVING WITHOUT OWNER CONSENT (DWOC)","DRUGS, TO A MINOR", "DRUNK ROLL", "EMBEZZLEMENT, GRAND", "EMBEZZLEMENT, PETTY"
                                                                     ,"EXTORTION","FAILURE TO DISPERSE", "FAILURE TO YIELD", "FALSE IMPRISONMENT", "FALSE POLICE REPORT"," GRAND THEFT / INSURANCE FRAUD", "HOMICIDE (NON-UCR)","ILLEGAL DUMPING", "INCITING A RIOT","INDECENT EXPOSURE","KIDNAPPING","KIDNAPPING - GRAND ATTEMPT","LETTERS, LEWD", "LEWD CONDUCT","LYNCHING","LYNCHING - ATTEMPTED","MANSLAUGHTER, NEGLIGENT", "ORAL COPULATION","OTHER MISCELLANEOUS CRIME", "PANDERING", "PEEPING TOM", "PICKPOCKET", "PICKPOCKET, ATTEMPT", "PROPERTY MISSING RVD VEH - GRAND", "PROPERTY MISSING RVD VEH - PETTY","PROPERTY MISSING RVD VEH - PETTY ($950 & UNDER)" 
                                                                     ,"PROWLER","PURSE SNATCHING","PURSE SNATCHING - ATTEMPT","RECKLESS DRIVING", "REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)", "GRAND THEFT / INSURANCE FRAUD","RESISTING ARREST", "SHOTS FIRED AT INHABITED DWELLING", "SHOTS FIRED AT MOVING VEHICLE, TRAIN OR AIRCRAFT", "SHOTS FROM MOVING VEHICLE", "SHOTS INHABITED DWELLING", "STALKING", "TELEPHONE PROPERTY - DAMAGE", "THREATENING PHONE CALLS/LETTERS", "THREATS, VERBAL/TERRORIST", "THROWING OBJECT AT MOVING VEHICLE", "TRESPASSING", "UNAUTHORIZED COMPUTER ACCESS", "VIOLATION OF COURT ORDER", "VIOLATION OF RESTRAINING ORDER", "VIOLATION OF TEMPORARY RESTRAINING ORDER",
                                                                     "WEAPONS POSSESSION/BOMBING" ), 'OTHER', crime_data$crime_category)  

length(unique(crime_data$crime_category))
table(crime_data$crime_category)


CrimeCountsbyCategory <- as.data.frame(table(crime_data$crime_category))
CrimeCountsbyCategory

#analysis
#histogram
hist <- ggplot(crime_data, aes(crime_data$crime_category)) + geom_bar(position = "dodge", fill="red", colour="darkgreen") + xlab("\nCrime") + ylab("\nFrequency") +   ggtitle("Histogram")+ theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))
hist
#count crimes per year
count(crime_data, c(crime_data$Year))

#hourly analysis for theft for all years
theft<- crime_data[which(crime_data$crime_category=='THEFT'),]
theft
aggtheftHourly <- aggregate(theft$Crime.Count ~ theft$hour+theft$Year, data = theft, FUN = sum, na.rm = TRUE)

aggtheftHourly$`theft$hour`[aggtheftHourly$`theft$hour`==''] <- 0
aggtheftHourly
sortedhourly <- aggtheftHourly[order(as.numeric(as.character(aggtheftHourly$`theft$hour`))), ]
sortedhourly

lc <- ggplot(sortedhourly, aes(x=as.numeric(sortedhourly$`theft$hour`), y=sortedhourly$`theft$Crime.Count`)) + 
  geom_line(aes(colour=sortedhourly$`theft$Year`, group=sortedhourly$`theft$Year`)) + ggtitle("Theft count on an hourly basis")+
  xlab("\nHour of the day") + ylab("\nTheft Count")+  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) + scale_colour_gradientn(colours=rainbow(7)) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
lc + labs(colour='')

#hourly analysis for theft for all areas
theft<- crime_data[which(crime_data$crime_category=='THEFT'),]
theft
aggtheftHourlyArea<- aggregate(theft$Crime.Count ~ theft$hour+theft$AREA.NAME, data = theft, FUN = sum, na.rm = TRUE)

aggtheftHourlyArea$`theft$hour`[aggtheftHourlyArea$`theft$hour`==''] <- 0
aggtheftHourlyArea
sortedhourlyArea <- aggtheftHourlyArea[order(as.numeric(as.character(aggtheftHourlyArea$`theft$hour`))), ]
sortedhourlyArea

lc3 <- ggplot(aggtheftHourlyArea, aes(x=aggtheftHourlyArea$`theft$hour`, y=aggtheftHourlyArea$`theft$Crime.Count`)) + geom_line(aes(colour=aggtheftHourlyArea$`theft$AREA.NAME`, group=aggtheftHourlyArea$`theft$AREA.NAME`)) + ggtitle("Theft count on an hourly basis") +xlab("\nHour of the day") + ylab("\nTheft Count")+  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) + scale_colour_gradientn(colours=rainbow(7)) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
lc3 
                
                
                
#count crimes per year
count(crime_data, c(crime_data$Year))

#aggregate crime by year
aggcrimeYear <- aggregate(crime_data$Crime.Count ~ crime_data$Year + crime_data$crime_category, data = crime_data, FUN = sum, na.rm = TRUE)
aggcrimeYear


#Comparison of crime count for all categories of crime 2012-2015
bc2 <- ggplot(data=aggcrimeYear, aes(x=aggcrimeYear$`crime_data$Year`, y=aggcrimeYear$`crime_data$Crime.Count`, fill=aggcrimeYear$`crime_data$crime_category`)) +
  geom_bar(stat="identity", position=position_dodge()) + ggtitle("Comparison of crime count for all categories, 2012-2015") + xlab("\nYear") + ylab("\nCrime Count")+  theme(legend.position = "top", plot.title = element_text(hjust = 0.5))  
bc2
bc2 + guides(fill=guide_legend(title=""))


#plot crimes per area
library(ggplot2)
#count crimes per area
AreaCounts <- as.data.frame(table(crime_data$AREA.NAME))
AreaCounts

areacount <- ggplot(AreaCounts, aes(x = Var1, y = Freq)) + geom_line((aes(group = 1))) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
areacount 

#count crime category by area
aggCrimeCatArea <- aggregate(Crime.Count ~ crime_data$AREA.NAME + crime_data$crime_category, data = crime_data, FUN = sum, na.rm = TRUE)
aggCrimeCatArea

#aggregate area 
aggAreaCrimes <- aggregate(crime_data$Crime.Count ~ crime_data$AREA.NAME + crime_data$Year, data = crime_data, FUN = sum, na.rm = TRUE)
aggAreaCrimes

#comparison of crime count per month for all 4 years
lc1 <- ggplot(aggAreaCrimes, aes(x=aggAreaCrimes$`crime_data$AREA.NAME`, y=aggAreaCrimes$`crime_data$Crime.Count`)) + 
  geom_line(aes(colour=aggAreaCrimes$`crime_data$Year`, group=aggAreaCrimes$`crime_data$Year`)) + # colour, group both depend on cond2
  geom_point(aes(colour=aggAreaCrimes$`crime_data$Year`),               # colour depends on cond2
             size=2) +  ggtitle("Area-wise comparison of crime count")+
  xlab("\nArea") + ylab("\nCrime Count")+  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) + scale_colour_gradientn(colours=rainbow(4)) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
lc1 + labs(colour='')

#aggregate crime by month and category
aggCrimeMonth <- aggregate(crime_data$Crime.Count~ crime_data$Month + crime_data$crime_category, data = crime_data, FUN = sum, na.rm = TRUE)
aggCrimeMonth


#Crime count for all categories on a monthly basis from 2012-2015
a <- ggplot(aggCrimeMonth, aes(x=aggCrimeMonth$`crime_data$Month`, y=aggCrimeMonth$`crime_data$Crime.Count`)) + 
  geom_line(aes(colour=aggCrimeMonth$`crime_data$crime_category`, group=aggCrimeMonth$`crime_data$crime_category`)) + # colour, group both depend on cond2
  geom_point(aes(colour=aggCrimeMonth$`crime_data$crime_category`),               # colour depends on cond2
             size=3) + scale_x_continuous(breaks=c(seq(1,12,1)))+
  scale_y_continuous(breaks=c(seq(0,25000,5000))) + 
  ggtitle("Comparison of crime count for all categories on a monthly basis, 2012-2015")+
  xlab("\nMonth") + ylab("\nCrime Count")+ theme(legend.position = "top", plot.title = element_text(hjust = 0.5))  

a + labs(colour='')

#aggregate crime by monthyear
aggmonthYear <- aggregate(crime_data$Crime.Count ~ crime_data$Month + crime_data$Year, data = crime_data, FUN = sum, na.rm = TRUE)
aggmonthYear

#Crime count on a monthly basis from 2012-2015
b <- ggplot(aggmonthYear, aes(x=aggmonthYear$`crime_data$Month`, y=aggmonthYear$`crime_data$Crime.Count`)) + 
  geom_line(aes(colour=aggmonthYear$`crime_data$Year`, group=aggmonthYear$`crime_data$Year`)) + scale_x_continuous(breaks=c(seq(1,12,1)))+ scale_y_continuous(breaks=c(seq(0,25000,5000))) +  ggtitle("Comparison of crime count on a monthly basis")+ xlab("\nMonth") + ylab("\nCrime Count")+ scale_colour_gradientn(colours=rainbow(3))+theme(legend.position = "top", plot.title = element_text(hjust = 0.5))  

b + labs(colour='')

#pie chart
slices <- c(153150, 152222, 10434, 84306, 10259, 270218, 11141, 166854, 74616) 
lbls <- c("Assault", "Burglary", "Fraud", "Other","Sex","Theft", "Child Abuse","Traffic","Vandalism")
pct <- round(slices/sum(slices)*100, 1)

lbls <- paste(pct,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls, main="Percentage of Crime Per Category", col= rainbow(length(slices)))
par(xpd=TRUE) # Allow legend to be plotted outside plot
legend("top", c("Assault", "Burglary", "Fraud", "Other","Sex","Theft", "Child","Traffic","Vandalism"), cex=0.6, border=NA, fill = rainbow(length(slices)), horiz = TRUE)

install.packages('caTools')
library(caTools)

set.seed(88)
split <- sample.split(crime_data$Recommended, SplitRatio = 0.5)


Time<- crime_data$TIME.OCC
x <- data.frame(table(crime_data$TIME.OCC))
colnames(x) <- c("Time_Day","Count_Theft")

Time<-x$Time_Day
TheftCount<-x$Count_Theft

Time_new <- as.numeric(as.character( Time ))
timecode <- ifelse(Time_new >= 900 & Time_new < 2100,0,1)

plot(TheftCount,jitter(timecode,0.15),pch=19,xlab="Count of Thefts",ylab="Time of Day (0 - Day, 1 - Night)")

#fit logistic regression model using glm function
model <- glm(timecode~TheftCount,binomial)
summary(model)

xv<-seq(min(TheftCount),max(TheftCount),0.01)
yv<-predict(model,list(TheftCount=xv),type="response")
lines(xv,yv,col="red")

x[order(-x$Count_Theft),]
x_new <-x[-c(720),]
cat("\014")  

x_new[order(-x_new$Count_Theft),]

Time<-x_new$Time_Day
TheftCount<-x_new$Count_Theft

Time_new <- as.numeric(as.character( Time ))
timecode <- ifelse(Time_new >= 900 & Time_new < 2100,0,1)

plot(TheftCount,jitter(timecode,0.15),pch=19,xlab="Count of Thefts",ylab="Time of Day (0 - Day, 1 - Night)")

#fit logistic regression model using glm function
model <- glm(timecode~TheftCount,binomial)
summary(model)

xv<-seq(min(TheftCount),max(TheftCount),0.01)
yv<-predict(model,list(TheftCount=xv),type="response")
lines(xv,yv,col="red")

