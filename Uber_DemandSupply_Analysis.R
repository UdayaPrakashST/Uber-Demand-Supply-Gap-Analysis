
rm(list=ls())
#--------------------------------------------------------------------------
#Read data into R
#<Segment Starts here>

uber_request_data <-
  read.csv("Uber Request Data.csv",
           stringsAsFactors = F,
           header = T)
#<Segment Ends here>
#--------------------------------------------------------------------------


#---------------------------------------------------------------------------
#Univariate Analysis And Data Discrepancy Analysis
#<Segment Starts here>

#Explore to identify inconsistencies or quality issues in dataset

#Get summary of dataset

str(uber_request_data)
summary(uber_request_data)

# Initial Understanding
#No of observations and fields
nrow(uber_request_data)
ncol(uber_request_data)

#Request.id is the primary key of the table

#Primary Key Check
#NA values check
ifelse(sum(is.na(uber_request_data$Request.id))==0,"No NA values","NA values are present")

ifelse(
  nrow(uber_request_data) == length(unique(uber_request_data$Request.id)),
  paste("Request.id is a primary key of the dataset"),
  paste("Request.id contains duplicate values and needs more exploration")
)
#RequestID is the primary key of this dataset
#Pickup.point

#NA values check
ifelse(sum(is.na(uber_request_data$Pickup.point))==0,"No NA values","NA values are present")
# No NA values observed.
#Unique Values
unique(uber_request_data$Pickup.point)
#There are two pickup points in the dataset. City and Airport.
# Cross tab of Pickup.Point
table(uber_request_data$Pickup.point)



#Driver.id 
ifelse(sum(is.na(uber_request_data$Driver.id))==0,"No NA values","NA values are present")
#NA values are present in Driver.id

#percentage of NA values in Driver.id column
sum(is.na(uber_request_data$Driver.id))
(sum(is.na(uber_request_data$Driver.id))/nrow(uber_request_data))*100

#Approximately 39% of data in Driver.id column is NA. 

#Number of unique drivers
length(unique(uber_request_data[which(is.na(uber_request_data$Driver.id)==F),3]))
#There are 300 unique drivers in the data set.


#Status
ifelse(sum(is.na(uber_request_data$Status))==0,"No NA values","NA values are present")
#No Na values are present in this column

#Unique Values
unique(uber_request_data$Status)
#There are three unique values of status
#"Trip Completed"    "Cancelled"         "No Cars Available"

#Cross tab of Status to show occurence frequency

table(uber_request_data$Status)

#2831 Trips are completed.
#1264 Trips are cancelled by Drivers
#2650 Trips are cancelled due to unavailability of cars


#Request.timestamp

#Determine the datatype of the field

typeof(uber_request_data$Request.timestamp)

#The field is of type character indicating string values are entered.

#Data Quality?
table(nchar(uber_request_data$Request.timestamp))

#Variable character lengths of 14,15 and 19 are observed.

#Analyze why differnce of 1 character is present in Request.timestamp (nchar14 vs nchar15)
head(uber_request_data[which(nchar(uber_request_data$Request.timestamp)==14),5])

head(uber_request_data[which(nchar(uber_request_data$Request.timestamp)==15),5])

#date format is consistent for lengths of 14 and 15.This is basically due 
#to differnce in digits of hours(0-9 1 digit,>9 2 digits.)

#There is no seconds field available for this 



#Lets analyze the character length of 19 available in the dataset

head(uber_request_data[which(nchar(uber_request_data$Request.timestamp)==19),5])

#the Seconds field is available when Request.timestamp has character length of 19

#<Segment Ends here>
#Findings:
#Data Discrepancy Identified in Request.timestamp and Drop.timestamp


#--------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#Data Cleaning 

#<Segment Starts here> 
#Can Lubridate take care of this issue? Yes, Absolutely.
#install.packages("lubridate")
#library(lubridate)




uber_request_data$Request.timestamp_formatted <-
  parse_date_time(uber_request_data$Request.timestamp,
                  c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"))

uber_request_data$Drop.timestamp_formatted <-
  parse_date_time(uber_request_data$Drop.timestamp,
                  c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"))


#<Segment Starts here> 
#------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#Derived Fields

#<Segment Starts Here>

uber_request_data$Request<-1
uber_request_data$Completed<- ifelse(uber_request_data$Status=="Trip Completed",1,0)
uber_request_data$Not_Completed<- ifelse(uber_request_data$Status!="Trip Completed",1,0)

uber_request_data$Request.timestamp_date<-date(uber_request_data$Request.timestamp_formatted)
uber_request_data$Request.timestamp_year<-year(uber_request_data$Request.timestamp_formatted)
uber_request_data$Request.timestamp_month<-month(uber_request_data$Request.timestamp_formatted)
uber_request_data$Request.timestamp_day<-day(uber_request_data$Request.timestamp_formatted)
uber_request_data$Request.timestamp_dayofweek<-wday(uber_request_data$Request.timestamp_formatted,label = T)
uber_request_data$Request.timestamp_hour<-hour(uber_request_data$Request.timestamp_formatted)
uber_request_data$Request.timestamp_minute<-minute(uber_request_data$Request.timestamp_formatted)

uber_request_data$Drop.timestamp_date<-date(uber_request_data$Drop.timestamp_formatted)
uber_request_data$Drop.timestamp_year<-year(uber_request_data$Drop.timestamp_formatted)
uber_request_data$Drop.timestamp_month<-month(uber_request_data$Drop.timestamp_formatted)
uber_request_data$Drop.timestamp_day<-day(uber_request_data$Drop.timestamp_formatted)
uber_request_data$Drop.timestamp_dayofweek<-wday(uber_request_data$Drop.timestamp_formatted,label = T)
uber_request_data$Drop.timestamp_hour<-hour(uber_request_data$Drop.timestamp_formatted)
uber_request_data$Drop.timestamp_minute<-minute(uber_request_data$Drop.timestamp_formatted)



uber_request_data$Request_timeslots<-ifelse(uber_request_data$Request.timestamp_hour<=3,"Early Morning",
                                            ifelse(uber_request_data$Request.timestamp_hour<=7,"Morning",
                                                   ifelse(uber_request_data$Request.timestamp_hour<=11,"Late Morning",
                                                          ifelse(uber_request_data$Request.timestamp_hour<=15,"After Noon",
                                                                 ifelse(uber_request_data$Request.timestamp_hour<=19,"Evening","Late Evening")))))

#<Segment Ends here>
#--------------------------------------------------------------------------------------------
#Load Required Libraries
#<Segment Starts here>
library(reshape2)

#Letsplot!
library(ggplot2)

#Lets plot a bar chart of Demand Vs Supply at a date level
library(dplyr)

library(scales)
#<Segment Ends here>
-------------------------------------------------------------------------------
#############################################################################
#Analysis  
###########################################################################  

#<Segment Starts Here>

#Analysis1: Day wise analysis

#Completed and Not Completed Requests Across Different Days

date_aggregated_dataset <- uber_request_data %>%  
  group_by(Pickup.point,Request.timestamp_date)%>% 
  summarise(Request=sum(Request),Completed=sum(Completed),CompletedtoRequest=sum(Completed)/sum(Request))

#Plot Completed and Not Completed trips
plot_dataset<-melt(date_aggregated_dataset[,-5],id=c("Request.timestamp_date","Pickup.point"))


ggplot(plot_dataset, aes(x = Request.timestamp_date, y = value)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8))+
  facet_wrap(~Pickup.point,nrow = 2)+
  xlab("Request Date")+
  ylab("Sum")+
  ggtitle("Requests and Completed trips across days")

#There is a very high Gap between Supply and Demand

#Plot CompletedtoRequest Ratio in a line chart across different days

plot_dataset1<-melt(date_aggregated_dataset[,-c(3,4)],id=c("Request.timestamp_date","Pickup.point"))

#Mean CompletedtoRequest Ratio
Mean_CompletedtoRequest<- sum(uber_request_data$Completed)/sum(uber_request_data$Request)


ggplot(plot_dataset1,aes(x=Request.timestamp_date,y=value,label=percent(value)))+geom_line(color='steelblue', size=2)+
  geom_text()+scale_y_continuous(labels = scales::percent)+
  geom_hline(aes(yintercept=Mean_CompletedtoRequest), colour="#BB0000", linetype="dashed")+
  facet_wrap(~Pickup.point,nrow = 2)+
  xlab("Request Date")+
  ylab("CompletedtoRequestRatio")+
  ggtitle("Completed-RequestRatio Across Days")

#Analysis1:Observation:
##Uber CompletedtoRequest Ratio is very low. Average CompletedtoRequest ratio 
##is 42%


#Analysis2: Trip Status Analysis



percentData <- uber_request_data %>% group_by(Pickup.point) %>% count(Status) %>%
  mutate(ratio=scales::percent(n/sum(n)))

ggplot(uber_request_data,aes(x = Pickup.point,fill = Status)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent_format())+
  geom_text(data=percentData, aes(y=n,label=ratio),
            position=position_fill(vjust=0.5))+
  xlab("Pickup Point")+
  ylab("Percentage Distribution")+
  ggtitle("Percentage Distribution of Trip Status across pickup points")

#Analysis2: Observation
##53% of total requests from airports have been cancelled due to cars unavailability
##30% of total requests from cities have been cancelled by drivers and 26% have been cancelled due to cars unavailability 

#Analysis3: Not Completed Trips Distribution
##Lets see Percentage distribution of Trips Not completed 

percentData2 <- uber_request_data %>% filter(Status!="Trip Completed") %>% group_by(Pickup.point) %>% count(Status) %>%
  mutate(ratio=scales::percent(n/sum(n)))

ggplot(uber_request_data[uber_request_data$Status!="Trip Completed",],aes(x = Pickup.point,fill = Status)) + 
  geom_bar(position = "fill") + 
  #geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  geom_text(data=percentData2, aes(y=n,label=ratio),
            position=position_fill(vjust=0.5))+
  xlab("Pickup Point")+
  ylab("Percentage Distribution")+
  ggtitle("Percentage Distribution of Cancelled Trip Status across pickup points")

#Analysis3: Observation
#90% of incomplete trips from airports are due to Car Unavailability
#54% of incomplete trips from airports are due to drivers cancellation
  
#Analysis4: Hour Wise Analysis

hour_Pickup_aggregated_dataset <-uber_request_data %>% 
  group_by(Pickup.point,Request.timestamp_hour) %>%
  summarize(Completed=sum(Completed),NotCompleted=sum(Not_Completed),CompletedtoRequestRatio=sum(Completed)/sum(Request))

plot_dataset3<-melt(hour_Pickup_aggregated_dataset[,-5],id=c("Request.timestamp_hour","Pickup.point"))




ggplot(plot_dataset3, aes(x = factor(Request.timestamp_hour), y = value,fill=factor(variable),label=value)) + 
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~Pickup.point,nrow = 2)+
  geom_text(angle=45,size=2,position="dodge")+
  xlab("Hour of day")+
  ylab("Count")+
  scale_fill_discrete(name = "Trips")+
  ggtitle("Completed and Not Completed Trips across hours of day")


  
  
  plot_dataset4<-melt(hour_Pickup_aggregated_dataset[,-(3:4)],id=c("Request.timestamp_hour","Pickup.point"))
  
  
  ggplot(plot_dataset4, aes(x = factor(Request.timestamp_hour),group=1, y =value,label=percent(value))) + 
    geom_line(color='steelblue', size=2)+
    geom_point()+
    facet_wrap(~Pickup.point,nrow = 2)+
    scale_y_continuous(labels = percent_format())+
    geom_text(angle=45,size=3, vjust = 2)+
    xlab("Hour of day")+
    ylab("CompletedtoRequestRatio")+
    scale_fill_discrete(name = "Trips")+
    ggtitle("CompletedtoRequest across different hours of day")
  

#Analysis4: Observations 
  #There are different trends across the two pick up points across different hours of day.
  
  

  
  
  
#Analyis5: Analysis across TimePeriod of Day  
  
  hourbucket_Pickup_aggregated_dataset <-uber_request_data %>% 
    group_by(Pickup.point,Request_timeslots) %>%
    summarize(Completed=sum(Completed),NotCompleted=sum(Not_Completed),CompletedtoRequestRatio=sum(Completed)/sum(Request))
 
  
   hourbucket_Pickup_aggregated_dataset$order<-ifelse(hourbucket_Pickup_aggregated_dataset$Request_timeslots=="Early Morning",1,
                                                     ifelse(hourbucket_Pickup_aggregated_dataset$Request_timeslots=="Morning",2,
                                                            ifelse(hourbucket_Pickup_aggregated_dataset$Request_timeslots=="Late Morning",3,
                                                                   ifelse(hourbucket_Pickup_aggregated_dataset$Request_timeslots=="After Noon",4,
                                                                          ifelse(hourbucket_Pickup_aggregated_dataset$Request_timeslots=="Evening",5,6)))))
  
  
  
  
  plot_dataset5<-melt(hourbucket_Pickup_aggregated_dataset[,-5],id=c("Request_timeslots","Pickup.point","order"))
  
  
  
  ggplot(plot_dataset5, aes(x =reorder(Request_timeslots,order), y = value,fill=factor(variable),label=value)) + 
    geom_bar(stat="identity",position="dodge")+
    facet_wrap(~Pickup.point,nrow = 2)+
    geom_text(position = "dodge",size=2)+
    xlab("TimerPeriod of Day")+
    ylab("Count")+
    ggtitle("Completed and Not Completed Trips across different time periods")+
    scale_fill_discrete(name = "Trips")
  
  
  plot_dataset6<-melt(hourbucket_Pickup_aggregated_dataset[,-(3:4)],id=c("Request_timeslots","Pickup.point","order"))
  
  
  ggplot(plot_dataset6, aes(x = reorder(Request_timeslots,order),group=1, y =value,label=percent(value))) +
    geom_line(color='steelblue', size=2)+
    facet_wrap(~Pickup.point,nrow = 2)+
    scale_y_continuous(labels = percent_format())+
    geom_text(size=3)+
    xlab("TimerPeriod of Day")+
    ylab("Count")+
    ggtitle("CompletedtoRequest ratio  across different time periods")

  
#Analysis 5: Observations  
#Above graph indicates that early Morning has very low CompletedtoRequestRatio across both the pick up points 
#Considering airport as the pickup point, Evening and Late Evening have a very low CompletedtoRequestRatio
#Considering City as the pickup point, Morning and Late Morning have a very low CompletedtoRequestRatio

#Analysis 6 :Early Morning analysis

  percentData3 <- uber_request_data %>%filter(Request_timeslots=="Early Morning",Status!="Trip Completed") %>% group_by(Pickup.point) %>% count(Status) %>%
    mutate(ratio=scales::percent(n/sum(n)))

  plot_dataset7<-uber_request_data %>%filter(Request_timeslots=="Early Morning",Status!="Trip Completed")
  ggplot(plot_dataset7,aes(x = Pickup.point,fill = Status)) + 
    geom_bar(position = "fill") + 
    #geom_bar(position = position_fill(), stat = "identity") 
    scale_y_continuous(labels = percent_format())+
    geom_text(data=percentData3, aes(y=n,label=ratio),
              position=position_fill(vjust=0.5))+
    xlab("Pickup Point")+
    ylab("Percentage Distribution")+
    ggtitle("Not Completed Trips during Early Morning(00:00-03:59)")


  #Analysis 5: Observations 
#Looking at trips not completed across pick up points during early morning
#1. 100 percent of the time incomplete trips from airport have been because of No cars availability
#2. More than 85% of the incomplete trips from airport have been because of No cars availability
  
  
#Analysis 6: #Evenings and Late Evenings Analysis:
  
  percentData4 <- uber_request_data %>%filter((Request_timeslots=="Evening"|Request_timeslots=="Late Evening"),Status!="Trip Completed",Pickup.point=="Airport") %>% group_by(Request_timeslots) %>% count(Status) %>%
    mutate(ratio=scales::percent(n/sum(n)))
  plot_dataset8<-uber_request_data %>%filter((Request_timeslots=="Evening"|Request_timeslots=="Late Evening"),Status!="Trip Completed",Pickup.point=="Airport")
 
   ggplot(plot_dataset8,aes(x =Request_timeslots ,fill = Status)) + 
    geom_bar(position = "fill") + 
    #geom_bar(position = position_fill(), stat = "identity") 
    scale_y_continuous(labels = percent_format())+
    geom_text(data=percentData4, aes(y=n,label=ratio),
              position=position_fill(vjust=0.5))+
     xlab("TimePeriod of day")+
     ylab("Percentage Distribution")+
     ggtitle("Not Completed Trips from Airport during Evening and Late evening")
   
#Analysis 6 Observations
#More than 90% of incomplete trips during evening and late evening from Airport
#are due to cab inavailability

#Analysis 7 Observations
   
#Morning and Late Morning Analysis
   
   percentData5 <- uber_request_data %>%filter((Request_timeslots=="Morning"|Request_timeslots=="Late Morning"),Status!="Trip Completed",Pickup.point=="City") %>% group_by(Request_timeslots) %>% count(Status) %>%
     mutate(ratio=scales::percent(n/sum(n)))
   plot_dataset9<-uber_request_data %>%filter((Request_timeslots=="Morning"|Request_timeslots=="Late Morning"),Status!="Trip Completed",Pickup.point=="City") %>% group_by(Request_timeslots)
   
   ggplot(plot_dataset9,aes(x =Request_timeslots ,fill = Status)) + 
     geom_bar(position = "fill") + 
     scale_y_continuous(labels = percent_format())+
     geom_text(data=percentData5, aes(y=n,label=ratio),
               position=position_fill(vjust=0.5))+
     xlab("TimePeriod of day")+
     ylab("Percentage Distribution")+
     ggtitle("Not Completed Trips from city during Morning(04:00-07:59) and Late morning (08:00-11:59)")
   
#Analysis 6: Observations
   
#More than 60% of the incomplete trips during Morning and Late Morning
#are because of the drivers cancelling it
   
#Possible Hypothesis : Driver Wait time to get a trip right during this time slots might be higher.
   
#<Segment Ends here>
   
#------------------------------------------------------------------------------   

#Recommendations
#   *- Uber should increase the number of cars available during Early Morning
   
#   *-90% of incomplete trips during evening and late evening from airport is
   #due to No cars availability. This can be associated to higher number of 
   #flights landing during this period and lower influx of cabs to airport 
   #during the earlier part of the day
   
#   *- More than 60% of the incomplete trips from City during 
   #Morning and Late Morning are because of the drivers cancelling it. 
   #This can be attributed to increase in wait time of drivers after 
   #reaching airport
   
 #---------------------------------------------------------------------------
   
#End   