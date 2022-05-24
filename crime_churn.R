CrimeDataset<-read.csv(file = '/Users/leechenhsin/Desktop/Study@USA/07_UW_School/IMT573/crime_data.csv')
summary(CrimeDataset)

getmode <- function(v) {uniqv <- unique(v) uniqv[which.max(tabulate(match(v, uniqv)))]}
Occurredtime<-getmode(CrimeDataset$Occurred.Time)
Occurredtime

#For my observation, I found that the most time that crime event happened is at
#night like 22:00. Besides, the most happened crime subcategory is car prowl.
#In beat area, the most common area that beat happened is K3 and the most common
#neighborhood that crime event happened is downtown commercial.

Occurred.Year<- substr(CrimeDataset$Occurred.Date,7,10)
Occurred.Year=as.integer(Occurred.Year)
Occurred.Year=na.omit(Occurred.Year)
min(Occurred.Year)

subsetcrime<- na.omit(CrimeDataset)
subsetcrime$Year <- Occurred.Year
subsetcrime %>%
  group_by(Year)%>%
      summarise(count=n())%>%
        arrange(-count)
        
number<-group_by(subsetcrime, Year)
yearnumber<-summarise(number,count=n())
subsetcrime2<- merge(subsetcrime,yearnumber,by='Year')


 ggplot(data=subsetcrime2, aes(x=Year, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+ ggtitle("year trend")+xlab("year") + ylab("crime member")

 getmode <- function(v) {
   uniqv <- unique(v) uniqv[which.max(tabulate(match(v, uniqv)))]
 }
 Occurredtime<-getmode(CrimeDataset$Occurred.Time)
 Occurredtime
 
 
 For my observation, I found that the most time that crime event happened is at
 #night like 22:00. Besides, the most happened crime subcategory is car prowl.
 #In beat area, the most common area that beat happened is K3 and the most common
 #neighborhood that crime event happened is downtown commercial.
 
 
 subsetcrime<- na.omit(CrimeDataset)
 subsetcrime$Year <- Occurred.Year
 subsetcrime %>%
   group_by(Year)%>%
   summarise(count=n())%>%
   arrange(-count)
 
 number<-group_by(subsetcrime, Year)
 yearnumber<-summarise(number,count=n())
 subsetcrime2<- merge(subsetcrime,yearnumber,by='Year')
 
 
 ggplot(data=subsetcrime2, aes(x=Year, y=count)) +
   geom_bar(stat="identity", fill="steelblue")+
   theme_minimal()+ ggtitle("year trend")+xlab("year") + ylab("crime number")
 
 crime.after.2012=filter(subsetcrime2,Year>=2012)
 
 #A police beat means the police department and the territory that a police
 #officer patrols.
 #And in this dataset beat means designated police sector boundary where
 #offense(s) occurred.
 # Bar
 ggplot(crime.after.2012, aes(x=factor(Beat)))+
   geom_bar(stat="count", width=0.7, fill="steelblue")+
   theme_minimal()+ ggtitle("The beats in the Crime")+xlab("Beat") + ylab("Count")
 
 
 summary(crime.after.2012$Beat)
 #Yes,there are 2054 missing beats.
 
 
 BeatsDataset<-read.csv(file = '/Users/leechenhsin/Desktop/Study@USA/07_UW_School/IMT573/police_beat_and_precinct_centerpoints.csv')
 BeatsDataset
 
 #dplyr summarize (setdf)
 #Yes,Crime Dataset include 10  police beats that are not present in the Beats
 #Dataset. The frequency of them are CTY:1, DET:7,H1:0, INV:0, K:1,
 #LAPT:0, S:4, SS:1, WS:1,X9:0
 #They are rather infrequent in the dataset.
 #I think remove them will not drastically alter the scope of the Crime Dataset
 #since the number of them does't account big enough.
 summary(crime.after.2012$Beat)
 
 
 #crime.after.2012$Beat=toString(crime.after.2012$Beat)
 new_crime.after.2012=filter(crime.after.2012,crime.after.2012$Beat!="")
 beat_group=group_by(new_crime.after.2012,new_crime.after.2012$Beat)
 beat_summary=summarise(beat_group,count=n())
 
 beat_greater_than_10=filter(beat_summary,count>=10)
 sum(beat_greater_than_10$count)
 
 
library(tigris)
call_geolocator_latlon(40.61847, -74.02123)
 
geolocate <-mapply(call_geolocator_latlon, lat = BeatsDataset$Latitude, lon =BeatsDataset$Longitude)
BeatsDataset$geoloaction=geolocate

Statecode<- substr(BeatsDataset$geoloaction,1,2)
Countrycode<- substr(BeatsDataset$geoloaction,3,5)
BeatsDataset$Statecode=Statecode
BeatsDataset$Countrycode=Countrycode
#yes, the extracted state and country codes are what I am expect to be.
#since the state code of WA State is 53 and country code is 033.

digitcode<- substr(BeatsDataset$geoloaction,1,11)
BeatsDataset$digitcode=digitcode

censusdata<-read.csv(file = '/Users/leechenhsin/Desktop/Study@USA/07_UW_School/IMT573/census_edu_data.csv')
digitcode<- substr(censusdata$GEO.id,10,21)
censusdata$digitcode=digitcode

beat_census= censusdata %>% inner_join(BeatsDataset,by="digitcode")
save(beat_census,file='/Users/leechenhsin/Desktop/study@USA/07_UW_School/IMT573/beat_census.RData')

              







