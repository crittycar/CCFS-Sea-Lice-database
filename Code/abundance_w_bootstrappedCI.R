#making the mean lice over time with sites.

#subset the best2019 to get right species (fishbest2019). Then only take the columns that I want to show mean lice stages : stage sums per fish, date, j.date and grouped sites.
#subset to focus on Cypre, ritchie and bedwell north
#Then assign weekly intervals to dates 
#Now you can subset by site
#then you can use bootstrap resampling to get the means of sites by the assigned weekly intervals.
#create table for mean lci and uci of each site for each date
#Now you can plot each site's mean, lci and uci over time
#you can subplot this too. I think it will end up showing bedwell has lowest numbers, ritchie highest, cypre also high.


n.boot.b<-1000


wd
getwd()
wd <- getwd()  # working directory
setwd(wd)
folders <- c("Code", "Data", "OutputFigures", "OutputData")

# function to create folders below
for(i in 1:length(folders)){
  if(file.exists(folders[i]) == FALSE)
    dir.create(folders[i])
}

# we also need to store the paths to these new folders

code.output.path <- paste(wd, "/", folders[1], sep = "")
data.input.path <- paste(wd, "/", folders[2], sep = "")
figures.path <- paste(wd, "/", folders[3], sep = "")
data.output.path <- paste(wd, "/", folders[4], sep = "")

# our raw data is stored in different folders, lets make the paths
forplots2020.path <- paste(wd, "/", "Data", sep = "")

# now we can access and save stuff to these folders!



#---------------------Below, we upload and clean the  data----------


# time to upload the datas into folder
forplots2020 <- read.csv(paste(forplots2020.path, "/", "forplots2020.csv",
                               sep = ""), stringsAsFactors = FALSE)

# I couldn't get this wd to work on the second try. So I have made my own here
#wd <- "C:/Users/Rowen/OneDrive/Desktop/github/CCFS-Sea-Lice-database/Data"
setwd(wd)
forplots2020 <- read.csv("data/forplots2020.csv")



#unhashtag to install packages below 
#install.packages(c("boot", "MASS","plyr","dplyr", "ggplot2", "tibble", "car", "reshape2",
#                  "epitools", "readxl", "tidyverse","arsenal")))
library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(car)
library(reshape2)
library(epitools)
library(readxl)
library(tidyverse)
library(readr)
library(arsenal)

warnings()

#***************************
#change the file and/or sheet name if necessary
#if you want plots 
#I make a csv of final data, as the read_xlsx function was giving me an error in August. 


#bestclay <- read_xlsx("Best.Clayoquot.Sealice.data.2019.xlsx", sheet = "clayoquot.sealice.fish.data", 
#col_names = TRUE)

#************************ 
#change the year if necessary
# RM : I am changing the instances of "2020" so that we can just put the vector "yr" instead which we will define here, at the beginning
yr <- "2020"
data2020<-subset(forplots2020, year == yr)

# RM : I think it might be worthwhile defining the list of sites you want to include 
# in the study, here. But you might need to go through the code to replace the various
# site lists with this vector. And make sure that this vector is defined appropriately so that you can automate it.
# something like ...
# site.list <- unique(data2020$location)

#You may want to remove pink from the analysis. There aren't many of them in the data.
best2020<-data.frame(subset(data2020, species == "coho"|species == "chum"|species == "chinook"|
                              species == "sockeye"|species == "pink"))
best2020$year<-as.numeric(best2020$year)

#adjusting to as.date and as.numeric***********************
best2020$date <- as.Date(with(best2020, paste(year, month, day, sep="-")), "%Y-%m-%d")
best2020$height<-as.numeric(best2020$height)
best2020<-data.frame(best2020)
best2020[ , 11:25][is.na(best2020[ , 11:25] ) ] <- 0 
names(best2020)[1]<-paste("fish_id")
best2020$sum_all_lice[is.na(best2020$sum_all_lice)]<-0

#for some reason the sum_all_lice column is not calculating adding all the lice counts properly
#to fix this we need to replace the column entirely by summing across all the rows
#library(dplyr)

best2020 <- best2020 %>% rowwise() %>%
  dplyr::mutate(Sum_all_lice = sum(c_across(Lep_cope:unid_adult)))


warnings()
#comparedsums <- cbind(best2020$Sum_all_lice,best2020$sum_all_lice)


#now lets us aresenal to compare the two sum coloumns 
#sum <- data.frame(best2020$sum_all_lice)
#Sum <- data.frame(best2020$Sum_all_lice)
#summary(comparedf(sum,Sum))
#Not sure why there is no difference in values, but you need to make sure to replace the sum_all_lice with a calculated version

#first mean estimate of lice abundances

#setting up the new names for the locations. 

#***********************
#If you have changed the name of any sites, please change them here to match
#the line of code below is used to lump sites together by naming them the same thing. 
#Example, Bedwell estuary 3 and 2 are now called Bedwell Estuary Middle
#Ex 2020 had no differentiation among bedwell sites, so all are just named Bedwell  

#from here its clear that 2020 only had 5 unique sample locations
#below, just make sure that those 5 correspond the right new names 
#etc: bedwell river <- bedwell estuary north
#change bedwell estuary to bedwell sound in all cases
best2020$location
best2020$groupedsites<-best2020$location
levels(best2020$groupedsites)<-c(levels(best2020$groupedsites), c("Bedwell Sound South","Bedwell Sound North","Bedwell Sound Middle"))
best2020$groupedsites[best2020$groupedsites == "Bedwell estuary"]<- "Bedwell Sound South"
best2020$groupedsites[best2020$groupedsites == "Bedwell estuary 4"]<-"Bedwell Sound North"
best2020$groupedsites[best2020$groupedsites == "Bedwell River"]<- "Bedwell Sound North"
best2020$groupedsites[best2020$groupedsites == "Bedwell estuary 2"]<- "Bedwell Sound Middle"
best2020$groupedsites[best2020$groupedsites == "Bedwell estuary 3"]<-"Bedwell Sound Middle"
best2020$groupedsites[best2020$groupedsites == "Sniffles"]<- "Bedwell Sound Middle"
best2020$groupedsites[best2020$groupedsites == "Sniffles 2"]<- "Bedwell Sound Middle"

#Just some subsets that are useful for outlining groups of sites, but weren't used much for plots.
#replace all estuary to sound
bedwell2020<-data.frame(subset(best2020, groupedsites == "Bedwell Sound North" | groupedsites == "Bedwell Sound Middle" | groupedsites == "Bedwell Sound South"))
Misc2020<- data.frame(subset(best2020, groupedsites == "Tranquil estuary"| groupedsites == "Keltsmaht"| groupedsites == "Moyeha"| groupedsites == "Elbow Bank" | groupedsites == "TRM"|groupedsites == "Tsapee Narrows"))
#make sure list of sites corresponds to actual list from grouped sites, I think- CC
unique(best2020$groupedsites)
listofsites<-c("Bedwell Sound North","North Meares", "Cypre River", "Ritchie Bay", "Tsapee Narrows")
Macks2020<- data.frame(subset(best2020, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay"))
Vargas2020<-data.frame(subset(best2020, groupedsites == "Elbow Bank" | groupedsites == "Buckle Bay"| groupedsites == "Keltsmaht"))
Herbertinlet2020<- data.frame(subset(best2020, groupedsites == "Moyeha"))
#Below = Tofino Inlet to Browning Passage to Duffin Passage
Tofino2020<- data.frame(subset(best2020, groupedsites == "Tranquil estuary" | groupedsites == "TRM"|groupedsites == "Tsapee Narrows"))
focus2020<-data.frame(subset(best2020, groupedsites == "North Meares" |groupedsites == "Cypre River" | groupedsites == "Ritchie Bay"))
#view(focus2020)
#summary(comparedf(focus2020,best2020))

## END OF SET UP ##

## CALCULATING USEFUL VALUES FOR PREVALENCE AND MEAN LICE PLOTS ##

#creating the lice sums for motile and attached
#These motsum attempts did not sum across the columns due to NA. So used na.rm = TRUE. 

#***********************
#may need to change the column numbers selected just below to include the written columns of the 
#following vectors if the main template changed. May also need to change the names of the written
#columns if the column names have changed.
colnames(best2020)
#not sure what this subset is for? -CC
#I think it is for a total count of the lice -RM
salmcounts<-subset(best2020[,c(11:25)])
#motile lice sub
motlice<-best2020[,c("Caligus_mot", "Caligus_gravid", "Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale", "unid_PA", "unid_adult")]
#cope lice sub
copes<-best2020[,c("Lep_cope", "Caligus_cope", "unid_cope")]
#chalimus lice sub
chals<-best2020[,c("chalA", "chalB", "chal_unid")]
#attached lice sub
attlice<-best2020[,c("Lep_cope","chalA","chalB","Caligus_cope","unid_cope","chal_unid")]
#total lice sub

unique(best2020$date)

#Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
best2020$motsum<-rowSums(motlice, na.rm = TRUE)
best2020$copsum<-rowSums(copes, na.rm = TRUE)
best2020$chalsum<-rowSums(chals, na.rm = TRUE)
best2020$attachedsum<-rowSums(attlice, na.rm = TRUE)
best2020$Sum_all_lice<-rowSums(salmcounts, na.rm = T)
best2020$motsum<- as.numeric(best2020$motsum)
best2020$copsum<- as.numeric(best2020$copsum)
best2020$attachedsum<-as.numeric(best2020$attachedsum)
best2020$Sum_all_lice<-as.numeric(best2020$Sum_all_lice)

##ASSIGNING WEEKLY INTERVALS TO ALL THE dates in the best2020 data set 
#weekly intervals.
#set up vectors to hold data


#making a table with weekly intervals.
juliandates<-julian(best2020$date)
firstday<-min(juliandates)
no.weeks<-ceiling((max(juliandates)-min(juliandates))/7)
JDweeklyintervals<-rep(0, times = no.weeks)

for (i in 1:no.weeks) {
  
  JDweeklyintervals[i]<-firstday+(7*i)
}

#Below converts julian to normal date. This is a useful bit of code to recycle... 
weeklyintervals<-as.Date(JDweeklyintervals, origin=as.Date("1970-01-01"))
#weekly intervals are given above to use for making weekly means. Now you can calculate means within those dates.

#may need to make the best2020 into julian date
best2020$j.date<-julian(best2020$date)

JDweeklyintervalsloops<-c(0, JDweeklyintervals)
best2020$weeklyintvl<-rep(0, each = length(best2020$date))
#using subsets to add data of appropriate date to the vectors
for (i in 1:(length(JDweeklyintervalsloops)-1)) {
  loopintvl<-subset(best2020, best2020$j.date > JDweeklyintervalsloops[i] & best2020$j.date <= JDweeklyintervalsloops[i+1])
  positionsforaddingtobest2020<-which(best2020$j.date > JDweeklyintervalsloops[i] & best2020$j.date <= JDweeklyintervalsloops[i+1])
  intvladd<-rep(JDweeklyintervalsloops[i+1], each = length(loopintvl$date))
  best2020$weeklyintvl[positionsforaddingtobest2020]<-intvladd
  
  
}


View(focusweeksitelice)
salmonlicebest2019<-subset(best2020, species == "chum" | species == "coho" | species == "chinook"| species == "sockeye" |species == "salmon")
weeksitelice<-data.frame(salmonlicebest2019$date, salmonlicebest2019$j.date, salmonlicebest2019$weeklyintvl ,salmonlicebest2019$groupedsites,  
                         salmonlicebest2019$copsum, salmonlicebest2019$chalsum, salmonlicebest2019$motsum, salmonlicebest2019$Sum_all_lice)
names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "groupedsites", "copsum", "chalsum", "motsum", "sum_all_lice"))
focusweeksitelice<-subset(weeksitelice, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay" | groupedsites == "North Meares")
focussitelist<- unique(focusweeksitelice$groupedsites)
View(focusweeksitelice)

rowcounts <- data.frame(weeklyintvl = numeric(0),
                     site = numeric(0),
                     mean = numeric(0),
                     lci = numeric(0),
                     uci = numeric(0))
rowcounts$site <- factor(rowcounts$site, levels=focussitelist)  

lci<- NULL
uci <- NULL
site.boot<-NULL
#put site.boot here
bigsiteboot<-rep(0, times =  length(focussitelist)*length(JDweeklyintervals))
weeklyintervalscol<- rep(JDweeklyintervals, times = length(focussitelist))
meanbootcol<-NULL

#for each site we have samples of 1000 for each interval which are all put into mean boot. to put into vector
for (i in 1 : length(focussitelist)){
  
  #this subsets by site
loopfocussite<-focussitelist[i]
loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))



for (j in 1:length(JDweeklyintervals)) {
  mean.boot<-NULL
    #for each interval 1000 iterations are done
  #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
  temp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
  
  #create the vessel for the 1000 samples which will renew for every weekly interval
  temp.sample <- NULL

  
  for(k in 1:n.boot.b) {  
  # for each of the 1000 iterations of boostrapping
  
    # Object to store resampled data for this iteration of bootstrapping
  temp.sample <- c(temp.sample, sample(x=temp$motsum, size=length(temp$motsum), replace=T)) 
mean.boot <- c(mean.boot, mean(temp.sample))
  }   
  
  #I have 1000 means of 1000 samples to get the 25 and 976 percentiles from. And the mean of the means.
mean.boot <- sort(mean.boot)
#getting lci and uci and then out of the loop
lcisamp<-mean.boot[250]
ucisamp<-mean.boot[976]
lci<-c(lci, lcisamp)
uci<-c(uci, ucisamp)
#Getting mean.boot output out of loop
meanbootcol<- c(meanbootcol, mean(mean.boot))
  }
#vector for the means of the weekly intervals
#gives the site for each of the mean boot intervals

#siteassign <-rep(loopfocussite, times = length(mean.boot))
#site.boot<- c(site.boot, siteassign)

}


JDweeklyintervalsfin<-rep(JDweeklyintervals, times = length(focussitelist))
ntable<-length(JDweeklyintervals)*length(focussitelist)
for (i in 1:ntable) {
  rowcounts[(i),1] <- c(JDweeklyintervalsfin[i])
  rowcounts[(i),3:5] <- c(meanbootcol[i], lci[i], uci[i])
}

rowcounts[2]<-rep(focussitelist, each = length(JDweeklyintervals))

motmeanboot<-rowcounts

assignstage<-rep("motile", each = length(motmeanboot$weeklyintvl))
motmeanboot<-cbind(assignstage, motmeanboot)
View(motmeanboot)
###


salmonlicebest2019<-subset(best2020, species == "chum" | species == "coho" | species == "chinook"| species == "sockeye" |species == "salmon")
weeksitelice<-data.frame(salmonlicebest2019$date, salmonlicebest2019$j.date, salmonlicebest2019$weeklyintvl ,salmonlicebest2019$groupedsites,  
                         salmonlicebest2019$copsum, salmonlicebest2019$chalsum, salmonlicebest2019$motsum, salmonlicebest2019$Sum_all_lice)
names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "groupedsites", "copsum", "chalsum", "motsum", "sum_all_lice"))
focusweeksitelice<-subset(weeksitelice, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay" | groupedsites == "North Meares")
focussitelist<- unique(focusweeksitelice$groupedsites)

rowcounts <- data.frame(weeklyintvl = numeric(0),
                        site = numeric(0),
                        mean = numeric(0),
                        lci = numeric(0),
                        uci = numeric(0))
rowcounts$site <- factor(rowcounts$site, levels=focussitelist)  

lci<- NULL
uci <- NULL
site.boot<-NULL
#put site.boot here
bigsiteboot<-rep(0, times =  length(focussitelist)*length(JDweeklyintervals))
weeklyintervalscol<- rep(JDweeklyintervals, times = length(focussitelist))
meanbootcol<-NULL

#for each site we have samples of 1000 for each interval which are all put into mean boot. to put into vector
for (i in 1 : length(focussitelist)){
  
  #this subsets by site
  loopfocussite<-focussitelist[i]
  loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))
  
  
  
  
  for (j in 1:length(JDweeklyintervals)) {
    mean.boot<-NULL
    #for each interval 1000 iterations are done
    #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
    temp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
    
    #create the vessel for the 1000 samples which will renew for every weekly interval
    temp.sample <- NULL
    
    
    for(k in 1:n.boot.b) {  
      # for each of the 1000 iterations of boostrapping
      
      # Object to store resampled data for this iteration of bootstrapping
      temp.sample <- c(temp.sample, sample(x=temp$sum_all_lice, size=length(temp$sum_all_lice), replace=T)) 
      mean.boot <- c(mean.boot, mean(temp.sample))
    }   
    
    #I have 1000 means of 1000 samples to get the 25 and 976 percentiles from. And the mean of the means.
    mean.boot <- sort(mean.boot)
    #getting lci and uci and then out of the loop
    lcisamp<-mean.boot[250]
    ucisamp<-mean.boot[976]
    lci<-c(lci, lcisamp)
    uci<-c(uci, ucisamp)
    #Getting mean.boot output out of loop
    meanbootcol<- c(meanbootcol, mean(mean.boot))
  }
  #vector for the means of the weekly intervals
  #gives the site for each of the mean boot intervals
  
  #siteassign <-rep(loopfocussite, times = length(mean.boot))
  #site.boot<- c(site.boot, siteassign)
  
}


JDweeklyintervalsfin<-rep(JDweeklyintervals, times = length(focussitelist))
ntable<-length(JDweeklyintervals)*length(focussitelist)
for (i in 1:ntable) {
  rowcounts[(i),1] <- c(JDweeklyintervalsfin[i])
  rowcounts[(i),3:5] <- c(meanbootcol[i], lci[i], uci[i])
}

rowcounts[2]<-rep(focussitelist, each = length(JDweeklyintervals))


sumalllicemeanboot<-rowcounts

assignstage<-rep("total", each = length(sumalllicemeanboot$weeklyintvl))
sumalllicemeanboot<-cbind(assignstage, sumalllicemeanboot)

###############################################
salmonlicebest2019<-subset(best2020, species == "chum" | species == "coho" | species == "chinook"| species == "sockeye" |species == "salmon")
weeksitelice<-data.frame(salmonlicebest2019$date, salmonlicebest2019$j.date, salmonlicebest2019$weeklyintvl ,salmonlicebest2019$groupedsites,  
                         salmonlicebest2019$copsum, salmonlicebest2019$chalsum, salmonlicebest2019$motsum, salmonlicebest2019$Sum_all_lice)
names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "groupedsites", "copsum", "chalsum", "motsum", "sum_all_lice"))
focusweeksitelice<-subset(weeksitelice, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay" | groupedsites == "North Meares")
focussitelist<- unique(focusweeksitelice$groupedsites)

rowcounts <- data.frame(weeklyintvl = numeric(0),
                        site = numeric(0),
                        mean = numeric(0),
                        lci = numeric(0),
                        uci = numeric(0))
rowcounts$site <- factor(rowcounts$site, levels=focussitelist)  

lci<- NULL
uci <- NULL
site.boot<-NULL
#put site.boot here
bigsiteboot<-rep(0, times =  length(focussitelist)*length(JDweeklyintervals))
weeklyintervalscol<- rep(JDweeklyintervals, times = length(focussitelist))
meanbootcol<-NULL

#for each site we have samples of 1000 for each interval which are all put into mean boot. to put into vector
for (i in 1 : length(focussitelist)){
  
  #this subsets by site
  loopfocussite<-focussitelist[i]
  loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))
  
  
  
  
  for (j in 1:length(JDweeklyintervals)) {
    mean.boot<-NULL
    #for each interval 1000 iterations are done
    #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
    temp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
    
    #create the vessel for the 1000 samples which will renew for every weekly interval
    temp.sample <- NULL
    
    
    for(k in 1:n.boot.b) {  
      # for each of the 1000 iterations of boostrapping
      
      # Object to store resampled data for this iteration of bootstrapping
      temp.sample <- c(temp.sample, sample(x=temp$chalsum, size=length(temp$chalsum), replace=T)) 
      mean.boot <- c(mean.boot, mean(temp.sample))
    }   
    
    #I have 1000 means of 1000 samples to get the 25 and 976 percentiles from. And the mean of the means.
    mean.boot <- sort(mean.boot)
    #getting lci and uci and then out of the loop
    lcisamp<-mean.boot[250]
    ucisamp<-mean.boot[976]
    lci<-c(lci, lcisamp)
    uci<-c(uci, ucisamp)
    #Getting mean.boot output out of loop
    meanbootcol<- c(meanbootcol, mean(mean.boot))
  }
  #vector for the means of the weekly intervals
  #gives the site for each of the mean boot intervals
  
  #siteassign <-rep(loopfocussite, times = length(mean.boot))
  #site.boot<- c(site.boot, siteassign)
  
}


JDweeklyintervalsfin<-rep(JDweeklyintervals, times = length(focussitelist))
ntable<-length(JDweeklyintervals)*length(focussitelist)
for (i in 1:ntable) {
  rowcounts[(i),1] <- c(JDweeklyintervalsfin[i])
  rowcounts[(i),3:5] <- c(meanbootcol[i], lci[i], uci[i])
}

rowcounts[2]<-rep(focussitelist, each = length(JDweeklyintervals))


chalmeanboot<-rowcounts

assignstage<-rep("chalimus", each = length(chalmeanboot$weeklyintvl))
chalmeanboot<-cbind(assignstage, chalmeanboot)


##############################################################




salmonlicebest2019<-subset(best2020, species == "chum" | species == "coho" | species == "chinook"| species == "sockeye" |species == "salmon")
weeksitelice<-data.frame(salmonlicebest2019$date, salmonlicebest2019$j.date, salmonlicebest2019$weeklyintvl ,salmonlicebest2019$groupedsites,  
                         salmonlicebest2019$copsum, salmonlicebest2019$chalsum, salmonlicebest2019$motsum, salmonlicebest2019$Sum_all_lice)
names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "groupedsites", "copsum", "chalsum", "motsum", "sum_all_lice"))
focusweeksitelice<-subset(weeksitelice, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay" | groupedsites == "North Meares")
focussitelist<- unique(focusweeksitelice$groupedsites)

rowcounts <- data.frame(weeklyintvl = numeric(0),
                        site = numeric(0),
                        mean = numeric(0),
                        lci = numeric(0),
                        uci = numeric(0))
rowcounts$site <- factor(rowcounts$site, levels=focussitelist)  

lci<- NULL
uci <- NULL
site.boot<-NULL
#put site.boot here
bigsiteboot<-rep(0, times =  length(focussitelist)*length(JDweeklyintervals))
weeklyintervalscol<- rep(JDweeklyintervals, times = length(focussitelist))
meanbootcol<-NULL

#for each site we have samples of 1000 for each interval which are all put into mean boot. to put into vector
for (i in 1 : length(focussitelist)){
  
  #this subsets by site
  loopfocussite<-focussitelist[i]
  loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))
  
  
  
  
  for (j in 1:length(JDweeklyintervals)) {
    mean.boot<-NULL
    #for each interval 1000 iterations are done
    #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
    temp<-subset(best2020, weeklyintvl == JDweeklyintervals[j])
    
    #create the vessel for the 1000 samples which will renew for every weekly interval
    temp.sample <- NULL
    
    unique(best)
    for(k in 1:n.boot.b) {  
      # for each of the 1000 iterations of boostrapping
      
      # Object to store resampled data for this iteration of bootstrapping
      temp.sample <- c(temp.sample, sample(x=temp$copsum, size=length(temp$copsum), replace=T)) 
      mean.boot <- c(mean.boot, mean(temp.sample))
    }   
    
    #I have 1000 means of 1000 samples to get the 25 and 976 percentiles from. And the mean of the means.
    mean.boot <- sort(mean.boot)
    #getting lci and uci and then out of the loop
    lcisamp<-mean.boot[250]
    ucisamp<-mean.boot[976]
    lci<-c(lci, lcisamp)
    uci<-c(uci, ucisamp)
    #Getting mean.boot output out of loop
    meanbootcol<- c(meanbootcol, mean(mean.boot))
  }
  #vector for the means of the weekly intervals
  #gives the site for each of the mean boot intervals
  
  #siteassign <-rep(loopfocussite, times = length(mean.boot))
  #site.boot<- c(site.boot, siteassign)
  
}


JDweeklyintervalsfin<-rep(JDweeklyintervals, times = length(focussitelist))
ntable<-length(JDweeklyintervals)*length(focussitelist)
for (i in 1:ntable) {
  rowcounts[(i),1] <- c(JDweeklyintervalsfin[i])
  rowcounts[(i),3:5] <- c(meanbootcol[i], lci[i], uci[i])
}

rowcounts[2]<-rep(focussitelist, each = length(JDweeklyintervals))


copsmeanboot<-rowcounts

assignstage<-rep("copepodid", each = length(copsmeanboot$weeklyintvl))
copsmeanboot<-cbind(assignstage, copsmeanboot)








weeklyliceloctable<-rbind(sumalllicemeanboot, motmeanboot, chalmeanboot, copsmeanboot)
view(weeklyliceloctable)
###making JDate into weekly again
weeklyliceloctable$weeklyintvl<-as.Date(weeklyliceloctable$weeklyintvl, origin = as.Date("1970-01-01"))
###


  # sample data with replacement and store
  #need to put this into a table that has the site name and the date and the means for all lice. 

View(weeklyliceloctable)

weeklyliceloctable$weeklyintvl<-as.Date(weeklyliceloctable$weeklyintvl, origin = as.Date("1970-01-01"))
weeklyliceloctable$weeklyintvl<-format(weeklyliceloctable$weeklyintvl, format = "%b %d %y")

#to get proper table .... Need to subset by site, then assigned stage. (same dates)
# then cbind the mean, lci, uci to the thing. Paste the new names.
NorthMearesweeklies<-subset(weeklyliceloctable, site == "North Meares")
Cypreweeklies<-subset(weeklyliceloctable, site == "Cypre River")
Ritchieweeklies<-subset(weeklyliceloctable, site == "Ritchie Bay")
View(Ritchieweeklies)

NorthMearesweeklies <- na.omit(NorthMearesweeklies)
Cypreweeklies <- na.omit(Cypreweeklies)
Ritchieweeklies <- na.omit(Ritchieweeklies)

colnames(Ritchieweeklies)
#ritchie #ggplot attempt
Ritchieweeklies <- subset(Ritchieweeklies,select= -c(site))

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "assignstage")] <- "Stage"

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "weeklyintvl")] <- "Week"

colnames(Ritchieweeklies)[which(names(Ritchieweeklies) == "site")] <- "Site"

#reshape
unique(Ritchieweeklies$Week)
library(reshape2)
Ritchieweeklies$Week <- factor(Ritchieweeklies$Week, c("Apr 17 20","Apr 24 20","May 01 20","May 08 20", "May 15 20","May 29 20","Jun 05 20","Jun 12 20","Jun 19 20","Jun 26 20","Jul 10 20"))
unique(Ritchieweeklies$Stage)
Ritchieweeklies$Stage <- factor(Ritchieweeklies$Stage, c("total","chalimus","copepodid","motile"))

?colour
head(Ritchieweeklies)
ggplot(Ritchieweeklies, aes(Week, mean, fill= Stage)) + 
  geom_bar(stat = 'identity', position = 'dodge')+ labs(x = "Week", y = "Mean Abundance") + 
  theme_classic()+
  scale_fill_brewer(palette="Greys")+
  geom_errorbar(data = Ritchieweeklies,aes(ymin=lci, ymax=uci), position=position_dodge(.9), width=0.1)+
  theme(axis.text=element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))


Ritchiedates <- subset(best2020, groupedsites == "Ritchie Bay")
unique(Ritchiedates$date) 
