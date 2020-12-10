#Title: Sea Lice Monitoring Outputs
#Author : Rowen Monks
#Last updated : August 28, 2019
#Description : Makes mean prevalence, abundance, TS and forklength plots for 
#Sea Lice Monitoring Program 
#at Cedar Coast Field Station. Plots are made for all available locations, or for Bedwell Estuary
#(Bedwell North/Bedwell River), Cypre River, and Ritchie Bay
#Based on sea lice monitoring data of Cedar Coast Field Station

#NOTES: 
#Notes for Rowen: 
#Hi! Thanks for taking a look at this! The sections I am having trouble with can
#be found be searchig "#X#". There is a description of the issue I am facing 
#with all of them. First, do get yourself setup, follow the instructions below!

#This is a mostly automated code. That means that if you have the data and the code, you should be able to run everything and have a solid output - in theory. To start, make a new project and set your working directory. Make sure you set your working directory to a specified folder. In the folder you should make a new folder called /Data, where you put the dataset required for this project: the sea lice data, forplots2020, and the site data, clayoquot.site.data. Make sure also that the code is in the main folder where you set the wd. Now, you should be able to run the code and all the output data and figures should save to specified folders created from here on out.

#To use this code, make a .csv file of your new datafile, and ensure it matches the format of the original data file.
#Prevalence = number of infected fish / total number of fish
# The purse seined fish were put into a different sheet on the Best.Clayoquot.Sealice.data.2019.xlsx file. ie. not
# included in the following analysis.

# This code has been made so that it only has to be edited slightly to produce 
#plots/tables on different data.
# I tried to indicate where any edits should be made
# Please look for ##******************* which indicates that there is something
# for you to add/edit in the code (ex. "groupedsites name",)

############ All data/plots are currently based on 2020 data. 

###############################################################
# Can search with ctrl F for main headings or their subheadings :
# SET UP  
    # JULIAN DATES

# CALCULATING
#weekly intervals : ##ASSIGNING WEEKLY INTERVALS TO ALL THE dates in the best2020 data set 
# PLOTTING
  # PREVALENCE Aa : Prevalence with stages, PREVALENCE WITH STAGES plots for each site,
  # PREVALENCE Ab : Prevalence for stages, for the main ccfs sites (cypre, bedwell estuary and ritchie), 
  #                 with standard deviation
  # ABUNDANCE : lice stages A (gives columns for lice stages and lice species), #Daily mean lice with stages, #MAKING WEEKLY LICE TABLES
  # MEAN ABUNDANCE Aa : Produces plots for each target site. Not a for loops so if you want to include a site, you must
  # copy and paste the code that was already done and edit the site used.
  # FORKLENGTH
  # TS PLOTS: you must change a bit of the code to change which site you produce TS plots/tables for.
  #           To find the code that you edit search #$%^&

# To change which year you want to look at replace all 2019 with your desired yyyy year. It must match
# the year format in your main database.

#%$ - used for finding a bookmark

#X# Represents a current break in code that needs to be worked on

##############################################################

##SET UP##
#first make sure R and R studio are up to date
#Update R
#install.packages("installr")
#library(installr)

#updateR()
#update r studio
#From within RStudio, go to Help > Check for Updates to install newer version of RStudio (if available, optional).
#************** change to your own directory

#--------------make project folders and folder paths----------------------------
#set your wd here, MAKE SURE ITS SET TO YOUR PROJECT DATA BASE IN SESSION DROPDOWN MENU ABOVE
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


#count total fish using length function
best2020$countcol<-rep(1, length(best2020$fish_id))

abstotalfish<-sum(best2020$countcol)


speclist <- unique(best2020$species)

#count breakdown by species
#RM : I don't know what this is for but here is the proper code


countbyspecies <- aggregate(countcol ~ species, best2020, sum)

#x#
#this is returning the count for chinook but not any of the others -> hold onto for Rowen

#determining instances by date a.k.a how many fish were caught each sampling day
#1 or more
nrow(best2020[best2020$Sum_all_lice > 60, ])
#total amount of lice for > 1 group
sum(subset(best2020, Sum_all_lice > 1)$Sum_all_lice)

#3 or more 
# RM : if you want 3 or more, use the >= I have added it below
nrow(best2020[best2020$Sum_all_lice >= 3, ])
sum(subset(best2020, Sum_all_lice >= 3)$Sum_all_lice)

#species breakdown
#make a loop for this
#coho

#RM : for loop :
sp.breakdown <- data.frame(coho = numeric(0),
           chum = numeric(0),
           chinook = numeric(0),
           pink = numeric(0),
           sockeye = numeric(0))

for (i in speclist) {
  sp.breakdown[1,i] <- sum(subset(best2020, species == i)$Sum_all_lice)/nrow(best2020[best2020$species == i, ])
}

##########

#must use countcol for counting total fish because fish_ID is given to species that aren't included in analysis.
#mean lice = sum of sum of lice / abstotal

#Below gives columns of summed motiles, attached, copepodids, chalimus, and total counts. Useful for prevalence and abundance plots.
best2020$motsum<-rowSums(motlice, na.rm = TRUE)
best2020$copsum<-rowSums(copes, na.rm = TRUE)
best2020$chalsum<-rowSums(chals, na.rm = TRUE)
best2020$attachedsum<-rowSums(attlice, na.rm = TRUE)
best2020$Sum_all_lice<-rowSums(salmcounts, na.rm = T)

#Last line in this chunk assembles the stages-tables to give the SUM of all lice stages by groupedsites

Motlicetab<-aggregate(motsum~groupedsites, data = best2020, sum)
Attlicetab<-aggregate(attachedsum~groupedsites, data = best2020, sum)
Coplicetab<-aggregate(copsum~groupedsites, data = best2020, sum)
Challicetab<-aggregate(chalsum~groupedsites, data = best2020, sum)
alltab<-aggregate(Sum_all_lice~groupedsites, data = best2020, sum)
# This is the final table for plots of sums! :)))
licetable<-data.frame(Motlicetab, Coplicetab[2], Challicetab[2], alltab[2], Attlicetab[2])

# This is a table for means by site and date

Motlicetab.mean.site.date<-aggregate(motsum~groupedsites + date, data = best2020, mean)
Attlicetab.mean.site.date<-aggregate(attachedsum~groupedsites + date, data = best2020, mean)
Coplicetab.mean.site.date<-aggregate(copsum~groupedsites+ date, data = best2020, mean)
Challicetab.mean.site.date<-aggregate(chalsum~groupedsites+ date, data = best2020, mean)
alltab.mean.site.date<-aggregate(Sum_all_lice~groupedsites+ date, data = best2020, mean)
# This is the final table for plots of means! :)))
licetable.mean.site.date<-data.frame(Motlicetab.mean.site.date, Coplicetab.mean.site.date[3], Challicetab.mean.site.date[3], alltab.mean.site.date[3], Attlicetab.mean.site.date[3])

#dir.out <- "C:/Users/Rowen/OneDrive/Desktop/github/CCFS-Sea-Lice-database/Data/OutputData"

#RM : still can't get directories to work :)
#setwd(dir.out)
#write.csv(licetable.mean.site.date, paste(yr, "mean_lice_by_site_date.csv", sep = "_"))





#view to make sense
#view(licetable)
#make sure to check that sums make sense

#Last line in this chunk assembles the stages-tables to give the MEAN of all lice stages by groupedsites
mMotlicetab<-aggregate(motsum~groupedsites, data = best2020,mean)
mAttlicetab<-aggregate(attachedsum~groupedsites, data = best2020, mean)
mCoplicetab<-aggregate(copsum~groupedsites, data = best2020, mean)
mChallicetab<-aggregate(chalsum~groupedsites, data = best2020, mean)
malltab<-aggregate(Sum_all_lice~groupedsites, data = best2020, mean)
meanlicetable<-data.frame(mMotlicetab, mAttlicetab[2], mCoplicetab[2], mChallicetab[2], malltab[2])
meanlicetable<-meanlicetable[order(meanlicetable$groupedsites),]
secols<-data.frame(motsum = numeric(0), attsum = numeric(0), copesum = numeric(0), chalsum = numeric(0), allsum = numeric(0))
for (i in 1:length(meanlicetable$groupedsites)) {
 
  semeans.site.temp<-subset(best2020, groupedsites == meanlicetable$groupedsites[i])
  secols[i,1]<-sd(semeans.site.temp$motsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,2]<-sd(semeans.site.temp$attachedsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,3]<-sd(semeans.site.temp$copsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,4]<-sd(semeans.site.temp$chalsum)/sqrt(length(semeans.site.temp$motsum))
  secols[i,5]<-sd(semeans.site.temp$Sum_all_lice)/sqrt(length(semeans.site.temp$motsum))
}

names(secols)<-paste(c("SE.motile", "SE.attached", "SE.cops", "SE.chals", "SE.all"))
#line of code below shows error " Error in data.frame(..., check.names = FALSE) : 
    #arguments imply differing number of rows: 5, 0
meanlicetablewithtotalse<-(cbind(meanlicetable, secols))
view(meanlicetablewithtotalse)
# This is the final table for plots of means! :)))
#liceofmeanlicetable<-data.frame(cbind(meanlicetable$motsum, meanlicetable$chalsum, meanlicetable$copsum))
allmeanlice <- data.frame(cbind(meanlicetable$motsum, meanlicetable$chalsum, meanlicetable$copsum, deparse.level = 1))
              #names(allmeanlice[names(allmeanlice)== "X1"]) <- "motsum"
              #names(allmeanlice[names(allmeanlice)== "X2"]) <- "chalsum"
              #names(allmeanlice[names(allmeanlice)== "X3"]) <- "copsum"
#check order of names from grouped sites in best 2020, copy that orientation below
unique(best2020$groupedsites)
colnames(allmeanlice)<- c("motsum", "chalsum", "copsum")
rownames(allmeanlice) <- c("Bedwell Sound North", "North Meares","Cypre River", "Ritchie Bay", "Tsapee Narrows")

#$% issue with Total showing up....
#making a shareable table of the lice means
getwd()

#save above table to csv with specified path = wd/data.output.path
#automated version
write.csv(meanlicetablewithtotalse,file.path(data.output.path,"meanlicetable.bysite.csv"))


write.csv(licetable,file.path(data.output.path,"totalsumslicetable.csv"))

#Interlude for ~PLOTTING~

#BARPLOT OF MEAN LICE BY LOCATION
#$% need to put updated plot here for mean lice 
#**********************
#Will have to change the vector for names.arg in the barplot if the sites names change.
#ok not sure what is going on here but this is not working. error: Error in -0.01 * height : non-numeric argument to binary operator
#OLD 2019
#licesitenameedit<-c("Buckle Bay", "Cypre River", "Elbow Bank",
 #                   "Keltsmaht", "Lone Cone Light", "Moyeha", #"Ritchie Bay", "Tranquil Est", 
 #                   "Tsapee Narrows","Bedwell Sound S", "Bedwell Est N", "Bedwell Middle"  )

#2020 Update, change name in names.arg
licesitenameedit <- as.list(print(meanlicetablewithtotalse[,1]))
licecol<-c("dodgerblue","red","darkgreen","darkgray")
#check if site names match
meanlicetablewithtotalse$groupedsites
meanlicetablewithtotalse <- na.omit(meanlicetablewithtotalse)
meanlicetablewithtotalse[,2:6] <- as.matrix(sapply(meanlicetablewithtotalse[,2:ncol(meanlicetablewithtotalse)], as.numeric))
view(meanlicetablewithtotalse)
#plot area 
#x#This plot requires code from farther down to be initiated properly, will not run
#w/o objects futher down
par(mar=c(10,5,4,2))
#barplot(t(meanlicetablewithtotalse, col = c("dodgerblue","red","darkgreen"), border="white", 
#        font.axis = 2, beside=T, legend=c(), font.lab=2, ylim = c(0,ceiling(max(meanlicetablewithtotalse))),
#        ylab = "Mean Lice per Fish",main = "Daily Mean Lice - Clayoquot Salmon 2020", names.arg = licesitenameedit, las = 2))

#Below works to create a barplot. - RM
# The names argument changed so it didn't match with the mean lice table
# The colours were standardized using licecol (moved above)
# The barplot gives motile, copepodid, chalimus and all stages of lice. 
barplot(t(as.matrix(meanlicetablewithtotalse[,c(2,4,5,6)])), names = meanlicetablewithtotalse[,1], col = licecol ,border="white", 
          font.axis = 2, beside=T, font.lab=2, ylim = c(0,ceiling(max(meanlicetablewithtotalse$Sum_all_lice))),
          ylab = "Mean Lice per Fish",main = "Daily Mean Lice - Clayoquot Salmon 2020", names.arg = licesitenameedit, las = 2)

#abline(h= seq(0, ceiling(max(liceofmeanlicetable)), 1), col = "light gray")

legend("topright", cex=0.6, legend = c("Motile", "Copepodid", "Chalimus", "All Stages"), col = licecol, title = "Lice Stage", lty = 1, lwd = 4)

#legend("topright", cex=0.6, legend = c("Motile", "Chalimus", "Copepodid"), col = c("dodgerblue","red","darkgreen"), title = "Lice Stage", lty = 1, lwd = 4)
#licestagelegend<-legend("topright", cex=0.6, legend = c("Total Lice", "Motile", "Chalimus", "Copepodid"), col = licecol, title = "Lice Stage", lty = 1, lwd = 4)


# Resuming ~VECTOR CREATION~

##JULIAN DATES

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

#the means should be made from dates <[i] and >min(juliandates)

#now aggregate a table with required info
datemot<-aggregate(motsum~date, data = best2020, sum)
dateatt<-aggregate(attachedsum~date, data = best2020, sum)
datetot<-aggregate(Sum_all_lice~date, data = best2020, sum)
datecop<-aggregate(copsum~date, data = best2020, sum)
datechal<-aggregate(chalsum~date, data = best2020, sum)
datetable<-data.frame(datetot, datemot[2], dateatt[2], datecop[2], datechal[2])

meandatetable1<-datetable[,-1]/abstotalfish
meandatetable<-cbind(datetable[,1], meandatetable1)
names(meandatetable)[1]<-paste("date")
#this is the mean lice per fish for South Clayoquot Sound (all sample sites). 
dates<-as.Date(meandatetable$date)
dates<-format(dates,)
          #### this goes in the line above        #format = "%b %d", levels= c("Apr", "May", "Jun", "Jul")

#this can be condensed.
names(meandatetable)[2]<-paste("mean.total.lice")
names(meandatetable)[3]<-paste("mean.motile")
names(meandatetable)[4]<-paste("mean.attached")
names(meandatetable)[5]<-paste("mean.copepodid")
names(meandatetable)[6]<-paste("mean.chalimus")
#dates <- as.Date(meandatetable$date, "%b %d %y")

# Daily mean lice per fish for all of clayoquot
#need to make the x axis weekly intervals. Need means for the weekly intervals tooo. So I
#think I need to redo the meandatetable with new mean.total.lice and then need the weekly intervals.
barplot(meandatetable$mean.total.lice~dates, main = "Mean Lice Per Fish 2020", ylab = "Mean Lice per Fish", xlab = "", ylim = c(0,1))
        
for (i in 1:length(meandatetable$date)) {
  dateflloop<-(subset(best2020, best2020$date == meandatetable$date[i]))  
  meandatetable$meanfl[i]<-mean(dateflloop$length)
  
}

#coho forklength
for (i in 1:length(meandatetable$date)) {
  dateflloopcoho<-(subset(best2020, best2020$date == meandatetable$date[i] & best2020$species == "coho"))  
  meandatetable$meanflcoho[i]<-mean(dateflloopcoho$length)
  
}

#chum forklength
for (i in 1:length(meandatetable$date)) {
  dateflloopchum<-(subset(best2020, best2020$date == meandatetable$date[i] & best2020$species == "chum"))  
  meandatetable$meanflchum[i]<-mean(dateflloopchum$length)
  
}

#chinook forklength
for (i in 1:length(meandatetable$date)) {
  dateflloopchin<-(subset(best2020, best2020$date == meandatetable$date[i] & best2020$species == "chinook"))  
  meandatetable$meanflchinook[i]<-mean(dateflloopchin$length)
  
}

#too few sockeye to include them on the graphs.
for (i in 1:length(meandatetable$date)) {
  dateflloopsox<-(subset(best2020, best2020$date == meandatetable$date[i] & best2020$species == "sockeye"))  
  meandatetable$meanflsockeye[i]<-mean(dateflloopsox$length)
  
}

notmissingchin<-!is.na(meandatetable$meanflchinook)
presentmeanflchinook<-meandatetable[notmissingchin,]
notmissingcoho<-!is.na(meandatetable$meanflcoho)
presentmeanflcoho<-meandatetable[notmissingcoho,]
notmissingchum<-!is.na(meandatetable$meanflchum)
presentmeanflchum<-meandatetable[notmissingchum,]

#table of mean forklength and mean lice numbers

write.csv(meandatetable,file.path(data.output.path,"mean.lice.and.forklength.by.date.2020.csv"))

#daily forklength for all species.
#begin with setting the path for the figure to be saved to: figures.path
jpeg(filename = "OutputFigures/DailyForkLength2020.jpg")
#note, if you are trying to adjust aesthetics, you will have take off the above save code to see the plots. When jpeg is open, you won't see you plot untill you use dev.off to save it to the specified location.
# Can I recommend putting this at the bottom of the plot so you can see it being created before the file is made? - RM

#begin plot, set ranges by looking at max/ min values from mean table
yrangefl<-0:95
xrangefl<-meandatetable$date
par(mar = c(5,5,5,2), xpd = TRUE)
plot(meandatetable$meanfl~meandatetable$date, cex.lab = 1, pch = 19 , cex.axis = 1.4,ylab = "Mean Forklength (mm)", xlab = "Months",  main = "Daily Forklength of Clayoquot Salmon, 2020", ylim=c(40,95), type = "n")

#if you want lines for all species, use code below
#lines(meandatetable$meanfl~meandatetable$date, lwd = 2, lty = 2)


#if you want broken lines for Coho, use code below
#broken lines: lines(meandatetable$meanflcoho~meandatetable$date, na.pass=TRUE, lwd = 2)

#Lines to put on to one plot

#COHO
points(presentmeanflcoho$meanflcoho~presentmeanflcoho$date, col = "black", pch = 19)
lines(presentmeanflcoho$meanflcoho~presentmeanflcoho$date, lwd = 2, lty = 3, col = "black")
#abline(lm(presentmeanflcoho$meanflcoho~presentmeanflcoho$date, na.pass=TRUE, lwd = 2, lty = 1, col = "darkgray"))

#CHUM
points(presentmeanflchum$meanflchum~presentmeanflchum$date,pch = 19, col = "dodgerblue")
lines(presentmeanflchum$meanflchum~presentmeanflchum$date, na.pass=TRUE, lwd = 2, col = "dodgerblue", lty = 3)

#CHINOOK
points(presentmeanflchinook$meanflchinook~presentmeanflchinook$date, pch = 19, col = "red")
lines(presentmeanflchinook$meanflchinook~presentmeanflchinook$date, lwd = 2, col = "red", lty = 3)

#run this before making legend because defines the species being looked at
listspeciesinterest<-c("Chum", "Coho", "Chinook")

#Legend
legend("topright", legend = listspeciesinterest, col = c("dodgerblue", "black","red"), cex = 1, lwd = 1, title = "Species", lty = c(1,2,3))

#save current plot pane as jpeg, trying to automate this but it may require ggplot, so we'll hold off for now. use def.off at the end of all plots to save to figures path
# RM : if you want to see the plot as it is being created : jpeg(filename = "OutputFigures/DailyForkLength2020.jpg")
dev.off()

#May want to put 2018 on there too in diff colour. 

#One plot for each species
jpeg(filename = "OutputFigures/DailyForkLengthChum.jpg")

plot(presentmeanflchum$meanflchum~presentmeanflchum$date, ylab = "Mean Forklength (mm)", xlab = "", main = "Chum Mean Forklength, 2020", cex.lab = 1.5 , cex.axis = 1.4)
lines(presentmeanflchum$meanflchum~presentmeanflchum$date, na.pass=TRUE, lwd = 2, col = "dodgerblue", type = "p", pch = 1)
abline(lm(presentmeanflchum$meanflchum~presentmeanflchum$date, na.pass=TRUE, lwd = 2), col = "dodgerblue")
dev.off()

jpeg(filename = "OutputFigures/DailyForkLengthCoho.jpg")
plot(presentmeanflcoho$meanflcoho~presentmeanflcoho$date, ylab = "Mean Forklength (mm)", xlab = "", main = "Coho Mean Forklength, 2020", cex.lab = 1.5 , cex.axis = 1.4)
lines(presentmeanflcoho$meanflcoho~presentmeanflcoho$date, na.pass=TRUE, lwd = 2, lty = 1, col = "darkgray", type = "p", pch = 2)
abline(lm(presentmeanflcoho$meanflcoho~presentmeanflcoho$date, na.pass=TRUE, lwd = 2), col = "lightgray")
dev.off()

jpeg(filename = "OutputFigures/DailyForkLengthChinook.jpg")



plot(presentmeanflchinook$meanflchinook~presentmeanflchinook$date, ylab = "Mean Forklength (mm)", xlab = "", main = "Chinook Mean Forklength, 2020", cex.lab = 1.5 , cex.axis = 1.4, type = "n")
lines(presentmeanflchinook$meanflchinook~presentmeanflchinook$date, na.pass=TRUE, lwd = 2, col = "red", na.rm = TRUE, lty = 2, type = "p", pch =3)
abline(lm(presentmeanflchinook$meanflchinook~presentmeanflchinook$date, na.pass=TRUE, lwd = 2), col = "red")

legend("topleft", legend = listspeciesinterest, col = c("dodgerblue", "lightgray","red"), cex = 0.7, lwd = 1, title = "Species", lty = c(1,1,1), pch = c(1, 2, 3))
dev.off()

## MAKING WEEKLY INTERVAL DATA FOR FORKLENGTHS########
#%$ 

#mean forklength in an interval

#********************
unique(best2020$species)

#Can change the list to include sockeye and/or pink.
# RM: Have you considered putting these kind of vectorised variables at the beginning? 
listspeciesinterest<-c("chum", "coho", "chinook")

meanflfish<-rep(0, times = length(weeklyintervals)*length(listspeciesinterest))
sdflfish<-rep(0, times = length(weeklyintervals)*length(listspeciesinterest))
speciesinterest<-rep(c("chum", "coho", "chinook"), each = length(weeklyintervals))

for (i in 1:length(listspeciesinterest)) {
  fishbest2020<-subset(best2020, species == listspeciesinterest[i])
  weeklyfl<-subset(fishbest2020, fishbest2020$j.date <= weeklyintervals[1])
  #**********************
  #if chum is not the first species in the list, then you need to tweek the code below.
  #chumfishbest2020<-subset(best2020, species == listspeciesinterest[1])
  #chumweeklyfl<-subset(fishbest2020, fishbest2020$j.date <= weeklyintervals[1])
  #meanflfish[1]<-mean(chumweeklyfl$length)
  meanflfish[(i*length(weeklyintervals))-(length(weeklyintervals)-1)]<-mean(weeklyfl$length)
  sdflfish[(i*length(weeklyintervals))-(length(weeklyintervals)-1)]<-sd(weeklyfl$length)
}


#weekly intervals of forklength
addonmeanflfish<-rep(0, times = ((length(weeklyintervals)*length(listspeciesinterest))-length(listspeciesinterest)+1))
addonsdflfish<-rep(0, times = ((length(weeklyintervals)*length(listspeciesinterest))-length(listspeciesinterest)+1))
speciesinterest<-rep(c("chum", "coho", "chinook"), times = length(weeklyintervals))
for (i in 1:length(listspeciesinterest)) {
  fishbest2020<-subset(best2020, species == listspeciesinterest[i])
  
  
  for (j in 2:(length(weeklyintervals))) {
    weeklyfl<-subset(fishbest2020, fishbest2020$j.date <= weeklyintervals[j] & fishbest2020$j.date > weeklyintervals[j-1])
    addonmeanflfish[j+((i-1)*(length(weeklyintervals)-1))]<-mean(weeklyfl$length)
    addonsdflfish[j+((i-1)*(length(weeklyintervals)-1))]<-sd(weeklyfl$length) 
  }}

#chopping up addonmeanflfish to add into the meanflfish
#**********************
# Can add a mean or SD weeklyfl vector for an additional species.
chumweeklyfl<-addonmeanflfish[2:(length(weeklyintervals))]
cohoweeklyfl<-addonmeanflfish[(length(weeklyintervals)+1):((length(weeklyintervals)*2)-1)]
chinookweeklyfl<-addonmeanflfish[((length(weeklyintervals)*2)):(((length(weeklyintervals))*2)+(length(weeklyintervals)-2))]
sdchumweeklyfl<-addonsdflfish[2:(length(weeklyintervals))]
sdcohoweeklyfl<-addonsdflfish[(length(weeklyintervals)+1):((length(weeklyintervals)*2)-1)]
sdchinookweeklyfl<-addonsdflfish[((length(weeklyintervals)*2)):(((length(weeklyintervals))*2)+(length(weeklyintervals)-2))]

#this part adds on the means for all other weekly intervals to the first weekly interval. This could likely 
# be condensed with if statement
meanflfish[2:(length(weeklyintervals))]<-chumweeklyfl
meanflfish[(2+length(weeklyintervals)):(length(weeklyintervals)*2)]<-cohoweeklyfl
meanflfish[((length(weeklyintervals)*2)+2):(length(weeklyintervals)*3)]<-chinookweeklyfl
sdflfish[2:(length(weeklyintervals))]<-sdchumweeklyfl
sdflfish[(2+length(weeklyintervals)):(length(weeklyintervals)*2)]<-sdcohoweeklyfl
sdflfish[((length(weeklyintervals)*2)+2):(length(weeklyintervals)*3)]<-sdchinookweeklyfl

meanflfish<-as.numeric(as.character(meanflfish))

meanflfish <- as.numeric(as.character(meanflfish))

######RUN THIS AFTER NEXT CHUNK OF CODE TO MAKE WORK, chunk starting with speciesintrest
# RM : I am going to put this line beneath the speciesinterest code.
#weeklyfl$meanflfish<-as.numeric(as.character(weeklyfl$meanflfish))

speciesinterest<-rep(c("chum", "coho", "chinook"), each = length(weeklyintervals))
weeklyfl<-data.frame(cbind(speciesinterest, meanflfish, sdflfish))
chumfl1<-subset(weeklyfl, speciesinterest == "chum")
chumfl<-cbind(chumfl1, weeklyintervals)
cohofl1<-subset(weeklyfl, speciesinterest == "coho")
cohofl<-cbind(cohofl1,weeklyintervals)
chinfl1<-subset(weeklyfl, speciesinterest == "chinook")
chinfl<-cbind(chinfl1,weeklyintervals)
weeklyfl$intervals<-rep(weeklyintervals, times = 3)
#names(weeklyfl)[2]<-paste("meanfl")

weeklyfl$meanflfish<-as.numeric(as.character(weeklyfl$meanflfish))


### Below, make chum into coho, or different species of interest, for lines of that species mean, weekly forklength 
# to plot lines for diffe species, use find and replace function, replace species nick names (chin = chinook) and then add a line to the plot

chinfl$weeklyintervals<-format(chinfl$weeklyintervals, format = "%b %d %y")
xrangefl<-chinfl$weeklyintervals



chinflnan<-which(chinfl$meanflfish == "NaN")
plotchinfl<-chinfl[-chinflnan,]
plotchinfl$meanflfish<-as.numeric(as.character(plotchinfl$meanflfish))
plotchinfl$weeklyintervals<-as.Date(plotchinfl$weeklyintervals, format = "%b %d %y")

cohofl$weeklyintervals<-format(cohofl$weeklyintervals, format = "%b %d %y")

xrangefl<-cohofl$weeklyintervals



cohoflnan<-which(cohofl$meanflfish == "NaN")
plotcohofl<-cohofl[-cohoflnan,]
plotcohofl$meanflfish<-as.numeric(as.character(plotcohofl$meanflfish))
plotcohofl$weeklyintervals<-as.Date(plotcohofl$weeklyintervals, format = "%b %d %y")

chumfl$weeklyintervals<-format(chumfl$weeklyintervals, format = "%b %d %y")
xrangefl<-chumfl$weeklyintervals



chumflnan<-which(chumfl$meanflfish == "NaN")
plotchumfl<-chumfl[-chumflnan,]
plotchumfl$meanflfish<-as.numeric(as.character(plotchumfl$meanflfish))
plotchumfl$weeklyintervals<-as.Date(plotchumfl$weeklyintervals, format = "%b %d %y")
#chinNan<-as.numeric(as.character(subset(chinfl, meanflfish == "NaN")))
#chinflmeanpresent<-chinfl[-chinNan,2]
#chinfl[chinflmeanpresent]

#set range for axes
yrangefl<-seq(30, 120, length.out = length(weeklyintervals))

#mean weekly fl for clayoquot

#begin plot base

#save area
jpeg(filename = "OutputFigures/WeeklyForkLength2020.jpg")

plot(yrangefl~weeklyintervals, cex.lab = 1.5 , cex.axis = 1.4, ylab = "Mean Forklength (mm)", type = "n", xlab = "", main = " Weekly Forklength of Clayoquot Salmon, 2020")

#chum
lines(plotchumfl$meanflfish~plotchumfl$weeklyintervals, lwd = 2, lty = 3)

#coho
lines(plotcohofl$meanflfish~plotcohofl$weeklyintervals, lwd = 2, lty = 3, col = "dodgerblue", na.pass = TRUE)

#chin
lines(plotchinfl$meanflfish~plotchinfl$weeklyintervals, lwd = 2, lty = 3, col = "red", na.pass = TRUE)


legend("topright", cex=1, legend = listspeciesinterest,
       lty= c(1,3,3), col = c("black", "dodgerblue", "red"), title = "Species", lwd = 2)

# save plot as WeeklyForklength2020

dev.off()

####################


#Mean lice per fish for each grouping of sites
#Following code gives you the columns in best2020 for sum of the stages of lice and species of lice
#lice stages A
salmcounts<-subset(best2020[,c(11:36)])
motlice<-best2020[,c("Caligus_mot", "Caligus_gravid", "Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale", "unid_PA", "unid_adult")]
attlice<-best2020[,c("Lep_cope","chalA","chalB","Caligus_cope","unid_cope","chal_unid")]
copes<-best2020[,c("Lep_cope", "Caligus_cope", "unid_cope")]
chals<-best2020[,c("chalA", "chalB", "chal_unid")]
callice<-best2020[,c("Caligus_mot", "Caligus_gravid")]
leplice<-best2020[,c("Lep_gravid", "Lep_nongravid", "Lep_male", "Lep_PAfemale", "Lep_PAmale")]
best2020$countcol<-rep(1, length(best2020$fish_id))
abstotalfish<-sum(best2020$countcol)

#There is likely a discrepancy between countcol sum and the max 
#for fish id, because fish id includes herring which was subsetted out of best2020. 

best2020$motsum<-rowSums(motlice, na.rm = TRUE)
best2020$attachedsum<-rowSums(attlice, na.rm = TRUE)
best2020$copsum<-rowSums(copes, na.rm = TRUE)
best2020$chalsum<-rowSums(chals, na.rm = TRUE)
best2020$lepmotsum<-rowSums(leplice, na.rm = TRUE)
best2020$calmotsum<-rowSums(callice, na.rm = TRUE)

#df1<-group_by(bestclay, year, bestclay$groupedsites, month, day)
#number of mot lice per groupedsites
#motlice1<-summarize(groupedsites, bestclay[,c(14:18, 20,21, 24, 25)] = n())
#summarise(df1, motilelice1 = colSums(bestclay[,c(14:18, 20,21, 24, 25)]))

#total lice by groupedsites
#Totallicetab<-aggregate(motsum + attachedsum~groupedsites, data = best2020, sum)
#motile lice and attached lice by groupedsites
Motlicetab<-aggregate(motsum~groupedsites, data = best2020, sum)
Attlicetab<-aggregate(attachedsum~groupedsites, data = best2020, sum)
Coplicetab<-aggregate(copsum~groupedsites, data = best2020, sum)
Challicetab<-aggregate(chalsum~groupedsites, data = best2020, sum)
Leplicetab<-aggregate(lepmotsum~groupedsites, data = best2020, sum)
Callicetab<-aggregate(calmotsum~groupedsites, data = best2020, sum)

# This is the final bar chart table! :)))
licetable<-data.frame(Motlicetab, Attlicetab[2], Coplicetab[2], Challicetab[2], Leplicetab[2], Callicetab[2])
View(licetable)
#barplot for total mean lice per location group

# want motsum and attached and Sum_all_lice, number of fish infected and number of total fish by date.
#can get motsum and attached and sum all lice by date

#now aggregate a table with required info
datemot<-aggregate(motsum~date, data = best2020, sum)
dateatt<-aggregate(attachedsum~date, data = best2020, sum)
datetot<-aggregate(Sum_all_lice~date, data = best2020, sum)
datecop<-aggregate(copsum~date, data = best2020, sum)
datechal<-aggregate(chalsum~date, data = best2020, sum)
datecalmot<-aggregate(calmotsum~date, data = best2020, sum)
datelepmot<-aggregate(lepmotsum~date, data = best2020, sum)
datetable<-data.frame(datetot, datemot[2], dateatt[2], datecop[2], datechal[2], datecalmot[2], datelepmot[2])

bestmeandatetable1<-datetable[,-1]/abstotalfish
bestmeandatetable<-cbind(datetable[,1], bestmeandatetable1)
names(bestmeandatetable)[1]<-paste("date")
#now we want prevalence for each date. So I need to create a column with number of fish caught on the date. 
#I can subset per date and then count the number of dates and then sum. Must go down to forloops section

##


##Prevalence Aa
###*********************
#copy and paste the line of code at the bottom of this segment. Then replace the name of the groupedsites with the new sample site.
#Syntax must match that of the data sheet uploaded
###This will give you number of fish at each groupedsites.

licetablesums<-licetable
#this was done so that the code didn't recount the sum columns
licetablesums$totalsum<-rowSums(licetable[-1], na.rm = TRUE)
totalfishwithlice<-length(which(best2020$Sum_all_lice >0))

n <-length(licetable$groupedsites)
infected.fish<-rep(NA, n)
total.fish<-rep(NA, n)
#we make a results object with the same length as the number of sites. The results object is where results from our for loops will go.
#res is where we will keep the number of infected fish at each site
#res2 is where we will keep the number of fish at each site

for(i in 1:n){
  site<-subset(best2020, best2020$groupedsites == licetable[i,1])
  total.fish[i]<-length(which(site$Sum_all_lice >= 0))
}
total.fish

for(i in 1:n){
  site<-subset(best2020, best2020$groupedsites == licetable[i,1])
  infected.fish[i]<-length(which(site$Sum_all_lice>0))
}
infected.fish

## want prevalence per date
dates<-data.frame(Dates = datetable$date)
nd <-length(datetable$date)
#redundant vv
#nnd<-nd[2]
#nnnd<-sum(nnd)
dinfected.fish<-rep(NA, nd)
dtotal.fish<-rep(NA, nd)

for(i in 1:nd){
  datefor<-subset(best2020, best2020$date == dates[i,1])
  dtotal.fish[i]<-length(which(datefor$Sum_all_lice >= 0))
}
total.fish

for(i in 1:nd){
  datefor<-subset(best2020, best2020$date == dates[i,1])
  dinfected.fish[i]<-length(which(datefor$Sum_all_lice>0))
}
dinfected.fish

datetable$dinfected.fish<-dinfected.fish
datetable$dtotal.fish<-dtotal.fish
datetable$prevalence<-datetable$dinfected.fish/datetable$dtotal.fish

siteprevalence<-data.frame(licetable[1], total.fish, infected.fish)
siteprevalence$prevalence<-siteprevalence$infected.fish/siteprevalence$total.fish


write.csv(siteprevalence, "OutputData/Clayoquot.site.prevalence.2020.csv")


totals<-data.frame(mean.cop = numeric(0), mean.chal = numeric(0), mean.mot = numeric(0), 
                            mean.tot = numeric(0),se.cop = numeric(0), se.chal = numeric(0), se.mot = numeric(0), 
                            se.tot = numeric(0), mean.prev = numeric(0), sd.prev = numeric(0), se.prev = numeric(0))

totals[1,1:11]<-c(mean(best2020$copsum), mean(best2020$chalsum), mean(best2020$motsum),mean(best2020$Sum_all_lice),
                          sd(best2020$copsum)/sqrt(length(best2020$fish_id)), 
                          sd(best2020$chalsum)/sqrt(length(best2020$fish_id)), 
                          sd(best2020$motsum)/sqrt(length(best2020$fish_id)), 
                          sd(best2020$Sum_all_lice)/sqrt(length(best2020$fish_id)), 
                          mean(datetable$prevalence), sd(datetable$prevalence), 
                          sd(datetable$prevalence)/sqrt(length(datetable$date)))

write.csv(totals, "OutputData/totalabundances.prevalence.csv")

####optional subsets for site groupings################
#prev.bedwell2019<-data.frame(subset(siteprevalence, groupedsites == "Bedwell Estuary North" | groupedsites == "Bedwell Estuary Middle" | groupedsites == "Bedwell Estuary South"))
#prev.Macks2019<- data.frame(subset(siteprevalence, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay" | groupedsites == "Buckle Bay"))
#prev.Misc2019<- data.frame(subset(siteprevalence, groupedsites == "Tranquil estuary"| groupedsites == "Keltsmaht"| groupedsites == "Moyeha"| groupedsites == "Elbow Bank" | groupedsites == "TRM"|groupedsites == "Tsapee Narrows"))

#########this is prevalence at each site which can be shown in the barchart

#current sample site list
#"Bedwell Estuary", "Bedwell Estuary 2", "Bedwell Estuary 3", "Bedwell Estuary 4", 
#"Bedwell River", "Buckle Bay", "Cypre River", "Elbow Bank", "Keltsmaht", "Moyeha", "Ritchie Bay", 
#"Sniffles", "Sniffles 2", "Tranquil Estuary", "TRM", "Tsapee Narrows"

##

#making the attached and motile lice into one column. 
#This is for the ggplot which is pretty meh. We didn't use it, but it could be used.
motile_lice<-licetable$motsum
attached_lice<-licetable$attachedsum

Lice_Sum<-c(rbind(motile_lice, attached_lice))
##****************************

#Need to add any new sites here in "_", like Tsapee Narrows and TRM
unique(best2020$groupedsites)
Sample_Site<- c(rep(c("Bedwell Sound North","North Meares" , "Cypre River", "Ritchie Bay", "Tsapee Narrows"), each = 2))

#need to put in the total number of sample sites here
ns<-(length(Sample_Site)/2)
Lice_Stages<- c(rep(c("Motile", "Attached"), times = ns))
Data<-data.frame(Sample_Site, Lice_Stages, Lice_Sum)
## END OF CALCULATIONS ##


## PLOTTING ##

#setting up plot descriptions
colours<-rainbow(n)
linetype<-c(1:n)   
plotchar<-seq(18,18+n,1)

##PREVALENCE WITH STAGES plots for each site

#This gives you the prevalence for different stages over time. 

#Need this vector for the legend in the for loops.
prevalence.stage.legend<-c("Total","Copepodid", "Chalimus", "Motile")
groups.locations<-data.frame(Sample_Site)


prevsiteday <- data.frame(date = numeric(0),
                          site = character(0),
                          totalprev = numeric(0),
                          motprev = numeric(0),
                          chalprev = numeric(0),
                          copeprev = numeric(0))

#x# this line wont run, counts object does not exist. Refering to salmocounts?
# RM : I think this is just formatting the dates for the plots by weeklyintervals. So we can apply it when/if necessary as long as we provide dates that match the data to be plotted.
#prevsiteday$date <- as.Date(counts$date, levels=weeklyintervals, origin=as.Date("1970-01-01"), format = "%b %d %y")  # Make sure months are ordered correctly for future plotting

#how to store the data in forloops
#prevsiteweek[i,1] <- date.name[i]
#prevsiteweek[i,2:5] <- c(mean(mean.boot), mean.boot[25], mean.boot[976])
dev.off()
### 
#Just doing a for loops for each location.

nloop<-length(listofsites)

#RM : trying to make the legend appear outside the plot so it doesn't get overlapped by data
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

for (i in 1:nloop) {
  par(mfrow=c(1,1))
  site3<-subset(best2020, groupedsites == listofsites[i]) 
  #this gives you an individual site to work with.
  #optional subset for chum. Subsetting for chinook and coho might be ok, but probably very low numbers.
  
  #site.s3<-subset(site3, species == "chum")
  #for (j in 1:datecount) {
  
  site3$countcol <- rep(1,nrow(site3))
  #this gives you a column of ones
  nc3<-length(site3$countcol)
  #this is the count of fish at the sites
  site3$infected<-rep(0,nc3)
  site3$infected = site3$infected + (site3$Sum_all_lice > 0)
  #This gives you a column of 1 or 0 where 1 means they are infected and 0 means they are clean
  
  ##Trying to make prevalence of the different stages##
  #Make a column for the cope, chal and motile stages
  site3$copinf<-rep(0,nc3)
  site3$copinf = site3$copinf + (site3$Lep_cope >0 | site3$unid_cope >0| site3$Caligus_cope >0)
  site3$copinf[is.na(site3$copinf)]<-0
  site3$chalinf<-rep(0,nc3)
  site3$chalinf = site3$chalinf + (site3$chalA >0 | site3$chalB >0| site3$chal_unid >0)
  site3$chalinf[is.na(site3$chalinf)]<-0
  site3$motinf<-rep(0,nc3)
  site3$motinf = site3$motinf + (site3$Lep_PAmale >0 | site3$Lep_PAfemale >0| site3$Lep_male >0 |site3$Lep_nongravid >0| site3$Lep_gravid >0|site3$Caligus_mot >0|site3$Caligus_gravid >0|site3$unid_PA >0 |site3$unid_adult >0)
  site3$motinf[is.na(site3$motinf)]<-0
  #The above seems weird because the total prevalence is not additive of all the different stages.So you can have as many total infected as the max number for a given stage.
  
  
  
  # now just need to aggregate using date.
  siteagg3<-aggregate(x = site3[c("infected", "countcol", "copinf", "chalinf", "motinf")], FUN = sum, by = list(Group.date = site3$date))
  #shows you how many were infected for each date that the specific site was sampled
  siteforsiteagg<-rep(paste(listofsites[i]), length(siteagg3$Group.date))
  siteagg3$site<-siteforsiteagg
  #aggregates for the other stages so that we may have a prevalence line per stage
  #siteaggcop<-aggregate(x = site3[c("copinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  #siteaggchal<-aggregate(x = site3[c("chalinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  #siteaggmot<-aggregate(x = site3[c("motinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  
  names(siteagg3)[3]<-paste("total.fish")
  #changing header names
  names(siteagg3)[2]<-paste("total.infected.fish")
  
  #calculating prevalence for sites
  siteagg3$copprev<-siteagg3$copinf/siteagg3$total.fish
  siteagg3$chalprev<-siteagg3$chalinf/siteagg3$total.fish
  siteagg3$motprev<-siteagg3$motinf/siteagg3$total.fish
  siteagg3$totalprevalence<-siteagg3$total.infected.fish/siteagg3$total.fish
  
  #*************************
  #can change ranges to match the subset
  names(siteagg3)[8]<-paste("copepodid.prevalence")
  names(siteagg3)[9]<-paste("chalimus.prevalence")
  names(siteagg3)[10]<-paste("motile.prevalence")
  names(siteagg3)[11]<-paste("total.prevalence")
  
  loopssubset1<-subset(best2020, groupedsites == listofsites[i])
  loop1xrange.dp<-range(loopssubset1$date) 
  #xrange.dp<-range(best2020$date)
  forprevyrange<-seq(0.00, signif(max(siteagg3$total.prevalence, na.omit = TRUE ), digits = 2), 0.01)
  loops1yrange.dp<-range(forprevyrange)
  coloursloop<-c("black","red","blue3","forestgreen")

  
    plot(siteagg3$total.prevalence~siteagg3$Group.date, xlim = loop1xrange.dp, ylim = loops1yrange.dp, type="n", xlab = "Date", ylab = "Prevalence (infected fish/total fish)") 
  
  # RM : if a site was only sampled once, the plot looks pretty silly. It shows 2000 - 2040 with data for one x value. 
  #     We probably don't need to change these plots because seeing this data over time is uneccesary. They can just be discarded and the info can be seen in the barplot

  legend("topright",inset=c(-0.2,0), cex=0.6, legend = prevalence.stage.legend, pch=plotchar, lty=linetype, title = "Louse Stages", col = coloursloop)
  title(main = listofsites[i])
  
  
  lines(siteagg3$Group.date, siteagg3$total.prevalence, lty=linetype[1], pch=plotchar[1], lwd = 1.5, type ="b", col = "black")
  lines(siteagg3$Group.date, siteagg3$copepodid.prevalence, lty=linetype[2], pch=plotchar[2], lwd = 1.5, type ="b", col = "red" )
  lines(siteagg3$Group.date, siteagg3$chalimus.prevalence, lty=linetype[3], pch=plotchar[3], lwd = 1.5, type ="b", col = "blue3" )
  lines(siteagg3$Group.date, siteagg3$motile.prevalence, lty=linetype[4], pch=plotchar[4], lwd = 1.5, type ="b", "forestgreen" )
  
  prevsiteday<-rbind(prevsiteday, siteagg3)
  
}
# RM : The line below will make the legend come back into the plot panel.
#par(xpd=FALSE)

#Daily mean lice with stages
datesforstages<-format(meandatetable$date, format = "%b %d %y")
groupedstagesdata<-meandatetable[,c(2,3,6,5)]

colnames(groupedstagesdata)=c("Mean Total Lice","Mean Motile","Mean Chalimus","Mean Copepodid")
rownames(groupedstagesdata)= datesforstages
stagesmatrix=matrix(groupedstagesdata)

barplot(t(groupedstagesdata), col= c("darkgray","dodgerblue","red","darkgreen") , border="white", font.axis=2, 
        beside=T, legend=c(), xlab="group", font.lab=2, ylim = c(0,0.8), ylab = "Mean Lice per Fish", main = "Daily Mean Lice - Clayoquot Salmon, 2020")
#abline(h= seq(0, ceiling(max(groupedstagesdata)), 0.1), col = "light gray")


licecol2<-c("darkgray","dodgerblue","red","darkgreen")
legend("topright", cex=0.6, legend = c("Total Lice", "Motile", "Chalimus", "Copepodid"), col = licecol2, title = "Lice Stage", lty = 1, lwd = 4)


 
dev.copy(png,'Outputfigures/Clayoquot.daily.mean.lice.2020.png')


#MAKING WEEKLY LICE TABLES
meanlicefish<-matrix(data = NA, nrow = length(weeklyintervals), ncol = 4)
colnames(meanlicefish)<-names(groupedstagesdata)
SDcol<-rep(0, length(weeklyintervals))
#whereIcan put the sd's
sdliceweekly<-data.frame(cbind(SDcol, SDcol, SDcol, SDcol))
colnames(sdliceweekly)<-c("sdtotal", "sdmotile", "sdchalimus", "sdcop")
rownames(meanlicefish)<-weeklyintervals

#4 columns of mean lice (total + 3 stages)
sdlicefish<-rep(0, times = length(weeklyintervals)*4)
meandatetable$j.date<-julian(meandatetable$date)
#groupedstagesdata has the columns for means. The columns will be what i references in for loops = mean for every stage of louse = i in groupedstagesdata[1:4]
#making a vector with first week means shown

for (i in 1:4) {
  
  firstweeklice<-subset(meandatetable, meandatetable$j.date <= weeklyintervals[1])
  firstweeklice<-firstweeklice[,c(2,3,6,5)]
  meanlicefish[1,i]<-mean(firstweeklice[1,i])
  
}
#sd might be harder to do because we might want all of the fish from the days in the intervals. Instead of just the means of the fish. Would need calculate the sd's of all the fish from those days.

#addonmeanflfish<-rep(0, times = ((length(weeklyintervals)*length(listspeciesinterest))-length(listspeciesinterest)+1))
#addonsdflfish<-rep(0, times = ((length(weeklyintervals)*length(listspeciesinterest))-length(listspeciesinterest)+1))
#speciesinterest<-rep(c("chum", "coho", "chinook"), times = length(weeklyintervals))
#for (i in 1:length(listspeciesinterest)) {
#  fishbest2020<-subset(best2020, species == listspeciesinterest[i])


for (i in 2:(length(weeklyintervals))) {
  weeklylice<-subset(meandatetable, meandatetable$j.date <= weeklyintervals[i] & meandatetable$j.date > weeklyintervals[i-1])
  weeklylice<-weeklylice[,c(2,3,6,5)]
  for (j in 1:4) {
    
    meanlicefish[i,j]<-mean(weeklylice[,j])
    
  }
}



meanlicefish1<-data.frame(cbind(meanlicefish, weeklyintervals))
rownames(meanlicefish)<-as.Date(weeklyintervals, origin=as.Date("1970-01-01"))
#meanlicefish has all the means you need. Just need to give it rownames that = dates.
# Now I can put this shit into the barplot.

datesforstages<-format(weeklyintervals, format = "%b %d %y")
groupedstagesdata<-meanlicefish

colnames(groupedstagesdata)=c("Mean Total Lice","Mean Motile","Mean Chalimus","Mean Copepodid")
rownames(groupedstagesdata)= datesforstages
stagesmatrix=matrix(groupedstagesdata)

barplot(t(groupedstagesdata), col= c("darkgray","dodgerblue","red","darkgreen") , border="white", font.axis=2, 
        beside=T, legend=c(), font.lab=2, ylim = c(0,0.8), ylab = "Mean Lice per Fish", main = "Mean Lice per Fish")
#abline(h= seq(0, 0.8, 0.1), col = "light gray")
licecol<-c("darkgray","dodgerblue","red","darkgreen")
legend("topright", cex=0.6, legend = c("Total Lice", "Motile", "Chalimus", "Copepodid"), col = licecol, title = "Lice Stage", lty = 1, lwd = 4)

dev.copy(png,'Outputfigures/Clayoquot.weekly.mean.lice.2020.png')
#bookmark
#need to actually make sure that it is giving mean lice/fish because it looks like too drastic of a drop in mean lice to actually be real...

##ASSIGNING WEEKLY INTERVALS TO ALL THE dates in the best2020 data set 
#weekly intervals.
#set up vectors to hold data
JDweeklyintervalsloops<-c(0, JDweeklyintervals)
best2020$weeklyintvl<-rep(0, each = length(best2020$date))
#using subsets to add data of appropriate date to the vectors
for (i in 1:(length(JDweeklyintervalsloops)-1)) {
  loopintvl<-subset(best2020, best2020$j.date > JDweeklyintervalsloops[i] & best2020$j.date <= JDweeklyintervalsloops[i+1])
  positionsforaddingtobest2020<-which(best2020$j.date > JDweeklyintervalsloops[i] & best2020$j.date <= JDweeklyintervalsloops[i+1])
  intvladd<-rep(JDweeklyintervalsloops[i+1], each = length(loopintvl$date))
  best2020$weeklyintvl[positionsforaddingtobest2020]<-intvladd
  
  
}


#TS PLOTS

#For TS plots, just change the spots that are marked (copy and paste )


tsaltemp <- read.csv("Data/clayoquot.site.data.csv", header=TRUE, stringsAsFactors=FALSE,
         fileEncoding="latin1")

#RM : This is just my own wd workaround
#tsaltemp <- read.csv("clayoquot.site.data.csv")


tsal2020 <- subset(tsaltemp, year == yr)
#View(tsal2020)
#***********************
#remove all the comments on the csv file
#making the separate day, month and year columns into date
tsal2020$date <- as.Date(with(tsal2020, paste(year, month, day, sep="-")), "%Y-%m-%d")
#setting up the new names for the locations. 

#***********************
#If you have changed the name of any sites, please change them here to match
#the line of code below is used to lump sites together by naming them the same thing. 
#Example, Bedwell River 3 and 2 are now called Bedwell Sound Middle

tsal2020$groupedsites<-tsal2020$location
#levels(tsal2020$groupedsites)<-c(levels(tsal2020$groupedsites), c("Bedwell Sound South","Bedwell Sound North","Bedwell Sound Middle"))
tsal2020$groupedsites[tsal2020$groupedsites == "Bedwell River"]<- "Bedwell Sound North"
#tsal2020$groupedsites[tsal2020$groupedsites == "Bedwell River 4"]<-"Bedwell Sound North"
#tsal2020$groupedsites[tsal2020$groupedsites == "Bedwell River"]<- "Bedwell Sound North"
#tsal2020$groupedsites[tsal2020$groupedsites == "Bedwell River 2"]<- "Bedwell Sound Middle"
#tsal2020$groupedsites[tsal2020$groupedsites == "Bedwell River 3"]<-"Bedwell Sound Middle"
#tsal2020$groupedsites[tsal2020$groupedsites == "Sniffles"]<- "Bedwell Sound Middle"
#tsal2020$groupedsites[tsal2020$groupedsites == "Sniffles 2"]<- "Bedwell Sound Middle"
##*******************change the focus sites for the TS plots, if you like
# RM : this code could be automated to paste the site name without spaces. Then you could call on these below.
loctsal<-c("bedwellplottsal","mearesplottsal","cypreplottsal","ritchieplottsal")
# RM : This is an example of where you might use a vector, defined once at the beginning, for listing focus sites for the study year
tsalsites<-unique(tsal2020$groupedsites)
tsal2020$salt_surf<-as.numeric(as.character(tsal2020$salt_surf))
tsal2020$salt_1m<-as.numeric(as.character(tsal2020$salt_1m))
tsal2020$temp_surf<-as.numeric(as.character(tsal2020$temp_surf))
tsal2020$temp_1m<-as.numeric(as.character(tsal2020$temp_1m))
tsal2020$date<-as.Date(tsal2020$date, origin ="%Y-%m-%d")

# RM : I made this part a for loop
#ritchieplottsal<-data.frame(meansurfsalt = numeric(0),
                    #        meansalt1 = numeric(0),
                    #        meantempsurf = numeric(0),
                    #        meantemp1 = numeric(0))

#cypreplottsal<-data.frame(meansurfsalt = numeric(0),
                     #       meansalt1 = numeric(0),
                    #        meantempsurf = numeric(0),
                    #        meantemp1 = numeric(0))

#bedwellplottsal<-data.frame(meansurfsalt = numeric(0),
                     #       meansalt1 = numeric(0),
                    ##        meantempsurf = numeric(0),
                      #      meantemp1 = numeric(0))

#mearesplottsal<-data.frame(meansurfsalt = numeric(0),
                       #     meansalt1 = numeric(0),
                      #      meantempsurf = numeric(0),
                      #      meantemp1 = numeric(0))



########Tables and plots of site data - Ritchie as example below
#$%^&
#********
#for a table and plot of a site, change site you choose in tsalsites[#], if you like. You can
#make a for loop to produce all the sites' plots and tables at once. I ran out of time
#ex if you want Bedwell Sound north, you input 1 into tsalsites[]
# RM : I made a for loop to help automate this part. The code below could also be automated and this loop could be better (see note at beginning of TS Plots)
year <- yr

for (d in 1:length(tsalsites)) {
  
temptsal<-subset(tsal2020, groupedsites == tsalsites[d])

datetsal <- as.Date(unique(temptsal$date), origin ="%Y-%m-%d")
tsd.site<- data.frame(meansurfsalt = numeric(0),
                           meansalt1 = numeric(0),
                           meantempsurf = numeric(0),
                           meantemp1 = numeric(0))


#x# it looks like the objects being pulled from in this forloop are not returning any actual values, just NA's and 0's. I think it has to do with the plotsal df's, 
#these issues seem to propogate into the plots below when tempdates is used
# RM : I think the issue is that the length is based on the subset of data for one site, but is applied to all sites. I would recommend redoing this whole section as a forloop.
for (j in 1:length(datetsal)) {
  #subsetting by the first date
  tempdates<-subset(temptsal, date == datetsal[j])  
  for (k in 1:4) {
    #taking the mean of the salinity/temp values for the one date
    tsd.site[j,k]<-mean(tempdates[,(6+k)])
     }
}
site.ts <- rep(tsalsites[d], each = length(tsd.site$meantemp1))
tsd.site1 <- cbind(site.ts, tsd.site)
if(d == 1) {tsdframe <- as.data.frame(tsd.site1)
} else if(d > 1 ){tsdframe <- as.data.frame(rbind(tsdframe, tsd.site1))}
}


# RM : This should be a for loop.
for(i in 1 : length(tsalsites)){

    temptsal<-subset(tsal2020, groupedsites == tsalsites[i])
      datetsal <- as.Date(unique(temptsal$date), origin ="%Y-%m-%d")
      temp.d <- subset(tsdframe, site.ts == tsalsites[i])
ritchieplottsal<-data.frame(datetsal, temp.d)
ritchieplottsal$datetsal<-as.Date(format(ritchieplottsal$datetsal, format = "%Y/%m/%d"))
#RM : note that the line below could omit more rows than you want if the row is Only missing surface salinity
ritchieplottsal <- ritchieplottsal[!is.na(ritchieplottsal$meansurfsalt),]
ritchieplottsal1 <- as.data.frame(ritchieplottsal) 
ritchieplottsal1 <- ritchieplottsal1[, colSums(is.na(ritchieplottsal1)) < nrow(ritchieplottsal1)]

#na.omit(ritchieplottsal1)
#complete.cases(ritchieplottsal1)
#ritchieplottsal1$meansurfsalt <- NULL
#ritchieplottsal1<-as.data.frame(na.omit(ritchieplottsal), stringsAsFactors=FALSE)  
xrangets<-as.Date(format(range(ritchieplottsal1$datetsal), format = "%Y-%m-%d"))
xts<-as.Date(format(ritchieplottsal1$datetsal, format = "%Y-%m-%d"))

yrangets2<-c(10,30)

par(mar = c(5,5,5,5), xpd = F, new = F)

plot(ritchieplottsal1$meansalt1~xts,  type = "n", 
     xlim = xrangets, ylim = yrangets2, ylab = "Salinity (psu)",
     cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = paste("Temp Salinity", tsalsites[i], year, sep = " "), cex.main = 2)

lines(ritchieplottsal1$meansalt1~xts,
      type = "b", lwd = 2, lty = 1, col = "darkgray") 
lines(ritchieplottsal1$meansurfsalt~xts, 
      type = "b", lwd = 2, lty = 2, col = "dodgerblue") 

legend("bottomright", legend = c("Sal 0 m", "Sal 1 m", "Temp 0 m", "Temp 1 m"),
       col = c("dodgerblue", "lightgray", "navyblue", "gray44"), cex = 1.5,bty = "n",
       lwd = 1.75, title = "Depth", lty = c(2,1), pch = 1)

par(new = TRUE)

yrangets3<-c(5,20)

plot(ritchieplottsal1$meantemp1~xts,  type = "n", axes = FALSE,
     xlim = xrangets, ylim = yrangets3, ylab = "",
     cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = "", cex.main = 2)

lines(ritchieplottsal1$meantemp1~xts,
      type = "b", lwd = 2, lty = 1, col = "gray44") 
lines(ritchieplottsal1$meantempsurf~xts, 
      type = "b", lwd = 2, lty = 2, col = " navyblue") 
axis(4, ylim = yrangets3, cex.lab=1.5,cex.axis=1.5)
mtext(side = 4, "Temperature (C)", line = 2.5, cex = 1.5)


write.csv(ritchieplottsal1, paste("meanTS.",tsalsites[i], sep ="_"))

jpeg(paste(tsalsites[i],year,"TS.pdf", sep='_'),5,5)
dev.off()

}



#RM : you shouldn't need the code between these hashtags

#######################################################################################

#####Cypre

temptsal<-subset(tsal2020, groupedsites == tsalsites[2])
temptsal$date<-as.Date(temptsal$date, origin ="%Y-%m-%d")
datetsal<-as.Date(unique(temptsal$date), origin = "%Y-%m-%d")

for (j in 1:length(datetsal)) {
  #subsetting by the first date
  tempdates<-subset(temptsal, date == datetsal[j])  
  for (k in 1:4) {
    #taking the mean of the salinity/temp values for the one date
    cypreplottsal[j,k]<-mean(tempdates[,(5+k)]) 
  }
  {
    ritchieplottsal[j,k]<-mean(tempdates[,(5+k)])
  }
  {
    mearesplottsal[j,k] <- mean(tempdates[, (5+k)])
  }
}


cypreplottsal<-data.frame(datetsal, cypreplottsal)
cypreplottsal$datetsal<-as.Date(format(cypreplottsal$datetsal, format = "%Y/%m/%d"))

cypreplottsal1 <- as.data.frame(cypreplottsal) 
cypreplottsal1 <- cypreplottsal1[, colSums(is.na(cypreplottsal1)) < nrow(cypreplottsal1)]

#na.omit(ritchieplottsal1)
#complete.cases(ritchieplottsal1)
#ritchieplottsal1$meansurfsalt <- NULL
#ritchieplottsal1<-as.data.frame(na.omit(ritchieplottsal), stringsAsFactors=FALSE)  
xrangets<-as.Date(format(range(cypreplottsal1$datetsal), format = "%Y-%m-%d"))
xts<-as.Date(format(cypreplottsal1$datetsal, format = "%Y-%m-%d"))
range(cypreplottsal1$meansalt1)
yrangets2<-c(5,35)

par(mar = c(4,5,5,4))

par(new = FALSE)

plot(cypreplottsal1$meansalt1~xts,  type = "n", 
     xlim = xrangets, ylim = yrangets2, ylab = "Salinity (psu)",
     cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = "Temp Salinity Cypre Bay, 2020", cex.main = 2)

lines(cypreplottsal1$meansalt1~xts,
      type = "b", lwd = 2, lty = 1, col = "darkgray") 
lines(cypreplottsal1$meansurfsalt~xts, 
      type = "b", lwd = 2, lty = 2, col = "dodgerblue") 

legend("bottomright", legend = c("Sal 0 m", "Sal 1 m", "Temp 0 m", "Temp 1 m"),
       col = c("dodgerblue", "lightgray", "navyblue", "gray44"), cex = 1.5,bty = "n",
       lwd = 1.75, title = "Depth", lty = c(2,1), pch = 1)

par(new = TRUE)

yrangets3<-c(8,16)

plot(cypreplottsal1$meantemp1~xts,  type = "n", axes = FALSE,
     xlim = xrangets, ylim = yrangets3, ylab = "",
     cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = "", cex.main = 2)

lines(cypreplottsal1$meantemp1~xts,
      type = "b", lwd = 2, lty = 1, col = "gray44") 
lines(cypreplottsal1$meantempsurf~xts, 
      type = "b", lwd = 2, lty = 2, col = " navyblue") 
axis(4, ylim = yrangets3, cex.lab=1.5,cex.axis=1.5)
mtext(side = 4, "Temperature (C)", line = 2.5, cex = 1.5)

#####North Meares
temptsal<-subset(tsal2020, groupedsites == tsalsites[4])
temptsal$date<-as.Date(temptsal$date, origin ="%Y-%m-%d")
datetsal<-as.Date(unique(temptsal$date), origin = "%Y-%m-%d")

for (j in 1:length(datetsal)) {
  #subsetting by the first date
  tempdates<-subset(temptsal, date == datetsal[j])  
  for (k in 1:4) {
    #taking the mean of the salinity/temp values for the one date
    mearesplottsal[j,k] <- mean(tempdates[, (5+k)]) 
  }
  {
    ritchieplottsal[j,k]<-mean(tempdates[,(5+k)])
  }
  {
    cypreplottsal[j,k]<-mean(tempdates[,(5+k)])
  }
  
}


mearesplottsal<-data.frame(datetsal, mearesplottsal)
mearesplottsal$datetsal<-as.Date(format(mearesplottsal$datetsal, format = "%Y/%m/%d"))

mearesplottsal1 <- as.data.frame(mearesplottsal) 
mearesplottsal1 <- mearesplottsal1[, colSums(is.na(mearesplottsal1)) < nrow(mearesplottsal1)]

#na.omit(ritchieplottsal1)
#complete.cases(ritchieplottsal1)
#ritchieplottsal1$meansurfsalt <- NULL
#ritchieplottsal1<-as.data.frame(na.omit(ritchieplottsal), stringsAsFactors=FALSE)  
xrangets<-as.Date(format(range(mearesplottsal1$datetsal), format = "%Y-%m-%d"))
xts<-as.Date(format(mearesplottsal1$datetsal, format = "%Y-%m-%d"))
range(mearesplottsal1$meansalt1)
yrangets2<-c(5,35)

par(mar = c(4,5,5,4))

par(new = FALSE)

plot(mearesplottsal1$meansalt1~xts,  type = "n", 
     xlim = xrangets, ylim = yrangets2, ylab = "Salinity (psu)",
     cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = "Temp Salinity North Meares Bay, 2020", cex.main = 2)

lines(mearesplottsal1$meansalt1~xts,
      type = "b", lwd = 2, lty = 1, col = "darkgray") 
lines(mearesplottsal1$meansurfsalt~xts, 
      type = "b", lwd = 2, lty = 2, col = "dodgerblue") 

legend("bottomright", legend = c("Sal 0 m", "Sal 1 m", "Temp 0 m", "Temp 1 m"),
       col = c("dodgerblue", "lightgray", "navyblue", "gray44"), cex = 1.5,bty = "n",
       lwd = 1.75, title = "Depth", lty = c(2,1), pch = 1)

par(new = TRUE)

yrangets3<-c(8,16)

plot(mearesplottsal1$meantemp1~xts,  type = "n", axes = FALSE,
     xlim = xrangets, ylim = yrangets3, ylab = "",
     cex.lab = 1.5, cex.axis = 1.5, xlab = "", main = "", cex.main = 2)

lines(mearesplottsal1$meantemp1~xts,
      type = "b", lwd = 2, lty = 1, col = "gray44") 
lines(mearesplottsal1$meantempsurf~xts, 
      type = "b", lwd = 2, lty = 2, col = " navyblue") 
axis(4, ylim = yrangets3, cex.lab=1.5,cex.axis=1.5)
mtext(side = 4, "Temperature (C)", line = 2.5, cex = 1.5)

# RM : you shouldn't need the code between this hash line and the one above.
#######################################################################################







# PREVALENCE Ab
#Below gives prevalence for the main 3 ccfs sites (cypre, north meares, and ritchie), 
#with standard deviation

#****************
#ctrl f the astrix row to find where edits must be made
colours<-rainbow(n)
linetype<-c(1:n)   
plotchar<-seq(18,18+n,1)

par(mar = c(3, 5,5,2))

par(mfrow=c(1,1))

#remove bedwell as not enough data for analysis
#x# for some reason this function does not work on my end, the next line seems to fix it
nobedwell <- best2020 %>% filter(groupedsites != "Bedwell Sound North")
nobedwell <- subset(best2020, groupedsites!="Bedwell Sound North")
#check with unique
unique(nobedwell$location)
site3<- nobedwell
#this gives you an individual site to work with.

site3$countcol <- rep(1,nrow(site3))
#this gives you a column of ones
nc3<-length(site3$countcol)
#this is the count of fish at the sites
site3$infected<-rep(0,nc3)
site3$infected = site3$infected + (site3$Sum_all_lice > 0)
#This gives you a column of 1 or 0 where 1 means they are infected and 0 means they are clean

##Trying to make prevalence of the different stages##
#Make a column for the cope, chal and motile stages
site3$copinf<-rep(0,nc3)
site3$copinf = site3$copinf + (site3$Lep_cope >0 | site3$unid_cope >0| site3$Caligus_cope >0)

site3$copinf[is.na(site3$copinf)]<-0
site3$chalinf<-rep(0,nc3)
site3$chalinf = site3$chalinf + (site3$chalA >0 | site3$chalB >0| site3$chal_unid >0)
site3$chalinf[is.na(site3$chalinf)]<-0
site3$motinf<-rep(0,nc3)
site3$motinf = site3$motinf + (site3$Lep_PAmale >0 | site3$Lep_PAfemale >0| site3$Lep_male >0 |site3$Lep_nongravid >0| site3$Lep_gravid >0|site3$Caligus_mot >0|site3$Caligus_gravid >0|site3$unid_PA >0 |site3$unid_adult >0)
site3$motinf[is.na(site3$motinf)]<-0
#The above seems weird because the total prevalence is not additive of all the different stages.So you can have as many total infected as the max number for a given stage.

##getting the date subset so that we can get Sd.
datelistsdp<-unique(site3$date)
sdcols<-  data.frame(
  sdtot = as.numeric(0),
  sdcope = as.numeric(0),
  sdchal = as.numeric(0),
  sdmot = as.numeric(0)) 

# SD for prevalence
# RM : you can't get SD for the prevalence associated with one date unless you find sd for each site used in the prevalence-by-date. So we shan't provide it.
# RM : If you really want to, I think this for loop gives it. But it makes the bars very large for June 12
for (j in 1:length(datelistsdp)) {
  date3<-subset(site3, date == datelistsdp[j])
  date33 <- as.data.frame(cbind(date3$location, date3$Sum_all_lice, date3$copsum, date3$chalsum, date3$motsum, date3$countcol ))
  loctp <- unique(date33$location)
  #more than 1 location
  if(length(unique(date33$location)) > 1){
  
    #prevalence for each location
    for (a in 1:length(unique(date33$location))) {
    prev.t <- subset(date33, location == loctp[a])
    
        # Create a list of prevalences from which to get their sd. 
        for (p in 1:4) {
         for (r in 1:nrow(prev.t))
           if(prev.t[r,(p+1)] >=1) {prev.t[r,(p+1)] <- 1}          
           l.p <- sum(prev.t[,(p+1)])/sum(prev.t$countcol) 
  }
    #bind the rows for each site's prevalence into data frame
    if (a == 1){pre.s <- l.p}
    if (a >1 ) {pre.s <- as.data.frame(rbind(pre.s,lp))}    
    
  }
    #sd for prevalences at all sites
    for (k in 1:4) {
        sdcols[j,k]<-sd(pre.s[k])
  }
  } else if(length(unique(date3$location)) == 1) {sdcols[j,k] <- 0}
  }

# now just need to aggregate using date.
siteagg3<-aggregate(x = site3[c("infected", "countcol", "copinf", "chalinf", "motinf")], FUN = sum, by = list(Group.date = site3$date))
#shows you how many were infected for each date that the specific site was sampled
siteforsiteagg<-rep("Clayoquot", length(siteagg3$Group.date))
siteagg3$site<-siteforsiteagg
#aggregates for the other stages so that we may have a prevalence line per stage
#siteaggcop<-aggregate(x = site3[c("copinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
#siteaggchal<-aggregate(x = site3[c("chalinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
#siteaggmot<-aggregate(x = site3[c("motinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))

names(siteagg3)[3]<-paste("total.fish")
#changing header names
names(siteagg3)[2]<-paste("total.infected.fish")

#calculating prevalence for dates
siteagg3$copprev<-siteagg3$copinf/siteagg3$total.fish
siteagg3$chalprev<-siteagg3$chalinf/siteagg3$total.fish
siteagg3$motprev<-siteagg3$motinf/siteagg3$total.fish
siteagg3$totalprevalence<-siteagg3$total.infected.fish/siteagg3$total.fish



#can change ranges to match the subset
names(siteagg3)[8]<-paste("copepodid.prevalence")
names(siteagg3)[9]<-paste("chalimus.prevalence")
names(siteagg3)[10]<-paste("motile.prevalence")
names(siteagg3)[11]<-paste("total.prevalence")

siteagg3<-data.frame(siteagg3, sdcols)
loopssubset1<-best2020
loop1xrange.dp<-range(loopssubset1$date) 
#xrange.dp<-range(best2020$date)

weeklyprev<-NULL
weeklyprevstages<- data.frame(copep = as.numeric(0),
                              chalp = as.numeric(0),
                              motp = as.numeric(0),
                              totp = as.numeric(0),
                              sdtot = as.numeric(0),
                              sdcope = as.numeric(0),
                              sdchal= as.numeric(0),
                              sdmot = as.numeric(0))
dddd<-julian(siteagg3$Group.date)
ddprevweek<-data.frame(siteagg3, dddd)
#prevsiteday<-rbind(prevsiteday, siteagg3)
for (i in 1:(length(JDweeklyintervalsloops)-1)) {
  
  loopintvl<-subset(ddprevweek, ddprevweek$dddd > JDweeklyintervalsloops[i] & ddprevweek$dddd <= JDweeklyintervalsloops[i+1])
  for (j in 1:8) {
    weeklyprevstages[i,j]<-mean(loopintvl[,(7+j)])  
  }}

#overall prevalence for all stages for different dates.
overallprev<-data.frame(weeklyprevstages,JDweeklyintervalsloops[-1])

#formatting the date
overallprev$JDweeklyintervalsloops..1.<-as.Date(overallprev$JDweeklyintervalsloops..1., origin = as.Date("1970-01-01"))
overallprev$JDweeklyintervalsloops..1.<-format(  overallprev$JDweeklyintervalsloops..1., format = "%b %d %y")


#the range for the plots
forprevyrange<-seq(0.00, signif(max(siteagg3$total.prevalence, na.omit = TRUE ), digits = 2), 0.01)
loops1yrange.dp<-range(forprevyrange)
#colours for the plot
coloursloop<-c("darkgray","darkgreen","dodgerblue","red")
#x# unsure where to fix this difference in row length
overallprev$weekly<-as.Date(weeklyintervals, format = "%Y-%m-%d")

#RM : I think this line below is too general. It removes all rows because Sd is not a thing for prevalence anymore
#overallprev<-na.omit(overallprev)
ylimoverall<-as.numeric(range(overallprev$totp))

drange<-range(overallprev$weekly)
prevoverlim<-as.Date(drange, format = "%Y-%m-%d")
overallprev$JDweeklyintervalsloops..1.<-as.Date( overallprev$JDweeklyintervalsloops..1., format = "%b %d %Y")

#  legend("topleft", cex=0.6, legend = prevalence.stage.legend, pch=plotchar, lty=linetype, title = "Louse Stages", col = coloursloop)
prevxx<-plot(overallprev$totp~overallprev$weekly, yaxt="n", xlim = prevoverlim, ylim =  c(0,1.0), type="n", ylab = "Prevalence (infected fish/total fish)", cex.lab = 1.5, cex.axis = 1.5, xlab = "Date")

legend("topleft", bty = "n", col = c("darkgreen","dodgerblue","red","darkgray"), title = "Louse Stages", lwd = 1.5, cex = 1, pch=1, lty=linetype, legend = c("Copepodid", "Chalimus", "Motile", "All"))
axis(side = 2, at = seq(0 , 1.0 , 0.2), las = 1, cex.lab = 1.5, cex.axis = 1.5)
title(main = "Clayoquot", cex.main = 2)

segp<-overallprev$weekly

totysegl<-overallprev$totp-overallprev$sdtot
copeysegl<-(overallprev$copep-overallprev$sdcope)
chalysegl<-(overallprev$chalp-overallprev$sdchal)
motysegl<-(overallprev$motp-overallprev$sdmot)


totysegu<-(overallprev$totp+overallprev$sdtot)
copeysegu<-(overallprev$copep+overallprev$sdcope)
chalysegu<-(overallprev$chalp+overallprev$sdchal)
motysegu<-(overallprev$motp+overallprev$sdmot)

# RM : the sd has been removed from this plot because you can't take sd from one data point. Which is the prevalence for each date.
#       I have provided SD calculated from the prevalences of each site specific to the date (above for loop)
#       But I don't think the sd is very strong for one (only a couple of sites or just 1), and it provides very large values for june 12.
lines(overallprev$weekly, overallprev$totp, lty=1, pch=1, lwd = 1.5, type ="b", col = "darkgray")
#segments(x0 = segp, totysegl, x1 =segp, totysegu, lwd = 2, col = "darkgray")  # confidence intervals
#arrows(x0 = segp, totysegl, x1 =segp, totysegu, lwd = 1, angle = 90,
#       code = 3, length = 0.05, col = "darkgray")

lines(overallprev$weekly, overallprev$copep, lty=linetype[2], pch=1, lwd = 1.5, type ="b", col = "darkgreen" )
#segments(x0 = segp, copeysegl, x1 = segp, copeysegu, lwd = 2, col = "darkgreen")  # confidence intervals
#arrows(x0 = segp, copeysegl, x1 = segp, copeysegu, lwd = 1, angle = 90,
#       code = 3, length = 0.05, col = "darkgreen")

lines(overallprev$weekly, overallprev$chalp, lty=linetype[3], pch=1, lwd = 1.5, type ="b", col = "dodgerblue" )
#segments(x0 = segp, y0 = chalysegl, x1 = segp, y1 = chalysegu, lwd = 2, col = "dodgerblue")  # confidence intervals
#arrows(x0 = segp, chalysegl, x1 = segp, chalysegu, lwd = 1, angle = 90,
#       code = 3, length = 0.05, col = "dodgerblue")
#x# return to fix missing values or NA's
lines(overallprev$weekly, overallprev$motp, lty=linetype[4], pch=1, lwd = 1.5, type ="b", col = "red" )
#segments(x0 = segp, y0 = motysegl, x1 = segp, y1 = motysegu, lwd = 2, col = "red" )  # confidence intervals
#arrows(x0 = segp, motysegl, x1 = segp, motysegu, lwd = 1, angle = 90,col = "red" ,
#       code = 3, length = 0.05)

#Prevelance means by site
unique(prevsiteday$site)

Meares.prev.mean <-data.frame(subset(prevsiteday, site == "North Meares"))
Meares.prev.mean <- mean(Meares.prev.mean$total.prevalence)
print(Meares.prev.mean)
#Ritchie
Ritchie.prev.mean <-data.frame(subset(prevsiteday, site == "Ritchie Bay"))
Ritchie.prev.mean <- mean(Ritchie.prev.mean$total.prevalence)
print(Ritchie.prev.mean)
#Cypre
Cypre.prev.mean <-data.frame(subset(prevsiteday, site == "Cypre River"))
Cypre.prev.mean <- mean(Cypre.prev.mean$total.prevalence)
print(Cypre.prev.mean)
#new df
total.prevalance.means <- c(rbind(Meares.prev.mean,Ritchie.prev.mean,Cypre.prev.mean))
Prev.tot.list <- c(rbind("North Meares", "Ritchie Bay", "Cypre River"))
total.prevalance.means <- cbind(Prev.tot.list,total.prevalance.means)
View(total.prevalance.means)

write.csv(total.prevalance.means, "OutputData/site.prevalence.totals.2020.csv")


#prevweekly<-which(best2020$j.date > JDweeklyintervalsloops[i] & best2020$j.date <= JDweeklyintervalsloops[i+1])

#intvladd<-rep(JDweeklyintervalsloops[i+1], each = length(loopintvl$date))

#best2020$weeklyintvl[prevweekly]<-intvladd


write.csv(overallprev[,-9], "OutputData/Clayoquot.weekly.mean.prevalence.2020.csv")

#saves the plot

dev.copy(png,'OutputFigures/Clayoquot.weekly.mean.prevalence.2020.png')
dev.off()

View(overallprev)

#A for loops for Prevalence at specific sites. 
#*******************
#Change the names in the focussitelist if they do in fact change :)

salmonlicebest2020<-subset(nobedwell, species == "chum" | species == "coho" | species == "chinook"| species == "sockeye" |species == "salmon")
weeksitelice<-data.frame(salmonlicebest2020$date, salmonlicebest2020$j.date, salmonlicebest2020$weeklyintvl ,salmonlicebest2020$groupedsites,  
                         salmonlicebest2020$copsum, salmonlicebest2020$chalsum, salmonlicebest2020$motsum, salmonlicebest2020$Sum_all_lice)
names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "groupedsites", "copsum", "chalsum", "motsum", "Sum_all_lice"))
#%%%%
#*********************
#to add a site to the focussitelist, add | groupedsites == "desired site" to the code below for vector focusweeksitelice
focusweeksitelice<-subset(weeksitelice, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay" | groupedsites == "North Meares" | groupedsites == "Bedwell Sound North")
focussitelist<- unique(focusweeksitelice$groupedsites)

for (i in 1:length(focussitelist)){
  
  site3<-subset(nobedwell, groupedsites == focussitelist[i])
  #this gives you an individual site to work with.
  
  site3$countcol <- rep(1,nrow(site3))
  #this gives you a column of ones
  nc3<-length(site3$countcol)
  #this is the count of fish at the sites
  site3$infected<-rep(0,nc3)
  site3$infected = site3$infected + (site3$Sum_all_lice > 0)
  #This gives you a column of 1 or 0 where 1 means they are infected and 0 means they are clean
  
  ##Trying to make prevalence of the different stages##
  #Make a column for the cope, chal and motile stages
  site3$copinf<-rep(0,nc3)
  site3$copinf = site3$copinf + (site3$Lep_cope >0 | site3$unid_cope >0| site3$Caligus_cope >0)
  site3$copinf[is.na(site3$copinf)]<-0
  site3$chalinf<-rep(0,nc3)
  site3$chalinf = site3$chalinf + (site3$chalA >0 | site3$chalB >0| site3$chal_unid >0)
  site3$chalinf[is.na(site3$chalinf)]<-0
  site3$motinf<-rep(0,nc3)
  site3$motinf = site3$motinf + (site3$Lep_PAmale >0 | site3$Lep_PAfemale >0| site3$Lep_male >0 |site3$Lep_nongravid >0| site3$Lep_gravid >0|site3$Caligus_mot >0|site3$Caligus_gravid >0|site3$unid_PA >0 |site3$unid_adult >0)
  site3$motinf[is.na(site3$motinf)]<-0
  #The above seems weird because the total prevalence is not additive of all the different stages.So you can have as many total infected as the max number for a given stage.
  
  ##getting the date subset so that we can get Sd.
  datelistsdp<-unique(site3$date)
  sdcols<-  data.frame(
    sdtot = as.numeric(0),
    sdcope = as.numeric(0),
    sdchal = as.numeric(0),
    sdmot = as.numeric(0)) 
  
  # RM : I don't think this is correct. I don't think you can get sd from one date. 
  # The prevalence is calculated from one date. So you need multiple prevalences to make sd, 
  # but you then would have only 1 sd for prevalence by date.

    #Getting SD.
 # for (j in 1:length(unique(site3$date))) {
  #  date3<-subset(site3, date == datelistsdp[j])
  # for (k in 1:4) {
  #    
  #    sdcols[j,k]<-(sd(date3[,50+k]))/sum(date3$countcol)
  #  }
  #}
  

  # now just need to aggregate using date.
  siteagg3<-aggregate(x = site3[c("infected", "countcol", "copinf", "chalinf", "motinf")], FUN = sum, by = list(Group.date = site3$date))
  #shows you how many were infected for each date that the specific site was sampled
  siteforsiteagg<-rep("Clayoquot", length(siteagg3$Group.date))
  siteagg3$site<-siteforsiteagg
  #aggregates for the other stages so that we may have a prevalence line per stage
  #siteaggcop<-aggregate(x = site3[c("copinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  #siteaggchal<-aggregate(x = site3[c("chalinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  #siteaggmot<-aggregate(x = site3[c("motinf", "countcol")]), FUN = sum, by = list(Group.date = site3$date))
  
  names(siteagg3)[3]<-paste("total.fish")
  #changing header names
  names(siteagg3)[2]<-paste("total.infected.fish")
  
  #calculating prevalence for sites
  siteagg3$copprev<-siteagg3$copinf/siteagg3$total.fish
  siteagg3$chalprev<-siteagg3$chalinf/siteagg3$total.fish
  siteagg3$motprev<-siteagg3$motinf/siteagg3$total.fish
  siteagg3$totalprevalence<-siteagg3$total.infected.fish/siteagg3$total.fish
  
  #can change ranges to match the subset
  names(siteagg3)[8]<-paste("copepodid.prevalence")
  names(siteagg3)[9]<-paste("chalimus.prevalence")
  names(siteagg3)[10]<-paste("motile.prevalence")
  names(siteagg3)[11]<-paste("total.prevalence")
  
  siteagg3<-data.frame(siteagg3, sdcols)
  loopssubset1<-best2020
  loop1xrange.dp<-range(loopssubset1$date) 
  #xrange.dp<-range(best2020$date)
  
  weeklyprev<-NULL
  weeklyprevstages<- data.frame(copep = as.numeric(0),
                                chalp = as.numeric(0),
                                motp = as.numeric(0),
                                totp = as.numeric(0),
                                sdtot = as.numeric(0),
                                sdcope = as.numeric(0),
                                sdchal= as.numeric(0),
                                sdmot = as.numeric(0))
  dddd<-julian(siteagg3$Group.date)
  ddprevweek<-data.frame(siteagg3, dddd)
  #prevsiteday<-rbind(prevsiteday, siteagg3)
  for (h in 1:(length(JDweeklyintervalsloops)-1)) {
    
    loopintvl<-subset(ddprevweek, ddprevweek$dddd > JDweeklyintervalsloops[h] & ddprevweek$dddd <= JDweeklyintervalsloops[h+1])
    for (j in 1:8) {
      weeklyprevstages[h,j]<-mean(loopintvl[,(7+j)])  
    }}
  #overall prevalence for all stages for different dates.
  overallprev<-data.frame(weeklyprevstages,JDweeklyintervalsloops[-1])
  
  #formatting the date
  overallprev$JDweeklyintervalsloops..1.<-as.Date(overallprev$JDweeklyintervalsloops..1., origin = as.Date("1970-01-01"))
  overallprev$JDweeklyintervalsloops..1.<-format(overallprev$JDweeklyintervalsloops..1., format = "%b %d %y")
  
  #the range for the plots
  forprevyrange<-seq(0.00, signif(max(siteagg3$total.prevalence, na.omit = TRUE ), digits = 2), 0.01)
  loops1yrange.dp<-range(forprevyrange)
  #colours for the plot
  coloursloop<-c("darkgray","darkgreen","dodgerblue","red")
  
  overallprev$weekly<-as.Date(weeklyintervals, format = "%Y-%m-%d")
  overallprev<-na.omit(overallprev)
  ylimoverall<-as.numeric(range(overallprev$totp))
  
  drange<-range(overallprev$weekly)
  prevoverlim<-as.Date(drange, format = "%Y-%m-%d")
  overallprev$JDweeklyintervalsloops..1.<-as.Date( overallprev$JDweeklyintervalsloops..1., format = "%b %d %Y")
  
  #RM : this par should probably be called earlier to apply to all plots.
  par(mar = c(5,5,5,8), xpd = TRUE, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
  prevxx<-plot(overallprev$totp~overallprev$weekly, yaxt="n", xlim = prevoverlim, ylim =  c(0,1.0), type="n", ylab = "Prevalence (infected fish/total fish)", xlab = "Date")
  # RM : The inset is used to make the legend appear in the margin, but it may need to
  #     be tweaked, or your plot pane in Rstudio may need to be widened when making the plot
  legend("topright", inset=c(-0.2,0), col = c("darkgreen","dodgerblue","red","darkgray"), legend = c("Copepodid", "Chalimus", "Motile", "All"), bty = "n",title = "Louse Stages", lwd = 1.5, cex = 1, pch=1, lty=linetype)
  axis(side = 2, at = seq(0 , 1.0 , 0.2), las = 1)
  #**************** 
  #change title if needed
  title(main = paste(focussitelist[i],"Prevalence, 2020"))
  
  segp<-overallprev$weekly
  
  totysegl<-overallprev$totp-overallprev$sdtot
  copeysegl<-(overallprev$copep-overallprev$sdcope)
  chalysegl<-(overallprev$chalp-overallprev$sdchal)
  motysegl<-(overallprev$motp-overallprev$sdmot)
  
  
  totysegu<-(overallprev$totp+overallprev$sdtot)
  copeysegu<-(overallprev$copep+overallprev$sdcope)
  chalysegu<-(overallprev$chalp+overallprev$sdchal)
  motysegu<-(overallprev$motp+overallprev$sdmot)
  
# RM: i do not think we can get sd for these prevalences. Only sd for prevalence by site, but this is plotted by date for 1 site.  
  lines(overallprev$weekly, overallprev$totp, lty=1, pch=1, lwd = 1.5, type ="b", col = "darkgray")
 # segments(x0 = segp, totysegl, x1 =segp, totysegu, lwd = 2, col = "darkgray")  # confidence intervals
#  arrows(x0 = segp, totysegl, x1 =segp, totysegu, lwd = 1, angle = 90,
#         code = 3, length = 0.05, col = "darkgray")
  
  lines(overallprev$weekly, overallprev$copep, lty=linetype[2], pch=1, lwd = 1.5, type ="b", col = "darkgreen" )
 # segments(x0 = segp, copeysegl, x1 = segp, copeysegu, lwd = 2, col = "darkgreen")  # confidence intervals
#  arrows(x0 = segp, copeysegl, x1 = segp, copeysegu, lwd = 1, angle = 90,
#         code = 3, length = 0.05, col = "darkgreen")
  
  lines(overallprev$weekly, overallprev$chalp, lty=linetype[3], pch=1, lwd = 1.5, type ="b", col = "dodgerblue" )
#  segments(x0 = segp, y0 = chalysegl, x1 = segp, y1 = chalysegu, lwd = 2, col = "dodgerblue")  # confidence intervals
#  arrows(x0 = segp, chalysegl, x1 = segp, chalysegu, lwd = 1, angle = 90,
#         code = 3, length = 0.05, col = "dodgerblue")
  
  lines(overallprev$weekly, overallprev$motp, lty=linetype[4], pch=1, lwd = 1.5, type ="b", col = "red" )
#  segments(x0 = segp, y0 = motysegl, x1 = segp, y1 = motysegu, lwd = 2, col = "red" )  # confidence intervals
#  arrows(x0 = segp, motysegl, x1 = segp, motysegu, lwd = 1, angle = 90,col = "red" ,
#         code = 3, length = 0.05)
  
  
  #saves the plot
  # RM : The dev.copy is the only error remaining. As I don't fully understand how the directories work between github and R, I will leave it with you :) 
  #dev.copy(png,'OutputFigures/Clayoquot.weekly.mean.prevalence.2020.png',paste(focussitelist[i]))
  #dev.off()
  #tables of data for each site
#  write.csv(overallprev[,-9], paste(focussitelist[i],"OutputData/weekly.mean.prevalence.2020"))
  
}



# RM : This could probably be made into a much smaller for loop and automated by defining which sites you wish to see abundance for.
#MEAN ABUNDANCE Aa
#Mean Abundance 2019
#making the mean lice over time with sites.


#if you want to add a site, first, edit the focussitelist vector to include your desired site, in the code above (ctrl f %%%%)
# then copy/paste the code within the "1st FIND AND REPLACE"(use ctrl f 1st FIND AND REPLACE) boundaries and then change the 
    # site name using the ctrl f replace all function
# Then copy/paste the code within the "2nd FIND AND REPLACE" (use ctrl f 2nd FIND AND REPLACE) boundaries and then change the 
    # site name using the ctrl f replace all function



#subset the best2020 to get right species (fishbest2020). Then only take the columns that I want to show mean lice stages : stage sums per fish, date, j.date and grouped sites.
#subset to focus on ritchie, ritchie and bedwell north
#Then assign weekly intervals to dates 
#Now you can subset by site
#then you can use bootstrap resampling to get the means of sites by the assigned weekly intervals.
#create table for mean lci and uci of each site for each date
#Now you can plot each site's mean, lci and uci over time
#you can subplot this too. I think it will end up showing bedwell has lowest numbers, ritchie highest, cypre also high.
salmonlicebest2020<-subset(nobedwell, species == "chum" | species == "coho" | species == "chinook"| species == "sockeye" |species == "salmon")
#x# when creating this df it looks like weekly invterval does not exist. I went 
#I went back to check its integration and I can't seem to get it to work at all
#line 1051 its added to best 2020 but it seems like it does not add here
#this error propogates all the way to focussitelist inception
weeksitelice<-data.frame(salmonlicebest2020$date, salmonlicebest2020$j.date, salmonlicebest2020$weeklyintvl ,salmonlicebest2020$groupedsites,  
                         salmonlicebest2020$copsum, salmonlicebest2020$chalsum, salmonlicebest2020$motsum, salmonlicebest2020$Sum_all_lice)
names(weeksitelice)<-paste(c("date", "j.date", "weeklyintvl", "groupedsites", "copsum", "chalsum", "motsum", "Sum_all_lice"))
#%%%%
#*********************
#to add a site to the focussitelist, add | groupedsites == "desired site" to the code below for vector focusweeksitelice
focusweeksitelice<-subset(weeksitelice, groupedsites == "Cypre River" | groupedsites == "Ritchie Bay" | groupedsites == "North Meares" | groupedsites == "Bedwell Sound North")
focussitelist<- unique(focusweeksitelice$groupedsites)

# RM : This isn't actually used anywhere else, so we can forget about it and the error below
#rowcounts <- data.frame(weeklyintvl = numeric(0),
#                        site = numeric(0),
#                        mean = numeric(0),
#                        lci = numeric(0),
#                        uci = numeric(0))
#rowcounts$site <- factor(focusweeksitelice$groupedsites, levels=focussitelist)  

lci<- NULL
uci <- NULL
site.boot<-NULL
#put site.boot here
bigsiteboot<-rep(0, times =  length(focussitelist)*length(JDweeklyintervals))
weeklyintervalscol<- rep(JDweeklyintervals, times = length(focussitelist))
meanbootcol<-NULL
print(focussitelist)
#1st FIND AND REPLACE STARTS HERE##############################
#to put the means into vector

ritchiemeansdp<-data.frame(meancop = as.numeric(0), 
                           meanchal = as.numeric(0),
                           meanmot = as.numeric(0),
                           meantot = as.numeric(0))

ritchiesdp<-data.frame(sdcop = as.numeric(0), 
                       sdchal = as.numeric(0),
                       sdmot = as.numeric(0),
                       sdtot = as.numeric(0))


#for (i in 1 : length(focussitelist)){

#this subsets by site
#*******************
#Change the site by changing the focussitelist[#] below
loopfocussite<-focussitelist[3]
loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))



for (j in 1:length(JDweeklyintervals)) {
  
  #for each interval 1000 iterations are done
  #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
  tempsdp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
  for (r in 1:4) {
    ritchiemeansdp[j,r] <- mean(tempsdp[,(4+r)])
    ritchiesdp[j,r] <- (sd(tempsdp[,(4+r)]))/sqrt(length(tempsdp$date))
  }      
  
}   

#END OF 1st FIND AND REPLACE##############################

ritchiemeansdp<-data.frame(meancop = as.numeric(0), 
                           meanchal = as.numeric(0),
                           meanmot = as.numeric(0),
                           meantot = as.numeric(0))

ritchiesdp<-data.frame(sdcop = as.numeric(0), 
                       sdchal = as.numeric(0),
                       sdmot = as.numeric(0),
                       sdtot = as.numeric(0))


#for (i in 1 : length(focussitelist)){

#this subsets by site
loopfocussite<-focussitelist[3]
loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))



for (j in 1:length(JDweeklyintervals)) {
  
  #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
  tempsdp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
  for (r in 1:4) {
    ritchiemeansdp[j,r] <- mean(tempsdp[,(4+r)])
    ritchiesdp[j,r] <- sd(tempsdp[,(4+r)])/sqrt(length(tempsdp$date))
  }      
  
}   


cypremeansdp<-data.frame(meancop = as.numeric(0), 
                         meanchal = as.numeric(0),
                         meanmot = as.numeric(0),
                         meantot = as.numeric(0))

cypresdp<-data.frame(sdcop = as.numeric(0), 
                     sdchal = as.numeric(0),
                     sdmot = as.numeric(0),
                     sdtot = as.numeric(0))


#for (i in 1 : length(focussitelist)){

#this subsets by site
loopfocussite<-focussitelist[1]
loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))



for (j in 1:length(JDweeklyintervals)) {
  #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
  tempsdp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
  for (r in 1:4) {
    cypremeansdp[j,r] <- mean(tempsdp[,(4+r)])
    cypresdp[j,r] <- sd(tempsdp[,(4+r)])/sqrt(length(tempsdp$date))
  } }   




mearesmeansdp<-data.frame(meancop = as.numeric(0), 
                           meanchal = as.numeric(0),
                           meanmot = as.numeric(0),
                           meantot = as.numeric(0))

mearessdp<-data.frame(sdcop = as.numeric(0), 
                       sdchal = as.numeric(0),
                       sdmot = as.numeric(0),
                       sdtot = as.numeric(0))


#for (i in 1 : length(focussitelist)){

#this subsets by site
loopfocussite<-focussitelist[2]


loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(loopfocussite))



for (j in 1:length(JDweeklyintervals)) {
  
  #for each interval 1000 iterations are done
  #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
  tempsdp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
  for (r in 1:4) {
    mearesmeansdp[j,r] <- mean(tempsdp[,(4+r)])
    mearessdp[j,r] <- sd(tempsdp[,(4+r)])/sqrt(length(tempsdp$date))
  }      
  
}   

#overall



allmeansdp<-data.frame(meancop = as.numeric(0), 
                       meanchal = as.numeric(0),
                       meanmot = as.numeric(0),
                       meantot = as.numeric(0))

allsdp<-data.frame(sdcop = as.numeric(0), 
                   sdchal = as.numeric(0),
                   sdmot = as.numeric(0),
                   sdtot = as.numeric(0))

#something about the means is wrong. The total should be the highest bar. And the Sd is off kilter.
loopsfocusdata<-subset(focusweeksitelice, groupedsites == paste(focussitelist[1]) | groupedsites == paste(focussitelist[2]) |groupedsites == paste(focussitelist[3]))


for (j in 1:length(JDweeklyintervals)) {
  
  #for each interval 1000 iterations are done
  #Subset main data by 1 weeklyintvl to get samples of counts per fish for all dates of one interval.
  tempsdp<-subset(loopsfocusdata, weeklyintvl == JDweeklyintervals[j])
  for (r in 1:4) {
    allmeansdp[j,r] <- as.numeric(mean(tempsdp[,(4+r)]))
    allsdp[j,r] <- as.numeric(sd(tempsdp[,(4+r)]))/sqrt(length(tempsdp$date))
  }      
  
}   


########################### 2nd FIND AND REPLACE USING : cypre, ritchie, all, or meares (careful when replacing with all)

allsdp[is.na(allsdp)]<-0
allsdpl<-allsdp-allmeansdp
allsdpl[is.na(allsdpl)]<-0
allsdplneg<-allsdpl

allsdpu<-allsdp+allmeansdp
allsdpu[is.na(allsdpu)]<-0

allsdplneg[sapply(allsdplneg, is.numeric)] <- allsdplneg[sapply(allsdplneg, is.numeric)] * -1

check<-data.frame(allsdpl[1], allsdpu[1], allsdp[1], allmeansdp[1])


weeklyintsd<-rep(weeklyintervals, times = length(focussitelist))
weeklyintsd<-format(weeklyintsd, format = "%b %d %Y")
sitesdp<-rep(focussitelist, each = length(JDweeklyintervals))  
mpintvls<-rep(weeklyintervals, times = 4)  
tempsdpp<-data.frame(rbind(allmeansdp, allmeansdp, allmeansdp))
finalsd<-data.frame(sitesdp, tempsdpp, weeklyintsd)
View(finalsd)
#<-finalsd[c(-1, -6)]
#overallsd<-row.names(finalsd[,6])
par(mar=c(5.1, 4.1, 4.1, 2.1))
weeklyintervals<-format(weeklyintervals, format= "%b %d %y")
noyrweekintvl<-format(weeklyintervals, format = "%b %d" )
mp<- barplot(t(allmeansdp), ylim=c(0,33), yaxt = "n", 
             main = "Clayoquot Mean Lice Abundance per Fish", 
             col=c("darkgreen","dodgerblue","red","darkgray"), 
             cex.lab = 1.5, cex.axis = 2, beside = T,
             names.arg = noyrweekintvl, axes = TRUE, ylab ="Mean Lice per Fish")

segments( x0 = mp, t(allsdplneg), x1 = mp, t(allsdpu), lwd = 1)  # confidence intervals
arrows(x0 = mp, t(allsdplneg), x1 = mp, t(allsdpu), lwd = 0.75, angle = 90,
       code = 3, length = 0.05)

legend("topleft", bty = "n", col = c("darkgreen","dodgerblue","red","darkgray"), lwd = 3, cex = 1, legend = c("Copepodid", "Chalimus", "Motile", "Total"))
axis(side = 2, at = seq(from=0, to=33, by=3), las = 1)

dev.copy(png,'OutputFigures/ClayoquotMeanLiceAbundancePerFish_2020.png')
dev.off()
warnings()
################################################ END OF 2nd FIND AND REPLACE

#Cypre

cypresdp[is.na(cypresdp)]<-0
cypresdpl<-cypresdp-cypremeansdp
cypresdpl[is.na(cypresdpl)]<-0
cypresdplneg<-cypresdpl

cypresdpu<-cypresdp+cypremeansdp
cypresdpu[is.na(cypresdpu)]<-0

cypresdplneg[sapply(cypresdplneg, is.numeric)] <- cypresdplneg[sapply(cypresdplneg, is.numeric)] * -1

check<-data.frame(cypresdpl[1], cypresdpu[1], cypresdp[1], cypremeansdp[1])


weeklyintsd<-rep(weeklyintervals, times = length(focussitelist))
weeklyintsd<-format(weeklyintsd, format = "%b %d %Y")
sitesdp<-rep(focussitelist, each = length(JDweeklyintervals))  
mpintvls<-rep(weeklyintervals, times = 4)  
tempsdpp<-data.frame(rbind(cypremeansdp, cypremeansdp, cypremeansdp))
finalsd<-data.frame(sitesdp, tempsdpp, weeklyintsd)

#<-finalsd[c(-1, -6)]
#overcypresd<-row.names(finalsd[,6])
par(mar=c(5.1, 4.1, 4.1, 2.1))
weeklyintervals<-format(weeklyintervals, format= "%b %d %y")
noyrweekintvl<-format(weeklyintervals, format = "%b %d" )
mp<- barplot(t(cypremeansdp), ylim=c(0,40), yaxt = "n", 
             main = "Cypre River Mean Lice Abundance per Fish", 
             col=c("darkgreen","dodgerblue","red","darkgray"), 
             cex.lab = 1.5, cex.axis = 2, beside = T,
             names.arg = noyrweekintvl, axes = TRUE, ylab ="Mean Lice per Fish")

segments( x0 = mp, t(cypresdplneg), x1 = mp, t(cypresdpu), lwd = 1)  # confidence intervals
arrows(x0 = mp, t(cypresdplneg), x1 = mp, t(cypresdpu), lwd = 0.75, angle = 90,
       code = 3, length = 0.05)

legend("topleft", bty = "n", col = c("darkgreen","dodgerblue","red","darkgray"), lwd = 3, cex = 1, legend = c("Copepodid", "Chalimus", "Motile", "Total"))
axis(side = 2, at = seq(from=0, to=40, by=5), las = 1)


dev.copy(png,'OutputFigures/CypreRiverMeanLiceAbundancePerFish_2020.png')
dev.off()
dev.off()

################################################


#Ritchie



ritchiesdp[is.na(ritchiesdp)]<-0
ritchiesdpl<-ritchiesdp-ritchiemeansdp
ritchiesdpl[is.na(ritchiesdpl)]<-0
ritchiesdplneg<-ritchiesdpl

ritchiesdpu<-ritchiesdp+ritchiemeansdp
ritchiesdpu[is.na(ritchiesdpu)]<-0

ritchiesdplneg[sapply(ritchiesdplneg, is.numeric)] <- ritchiesdplneg[sapply(ritchiesdplneg, is.numeric)] * -1

check<-data.frame(ritchiesdpl[1], ritchiesdpu[1], ritchiesdp[1], ritchiemeansdp[1])


weeklyintsd<-rep(weeklyintervals, times = length(focussitelist))
weeklyintsd<-format(weeklyintsd, format = "%b %d %Y")
sitesdp<-rep(focussitelist, each = length(JDweeklyintervals))  
mpintvls<-rep(weeklyintervals, times = 4)  
tempsdpp<-data.frame(rbind(ritchiemeansdp, ritchiemeansdp, ritchiemeansdp))
finalsd<-data.frame(sitesdp, tempsdpp, weeklyintsd)

#<-finalsd[c(-1, -6)]
#overritchiesd<-row.names(finalsd[,6])
par(mar=c(5.1, 4.1, 4.1, 2.1))
weeklyintervals<-format(weeklyintervals, format= "%b %d %y")
noyrweekintvl<-format(weeklyintervals, format = "%b %d" )
mp<- barplot(t(ritchiemeansdp), ylim=c(0,26), yaxt = "n", 
             main = "Ritchie Bay Mean Lice Abundance per Fish", 
             col=c("darkgreen","dodgerblue","red","darkgray"), 
             cex.lab = 1.5, cex.axis = 2, beside = T,
             names.arg = noyrweekintvl, axes = TRUE, ylab ="Mean Lice per Fish")

segments( x0 = mp, t(ritchiesdplneg), x1 = mp, t(ritchiesdpu), lwd = 1)  # confidence intervals
arrows(x0 = mp, t(ritchiesdplneg), x1 = mp, t(ritchiesdpu), lwd = 0.75, angle = 90,
       code = 3, length = 0.05)

legend("topleft", bty = "n", col = c("darkgreen","dodgerblue","red","darkgray"), lwd = 3, cex = 1, legend = c("Copepodid", "Chalimus", "Motile", "Total"))
axis(side = 2, at = seq(from=0, to=26, by=2), las = 1)


dev.copy(png,'OutputFigures/RitchieBayMeanLiceAbundancePerFish_2020.png')
dev.off()
dev.off()

###############################################


#North Meares


mearessdp[is.na(mearessdp)]<-0
mearessdpl<-mearessdp-mearesmeansdp
mearessdpl[is.na(mearessdpl)]<-0
mearessdplneg<-mearessdpl

mearessdpu<-mearessdp+mearesmeansdp
mearessdpu[is.na(mearessdpu)]<-0

mearessdplneg[sapply(mearessdplneg, is.numeric)] <- mearessdplneg[sapply(mearessdplneg, is.numeric)] * -1

check<-data.frame(mearessdpl[1], mearessdpu[1], mearessdp[1], mearesmeansdp[1])


weeklyintsd<-rep(weeklyintervals, times = length(focussitelist))
weeklyintsd<-format(weeklyintsd, format = "%b %d %Y")
sitesdp<-rep(focussitelist, each = length(JDweeklyintervals))  
mpintvls<-rep(weeklyintervals, times = 4)  
tempsdpp<-data.frame(rbind(mearesmeansdp, mearesmeansdp, mearesmeansdp))
finalsd<-data.frame(sitesdp, tempsdpp, weeklyintsd)

#<-finalsd[c(-1, -6)]
#overbedwellsd<-row.names(finalsd[,6])
par(mar=c(5.1, 4.1, 4.1, 2.1))
weeklyintervals<-format(weeklyintervals, format= "%b %d %y")
noyrweekintvl<-format(weeklyintervals, format = "%b %d" )
mp<- barplot(t(mearesmeansdp), ylim=c(0,10), yaxt = "n", 
             main = "North Meares Mean Lice Abundance per Fish", 
             col=c("darkgreen","dodgerblue","red","darkgray"), 
             cex.lab = 1.5, cex.axis = 2, beside = T,
             names.arg = noyrweekintvl, axes = TRUE, ylab ="Mean Lice per Fish")

segments( x0 = mp, t(mearessdplneg), x1 = mp, t(mearessdpu), lwd = 1)  # confidence intervals
arrows(x0 = mp, t(mearessdplneg), x1 = mp, t(mearessdpu), lwd = 0.75, angle = 90,
       code = 3, length = 0.05)

legend("topleft", bty = "n", col = c("darkgreen","dodgerblue","red","darkgray"), lwd = 3, cex = 1, legend = c("Copepodid", "Chalimus", "Motile", "Total"))
axis(side = 2, at = seq(from=0, to=10, by=2), las = 1)



dev.copy(png,'OutputFigures/NorthMearesMeanLiceAbundancePerFish_2020.png')
dev.off()
dev.off()


################################################################






axis(side = 1, at = seq(1, 68, 4), labels = noyrweekintvl) 
length(mp)
#cypre
#bedwell
#ritchie
#all
#mtext("Mean Lice Abundance per Fish", side = 2, line = 2.4)

##WHY ARE THE Ritchie PLOTS THE SAME AS THE BEDWELLL PLOTS!?
####################
colnames(Bedwellweektable)=c("Mean Total Lice","Mean Motile","Mean Chalimus","Mean Copepodid")
rownames(groupedstagesdata)= datesforstages
stagesmatrix=matrix(groupedstagesdata)
####################

# Axis labels and titles
axis(side = 2, at = seq(from=0, to=8, by=2), las = 1)
mtext("Date", side = 1, line = 2.7)
mtext("Count", side = 2, line = 2.4)

####################

# RM so no weeklyliceloctable... I suggest just making your own from scratch-ola if the data it would present is not to be found above.
#
weeklyliceloctable <- data.frame(meancop = as.numeric(0), 
                         meanchal = as.numeric(0),
                         meanmot = as.numeric(0),
                         meantot = as.numeric(0))

weeklycopf<-subset(weeklyliceloctable, assignstage == "copepodid")
weeklychalf<-subset(weeklyliceloctable, assignstage == "chalimus")
weeklymotilef<-subset(weeklyliceloctable, assignstage == "motile")
weeklytotalf<-subset(weeklyliceloctable, assignstage == "total")
weektablef<-cbind(weeklycopf, weeklychalf[4:6], weeklymotilef[4:6], weeklytotalf[4:6])
names(weektablef)[4:15]<-paste(c("copmean", "coplci", "copuci", "chalmean", "chbedwellci", "chaluci", "motmean", "motlci", "motuci", "totalmean", "totallci", "totaluci"))
bpintvls<-weektablef$weeklyintvl
weektablef1<-weektablef[c(-1,-2,-3,-5,-6,-8,-9,-11,-12,-14,-15)]
weektableflci<-weektablef[c(-1,-2,-3,-4,-6,-7,-9,-10,-12,-13,-15)]
weektablefuci<-weektablef[c(-1,-2,-3,-4,-5,-7,-8,-10,-11,-13,-14)]
weektableflci[is.na(weektableflci)]<-0
weektablefuci[is.na(weektablefuci)]<-0



#bpintvls1<-as.Date(bpintvls, origin = as.Date("1970-01-01"), format = "%b %d %y")
par(mar=c(10,5,4,2))
mp <- barplot(t(weektablef1), ann=F, yaxt="n", ylim=c(0,8), 
              main = "Bedwell Sound Mean Lice Abundance per Fish", col=c("darkgreen","dodgerblue","red","darkgray"), 
              beside = T, names.arg = bpintvls, axes = TRUE)
segments( x0 = mp, t(weektableflci), x1 = mp, t(weektablefuci), lwd = 2)  # confidence intervals
arrows(x0 = mp, t(weektableflci), x1 = mp, t(weektablefuci), lwd = 1, angle = 90,
       code = 3, length = 0.05)


weektablef<-as.numeric(as.character(weektablef[2:5]))
weektablef<-as.character(weektablef[1])
xmin<-min(weektablef1)

legend("topleft", bty = "n", col = c("darkgreen","dodgerblue","red","darkgray"), lwd = 3, cex = 1, legend = c("Copepodid", "Chalimus", "Motile", "All"))
axis(side = 2, at = seq(from=0, to=8, by=2), las = 1)
axis(1, at=match(seq(as.Date(x_min), x_max, "years"),index(df))*(1+space),
     labels = format(seq(as.Date(x_min), x_max, "years"),"%Y"),lwd=0)

axis(side = 1, at = length(bpintvls), labels = bpintvls)
mtext("Mean Lice Abundance per Fish", side = 2, line = 2.4)

dev.copy(png,'OutputFigures/MeanLiceAbundancePerFish_2020.png')
dev.off()


####making a data table with min and max lice for each species with respective date and location
colnames(best2020)
chum_abun_cypre <- data.frame(subset(best2020, species == "chum"& location == "Cypre River"))
print(max(chum_abun_cypre$Sum_all_lice))
#now input the max into the next df creation
chum_abun_cypre <- data.frame(subset(chum_abun_cypre, Sum_all_lice == "11"))
View(chum_abun_cypre)


library(dplyr) 

chum_abun <- filter(best2020, species == "chum" & location == "Cypre River")

chinook_abun

coho_abun

pink_abun

sock_abun
