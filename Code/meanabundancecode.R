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

#Last line in this chunk assembles the stages-tables to give the SUM of all lice stages by groupedsites

# FOR CRITTY 
#EXAMPLE OF HO TO GET SD AND SE FOR SUM TABLES
Motlicetab<-do.call(data.frame, aggregate(motsum~groupedsites, data = best2020, function(x) c(sum = sum(x), sd = sd(x))))
#install.packages('pracma')
require("pracma")
Motlicetab$SE <- nthroot(Motlicetab$motsum.sd, length(Motlicetab$motsum.sd))
#####

Attlicetab<-aggregate(attachedsum~groupedsites, data = best2020, sum)
Coplicetab<-aggregate(copsum~groupedsites, data = best2020, sum)
Challicetab<-aggregate(chalsum~groupedsites, data = best2020, sum)
alltab<-aggregate(Sum_all_lice~groupedsites, data = best2020, sum)
# This is the final table for plots of sums! :)))
licetable<-data.frame(Motlicetab, Coplicetab[2], Challicetab[2], alltab[2], Attlicetab[2])

#RM: if you want means
Motlicetab.mean<-aggregate(motsum~groupedsites, data = best2020, mean)
Attlicetab.mean<-aggregate(attachedsum~groupedsites, data = best2020, mean)
Coplicetab.mean<-aggregate(copsum~groupedsites, data = best2020, mean)
Challicetab.mean<-aggregate(chalsum~groupedsites, data = best2020, mean)
alltab.mean<-aggregate(Sum_all_lice~groupedsites, data = best2020, mean)
# This is the final table for plots of means! :)))
licetable.mean<-data.frame(Motlicetab.mean, Coplicetab.mean[2], Challicetab.mean[2], alltab.mean[2], Attlicetab.mean[2])

# This is a table for means by site and date
Motlicetab.mean.site.date<-do.call(data.frame, aggregate(motsum~groupedsites + date, data = best2020, function(x) c(mean = mean(x), sd = sd(x))))
require("pracma")
Motlicetab.mean.site.date$mot.SE <- nthroot(Motlicetab$motsum.sd, length(Motlicetab$motsum.sd))

Attlicetab.mean.site.date<-do.call(data.frame, aggregate(attachedsum~groupedsites + date, data = best2020, function(x) c(mean = mean(x), sd = sd(x))))
require("pracma")
Attlicetab.mean.site.date$att.SE <- nthroot(Motlicetab$motsum.sd, length(Motlicetab$motsum.sd))

Coplicetab.mean.site.date<-do.call(data.frame, aggregate(copsum~groupedsites+ date, data = best2020, function(x) c(mean = mean(x), sd = sd(x))))
require("pracma")
Coplicetab.mean.site.date$cop.SE <- nthroot(Motlicetab$motsum.sd, length(Motlicetab$motsum.sd))

Challicetab.mean.site.date<-do.call(data.frame, aggregate(chalsum~groupedsites+ date, data = best2020, function(x) c(mean = mean(x), sd = sd(x))))
require("pracma")
Challicetab.mean.site.date$chal.SE <- nthroot(Motlicetab$motsum.sd, length(Motlicetab$motsum.sd))

alltab.mean.site.date<-do.call(data.frame, aggregate(Sum_all_lice~groupedsites+ date, data = best2020, function(x) c(mean = mean(x), sd = sd(x))))
require("pracma")
alltab.mean.site.date$all.SE <- nthroot(Motlicetab$motsum.sd, length(Motlicetab$motsum.sd))

# This is the final table for plots of means! :)))
licetable.mean.site.date<-data.frame(Motlicetab.mean.site.date[1:3], Coplicetab.mean.site.date[3], Challicetab.mean.site.date[3], alltab.mean.site.date[3], Attlicetab.mean.site.date[3])
# The SD table
licetable.sd.site.date<-data.frame(Motlicetab.mean.site.date[c(1:2, 4)], Coplicetab.mean.site.date[4], Challicetab.mean.site.date[4], alltab.mean.site.date[4], Attlicetab.mean.site.date[4])
# The SE table
licetable.se.site.date<-data.frame(Motlicetab.mean.site.date[c(1:2,5)], Coplicetab.mean.site.date[5], Challicetab.mean.site.date[5], alltab.mean.site.date[5], Attlicetab.mean.site.date[5])

View(licetable.mean.site.date)

View(licetable.sd.site.date)

#ensure all is numeric
#means
colnames(licetable.mean.site.date)
licetable.mean.site.date$motsum.mean<-as.numeric(licetable.mean.site.date$motsum.mean)
licetable.mean.site.date$copsum.mean<-as.numeric(licetable.mean.site.date$copsum.mean)
licetable.mean.site.date$chalsum.mean<-as.numeric(licetable.mean.site.date$chalsum.mean)
licetable.mean.site.date$Sum_all_lice.mean<-as.numeric(licetable.mean.site.date$Sum_all_lice.mean)
licetable.mean.site.date$attachedsum.mean<-as.numeric(licetable.mean.site.date$attachedsum.mean)
licetable.mean.site.date$date<-as.Date(licetable.mean.site.date$date, origin = as.Date("1970-01-01"))
licetable.mean.site.date$date<-format( licetable.mean.site.date$date, format = "%b %d %y")

#sd
colnames(licetable.sd.site.date)
licetable.sd.site.date$motsum.sd<-as.numeric(licetable.sd.site.date$motsum.sd)
licetable.sd.site.date$copsum.sd<-as.numeric(licetable.sd.site.date$copsum.sd)
licetable.sd.site.date$chalsum.sd<-as.numeric(licetable.sd.site.date$chalsum.sd)
li

?geom_bar
## plot
#Ritchie
colnames(licetable.mean.site.date)
RitchieSub <- subset(licetable.mean.site.date, groupedsites == "Ritchie Bay" )
weeklyintervals<-format(RitchieSub$date, format= "%b %d %y")
noyrweekintvl<-format(RitchieSub$date, format = "%b %d" )
View(RitchieSub)
#ggplot attempt
RitchieSub <- subset(RitchieSub,select= -c(groupedsites))
RitchieSub$motsum.mean <- as.numeric(RitchieSub$motsum.mean)
RitchieSub$copsum.mean<- as.numeric(RitchieSub$copsum.mean)
RitchieSub$Sum_all_lice.mean <- as.numeric(RitchieSub$Sum_all_lice.mean)
RitchieSub$attachedsum.mean<- as.numeric(RitchieSub$attachedsum.mean)
RitchieSub$date<- as.Date(RitchieSub$date, format = "%b/%d/%Y")
colnames(RitchieSub)

RitchieSubDos <- tidyr::pivot_longer(RitchieSub, cols=c("motsum.mean","copsum.mean","chalsum.mean","Sum_all_lice.mean","attachedsum.mean"), names_to='variable', 
                    values_to="value")
head(RitchieSubDos)

ggplot(RitchieSubDos, aes(x=date, y=value, fill=variable)) + 
geom_bar(stat = 'identity', position = 'dodge')+
  labs(x = "Date", y = "Mean Abundance") + 
  theme_classic()



par(mar=c(5.1, 4.1, 4.1, 2.1))


mp<- barplot(t(licetable.mean.site.date), ylim=c(0,50), yaxt = "n", 
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
## end plot


dir.out <- "C:/Users/Rowen/OneDrive/Desktop/github/CCFS-Sea-Lice-database/Data/OutputData"

setwd(dir.out)
write.csv(licetable.mean.site.date, paste(yr, "mean_lice_by_site_date.csv", sep = "_"))






