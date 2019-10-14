#Using pps and community selection. PPS = probability proportional to size

install.packages("pps")
library(pps)
library(tidyverse)

data(calif) #data comes with pps
data(califcty)

#working through some exercises detialed in this document: 
#http://www.imsbio.co.jp/RGM-files/R_CC/download/pps/inst/doc/pps-ug.pdf

#PPS with replacement
county <- calif[calif$county == 1,]
countysample <- county[ppswr(county$population,5),] #the 5 indicates how many units to select

#the function returns the indices of the selected units in the data frame. by subsetting the function within the datframe
#you can get a frame of just the selected units 


#A visiting epi PhD from UCLA overheard our meeting and came up with the following code for random selection of communities

#draw initial sample
sample <- califcty[ppswr(census$population,20),] # select 20 units
view(sample)  #usually there will be duplicates

for(i in 1:50){
  
  #identify number of duplicates on your current draw
  dupes <- length(sample$county[duplicated(sample$county)])
  
  if(dupes>0){
    #new random draw to replace dupes, only drawing from the dupes
    sample_2<-califcty[ppswr(census$population,dupes),]
    
    #remove dupes from first draw
    sample_nodups<- sample[!duplicated(sample$county),]
    
    #replace dupes with new draw
    sample <- rbind(sample_nodups,sample_2)
    
  } else { break }
}

view(sample)
duplicated(sample) #no duplicates
