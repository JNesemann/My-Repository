library(pps)
library(tidyverse)
library(skimr)

communities_list <- read_excel("List of all communities.xlsx")

#Started a project with the first sheet of the excel with all of the included communities
skim(communities_list)

#clean 
communities_clean <- communities_list %>%
  rename(healthpost = `Health Post`,
         districts = microred,                       #Peru organization goes Regions > Provinces > Districts,
         com_name = `Town name`,                     #we are interested in the Alto Amazonas province and its subdistricts
         indigenous = `Indigenous?`,                 
         n_fam = `N families`,
         n_houses = `N houses`,
         oneto9 = `total 1-9 year olds`,
         pop = `total pop`) %>%
  select(healthpost, 
         districts,
         com_name,
         indigenous,
         n_fam,
         n_houses,
         oneto9,
         pop)

#now to select 20 communities from all the included communities from Alto Amazonas

communitysample <- communities_clean[ppswr(communities_clean$pop, 20),] #selecting 20

for(i in 1:50) {

dups <- length(communitysample$com_name[duplicated(communitysample$com_name)]) #ID number of duplicates on current draw

   if(dups>0){
     
     #new random sample to replace dupes
     sample2 <- communities_clean[ppswr(communities_clean$pop, dups), ] 
     
     #remove duplicates from draw
     sample_nodups <- communitysample[!duplicated(communitysample$com_name), ]
     
     #replace dupes with new communities
     communitysample <- rbind(sample2, sample_nodups)
     
   } else { break }
}

view(communitysample)
duplicated(communitysample) #no duplicates
print(communitysample)

  sum(communitysample$oneto9)
  sum(communitysample$pop)    

#I can select just from Balsapuerto

Balsapuerto <- communities_clean %>%
  filter(districts == "Balsapuerto")

skim(Balsapuerto)

Balsapuertosample <- Balsapuerto[ppswr(Balsapuerto$pop, 20),] #selecting 20

for(i in 1:50) {
  dups <- length(Balsapuertosample$com_name[duplicated(Balsapuertosample$com_name)])
  if(dups>0){
    sample2 <- Balsapuerto[ppswr(Balsapuerto$pop, dups), ] 
    sample_nodups <- Balsapuertosample[!duplicated(Balsapuertosample$com_name), ]
    Balsapuertosample <- rbind(sample2, sample_nodups)
  } else { break }
}
duplicated(Balsapuertosample) #no duplicates
print(Balsapuertosample)
view(Balsapuertosample)

sum(Balsapuertosample$oneto9) 
sum(Balsapuertosample$pop)    

#I can choose more or do the selection process based on the health post instead of communities
#if you'd like me to, will contact Sra. Muñoz this week in regards to the communities