#import data from an R project I have titled "Nepal disability and VA"
library(tidyverse)
NEW2VIEWIIPilotFinal_DATA_2019.09.03_1817 <- read.csv("~/Desktop/Gap Year/Gap Year Projects/Nepal VA and Disability/NEW2VIEWIIPilotFinal_DATA_2019-09-03_1817.csv", row.names=1)

pilotfinal <- NEW2VIEWIIPilotFinal_DATA_2019.09.03_1817
View(pilotfinal)

#explore the data
str(pilotfinal)
dim(pilotfinal)

#disability varibles of interest: dis_see, dis_hear, dis_walk, dis_rem, dis_wash, dis_comm
#columns with VA data: six60_od, ph_six60_od, six18_od, ph_six18_od, six9_od, ph_six9_od, six60_os, ph_six60_os, 
#six18_os, ph_six18_os, six9_os, ph_six9_os
#get some information on these
xtabs(data = pilotfinal, ~six60_od, addNA = TRUE) #can you see the line? 0 = no, 1 = yes, 2 = ?, NA = 1646
xtabs(data = pilotfinal, ~ph_six60_od, addNA = TRUE) # 0 = no, 1 = yes, NA = 5569, the n for this = number of 0 for six60_od
xtabs(data = pilotfinal, ~six18_od, addNA = TRUE) # 0 = no, 1 = yes, 0 --> ph_six18_od ; 1 --> six9_od
xtabs(data = pilotfinal, ~ph_six18_od, addNA = TRUE) #etc.
xtabs(data = pilotfinal, ~six9_od, addNA = TRUE)
xtabs(data = pilotfinal, ~ph_six9_od, addNA = TRUE)
xtabs(data = pilotfinal, ~six60_os, addNA = TRUE) # 0 = no, 1 = yes, 2 = ?, NA = 1646
xtabs(data = pilotfinal, ~ph_six60_os, addNA = TRUE)
xtabs(data = pilotfinal, ~six18_os, addNA = TRUE)
xtabs(data = pilotfinal, ~ph_six18_os, addNA = TRUE)
xtabs(data = pilotfinal, ~six9_os, addNA = TRUE)
xtabs(data = pilotfinal, ~ph_six9_os, addNA = TRUE)

#creating new dataframe with only the values of interest. split by eye for analysis as unsure how to present both otherwise
disvaOD <- pilotfinal %>%
  select(name, dis_see, dis_hear, dis_walk, dis_rem, dis_wash, dis_comm, six60_od, ph_six60_od, six18_od, ph_six18_od, 
         six9_od, ph_six9_od)

disvaOS <- pilotfinal %>%
  select(name, dis_see, dis_hear, dis_walk, dis_rem, dis_wash, dis_comm, six60_os, ph_six60_os, six18_os, ph_six18_os, 
         six9_os, ph_six9_os)
  
#change disability to factorial
disvaODfact <- disvaOD %>%
 mutate(dis_see_fact=as.factor(disvaOD$dis_see),
  #check that what I did worked
  #xtabs(data=disvaODfact, ~dis_see + dis_see_fact, addNA=TRUE)
  #class(disvaODfact$dis_see_fact), it did so convert the rest of the variables to factorial
  dis_hear_fact=as.factor(disvaOD$dis_hear),
  dis_walk_fact=as.factor(disvaOD$dis_walk),
  dis_rem_fact=as.factor(disvaOD$dis_rem),
  dis_wash_fact=as.factor(disvaOD$dis_wash),
  dis_comm_fact=as.factor(disvaOD$dis_comm)) 

#labelling the levels of the above factors
disfactlabels <- c("no", "some", "alot", "unable")
levels(disvaODfact$dis_see_fact) = disfactlabels  #check my work with levels(disvaODfact$dis_see_fact)
levels(disvaODfact$dis_hear_fact) = disfactlabels
levels(disvaODfact$dis_walk_fact) = disfactlabels
levels(disvaODfact$dis_rem_fact) = disfactlabels
levels(disvaODfact$dis_wash_fact) = disfactlabels
levels(disvaODfact$dis_comm_fact) = disfactlabels 

#now to change the binary for each VA to BCVA for each eye
disvaODfact2 <- disvaODfact %>%
  mutate(bcva=as.character(case_when(ph_six60_od == 0 ~ "?HM",                 #if cannot see 6/60 = ? HM
                                    ph_six60_od == 1 ~ "PH6/60",               #if can see 6/60 w/ PH = PH6/60
                                    ph_six18_od == 0 & six60_od == 1 ~ "6/60", #if cannot see PH6/18 and can see 6/60 = 6/60
                                    ph_six18_od == 1 ~ "PH6/18",               # if can see 6/18 w/ PH = PH6/18
                                    ph_six9_od == 0 & six18_od == 1 ~ "6/18",  #if cannot see PH6/9 and can see 6/18 = 6/18
                                    ph_six9_od == 1 ~ "PH6/9",                 # if can see 6/9 with PH = PH6/9
                                    six9_od == 1 ~ "6/9",                      #if can see 6/9 = 6/9
                                    )))
#Now I have one new column that has the BCVA for each participant named bcva. but it is character and not factor

disvaODfact2$bcva <- factor(disvaODfact2$bcva,
                            levels = c("?HM", "PH6/60", "6/60", "PH6/18", "6/18", "PH6/9", "6/9"))
  #confirm work with levels(disvaODfact2$bcva) and class(disvaODfact2$bcva). now xtabs and plots, 
  #I think % answering no is important

bcvasee <- xtabs(data=disvaODfact2, ~bcva + dis_see_fact)  #?HM only 55/178 individuals answered something other than "no" overall 463 answered other than "no"
bcvahear <- xtabs(data=disvaODfact2, ~bcva + dis_hear_fact) #185 answered other than "no"
bcvawalk <- xtabs(data=disvaODfact2, ~bcva + dis_walk_fact) #362 answered other than "no"
bcvarem <- xtabs(data=disvaODfact2, ~bcva + dis_rem_fact) #206 ""
bcvawash <- xtabs(data=disvaODfact2, ~bcva + dis_wash_fact) #87
bcvacomm <- xtabs(data=disvaODfact2, ~bcva + dis_comm_fact) #86
#could not figure out how to add the number of responses to the disability questionnaire that != "no" 
#so I had to do it manually.

#I can do some more analysis on this but wanted to check my work prior to repeating everything for OS and doing some stats
#I was thinking of chi squared or factor analysis but I honestly do not know that much about statistics.


