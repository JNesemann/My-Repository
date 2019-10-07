#import data from an R project I have titled "Nepal disability and VA" I am using a seperate project folder for this project
library(tidyverse)
library(readr)
#using read_csv from an existing project and naming it appropriately the first time around
pilotfinal <- read_csv("NEW2VIEWIIPilotFinal_DATA_2019-09-03_1817.csv")
View(pilotfinal)

#explore the data
str(pilotfinal)
dim(pilotfinal)
#downloading and installing skimer
install.packages("skimr")
library(skimr)

glimpse(pilotfinal)
skim(pilotfinal) #I like skim a lot more, cool how it groups everything by data type

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

# JK: Try this; please go through and do line-by-line so you understand (or I guess actually, do pipe-by-pipe)--Done JN
# JK: I did your bcva a little differently, and call it visionscreen because it's not really bcva
disva <- pilotfinal %>%
  # JK: Need to remove the first underscore because later in separate command you are telling R to separate at the underscore
  rename(phsix60_os=ph_six60_os,
         phsix18_os=ph_six18_os,
         phsix9_os=ph_six9_os,
         phsix60_od=ph_six60_od,
         phsix18_od=ph_six18_od,
         phsix9_od=ph_six9_od) %>%
  select(id, date_screen, name, dis_see, dis_hear, dis_walk, dis_rem, dis_wash, dis_comm, six60_os, phsix60_os, six18_os, phsix18_os,
         six9_os, phsix9_os, six60_od, phsix60_od, six18_od, phsix18_od, six9_od, phsix9_od) %>%
  # JK: general pattern for gather always has 3 elements: field, value, range of values you want to put in a single column
  # JK: I normally use the actual word field, though modify it in cases like this
  gather(fieldeye, value, six60_os:phsix9_od) %>%  #JN: view(disva). gather collapses all visonscreen column into one column = fieldeye 
  #JN: and 1 or 0 for each fieldeye is recorded in the value column. each individual's name is repeated 12 times: once for each visscreen column
  separate(fieldeye, into=c("field", "eye"), sep="_") %>%  # JN: now fieldeye is seperated into VA and OD/OS, still 12 rows per individual
  spread(field, value, convert=TRUE) %>% #spreads the data out converting field and the associated value back into columns 
  #but now with two seperate rows per pt, OD and OS
  mutate(visionscreen=case_when(six60==0 & phsix60==0 ~ 1,  #make an intiger outcome for every possibility of the vision screen in new col.
                                six60==0 & phsix60==1 & phsix18==0 ~ 2, 
                                six60==0 & phsix60==1 & phsix18==1 & phsix9==0 ~ 3, 
                                six60==0 & phsix60==1 & phsix18==1 & phsix9==1 ~ 4,
                                six60==1 & six18==0 & phsix18==0 ~ 5,
                                six60==1 & six18==0 & phsix18==1 & phsix9==0 ~ 6,
                                six60==1 & six18==0 & phsix18==1 & phsix9==1 ~ 7,
                                six60==1 & six18==1 & six9==0 & phsix9==0 ~ 8,
                                six60==1 & six18==1 & six9==0 & phsix9==1 ~ 9,
                                six60==1 & six18==1 & six9==1 ~ 10,
                                TRUE ~ NA_real_)) %>%
  gather(fielddis, value, dis_see:dis_comm) %>%  #same process as outlined above but gathering all dis data into one col (fielddis)
  #with responses stored in value
  separate(fielddis, into=c("field", "distype"), sep="_") %>% #JN: seperating fielddis (dis_see) into field (dis) and distype (see)
  spread(field, value, convert=TRUE) %>% #data from field ("dis") is now its own column with values from value
  rename(dis_score=dis) %>%
  mutate(dis_score= recode_factor(dis_score,  `1`="no", `2`="some", `3`="alot", `4`="unable", .ordered=TRUE)) 
#recode_factor in dplyer is a nice new way to recode values into factors, seems easier than as.factor

xtabs(data=disva, ~visionscreen,addNA=TRUE)

# JK: I use ts as a troubleshoot object that I don't really care about; 
# JK: I will use it multiple times and overwrite the previous ones just to figure out what is going on
ts <- disva %>% filter(visionscreen==999)
# JK: The ones not getting classified are where the pinhole acuity is missing; this was a Redcap branching logic problem early in the study

# JK: The following is mostly included above, so I just inactivated for now...
# #change disability to factorial
# disvaODfact <- disvaOD %>%
#  mutate(dis_see_fact=as.factor(disvaOD$dis_see),
#   #check that what I did worked
#   #xtabs(data=disvaODfact, ~dis_see + dis_see_fact, addNA=TRUE)
#   #class(disvaODfact$dis_see_fact), it did so convert the rest of the variables to factorial
#   dis_hear_fact=as.factor(disvaOD$dis_hear),
#   dis_walk_fact=as.factor(disvaOD$dis_walk),
#   dis_rem_fact=as.factor(disvaOD$dis_rem),
#   dis_wash_fact=as.factor(disvaOD$dis_wash),
#   dis_comm_fact=as.factor(disvaOD$dis_comm)) 
# 
# #labelling the levels of the above factors
# disfactlabels <- c("no", "some", "alot", "unable")
# levels(disvaODfact$dis_see_fact) = disfactlabels  #check my work with levels(disvaODfact$dis_see_fact)
# levels(disvaODfact$dis_hear_fact) = disfactlabels
# levels(disvaODfact$dis_walk_fact) = disfactlabels
# levels(disvaODfact$dis_rem_fact) = disfactlabels
# levels(disvaODfact$dis_wash_fact) = disfactlabels
# levels(disvaODfact$dis_comm_fact) = disfactlabels 
# 
# #now to change the binary for each VA to BCVA for each eye
# disvaODfact2 <- disvaODfact %>%
#   mutate(bcva=as.character(case_when(ph_six60_od == 0 ~ "?HM",                 #if cannot see 6/60 = ? HM
#                                     ph_six60_od == 1 ~ "PH6/60",               #if can see 6/60 w/ PH = PH6/60
#                                     ph_six18_od == 0 & six60_od == 1 ~ "6/60", #if cannot see PH6/18 and can see 6/60 = 6/60
#                                     ph_six18_od == 1 ~ "PH6/18",               # if can see 6/18 w/ PH = PH6/18
#                                     ph_six9_od == 0 & six18_od == 1 ~ "6/18",  #if cannot see PH6/9 and can see 6/18 = 6/18
#                                     ph_six9_od == 1 ~ "PH6/9",                 # if can see 6/9 with PH = PH6/9
#                                     six9_od == 1 ~ "6/9",                      #if can see 6/9 = 6/9
#                                     )))
# #Now I have one new column that has the BCVA for each participant named bcva. but it is character and not factor
# 
# disvaODfact2$bcva <- factor(disvaODfact2$bcva,
#                             levels = c("?HM", "PH6/60", "6/60", "PH6/18", "6/18", "PH6/9", "6/9"))
#   #confirm work with levels(disvaODfact2$bcva) and class(disvaODfact2$bcva). now xtabs and plots, 
#   #I think % answering no is important

# JK: Try this instead
table1 <- disva %>%
  group_by(distype, dis_score) %>%
  summarize(meanvisionscreen=mean(visionscreen, na.rm=TRUE),
            totalvisionscreen=sum(!is.na(visionscreen))) #total all visscreen responses for each group
#JN: higher vision screen score = better vision, as dis score increases mean vision screen downtrends, relationship seems strongest 
# with see and walk

plot1 <- ggplot(data=table1, aes(x=dis_score, y=distype)) +
  geom_tile(aes(fill = meanvisionscreen), colour = "white") + #tiles each of which correpond to meanvisionscreen for each group
  scale_fill_gradient(low = "white", high = "steelblue") +    #assign color gradient to fill the tiles
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot1
plot2 <- ggplot(data=disva, aes(x=dis_score, y=visionscreen, )) +
  geom_point(position="jitter", alpha=0.1) + #adds a small amount of random variation (alpha) around each data point
  facet_grid(distype ~ .) #facets on combo of two variables, (var1 ~ var2), can use "." instead of varname to no facet in the row or column dimension
plot2
plot3 <- ggplot(data=disva, aes(x=dis_score, y=visionscreen, )) +
  geom_boxplot() +
  facet_grid(distype ~ .) #JN: creates boxplots faceted in rows by distype
plot3 

# JK: Testing statistical significance, just as a first pass, not accounting for clustering...
# For "see"
addmargins(xtabs(data=filter(disva, distype=="see" & dis_score %in% c("no", "some", "alot")), ~visionscreen+dis_score))
kruskal.test(visionscreen ~ dis_score, data=filter(disva, distype=="see" & dis_score %in% c("no", "some", "alot")))
# For "hear"
addmargins(xtabs(data=filter(disva, distype=="hear" & dis_score %in% c("no", "some", "alot")), ~visionscreen+dis_score))
kruskal.test(visionscreen ~ dis_score, data=filter(disva, distype=="hear" & dis_score %in% c("no", "some", "alot")))

#for "walk"
addmargins(xtabs(data=filter(disva, distype=="walk" & dis_score %in% c("no", "some", "alot")), ~visionscreen+dis_score))
kruskal.test(visionscreen ~ dis_score, data=filter(disva, distype=="walk" & dis_score %in% c("no", "some", "alot")))

#for "rem"
addmargins(xtabs(data=filter(disva, distype=="rem" & dis_score %in% c("no", "some", "alot")), ~visionscreen+dis_score))
kruskal.test(visionscreen ~ dis_score, data=filter(disva, distype=="rem" & dis_score %in% c("no", "some", "alot")))

#For "wash"
addmargins(xtabs(data=filter(disva, distype=="wash" & dis_score %in% c("no", "some", "alot")), ~visionscreen+dis_score))
kruskal.test(visionscreen ~ dis_score, data=filter(disva, distype=="wash" & dis_score %in% c("no", "some", "alot")))

#For "comm"
addmargins(xtabs(data=filter(disva, distype=="comm" & dis_score %in% c("no", "some", "alot")), ~visionscreen+dis_score))
kruskal.test(visionscreen ~ dis_score, data=filter(disva, distype=="comm" & dis_score %in% c("no", "some", "alot")))

# JK:
# Please look up these nonparametric tests: Wilcoxon rank sum and Wilcoxon sign rank test, the Kruskal-Wallis test
# And these parametric tests: t-test, paired t-test, ANOVA
# What is the fundamental difference between nonparametric and parametric?
# When would you use parametric vs nonparametric? What are the assumptions of each?
# What are the summary measures usually given for parametric? For nonparametric? (ie, mean, sd, median, IQR)
