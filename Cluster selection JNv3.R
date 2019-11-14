library(tidyverse)
library(skimr)
library(readxl)

communities_list <- read_excel("alto_amazonas_communities_list.xlsx", sheet = "Included")
#clean 
communities_clean <- communities_list %>%
  rename(healthpost = `Health Post`,
         districts = microred,                       #Peru organization goes Regions > Provinces > Districts,
         com_name = `Town name`,                     #we are interested in the Alto Amazonas province and its districts
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
         pop) %>%
  mutate(clusterid=dense_rank(paste(districts, com_name, healthpost)),
         kidsperhh=oneto9/n_houses)  #   %>%
                                                                        #   group_by(clusterid) %>%
                                                                        #   mutate(dup=n())
                                                                        # xtabs(data=communities_clean, ~dup, addNA=TRUE)

xtabs(data=communities_clean, ~districts+indigenous, addNA=TRUE)

# JK question: we need to write out what our sampling frame is and why we decided on that.
#JN- I should have done this from the start but put all communities on imported excel sheet 1 and will exclude using R so it is
#reproducible/easier to follow.

#sampling frame: direct element sampling from all communities listed in the most recent census obtained form AA health network
  #excluding: large urban areas as they have low likelihood of having trachoma. 
    #Specifically the capital cities/towns: Yurimaguas, Jeberos, Balsapuerto, Santa Cruz, Lagunas.

  #also excluding: health posts that are on the road from Tarapoto to Yurimaguas as the communities they serve are peri-urban and 
  #thus have a low likelihood of having trachoma. 
  #Based on GPS coordinates of health posts https://drive.google.com/open?id=1O5X0jLKe0MhXjf9rfm2DOett0qgsN-4S&usp=sharing
  #the health posts excluded so far are makred with red pins on the maps
    #Specifically: 30 de agosto Km 17, Santo Tomas, san juan de pamploma, Grau km 40, Pampa Hermosa, INDEPENDENCIA DEL SHANUSI,
    #Cotoyacu, Puerto Peru, Independencia, Tupac Amaru

  #JN question: we also could exclude health posts on the road from Yurimaguas to Balsapuerto, what do you think?
    #specifically these would be the health posts of: Munichis, Santa Lucia, and ACHUAL LIMON.
    #I have not excluded these yet, will ask Muñoz about it.

samplingframe <- communities_clean %>%
  filter(healthpost != "Yurimaguas",      #the capital towns of each district, therefore urban and excluded
           healthpost != "Jeberos", 
           healthpost != "Natividad",     #natividad is a large hospital located in the center of Yurimaguas
           healthpost != "Lagunas",
           healthpost != "Santa Cruz",
           healthpost != "Shucushyacu") %>% #
  filter(healthpost != "de Agosto km 17",   #health posts that are on the road from Tarapoto to Yurimaguas
         healthpost != "Santo Tomas",
         healthpost != "san juan de pamploma",
         healthpost != "Grau km 40",
         healthpost !="Pampa Hermosa",
         healthpost != "INDEPENDENCIA DEL SHANUSI",
         healthpost != "Cotoyacu",
         healthpost != "Puerto Peru",
         healthpost != "Independencia",
         healthpost != "Tupac Amaru")  %>%  #note I have not excluded any barrios or large population centers
  filter(com_name != "ARAHUANTE") %>% #majority of houses have water running from tank into house
  filter(districts != "Yurimaguas") %>%  #excluding yurimaguas as most centers have running water and are better off than more remote areas
  filter(pop>=100) # we are considering places <100 people to be a hamlet not a village, as per open street map guidance
  
  #I included Balsapuerto/Yurimaguas district after looking at a map and noting these communities are all in Balsapuerto
  #I also included barrio central in Lagunas

ggplot(data=samplingframe, aes(y=pop, x=districts)) +
  geom_boxplot()

xtabs(data = samplingframe, ~districts+indigenous, addNA = TRUE)

# normalization function to get the probability of being chosen; unclear if you actually need to do this or not but Travis did it
nrm <- function(x)x/sum(x)
n.to.sample <- 22
namevector <- samplingframe$clusterid
vector.of.sizes <- samplingframe$pop
normalized.vector.of.sizes <- nrm(vector.of.sizes)
set.seed(2768) # Last 4 digits of a phone number on a sticky note at the desk I am using in the local health network - JN
randomsample <- sample(namevector, size=n.to.sample, prob=nrm(vector.of.sizes),replace=FALSE)
randomsample.df <- as.data.frame(randomsample) %>% rename(clusterid=randomsample) %>% mutate(selected=1)

a <- bind_cols(samplingframe,as.data.frame(normalized.vector.of.sizes))
b <- full_join(a, randomsample.df, by="clusterid")
brandom <- b %>%
  filter(selected==1) %>%
  mutate(kidsin40hh=round((kidsperhh*40), digits=0),
         kidsin40hh2=if_else((kidsin40hh-oneto9)>0,oneto9, kidsin40hh),
         kidsin40hh2=if_else(is.na(kidsin40hh2), 58, kidsin40hh2),
         kidsin35hh=round((kidsperhh*35), digits=0),
         kidsin35hh2=if_else((kidsin35hh-oneto9)>0,oneto9, kidsin35hh),
         kidsin35hh2=if_else(is.na(kidsin35hh2), 58, kidsin35hh2))
brandom %>% summarize(mean(kidsin40hh2, na.rm=TRUE))
brandom %>% summarize(sum(kidsin40hh2, na.rm=TRUE))
brandom %>% summarize(mean(kidsin35hh2, na.rm=TRUE))
brandom %>% summarize(sum(kidsin35hh2, na.rm=TRUE))
xtabs(data=samplingframe, ~districts, addNA=TRUE)
addmargins(xtabs(data=brandom, ~districts+indigenous, addNA=TRUE))
# ggplot(data=b, aes(pop, normalized.vector.of.sizes)) + geom_point()

mean(brandom$kidsperhh, na.rm = TRUE) #1.63

#creating new data table with NA kidsperhh replaced with the mean of all communities (1.63)
dummy1_9 <- brandom %>%
  select(com_name, n_fam, n_houses, oneto9, pop, kidsperhh, kidsin35hh, kidsin35hh2, kidsin40hh, kidsin40hh2) %>%
  mutate(kidsperhh_estimated = case_when(is.na(kidsperhh) ~1.63,
                                         !is.na(kidsperhh) ~ kidsperhh)) %>%
  select(-kidsperhh, -(kidsin35hh:kidsin40hh2)) %>%
  mutate(kidsin40hh=round((kidsperhh_estimated*40), digits=0),
         kidsin40hh2=if_else((kidsin40hh-oneto9)>0,oneto9, kidsin40hh),
         kidsin35hh=round((kidsperhh_estimated*35), digits=0),
         kidsin35hh2=if_else((kidsin35hh-oneto9)>0,oneto9, kidsin35hh),
         kidsin30hh=round((kidsperhh_estimated*30), digits = 0),
         kidsin30hh2=if_else((kidsin30hh-oneto9)>0, oneto9, kidsin30hh))%>%
  select(com_name, n_fam, n_houses, oneto9, pop, kidsperhh_estimated, kidsin35hh2, kidsin40hh2, kidsin30hh2)

dummy1_9 %>% summarize(sum(oneto9))  #total of 1877 1-9 year olds, required sample size is 1200
dummy1_9 %>% summarize_at(vars(kidsin35hh2:kidsin30hh2), sum, na.rm = TRUE) #30hh=1035, 35hh=1161, 40hh=1270

#Answered Questions:

# JK - As of now it appears to be 5 (6)? districts, excluding Yurimaguas. JN-5 districts is correct
# JK - What is the Balsa/Yurimaguas designation? JN-Health posts on the border, I believe serving communities from both districts
# JK - Are there places in Yurimaguas that are smaller that we should not be excluding?
#JN-there probably are, I can review the entire list with the epidemiologist in AA

# JK- Did MOH tell you to exclude all of Yurimaguas. 
#JN- this was after a conversation with Willy as it is a more developped area, lots of peri urban communities,
#roads, cars. However when I talked to Durand he said Lagunas or Balsapuerto districts would be our best bet.
