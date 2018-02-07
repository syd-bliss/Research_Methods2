# Swallow Breeding Performance & Phenology in the Maritimes

# Q1 - Has swallow breeding phenology (clutch initiation date) changed from 1962-2016? 
# Q2 - Has swallow breeding performance (chick survival) changed from 1962-2016? 
# Q3 - Is breeding phenology affected by climate? 
# Q4 - Is breeding performance affected by climate?


##### STEP 1: IMPORT PACKAGES & DATA
library(tidyverse) 

rm(list = ls())
swallows.df <- read_csv("swallows.csv") # import dataset and save to an object
View(swallows.df)  # make sure R is reading data correctly
problems(swallows.df) 


##### STEP 2: TIDY DATA

# drop unnecessary columns to simplify data frame: 
swallows.df.2 <- select(swallows.df, Species, NestID, Province, Site, Latitude, Longitude, Year, CID, HD, CS, BS, SD12, minFt, minJFt, medJFt, medJp, meanMAp)

# create 3 time bins: 1) 1960-1972, 2) 1973-2005, 3) 2006-2016 
swallows.df.3 <- swallows.df.2 %>% mutate(Yr = cut(Year, breaks = c(1959, 1972, 2005, 2016), labels = c("1960-1972", "1973-2005", "2006-2016")))
summary(swallows.df.3)     # make sure data look ok
str(swallows.df.3)         # have 1 outlier
View(swallows.df.3)        

# change SD12 variable (# of chicks that survived to day 12, count data) to the % of the clutch that survived, to get survival rate
swallows.df.4 <- swallows.df.3 %>% mutate(Surv.Rate = SD12 / CS)
summary(swallows.df.4)     # make sure data look ok
View(swallows.df.4)

# outlier in CID variable (value = 96, or Aug 4) which is biologically unlikely. Probably due to recording error
# drop outlier:
swallows.df.5 <- filter(swallows.df.4, CID < 95)
summary(swallows.df.5)     # make sure data look ok

# make a tibble for each species so they can be examined seperately:
TRES.df <- filter(swallows.df.3, Species == "TRES")  # tree swallows
BARS.df <- filter(swallows.df.3, Species == "BARS")  # barn swallows


##### STEP 3: VISUALIZE DATA for each biological question

# histogram, see how many records exist for each species, likely unbalanced due to volunteer effort
ggplot(swallows.df.5, aes(x = Year)) + 
  geom_histogram(bins = 60) + 
  facet_grid(. ~ Species) + 
  xlab("Year") + 
  ylab("Count") +
  labs(title = "Number of Nests Sampled, Tree and Barn Swallows") 


# Q1 - changes in breeding phenology 
#    - look at clutch initiation date (CID) in both species     

# scatterplot, see spread of data
ggplot(swallows.df.5, aes(Year, CID)) + 
  geom_point() + 
  geom_jitter() +
  xlab("Year") + 
  ylab("Clutch initiation date (May 1 = day 1)") + 
  facet_grid(. ~ Species)
  
#boxplot, see if mean is changing over time
ggplot(swallows.df.5, aes(Yr, CID)) + 
  stat_boxplot(geom="errorbar") + 
  geom_boxplot(outlier.shape=1) + 
  xlab("Period") + ylab("Clutch initiation date (May 1 = day 1)") + 
  facet_grid(. ~ Species)     


# Q2 - changes breeding performance 
#    - look at % of clutch that survived (Surv.Rate) in both species        
        
#boxplot + scatterplot, see if mean is changing over time
# boxplot and scatterplot both difficult to interpret by themselves, more informative when combined
ggplot(swallows.df.5, aes(Yr, Surv.Rate)) + 
  stat_boxplot(geom="errorbar") + 
  geom_boxplot(outlier.shape=1) + 
  xlab("Period") + ylab("Survival") + 
  facet_grid(. ~ Species)  +   
  geom_jitter()
