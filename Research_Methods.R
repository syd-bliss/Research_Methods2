# Swallow Breeding Performance & Phenology in the Maritimes

# Q1 - Has swallow breeding phenology (clutch initiation date, CID) changed from 1962-2016? 
# Hypothesis: both spp. will have advanced CID over time
# Rationale: insects are likely emerging earlier due to advanced spring thaw-up, and swallows have adjusted their phenology in response

# Q2 - Has swallow breeding performance (chick survival) changed from 1962-2016? 
# Hypothesis: chick survival has decreased over time, contributing to the overall population declines in both spp.
# Rationale: decreased chick survial (one measure of breeding performance) would be an obvious explanation as to why 
# populations have broadly declined. If chick survial has not changed, it suggests declines may be occuring at other
# points of the annual cycle (ex. high mortality during migration or overwintering) 
# NOTE: could also examine other measures of breeding performance such as clutch size and brood size

# Q3 - Is breeding phenology (clutch initiation date) affected by climate? 
# Hypothesis: rainfall and temperature variables will explain the most variation in clutch initiation date
# Rationale: rainfall and temperature are both positively associated with insect abundance, and likely emergence

# Q4 - Is breeding performance (chick survival) affected by climate?
# Hypothesis: chick survival has been negatively affected by changes in climate
# Rationale: ???




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
TRES.df <- filter(swallows.df.5, Species == "TRES")  # tree swallows
BARS.df <- filter(swallows.df.5, Species == "BARS")  # barn swallows


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
  geom_jitter() + geom_point(alpha=0.3)




### use summarise() and group_by() to get counts of Surv.Rate, add to last to plot ?
  geom_point(aes(size = count), alpha = 0.3) 
  
  
  
#Q3 - breeding phenology & climate
#    - are there relationships between climate variables and CID?
#    - need to pull apart BARS clutches
  
#climate variable #1 - minimum February temperature  
# looks like minFt has a negative relationship with CID
ggplot(swallows.df.5, aes(minFt, CID)) + 
    geom_point() + 
    geom_jitter() +
    xlab("Minimum February temperature") + 
    ylab("Clutch initiation date (May 1 = day 1)") + 
    facet_grid(. ~ Species)
  
ggplot(swallows.df.5, aes(minFt, CID)) + 
    geom_smooth(mapping = aes(x = minFt, y = CID)) + 
    xlab("Minimum February temperature (C)") + 
    ylab("Clutch initiation date (May 1 = day 1)") + 
    facet_grid(. ~ Species)
  
#climate variable #2 - minimum January-February temperature 
# looks like minJFt has a negative relationship with CID in TRES, but not in BARS
ggplot(swallows.df.5, aes(minJFt, CID)) + 
    geom_point() + 
    geom_jitter() +
    xlab("Minimum Jan-Feb temperature") + 
    ylab("Clutch initiation date (May 1 = day 1)") + 
    facet_grid(. ~ Species)
  
ggplot(swallows.df.5, aes(minJFt, CID)) + 
    geom_smooth(mapping = aes(x = minJFt, y = CID)) + 
    xlab("Minimum Jan-Feb temperature (C)") + 
    ylab("Clutch initiation date (May 1 = day 1)") + 
    facet_grid(. ~ Species)

#climate variable #3 - median January-February temperature 
# no consistent relationship in either spp.
ggplot(swallows.df.5, aes(medJFt, CID)) + 
  geom_point() + 
  geom_jitter() +
  xlab("Median Jan-Feb temperature") + 
  ylab("Clutch initiation date (May 1 = day 1)") + 
  facet_grid(. ~ Species)

ggplot(swallows.df.5, aes(medJFt, CID)) + 
  geom_smooth(mapping = aes(x = medJFt, y = CID)) + 
  xlab("Median Jan-Feb temperature (C)") + 
  ylab("Clutch initiation date (May 1 = day 1)") + 
  facet_grid(. ~ Species)

#climate variable #4 - mean annual precipitation 
# CID earliest with mean monthly precipitation is lowest and almost highest
# does this make sense?
ggplot(swallows.df.5, aes(meanMAp, CID)) + 
  geom_point() + 
  geom_jitter() +
  xlab("Mean monthly precipitation (cm)") + 
  ylab("Clutch initiation date (May 1 = day 1)") + 
  facet_grid(. ~ Species)

ggplot(swallows.df.5, aes(meanMAp, CID)) + 
  geom_smooth(mapping = aes(x = meanMAp, y = CID)) + 
  xlab("Mean monthly precipitation (cm)") + 
  ylab("Clutch initiation date (May 1 = day 1)") + 
  facet_grid(. ~ Species)






