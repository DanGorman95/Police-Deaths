setwd("c:/Users/gormo/Desktop/Data") #<-- change this to where you have downloaded the csv

clean <- read.csv("police_deaths.csv" ,stringsAsFactors = T, header = T)

#2nd CSV, contains population of each state and the gun ownership figures of each state.. 
#TO be merged with police_deaths.csv based on state name e.g. "TX" = Texas 
guns <- read.csv("Gun_Stats.csv", stringsAsFactors = T, header = T)

install.packages("dtplyr")
#library(psych) Unsed
library(tidyverse)
library(dtplyr)
library(ggplot2)
library(plyr)
library(tidyr)
#library(vcd) unused
#library(gmodels) Unsued, code located in extra section at the end

#--------Cleaning-----------
#Used to remove names of police officers to analysis on Ranking of police officers killed
ff = function(x, patterns, replacements = patterns, fill = NA, ...)
{
  stopifnot(length(patterns) == length(replacements))
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
  return(ans)
}
#Listing the ranks to be drawn from person
clean$Rank <- ff(clean$person, 
c('Sheriff','Watchman','Inspector','Marshal','Constable','Guard','Private','Captain','Sergeant','Officer','Patrolman','Chief of Police','Special Agent', "Trooper", "Detective", "Border Patrol", "K9", "Investigator", "Lieutenant"), 
c('Sheriff','Watchman','Inspector','Marshal','Constable','Guard','Private','Captain','Sergeant','Police Officer','Patrolman','Chief of Police','Special Agent', "Trooper", "Detective", "Border Patrol", "K9", "Investigator", "Lieutenant"),
"Other", ignore.case = TRUE)

clean$Day <- ff(clean$eow, 
c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), 
c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), 
"Other", ignore.case = TRUE)

#Removing attributes not needed
clean[["cause"]] = NULL
clean[["person"]] = NULL
clean[["eow"]] = NULL
clean[["canine"]] = NULL
guns[["pop.1900"]] = NULL
guns[["pop.1910"]]  = NULL
guns[["pop.1920"]] = NULL 
guns[["pop.1930"]] = NULL 
guns[["pop.1940"]] = NULL 
guns[["pop.1950"]] = NULL 
guns[["pop.1960"]] = NULL 
guns[["pop.1970"]] = NULL 
guns[["pop.1980"]] = NULL 
guns[["pop.1990"]] = NULL 
guns[["Gun.Murder.Rate.per.100K..2010."]] = NULL
#Changing column name
colnames(clean)[colnames(clean)=="cause_short"] <- "cause"

#Removing any N/A Values
clean[!is.na(clean)]
guns[!is.na(guns)]

#Changing TO Factors
clean$Rank   = as.factor(clean$Rank)
clean$Day = as.factor(clean$Day)
#clean$date <- as.Date(clean$date) #Problems with dates, some years wont format to match others causng N/A values to appear
clean$year <- as.integer(clean$year)
clean$Day = factor(clean$Day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

#Mergering two csv files together
df = merge(clean, guns)
df = merge(df, dea) #Used to merge numeric value taken from deaths with the newly created data frame 
#Remove these three objects
rm("guns", "clean")
#------END OF CLEANING-------

#---Creating Subsets for MapReduce etc.------
#Split_1.Split_2,Split_3 Created for use with Mapreduce
split_1 <- during %>% 
  group_by(state) %>%
  summarise(Deaths = n())
#Creating data.frame for numeric value of each state
split_1 <- post %>% 
  group_by(state) %>%
  summarise(Deaths = n())
#Write to CSV file
write.csv(split_1, "split_1.csv" )

split_2 <- war %>% 
  group_by(state) %>%
  summarise(Deaths = n())
#Write to CSV file
write.csv(split_2, "split_2.csv" )

split_3<- war %>% 
  group_by(state) %>%
  summarise(Deaths = n())
#Write to CSV file
write.csv(split_3, "split_3.csv" )

#Drawing Numeric value from states to add to the df datafram for easier testing
dea<-  df %>% 
  group_by(state) %>%
  summarise(Deaths = n())
during<-  during %>% 
  group_by(cause) %>%
  summarise(Deaths = n())
#Get % of deaths by state 
dea$Pct <- dea$Deaths / sum(dea$Deaths)*100

#Creating a subset before I remove certain years of data
during<-subset(df, year >= 1920 &  year <= 1933)
pre<-subset(df, year <= 1919)
post<-subset(df, year >= 1934 &  year <= 1944)
war<-subset(df, year >= 1971 &  year <= 2000)
pre_proh <- subset(df, year < 1920)
pre_during <- subset(df, year <= 1933)
mod <- subset(df, year > 2000)
#Used for a host of different graphs and tests

#----Analysis----
summary(df)
median(df$year)
mean(df$year)
#Testing correlation among two variables, noted in my results section
cor.test(df$Deaths, df$Guns_2007)
#Used in results section in figure 6
hist(pre_during$year, breaks=200,
main = "Police Officers death toll 1791-1933",
xlab= "Year",    
col =heat.colors(15))
#Unused
#ggplot(df, aes(year)) + 
#geom_bar(fill = "blue") +
#labs( x = "Year", y = "Total Number of Deaths")

newdata <- during %>% 
  group_by(state) %>% 
  summarise(deaths = n())
newdata %>% filter(deaths > 140) %>% 
  ggplot(aes(reorder(state, deaths), deaths)) + # reorder to sort by ascending deaths
  geom_bar(stat = "identity") 

war2 <- war %>% 
  group_by(cause) %>% 
  summarise(deaths = n())
war2 %>% filter(deaths > 120) %>% 
  ggplot(aes(reorder(cause, deaths), deaths)) + # reorder to sort by ascending deaths
  geom_bar(stat = "identity") 

pree <- post %>% 
  group_by(state) %>% 
  summarise(deaths = n())
pree %>% filter(deaths > 80) %>% 
  ggplot(aes(reorder(state, deaths), deaths, xlab="Dea")) + # reorder to sort by ascending deaths
  geom_bar(stat = "identity")
#Used to create subsets for t-test
tester1 <- BO %>%
  group_by(state) %>%
  summarise(deaths_B = n())

tester <- GB %>%
  group_by(state) %>%
  summarise(deaths_B = n())

#Used to create subsets for t-test
tess <- merge(tester, tester1)
GB <- subset(df, year >= 2001 & year <= 2008 )
BO <- subset(df, year >= 2009 & year <= 2016 )
#-----Graphs-------
#Belong to figure 7
newdata <- during %>% 
  group_by(state) %>% 
  summarise(deaths = n())
newdata %>% filter(deaths > 140) %>% 
  ggplot(aes(reorder(state, deaths), deaths)) + # reorder to sort by ascending deaths
  geom_bar(stat = "identity") 

#Belong to figure 8
newdata1 <- post %>% 
  group_by(state) %>% 
  summarise(deaths = n())
newdata1 %>% filter(deaths > 80) %>% 
  ggplot(aes(reorder(state, deaths), deaths)) + # reorder to sort by ascending deaths
  geom_bar(stat = "identity") 

#Reset plots back to 1,1
par(mfrow=c(1,1))
par(mfrow = c(2,2))


#=====Unused==========
my.lm = lm(mod$year ~ mod$pop.2010 + mod$pop.2000 )
plot(my.lm)

      
plot(test)       
test <- table(df$state)
test

joint = CrossTable(df$year, prop.chisq = F)
joint_counts = joint$t
barplot(joint_counts, beside=TRUE, col = , ylab = 'Freq', xlab='sex')
legend('bottomright', horiz=TRUE ,c('Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday', 'Sunday'), pch=15, col = blues9)

ggplot(war, aes(reorder(cause, cause, function(x) + length(x)))) + 
  geom_bar(fill = "darkblue") + 
  coord_flip() +
  labs( x = "Year", y = "Total Number of Deaths")

ggplot(war,aes(x =state,fill=cause ))+ stat_count(width = .5)+coord_flip()+ggtitle("Deaths by Department")
