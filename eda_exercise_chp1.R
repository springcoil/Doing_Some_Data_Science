# Code based on 'Doing Data Science' Chp2 
# This is an exercise in Exploratory Data Analysis
# The data set is the NYT data set
setwd('Documents/Code/Doing_Data_Science/')
data1 <- read.csv('nyt1.csv')
summary(data1)
sapply(data1, class)

# Create a new variable age_group that categorizes users
# Note that you need the c because it is a categorical variable
data1$age_group <- cut(data1$Age, c(-Inf, 18, 24, 34, 44, 54, 65, Inf))
#view 
summary(data1)
#brackets
install.packages("doBy")
library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~age_group, data = data1, FUN=siterange)


# so only signed in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks ~ age_group, data =data1)

#plot
install.packages("ggplot2")
library("ggplot2")
ggplot(data1, aes(x=Impressions, fill=age_group))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=age_group, y=Impressions, fill=age_group))+geom_boxplot()

# create click thru rate
# we don't care about clicks if there are no impressions
# if there are clicks with no imps my assumptions about 
# this data are wrong
data1$hasimps <- cut(data1$Impressions, c(-Inf, 0, Inf))
summaryBy(Clicks~hasimps, data =data1,FUN=siterange)
ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions, color=age_group)) + 
  geom_density()
ggplot(subset(data1, Clicks>0), aes(x=Clicks/Impressions, color=age_group)) + 
  geom_density()
ggplot(subset(data1, Clicks>0), aes(x=age_group, y=Clicks, fill=age_group)) + 
  geom_boxplot()
ggplot(subset(data1, Clicks>0), aes(x=Clicks, color=age_group)) + 
  geom_density()

# create categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0]  <- "Clicks"

#Convert the column to a factor
data1$scode <- factor(data1$scode)
head(data1)

#Look at levels
clen <- function(x){c(length(x))}
etable<-summaryBy(Impressions~scode+Gender+age_group, data=data1, FUN=clen)
head(etable)
tail(etable)

#We want to create metrics/measurements/statistics that summarize the data.
cstats <- function(x){c(length(x), min=min(x), mean=mean(x), max(x), var(x))}
statstable<-summaryBy(Impressions~scode+Gender+age_group, data=data1, FUN=cstats)
head(statstable)
