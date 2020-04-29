---
Author: Group 25
title: "Lab 4"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#remove.packages("ggplot2")
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl") # not sure if we need this but it doesnt hurt
library("readxl") # not sure if we need this but it doesnt hurt
install.packages("mosaic")
library("mosaic")
data = read.csv("dodgers_traffic_data.csv")
#head(data)  

```
        
                                       Hypothesis Tests
```{r}
library(tidyverse)
## Q1: H_0: mu1 - mu2 = 0 vs. H1: mu1 != mu2, where mu1 = weekdays and mu2 = weekends

## Q2: A2. We will use a "Hypothesis Tests on the Difference in Means" referenced from chapter 10.2.1.2. 

## Q3: Done √

## Q4: t = (meanWeekday - meanWeekend)/sqrt((sampleWeekday^2/countWeekday^) + (sampleWeekend^2/countWeekend^))

## Q5: Use a t-distribution and the degrees of freedom is a "Satterthwaite Approximation". If the approximation is not an integer, you would round down (i.e 4.19 --> 4).

## Q6: p-value = 0.000039 

## Q7: Since α>p, we reject the null hypthesis of mu1 - mu2 = 0.

weekday_data <- filter(data, data$DayType == "Weekday") 
weekend_data <- filter(data, data$DayType == "Weekend")

meanWeekday = mean(weekday_data$Number.Cars)
meanWeekend = mean(weekend_data$Number.Cars)

stdWeekday = sd(weekday_data$Number.Cars)
stdWeekend = sd(weekend_data$Number.Cars)

sampleWeekday = count(weekday_data)
sampleWeekend = count(weekday_data)


## Q4: Done √
t_stat = (meanWeekday - meanWeekend)/sqrt(((stdWeekday^2)/sampleWeekday)+((stdWeekend^2)/sampleWeekend))

## Q5: Use a t-distribution and the degrees of freedom is a "Satterthwaite Approximation". If the approximation is not an integer, you would round down (i.e 4.19 --> 4).

## Q6: p-value = 0.000039 

## Q7: Since α>p, we reject the null hypthesis of mu1 - mu2 = 0. This makes sense since we didn'y expect the average number of cars on weekends vs. weekdays to be equal.


p = ggplot(data) + 
  geom_line(aes(x = Day.number, y = meanWeekday), color = "blue") +
  geom_line(aes(x = Day.number, y = meanWeekend), color = "red") +
  xlab('Day number') +
  ylab('Average traffic')

print(p)

## Q8: This matches our conclusion from our hypothesis test becasue they are clearly NOT EQAUL.

```


                            Introduction to Regression Analysis

```{r}

## Q1: A linear realtionship does seem reasonable. To display a linear relationship, the plotted data should be evenly split between a linear regression line. In the case of traffic and attendance, the plotted data seems to fit the criteria, so a linear relationship is reasonable.

## Q2: Since the response variable is the focus of an experiment, in our case, Number.Cars (traffic) is the response variable. An explanatory variable is one that explains changes, which would be the independant variable, or Number.Attendance.

## Q3: Done √

## Q4: Estimate of the slope from summary() = -3.533e-03 = -0.003533

## Q5: Estimate of the intercept from summary() = 0.6117e+04

## Q6: Done √

## Q7: Based on our group's gut reaction, this is not an adequate model for describing the impact Dodger attendance has on LA traffic near the stadium. Our group expexted LA traffic to have some impact, but not an overly-dramtic one, so this is more-or-less close to what we expected.

library("mosaic")
# create a filtered dataset where for only finite values of Number.Cars and Number.Attendees
filteredDataCarsAndAttendees <- filter(data, data$Number.Attendees != "NA")
model <- lm(Number.Cars ~ Number.Attendees, data=filteredDataCarsAndAttendees)
l <- makeFun(model)
xyplot(Number.Cars ~ Number.Attendees, data=filteredDataCarsAndAttendees)
plotFun(l(Number.Attendees) ~ Number.Attendees, add=TRUE)
summary(model)
```
