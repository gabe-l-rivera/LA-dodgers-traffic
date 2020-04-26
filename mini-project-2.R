---
title: "mini-project-2"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl") 
library("readxl") 
install.packages("mosaic")
library(tidyverse)
library("mosaic")
```

# (6 points) In Lab 4, you performed a regression analysis; here we will dig a little deeper into some of the information in this model fit. Don’t forget to load any libraries you might want to use in your Rmd file. For help on how to fit and analyze regression models in R, review the Regression Example Code in the Lecture 30 Module on Canvas.

Q1. (2 points) Load the traffic data and create your subset of the data that only includes days with Dodgers home games. Fit the regression model from Lab 4 and print out a summary of the model.

```{r}
library("mosaic")
data = read.csv("dodgers_traffic_data.csv")
homeGameData = filter(data, data$GameDay == "Game") 
filteredDataCarsAndAttendees <- filter(homeGameData, homeGameData$Number.Attendees != "NA")
```

```{r}
model <- lm(Number.Cars ~ Number.Attendees, data=filteredDataCarsAndAttendees)
l <- makeFun(model)
xyplot(Number.Cars ~ Number.Attendees, data=filteredDataCarsAndAttendees)
plotFun(l(Number.Attendees) ~ Number.Attendees, add=TRUE)
summary(model)
```

Q2. (4 points) Looking at this summary of your model, what is the reported coefficient of determination. What does this mean? How do we interpret the sign and magnitude of this number?

The coefficient of determination is the 'R-Squared' value from the summary of the model, which is equal to 0.0004856
In terms of a coefficient of determination (R^2), we refer to it as the amount of variablilty in the data explained by the regression model (11.5 & 11.7: Confidence Intervals on the Slope & Adequacy of the Regression Model). A value like 0.7 for R^2 would mean that 70% of variability can be explained by the model, and 30% is due to something else. We aim for 100% and want a high value. In our case, R^2 = 0.0004856 ≈ 0.0005, meaning 99.95% of our variability in the number of cars cannot be explained by our model (or its relationship to number of attendees.

# (14 points) Now we’ll look more closely at the estimates of our regression model.

Q3. (4 points) Write down the null and alternative hypotheses for the hypothesis test for significance of the regression model. What does the parameter in this test represent? You can try editing this syntax (outside of an R chunk) to write your hypotheses: 
$H_0:  \beta_1 = 0 $ v.s $H_0: \beta_1 ≠ 0$

Q4.  (4 points) Looking at the summary of the model from Question 1, what are the test statistic and p-value for this hypothesis test? Make a decision to reject or fail to reject the null hypothesis at the α=0.05 level.
```{r}
testStatistic = -0.196
pValue = 0.845
print(paste0("Test statistic = ", testStatistic , " and p-value = ", pValue, " based on the summary provided above."))
```
Since the p-value is greater than 0.05, we fail to reject the null hypothesis.

Q5. (3 points) Interpret your decision to reject or fail to reject H0 in the context of this problem.

Since we fail to reject the null hypothesis, this means that we do not have sufficient eveidence to conclude that there exists some linear relationship between the number of cars and number of attendees. In other words, there is not sufficient evidence to conclude that there exists a linear, or non-zero, correlation.

Q6. (3 points) Interpret the p-value in the context of this problem (You may want to re-watch the Lecture Video on p-values).

From the video lecture on p-values, we can start by defining the p-value as the probability that the test statistic will take on a value that is at least as extreme as the observed value of the statstic when the null hypothesis is true. Breaking that down and applying it to our situation, in terms of the relationship between the number of cars and the number of attendees, on a game day, there is an 85% change the two variables will not be related to each other.


