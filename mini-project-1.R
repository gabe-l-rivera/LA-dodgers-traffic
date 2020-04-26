---
title: "Mini-Project-1"
author: "Group 25: Gabe, Noah, Trent, and Cam"
date: "4/25/2020"
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

# (20 points) In Lab 4, you performed a regression analysis; now we will check to make sure that none of the assumptions of that model appear to be violated. Don’t forget to load any libraries you might want to use in your Rmd file. For help on how to fit and analyze regression models in R, review the Regression Example Code in the Lecture 30 Module on Canvas.

Q1. (2 points) Load the traffic data. Note that our regression model from Lab 4 only uses data that corresponds to days on which the LA Dodgers have a home game. Create a subset of the traffic data that only includes days with Dodgers home games (hint: use the filter() function we examined in Lab 2). Print out the number of rows in the original (full) dataset and in your new subset.
```{r}
data = read.csv("dodgers_traffic_data.csv")
homeGameData = filter(data, data$GameDay == "Game") # "Game" = home game, "No Game" = away game
# double check this 28-32. Does she really mean rows?
originalRows = length(data)
print(paste0("Rows from origina data set: ", originalRows))
filteredRows = length(homeGameData)
print(paste0("Rows from filtered data set ", filteredRows))
```

Q2. (2 points) Fit the regression model from Lab 4 (using your subset of data created above) and print out a summary of the model. Does this match your estimates from Lab 4?
```{r}
# create a filtered dataset where for only finite values of Number.Cars and Number.Attendees
filteredDataCarsAndAttendees <- filter(homeGameData, homeGameData$Number.Attendees != "NA")

model <- lm(Number.Cars ~ Number.Attendees, data=filteredDataCarsAndAttendees)
l <- makeFun(model)
xyplot(Number.Cars ~ Number.Attendees, data=filteredDataCarsAndAttendees)
plotFun(l(Number.Attendees) ~ Number.Attendees, add=TRUE)
summary(model)
```
This does match our estimates from Lab 4 as we estimated no major changes.

Q3. (2 points) Calculate the residuals and print out the mean of the residuals. What do we expect the mean of the residuals to be? To get the fitted values from your model, work off of the following syntax: fitted( mymodel )
```{r}
residuals = resid(model)
fitted = fitted(model)
meanResiduals = mean(residuals)
print(paste0("Mean of residuals: ", meanResiduals))

```

Q4 (2 points) Add the fitted values and residuals as new columns in your subset of the game data. Print out the first few rows of your dataset after adding these new columns to check your work. Use this generic syntax to add each new column: myData$newColumnName <- myVector
```{r}
#add_residuals(homeGameData, model, var = "resid")
homeGameData$Residuals <- residuals
homeGameData$FittedVals <- fitted
head(homeGameData)
```

Q5 (4 points) One of the assumptions of a linear regression model is that the error term is normally distributed. Our residuals are like estimates of the error terms. Use ggplot to make a histogram of the residuals. Does a Normal distribution seem reasonable? Be sure to address the main attributes of the Normal distribution: symmetric, centered at zero, and bell-shaped.
```{r}
ggplot(homeGameData, aes(x=Residuals)) + geom_histogram(bins=40)
```

After plotting a histogram of the residuals, our group was stuck between choosing whether or not a normal distribution was reasonable and unreasonable. In end, we chose to claim that a normal distribution does not seem reasonable for the histogram. Let us first point out that outliers are present to the far left of zero, displaying a negative skew to the plot. One could argue the plot has a slight bell-shape, but due to the negative skew, a bell-shape does not seem truly reasonable. Additionally, the plot is not centered at zero and doesn't display the symmetric qulity where the left side of the curve mirrors the right side.

Q6 (4 points) Another way to check for normality of the error terms is to make a normal probability plot of the residuals. Create this plot using the sample code below; what does the plot tell you about the assumption of Normality of the error terms (You may want to refer to Figure 6.24 in your textbook)?
```{r}
ggplot( homeGameData, aes(sample = Residuals)) + geom_qq( distribution = qnorm ) + geom_qq_line()
```

In this plot, we can see two things. 1) a large grouping of the residuals that follow the quantile-quantile plot line. And 2) there are a number of residuals outside the quantile-quantile line which in turn skew the line made by the residuals. From our knoweledge of normality in terms of error, if the plot of residuals provides a straight, diagonal line then our data is normally distributed. If the plot of residuals provides a skewed line, then our data is not normally distributed. Therefore, out plot tells us that we do not have normality with respect to our errors.

Q7 (4 points) Finally, check for any pattern in the residuals, as they relate to the fitted values. Use ggplot to make a plot of the fitted values vs. the residuals. Do you see any evidence of a pattern in this plot (You may want to refer to Figure 11.9 in your textbook)? What does this mean?
```{r}
ggplot(homeGameData, aes(FittedVals, Residuals))+geom_point()
```

In the Fitted vs. Residual plot, I immediately see a grouping of points between -2000 and 2000 on the Residual axis distributed throughout the Fitted axis. This is evidence of a pattern where the residuals are floating around a roughly similar positive and negative distance from zero. We know from the lecture videos '11.5 & 11.7: Confidence Intervals on the Slope & Adequacy of the Regression Model' that any instance of a pattern of residuals vs. fitted values means that we have not met our model assumptions.


