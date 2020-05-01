---
title: "**Applied Statistic: Traffic Pattern Analysis**"
author: "Group 25: Gabe Rivera Trent Buchert, Noah Kremer and Cam Savage"
date: "April 27, 2020"
output:
  html_document:
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
---

## Introduction
This report represents group 25's final project for STA 381: Engineering Statistics. Our report provides a statistical analysis of data for the Los Angeles Dodgers baseball team. This data consists of the following variables:

* `Date`
* `Number.Cars`: *traffic at the off-ramp*
* `Day.number`: *an index variable*
* `Number.Attendees`: *at the game that day; NA, if no game*
* `Team`
* `Result.Score`: *NA, if no game*
* `DayType`: *Weekday or Weekend*
* `GameDay`: *Game or No Game*

The traffic in the data set is counted at the off-ramp of a nearby highway exit; this is a summary version of the Dodgers Loop Sensor Data Set available in UCI’s Machine Learning Data Repository.

This report is an amalgamation of previous labs and will perform analyses including, but not limited to confidence intervals, numerical summaries, hypothesis tests, and linear regressions.

```{r, message=FALSE, include=FALSE}
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl") 
library("readxl") 
install.packages("mosaic")
library(tidyverse)
library("mosaic")
```

## Graphical and Numeric Summary Methodology
This section will focus on graphical and numeric summaries specific to four variables: `Number.Cars`, `Number.Attendees`, `DayType`, and `GameDay`. The graphical summaries will be implemented using `ggplot` and the numerical summaries will be implemented using standed R commands such as `summary()`. The numerical summaries for quantitative variables will include a  five-number summary, the mean, and standard deviation. As for categorical variables, we will display the percentages for each. 

### `Number.Cars`
```{r include=FALSE}
dodgers_data = read.csv("dodgers_traffic_data.csv")
read_in_data = read_xlsx("ggplot_data.xlsx")
```

Below are the graphical and numerical summaries for `Number.Cars`. This is a quantitative variable that represents the number of cars counted at the off-ramp of a nearby highway exit. 

```{r}
# Graphical summary for Number.Cars
ggplot(dodgers_data) + geom_histogram(aes(Number.Cars), fill = "skyblue3", col = "gold2", bins = 20) + ylab("Frequency") + ggtitle("Distribution of Traffic")
```

This plot above displays a histogram for `Number.Cars`. We can see a negative skew in the data as the left side of the plot is drawn out and asymmetric compared to the right side. The center of the plot is around 6,000 cars and based on the spread, we can assume that the mean for `Number.Cars` would be in the neighborhood of 6,000. Additionally, there are some gaps noticeable towards the origin; we can attribute these gaps to the result of there being too many bins, relative to the number of observations (a bin being a block you use to combine values before getting the frequency/count).

```{r}
# Numerical summary for Number.Cars
mean_number_cars = round(mean(dodgers_data$Number.Cars))
sd_number_cars = round(sd(dodgers_data$Number.Cars))
summary(dodgers_data$Number.Cars)
print(paste0("The mean and standard deviation for Number.Cars are ", mean_number_cars, " and " , sd_number_cars, " respectively."))
```

From the R code above, we calculated our mean and standard deviation to be 5,709 cars and 1,232 cars, respectively. After calculating the mean, we can better understand why the histogram for `Number.cars` is centered *close* to 6,000 cars with a negative skew, as our mean is equal to 5709. `Number.cars` occurs 174 times in the data sheet, meaning there was not a single day during the data acquisition that this variable could not be recorded. As we continue on, we will come across variables that do not occur the total 174 times.

### `Number.Attendees`
Next are the graphical and numerical summaries for `Number.Attendees`. This is a quantitative variable that represents the number of attendees counted at Dodgers home games. Defining that `Number.Attendees` is specific to Dodgers home games is essential, as data for attendees at away games are **not** considered in the data set.

```{r warning=FALSE}
# Graphical summary for Number.Attendees
ggplot(dodgers_data) + geom_histogram(aes(Number.Attendees), fill = "skyblue3", col = "gold2", bins = 20) + ylab("Frequency") + ggtitle("Distribution of Home Game Attendance")
```

The plot above displays a histogram for `Number.Attendees`. There is one noticeable characteristics about this graph: the gap near 40,000 attendees. This is due to rounding, as the number of bins is slightly larger relative to the number of observations, similar to the histogram for `Number.Cars`. Moreover, this plot is evenly distributed for the most part with clear spikes in mid 40,000 and 50,000 attendee range.

```{r}
# Numerical summary for Number.Attendees
non_na_dodgers_data <- filter(dodgers_data, dodgers_data$Number.Attendees != "NA")
mean_number_attendees = round(mean(non_na_dodgers_data$Number.Attendees))
sd_number_attendees = round(sd(non_na_dodgers_data$Number.Attendees))
summary(dodgers_data$Number.Attendees)
print(paste0("The mean and standard deviation for Number.Attendees are ", mean_number_attendees, " and " , sd_number_attendees, " respectively."))

```

We can break the R code above into two parts: filtering the data and the calculations. As previously mentioned, each variable has a different number of overall occurrences, as data was only acquired during home games. Therefore, the number of recorded attendances is dependent on the number of home games: 81 games. In light of this, we used the `filter()` command to filter the data set and calculated our mean and standard deviation to be 44,490 people and 7,058 people, respectively. With the mean and standard deviation in mind, we can better understand why the `Number.Attendees` peaks in the mid 40,000 person range (mean ≈ 44,500 attendees) and why the data is spread out with a high variation (standard deviation ≈ 7,060 attendees).

### `DayType`
As we finish the graphical and numerical analyses for the quantitative variables, we move onto the first categorical variable: `DayType`. This variable simply represents the type of day - `Weekday` or `Weekend` - that a game (inclusive of both home and away games) was played on. `DayType` was recorded 174 times. Below are the graphical and numerical summaries:

```{r}
# Graphical summary for DayType
ggplot(dodgers_data) + geom_bar(aes(DayType), fill = "skyblue3", col = "gold2") + ylab("Frequency") + ggtitle("Frequency of Games by Day Type")
```

The bar graph above displays the frequencies of the two forms of `DayType`: `Weekday` and `Weekend`. It's clear that there is a larger number of games played on weekdays than weekends. When we think about this in terms of a single, seven-day week, there is 5/7 (71.4%) chance that a day, chosen at random, would land on a weekday (Mon-Fri). Therefore, when we compare the proportions, it would make sense why more games occur on weekdays than weekends. 

```{r}
# Numerical summary for DayType
dodgers_data %>% count(DayType)
```

After analyzing the result of `dodgers_data %>% count(DayType)`, which displays the count of `Weekday` and `Weekend` within `DayType`, we see that 131 of the 174 (75.4%) games were played on weekdays and 43 of the 174 (24.7%) were played on weekends. This numerical analysis strengthens our observation from above and proves mathematically that a larger proportion of games were played on weekdays than weekends.

### `GameDay`
The last variable represented in this section is the categorical variable `GameDay`. This variable comes in two forms: `No Game` or `Game` and was recorded 174 total times. It is important to clarify that away games are represented by `No Game` and home games are represented by `Game.` Below are the graphical and numerical summaries:

```{r}
# Graphical summary for GameDay
ggplot(dodgers_data) + geom_bar(aes(GameDay), fill = "skyblue3", col = "gold2") + ylab("Frequency") + ggtitle("Frequency of GameDay")
```

The bar graph above displays frequencies for the two forms of `GameDay`: `No Game` and `Game`. Visually, we can see a slight difference in the two frequencies, where `No Game` is larger than `Game`, but not by much. Making sense of this, we can confirm visually that there were more away games than home games. We will be able to further analyze this graph in our numerical analysis below.

```{r}
# Numerical summary for GameDay
dodgers_data %>% count(GameDay)
```

Similar to the numerical analysis for `DayType`, we can use the R code `dodgers_data %>% count(GameDay)` to provide the frequencies of `GameDay`. It shows that `Game` occurs 81 of the 174 recorded times and `No Game` occurs 91 of the 174 recorded times. Understanding the numerical analysis of `GameDay` aids us in better visualizing the slight difference between `No Game` and `Game`, as the frequencies only differ by 10.

## Confidence Interval Methodology
In this section, our group will construct two 95% confidence intervals: the first will be for the mean number of attendees at a Dodgers game and the second will be for the proportion of days with Dodgers games. These calculations will include and focus on the variables `Number.Attendees` and `DayType`. Computations within the section include, but are not limited to the mean, standard deviation, and margin of error. Note that methodology in the rounding of this section follows the IEC 60559 standard.

### Confidence Interval for Mean Attendance at a Dodgers Game
```{r}
# CI for mean number of attendees at a Dodgers game
point_estimate_attendees <- signif(mean(non_na_dodgers_data$Number.Attendees))
sample_size <- length(non_na_dodgers_data$Number.Attendees)
m_o_e_for_attendees <- signif(qnorm(0.975) * sd_number_attendees/sqrt(sample_size))
left_attendee_CI <- signif(point_estimate_attendees - m_o_e_for_attendees)
right_attendee_CI <- signif(point_estimate_attendees + m_o_e_for_attendees)
print(paste0("95% confidence interval for mean number of attendees at a Dodgers game: (", left_attendee_CI, ", ", right_attendee_CI,")"))
```

The R code above represents the calculations required for a 95% confidence interval for the mean number of attendees at a Dodgers game. First, it's important to note that the data used in this calculation was the filterd `non_na_dodgers_data`, which has all "NA" data points filtered out. This confidence interval represents the range of values where we can be 95% confident that mean number of attendees can be found (42952.7, 46027.1). 

### Confidence Interval for Proportion of Days with Dodgers games
```{r}
# CI for the proportion of days with Dodgers games.
game_proportion = signif(81/174) # from numerical analysis section
sample_size_games = 174
sd_games <- signif(sqrt((game_proportion*(1-game_proportion))/sample_size_games))
m_o_e_for_games <- signif(qnorm(0.975) * sd_games/sqrt(sample_size_games))
left_game_CI <- signif(game_proportion - m_o_e_for_games)
right_game_CI <- signif(game_proportion + m_o_e_for_games)
print(paste0("95% confidence interval for for the proportion of days with Dodgers games: (", left_game_CI, ", ", right_game_CI,")"))

```

The R code above represents the calculations required for the second 95% confidence interval, which is for the proportion of days with Dodgers games. The data used in this calculation was the non-filtered data since this variable was recorded every time. This confidence interval represents the range of values where we can be 95% confident that the proportion of Dodger games that are home games can be found in (0.459898, 0.471136).

## Hypothesis Test Methodology on the Mean 
Moving on from confidence intervals, we will now focus on how average traffic count differs by day type. In this section, we will run a hypothesis test on the mean traffic counts for weekdays vs. weekends. To solve such problem, our group will incorporate filtered data, calculations for standard deviation and test-statistic, and utilization of Satterthwaite's Approximation for computing degrees of freedom. Additionally, a visualization of the data and an analysis of the test's practicality will be assessed at the end of this section. Note that methodology in the rounding of this section follows the IEC 60559 standard.

### Null and Alternative Hypothesis
$$H_0:  \mu_1 - \mu_2 = 0 \text{ vs. } H_1:  \mu_1 - \mu_2 > 0$$ Where $\mu_1$ is the average traffic count on weekdays and $\mu_2$ is the average traffic count on weekends.

### Hypothesis Test Calculations and Decision
```{r}
# Hypothesis Test Calculations
weekday_data <- filter(dodgers_data, dodgers_data$DayType == "Weekday") 
weekend_data <- filter(dodgers_data, dodgers_data$DayType == "Weekend")
meanWeekday = signif(mean(weekday_data$Number.Cars))
meanWeekend = signif(mean(weekend_data$Number.Cars))
sdWeekday = signif(sd(weekday_data$Number.Cars))
sdWeekend = signif(sd(weekend_data$Number.Cars))
sampleWeekday = count(weekday_data)
sampleWeekend = count(weekday_data)
t_statistic <- signif((meanWeekday - meanWeekend)/sqrt(((sdWeekday^2)/sampleWeekday)+((sdWeekend^2)/sampleWeekend)))
print(paste0("From the calculations above, our Test statistic = ", t_statistic))

#Use a t-distribution and the degrees of freedom is a "Satterthwaite Approximation". If the approximation is not an integer, you would round down (i.e 4.19 --> 4).

```

The R code above demonstrates the variables needed to calculate the p-value so our group can make a decision to reject or fail to reject the null hypothesis at the $\alpha$ = 0.05 level. Our calculations, consisting of a $df = 318$ and a $test-statistic = 4.19$, we conclude that the $p-value = 0.000018$, which is less than $\alpha$. Therefore, we will reject our null hypothesis.

### Interpretation of Hypothesis Test and P-value
Since our group rejected the null hypothesis, this means that we had sufficient evidence to support the claim that there is a higher average traffic count on weekdays compared to weekends. In the context of this data, our $p-value$ represents a 0.0018% chance that the average values of traffic counts of weekdays and weekends would be equal.

### Modeled Average Traffic Differences by Day Type
```{r}
# Graphical summary for GameDay 
ggplot(data=read_in_data, aes(x=DayType, y=Count, group=1)) + geom_line(colour="skyblue3") + geom_point(colour = "gold2") + expand_limits(y=0) + xlab("Day Type") + ylab("Averaeg Traffic") + ggtitle("Average Traffic by Day Type")
```

Above is a plot for the average `Number.Cars` (traffic) by `DayType`. Visually, we can see a discrepancy between the average traffic on weekdays vs. weekends - and this is exactly what we *should* see. This discrepancy displays that there does lie a difference between the two averages of traffic on weekdays vs. weekends, agreeing with our decision to reject $H_0$. 

### Practical Significance
Practical significance refers to the real-world relevance of the study. Sometimes, even if a study is statistically significant ($p-value < \alpha$), it doesn't always mean the study is practically significant. For example, for tests where, say, $p = 0.0499$ or $p = 0.0501$, for $\alpha = 0.05$, trouble arises when determining whether the results are *truly* statistically and practically significant. Since changing the sample size yields a trivial effect, this is where effect-size measures come in. Effect-size measures helps determine the sizes of associations, or the sizes of differences, similar to correlation/regression coefficients. In the code below we will introduce another common measure of effect size, sometimes referred to as "Cohen's $d$" [1].

```{r}
#Practical significance calculations
numerator = signif((meanWeekday - meanWeekend))
s_pooled = signif(sqrt((1/2)*((sdWeekday^2)+(sdWeekend^2))))
cohens_d = signif(numerator/s_pooled)
```

Our computations yield `Cohen's $d$ = 0.518`, A $d$-value of .5 represents a medium effect size and means that the two groups' means differ by half a standard deviation. With a medium effect size, it would make sense to reject the null hypothesis. Failing to reject the null with a medium effect size would be unreasonable, compared to failing to reject with a small effect size [1].

## Least-Squares Regression and Correlation Coefficient Methodology
Moving on from hypothesis tests on the mean, we will introduce a least-squares regression line of traffic and attendance (only for game days) and the correlation coefficient. Incorporating the `mosaic` library will allow for enhanced mathematical and statistical modeling, which will come in handy during regression modeling. Using the `summary()` command will aid our analysis by providing a valuable results within the least-squares regression. 

### Least-Squares Regression Plot and Summary
```{r}
#Least-squares regression graphical numerical summary 
model <- lm(Number.Cars ~ Number.Attendees, data=non_na_dodgers_data)
l <- makeFun(model)
xyplot(Number.Cars ~ Number.Attendees, data=non_na_dodgers_data, main="Traffic and Attendance Scatter Plot")
plotFun(l(Number.Attendees) ~ Number.Attendees, add=TRUE)
summary(model)
```

A linear relationship between `Number.Cars` and `Number.Attendees` does, in fact, seem reasonable. To display a linear relationship, the plotted data should be evenly split between a linear regression line. In the case of traffic and attendance, the plotted data seems to fit the criteria, so a linear relationship is reasonable. Since the response variable is the focus of an experiment, in our case, `Number.Cars` (traffic) is the response variable. An explanatory variable is one that explains changes, which would be the independent variable, or `Number.Attendance`. 

### Least-Squares Regression Equation
Based on the equation $Y = \beta_0 + \beta_1x$ and data from the model `summary()`, we conclude that  the equation of the least-squares regression line for `Number.Cars vs. Number.Attendees` is $Y = 6.117*10^{3} - 3.533*10^{-3}*x$

### Residuals and Adequacy of Regression
To display the adequacy of the regression, our group used ggplot to create diagnostic plots of the residuals. Calculating the residuals and fitted values, we were able to display the following models, which will aid in understand the adequacy from the regression model assumptions in a slightly different context.

```{r}
#Residual vs. fitted value modeling
non_na_dodgers_data$Residuals <- resid(model)
non_na_dodgers_data$FittedVals <- fitted(model)
ggplot(non_na_dodgers_data, aes(FittedVals, Residuals))+geom_point(colour = "skyblue3")+ ggtitle("Residual Values vs. Fitted Values")
```

First, our initial scatter plot of the `Residual Values vs. Fitted Values` provides a visual understanding that a pattern between the two values exists. Specifically, a grouping of points between -2000 and 2000 on the Residual axis distributed throughout the Fitted axis. This is evidence of a pattern where the residuals are floating around a roughly similar positive and negative distance from zero. We know from the lecture videos '11.5 & 11.7: Confidence Intervals on the Slope & Adequacy of the Regression Model' that any instance of a pattern of residuals vs. fitted values means that we have not met our model assumptions.

```{r}
#Residual histogram model
ggplot(non_na_dodgers_data, aes(x=Residuals)) + geom_histogram(fill = "skyblue3", col = "gold2",bins=40) +  ylab("Frequency")+ ggtitle("Distribution of Residual Values")
```

Taking a look at the `Distribution of Residual Values` plot, we can determine that a normal distribution does not seem reasonable for the histogram. Let us first point out that outliers are present to the far left of zero, displaying a negative skew to the plot. One could argue the plot has a slight bell-shape, but due to the negative skew, a bell-shape does not seem truly reasonable. Additionally, the plot is not centered at zero and doesn't display the symmetric qulity where the left side of the curve mirrors the right side.

```{r}
#Normal Probability Plot of Residuals
ggplot(non_na_dodgers_data, aes(sample = Residuals)) + geom_qq( distribution = qnorm, colour = "skyblue3") + geom_qq_line(colour = "gold2") + xlab("Theoretical") + ylab("Sample") + ggtitle("Normal Probability Plot of Residuals")
```

Last but not least, after analyzing the `Normal Probability Plot of Residuals` above, we can see two things. 1) A large grouping of the residuals that follow the quantile-quantile plot line. And 2) there are a number of residuals outside the quantile-quantile line which in turn skew the line made by the residuals. From our knowledge of normality in terms of error, if the plot of residuals provides a straight, diagonal line then our data is normally distributed. If the plot of residuals provides a skewed line, then our data is not normally distributed. Therefore, out plot tells us that we do not have normality with respect to our errors.

### Coefficient of Determination
The coefficient of determination is the "Multiple R-squared" value from the `summary()` of the model, which is equal to 0.0004856. In terms of a coefficient of determination ($R^2$), we refer to it as the amount of variability in the data explained by the regression model (11.5 & 11.7: Confidence Intervals on the Slope & Adequacy of the Regression Model). A value like 0.7 for $R^2$ would mean that 70% of variability can be explained by the model, and 30% is due to something else. We aim for 100% and want a high value. In our case, $R^2 = 0.0004856 ≈ 0.0005 $, meaning 99.95% of our variability in the number of cars cannot be explained by our model (or its relationship to number of attendees.

## Regression Line Hypothesis Test Methodology
For the last section of this report, we will formulate an appropriate hypothesis for significance of regression. This section will utilize R to calculate and report the p-value for this hypothesis test. Based on a significance level of 0.05 ($\alpha = 0.05$), we will then make a decision to reject or fail to reject the null hypothesis and then interpret our decision accordingly.

### Null and Alternative Hypothesis
$$H_0:  \beta_0 = 0 \text{ vs. } H_1:  \beta_0 \ne 0$$ Where $\beta_1$ is the slope of the regression line.

### Hypothesis Test Calculations and Decision
```{r}
#Numbers based on summary() of model
testStatistic = -0.196
pValue = 0.845
```
Looking at the summary of the model from `Least-Squares Regression Plot and Summary` and out calculations of the test statistic and p-value for this hypothesis test above, we fail to reject the null hypothesis since the $p-value > \alpha$. 

### Interpretation of Hypothesis Test and P-value
Since we fail to reject the null hypothesis, this means that we do not have sufficient evidence to conclude that there exists some linear relationship between the number of cars and number of attendees. In other words, there is not sufficient evidence to conclude that there exists a linear, or non-zero, correlation.From the video lecture on p-values, we can start by defining the p-value as the probability that the test statistic will take on a value that is at least as extreme as the observed value of the statistic when the null hypothesis is true. Breaking that down and applying it to our situation, in terms of the relationship between the number of cars and the number of attendees, on a game day, there is an 85% change the two variables will not be related to each other.

## Conclusion
All in all, based on the collection of statistical methods used throughout this report, our group has arrived at two main conclusions. First, the average traffic on weekdays is greater than the average traffic on weekends. And two, there exists no linear relationship between the number of cars and number of attendees.  

## References
Sources within this report include:

[1] [Statistics for Psychology](https://uk.instructure.com/courses/1965671/files/94610468?fd_cookie_set=1)

[2] [US Department of Transportation](https://ops.fhwa.dot.gov/congestion_report/chapter2.htm)

[3] [Article about LA Dodgers Traffic in 2017](https://laist.com/2017/10/25/dodger_traffic.php)

This report was produced using the following `R` packages:  `tidyverse`, `ggplot2`, `readxl`, and `mosaic`.


