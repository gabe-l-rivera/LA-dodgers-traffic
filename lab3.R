---
title: "lab3"
author: "Group25"
date: "3/31/2020"
output: html_document
---
### Graphical Summaries
---
Question 1: 
Importing the data via Canavs into Files.
Place into variable named traffic.
---
```{r}
#Q1
traffic <<- read.csv("dodgers_traffic_data.csv")
head(traffic)
tail(traffic)
```
---
Question 2, 3 & 4: 
Make histograms of Number.Cars and Number.Attendees.
Describe the distributions.
In your histogram for Number.Attendees, you may notice some “gaps” along the x-axis - why do you think we’re seeing this? (Hint: How many observations are displayed in this histogram?)
---
```{r}
#Install packages
install.packages("tidyverse")
library(tidyverse)

#Q2 
ggplot(traffic) + geom_histogram(aes(Number.Cars),fill="skyblue",col="black",bins=30)
# Description: The distribution is skewed to the right with a center around 6000 cars. We believe this data is skewed to the right because the histogram still accounts for data from away-games. 

#Q3
ggplot(traffic) + geom_histogram(aes(Number.Attendees),fill="blue",col="black",bins=30) 
# Description: Compared to the Number.Cars distribution, Number.Attendees is more spread out, but still has an upward trend to the right. It is also important to note that the data from Number.Attendees has been filtered for indicies equal to "NA", which represent away-game data.

#Q4: 
# The “gaps” along the x-axis are a result in special events occurring Dodger Field. For example, say the Dodgers have "Bobble-Head Day" where free Bobble-Heads are given away to first 15,000 attendees present at the stadium. Therefore, Number.Attendees would increase to a higher level compared to days without special events. 

```
---
Question 5, 6 & 7: 
Make a summary plot of DayType and calculate the observed frequencies of each value.
Do the observed proportions for the DayType variable above match your expectations? Why or why not? As a quick check make a line plot of Day.number by DayType and explain (Hint: Use the function as.numeric() to force ggplot to treat DayType as a quantitative variable, instead of a categorical one, in your plot).
Make a summary plot of GameDay and calculate the observed frequencies of each value.
---
```{r}
#Q5
#Description: I figured since the variable DayType is categorical (either weekday or weekend), a bar graph would work best.  
# Now, to find frequency I did the following:
# > traffic %>% count(DayType)
# -> 1 Weekday   131
# -> 2 Weekend    43

ggplot(traffic) + geom_bar(aes(DayType,col=DayType))
traffic %>% count(DayType)

ggplot(traffic) + geom_line(aes(x=DayType, y=Day.number))

#Q6
# The observed proportions for the DayType variable match our expectations as there are more weekdays in the week (5/7) then weekend-days. Lets represent the odds of a weekday as (5/7 = 71%) and multiply it by the total no. of recorded games (174 games). This gives us (174 games * 71%) = 123 games on weekdays which is not far from 131 games which makes logical sense.

#Q7
#Description: GameDay is either "No Game" or "Game" -> Bar Chart
# Per frequencies, I did the same thing for Q5
ggplot(traffic) + geom_bar(aes(GameDay,col=GameDay))
traffic %>% count(GameDay)



```

### Confidence Intervals
# For each of the questions below, print out your point estimate, standard error, and final confidence interval. And be sure to think carefully about what the right n is.
---
Question 1:
Make an 85% confidence interval for the mean number of attendees at a Dodgers game and provide an appropriate interpretation.
---
```{r}
#Q1 
# Step 1) filter data -> temp <- filter(traffic, traffic$Number.Attendees != "NA")
# Variable = Number.Attendees
# sample mean = 44489.88 -> mean(temp$Number.Attendees) = 44489.88
# std. dev -> sd(temp$Number.Attendees) = 7058.582
# sample size -> length(temp$Number.Attendees) = 81
# ave +- z *se

temp <- filter(traffic, traffic$Number.Attendees != "NA")
point_estimate1 <- mean(temp$Number.Attendees)
sample_size <- length(temp$Number.Attendees)
st_dev <- sd(temp$Number.Attendees)
m_o_e <- qnorm(0.925) * st_dev/sqrt(sample_size)
left <- point_estimate1 - m_o_e
right <- point_estimate1 + m_o_e

# Interpretation: There lies an 85% chance (0.85) that our calculated confidence interval contains the mean, where in this case the mean = 44489.87 and the interval = (43360.87, 45618.88)

```

---
Question 2:
Recreate this confidence interval with a 99.99% confidence level. How do these intervals compare? Is this what you expect? Explain.
---
```{r}
#Q2 
# Variable = Number.Attendees
# sample mean = 44489.88 -> mean(temp$Number.Attendees) = 44489.88
# std. dev -> sd(temp$Number.Attendees) = 7058.582
# sample size -> length(temp$Number.Attendees) = 81
# ave +- z *se

temp <- filter(traffic, traffic$Number.Attendees != "NA")
point_estimate2 <- mean(temp$Number.Attendees)
sample_size <- length(temp$Number.Attendees)
st_dev <- sd(temp$Number.Attendees)
m_o_e <- qnorm(0.9995) * st_dev/sqrt(sample_size) #double check z score
left <- point_estimate2 - m_o_e
right <- point_estimate2 + m_o_e

# How do these intervals compare? Is this what you expect? Explain.
# Interpretation: While comparing the 99.99% confidence interval to the 85% confidence interval, we noticed that the 99.99% confidence interval provided an interval with a larger range. Specifically, our 99.99% confidence interval ranged from (41909.15,47070.59). This is what we expected because the 99.99% CI provides a higher probability that the mean will mean will be found within the interval.
```

---
Question 3:
Make an 80% confidence interval for the proportion of days with Dodgers games and provide an appropriate interpretation.
---
```{r}
#Q3 total number of days = 174, where gamesPlayed = 81 -> 81/174 = 81/174

gameProportion = 81/174 # point estimate
sample_size <- length(traffic$Day.number)
st_dev <- sqrt((gameProportion*(1-gameProportion))/length(traffic$Day.number))
m_o_e <- 1.282 * st_dev/sqrt(sample_size)
left <- gameProportion - m_o_e
right <- gameProportion + m_o_e

# Interpretation: This confidence interval displays an 80% probability that the confidence interval calculated includes the proportion of days with Dodgers games.
```

---
Question 4:
Suppose a friend of yours looks at this dataset and asks you to make a 95% confidence interval for the proportion of home games that the Dodgers won during the 2005 season. Could you and/or should you do this? Explain.
---
```{r}
# You COULD find the 95% confidence interval for the proportion of home games that the Dodgers won during the 2005 season. However, we believe that a calculation like this would be meaningless. From season to season, many different variables and factors change which imply that even though this data COULD be applied to future seasons, it would NOT be accurate, most likely.
```
---
Question 5:
Now your friend wants a confidence interval for the proportion of ALL games that the Dodgers won during the 2005 season. Could you and/or should you do this? Explain.
---
```{r}
# No, you cannot calculate a confidence interval for the proportion of ALL games that the Dodgers won during the 2005 season because the dataset provided ONLY applies to home game data. Even if you could calculate this confidence interval, the CI would be less accurate then the first with the addition of variables from away games.
```

### Hypothesis Test
### In the next lab, you will perform a hypothesis test using this data. Prior to performing this test, you’ll do some research to determine what would be good null and alternative hypotheses.

```{r}
# Ho: u1 - u2 = 0                
# Ha: u1 - u2 != 0

("A Comparison of Weekend and Weekday TravelBehavior Characteristics in Urban Areas")["https://scholarcommons.usf.edu/cgi/viewcontent.cgi?referer=https://www.google.com/&httpsredir=1&article=1935&context=etd"]
```



