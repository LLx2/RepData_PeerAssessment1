---
title: "PA1_template"
author: "Lon Lieberman"
date: "September 20, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## PEER GRADED ASSIGNMENT: COURSE PROJECT 1

## Library
```{r}
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(knitr)
library(markdown)
```
## Loading the Data
*Set working directory & how any code tht is needed load data*
```{r}
setwd("/Users/lonlieberman/Documents/R/Assignments/Data_Cleaning_Coursera/Week 4/Course Project 2/Reproducible Research/Knitr")
activity <- read_csv('activity.csv', 
                     col_types = cols(date = "D", 
                                      steps = "i",
                                      interval = "d"))
```
*Show any code to process the data and possibly transform it*
```{r echo = FALSE}
activity_df <- tbl_df(activity)
```
**What is mean total number of steps taken per day?**   
    
-- PART 1, Calculate steps  
```{r}
steps_sum <- activity_df %>%
        select(steps, date) %>%
        group_by(date) %>%
        summarize(stepSumz = sum(steps))

steps_df <- activity_df %>% 
        select(steps, date) %>%
        group_by(date) %>%
        summarize(stepMean = mean(steps)) %>%
        filter(stepMean >= 0)

summary(steps_sum$stepSumz)
```
Therefore, the mean steps taken each day is **10,770** & the median steps taken per day is **10,760**.
-- PART 2, Using ggplot2 build the histogram to show mean steps taken per day:
```{r}
stepsHisto <- ggplot(steps_df, aes(stepMean)) +
                    geom_histogram(
                            aes(y=..density..),
                            bins = 9, 
                            colour = "black",
                            fill = "white") +
                    coord_cartesian() +
                    ggtitle("Mean steps per day")+
                    xlab("Mean Steps") +
                    ylab("Density (of days)") +
                    theme_bw() +
                    geom_vline(aes(xintercept =mean(stepMean)),
                            color = "red",
                            linetype = "dashed",
                            size = 1) +
                    geom_density(
                            alpha = .2, 
                            fill = "#FF6666")
stepsHisto
```
-- Part 3, Calulate and report the mean and median of the total number of steps taken per day:  

Modify data set via dplyr
```{r}
stepMean <- activity_df %>%
        select(steps) %>%
        filter(steps >=0) %>%
        summarise(Mean = mean(steps)) %>%
        summarise_each(funs(mean))

stepMedian <- activity_df %>%
        select(steps) %>%
        filter(steps >= 1) %>%
        summarise(Median = median(steps)) %>%
        summarise_each(funs(median))

stepMeanAndMedian <- bind_cols(stepMean, stepMedian)
```
Using kable, format and display data into tables
```{r results = "asis"}
knitr::kable(x = stepMeanAndMedian,
       caption = "Mean & Median of Total # of Daily Steps",
       align = 'c')
```
  
**What is the average daily activity pattern?**  
-- Part 1, Using ggplot to create the time series plot
```{r}
fiveMinute <- ggplot(steps_df, 
        aes(date, stepMean, 
            colour = "FF6666")) +
        geom_line() +
        scale_x_date() +
        ggtitle("Five Minute Interval by Average Steps") +
        xlab("5 Minute Interval") +
        ylab("Avg Steps Taken Across All Days")
fiveMinute                
```   
-- Part 2  
```{r}
maxSteps <- activity_df %>%
        group_by(interval) %>%
        filter(steps >=0) %>%
        summarize(maxNumber = max(steps)) %>%
        filter(maxNumber > 0) %>%
        summarise_each(funs(max))
```
Using kable, format and display data as a table
```{r}
knitr::kable(x = maxSteps, caption = "5 Minute interval containing max steps", align = 'c')
```
  
**Imputing missing values**  
  
-- Part 1   
Calculate the total number of NAs ..   
   
Report the total number of missing values:  
Number of missing steps:  **`r sum(is.na(activity_df$steps))`**  
Proportion of missing values is **`r mean(is.na(activity_df))`**  

-- Parts 2 & 3    
Fill in missing data.   
```{r}
dailyMean <- activity_df %>%
        fill(steps, .direction = "up") %>%
        fill(steps, .direction = "down")
```
-- Part 4  
Sum steps by day  
```{r}
dailyMean_df <- dailyMean %>%
        select(steps, date) %>%
        group_by(date) %>%
        summarize(totalDaily = sum(steps))
```
Histogram of total # of steps each day   
```{r}
dmeanHisto <- ggplot(dailyMean_df, aes(totalDaily)) +
                    geom_histogram(
                            aes(y=..density..),
                            bins = 9, 
                            colour = "black",
                            fill = "white") +
                    coord_cartesian() +
                    ggtitle("Total steps per day")+
                    xlab("Total Steps") +
                    ylab("Density (of days)") +
                    theme_bw() +
                    geom_vline(aes(xintercept =mean(totalDaily)),
                            color = "red",
                            linetype = "dashed",
                            size = 1) +
                    geom_density(
                            alpha = .2,
                            fill = "#FF6666")

dmeanHisto
```
Reporting the mean and median total number of steps taken per day:
```{r}
summary(dailyMean_df$totalDaily)
```
  
Therefore, the mean steps taken each day is **9,354** & the median steps taken per day is **10,400**.     
These values **DO** differ from the ex-ante values.    
By imputing missing value we see the median # of steps *decrese* by *~13%* and the median value decrease by *~3.5%.**

**Are there differences in activity patterns between weekdays & weekends?**  

Create new factors "weekend" & "weekday" using the dataset dailyMean(dataset with filled in values)  
```{r}
fullWeek_df <- dailyMean
fullWeek_df$dayOfWeek <- weekdays(strptime(fullWeek_df$date, format = "%Y-%m-%d"))
fullWeek_df_copy <- fullWeek_df %>%
        mutate(dayOfWeekFactors = factor(1*(dayOfWeek == "Saturday" | dayOfWeek == "Sunday"), labels = c("weekday", "weekend")))
```
To calibrate factors for charting  
```{r}
weekdayMeanSteps <- fullWeek_df_copy %>%
        filter(dayOfWeekFactors =="weekday") %>%
        group_by(interval = factor(interval)) %>%
        summarise(date = "weekday", meanStep = mean(steps))
weekendMeanSteps <- fullWeek_df_copy %>%
        filter(dayOfWeekFactors =="weekend") %>%
        group_by(interval = factor(interval)) %>%
        summarise(date = "weekend", meanStep = mean(steps))
weekFactors <- rbind(weekdayMeanSteps, weekendMeanSteps)
```
Panel plot  
```{r}
panelPlot <- ggplot(weekFactors, aes(interval, meanStep))+
        geom_point(color = "red",
                   size = 0.75) +
        facet_grid(date ~.) +
        ggtitle("Panel Plot for Weekdays vs Weekends") +
        xlab("Interval") +
        ylab("Number of Steps") +
        theme_bw()
panelPlot
```
