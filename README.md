# Stat-120-Final-Project
---
Final Project\
Nathan Zhang\
2024-02-20
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(CarletonStats)
```
##Tempo vs popularity
We are creating a scatterplot for tempo vs. popularity, as well as the residual line and interpreting the line of best fit and the strength of the correlation to see what relationship, if any, exists between the explanatory tempo and the response popularity.
```{r}
# read data
# create filtered data frame without value o or less for hotttnesss and song.tempo
# print the counts for the filtered data
# create scatterplot
# create residual plot
# make the bootstrap interval for the correlation
music <- read.csv("https://corgis-edu.github.io/corgis/datasets/csv/music/music.csv")
mus <- music%>% filter(song.tempo > 0, song.hotttnesss>0)
count(mus)
ggplot(mus, aes(x = song.tempo, y = song.hotttnesss)) + 
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot for Tempo and Popularity", x = "Tempo (BPM)", y = "Popularity")
library(broom)
mus.lm <- lm(song.hotttnesss ~ song.tempo, data = mus)
summary(mus.lm)
mus.aug <- augment(mus.lm)
ggplot(mus.aug, aes(x = song.tempo, y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residual Plot for Tempo and Popularity", x = "Tempo (BPM)", y = "Residual")
bootCor(song.hotttnesss ~ song.tempo, data = mus)
```
## Tempo vs. Popularity Graph Summary of Results
There doesn't seem to be an obvious curve fit for this data. Attempts were made to transform the data by setting the x and/or y values to log counterparts; however, no meaningful fit was discovered. The line of best fit was found to be popularity = 4.152e-01 + 3.551e-04(tempo). It is very clumpy and doesn't have a very noticeable slope, although the line of best fit gives the slope at 3.551e-04, which is a very small positive slope. The r value for this line of best fit is at 0.0726. Further analysis will interpret the slope and intercept of the line of best fit and the r value. There will also be analysis into the uncertainty and the reliability of using this line of best fit.

## Duration vs. Popularity 
```{r}
music_clean <- music%>% filter(song.duration != 0, song.hotttnesss > 0) # Removes null values for song duration and song popularity

min(music_clean$song.duration) # verifies that the minimum duration isn't 0, which would be a null value
cor(music_clean$song.hotttnesss, music_clean$song.duration)# gives correlation coefficient
ggplot(music_clean, aes(x = song.duration, y = song.hotttnesss)) + geom_point() #plots scatterplot
ggplot(music_clean, aes(x = song.duration, y = log(song.hotttnesss))) + geom_point() # log transformation
ggplot(music_clean, aes(x = song.duration, y = sqrt(song.hotttnesss))) + geom_point() # square root transformation

