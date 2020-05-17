#remove everything in the global environment and setting the working directory
rm(list = ls())
setwd("~/Desktop")

#loading the data into excel
library(readxl)
BregmanStats <- read_excel("BregmanStatistics.xlsx")

#creating a scatterplot of Bregman's predicted career performance, by estimating his WRC+ each season over the next eight years of his career, which (according to Bill James' online formula) looks to predict the remaining length of an MLB player's career
library(ggplot2)
head(BregmanStats)
ggplot(BregmanStats, aes(x=AGE, y=OBPAVG)) + geom_point() + ggtitle("Alex Bregman Career Trajectory (Age vs. OBPAVG)") + xlim(22,32.5)

#creating a density plot to show Bregman's expected wOBA output by density - I didn't inlude this in my visualization presentation because I anayzed Bregman's WRC+ as my performance metric of choice during his career, but through this chart, we can see that Bregman's wOBA is predicted to concentrate between 0.350 and 0.360, based on the performance of the other nine similar player I looked at in determining Bregman's future statistics
fill <- "blue"
line <- "red"
ggplot(BregmanStats, aes(x=wOBAAVG)) + geom_density() + ggtitle("Alex Bregman Career Trajectory by Density Plot") +
  geom_density(fill = fill, colour = line,
               alpha = 0.4)

#creating a density plot for Bregman's WRC+; Bregman is predicted to have a WRC+ total between 115 and 125 most often during the rest of his career, according to my models - I chose not to include this chart in my visualization because it did not look as visually appealing as my scatterplot of his performance, even though this chart still reveals interesting information
fill <- "orange"
line <- "blue"
ggplot(BregmanStats, aes(x=WRCPlusAVG)) + geom_density() + ggtitle("Alex Bregman Career Trajectory by Density Plot") +
  geom_density(fill = fill, colour = line,
               alpha = 0.6)

#building a linear model (woohoo!) in an effort to see what predicts avrage WRC+ best; Among all predictors, wOBAAVG (the average wOBA total for each season between ages 24 - 32 for each of the nine other players I looked at) had the strongest ability to predict average WRC+
BregmanMod <- lm(log(WRCPlusAVG) ~ wOBAAVG, BregmanStats) 
BregmanMod2 <- lm(WRCPlusAVG ~ ., BregmanStats)
plot(BregmanMod2)
summary(BregmanMod)

#wOBAAVG is the strongest factor for predicting WRCPlusAVG by itself, as the R-squared value in this model is 0.89. There are other combinations of factors which form models with a stronger relationship, but none as powerful where wOBAAVG is still a significant predictor. I chose to have WRC as my metric for measuring Alex Bregman's offensive production because it is the best metric for evaluating a hitter's offensive impact (at least in my opinion, and through Fangraphs' WRC+ formula, it's clear to see how well the statistic evaluates players based on the period they're playing in, where they are playing, and is much better than just WRC). I chose to include wOBA, SLG, BA, and OBP as predictors for WRC because these were four factors which I thought contributed most to a player's WRC+, and I took the average of the values for nine similar players to Alex Bregman according to Baseball Reference and used their average totals for nine years of their career here. I focused on eight years, from 24-32 in a player's career, based on Bill James' formula for expected length of career of a player, which is (42-age)/2  = 9.5, for Bregman, and will be 9 at the end of the month. Thus, these average values provide an interesting prediction for Bregman's next nine years in the majors, if he stays healthy

#Below are the summary metrics for my Bregman model above - note the multiple R-squared value (0.89)

#summary(BregmanMod)
#
#Call:
#  lm(formula = WRCPlusAVG ~ wOBAAVG, data = BregmanData)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4.7003 -0.8981 -0.2836  1.7517  3.4252 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -99.38      28.45  -3.494 0.010079 *  
#  wOBAAVG       611.78      80.06   7.641 0.000122 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.642 on 7 degrees of freedom
#Multiple R-squared:  0.8929,	Adjusted R-squared:  0.8777 
#F-statistic: 58.39 on 1 and 7 DF,  p-value: 0.000122

