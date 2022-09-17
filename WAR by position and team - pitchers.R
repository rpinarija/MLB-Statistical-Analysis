## Read in data file
data.pitch <- read.csv("Pitching WAR.csv")


############################################################
## Stacked Bar Plots for SP/RP WAR by Team and Year       ##
############################################################

## create data file that total WAR by SP & RP
Team.Totals.SP.RP <- aggregate(WAR ~ Team + Year + Position, 
                               data = data.pitch
                               , sum)

library(ggplot2)
library(viridis)
library(hrbrthemes)

#2019 plot
Team.Totals.SP.RP.2019 <- subset(Team.Totals.SP.RP,Year == "2019")

ggplot(Team.Totals.SP.RP.2019, aes(fill=Position, 
                                   y=WAR, x = reorder(Team, WAR))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Team Pitching WAR - 2019") +
  coord_flip()+
  theme_ipsum() +
  xlab("")


#2018 plot
Team.Totals.SP.RP.2018 <- subset(Team.Totals.SP.RP,Year == "2018")

ggplot(Team.Totals.SP.RP.2018, aes(fill=Position, 
                                   y=WAR, x = reorder(Team, WAR))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Team Pitching WAR - 2018") +
  coord_flip()+
  theme_ipsum() +
  xlab("")


#2017 plot
Team.Totals.SP.RP.2017 <- subset(Team.Totals.SP.RP,Year == "2017")

ggplot(Team.Totals.SP.RP.2017, aes(fill=Position, 
                                   y=WAR, x = reorder(Team, WAR))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Team Pitching WAR - 2017") +
  coord_flip()+
  theme_ipsum() +
  xlab("")


#2016 plot
Team.Totals.SP.RP.2016 <- subset(Team.Totals.SP.RP,Year == "2016")

ggplot(Team.Totals.SP.RP.2016, aes(fill=Position, 
                                   y=WAR, x = reorder(Team, WAR))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Team Pitching WAR - 2016") +
  coord_flip()+
  theme_ipsum() +
  xlab("")



#2015 plot
Team.Totals.SP.RP.2015 <- subset(Team.Totals.SP.RP,Year == "2015")

ggplot(Team.Totals.SP.RP.2015, aes(fill=Position, 
                                   y=WAR, x = reorder(Team, WAR))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Team Pitching WAR - 2015") +
  coord_flip()+
  theme_ipsum() +
  xlab("")




##################################################
## setting up regression data file              ##
#################################################

library(dplyr)

## set up indicator variables for starters/relievers and righties/lefties
data.pitch <- data.pitch %>% mutate( SP = ifelse(Position == "SP", 1, 0))
data.pitch <- data.pitch %>% mutate( RP = ifelse(Position == "RP", 1, 0))

data.pitch <- data.pitch %>% mutate( Righty = ifelse(Handedness == "R", 1, 0))
data.pitch <- data.pitch %>% mutate( Lefty = ifelse(Handedness == "L", 1, 0))

## set up WAR fields for for starters/relievers and righties/lefties
data.pitch$SP.WAR <- data.pitch$SP*data.pitch$WAR
data.pitch$RP.WAR <- data.pitch$RP*data.pitch$WAR
data.pitch$SP.WAR.R <- data.pitch$SP*data.pitch$WAR*data.pitch$Righty
data.pitch$SP.WAR.L <- data.pitch$SP*data.pitch$WAR*data.pitch$Lefty
data.pitch$RP.WAR.R <- data.pitch$RP*data.pitch$WAR*data.pitch$Righty
data.pitch$RP.WAR.L <- data.pitch$RP*data.pitch$WAR*data.pitch$Lefty


###############################################################
## compute average "pitching age" by team, year, SP/RP & L/R ##
###############################################################

data.pitch$Age.IP <- data.pitch$Age*data.pitch$IP

## total innings as starters or relievers for each team
Team.Innings.Pos <- as.data.frame(aggregate(IP ~ Team + Year + Position + Handedness, 
                                            data = data.pitch, sum))
colnames(Team.Innings.Pos) <- c("Team","Year","Position","Handedness", "Team.Inn.Pos")

## merge innings by position into original file
data.pitch <-merge(data.pitch, Team.Innings.Pos, by.x = c("Team","Year","Position",
                                                          "Handedness"),
                   by.y = c("Team","Year","Position", "Handedness"), all.x = TRUE)

## compute each player's contribution to age as starter or reliever
data.pitch$Age.Pos <- data.pitch$Age.IP/data.pitch$Team.Inn.Pos

## aggregate WAR and Age by team, year, SP/RP, L/R
r <- as.data.frame(aggregate(cbind(WAR,Age.Pos) ~ Team + Year + Position + Handedness, 
                             data = data.pitch, sum))

## merge in winning pct.
team <- read.csv("Team Totals.csv")
t <- team[,c(1,2,5)]
r <- merge(r, t, by.x = c('Team', 'Year'), 
           by.y = c('Team', 'Year'))

write.csv(r, file = "pitchers - regression.csv")


## old regression file that total WAR by SP / RP / lefty / righty
r1 <- aggregate(cbind(WAR,SP.WAR,RP.WAR,SP.WAR.R,SP.WAR.L,RP.WAR.R,RP.WAR.L) ~ 
                  Team + Year, 
                data = data.pitch, sum)
r2 <- aggregate(cbind(Win.Percentage,Age.Pos) ~ Team + Year, data = r, mean)

r <- cbind(r1,r2[3:4])

########################################
## Descriptive Plots                  ##
########################################

# histogram of response - winning pct.
hist(r$Win.Percentage, main = "Winning Pct.")

# scatterplot and correlation matrices by year

r.2015 <- subset(r,Year == "2015")
pairs(r.2015[,-c(1,2)], main = "2015")
round(cor(r.2015[,-c(1,2)]),2)

r.2016 <- subset(r,Year == "2016")
pairs(r.2016[,-c(1,2)], main = "2016")
round(cor(r.2016[,-c(1,2)]),2)

r.2017 <- subset(r,Year == "2017")
pairs(r.2017[,-c(1,2)], main = "2017")
round(cor(r.2017[,-c(1,2)]),2)

r.2018 <- subset(r,Year == "2018")
pairs(r.2018[,-c(1,2)], main = "2018")
round(cor(r.2018[,-c(1,2)]),2)

r.2019 <- subset(r,Year == "2019")
pairs(r.2019[,-c(1,2)], main = "2019")
round(cor(r.2019[,-c(1,2)]),2)

###########################################3
## Regression                             ##
############################################

fiti <- lm(Win.Percentage ~ SP.WAR.R + SP.WAR.L + 
             RP.WAR.R + RP.WAR.L + Age.Pos, 
           data = r)
summary(fiti)
anova(fiti)
extractAIC(fiti)
x <- as.data.frame(coef(fiti))
round(confint(fiti),4)
fit.value <- fitted(fiti)
resid.value <- residuals(fiti)
y <- as.data.frame(cbind(fit.value,resid.value))
hist(y$resid.value)
plot(y$fit.value,y$resid.value)

############################################
## Repeated K fold cross validation       ##
############################################
library(tidyverse)
library(caret)

set.seed(123)

# 10 folds and 3 repeats
# reports mean error for all repeats
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(Win.Percentage ~ SP.WAR.R + SP.WAR.L + 
                 RP.WAR.R + RP.WAR.L + Age.Pos, 
               data = r, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

###############################################################
## Bootstrapping CIs for coefficients                        ##
###############################################################

library(dplyr)
R<- 999

reps<- matrix(,nrow = R, ncol = 6)
n <- nrow(r) 
for (i in 1:R) {
  
  k<-sample_n(r,150,replace=TRUE)
  fiti <- lm(Win.Percentage ~ SP.WAR.R + SP.WAR.L + 
               RP.WAR.R + RP.WAR.L + Age.Pos, 
             data = k)
  reps[i,] <- as.numeric(coef(fiti))
}

b0 <- c(quantile(reps[,1],0.025),quantile(reps[,1],0.975))
b1 <- c(quantile(reps[,2],0.025),quantile(reps[,2],0.975))
b2 <- c(quantile(reps[,3],0.025),quantile(reps[,3],0.975))
b3 <- c(quantile(reps[,4],0.025),quantile(reps[,4],0.975))
b4 <- c(quantile(reps[,5],0.025),quantile(reps[,5],0.975))
b5 <- c(quantile(reps[,6],0.025),quantile(reps[,6],0.975))

CIs <- as.data.frame(rbind(b0,b1,b2,b3,b4,b5))
rownames(CIs) <- c("intercept","SP.WAR.R","SP.WAR.L","RP.WAR.R",
                   "RP.WAR.L","Avg.Age")
CIs
