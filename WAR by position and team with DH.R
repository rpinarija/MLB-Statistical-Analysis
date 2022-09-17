
## Read in data file
data <- read.csv("Positional WAR.csv")

#############################
## total innings           ##
#############################

## total innings for each player by team in a new data file
Innings <- as.data.frame(aggregate(INN ~ playerid + Team + Year, 
          data = data, sum))


colnames(Innings) <- c("playerid","Team","Year", "total.innings")

## merge total innings by team for each player into original data
m <- merge(data, Innings, by.x = c("playerid","Team","Year"), 
          by.y = c("playerid","Team","Year"), all.x = TRUE)


## convert WAR components (except Pos) to per inning
m$Bat.pos.team <- (m$INN/m$total.innings)*m$Bat
m$BsR.pos.team <- (m$INN/m$total.innings)*m$BsR
m$Lg.pos.team <- (m$INN/m$total.innings)*m$Lg
m$Rep.pos.team <- (m$INN/m$total.innings)*m$Rep
m$Fld.pos.team <- (m$INN/m$total.innings)*m$Fld


## create dataframe for positional fielding adjustments
x <- as.numeric(c("12.5", "-12.5", "2.5", "2.5", "7.5", "-7.5", "2.5", "-7.5", "-17.5"))
y <- c("C","1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH")
Pos.Adj <- as.data.frame(cbind(y,x))
colnames(Pos.Adj) <- c("Pos", "Pos.Adj")
Pos.Adj$Pos.Adj <- as.numeric(as.character(Pos.Adj$Pos.Adj))

## merge position adjustment constant into large data file
m <- merge(m, Pos.Adj, by.x = "Pos", by.y = "Pos", all.x = TRUE)

## convert position adjustment relative to innings played
# Possible this is where the the data isn't completely accurate - innings could be greater than 9
m$Pos.Adj.Inn <- ((m$INN/9)/162)*m$Pos.Adj

## add fielding by innings to Pos adjustment by innings
m$Fld.Pos.Adj <- m$Fld.pos.team + m$Pos.Adj.Inn

## create dataframe for seasonal WAR constants
x <- as.numeric(c("9.421", "9.778", "10.048", "9.714", "10.296"))
y <- as.numeric(c("2015","2016", "2017", "2018", "2019"))
season.WAR.constant <- as.data.frame(cbind(y,x))
colnames(season.WAR.constant) <- c("Year", "s.WAR.constant")

## merge seasonal WAR constant into large data file
m <- merge(m, season.WAR.constant, by.x = "Year", by.y = "Year", all.x = TRUE)

## total adjusted WAR by innings and position constant
m$WAR.adjust <- (m$Bat.pos.team + m$BsR.pos.team + m$Lg.pos.team +
                  m$Rep.pos.team + m$Fld.pos.team + m$Pos.Adj.Inn)/(m$s.WAR.constant)

write.csv(m,file = "m3.csv")
##############################################################
## total WAR, Bat, BsR, Fld.Pos.Adj by team                 ##
##############################################################


## create data file that total WAR, Bat, BsR, Fld, Age by team
Team.Totals <- aggregate(cbind(WAR.adjust,Bat.pos.team,BsR.pos.team, 
                               Fld.Pos.Adj) ~ Team + Year , data = m, sum)
colnames(Team.Totals) <- c("Team", "Year", "WAR", "Bat", "BsR", "Fld.Pos.Adj")
Team.Totals$WAR <- round(Team.Totals$WAR, digits = 1)
Team.Totals$Bat <- round(Team.Totals$Bat, digits = 1)
Team.Totals$BsR <- round(Team.Totals$BsR, digits = 1)
Team.Totals$Fld.Pos.Adj <- round(Team.Totals$Fld.Pos.Adj, digits = 1)

############################
## Plot WAR by Team       ##
############################

Team.Totals.2019 <- subset(Team.Totals,Year == "2019")

library(ggplot2)
ggplot(data=Team.Totals.2019, mapping = aes(x = reorder(Team, WAR), WAR)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=WAR), color="black", hjust = -0.3, size=3.0)+
  coord_flip()+
  ggtitle("Team WAR - 2019") +
  ylab("Total WAR") +
  xlab("Team")

############################
## Plot Bat by Team       ##
############################
library(ggplot2)
ggplot(data=Team.Totals.2019, mapping = aes(x = reorder(Team, Bat), Bat)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Bat), color="black", hjust = -0.3, size=3.0)+
  coord_flip()+
  ggtitle("Team Bat - 2019") +
  ylab("Total Bat") +
  xlab("Team")


############################
## Plot BsR by Team       ##
############################
library(ggplot2)
ggplot(data=Team.Totals.2019, mapping = aes(x = reorder(Team, BsR), BsR)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=BsR), color="black", hjust = -0.3, size=3.0)+
  coord_flip()+
  ggtitle("Team BsR - 2019") +
  ylab("Total BsR") +
  xlab("Team")


####################################
## Plot Fld.Pos.Adj by Team       ##
####################################
library(ggplot2)
ggplot(data=Team.Totals.2019, mapping = aes(x = reorder(Team, Fld.Pos.Adj), Fld.Pos.Adj)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Fld.Pos.Adj), color="black", hjust = -0.3, size=3.0)+
  coord_flip()+
  ggtitle("Team Fld.Pos.Adj - 2019") +
  ylab("Total Fld.Pos.Adj") +
  xlab("Team")




##############################################################
## Team WAR by Position & Handedness                        ##
##############################################################

## create data file that total WAR, Bat, BsR, Fld by team and position
t <- aggregate(cbind(WAR.adjust,Bat.pos.team,BsR.pos.team, 
                     Fld.Pos.Adj) ~ Team + Year + Pos + Handedness, data = m, sum)

colnames(t) <- c("Team", "Year", "Pos", "Handedness",
                 "WAR", "Bat", "BsR", "Fld.Pos.Adj")
t$WAR <- round(t$WAR, digits = 3)
t$Bat <- round(t$Bat, digits = 3)
t$BsR <- round(t$BsR, digits = 3)
t$Fld.Pos.Adj <- round(t$Fld.Pos.Adj, digits = 1)

write.csv(t,file = "t.csv")
##############################################################
## Avg. Team Playing Age at each position & Handedness      ##
##############################################################

## total innings played at each position for each team
Team.Innings.Pos <- as.data.frame(aggregate(INN ~ Team + Year + Pos + Handedness, 
                                            data = data, sum))
colnames(Team.Innings.Pos) <- c("Team","Year","Pos","Handedness", "Team.Inn.Pos")

## merge total team innings into original data
data <-merge(data, Team.Innings.Pos, by.x = c("Team","Year","Pos","Handedness"),
             by.y = c("Team","Year","Pos","Handedness"), all.x = TRUE)

## age contribution for each player at each position
data$Team.Pos.Age <- (data$INN/data$Team.Inn.Pos)*(data$Age)

## compute avg. age at each position for each team
Team.Age <- as.data.frame(aggregate(Team.Pos.Age ~ Team + Year + Pos + Handedness, 
                                    data = data, sum))

## merge average team age at each position into data
t <-merge(t, Team.Age, by.x = c("Team","Year","Pos","Handedness"), 
          by.y = c("Team","Year","Pos","Handedness"), all.x = TRUE)

write.csv(t,file = "t.csv")
##########################################
## write position data to file          ##
##########################################

write.csv(t,file = "players - regression.csv")

##################################################
## these charts combine all years              ###
##################################################

## stripcharts for Team WAR
stripchart(WAR ~ Pos,data = t, method = "jitter",
           pch = 1, las = 2, main = "Cumulative WAR by team & Position")

## boxplots for Team WAR
ggplot(t, aes(x=Pos, y=WAR)) + 
  geom_boxplot() +
  ggtitle("Team WAR by Position - 2019") +
  ylab("WAR") 

## boxplots for Team Bat
ggplot(t, aes(x=Pos, y=Bat)) + 
  geom_boxplot() +
  ggtitle("Team Bat by Position - 2019") +
  ylab("Bat") 

## boxplots for Team BsR
ggplot(t, aes(x=Pos, y=BsR)) + 
  geom_boxplot() +
  ggtitle("Team BsR by Position - 2019") +
  ylab("BsR") 

## boxplots for Team Fld.Pos.Adj
ggplot(t, aes(x=Pos, y=Fld.Pos.Adj)) + 
  geom_boxplot() +
  ggtitle("Team Fld by Position - 2019") +
  ylab("Fld.Pos.Adj") 

