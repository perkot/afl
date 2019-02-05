#-----------
#LOAD IN DEPENDENCIES
#-----------

library(readr)
library(tidyr)
library(dplyr)
library(anytime)
library(ggplot2)
library(plotly)
library(scales)
library(data.table)
library(kable)
library(knitr)
library(reshape)

#-----------
#LOAD IN DATA
#-----------

#Load in data URL and read to R, specifying columns and delimiters
url <- 'https://afltables.com/afl/stats/biglists/bg3.txt'

AllAFLData <- read_table(url, col_names = c("ID", "Date", "Round", "HomeTeam", "HomeScore", 
                                            "AwayTeam", "AwayScore", "Venue"), 
                         col_types = NULL, skip = 2)

#-----------
#CLEAN DATA, EXTRACT FEATURES
#-----------

#Convert to dataframe 
AllAFLData <- data.frame(as.list(AllAFLData))

AllAFLData <- data.frame(AllAFLData, stringsAsFactors = FALSE) 

str(AllAFLData)

#Change datatype of home score to character
AllAFLData$HomeScore <- as.character(AllAFLData$HomeScore)
AllAFLData$HomeTeam <- as.character(AllAFLData$HomeTeam)
AllAFLData$AwayTeam <- as.character(AllAFLData$AwayTeam)
AllAFLData$Date <- as.character(AllAFLData$Date)
AllAFLData$Date2 = AllAFLData$Date


#Separate date column into day, month, year
AllAFLData <- separate(AllAFLData, Date2, 
                       into = c("Day", "Month", "Year"), 
                       sep = "[-]")

AllAFLData$Day <- as.numeric(as.character(AllAFLData$Day))
AllAFLData$Year <- as.numeric(as.character(AllAFLData$Year))

#Separate score column into goals, behinds, and total 
AllAFLData <- separate(AllAFLData, HomeScore, 
                       into = c("HomeGoals", "HomeBehinds", "HomeTotal"), 
                       sep = "[.]")

#Separate score column into goals, behinds, and total 
AllAFLData <- separate(AllAFLData, AwayScore, 
                       into = c("AwayGoals", "AwayBehinds", "AwayTotal"), 
                       sep = "[.]")
#check all variable types 
sapply(AllAFLData, class)

#change newly created numeric columns to numeric 
cols.num <- c("HomeGoals","HomeBehinds", "HomeTotal", 
              "AwayGoals", "AwayBehinds", "AwayTotal")
AllAFLData[cols.num] <- sapply(AllAFLData[cols.num],as.numeric)

#Aggregate merged teams
AllAFLData[AllAFLData=="South Melbourne"] <- "South Melb/Syd"
AllAFLData[AllAFLData=="Sydney"] <- "South Melb/Syd"
AllAFLData[AllAFLData=="Fitzroy"] <- "Fitzroy/Brisbane"
AllAFLData[AllAFLData=="Brisbane Bears"] <- "Fitzroy/Brisbane"
AllAFLData[AllAFLData=="Brisbane Lions"] <- "Fitzroy/Brisbane"
AllAFLData[AllAFLData=="Kangaroos"] <- "North Melbourne"
AllAFLData[AllAFLData=="Footscray"] <- "Western Bulldogs"
AllAFLData[AllAFLData=="Western Bulldog"] <- "Western Bulldogs"
AllAFLData[AllAFLData=="GW Sydney"] <- "GWS"

#Break-down all AFL matches by decade
AllAFLData$Era <- ifelse(AllAFLData$Year>=1897 & AllAFLData$Year<=1899,"1890s",
                  ifelse(AllAFLData$Year>=1900 & AllAFLData$Year<=1909,"1900s",
                  ifelse(AllAFLData$Year>=1910 & AllAFLData$Year<=1919,"1910s",       
                  ifelse(AllAFLData$Year>=1920 & AllAFLData$Year<=1929,"1920s",
                  ifelse(AllAFLData$Year>=1930 & AllAFLData$Year<=1939,"1930s", 
                  ifelse(AllAFLData$Year>=1940 & AllAFLData$Year<=1949,"1940s",
                  ifelse(AllAFLData$Year>=1950 & AllAFLData$Year<=1959,"1950s",
                  ifelse(AllAFLData$Year>=1960 & AllAFLData$Year<=1969,"1960s",
                  ifelse(AllAFLData$Year>=1970 & AllAFLData$Year<=1979,"1970s",
                  ifelse(AllAFLData$Year>=1980 & AllAFLData$Year<=1989,"1980s",
                  ifelse(AllAFLData$Year>=1990 & AllAFLData$Year<=1999,"1990s",
                  ifelse(AllAFLData$Year>=2000 & AllAFLData$Year<=2009,"2000s",
                  ifelse(AllAFLData$Year>=2010 & AllAFLData$Year<=2019,"2010s", 0)))))))))))))

#Aggregate into a table summarising total number of games played, and total score per decade

#Total score per team, per decade for home games
EraScore <- AllAFLData %>%
  gather(HomeAway, Team, HomeTeam) %>% 
  group_by(Era, Team) %>%
  summarise(HomeTotal = sum(HomeTotal))

#Total score per team, per decade for away games
EraScore <- AllAFLData %>%
  gather(HomeAway, Team, AwayTeam) %>% 
  group_by(Era, Team) %>%
  summarise(AwayTotal = sum(AwayTotal)) %>%
  left_join(EraScore, 
            by = c("Team" = "Team", 
                   "Era" = "Era")) 

#Total number of home & away games per team 
EraScore<- AllAFLData %>%
  gather(HomeAway, Team, HomeTeam, AwayTeam) %>% 
  group_by(Era, Team) %>%
  summarise(GamesHome = sum(HomeAway == "HomeTeam"),
            GamesAway = sum(HomeAway == "AwayTeam")) %>% 
  left_join(EraScore, 
            by = c("Team" = "Team", 
                   "Era" = "Era")) 

#Total games per team, per decade
EraScore$TotalGames <- (EraScore$GamesHome + 
                          EraScore$GamesAway)
#Total score per team, per decade
EraScore$TotalScore <- (EraScore$HomeTotal + 
                          EraScore$AwayTotal)
#Average score per team, per decade
EraScore$AverageScore <- (EraScore$TotalScore / 
                            EraScore$TotalGames)
#round to 2 decimal places
EraScore[,'AverageScore']=round(EraScore[,'AverageScore'],2) 

#minimalistic theme for visualisation 
PlotlyTheme <-
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color='light grey'),
    plot.title = element_text(size=8, hjust=0.5),
    axis.title.x = element_blank(),
    axis.text = element_text(size=6),
    axis.text.x = element_text(angle = 45, hjust = 1, colour="#606060"),
    axis.text.y = element_text(hjust = 1, colour="#606060"),
    axis.title = element_text(size=8),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    legend.key = element_rect(fill = "white")+
      scale_y_continuous(label = comma)
  )

#animated plot, summarising every teams average score per decade
animatedscore <- EraScore %>% 
  ggplot(aes(x = Team, y = AverageScore)) + 
  geom_point(aes(frame = Era), colour="#3599B8") + 
  stat_summary(aes(y = AverageScore, group = 1, frame = Era), 
               fun.y=mean, colour="#3599B8", geom = "line", group = 1) +
  labs(title = "Average score per game, per decade for all AFL teams", 
       y = "Average score") +
  PlotlyTheme
ggplotly(animatedscore)

