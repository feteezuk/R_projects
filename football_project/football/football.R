#install.packages("tidyverse", type = "binary")
#install.packages("ggrepel", type = "binary")
#install.packages("ggimage", type = "binary")
#install.packages("nflfastR", type = "binary")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

#loads data play by play for 2019
data <- load_pbp(2018:2021)

str(data)

#week 4
# devin moss or singletary

data %>% 
  filter( season_type == 'REG', rush == 1 | pass == 1, !is.na(rushing_yards), !is.na(yards_gained) ) %>%
  group_by(rusher) %>%
  select(posteam,rusher_player_name, yards_gained, rushing_yards,rush_touchdown ) %>%
  summarize(
    yards_gained = mean(yards_gained),
    rushing_yards = mean(rushing_yards),
    rush_touchdown = sum(rush_touchdown),
    plays =n()
  )%>%
  arrange(-rush_touchdown) %>%
  filter(plays> 100) %>%
  head(20)

#check out mattisons stats
#check out zach moss stats
#Should I start Kirk Cousins or Aaron Rogers?
#K. Cousings has more passing yards and more touchdowns compared to Aaron Rogers this year.

# devin moss or singletary
data %>%
  filter(receiver_player_name=='Z.Moss' | receiver_player_name=='D.Chark', season == 2021,season_type=='REG',  !is.na(td_player_name), !is.na(passing_yards))%>%
  group_by(passer_player_name, receiver_player_name) %>%
  summarize(
    passing_yards = sum(passing_yards),
    series_success = sum(series_success),
    touchdown = sum(touchdown)
  ) %>%
  arrange(-touchdown, -passing_yards ) %>%
  head(20)

data %>%
  filter(season ==2021, season_type=='REG', !is.na(rushing_yards)) %>%
  group_by(rusher) %>%
  summarize(
    touchdown = sum(touchdown),
    rushing_yards = sum(rushing_yards),
    
  ) %>%
  arrange(-rushing_yards) %>%
  filter(rushing_yards> 150) %>%
  head(20)

#######
#Basics: how to look at your data
########

dim(data) #48034 rows and 372 columns
str(data[1:10]) #structure of data
names(data) #lists 372 variable names
View(data) #views all the data

data %>% #View 4 columns of data
  select(home_team, away_team, posteam, desc) %>%
  View()

data %>% #look at the first few rows (the “head”) of the data.
  select(posteam, defteam, desc, rush, pass) %>% 
  head()

#######
#HOW TO FILTER DATA
########

data %>% #filter for rush or pass plays
  filter(rush ==1 | pass ==1) %>%
  select(posteam, desc, rush, pass, name, passer, rusher, receiver) %>% 
  head()

data %>% #Find special teams plays
  filter(special_teams_play ==1) %>%
  select(down, ydstogo, desc) %>% 
  head()

data %>% #find 4th down plays
  filter(down ==4) %>%
  select(down, ydstogo, desc) %>% 
  head()

data %>% #find 4th down plays
  filter(down ==4 & special_teams_play==0 ) %>%
  select(down, ydstogo, desc) %>% 
  head()

#!is.na(epa) means to exclude plays with missing (na) EPA. 
pbp_rp <- data %>% 
  filter(rush==1 | pass==1, !is.na(epa))

##########
#Group by and Summarize
##########

pbp_rp %>%
  filter(posteam =="DAL", rush ==1, play_type=='run') %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa),
    success_rate = mean(success),
    ypc = mean(yards_gained),
    plays = n()
  )%>%
arrange(-mean_epa) %>%
filter(plays > 20)
  
pbp_rp %>%
  filter(play_type == "run", rush ==1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa),
    success_rate = mean(success),
    ypc = mean(yards_gained),
    td = length(touchdown),
    plays = n()
  ) %>%
  arrange(-td)

head(pbp_rp$touchdown)

twentyone%>%
  filter(play_type == "run", rush ==1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa),
    success_rate = mean(success),
    ypc = mean(yards_gained),
    rushing_att = n(),
    td = sum(touchdown)
  ) %>%
  filter(rushing_att>200) %>%
  arrange(-success_rate)


#most_td_by_rusher 
twentyone%>%
  filter(play_type =="run",  touchdown==1) %>%
  group_by(rusher) %>%
  summarise(td=n()) %>%
arrange(-td) 

#########
#CREATE NEW COLUMNS WITH MUTATE
#########

#adds new column home. 1 if it's a home team, 0 if away team
pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  select(posteam, home_team, home) %>%
  head(10)

pbp_rp %>%
  filter(!is.na(cp)) %>%
  mutate(
    depth = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep"
    )
  ) %>%
  group_by(depth) %>%
  summarize(cp = mean(cp))

schotty <- pbp_rp %>%
  filter( wp > .20 & wp<.80 & down <= 2 & qtr <= 2 & half_seconds_remaining >120) %>%
  group_by(posteam) %>%
  summarise(mean_pass = mean(pass),
            plays = n()
            ) %>%
  arrange(-mean_pass)
  

ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))

#############
#LOAD MULTIPLE SEASONS
###########

#get data for 2018,2019,2020
pbp <- load_pbp(2018:2020) 

pbp %>% #check how many plays per season 
  group_by(season) %>%
  summarise(n=n())


#############
#CHECK QB stats
###########

qbs <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa), #Expected points added 
    cpoe = mean(cpoe, na.rm = T), 
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 1000)

#The "team_colors_logos" dataframe is provided in the nflfastR package
head(teams_colors_logos)

##########
#LEFT JOIN
##########



#left_join means keep all the rows from the left dataframe (the first one provided, qbs),
#and join those rows to available rows in the other dataframe
qbs <- qbs %>%
  left_join(teams_colors_logos, by = c("team" = 'team_abbr'))

qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbs$n_plays / 45000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2015 - 2019",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


###########
#CREATE A WIN TOTAL MODEL
#############
install.packages("nflreadr")
library(nflreadr)
games <- nflreadr::load_schedules()
str(games)

head(games)

#create a dataframe where each row is a team-season observation,
#listing how many games they won.

#get home team results, 
home = games %>%
  filter(game_type=='REG') %>%
  select(season, week, home_team, result) %>%
  rename(team=home_team) 
  
#get away game results - > flip results because u get home team results
away <- games %>%
  filter(game_type=='REG') %>%
select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result) 

#bind the away and home team results with new win column
results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(
    win = case_when(
      result >0 ~ 1,
      result < 0 ~ 0,
      result ==0 ~ 0.5
    )
  )

results %>% filter(season == 2019 & team == 'SEA')

#get team wins by season
#Use group by and summarize together
team_wins <- results %>%
 group_by(season, team) %>%
  summarize(
    wins = sum(win),
    point_diff = sum(result)
  ) 

#Show wins 
  team_wins %>%
    arrange(-wins) %>%
    head(5)

names(data)
str(data)
View(data)

#Which qb had the most td's? most first down passes? 

data %>%
  filter( season ==2020,play_type=='pass', season_type=='REG', !is.na(passing_yards),!is.na(receiver_player_name), !is.na(first_down), !is.na(first_down_pass), !is.na(touchdown) ) %>%
  group_by(passer, receiver_player_name) %>%
  summarize(
    pass_attempt = sum(pass_attempt),
    pass_yards = sum(passing_yards),
    #first_down_pass = sum(first_down_pass),
    #first_down = sum(first_down),
    touchdown = sum(touchdown),
    air_yards = sum(air_yards),
   # yards_after_catch = mean(yards_after_catch),
    yards_gained = mean(yards_gained)
    #pass_touchdown = sum(pass_touchdown),
    #rush_touchdown = sum(rush_touchdown),
    
  ) %>%
 filter(pass_attempt > 60, pass_yards >1000)%>%
  arrange(-touchdown)

#Which passer throws the most TD's? most 
data %>%
  filter( season ==2019,play_type=='pass', season_type=='REG', !is.na(passing_yards),!is.na(receiver_player_name), !is.na(first_down), !is.na(first_down_pass), !is.na(touchdown) ) %>%
  group_by(passer) %>%
  summarize(
    pass_attempt = sum(pass_attempt),
    pass_yards = sum(passing_yards),
    #first_down_pass = sum(first_down_pass),
    #first_down = sum(first_down),
    touchdown = sum(touchdown),
    air_yards = sum(air_yards),
    
    yards_gained = mean(yards_gained)
   
    
  ) %>%
  filter(pass_attempt > 60, pass_yards >1000)%>%
  arrange(-touchdown)

table(data$play_type)

#best kicker for fantasy?
data %>%
  filter(season ==2019 | season==2020, play_type=='field_goal', season_type=='REG', !is.na(field_goal_attempt), !is.na(field_goal_result)) %>%
  group_by(kicker_player_name, field_goal_result	) %>%

  summarize(
    field_goal_attempt = sum(field_goal_attempt),
    kick_distance = mean(kick_distance)
  ) %>%
  filter(field_goal_result =='made', field_goal_attempt>50 ) %>%
  arrange(-field_goal_attempt)

#which team is most rush heavy in 2018, 2019, 2020?

data %>%
  filter(season==2018 | season==2019 | season ==2020, season_type=='REG', !is.na(rushing_yards), !is.na(pass_touchdown)) %>%
  group_by(posteam) %>%
  
  summarize(
    rush_attempt = sum(rush_attempt),
    rush_touchdown = sum(rush_touchdown),
    rushing_yards = sum(rushing_yards),
    pass_touchdown = sum(pass_touchdown),
    
  ) %>%
  filter(rush_attempt>200, rushing_yards>1500) %>%
  arrange(-rush_attempt)

# Which rushers are the rush leaders each year?

corit <- data %>%
  filter(season ==2018 | season ==2019 | season ==2020,  season_type=='REG' ) %>%
  group_by(rusher_player_name) %>%
  summarize(
    rush_attempt = sum(rush_attempt),
    yards_gained = mean(yards_gained),
    yards_gained_sum = sum(yards_gained),
    td = sum(touchdown),
    
  ) %>%
  arrange(-td)
 
  
 corit<-data %>%
  filter(season ==2018 | season ==2019 | season ==2020,  season_type=='REG',!is.na(first_down_rush),!is.na(yards_gained), !is.na(rusher_jersey_number),!is.na(rush_attempt),!is.na(rush_touchdown) ,!is.na(lateral_rush) ) %>%
   select(first_down_rush,yards_gained, rusher_jersey_number,rush_attempt, rush_touchdown, total_home_rush_epa, total_away_rush_epa, lateral_rush) 

 #first_down_rush is 33% correlated with rush_touchdown
 #first_down_rush is 50% correlated with yards_gained  
 #yards_gained is 11% correlated with rush attemptes
 
 cor(corit)
 
 ggplot(corit, aes(x=first_down_rush))+
   geom_histogram()
 ggplot(corit, aes(x = rush_touchdown))+
   geom_histogram()
 ggplot(corit, aes(x=yards_gained)) +
   geom_histogram()
 
 #Who is the most popular first_down rusher?
 #look for first_down_rush person because they are corelated with 
 #touchdowns and yards gained.
 data %>%
   filter(season==2018 | season==2019 | season==2020, season_type=='REG', !is.na(rushing_yards), !is.na(pass_touchdown)) %>%
   group_by(rusher_player_name, posteam) %>%
   
   summarize(
     first_down_rush = sum(first_down_rush),
     rush_attempt = sum(rush_attempt),
     yards_gained = mean(yards_gained),
     rush_touchdown = sum(rush_touchdown),
     rushing_yards = sum(rushing_yards),
     pass_touchdown = sum(pass_touchdown),
     
   ) %>%
   filter(rush_attempt>200) %>%
   arrange(-rush_touchdown)
 

 #who is responsible for best defense
 
 data %>%
   filter(season==2020, season_type=='REG') %>%
   group_by(defteam) %>%
   summarize(
     def_wp = mean(def_wp)
   
   ) %>%
   arrange(-def_wp)
