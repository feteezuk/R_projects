install.packages("nflfastR")
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggimage")
install.packages('scales')
install.packages('ggplot2')
install.packages("dplyr")
install.packages("ggimage")

library(ggimage)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)


options(scipen = 9999) #prefer not to display numbers in scientific notation

#read in data
data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

dim(data)#48034 rows and 370 colums

str(data[1:10]) #look at the 1st 10 columns

colnames(data) #look at column names
View(data)

data %>%
  select(home_team, away_team, posteam, desc) %>%
 head()

data %>% 
  select(posteam, defteam, desc, rush, pass) %>% 
  head()

data %>% #select offensive team, defensive team rush , pass
  select(posteam, defteam, desc, rush, pass) %>% 
  head()

data %>%  #filter for run plays OR pass play
  filter(rush == 1 | pass == 1) %>%
  select(posteam, desc, rush, pass, name, passer, rusher, receiver) %>% 
  head()

data %>% #filtering for kicks, punts, field goals
  filter(special == 1) %>%
  select(down, ydstogo, desc) %>% 
  head()

head(data$epa)

data %>% #look at 4th down plays that are not special teams plays or field goals.
  filter(down == 4 & special == 0) %>%
  select(down, ydstogo, desc) %>% 
  head()

pbp_rp <- data %>% #filter for pass or rush yards without NA values for epa(expected points added)
  filter(rush == 1 | pass == 1, !is.na(epa))

head(pbp_rp)


pbp_rp %>%
  filter(posteam == "DAL", rush == 1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), 
    success_rate = mean(success), 
    ypc = mean(yards_gained), 
    plays = n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20)

colnames(pbp_rp)

pbp_rp %>%
  mutate(#create a new column postteam, and add 1 if it is home_team.
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  select(posteam, home_team, home) %>%
  head(10)

pbp_rp %>% #Calculating home team as 1 if posteam = home_team
  mutate(  #then group by home
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  group_by(home) %>%
  summarize(epa = mean(epa))

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


#Cowboys’ running backs fared on run plays in 2019:
pbp_rp %>%
  group_by(rusher) %>%
  filter(rush==1, posteam=="DAL") %>% #specify dalas as team, and only accept run plays
  summarize (
    epa_success = mean(epa), #expected points added
    success_rate = mean(success), #success 
    ypc=mean(yards_gained),
    plays =n() #number of plays 
  ) %>%
  arrange(-epa_success) %>%
  filter(plays >20)

pbp_rp %>%
  mutate( home= ifelse(posteam==home_team,1,0)) %>%
  group_by(home) %>%
  summarize(epa = mean(epa))
  
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

# Which teams were the most pass-heavy in the first half 
#on early downs with win probability between 20 and 80, excluding
#the final 2 minutes of the half when everyone is pass-happy

schotty <- pbp_rp %>%
  filter(qtr<=2 & down <= 4 & wp >=.2 & wp <= .8 & 
           half_seconds_remaining >120) %>%
  group_by(posteam) %>%
  summarize(mean_pass=mean(pass),
            plays = n()) %>%
  arrange(-plays)

ggplot(data=schotty,aes(x=reorder(posteam,-mean_pass), y=mean_pass))+
  geom_text(aes(label=posteam))
  
typeof(data$pass)
colnames(pbp_rp)




seasons <- 2018:2020
pbp <- nflfastR::load_pbp(seasons)

head(pbp$game_id)
min(pbp$game_id) #"2018_01_ATL_PHI"
max(pbp$game_id) # "2020_21_KC_TB"

pbp %>% #pbp has games from 2018, 2019, 2020
  group_by(season) %>%
  summarize(n = n())

typeof(pbp$season_type)
min(pbp$season_type)

pbp %>% #groups by play type "pass, run, fg" and gives number for each
  group_by(play_type) %>%
  summarize(plays = n())

pbp %>% #
  group_by(season, play_type) %>%
  summarize(plays=n()) %>%
  arrange(-plays)

qbs <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  filter(n_dropbacks > 100 & n_plays > 1000)

head(teams_colors_logos) # new data frame of teams logos



head(qbs)


dim(data)
head(data)


data %>%
  filter(play_type=="pass") %>%
  group_by(passer,receiver) %>%
  summarize(n=n(),
            mean_yards = mean(ydsnet),
            min_yards = min(ydsnet),
            max_yards = max(ydsnet) ) %>%
  arrange(-(n)) 

data %>%
  filter(play_type=="pass") %>%
  group_by(passer) %>%
  summarize(n=n(),
            mean_yards = mean(ydsnet),
            min_yards = min(ydsnet),
            max_yards = max(ydsnet) ) %>%
  arrange(-(n)) 


data %>% #QB passes per year 
  filter(play_type=="pass") %>%
   group_by(passer,receiver) %>%
   summarize(completed_passes=n(),
             min_yards = min(ydsnet),
               max_yards = max(ydsnet),
             incomplete_pass = sum(incomplete_pass),
             total_yards = sum(ydsnet)) %>%
   arrange(-total_yards)

data %>% #QB passes per year 
  filter(play_type=="pass", season=="2019") %>%
  group_by(passer,receiver) %>%
  summarize(completed_passes=n(),
            min_yards = min(ydsnet),
            max_yards = max(ydsnet),
            incomplete_pass = sum(incomplete_pass),
            total_yards = sum(ydsnet)) %>%
  arrange(-total_yards)



pbp %>% #QB passes per year QB to receiver combo with highest total yards
  filter(play_type=="pass", season=="2019", season_type=="REG") %>%
  group_by(passer,receiver) %>%
  summarize(completed_passes=n(),
            incomplete_pass = sum(incomplete_pass),
            total_yards = sum(ydsnet)) %>%
  arrange(-total_yards)


pbp %>% #top total yards per quarterback
  filter(play_type=="pass", season=="2020", season_type=="REG") %>%
  group_by(passer) %>%
  summarize(completed_passes=n(),
            incomplete_pass = sum(incomplete_pass),
            total_yards = sum(ydsnet)) %>%
  arrange(-total_yards)

most_td<-pbp %>%
  filter(play_type=="pass"& season=="2020"& season_type=="REG"& series_result=="Touchdown") %>%
  group_by(passer, posteam, series_result)%>%
    summarise(n=n())%>%
arrange(-n)

most_td



head(most_td)
head(teams_colors_logos)

most_td <- most_td %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

head(most_td)





#PLOTTING


most_td %>%
  filter( n > 25 ) %>%
  ggplot(aes(x = reorder(passer,-n), y=n)) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  geom_text_repel(aes(label=passer)) +
  labs(x = "Quarterbacks",
       y = "Touchdowns Per 2020",
       title = "2020 Touchdowns per Quarterback",
       caption = "Data: @nflfastR")

####################
#################### Load data for 3 years
####################
seasons <- 2017:2020
tre <- nflfastR::load_pbp(seasons)
two <- nflfastR::load_pbp(seasons)

head(two)

two %>%
  filter(season == 2020, home_team == 'SEA', season_type=="REG") %>%
  group_by(week) %>%
  select(season,home_team, week) %>%
  summarize(
            n=n(),
            result=result)

 

tre %>%
  filter(season == 2020, home_team == 'SEA' | away_team=='SEA', season_type=="REG") %>%
  group_by(season,home_team,away_team, week) %>%
  summarize(
    plays=n(),
    pass_plays = sum(pass),
    rush_plays = sum(rush)
  ) %>%
  arrange(week)
  




```{r}

tre %>% #what are Seattles pass yard per game ?
  filter(season == 2020, home_team == 'SEA'| away_team=='SEA', season_type=="REG", !is.na(epa), !is.na(yards_gained), !is.na(rush_touchdown)) %>%
  group_by(season,week, posteam, home_team,away_team, pass) %>% 
  summarize(yards_gained_by_scoring_team =sum(yards_gained),
            rush_touchdown = sum(rush_touchdown),
            pass_touchdown = sum(pass_touchdown)) %>%
  rename(scoring_team=posteam) %>%
  filter(scoring_team=='SEA')%>%
 pivot_wider(names_from =pass ,values_from = yards_gained_by_scoring_team, values_fill = 0) %>%
  #pivot_wider(names_from = week,values_from = rush_touchdown)%>%
  rename(rush_yards='0', pass_yards='1')%>%
  arrange(week)

```

```{r}
tres<- tre %>% #touchdowns per week per player from (QB to receiver) 
  filter(season == 2020, posteam=='SEA', home_team == 'SEA'| away_team=='SEA', season_type=="REG", series_result=="Touchdown", !is.na(epa), !is.na(yards_gained), !is.na(rush_touchdown), !is.na(passer_player_name), !is.na(receiver_player_name)) %>%
  group_by(season,week, posteam, home_team, away_team, passer_player_name, receiver_player_name, series_result,td_team ) %>%
 summarise(
   series_success=sum(series_success)
 ) %>%
  rename(scoring_team=posteam) %>%
  arrange(-series_success)


  
```



```{r}

tre_one <- tre %>% #first_downs per year per player from (QB to receiver) 
  filter(season == 2020, posteam=='SEA', home_team == 'SEA'| away_team=='SEA', season_type=="REG", series_result=="First down", !is.na(epa), !is.na(yards_gained), !is.na(rush_touchdown), !is.na(passer_player_name), !is.na(receiver_player_name)) %>%
  group_by(season, week,posteam, home_team, away_team, passer_player_name, receiver_player_name, series_result ) %>%
 summarise(
   series_success=sum(series_success
 )) %>%
  rename(scoring_team=posteam)

head(tre_one)
View(tre_one)

```

```{r}

#How many games did each player get a 1st down?

tre_one %>% #first downs per player for the Entire Year
  ggplot( aes(receiver_player_name)) +
  geom_bar()
```


```{r}

tre_first_downs<- tre %>% #first_downs per year per player from (QB to receiver) 
  filter(season == 2020, posteam=='SEA', home_team == 'SEA'| away_team=='SEA', season_type=="REG", series_result=="First down", !is.na(epa), !is.na(yards_gained), !is.na(rush_touchdown), !is.na(passer_player_name), !is.na(receiver_player_name)) %>%
  group_by(season, posteam, receiver_player_name, series_result ) %>%
 summarise(
   series_success=sum(series_success)
 ) %>%
  rename(scoring_team=posteam) %>%
  arrange(-series_success)

head(tre_first_downs)


```

```{r}

#In 2020 Locket and Metcalf led their team in first downs.
# Locket had 90 first downs, and Metcalf had 75. 

tre_first_downs %>% 
  ggplot( aes(x=receiver_player_name, y=series_success)) +
  geom_col()

```



```{r}

#How many games did each receiver score a touchdown?
#Metcalf seems to have 11.5 and Lockett at 8 tds per season.

tres %>%
  ggplot( aes(receiver_player_name)) +
  geom_bar()

```





```{r}

#+
   #geom_image(aes(image = team_logo_espn), size = qbs$n_plays / 45000, asp = 16 / 9)
```


```{r}
#how many touchdowns does R. Wilson throw per week in 2020 and to whom?







```

```{r}

```


  
```{r}
tre %>%
group_by(posteam,season,pass) %>%
 summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa)
  head(4)
```
  
  

```{r}
tre %>% #first_downs per year per player from (QB to receiver) 
  filter(season == 2020, posteam=='SEA', home_team == 'SEA'| away_team=='SEA', season_type=="REG", series_result=="First down", !is.na(epa), !is.na(yards_gained), !is.na(rush_touchdown), !is.na(passer_player_name), !is.na(receiver_player_name)) %>%
  group_by(season, week,posteam, home_team, away_team, passer_player_name, receiver_player_name, series_result) %>%
 summarise(
   series_success=sum(series_success
 )) %>%
  rename(scoring_team=posteam)

head(tre_one)
```




```{r}
head(tre)

#Which team had the most TD's?
#WWhich team had the most yards?


summary <- tre %>%
  filter(season=="2020",season_type=="REG", !is.na(yards_gained), !is.na(passing_yards), !is.na(rushing_yards)) %>%
  group_by(season, posteam) %>%
  summarise(
    total_yards=sum(yards_gained),
    touchdowns = sum(touchdown),
    pass_touchdown = sum(pass_touchdown),
    rush_touchdown = sum(rush_touchdown)
   # passing_yards = sum(passing_yards)
  ) %>%
  filter(!is.na(total_yards)) %>%
  arrange(-pass_touchdown)

View(summary)

# Check 2019 total_yards
tre %>%
  filter(season=="2019",season_type=="REG", !is.na(yards_gained)) %>%
  group_by(season, posteam) %>%
  summarise(
    total_yards=sum(yards_gained),
    
  ) %>%
  filter(!is.na(total_yards)) %>%
  arrange(-total_yards)

```









##########################
#CREATE WIN-TOTAL MODEL
##########################




head(games)





