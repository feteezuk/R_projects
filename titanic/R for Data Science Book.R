#Ch1 Data Viz

install.packages("tidyverse")
library(tidyverse)

head(mpg) # df
ggplot(data=mpg) + #displ: engine size, hwy: mpg in city
  geom_point(mapping=aes(x=displ, y=hwy))

ggplot(data=mpg)

mtcars
nrow(mtcars) #32 rows
ncol(mtcars) #11 columns

# drv variable:
#the type of drive train, where f = front-wheel drive, 
#r = rear wheel drive, 4 = 4wd

mtcars

ggplot(data=mpg) +
  geom_point(mapping=aes(x=hwy, y=cyl))

ggplot(data=mpg) +
  geom_point(mapping=aes(x=class, y=drv))

ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, shape=class),  color="blue")

diamonds

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut, y=depth), stat="identity")

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut, color=depth))

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut, fill=clarity),
           position="dodge") 

diamonds
#create a circle chart pie graph 
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=cut),
           )+
  coord_polar()

#ch3
install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)


flights<- nycflights13::flights
flights
filter(flights, month==1, sched_arr_time==1022 | sched_arr_time==819)  %>%
  arrange(-sched_arr_time)

View(flights)
flights %>%
  filter (arr_delay < 120, dest=="IAH" | dest=="HOU")%>%
View()


#sort all missing  values to start
flights %>%
  arrange(-is.na(flights) ) %>%
  head()

#most delayed flights
flights %>%
  arrange(-dep_delay ) %>%
  head()



# flights that left earliert
flights %>%
  arrange(dep_delay) %>%
  head()

View(flights)
#fastest flights
flights %>%
  arrange(-air_time) %>%
  head()

flights %>%
  select(starts_with("ye"))






