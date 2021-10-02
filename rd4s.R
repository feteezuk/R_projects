install.packages("hexbin")
library(tidyverse)
library(hexbin)

ggplot(diamonds, aes(carat,price)) +
  geom_hex()

ggsave("diamonds.pdf")

write.csv(diamonds, "diamonds.csv")

#get count for cut, clarity color
diamonds %>%
  count(cut,clarity,color)

##### VIZUALISE CONTINUOUS DATA

#Use cut_width for CONTINUOUS VAR. 
diamonds %>%
  count(cut_width(carat,.5)) #cut_width 

ggplot(diamonds, mapping=aes(carat)) +
  geom_histogram(binwidth=1)
#Filter to get smaller subset of data
diamonds %>%
  filter(carat<3) %>%
ggplot(mapping=aes(x=carat))+
  geom_histogram(binwidth = .2) #visualize # of observations .1 carat (bin) apart
##### OVERLAY MULTIPLE HISTOGRAMS ON 1 GRAPH (use freqpoly)
ggplot(diamonds, mapping=aes(x=carat, color=cut ))+
  geom_freqpoly(binwidth=0.1)+
  xlim(0,3)

ggplot(diamonds, mapping=aes(carat))+
  geom_freqpoly(mapping=aes(color=cut),binwidth = .01)+
  xlim(0,2)

#HOW TO CREATE A HEATMAP USing count
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping=aes(x=color, y=cut))+
  geom_tile(mapping=aes(fill=n))
#Use "geom_bin2d()" to VISUALIZE CONTINUOUS DATA
ggplot(diamonds) +
  geom_bin2d(mapping=aes(x=carat, y=price))
#Use hexbin to visualize continuous data
ggplot(diamonds) +
  geom_hex(mapping=aes(x=carat, y=price))
#Use Boxplot to vizualize continious DATA******
ggplot(diamonds, mapping=aes(x=carat, y=price))+
  geom_boxplot(mapping=aes(group=cut_width(carat,0.1)))+
  xlim(.5, 2.8)
ggplot(diamonds,mapping=aes( x=carat, y=price))+
  geom_boxplot(mapping=aes(group=cut_width))
####################
# TIBBLES
####################
flights <- nycflights13::flights 
#prrint all columns of tibble
flights%>%
  print(n=10, width=Inf) #width=Inf displays all columns 4 tibble
# Extract by name, year from flights tibble $, [[]]
flights$year 
flights[["year"]]
flights[[3]]
#Extract 1st, 2nd column from flights
flights[[1]]
flights[[2]]
#Extract by name of column
flights[["year"]]
flights %>% .$year
#######
#CH8 Data Import with readr
#######
read_csv(
  "a, b, c, d #1st line is column names
  1,2,3,4
  4,5,6, 7"
)
#How to skip lines when importing data
read_csv("first line of metadata
  second line of metadata
  x,y,z
  1,2,3", skip=2, col_names=FALSE) #does not label col names
read_csv("a,b,c\n 1,2,3\n 1,2,3,4")







