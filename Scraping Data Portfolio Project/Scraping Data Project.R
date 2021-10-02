library(tidyverse)
library(rvest)
library(xml2)

url<-"https://www.indeed.com/jobs?q=data%20analyst&l=San%20Francisco%2C%20CA&vjk=0c2a6008b4969776"
page<-xml2::read_html(url)#function will read in the code from the webpage and break it down into different elements (<div>, <span>, <p>, etc.


#get job title
title<-page %>%
  html_nodes(".jobTitle") %>%
  html_text()
  
#get company Location
loc<-page %>%
  html_nodes(".companyLocation") %>%
  html_text()

#job snippet
snippet<-page %>%
  html_nodes(".job-snippet") %>%
  html_text()

#Get link 
desc<- page %>%
  html_nodes("a[data-jk]") %>%
  html_attr("href") 

# Create combine link 
combined_link <- paste("https://www.indeed.com", desc, sep="")

#Turn combined link into a session follow link



page1 <-  html_session(combined_link[[1]])
page1 %>%
  html_nodes(".iCIMS_JobContent, #jobDescriptionText") %>%
  html_text()

#one<- page %>% html_elements("a[id*='job']")

#create function return a list of page-returns

ret <- lapply(paste0("https://www.indeed.com", desc), xml2::read_html)


description<-purrr::map(ret[1:length(ret)], ~ .x %>% 
             html_nodes(".iCIMS_JobContent, #jobDescriptionText") %>%
             html_text())

#Combine to make Dataframe
c <- cbind(title, loc, snippet, combined_link,description )
View(c)

#Turn the page
# https://www.indeed.com/jobs?q=data%20analyst&l=San%20Francisco%2C%20CA&vjk=0c2a6008b4969776
#https://www.indeed.com/jobs?q=data%20analyst&l=San%20Francisco%2C%20CA&start=10&vjk=14ba77c18c90585f
#https://www.indeed.com/jobs?q=data%20analyst&l=San%20Francisco%2C%20CA&start=20&vjk=2f73a3d9cb046e50



#grab 10 ten pages 1-10, aka 100 results
url_2<- lapply(paste0("https://www.indeed.com/jobs?q=data%20analyst&l=San%20Francisco%2C%20CA&start=", sep=seq(10,100,length.out=10)), xml2::read_html)
url_2 #it works!

#get job title
titles_2<-purrr::map(url_2[1:length(url_2)], ~ .x %>% 
                          html_nodes(".jobTitle") %>%
                          html_text())
#get location
loc_2 <- purrr::map(url_2[1:length(url_2)], ~.x %>%
                     html_nodes(".companyLocation") %>%
                     html_text()
                     )

#job snippet
snippet <- purrr::map(url_2[1:length(url_2)], ~.x  %>%
                      html_nodes(".job-snippet") %>%
                      html_text()
)


#Get links
link_2 <-purrr::map(url_2[1:length(url_2)], ~.x  %>%
                    html_nodes("a[data-mobtk]") %>%
                    html_attr("href")
)

indeed <- "https://www.indeed.com"

# Add ""https://www.indeed.com"" to beginning of every element. think of link_2 as "x"
y=lapply(link_2, function(x) paste0(indeed, x)) #lapply goes thru every item in a list


y<-map(link_2, ~ indeed %>% 
      paste0(.x)
      )

 jerk<- unlist(y)   

 as.list(jerk)

 open<- purrr::map(jerk[1:length(jerk)], ~.x  %>%
              html_session() 
              )
 
 purrr::map(jerk[1:length(jerk)], ~.x  %>%
              html_session(.x) %>%
              html_nodes("h1") )
 
 
 
 