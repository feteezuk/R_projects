devtools::install_github("OmaymaS/yelpr")
library(yelpr)

api <- "MHOLXbRi1jNw6B3bmzvgO9G047FkLbXBXDc-xNS5V2RrjSqhfm8NopHy17dp8CdMRAxKF4ogC482O6_AgIG37dNjjnp9U_lzLj-kEqctpgE7vM9NTbAXkIamw6VrYHYx"

business_name <- "Olive Garden"
location <- "Los Angeles, CA"
limit <- 30

Yelp <- business_search(api_key = api, term=business_name, location=location, limit=limit)

Yelp1<-Yelp$businesses

View(Yelp1)


Yelp2<-business_search_review(api_key = api, business_id="cr5ZdMoW11llelF8Dv82QA")

Yelp2

Yelp2[,2]
getws()
ls()
