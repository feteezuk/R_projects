install.packages("tidymodels")
library(modeldata) # This is also loaded by the tidymodels package

tidymodels_prefer()
data(ames)

dim(ames)
summary(ames)


ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram()+
  scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))



# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(123)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop=.80)
ames_split
#> <Analysis/Assess/Total>
#> <2344/586/2930>
#data in the training set ( 2344)
 # data in test set ( 586 )
#original pool of samples ( 2930)


ames_train <- training(ames_split)
ames_test <- testing(ames_split)
 
dim(ames_train)


#Use Strata sample technique to avoid bias from right skewed data
set.seed(123)
ames_split <- initial_split(ames, prop=.80, strata= Sale_Price)
ames_train<- training(ames_split)
ames_test <- testing(ames_split)

dim(ames_train)
dim(ames_test)

##############################
# Model Fitting Walkt-thru

#To understand how the parsnip argument names map to the original names, use the help file 
#for the model (?rand_forest) as well as the translate() function:
###################################
lm_model <- linear_reg() %>% set_engine("lm")

lm_form_fit <- lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)


lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )


lm_form_fit

lm_xy_fit

#random Forest Tutorial
rand_forest(trees=1000, min_n=5) %>% 
  set_engine("ranger", verbose=TRUE, seed=100) %>%
  set_mode("regression") %>%
  translate()

#How to view model results? use 
#the results in a variety of ways; we might want to plot, print,
lm_form_fit %>%
  pluck("fit")


lm_form_fit %>%
  pluck("fit") %>%
  vcov()


  lm_form_fit %>% 
  pluck("fit") %>% 
  summary()

  tidy(lm_form_fit)

  ames_test_small <- ames_test %>% slice(1:5)
  predict(lm_form_fit, new_data = ames_test_small)
  #> # A tibble: 5 x 1

  library(tidymodels)
  parsnip_addin()

  args(tidy)
args(round)
