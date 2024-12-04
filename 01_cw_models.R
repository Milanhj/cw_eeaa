
# CW Models

library(tidyverse)



# Load Data --------------------------------------------------------------------


# Load Data: weight measures, clocks, control variables 
model_data <- read_rds("results/wght_clocks_ses.rds")



# BW ---------------------------------------------------------------------------


# BW measure with mean of mean bestbw but orthogonal to gestational age
  # bestbw = best measure of birth weight for individual
  # bestga = best estimate of gestational age



## Males --------------------------------------------


# Filter for male data
male_data <- model_data %>% 
  filter(male05 == 1 & inanalysis == 1)

# Regress BW on gestational age 
bw_adj_m <- lm(bestbw ~ bestga, male_data)

# Inspect results
summary(bw_adj_m)



### predictions --------------------------------

# Generate predictions with the linear model^^
b_adj_m_pred <- predict(bw_adj_m, male_data) %>%
  # bind predictions
  bind_cols(male_data) %>% 
  rename(pred = `...1`)



#### residuals ------------------------------


# Calculate residuals
male_data <- b_adj_m_pred %>% 
  mutate(resm = bestbw - pred)

# Visualize residuals
ggplot(male_data, aes(x = pred, y = resm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0)




### bestbwresm -------------------------------


# Sample mean birth weight
mean_bwm <- mean(male_data$bestbw)

# Create measure of prenatal growth as sample mean + residual from prediction
male_data <- male_data %>% 
  # convert to kilograms
  mutate(bestbwresm = (resm + mean_bwm)/1000) %>% #mean_bwm
  dplyr::select(-pred)




## Females ---------------------------------------------------------------------


# Filter for females 
female_data <- model_data %>% 
  filter(male05 == 0 & inanalysis == 1)

# Regress BW on gestational age 
bw_adj_f <- lm(bestbw ~ bestga, female_data)

# Inspect model
summary(bw_adj_f)



### predictions --------------------------------


# Bind predictions
b_adj_f_pred <- predict(bw_adj_f, female_data) %>%
  bind_cols(female_data) %>% 
  rename(pred = `...1`)



#### residuals --------------------------------------------

# Calculate residuals
female_data <- b_adj_f_pred %>% 
  mutate(resf = bestbw - pred)

# Visualize residuals
ggplot(female_data, aes(x = pred, y = resf)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0)



### bestbwresf -------------------------------------


# Sample mean birth weight
mean_bwf <- mean(female_data$bestbw)

# Create measure of prenatal growth as sample mean + residual from prediction
female_data <- female_data %>% 
  # convert to kilograms
  mutate(bestbwresf = (resf + mean_bwf)/1000) %>% #mean_bwf
  dplyr::select(-pred)




# CW24 -------------------------------------------------------------------------


## age^2 -----------------------------


# Age-squared terms
male_data <- male_data %>% 
  mutate(age24_2 = age24^2,
         age102_2 = age102^2,
         icageblood_2 = icageblood^2
  )

female_data <- female_data %>% 
  mutate(age24_2 = age24^2,
         age102_2 = age102^2,
         icageblood_2 = icageblood^2
  )



## Males -----------------------------------------------------------------------


### model ------------------------------------


# Regress weight at 2yo on wt24 on birth weight and age
cw_24_m <- lm(wt24 ~ bestbwresm + age24 + age24_2, 
              data = male_data)

summary(cw_24_m)



#### residuals ----------------------


# Bind predictions
male_data <- predict(cw_24_m, male_data) %>%
  bind_cols(male_data) %>% 
  rename(cw_pred = `...1`) %>% 
  # calculate residuals to store as conditional weight measure
  mutate(cw24m = wt24 - cw_pred) %>% 
  dplyr::select(-cw_pred)




## Females ---------------------------------------------------------------------


### model ----------------------------------------


# Regress weight at 2 yo on birthweight and age
cw_24_f <- lm(wt24 ~ bestbwresf + age24 + age24_2, 
              data = female_data)

summary(cw_24_f)



#### residuals ------------------------------------

# Bind predictions
female_data <- predict(cw_24_f, female_data) %>%
  bind_cols(female_data) %>% 
  rename(cw_pred = `...1`) %>% 
  # calculate residuals to store as conditional weight measure
  mutate(cw24f = wt24 - cw_pred) %>% 
  dplyr::select(-cw_pred)





# CW91 -------------------------------------------------------------------------


## Males -----------------------------------------------------------------------


### model ----------------------------------------


# Regress weight at 102mo  on conditional birth weight, cw at 24mo, and age
cw_91_m <- lm(wt102 ~ bestbwresm + cw24m + age102 + age102_2, 
              data = male_data)

summary(cw_91_m)



#### residuals ----------------------

# bind predictions
male_data <- predict(cw_91_m, male_data) %>%
  bind_cols(male_data) %>% 
  rename(cw_pred_91 = `...1`) %>% 
  # calculate residuals to store as conditional weight measure
  mutate(cw91m = wt102 - cw_pred_91) %>% 
  dplyr::select(-cw_pred_91)




## Females ---------------------------------------------------------------------


### model ----------------------------------------


# Regress weight at 102mo  on conditional birth weight, cw at 24mo, and age
cw_91_f <- lm(wt102 ~ bestbwresf + cw24f + age102 + age102_2, 
              data = female_data)

summary(cw_91_f)



#### residuals -------------------------------------

# bind predictions
female_data <- predict(cw_91_f, female_data) %>%
  bind_cols(female_data) %>% 
  rename(cw_pred_91 = `...1`) %>%
  # calculate residuals to store as conditional weight measure
  mutate(cw91f = wt102 - cw_pred_91) %>% 
  dplyr::select(-cw_pred_91)



# CW05 -------------------------------------------------------------------------


## Males ----------------------------------------------------------------------


### model ----------------------------------------


# Regress adult weight on all conditional weight measures and age
cw_05_m <- lm(wta ~ bestbwresm + cw24m + cw91m + icageblood + icageblood_2, 
              data = male_data)

summary(cw_05_m)



#### residuals ----------------------

# bind predictions
male_data <- predict(cw_05_m, male_data) %>%
  bind_cols(male_data) %>% 
  rename(cw_pred_05 = `...1`) %>% 
  # calculate residuals to store as conditional weight measure
  mutate(cwadultm = wta - cw_pred_05) %>% 
  dplyr::select(-cw_pred_05)




## Females --------------------------------------------------------------------


### model ----------------------


# Regress adult weight on all conditional weight measures and age
cw_05_f <- lm(wta ~ bestbwresf + cw24f + cw91f + icageblood + icageblood_2, 
              data = female_data)

summary(cw_05_f)



#### residuals ----------------------

# bind predictions
female_data <- predict(cw_05_f, female_data) %>%
  bind_cols(female_data) %>% 
  rename(cw_pred_05 = `...1`) %>% 
  # calculate residuals to store as conditional weight measure
  mutate(cwadultf = wta - cw_pred_05) %>% 
  dplyr::select(-cw_pred_05)




# Save Out ---------------------------------------------------------------------


# Save clean dataset with all variables for modeling
save(male_data, female_data, 
     file = "results/bestbwres.rda")





