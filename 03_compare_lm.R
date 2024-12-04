
# Compare model without control variables (reduced) to full Model


library(tidyverse)


# Load Data ------------------------------------------------------


# Data 
load("results/bestbwres.rda")

# Results from the full model
load("results/model_stats.rda")



# Clean Data ---------------------------------

# Remove any rows with missing values in variables needed for models
female_data_no_na <- female_data %>% 
  filter(!is.na(bestbwresf), !is.na(cw24f), !is.na(cw91f), !is.na(cwadultf),
         !is.na(bmif), !is.na(gradecom), !is.na(dhinc05), !is.na(pregblood),
         !is.na(dailysmoke), !is.na(icageblood), !is.na(dunedinpoam_45),
         !is.na(pchorvath1), !is.na(pchannum), !is.na(pcphenoage),
         !is.na(pcdnamtl), !is.na(pcgrimage))


male_data_no_na <- male_data %>% 
  filter(!is.na(bestbwresm), !is.na(cw24m), !is.na(cw91m), !is.na(cwadultm),
         !is.na(bmi05), !is.na(gradecom), !is.na(dhinc05),
         !is.na(dailysmoke), !is.na(icageblood), !is.na(dunedinpoam_45),
         !is.na(pchorvath1), !is.na(pchannum), !is.na(pcphenoage),
         !is.na(pcdnamtl), !is.na(pcgrimage))




# Functions -----------------------------------


# Function calculates 95% CIs of two different model estimates to compare
compare_ci <- function(mod_red, mod_full, var_name){
  
  out1 <- summary(mod_red)
  out2 <- summary(mod_full)
  
  # Matrix to hold results
  ci_results <- matrix(nrow = 2, ncol = 3,
         dimnames = list(c("reduced model", "full model"),
                         c("estimate", "2.5", "97.5"))
         )
  
  # Reduced Model Estimate
  ci_results[1,"estimate"] <- out1[[4]][var_name,"Estimate"]
  
  # Reduced Model CI
  ci_results[1,"2.5"] <- out1[[4]][var_name,"Estimate"] - 
    1.96*out1[[4]][var_name,"Std. Error"]
  
  ci_results[1,"97.5"] <- out1[[4]][var_name,"Estimate"] + 
    1.96*out1[[4]][var_name,"Std. Error"]
  
  
  # Full Model Estimate
  ci_results[2,"estimate"] <- out2[[4]][var_name,"Estimate"]
  
  # Full Model CI
  ci_results[2,"2.5"] <- out2[[4]][var_name,"Estimate"] - 
    1.96*out1[[4]][var_name,"Std. Error"]
  
  ci_results[2,"97.5"] <- out2[[4]][var_name,"Estimate"] + 
    1.96*out1[[4]][var_name,"Std. Error"]
  
  return(ci_results)
  
} # end function




# Modeling PC clocks and CW ----------------------------------------------------


## Females ---------------------------------------------------------------------


### horvath ----------------------------


# Reduced Model
base_horvath_f <- lm(pchorvath1 ~ bestbwresf + cw24f + cw91f +
                       cwadultf + icageblood, 
                data = female_data_no_na)


# ANOVA
anova(base_horvath_f, horvath_f) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_horvath_f, mod_full = horvath_f,
           var_name = "cw24f")




### hannum ----------------------------


# Reduced Model
base_hannum_f <- lm(pchannum ~ bestbwresf + cw24f + cw91f + 
                      cwadultf + icageblood, 
               data = female_data_no_na)


# ANOVA
anova(base_hannum_f, hannum_f) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_hannum_f, mod_full = hannum_f,
           var_name = "cw24f")




### phenoage ----------------------------


# Reduced model
base_phenoage_f <- lm(pcphenoage ~ bestbwresf + cw24f + cw91f +
                        cwadultf + icageblood, 
                 data = female_data_no_na)

# ANOVA
anova(base_phenoage_f, phenoage_f) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_phenoage_f, mod_full = phenoage_f,
           var_name = "cw24f")



### grimage ----------------------------


# Reduced Model
base_grimage_f <- lm(pcgrimage ~ bestbwresf + cw24f + cw91f +
                       cwadultf + icageblood, 
                data = female_data_no_na)


# ANOVA
anova(base_grimage_f, grimage_f) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_grimage_f, mod_full = grimage_f,
           var_name = "cw24f")




### pcdnamtl ----------------------------


# Reduced Model
base_pcdnamtl_f <- lm(pcdnamtl ~ bestbwresf + cw24f + cw91f +
                        cwadultf + icageblood, 
                 data = female_data_no_na)


# ANOVA
anova(base_pcdnamtl_f, pcdnamtl_f) # p<0.05



# Confidence Intervals
compare_ci(mod_red = base_pcdnamtl_f, mod_full = pcdnamtl_f,
           var_name = "cw24f")




### dunedinpoam_45 ----------------------------


# Reduced Model
base_dunedinpoam_45_f <- lm(dunedinpoam_45 ~ bestbwresf + cw24f +
                              cw91f + cwadultf + icageblood, 
                       data = female_data_no_na)


# ANOVA
anova(base_dunedinpoam_45_f, dunedinpoam_45_f) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_dunedinpoam_45_f, mod_full = dunedinpoam_45_f,
           var_name = "cw24f")




## Males -----------------------------------------------------------------------


### horvath ----------------------------


# Reduced Model
base_horvath_m <- lm(pchorvath1 ~ bestbwresm + cw24m + cw91m + cwadultm 
                     + icageblood, 
                     data = male_data_no_na)


# ANOVA
anova(base_horvath_m, horvath_m) # p>0.05



# Confidence Intervals
compare_ci(mod_red = base_horvath_m, mod_full = horvath_m,
           var_name = "bestbwresm")



### hannum ----------------------------


base_hannum_m <- lm(pchannum ~ bestbwresm + cw24m + cw91m +
                      cwadultm + icageblood, 
                    data = male_data_no_na)


# ANOVA
anova(base_hannum_m, hannum_m) # p<0.05



# Confidence Intervals
compare_ci(mod_red = base_hannum_m, mod_full = hannum_m,
           var_name = "bestbwresm")





### phenoage ----------------------------


# Reduced model
base_phenoage_m <- lm(pcphenoage ~ bestbwresm + cw24m + cw91m +
                        cwadultm + icageblood, 
                      data = male_data_no_na)


# ANOVA
anova(base_phenoage_m, phenoage_m) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_phenoage_m, mod_full = phenoage_m,
           var_name = "bestbwresm")





### grimage ----------------------------


# Reduced Model
base_grimage_m <- lm(pcgrimage ~ bestbwresm + cw24m + cw91m + 
                       cwadultm + icageblood, 
                     data = male_data_no_na)


# ANOVA
anova(base_grimage_m, grimage_m) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_grimage_m, mod_full = grimage_m,
           var_name = "bestbwresm")



### pcdnamtl ----------------------------


# Reduced Model
base_pcdnamtl_m <- lm(pcdnamtl ~ bestbwresm + cw24m + cw91m + 
                        cwadultm + icageblood, 
                      data = male_data_no_na)

# ANOVA
anova(base_pcdnamtl_m, pcdnamtl_m) # p<0.05



# Confidence Intervals
compare_ci(mod_red = base_pcdnamtl_m, mod_full = pcdnamtl_m,
           var_name = "bestbwresm")



### dunedinpoam_45 ------------------------


# Reduced Model
base_dunedinpoam_45_m <- lm(dunedinpoam_45 ~ bestbwresm + cw24m + cw91m +
                              cwadultm + icageblood, 
                            data = male_data_no_na)

# ANOVA
anova(base_dunedinpoam_45_m, dunedinpoam_45_m) # p<0.05


# Confidence Intervals
compare_ci(mod_red = base_dunedinpoam_45_m, mod_full = dunedinpoam_45_m,
           var_name = "bestbwresm")




