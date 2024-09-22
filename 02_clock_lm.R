
# Modeling PC clocks and CW ----------------------------------------------------

# DNAmAge and Conditional Weight stats: 


# Load Packages and Data ------------------------------------------------------ 

library(tidyverse)
library(stargazer)
library(QuantPsyc)
library(jtools)

set.seed(1234)



# Variables for modeling: clocks and cw
# male_data, female_data

load("results/bestbwres.rda") 




# Females --------------------------------------------------------------------


## horvath -----------

horvath_f <- lm(pchorvath1 ~ bestbwresf + cw24f + cw91f + cwadultf + 
     bmif + gradecom + dhinc05 + pregblood + dailysmoke + icageblood, 
   data = female_data)

horvath_f_stats <- summary(horvath_f)




## hannum ----------------

hannum_f <- lm(pchannum ~ bestbwresf + cw24f + cw91f + cwadultf + 
     bmif + gradecom + dhinc05 + pregblood + dailysmoke + icageblood, 
   data = female_data)

hannum_f_stats <- summary(hannum_f)


## phenoage ----------------

phenoage_f <- lm(pcphenoage ~ bestbwresf + cw24f + cw91f + cwadultf + 
     bmif + gradecom + dhinc05 + pregblood + dailysmoke + icageblood, 
   data = female_data)

phenoage_f_stats <- summary(phenoage_f)


## grimage --------------------

grimage_f <- lm(pcgrimage ~ bestbwresf + cw24f + cw91f + cwadultf + 
     bmif + gradecom + dhinc05 + pregblood + dailysmoke + icageblood, 
   data = female_data)

grimage_f_stats <- summary(grimage_f)


## pcdnamtl --------------------

pcdnamtl_f <- lm(pcdnamtl ~ bestbwresf + cw24f + cw91f + cwadultf + 
     bmif + gradecom + dhinc05 + pregblood + dailysmoke + icageblood, 
   data = female_data)

pcdnamtl_f_stats <- summary(pcdnamtl_f)



## dunedinpoam_45 ---------------

dunedinpoam_45_f <- lm(dunedinpoam_45 ~ bestbwresf + cw24f + cw91f + cwadultf + 
     bmif + gradecom + dhinc05 + pregblood + dailysmoke + icageblood, 
   data = female_data)


dunedinpoam_45_f_stats <- summary(dunedinpoam_45_f)


## combined table ---------------------

table_2_f <- stargazer(horvath_f, hannum_f, phenoage_f, 
                       grimage_f, pcdnamtl_f,
          type = "text",
          dep.var.labels = c("pchorvath1", "pchannum", "pcphenoage",
                             "pcgrimage",  "pcdnamtl"),
          title = "Table 2: Regression Results",
          digits = 3

)

stargazer(dunedinpoam_45_f,  
          type = "text",
          dep.var.labels = c("pcdunedinpoam_45"),
          title = "Table 2: Regression Results",
          digits = 4
          
)




# Males -----------------------------------------------------------------------


## horvath ----------------------

horvath_m <- lm(pchorvath1 ~ bestbwresm + cw24m + cw91m + cwadultm + bmi05 + gradecom + 
                  dhinc05 + dailysmoke + icageblood, 
                data = male_data)

horvath_m_stats <- summary(horvath_m)



## hannum -----------------------


hannum_m <- lm(pchannum ~ bestbwresm + cw24m + cw91m + cwadultm + bmi05 + gradecom + 
                 dhinc05 + dailysmoke + icageblood, 
               data = male_data)

hannum_m_stats <- summary(hannum_m)


## phenoage ---------------------

phenoage_m <- lm(pcphenoage ~ bestbwresm + cw24m + cw91m + cwadultm + bmi05 + gradecom + 
                   dhinc05 + dailysmoke + icageblood, 
                 data = male_data)

phenoage_m_stats <- summary(phenoage_m)


## grimage ----------------------


grimage_m <- lm(pcgrimage ~ bestbwresm + cw24m + cw91m + cwadultm + bmi05 + gradecom + 
                  dhinc05 + dailysmoke + icageblood, 
                data = male_data)

grimage_m_stats <- summary(grimage_m)



## pcdnamtl --------------------

pcdnamtl_m <- lm(pcdnamtl ~ bestbwresm + cw24m + cw91m + cwadultm + bmi05 + gradecom + 
                   dhinc05 + dailysmoke + icageblood, 
                 data = male_data)

pcdnamtl_m_stats <- summary(pcdnamtl_m)



## dunedinpoam_45 --------------------

dunedinpoam_45_m <- lm(dunedinpoam_45 ~ bestbwresm + cw24m + cw91m + cwadultm + bmi05 + gradecom + 
                         dhinc05 + dailysmoke + icageblood, 
                       data = male_data)


dunedinpoam_45_m_stats <- summary(dunedinpoam_45_m)



## combined table --------------------



table_2_m <- stargazer(horvath_m, hannum_m, phenoage_m, grimage_m, pcdnamtl_m,
          type = "text",
          dep.var.labels = c("pchorvath1", "pchannum", "pcphenoage",
                             "pcgrimage", "pcdnamtl"),
          title = "Table 2: Regression Results",
          digits = 4
          
)

stargazer(dunedinpoam_45_m,
          type = "text",
          dep.var.labels = c("pcdudedinpoam_45"),
          title = "Table 2: Regression Results",
          digits = 4
          
)




# Standardized coefficients CW models ---------------------------------------------------------------------

# same models but reporting standardized coefficients 
  # (beta = equivalent of correlation coefficients adjusting for everything else in the model)
  # figures showing the strength of relationships with growth during different intervals



## Females ---------------------------------------------------


# Store model objects (females)
beta_horvath_f <- lm.beta(horvath_f)
beta_hannum_f <- lm.beta(hannum_f)
beta_pheno_f <- lm.beta(phenoage_f)
beta_grim_f <- lm.beta(grimage_f)
beta_dnamtl_f <- lm.beta(pcdnamtl_f)
beta_dunedinpoam_f <- lm.beta(dunedinpoam_45_f)

summary(beta_horvath_f)
class(beta_horvath_f)


## Males ---------------------------------------------------

# Store model objects (males)
beta_horvath_m <- lm.beta(horvath_m)
beta_hannum_m <- lm.beta(hannum_m)
beta_pheno_m <- lm.beta(phenoage_m)
beta_grim_m <- lm.beta(grimage_m)
beta_dnamtl_m <- lm.beta(pcdnamtl_m)
beta_dunedinpoam_m <- lm.beta(dunedinpoam_45_m)


summary(beta_horvath_m)
class(beta_horvath_m)



## Beta Coefficients -----------------------------------------------------------


### Females --------------------------------------

#### bw ----------------------- 


# Position 1 of lm.beta output for CW at birth coefficient
beta_f_bw <- tibble(
  pchorvath = beta_horvath_f[[1]],
  pchannum = beta_hannum_f[[1]],
  pcphenoage = beta_pheno_f[[1]],
  pcgrimage = beta_grim_f[[1]],
  pcdnamtl = -abs(beta_dnamtl_f[[1]]),
  dunedinpoam_45 = beta_dunedinpoam_f[[1]]
)


#### cw24 ---------------------


# Position 2 for CW at 2 years old
beta_f_cw24 <- tibble(
  pchorvath = beta_horvath_f[[2]],
  pchannum = beta_hannum_f[[2]],
  pcphenoage = beta_pheno_f[[2]],
  pcgrimage = beta_grim_f[[2]],
  pcdnamtl = -abs(beta_dnamtl_f[[2]]),
  dunedinpoam_45 = beta_dunedinpoam_f[[2]]
)


#### cw91 -----------------------


# Position 3 for CW at 8 years old
beta_f_cw91 <- tibble(
  pchorvath = beta_horvath_f[[3]],
  pchannum = beta_hannum_f[[3]],
  pcphenoage = beta_pheno_f[[3]],
  pcgrimage = beta_grim_f[[3]],
  pcdnamtl = -abs(beta_dnamtl_f[[3]]),
  dunedinpoam_45 = beta_dunedinpoam_f[[3]]
)


#### cwadult ---------------------


# Position 4 for CW at 21 years old
beta_f_cwadult <- tibble(
  pchorvath = beta_horvath_f[[4]],
  pchannum = beta_hannum_f[[4]],
  pcphenoage = beta_pheno_f[[4]],
  pcgrimage = beta_grim_f[[4]],
  pcdnamtl = -abs(beta_dnamtl_f[[4]]),
  dunedinpoam_45 = beta_dunedinpoam_f[[4]]
)



### Males --------------------------------------



#### bw ------------------------


# Position 1 for CW at birth
beta_m_bw <- tibble(
  pchorvath = beta_horvath_m[[1]],
  pchannum = beta_hannum_m[[1]],
  pcphenoage = beta_pheno_m[[1]],
  pcgrimage = beta_grim_m[[1]],
  pcdnamtl = -abs(beta_dnamtl_m[[1]]),
  dunedinpoam_45 = beta_dunedinpoam_m[[1]]
)


#### cw24 -------------------------


# Position 2 for CW at 2 years old
beta_m_cw24 <- tibble(
  pchorvath = beta_horvath_m[[2]],
  pchannum = beta_hannum_m[[2]],
  pcphenoage = beta_pheno_m[[2]],
  pcgrimage = beta_grim_m[[2]],
  pcdnamtl = -abs(beta_dnamtl_m[[2]]),
  dunedinpoam_45 = beta_dunedinpoam_m[[2]]
)


#### cw91 -------------------------


# Position 3 for CW at 8 years old
beta_m_cw91 <- tibble(
  pchorvath = beta_horvath_m[[3]],
  pchannum = beta_hannum_m[[3]],
  pcphenoage = beta_pheno_m[[3]],
  pcgrimage = beta_grim_m[[3]],
  pcdnamtl = -abs(beta_dnamtl_m[[3]]),
  dunedinpoam_45 = beta_dunedinpoam_m[[3]]
)


#### cwadult -------------------------


# Position 4 for CW at 21 years old
beta_m_cwadult <- tibble(
  pchorvath = beta_horvath_m[[4]],
  pchannum = beta_hannum_m[[4]],
  pcphenoage = beta_pheno_m[[4]],
  pcgrimage = beta_grim_m[[4]],
  pcdnamtl = -abs(beta_dnamtl_m[[4]]),
  dunedinpoam_45 = beta_dunedinpoam_m[[4]]
)


# Save out -------------------------------------------------------------------------

save(
  # female models
  beta_horvath_f, horvath_f, horvath_f_stats,
  beta_hannum_f, hannum_f, hannum_f_stats,
  beta_pheno_f, phenoage_f, phenoage_f_stats,
  beta_grim_f, grimage_f, grimage_f_stats,
  beta_dnamtl_f, pcdnamtl_f, pcdnamtl_f_stats,
  beta_dunedinpoam_f, dunedinpoam_45_f, dunedinpoam_45_f_stats,
  # male models
  beta_horvath_m, horvath_m, horvath_m_stats,
  beta_hannum_m, hannum_m, hannum_m_stats,
  beta_pheno_m, phenoage_m, phenoage_m_stats,
  beta_grim_m, grimage_m, grimage_m_stats,
  beta_dnamtl_m, pcdnamtl_m, pcdnamtl_m_stats,
  beta_dunedinpoam_m, dunedinpoam_45_m, dunedinpoam_45_m_stats,
  # beta cw models
  beta_f_bw, beta_f_cw24,beta_f_cw91, beta_f_cwadult,  
  beta_m_bw, beta_m_cw24, beta_m_cw91, beta_m_cwadult,  
  file = "results/model_stats.rda"
)


save(table_2_f, table_2_m,
     file = "results/tables.rda")




