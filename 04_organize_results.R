

# Organize Model Results for Plotting


# Packages
library(tidyverse)
library(patchwork) # Combines individual plots nicely without having to facet



# Load Data --------------------------------------------------------------------

# Data 
load("results/bestbwres.rda")

# Results from the full model
load("results/model_stats.rda")





# Tidy CW measures -------------------------------------------------------------


## Females ------------------------

cw_f <- female_data %>% 
  dplyr::select(uncchdid, bestbw, cw24f, cw91f, cwadultf) %>% 
  pivot_longer(
    cols = c(cw24f, cw91f, cwadultf),
    names_to = "measure",
    values_to = "cw_f"
  ) %>% 
  # create a time variable for x axis
  mutate(year = case_when(measure == "cw24f" ~ 1986,
                          measure == "cw91f" ~ 1991,
                          measure == "cwadultf" ~ 2005)
  )



## Males --------------------

# exclude birth for now

cw_m <- male_data %>% 
  dplyr::select(uncchdid, male05, bestbw, cw24m, cw91m, cwadultm) %>% 
  pivot_longer(
    cols = c(cw24m, cw91m, cwadultm),
    names_to = "measure",
    values_to = "cw_m"
  ) %>% 
  # create a time variable for x axis
  mutate(year = case_when(measure == "cw24m" ~ 1986,
                          measure == "cw91m" ~ 1991,
                          measure == "cwadultm" ~ 2005
  )
  )



# Beta Tables ----------------------------------

## Females -----------------

# Prenatal
beta_f_bw_longer <- beta_f_bw %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage, 
             pcgrimage, pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_f")

# Infant
beta_f_cw24_longer <- beta_f_cw24 %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage
             , pcgrimage, pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_f")

# Child
beta_f_cw91_longer <- beta_f_cw91 %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage, 
             pcgrimage, pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_f")

# Adult
beta_f_cwadult_longer <- beta_f_cwadult %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage,
             pcgrimage, pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_f")




## Males -----------------

# Prenatal
beta_m_bw_longer <- beta_m_bw %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage, pcgrimage,
             pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_m")


# Infant
beta_m_cw24_longer <- beta_m_cw24 %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage, 
             pcgrimage, pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_m"
  )


# Child
beta_m_cw91_longer <- beta_m_cw91 %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage, 
             pcgrimage, pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_m")



# Adult
beta_m_cwadult_longer <- beta_m_cwadult %>% 
  pivot_longer(
    cols = c(pchorvath, pchannum, pcphenoage,
             pcgrimage, pcdnamtl, dunedinpoam_45
    ),
    names_to = "clock",
    values_to = "beta_m")








# Save -------------------------------------------------------------------------



save(cw_f,cw_m,
     # female data
     beta_f_bw_longer,
     beta_f_cw24_longer,
     beta_f_cw91_longer,
     beta_f_cwadult_longer,
     # male data
     beta_m_bw_longer,
     beta_m_cw24_longer,
     beta_m_cw91_longer,
     beta_m_cwadult_longer,
     # file name
     file = "results/results_for_plot.rda"
)



