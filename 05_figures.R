
# Visualize Model Results


# Packages
library(tidyverse)
library(patchwork) # Combines individual plots nicely without having to facet



# Load Data --------------------------------------------------------------------

# Organized results for plotting
load("results/results_for_plot.rda")




# Beta Plots -------------------------------------------------------------------


# Annotation codes
sig_0.1 <- "."
sig_0.05 <- "*"
sig_0.01 <- "**"
sig_0.001 <- "***"




## prenatal female -------------------------

p1 <- ggplot() +
  geom_col(beta_f_bw_longer, 
           mapping = aes(beta_f, clock),
           fill = "#B6ACD1") +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  labs(
    title = "Fetal Growth",
    subtitle = "Females",
    y = NULL,
    x = NULL
  ) +
  # phenoage
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.07,
    y = 6.3,
    size = 9,
    label = sig_0.1
  ) +
  #   
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )



## prenatal males ----------------------------

p2 <- ggplot() +
  geom_col(beta_m_bw_longer, 
           mapping = aes(beta_m, clock),
           fill = "#BBB8B8"
  ) +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  labs(
    subtitle = "Males",
    y = NULL,
    x = "beta"
    ) +
  # horvath
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0,
    x = -0.061,
    y = 5,
    size = 9,
    label = sig_0.1
    ) +
  # hannum
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0.5,
    x = -0.089,
    y = 3.9,
    size = 7.5,
    label = sig_0.05
  ) +
  # phenoage
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0.5,
    x = -0.133,
    y = 5.9,
    size = 7.5,
    label = sig_0.001
  ) +
  # grimage
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 1,
    x = -0.115,
    y = 3,
    size = 7.5,
    label = sig_0.01
  ) +
  # dnamtl
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.095,
    y = 1.9,
    size = 7.5,
    label = sig_0.05
  ) +
  # ddpm
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.095,
    y = 0.9,
    size = 7.5,
    label = sig_0.05
  ) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )



 
## cw24 female ----------------------------

p3 <- ggplot() +
  geom_col(beta_f_cw24_longer, 
           mapping = aes(beta_f, clock),
           fill = "#B6ACD1") +
  labs(
    title = "Birth - 2y",
    subtitle = "Females",
    y = NULL,
    x = NULL
  ) +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  # hannum
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0.5,
    x = -0.079,
    y = 3.9,
    size = 7.5,
    label = "*"
  ) +
  # phenoage
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0.5,
    x = -0.0915,
    y = 5.9,
    size = 7.5,
    label = "*"
  ) +
  # grimage
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.083,
    y = 2.9,
    size = 7.5,
    label = "*"
  ) +
  # dnamtl
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.098,
    y = 1.93,
    size = 7.5,
    label = "*"
  ) +
  # ddpm
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0.5,
    x = -0.142,
    y = 0.9,
    size = 7,
    label = "***"
  ) +
  scale_y_discrete(labels = NULL) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )



## cw24 male -------------------------------- 

p4 <- ggplot() +
  geom_col(beta_m_cw24_longer, 
           mapping = aes(beta_m, clock),
           fill = "#BBB8B8"
  ) +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  scale_y_discrete(labels = NULL) +
  # grimage
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.10,
    y = 2.84,
    size = 7.5,
    label = "*"
  ) +
  # dnamtl
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.09,
    y = 2.25,
    size = 9,
    label = "."
  ) +
  # ddpm
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.09,
    y = 1.25,
    size = 9,
    label = "."
  ) +
  labs(
    subtitle = "Males",
    y = NULL,
    x = "beta") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )










## cw91 female ----------------------------

p5 <- ggplot() +
  geom_col(beta_f_cw91_longer, 
           mapping = aes(beta_f, clock),
           fill = "#B6ACD1") +
  labs(
    title = "2y - 8y",
    subtitle = "Females",
    y = NULL,
    x = NULL
  ) +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  # horvath
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0,
    x = -0.09,
    y = 5,
    size = 9,
    label = "."
  ) +
  # hannum
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0.5,
    x = -0.09,
    y = 4.2,
    size = 9,
    label = "."
  ) +
  # dnamtl
  annotate(
    geom = "text", 
    hjust = 0,
    vjust = 0.5,
    x = -0.11,
    y = 2.2,
    size = 9,
    label = "."
  ) +
  scale_y_discrete(labels = NULL) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )





## cw91 males ----------------------------

p6 <- ggplot() +
  geom_col(beta_m_cw91_longer, 
           mapping = aes(beta_m, clock),
           fill = "#BBB8B8"
  ) +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  labs(
    subtitle = "Males",
    y = NULL,
    x = "beta") +
  scale_y_discrete(labels = NULL) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )





## cwadult females ----------------------------

p7 <- ggplot() +
  geom_col(beta_f_cwadult_longer, 
           mapping = aes(beta_f, clock),
           fill = "#B6ACD1") +
  labs(
    title = "8y - adult",
    subtitle = "Females",
    y = NULL,
    x = NULL
  ) +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  # horvath
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0,
    x = -0.167,
    y = 4.55,
    size = 7.5,
    label = "*"
  ) +
  # hannum
  annotate(
    geom = "text", 
    hjust = 1,
    vjust = 0.5,
    x = -0.115,
    y = 4.2,
    size = 9,
    label = "."
  ) +
  # dnamtl
  annotate(
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    x = -0.16,
    y = 1.9,
    size = 7.5,
    label = "*"
  ) +
  scale_y_discrete(labels = NULL) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )





## cwadult male ----------------------------

p8 <- ggplot() +
  geom_col(beta_m_cwadult_longer, 
           mapping = aes(beta_m, clock),
           fill = "#BBB8B8"
  ) +
  scale_x_continuous(
    limits = c(-.18, 0.04)
  ) +
  scale_y_discrete(labels = NULL) +
  labs(
    subtitle = "Males",
    y = NULL,
    x = "beta") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )



# Final Plot --------------------------------------------------------------------


# Add all plots together in specific order
p_total <- 
p1 + p3 + p5 + p7 + p2 + p4 + p6 + p8 +
  plot_layout(ncol = 4, nrow = 2)

p_total


#pdf(file = "final_figure.pdf")
#p_total
#dev.off()



# Save -------------------------------------------------------------------------


save(p_cw, p_total, file = "results/figures.rda")





