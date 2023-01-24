# install.packages("rstatix")
# install.packages("palmerpenguins")


# Libraries needed --------------------------------------------------------


library(tidyverse)
library(rstatix)
library(palmerpenguins)


# Sample Correlation Testing ---------------------------------------------
# bivariate correlation
# normalised measure of covariance (between -1 and 1)

# select Adelie penguins
adelie <- penguins %>% 
  filter(species == "Adelie") %>% 
  select(c(2, 3, 6)) %>% # keep only relevant data
  drop_na()

adelie

# manual covariance and correlation ---------------------------------------

adelie %>%
  summarise(cov = cov(bill_length_mm, body_mass_g), 
            sd_bill = sd(bill_length_mm),
            sd_body = sd(body_mass_g),
            n = n()) %>%
  mutate(r = cov / (sd_bill * sd_body)) %>%
  View


# using library(rstatix) --------------------------------------------------

# Pearson
adelie %>% 
  rstatix::cor_test(bill_length_mm, body_mass_g)

# Directional Testing (one-tailed)
adelie %>% 
  rstatix::cor_test(bill_length_mm, body_mass_g, 
           alternative = "greater")

adelie %>% 
  rstatix::cor_test(bill_length_mm, body_mass_g, 
                    alternative = "less")

# Correlation Matrix ------------------------------------------------------

penguins %>% 
  drop_na %>%
  rstatix::cor_mat(penguins %>% select(where(is.numeric)) %>% names)

penguins %>% 
  drop_na %>%
  rstatix::cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  rstatix::cor_reorder()

# get p-values

penguins %>% 
  drop_na %>%
  rstatix::cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  rstatix::cor_reorder() %>%
  rstatix::cor_get_pval()

# coeffiecents and pvalues

penguins %>% 
  drop_na %>%
  rstatix::cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  rstatix::cor_reorder() %>%
  rstatix::cor_mark_significant()

penguins %>% 
  drop_na %>%
  rstatix::cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  rstatix::cor_reorder() %>%
  rstatix::cor_mark_significant(
    cutpoints = c(0, 1e-04, 0.001, 0.01, 0.1, 1)
  )

# diamonds data (penguins are much more interesting!)

diamonds %>% 
  drop_na %>%
  cor_mat(c("x","y","z","price","carat")) %>%
  cor_mark_significant

# back to penguins

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_gather

# visualise

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_plot()

# Exercise (15 min) -------------------------------------------------------

# 1. How would you visualise the correlation with ggplot? 

