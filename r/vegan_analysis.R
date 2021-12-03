library(psych)

library(readr)
library(here) # for managing folders and paths
library(misinformation)
library(ltm)
library(tableone)
library(tidyverse)
library(lm.beta)


df <-  read_csv(here("data", "2019_vegandata.csv"))
meta <- read_csv(here("data", "2019vegan_meta.csv")) 

df <- meta_rename(df, meta, `import name`, var_name)

# compute some variables --------------------------------------------------

df <-  df %>% 
  mutate(describ_diet = as.factor(case_when(describ_diet == 1 ~  "Omnivore",
                                  describ_diet == 2 ~  "Flexetarian",
                                  describ_diet == 3 ~  "Vegetarian",
                                  describ_diet == 4 ~  "Vegan",
                                  describ_diet == 5 ~  "Other"
                                  )), 
         describ_diet = fct_rev(fct_relevel(describ_diet, 
                                            "Vegan", 
                                            "Vegetarian",
                                            "Flexetarian", 
                                            "Other")), 
    health_6r = 8 - health_6, #reverse code item
         health = (health_1 + 
           health_2 + 
           health_3 + 
           health_4 + 
           health_5 + 
           health_6r + 
           health_7 + 
           health_8 + 
           health_9 + 
           health_10) / 10, #creating summary healthc concern measure
         animal_attit_2r = 6 - animal_attit_2, 
         animal_attit_3r = 6- animal_attit_3,
         animal_attit_4r = 6- animal_attit_4,
         animal_attit_7r = 6- animal_attit_7,
         animal_attit_8r = 6- animal_attit_8,
         animal_attit = (animal_attit_1 + animal_attit_2r + 
                           animal_attit_3r + animal_attit_4r + animal_attit_5 + 
                           animal_attit_6 + animal_attit_7r + animal_attit_8r + 
                           animal_attit_9 + animal_attit_10)/10,
         ecocent = (eco_anth_1 + eco_anth_7 + eco_anth_12 + eco_anth_16 + 
                      eco_anth_21 + eco_anth_26 + eco_anth_30) / 7,
         anthro = (eco_anth_2 + eco_anth_4 + eco_anth_5 + eco_anth_8 + 
                     eco_anth_11 + eco_anth_13 + eco_anth_14 + eco_anth_19 + 
                     eco_anth_22 +  eco_anth_23 +  eco_anth_24 +  eco_anth_27 + 
                     eco_anth_28 +  eco_anth_29 +  eco_anth_31 +  eco_anth_32 + 
                     eco_anth_33) / 17,
         gen_apathy = (eco_anth_3 +
           eco_anth_6 +
           eco_anth_9 +
           eco_anth_10 +
           eco_anth_15 +
           eco_anth_17 +
           eco_anth_18 +
           eco_anth_20 + 
           eco_anth_25) / 9,
         ingroup = (moral_3  +
                    moral_9  +
                    moral_14 +
                    moral_19 +
                    moral_25 +
                    moral_30) / 6,
         harm = (moral_1  +
                 moral_7  +
                 moral_12 +
                 moral_17 +
                 moral_23 +
                 moral_28) / 6,
         fairness = (moral_2 +
                     moral_8  +
                     moral_13 +
                     moral_18 +
                     moral_24 +
                     moral_29) / 6,
         authority = (moral_4 + 
                        moral_10 + 
                        moral_15 + 
                        moral_20 + 
                        moral_26 + 
                        moral_31) / 6,
         purity = (moral_5 + 
                   moral_11 +
                   moral_16 +
                   moral_21 +
                   moral_27 +
                   moral_32) / 6
           )     

# estimate reliabilities ----------------
#health concerns
df %>% dplyr::select(starts_with("health_")) %>% dplyr::select(-health_6) %>% 
  omega() %>% pluck("omega.tot")

# once per factor

df %>% dplyr::select(eco_anth_2,
                     eco_anth_4,
                     eco_anth_5,
                     eco_anth_8,
                     eco_anth_11,
                     eco_anth_13,
                     eco_anth_14,
                     eco_anth_19,
                     eco_anth_22,
                     eco_anth_23,
                     eco_anth_24,
                     eco_anth_27,
                     eco_anth_28,
                     eco_anth_29,
                     eco_anth_31,
                     eco_anth_32,
                     eco_anth_33) %>% 
  omega() %>% pluck("omega.tot")

# descriptives ------------------------------------------------------------

CreateTableOne(vars = c("age", "gender", "education", "location", 
                        "health", "describ_diet", "belief_sys"),
               factorVars = c("gender", "education", 
                              "describ_diet", "belief_sys"),
               data = df)

# IRT section -------------------------------------------------------------

apu <- df %>% 
  dplyr::select(starts_with("apu_")) %>% 
  mutate_all(~(.-1))

out <- ltm(apu~z1)

out

# estimate vegan-level ----------------------------------------------------

factor_score <- factor.scores.ltm(object = out, resp.patterns = apu)
fscores2 <- factor_score$score.dat[, "z1"]

fscores2

df <- df %>% 
  mutate(vpa = fscores2) #adds the factor scores back to the dataframe


# Testing the relationship of vpa to other variables ----------------------

vpa_health <-  lm(vpa~ health, df)
vpa_animal_attit <-  lm(vpa~ animal_attit, df)
vpa_ecocent <-  lm(vpa~ ecocent + anthro + gen_apathy, df)
vpa_moral <-  lm(vpa ~ ingroup + harm + fairness + authority + purity, data = df)
summary(vpa_ecocent)
summary(vpa_health)
summary(vpa_animal_attit)
summary(vpa_moral)
lm.beta(vpa_ecocent)
lm.beta(vpa_health)
lm.beta(vpa_animal_attit)
lm.beta(vpa_moral)

vpa_diet <-  lm(vpa~ describ_diet, df)
summary(vpa_diet)

# graphs and stuff --------------------------------------------------------
df %>%
  filter(!is.na(describ_diet)) %>% 
  ggplot(aes(vpa, describ_diet, group = describ_diet)) + 
  geom_jitter(aes(fill = vpa), colour = "black", shape = 21, size = 2) + 
  scale_fill_viridis_c(option = "plasma") + 
  theme_classic() + 
  labs(x = "Vegan Practice Adoption", y = "Self-Identified Diet") + 
  theme(legend.position = "none")



coef <- as.data.frame(out$coefficients) %>% arrange(z1) %>% slice(6:31)

names(coef) <-  c("diff", "disc")

irt2 <- function(x, diff, disc) {
  1/(1 + exp(-disc * (x - diff)))
}

coef1 <- coef %>% dplyr::transmute(item = row.names(coef), diff = diff, 
                              disc = disc) %>% tidyr::crossing(x = seq(-5, 5, 0.1)) %>% 
  dplyr::mutate(y = irt2(x, diff, disc))

ggplot2::ggplot(coef1, ggplot2::aes(x = x, y = y, colour = item)) + 
  ggplot2::geom_line(size = 0.7) + ggplot2::theme_classic() + 
  ggplot2::xlab("Difficulty") + ggplot2::ylab("Probability of endorsement") + 
  ggplot2::labs(colour = "Item")


plot(out)

IRTplot(out)

# Spreads out the x axis

IRTplot(out) + coord_cartesian(xlim = c(-1.5, 3))

out
