# packages needed
library(tidyverse) # manage data
library(ggalluvial) # alluvial diagrams
library(viridis) # color
library(ggthemes) # themes

# get the github repo
library(remotes)
remotes::install_github("allisonhorst/palmerpenguins", force = TRUE)
library(palmerpenguins)
# package contains 2 datesets
data(package = 'palmerpenguins')
?penguins

head(penguins) # simplified version of raw dateset
head(penguins_raw) # raw dataset

str(penguins)
str(penguins_raw)

# analyze and clean data --------------------------------------------------
penguins

penguins %>%
  count(species)

penguins_01 <- penguins %>% # get rid of NA
  na.omit()

# Explore the data ----------------------------------------------
# bill vs flipper length
penguins_01 %>%
  ggplot(aes(bill_length_mm, flipper_length_mm)) +
  geom_point() 

penguins_01 %>% # by species 
  ggplot(aes(bill_length_mm, flipper_length_mm, group = species)) +
  geom_point(aes(shape = species, colour = species))

penguins_01 %>% # by species and sex
  ggplot(aes(bill_length_mm, flipper_length_mm, group = species)) +
  geom_point(aes(colour = sex)) +
  facet_wrap(~ species)

penguins_01 %>% # by island, species, and sex
  ggplot(aes(bill_length_mm, flipper_length_mm, group = island)) +
  geom_point(aes(colour = species, shape = sex)) +
  facet_wrap(~ island)

# body mass vs flipper length
penguins_01 %>% # by body mass (kg), species, and sex
  mutate(body_mass_kg = body_mass_g/1000) %>%
  arrange(-body_mass_kg) %>%
  ggplot(aes(flipper_length_mm, body_mass_kg)) +
  geom_point(aes(colour = species, shape = sex)) +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

# visualize ---------------------------------------------------------------
penguins_02 <- penguins_01 %>%
  mutate(body_mass_kg = body_mass_g/1000) %>%
  mutate(avg_bill_length = ifelse(bill_length_mm > 44, "Longer", "Shorter")) %>%
  mutate(avg_flipper_length = ifelse(flipper_length_mm > 200, "Longer", "Shorter")) %>%
  mutate(avg_body_mass = ifelse(body_mass_kg > 4.13, "Higher", "Lower")) %>%
  group_by(species, island, sex, year, avg_bill_length, avg_flipper_length, avg_body_mass) %>%
  summarise(
    freq = n(),
  )

# Viz1 - penguin's size (flipper length and body mass) by sex
penguins_02 %>%
  ggplot(aes(axis1 = species, 
             axis2 = avg_flipper_length, 
             axis3 = avg_body_mass, 
             y = freq)) +
  scale_x_discrete(limits = c("species", "flipper length", "body mass"), 
                   expand = c(.2, .05)) +
  geom_alluvium(aes(fill = sex),
                width = 1/12, 
                alpha = 0.7, 
                knot.pos = 0.4) +
  geom_stratum(width = 1/3) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_fill_manual(values  = c("orange1", "cyan4")) +
  theme_minimal() +
  labs(
    title = "Are male penguins bigger at Palmer Station LTER?",
    subtitle = "Body mass and flipper length by species and island",
    caption = "data source: https://oceaninformatics.ucsd.edu/datazoo/catalogs/pallter/datasets/220"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  )

# Viz2 - penguin's size (flipper length and body mass) by island
penguins_02 %>%
  ggplot(aes(axis1 = species, 
             axis2 = avg_flipper_length, 
             axis3 = avg_body_mass, 
             y = freq)) +
  scale_x_discrete(limits = c("species", "flipper length", "body mass"), 
                   expand = c(.2, .05)) +
  geom_alluvium(aes(fill = island, 
                    color = island),
                width = 1/12, 
                alpha = 0.7, 
                knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "How big are penguins at Palmer Station LTER?",
    subtitle = "Body mass and flipper length by species and island",
    caption = "data source: https://oceaninformatics.ucsd.edu/datazoo/catalogs/pallter/datasets/220"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  )

# Viz2, version 2 - penguin's size (flipper length and body mass) by island
penguins_02 %>%
  ggplot(aes(axis1 = species, 
             axis2 = avg_flipper_length, 
             axis3 = avg_body_mass, 
             y = freq)) +
  scale_x_discrete(limits = c("species", "flipper length", "body mass"), 
                   expand = c(.05, .3)) +
  geom_alluvium(aes(fill = island), 
                width = 1/12, 
                alpha = 0.7, 
                knot.pos = 0.4) +
  geom_stratum(width = 1/3) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, name = "Islands") + 
  labs(
    title = "How big are penguins at Palmer Station LTER?",
    subtitle = "A comparison of flipper length and body mass by each species and islands",
    caption = "data source: https://oceaninformatics.ucsd.edu/datazoo/catalogs/pallter/datasets/220"
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_blank()
  ) 
  ggsave("Penguins.png")


# Blog --------------------------------------------------------------------

# where can they be found?
penguins_02 %>%
  ggplot(aes(axis1 = species, 
             axis2 = island, 
             y = freq)) +
  scale_x_discrete(limits = c("species", "island"),
                   expand = c(.005, 0.025)) +
  geom_alluvium(aes(fill = sex),
                width = 1/12, 
                alpha = 0.7, 
                knot.pos = 0.4) +
  geom_stratum(width = 1/3) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_fill_manual(values  = c("orange1", "cyan4")) +
  labs(
    title = "At which island can they be found?",
    subtitle = "",
    caption = ""
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank()
  ) +
  ggsave("penguin_viz1.png")

# which species has a bigger culmen and flipper?
penguins_01 %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm, group = species)) +
  geom_point(aes(shape = species, colour = species)) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, name = "Species") + 
  labs(
    title = "Culmen vs flipper length",
    subtitle = "",
    x = "Culmen length", 
    y = "Flipper length",
    caption = ""
  ) +
  theme_fivethirtyeight() 
  ggsave("penguin_viz2.png")

# Penguin size comparison
# By island
penguins_02 %>%
  ggplot(aes(axis1 = species, 
             axis2 = avg_flipper_length, 
             axis3 = avg_body_mass, 
             y = freq)) +
  scale_x_discrete(limits = c("species", "flipper length", "body mass"), 
                   expand = c(.05, .3)) +
  geom_alluvium(aes(fill = island), 
                width = 1/12, 
                alpha = 0.7, 
                knot.pos = 0.4) +
  geom_stratum(width = 1/3) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, name = "Islands") + 
  labs(
    title = "Which species is bigger?",
    subtitle = "A comparison of flipper length and body mass by each islands",
    caption = ""
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_blank()
  ) 
  ggsave("penguin_viz3.png")

# By sex
penguins_02 %>%
  ggplot(aes(axis1 = species, 
             axis2 = avg_flipper_length, 
             axis3 = avg_body_mass, 
             y = freq)) +
  scale_x_discrete(limits = c("species", "flipper length", "body mass"), 
                   expand = c(.05, .3)) +
  geom_alluvium(aes(fill = sex), 
                width = 1/12, 
                alpha = 0.7, 
                knot.pos = 0.4) +
  geom_stratum(width = 1/3) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, name = "Islands") + 
  labs(
    title = "Which species is bigger?",
    subtitle = "A comparison of flipper length and body mass by each species",
    caption = ""
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_blank()
  ) 
  ggsave("penguin_viz4.png")


# Citation ----------------------------------------------------------------
citation("palmerpenguins")

