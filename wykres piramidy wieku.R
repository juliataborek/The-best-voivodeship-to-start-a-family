library(readxl)
df <- read_excel("2021 wiek.xlsx")

library(ggplot2)
library(magrittr)
library(dplyr)
library(vctrs)
library(scales)

# change male population to negative
df %>% mutate(
  Population = ifelse(Gender=="M", Population*(-1),
                      Population*1))%>%
  ggplot(aes(x = Age, y = Population, fill=Gender)) + 
  geom_bar(stat = "identity") +
  #geom_line(aes(x = Age, y = Population, group = Gender)) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "Grupa wiekowa", y = "Liczebność grupy", title = "Piramida ludności", fill = "Płeć")

# change male population to negative
df %>% mutate(
  Population = ifelse(Gender=="M", Population*(-1),
                      Population*1))%>%
  ggplot(aes(x = Population, y = Age, fill=Gender)) + 
  geom_bar(stat = "identity") +
  #coord_flip() +
  scale_fill_brewer(type = "seq",palette = 7) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))


df$age <- factor(df$Age)
apyramid::age_pyramid(data = df,
                       age_group = "age",
                       split_by = "Gender",
                      count = "Population")

