library(tidyr)
library(dplyr)
library(ggplot2)

mpg %>% 
  filter(manufacturer == "audi") %>% 
  group_by(model) %>% 
  summarize(hwy_mean = mean(hwy))

starwars %>% 
  filter(
    species == "Human",
    height  >= 190
  )

starwars %>% 
  filter(grepl("Skywalker", name))

starwars %>% 
  filter(!is.na(height))

starwars %>% 
  arrange(birth_year)

starwars %>% 
  arrange(name)

starwars %>% 
  arrange(desc(birth_year))

starwars %>% 
  select(name:skin_color, species, -height)

starwars %>% 
  select(alias=name, crib=homeworld, sex=gender)

starwars %>% 
  rename(alias=name, crib=homeworld, sex=gender)

starwars %>% 
  select(name, contains("color"))

starwars %>% 
  select(name, birth_year) %>% 
  mutate(dog_years = birth_year * 7) %>% 
  mutate(comment = paste0(name, " is ", dog_years, " in dog years."))

starwars %>% 
  select(name, birth_year) %>% 
  mutate(
    dog_years = birth_year * 7, 
    comment = paste0(name, " is ", dog_years, " in dog years.")
  )

starwars %>% 
  select(name, height) %>% 
  filter(name %in% c("Luke Skywalker", "Anakin Skywalker")) %>% 
  mutate(tall1 = height > 180) %>% 
  mutate(tall2 = ifelse(height > 180, "Tall", "Short"))

starwars %>% 
  select(name:eye_color) %>% 
  mutate(across(where(is.character), toupper)) %>% 
  head(5)

starwars %>% 
  group_by(species, gender) %>% 
  summarize(mean_height = mean(height), na.rm=T)

starwars %>% 
  slice(c(1,5))

starwars %>% 
  filter(gender=="female") %>%  pull(height)

starwars %>% count(species)
starwars %>% distinct(species)
starwars %>% 
  group_by(species) %>% mutate(num = n())

library(nycflights13)
flights
planes

left_join(flights, planes) %>% 
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, type, model)

left_join(
  flights,
  planes %>% rename(year_built = year), 
  by = "tailnum"
) %>% 
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, type, model, year_built) %>% 
  head(3)

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:5,
  X = rnorm(2, 0, 1),
  Y = rnorm(2, 0, 2),
  Z = rnorm(2, 0, 4)
)
stocks

tidy_stocks <- stocks %>% pivot_longer(-time, names_to = "stock", values_to = "price")

tidy_stocks %>% pivot_wider(names_from = "time", values_from = "price")

economists <- data.frame(
  name = c("A.Smith", "P.Samuelson", "M.Friedman")
)
economists %>% separate(name, c("first_name", "last_name"), sep=".")

jobs <- data.frame(
  name = c("Jack", "Jill"), 
  occupation = c("Homemaker", "Philosopher, Philanthropist, Troublemaker")
)
jobs

jobs %>% separate_rows(occupation)

gdp <- data.frame(
  yr = rep(2016, times = 4),
  mnth = rep(1, times = 4),
  dy = 1:4,
  gdp = rnorm(4, mean = 100, sd = 2)
)

gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-")
gdp_u <- gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-") %>% as_tibble()
gdp_u


library(lubridate)
gdp_u
gdp_u %>% mutate(date = ymd(date))

crossing(side=c("left", "right"), height=c("top", "bottom"))


























 