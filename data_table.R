library(data.table)

## Data 
input <- if (file.exists("flight14.csv")) {
  "flights14.csv"
} else{
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <-  fread(input)
flights
dim(flights)

DT <- data.table(
  ID = c("b", "b", "b", "a", "a", "c"),
  a = 1:6,
  b = 7:12,
  c = 13:18
)
DT
class(DT$ID)

# syntax
# i : on which rows? 
# j : what to do? 
# by : grouped by what?


# subset rows in i 
ans <- flights[origin == "JFK" & month == 6L]
head(ans)

ans <- flights[1:2]
ans

ans <- flights[order(origin, -dest)]
head(ans)

# select columns in j 
ans <- flights[, list(arr_delay)]
head(ans)


# What is the average height of the human characters by gender?
starwars_dt = as.data.table(starwars)
starwars_dt[
  species == "Human",
  mean(height, na.rm = T),
  by = gender]

starwars_dt[height>190 & species == "Human"]
starwars_dt[order(birth_year)]
starwars_dt[order(-birth_year)]

setorder(starwars_dt, birth_year, na.last=T)
starwars_dt[1:5, name:birth_year]

DT = data.table(x = 1:2)
DT[, x_sq := x^2]
DT_copy = copy(DT)
DT_copy[, x_sq := NULL]

DT2 = data.table(a = -2:2, b = LETTERS[1:5])
DT2[a<0, b := NA][]

DT[, ':=' (y=3:4, y_name = c("three", "four"))][]
DT[, ':=' (z=5:6, z_sq = z^2)][]
DT[, z := 5:6][, z_sq := z^2][]

library(magrittr)
DT %>% 
  .[, xyz := x+y+z] %>% 
  .[, xyz_sq := xyz^2] %>% 
  .[]

DT[, y_name := NULL][]

starwars_dt[1:2, c(1:3, 10)]
starwars_dt[1:2, .(name, height, mass, homeworld)]

starwars_dt[, !c("name", "height")]
setnames(starwars_dt, old = c("name", "homeworld"), new = c("alias", "crib"))[]
setnames(starwars_dt, old = c("alias", "crib"), new = c("name", "homeworld"), )[]

starwars_dt[1:2, .(alias = name, crib = homeworld)][]

starwars_dt[1:5, ] %>% 
  select(crib = homeworld, everything())

DT = data.table(x=1:10, y = LETTERS[1:10], key = "x")
DT = as.data.table(DF, key = "x")
setDT(DF, key = "x")

storms_dt_key = as.data.table(storms, key = c("name", "year", "month", "day"))
## collapse function for this keyed data.table. Everythin else stays the same
collapse_dt_key = function() {
  storms_dt_key[, .(wind=mean(wind), pressure = mean(pressure), category = first(category)), 
                by = .(name, year, month, day)]
}
## Run the benchmark on all three functions
microbenchmark(collapse_dplyr(), collapse_dt(), collapse_dt_key(), times = 10)


library(nycflights13)
flights_dt = as.data.table(flights)
planes_dt = as.data.table(planes)

# for dpylr 
library(dplyr)
left_join(
  flights, planes, by = "tailnum"
)

merge(
  flights_dt, 
  planes_dt,
  all.x = TRUE, ## omit for inner join
  by = "tailnum")

## Reshaping the data 
stocks = data.table(time = as.Date('2009-01-01') + 0:1,
                    X = rnorm(2, 0, 1),
                    Y = rnorm(2, 0, 2),
                    Z = rnorm(2, 0, 4))

melt(stocks, id.vars = "time")
stocks %>% 
  dt_pivot_longer(X:Z, names_to = "stocks", values_to = "price")

stocks_long = melt(stocks, id.vars = "time", 
                   variable.name = "stock", value.name = "price")
stocks_long

dcast(stocks_long, 
      time ~ stock, 
      value.var = "price")
stocks_long %>% 
  dt_pivot_wider(names_from = stock,
                 values_from = price)

## 
## Workflow
## 

library(ggplot2)
storms_dt[, .(wind=mean(wind), 
              pressure=mean(pressure),
              category=first(category)),
          by = .(name, year, month, day)] %>% 
  ggplot(aes(x = pressure, y = wind, col = category)) + 
  geom_point(alpha = 0.3) + 
  theme_minimal()

starwars_dt %>% 
  group_by(homeworld) %>% 
  summarize(height = mean(height, na.rm = T))

storms_dtplyr = lazy_dt(storms)
collapse_dtplyr = function() {
  storms_dtplyr %>% 
    group_by(name, year, month, day) %>% 
    summarize(wind = mean(wind), pressure = mean(pressure), category = first(category)) %>% 
    as_tibble()
}























