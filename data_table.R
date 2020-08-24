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











