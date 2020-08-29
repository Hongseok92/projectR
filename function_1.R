library(tidyverse)
library(data.table)
library(pbapply)

square <- function(x){
  sq <- x^2
  df <- tibble(value=x, value_squared=sq)
  return(df)
}

square(3)
