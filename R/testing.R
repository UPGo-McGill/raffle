## Testing file

# Libraries

lapply(c("sf","dplyr","spatstat","polyCub", "purrr"), library,
       character.only = TRUE)


# Import data

NOLA <-
  st_read("data", "NO_points") %>% 
  as_tibble() %>% 
  select(Property_ID = NOLA_Point, geometry) %>% 
  st_as_sf()

NOLA_polys <-
  st_read("data", "NO_BG2") %>% 
  as_tibble() %>% 
  select(GEOID, Housing = Housing_Un, geometry) %>% 
  st_as_sf()


# Testing component functions

str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 5)


# Benchmarking

library(bench)
library(profvis)

mark(
  "1" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 1)},
  "2" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 2)},
  "3" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 3)},
  "4" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 4)},
  "5" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 5)},
  "6" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 6)},
  "7" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 7)},
  check = FALSE)

mark(
  "1" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 1)},
  "7" = {str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 7)},
  check = FALSE)

t0 <- Sys.time()
str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 1)
t1 <- Sys.time()
str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 3)
t2 <- Sys.time()
str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 5)
t3 <- Sys.time()
str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 7)
t4 <- Sys.time()

t1 - t0
t2 - t1
t3 - t2
t4 - t3

