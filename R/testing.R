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

points <- raffle_setup_points(NOLA, Property_ID)
polys <- raffle_setup_polys(NOLA_polys, GEOID, Housing)
intersects <- raffle_intersect(points, polys, Housing, 200)
intersects <- raffle_integrate(intersects)
points <- raffle_choose_winner(
  points, intersects, Property_ID, GEOID, diagnostic = TRUE)

str_raffle(NOLA, NOLA_polys, Property_ID, GEOID, Housing, cores = 5)


# Benchmarking component functions

library(bench)
library(profvis)

mark(raffle_setup_points(NOLA, Property_ID))
mark(raffle_setup_polys(NOLA_polys, GEOID, Housing))
mark(raffle_intersect(points, polys, Housing, 200))
profvis(raffle_intersect(points, polys, Housing, 200))
