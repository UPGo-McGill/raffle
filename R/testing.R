## Testing file

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


raffle_setup_points(NOLA, Property_ID)