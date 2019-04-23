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


# Testing component functions

points <- raffle_setup_points(NOLA, Property_ID)
polys <- raffle_setup_polys(NOLA_polys, GEOID, Housing)
intersects <- raffle_intersect(points, polys, Housing, 200)


raffle_integrate <- function(intersects) {
  
  intersects %>% 
    mutate(probability = map2(geometry, int_units, ~{
      polyCub.midpoint(as(.x, "Spatial"), raffle_pdf) * int_units
      })
    )
  
  
  intersects$probability <-
    mapply(function(geom, units){
      polyCub.midpoint(as(geom,"Spatial"), function(x) {
        dnorm(sqrt(x[,1]^2 + x[,2]^2), mean = 100, sd = 50, log = FALSE) *
          (1 / (2 * pi))}
      ) * units
    },
    intersects$geometry,
    intersects$int_units)
}



# Benchmarking component functions

library(bench)
library(profvis)

mark(raffle_setup_points(NOLA, Property_ID))
mark(raffle_setup_polys(NOLA_polys, GEOID, Housing))
mark(raffle_intersect(points, polys, Housing, 200))
profvis(raffle_intersect(points, polys, Housing, 200))
mark({
  intersects %>%
    mutate(int_units = as.numeric(Housing * st_area(.) / poly_area)) %>% 
    select(-Housing, -poly_area) %>% 
    st_set_agr("constant")
})
