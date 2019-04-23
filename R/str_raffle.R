#' Raffle to assign STR listings to administrative units for spatial analysis
#' 
#' This function takes STR listings and assigns them probabilistically to
#' one of the administrative units (e.g. census tracts) in which, given their
#' reported location, they could be located in. The function uses a combination
#' of a reversal of Airbnb's method for obfuscating listing locations and a
#' weighting of possible locations by population or housing-unit data.


## 1. Load libraries -----------------------------------------------------------

lapply(c("sf","dplyr","spatstat","polyCub"), library, character.only = TRUE)


## 2. Point setup function, returns points -------------------------------------

raffle_setup_points <- function(points, point_ID) {
  
  point_ID <- enquo(point_ID)
  
  points <- 
    points %>%
    filter(!! point_ID > 0) %>%
    arrange(!! point_ID)
  
  points
}


## 3. Polygon setup function, returns polys ------------------------------------

raffle_setup_polys <- function(polys, poly_ID, units){
  
  poly_ID <- enquo(poly_ID)
  units <- enquo(units)
  
  polys <- 
    polys %>%
    filter(!! units > 0) %>%
    st_set_agr("constant") %>% # Prevent warnings from the st operations
    mutate(
      !! poly_ID := as.character(!! poly_ID), # Make sure poly_ID is not factor
      poly_area = st_area(.) # Calculate polygon areas
    ) %>% 
    st_set_agr("constant")
  
  polys
}


## 4. Intersect point buffers with polygons, returns intersects ----------------

raffle_intersect <- function(points, polys, units, distance) {
  
  units <- enquo(units)
  
  # Get point coordinates for future use
  points <- 
    points %>%
    mutate(
      point_x = st_coordinates(.)[,1], 
      point_y = st_coordinates(.)[,2]
      )
  
  # Generate buffers and intersect with polygons
  intersects <-
    points %>%
    st_buffer(dist = distance, nQuadSegs = 10) %>% 
    st_set_agr("constant") %>%
    st_intersection(polys)
  
  # Estimate int_units
  intersects <-
    intersects %>%
    mutate(int_units = as.numeric(!! units * st_area(geometry) / poly_area)) %>% 
    st_set_agr("constant")
  
  # Transform intersects relative to point coordinates
  intersects <- 
    intersects %>% 
    mutate(geometry = pmap(list(geometry, point_x, point_y), ~{
      ..1 - c(..2, ..3)
      }) %>% 
        st_sfc())
  
  intersects
}
 
## 5. PDF helper function, returns vector of probabilities ---------------------

raffle_pdf <- function(x) {
  dnorm(sqrt(x[,1]^2 + x[,2]^2), mean = 100, sd = 50, log = FALSE) *
    (1 / (2 * pi))
}

## 6. Integrate the PDF over intersect polygons, returns intersects ------------

raffle_integrate <- function(intersects, units) {
  
  units <- enquo(units)
  
  intersects$probability <-
    mapply(function(geom, units){
      polyCub.midpoint(as(geom,"Spatial"), function(x) {
        dnorm(sqrt(x[,1]^2 + x[,2]^2), mean = 100, sd = 50, log = FALSE) *
          (1 / (2 * pi))}
      ) * 
        units
    },
    intersects$geometry,
    intersects$int_units)
  
  intersects
}
  

## 6. Determine winners, returns points ----------------------------------------

raffle_choose_winner <- function() {
  
  results <-
    intersects %>%
    st_drop_geometry() %>%
    group_by(Property_ID)
  
  if (diagnostic == TRUE) {
    results <-
      results %>%
      summarize(winner = as.character(base::sample(ID, 1, prob = probability)),
                candidates = list(matrix(c(ID,(probability)/sum(probability)),ncol = 2))
      )
  } else {
    results <-
      results %>%
      summarize(winner = as.character(base::sample(ID, 1, prob = probability))
      )
  }
  
  points <-
    results %>% 
    left_join(points, ., by = "Property_ID")
  
  return(points)
}

## 4. MAIN compiler to run the whole process -----------------------------------

raffle <- function(
  points, polys, point_ID, poly_ID, units,
  distance = 200, diagnostic = FALSE, cores = 1) {
  
  lapply(c("sf","dplyr","spatstat","polyCub"), 
         library, character.only = TRUE)
  
  points <- raffle_setup_points(points, point_ID)
  polys <- raffle_setup_polys(polys, poly_ID, units)
  
  # Run single-core version by default, unless multiple cores are specified
  if (cores >= 2){
    library(parallel)
    points_list <- split(points, cut(seq_along(points$Property_ID),
                                     cores,
                                     labels = FALSE))
    points_list <- mclapply(points_list,
                            raffle_function,
                            polys = polys,
                            distance = distance,
                            diagnostic = diagnostic,
                            mc.cores = cores)
    points <- do.call(rbind, points_list)
  } else {
    points <- raffle_function(points,polys,distance = distance, diagnostic = diagnostic)
  }
  
  return(points)
}

