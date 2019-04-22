#' Raffle to assign STR listings to administrative units for spatial analysis
#' 
#' This function takes STR listings and assigns them probabilistically to
#' administrative units (e.g. census tracts) using a combination of the reported
#' latitude/longitude of the listings and population or housing-unit data.


## 1. Load libraries -----------------------------------------------------------

lapply(c("sf","dplyr","spatstat","polyCub"), 
       library, character.only = TRUE)


## 2. Point setup function, returns points -------------------------------------

raffle_setup_points <- function(points, point_ID) {
  point_ID <- enquo(point_ID)
  
  points %>%
    filter(!! point_ID > 0) %>%
    arrange(!! point_ID) %>%
    mutate(
      point_x = st_coordinates(.)[,1], # Get point coordinates for future use
      point_y = st_coordinates(.)[,2]
    )
}


## 3. Polygon setup function, returns polys ------------------------------------

raffle_setup_polys <- function(polys, poly_ID, units){
  poly_ID <- enquo(poly_ID)
  units <- enquo(units)
  
  polys %>%
    filter(!! units > 0) %>%
    st_set_agr("constant") %>% # Prevent warnings from the st operations
    mutate(
      !! poly_ID := as.character(!! poly_ID), # Make sure poly_ID is not factor
      poly_area = st_area(.) # Calculate polygon areas
    ) %>% 
    st_set_agr("constant")
}


## 4. RAFFLE FUNCTION, returns points  -----------------------------------------

raffle_function <- function(
  points, polys, distance = distance, diagnostic = diagnostic)  {
  
  # Generate buffers, intersect with polygons, estimate units, group by Property_ID
  intersects <-
    points %>%
    st_set_agr("constant") %>%
    st_buffer(dist = distance, nQuadSegs = 10) %>% 
    st_set_agr("constant") %>%
    st_intersection(polys) %>%
    mutate(int_units = as.numeric(units*st_area(.) / poly_area)) %>% 
    select(-units,-poly_area) %>% 
    st_set_agr("constant") %>%
    arrange(Property_ID, ID)
  
  # Transform intersects relative to point coordinates
  st_geometry(intersects) <-   
    mapply(function(geom,x,y){
      geom <- geom - c(x,y)
      geom
    },
    st_geometry(intersects),
    intersects$point_x,
    intersects$point_y,
    SIMPLIFY = FALSE) %>%
    st_sfc()
  
  # Integrate the PDF over intersect polygons
  intersects$probability <-
    mapply(function(geom,units){
      polyCub.midpoint(as(geom,"Spatial"), function(x) {
        dnorm(sqrt(x[,1]^2 + x[,2]^2), mean = 100, sd = 50, log = FALSE) *
          (1 / (2 * pi))}
      ) * 
        units
    },
    intersects$geometry,
    intersects$int_units)
  
  # Determine winners, add candidates field if diagnostic == TRUE
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
    left_join(points, ., by = "Property_ID") %>% 
    select(-point_x,-point_y)
  
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

