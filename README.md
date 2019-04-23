# str_raffle
 
A function for probablistically assigning STR listings to administrative
geographies (e.g. census tracts) based on reported latitude/longitude.
The function works by combining a known probability density function (e.g.
Airbnb's spatial obfuscation of listing locations) with an additional source
of information about possible listing locations--either population or housing
densities.

### Usage
```
str_raffle(points, polys, point_ID, poly_ID, units, distance = 200,
  diagnostic = FALSE, cores = 1)
```
