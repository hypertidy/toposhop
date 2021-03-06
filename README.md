
<!-- README.md is generated from README.Rmd. Please edit that file -->
toposhop
========

Early work on R-level topology fixes for simple features.

So far we have:

-   self-intersecting polygons, fixed by filtering out triangles that don't intersect (by evenodd rule) (same as 'bowtie' polygon)
-   wrong orientation of ring (sf doesn't complain about this, but the fix is fine)
-   Inner ring with one edge sharing part of an edge of the outer ring:
-   Dangling edge
-   Two adjacent inner rings
-   Polygon with an inner ring inside another inner ring

These do not yet work:

-   Outer ring not closed: <https://github.com/r-gris/sfdct/issues/12>

See Issues for examples and please add your own!

sfdct
-----

Using the constrained triangulation in <https://github.com/r-gris/sfdct> seems like that will fix many problems. The best documentation and explanation and examples of the problems is listed at the links below.

<https://3d.bk.tudelft.nl/hledoux/pdfs/12_agile.pdf>

<https://github.com/tudelft3d/prepair>

``` r
## first tests look fine
library(sf)
p <- data.frame(geometry = 'POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))')
d <- st_as_sf(p, wkt = "geometry")
st_is_valid(sf::st_union(sfdct::ct_triangulate(d)))
#[1] TRUE

## examples in data/ from prepair
fs <- c("data/CLC2006_180927.geojson",  "data/CLC2006_2018418.geojson")

library(sf)
d1 <- st_read(fs[1L])
st_is_valid(d1)
#[1] FALSE
#Warning message:
#In eval(substitute(expr), envir, enclos) :
 # Ring Self-intersection at or near point 20.137278515460533 68.32204464422999

library(sfdct)
d1_f <- ct_triangulate(d1)
d2 <- st_union(d1_f) ## back to multipolygon
st_is_valid(d2)
#[1] TRUE
```
