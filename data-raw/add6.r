library(sf)
add6 <- st_read("data-raw/add6", "Sub-antarctic_coastline_low_res_polygon_to30S", stringsAsFactors = FALSE)
bad <- !st_is_valid(add6)
invalid <- add6[which(bad)[1], ]
plot(invalid)


library(rangl)
ri <- rangl(as(as(invalid, "Spatial"), "SpatialLinesDataFrame"))
rg_as_sf <- function(x) {
  mkmatline <- function(a, v) {
    st_linestring(a %>% inner_join(v, "vertex_") %>% select(x_, y_) %>% as.matrix())
  }
    ## expect line segments
  stopifnot(all(c("lXv", "l") %in% names(x) ))
 g <- st_sfc(lapply(split(x$lXv, ordered(x$lXv$segment_, unique(x$lXv$segment_))), mkmatline, x$v), crs = x$meta$proj[1])
  d <- data.frame( id = seq_len(length(g)))
  d[["geometry"]] <- g
  st_as_sf(d)
}
si <- rg_as_sf(ri)

## these ones are the offenders in the first bad object
nodal <- st_as_sf(rgeos::gNode(as(si[c(82, 84), ], "Spatial")))
plot(nodal, asp = 1.2)
sp::plot(as(invalid, "Spatial"), add = TRUE, col = "grey", rule = "evenodd")
points(as(as(nodal, "Spatial"), "SpatialMultiPoints"))
text(ri$v$x_, ri$v$y_, lab = ri$v$vertex_, cex = 0.8, pos = 2, xpd = NA)
