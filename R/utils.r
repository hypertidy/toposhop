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

sf_as_sp_l <- function(x) {
  as(as(x, "Spatial"), "SpatialLinesDataFrame")
}
