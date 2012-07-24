#' @import rjson

json <- function(x, ...) toJSON(compact_rec(as.list(x, ...)))
as.list.proto <- function(x, ...) x$as.list(...)

# Needs jsonpp (http://jmhodges.github.com/jsonpp/) to work
jsonpp <- function(x) {
  json <- json(x)
  
  jsonpp <- pipe("jsonpp")
  on.exit(close(jsonpp))
  
  writeChar(json, jsonpp, eos = NULL)
}
val <- function(x) {
  json <- json(x)
  browseURL(paste0("http://jsonlint.com/?json=", URLencode(json)))
}

# How to deal with functions? oob, trans, rescaler, ...
# These are probably not so important for the first round
#
#' @examples
#' as.list(scale_x_continuous())
#' json(scale_x_continuous())
as.list.scale <- function(x, ...) {
  x$aesthetics <- x$aesthetics[1]
  x$domain <- x$range$range
  x$scale_name <- NULL
  x$class <- rev(class(x))
  vector_only(x)
}

as.list.facet <- function(x, ...) {
  x$class <- rev(class(x))
  x$facets <- as.character(x$facets)
  vector_only(x)  
}

as.list.coord <- function(x, ...) {
  x$class <- rev(class(x))
  vector_only(x)  
}

# At what point should I combine the aesthetics from the plot and the layer?
# Or should this be something that happens on the presentation layer?
Layer$as.list <- function(., ...) {
  list(
    geom = c(list(name = .$geom$objname), .$geom_params),
    stat = c(list(name = .$stat$objname), .$stat_params),
    adj  = .$position$as.list(),
    mapping = as.character(.$mapping),
    data = digest(.$data),
    show_guide = .$show_guide
  )
}

Position$as.list <- function(., ...) {
  compact(list(name = .$objname, width = .$width, height = .$height))
}

as.list.ggplot <- function(x, ...) {
  
  spec <- list(
    layers = lapply(x$layers, as.list, ...),
    scales = lapply(x$scales$scales, as.list, ...),
    facet = as.list(x$facet, ...),
    coord = as.list(x$coord, ...),
    mapping = as.character(x$mapping),
    data = digest(x$data)
  )
  
  built <- ggplot_build(x)
  
  list(spec = spec, data = data)
}

# Utility functions ----------------------------------------------------------

vector_only <- function(x) {
  x <- unclass(x)
  
  # Recursively apply to lists
  list <- vapply(x, is.list, logical(1))
  x[list] <- lapply(x[list], vector_only)
  
  # Remove any non vector (e.g. function) components
  atomic <- vapply(x, is.vector, logical(1))
  x[atomic]
}
compact_rec <- function(x) {
  list <- vapply(x, is.list, logical(1))
  x[list] <- lapply(x[list], compact2)

  Filter(function(x) length(x) > 0, x)
}