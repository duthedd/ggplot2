#' @import rjson

json <- function(x, ...) toJSON(compact_rec(as.list(x, ...)))
as.list.proto <- function(x, ...) x$as.list(...)

#
#' @examples
#' as.list(scale_x_continuous())
#' json(scale_x_continuous())
as.list.scale <- function(x, ...) {
  x$aesthetics <- x$aesthetics[1]
  x$domain <- x$range$range
  x$scale_name <- NULL
  x$class <- rev(class(x))
  
  # Also need to deal with functions (e.g. oob, trans, rescaler, ...)
  # Not so important for the first round, trans especially is important
  # for full plot specification
  
  vector_only(x)
}

as.list.facet <- function(x, ...) {
  # Don't support facetting until I can figure out what the scales
  # data structure should look like - probably should be nested list.
  if (!inherits(x, "null")) {
    stop("Facetting not currently supported", call. = FALSE)
  }
  
  x$class <- rev(class(x))
  x$facets <- as.character(x$facets)
  vector_only(x)  
}

as.list.coord <- function(x, ...) {
  x$class <- rev(class(x))
  vector_only(x)  
}

# At what point should I combine the aesthetics from the plot and the layer?
# Or should this be something that the presentation layer can do?
Layer$as.list <- function(., ...) {
  list(
    geom = c(list(name = .$geom$objname), .$geom_params),
    stat = c(list(name = .$stat$objname), .$stat_params),
    adj  = .$position$as.list(),
    mapping = as.character(.$mapping),
    data = if (!is.null(.$data)) digest(.$data),
    show_guide = .$show_guide
  )
}

Position$as.list <- function(., ...) {
  compact(list(name = .$objname, width = .$width, height = .$height))
}

as.list.ggplot <- function(x, ...) {
  built <- ggplot_build(x)
  
  # For now, assuming no facetting (facet_null)
  x_range <- name_matches(built$ranges, "$x\\.")
  y_range <- name_matches(built$ranges, "$y\\.")
  x_scale <- c(as.list(built$x_scales[[1]]), x_range)
  y_scale <- c(as.list(built$y_scales[[1]]), y_range)
  # All other non-position scales
  np_scales <- lapply(built$scales$scales, as.list, ...)

  spec <- list(
    layers = lapply(x$layers, as.list, ...),
    scales = c(list(x = x_scale, y = y_scale), scales),
    facet = as.list(x$facet, ...),
    coord = as.list(x$coord, ...),
    mapping = as.character(x$mapping),
    data = if (!is.null(x$data)) digest(x$data)
  )
  
  list(spec = spec, data = data)
}

# JSON inspection/validation -------------------------------------------------

# Needs jsonpp (http://jmhodges.github.com/jsonpp/) to work
pp <- function(x) {
  json <- json(x)
  
  jsonpp <- pipe("jsonpp")
  on.exit(close(jsonpp))
  
  writeChar(json, jsonpp, eos = NULL)
}
val <- function(x) {
  json <- json(x)
  browseURL(paste0("http://jsonlint.com/?json=", URLencode(json)))
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
name_matches <- function(x, pattern) {
  matches <- grepl(pattern, names(x))
  new_names <- gsub(pattern, "", names(x)[matches])
  setNames(x[matches], new_names)
}
