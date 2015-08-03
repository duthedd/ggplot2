#' Define margins.
#'
#' This is a convenience function that creates a grid unit object of the
#' correct length to use for setting margins.
#'
#' @export
#' @param t,b,r,l Dimensions of each margin. (To remember order, think trouble).
#' @param unit Default units of dimensions. Defaults to "pt" so it
#'   can be most easily scaled with the text.
#' @export
#' @examples
#' margin(4)
#' margin(4, 2)
#' margin(4, 3, 2, 1)
margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  structure(unit(c(t, r, b, l), unit), class = c("margin", "unit"))
}


margin_height <- function(grob, margins) {
  if (is.zero(grob)) return(unit(0, "cm"))

  grobHeight(grob) + margins[1] + margins[3]
}

margin_width <- function(grob, margins) {
  if (is.zero(grob)) return(unit(0, "cm"))

  grobWidth(grob) + margins[2] + margins[4]
}

titleGrob <- function(label, x, y, hjust, vjust, angle = 0, gp = gpar(),
                         margin = NULL, side = "t") {
  if (is.null(label))
    return(zeroGrob())

  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }

  theta <- angle / 180 * pi

  side <- match.arg(side, c("t", "r", "b", "l"))

  angle <- angle %% 360



  if (side == "t") {
    vjust <- 0
    y <- y %||% margin[3]
    x <- x %||% unit(hjust, "npc")
  } else if (side == "b") {
    vjust <- 1
    y <- y %||% (unit(1, "npc") - margin[1])
    x <- x %||% unit(hjust, "npc")
  } else if (side == "l") {
    if (angle == 90) {
      xp <- 1 - vjust
      yp <- hjust
    } else if (angle == 180) {
      xp <- 1 - hjust
      yp <- 1 - vjust
    } else if (angle == 270) {
      xp <- vjust
      yp <- 1 - hjust
    } else {
      xp <- hjust
      yp <- vjust
    }

    x <- x %||% unit(xp, "npc")
    y <- y %||% unit(yp, "npc")
  } else {
    stop("Not yet implemented")
  }

  grob <- textGrob(label, x, y, hjust = hjust, vjust = vjust, rot = angle,
    gp = gp)

  # Add on extra attributes needed by title grob
  grob$margin <- margin
  grob$side <- side
  class(grob) <- c("titleGrob", class(grob))
  grob
}

#' @export
widthDetails.titleGrob <- function(x) {
  textWidth <- NextMethod()
  if (x$side %in% c("t", "b"))
    return(textWidth)

  textWidth + x$margin[2] + x$margin[4]
}

#' @export
heightDetails.titleGrob <- function(x) {
  textHeight <- NextMethod()
  if (x$side %in% c("r", "l"))
    return(textHeight)

  textHeight + x$margin[1] + x$margin[3]
}

drawDetails.titleGrob <- function(x, ...) {
  grid.points(x$x[1], x$y[1], pch = 20, gp = gpar(col = "grey50"))
  NextMethod()
}
