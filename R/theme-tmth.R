
#' _Trees, maps, and theorems_ plot theme
#'
#' A ggplot2 theme following some guidelines from the book
#' _Trees, maps, and theorems_.
#'
#' @param base_size Foundational font size
#' @param base_family Foundational font family
#' @param base_line_size Foundational line size
#' @param base_rect_size Foundational rectangle size
#' @param aspect_ratio Foundational aspect ratio (default: golden mean)
#'
#' @return A new theme derived from [ggplot2::theme_bw()]
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars) +
#'   geom_point(aes(wt, mpg)) +
#'   scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6)) +
#'   scale_y_continuous(limits = c(10, 35)) +
#'   theme_tmth() +
#'   coord_capped_cart(left = "both", bottom = "both")
#' }
theme_tmth <- function(
    base_size = 12,
    base_family = "Source Sans Pro",  # FIXME: Check if installed
    base_line_size = 0.4,
    base_rect_size = 0.4,
    aspect_ratio = 2 / (1 + sqrt(5))) {

  new_theme <- ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )

  line_color <- "gray50"

  new_theme <- new_theme +
    ggplot2::theme(
      aspect.ratio = aspect_ratio,
      plot.margin = ggplot2::unit(
        rep(12, 4),
        units = "pt"
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(
        color = line_color,
        lineend = "square"
      ),
      axis.ticks.length = ggplot2::unit(-4, "pt"),
      axis.line.x.bottom = ggplot2::element_line(
        color = line_color,
        lineend = "square"
      ),
      axis.text.x.bottom = ggplot2::element_text(
        color = line_color,
        margin = ggplot2::margin(t = 6)
      ),
      axis.title.x.bottom = ggplot2::element_text(
        margin = ggplot2::margin(t = 6)
      ),
      axis.line.y.left = ggplot2::element_line(
        color = line_color,
        lineend = "square"
      ),
      axis.text.y.left = ggplot2::element_text(
        color = line_color,
        margin = ggplot2::margin(r = 6)
      ),
      axis.title.y.left = ggplot2::element_text(
        margin = ggplot2::margin(r = 6)
      )
    )

  new_theme
}
