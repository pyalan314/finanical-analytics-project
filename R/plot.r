library(ggplot2)

TRANS_THEME <- theme(
  # panel.background = element_rect(fill='transparent'),
  panel.background = element_rect(fill='#eeeeee'),
  plot.background = element_rect(fill='transparent', color=NA),
  # panel.grid.major = element_blank(),
  # panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

set_default_theme <- function(){
  theme_set(TRANS_THEME)
}

fill_density_curve <- function(density.func, from, to, fill){
  if (to == Inf){
    to <- max(density.func$x)
  }
  if (from == -Inf){
    from <- min(density.func$x)
  }
  geom_area(
    stat = "function",
    fun = function(x) approx(density.func$x, density.func$y, xout=x)$y,
    fill = fill,
    xlim = c(from, to)
  )
}

# genPdfSegment <- function(pdf.func, value, col){
#   prob <- approx(pdf.func$x, pdf.func$y, xout=value)$y
#   tibble(
#     x=c(value),
#     y=c(prob),
#     xend=c(value),
#     yend=c(-Inf),
#     col=col,
#   )
# }

gen_pdf_segments <- function(pdf.func, table){
  prob.func <- function(x) approx(pdf.func$x, pdf.func$y, xout=x)$y
  data <- tibble(
    x=table$value,
    y=sapply(table$value, prob.func),
    xend=table$value,
    yend=c(-Inf),
    col=table$col,
  )
  geom_segment(data=data, aes(x = x, y = y, xend = xend, yend = yend, col = col))
}

# genCdfSegment <- function(cdf.func, value, col){
#   prob <- cdf.func(value)
#   tibble(
#     x=c(-Inf, value),
#     y=c(prob, prob),
#     xend=c(value, value),
#     yend=c(prob, -Inf),
#     col=col,
#   )
# }

gen_cdf_segments <- function(cdf.func, table){
  prob.func <- function(x) approx(pdf.func$x, pdf.func$y, xout=x)$y
  data <- tibble(
    x=table$value,
    y=sapply(table$value, cdf.func),
    xend=table$value,
    yend=c(-Inf),
    col=table$col,
  )
  geom_segment(data=data, aes(x = x, y = y, xend = xend, yend = yend, col = col))
}
