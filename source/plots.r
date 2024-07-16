plot_cvar <- function(data, x = 'p', y = 'CVaR', zz = 1.64485, cvar.se =  "CVaR.se", alpha = 0.5, ylabel = "${CVaR}_{\\alpha}$", xlabel = "$\\alpha$") { 
  
  # Creación del gráfico
  p <- ggplot(data) +
    aes_string(x = x, y = y, ymax = paste0(y, " + ", zz, " * ", cvar.se), ymin = paste0(y, " - ", zz, " * ", cvar.se)) +
    geom_line() +
    geom_point() +
    geom_ribbon(alpha = 0.5) +
    ylab(TeX(ylabel)) +
    xlab(TeX(xlabel))
  
  return(p)
}

library(ggplot2)
library(latex2exp)

plot_cvar_groups <- function(data, x, y, ymax, ymin, color, fill = color, shape = NULL, ylab_text, xlab_text, alpha = 0.5, ribbon_color = NA, shape_legend_title = NULL, color_legend_title = NULL, fill_legend_title = NULL) {
  p <- ggplot(data) + 
    aes_string(
      x = x, y = y, 
      ymax = ymax, ymin = ymin, 
      color = color, fill = fill, shape = shape
    ) + 
    geom_line() + 
    geom_point() + 
    geom_ribbon(alpha = alpha, color = ribbon_color) +
    ylab(TeX(ylab_text)) + 
    xlab(TeX(xlab_text))
  
  if (!is.null(shape_legend_title)) {
    p <- p + guides(shape = guide_legend(title = shape_legend_title))
  }
  
  if (!is.null(color_legend_title)) {
    p <- p + guides(color = guide_legend(title = TeX(color_legend_title)))
  }
  
  if (!is.null(fill_legend_title)) {
    p <- p + guides(fill = guide_legend(title = TeX(fill_legend_title)))
  }
  
  return(p)
}
