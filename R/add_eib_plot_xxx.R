
#'
add_eib_plot_setup <- function(graph_params) {
  
}

#'
add_eib_line <- function(graph_params) {
  
  lines(he$k,
        he$eib,
        col = plot_aes$line$colors[1],
        lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]))
  
  lines(he$k, he$eib[,j],
        lty = plot_aes$line$types[min(j, length(plot_aes$line$types))], 
        lwd = ifelse(plot.cri, lwd + 1, lwd), 
        col = plot_aes$line$colors[min(j, length(plot_aes$line$colors))])
}

#'
add_cri <- function(graph_params) {
  
  if (plot.cri) {
    lines(he$k, cri$low, col = plot_aes$line$cri_colors[1], lty = 2)
    lines(he$k, cri$upp, col = plot_aes$line$cri_colors[1], lty = 2)
  }
  
  lines(he$k,
        cri$low[cri$comp == j],
        lwd = lwd, 
        col = plot_aes$line$cri_colors[min(j, length(plot_aes$line$cri_colors))],
        lty = plot_aes$line$types[min(j, length(plot_aes$line$types))])
  lines(he$k,
        cri$upp[cri$comp == j],
        lwd = lwd, 
        col = plot_aes$line$cri_colors[min(j, length(plot_aes$line$cri_colors))],
        lty = plot_aes$line$types[min(j, length(plot_aes$line$types))])
}

#'
add_bep <- function(graph_params) {
  
  if (length(he$kstar) > 0 & is.null(size)) {
    abline(v = he$kstar, col = "dark grey", lty = "dotted")
    text(he$kstar, min(yl), paste("k* = ", he$kstar ,sep = ""))
  }
}

