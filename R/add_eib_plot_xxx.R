
#'
add_eib_plot_setup <- function(graph_params) {
  
}

#'
add_eib_line <- function(graph_params) {
  
  lines(he$k,
        he$eib,
        col = graph_params$line$colors[1],
        lty = ifelse(is.null(graph_params$line$types), 1, graph_params$line$types[1]))
  
  lines(he$k, he$eib[,j],
        lty = graph_params$line$types[min(j, length(graph_params$line$types))], 
        lwd = ifelse(plot.cri, lwd + 1, lwd), 
        col = graph_params$line$colors[min(j, length(graph_params$line$colors))])
}

#'
add_cri <- function(graph_params) {
  
  if (plot.cri) {
    ##TODO: matplot?
    lines(he$k, cri$low, col = graph_params$line$cri_colors[1], lty = 2)
    lines(he$k, cri$upp, col = graph_params$line$cri_colors[1], lty = 2)
  }
  
  lines(he$k,
        cri$low[cri$comp == j],
        lwd = lwd, 
        col = graph_params$line$cri_colors[min(j, length(graph_params$line$cri_colors))],
        lty = graph_params$line$types[min(j, length(graph_params$line$types))])
  lines(he$k,
        cri$upp[cri$comp == j],
        lwd = lwd, 
        col = graph_params$line$cri_colors[min(j, length(graph_params$line$cri_colors))],
        lty = graph_params$line$types[min(j, length(graph_params$line$types))])
}

#'
add_bep <- function(graph_params) {
  
  if (length(he$kstar) > 0 & is.null(size)) {
    abline(v = he$kstar, col = "dark grey", lty = "dotted")
    text(he$kstar, min(yl), paste("k* = ", he$kstar ,sep = ""))
  }
}

