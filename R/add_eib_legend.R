
#'
add_eib_legend <- function(graph_params) {
  
  if (is.numeric(alt.legend)&length(alt.legend) == 2) {
    temp <- ""
    if (alt.legend[2] == 0)
      temp <- paste0(temp,"bottom")
    else
      temp <- paste0(temp,"top")
    if (alt.legend[1] == 1)
      temp <- paste0(temp,"right")
    else
      temp <- paste0(temp,"left")
    alt.legend <- temp
    if (length(grep("^(bottom|top)(left|right)$", temp)) == 0)
      alt.legend <- FALSE
  }
  if (is.logical(alt.legend)) {
    
    alt.legend <-
      if (!alt.legend)
       "topleft"
    else
      "topright"
  }
  
  text <- paste0(he$interventions[he$ref]," vs ",he$interventions[he$comp])
  legend(
    alt.legend,
    text,
    cex = 0.7,
    bty = "n",
    lty = plot_aes$line$types,
    lwd = ifelse(plot.cri,lwd + 1, lwd))
}


