
#' eib_plot_base
#'
#' @template args-he 
#' @param wtp Willingness-to-pay
#' @param pos_legend Position of legend
#' @param graph_params List
#'
#' @return 
#' @export
#' 
eib_plot_base <- function(he,
                          wtp,
                          pos_legend,
                          graph_params) {
  
  graph_params <- eib_base_params(graph_params)
  
  add_eib_plot_setup(graph_params)
  abline(h = 0, col = "grey")
  add_eib_line(graph_params)
  add_cri(graph_params)
  add_bep(graph_params)
  add_eib_legend(graph_params)
}

