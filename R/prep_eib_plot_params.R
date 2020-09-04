
#' prep_eib_plot_params
#' 
#' @param he 
#' @param ... 
#'
#' @keywords dplot
#'
#' @export
#' 
prep_eib_plot_params <- function(he, ...) {
  
  extra_args <- list(...)
  
  # extract subsets of user-defined parameters
  area_params <- 
    extra_params[
      names(extra_params) %in% c("area_include", "area_color")]
  names(area_params) <- gsub("area_", "", names(area_params))
  
  line_params <-
    extra_params[
      names(extra_params) %in% c("line_colors", "line_types", "line_cri_colors")]
  names(line_params) <- gsub("line_", "", names(line_params))
  
  # credible interval
  cri_params <- list(
    extra_params[names(extra_params) %in% "cri.quantile"],
    extra_params[names(extra_params) %in% "alpha"])
  
  cri_params <- modifyList(
    list(cri.quantile = FALSE,
         alpha = 0.05,
         plot = he$n_comparisons == 1),
    cri_params)
  
  if (cri_params$alpha < 0 | cri_params$alpha > 1) {
    warning("Argument alpha must be between 0 and 1. Reset to default value 0.95.")
    cri_params$alpha <- 0.05
  }
  if (cri_params$alpha > 0.80 & cri_params$cri.quantile) {
    warning(
      "Recommend adopting the normal approximation of the credible interval for high values of alpha.
          Please set the argument cri.quantile = FALSE to use the normal approximation.")
  }
  
  cri <- 
    if (cri_params$plot) {
      do.call(eib.plot.cri, c(he = he, cri_params))
    } else {
      data.frame(low = NA, upp = NA)}
  
  default_params <-
    list(title = FALSE,
         xlab = FALSE,
         ylab = FALSE,
         ylim = range(c(he$eib), cri$low, cri$upp, na.rm = TRUE),
         area = list(include = FALSE,
                     color = "grey"),
         line =
           list(colors = "black",
                types = NULL,
                cri_colors = "grey50"))
  
  ext_params <- list(area = area_params,
                     line = line_params)
  
  modifyList(default_params, ext_params)
}

