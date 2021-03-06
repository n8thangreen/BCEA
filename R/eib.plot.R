
#' @rdname eib.plot
#' @importFrom graphics lines abline text legend
#' @import ggplot2
#' @export
#' 
eib.plot.bcea <- function(he,
                          comparison = NULL,
                          pos = c(1, 0),
                          size = NULL,
                          plot.cri = NULL,
                          graph = c("base", "ggplot2", "plotly"),
                          ...) {
  
  ##TODO: this is needed because its not defined in bcea()
  he$change_comp <- FALSE
  
  alt.legend <- pos
  # choose graphical engine
  if (any(is.null(graph)) || any(is.na(graph))) graph <- "base"
  
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  
  if (graph_choice == 2 &&
      !requireNamespace("ggplot2", quietly = TRUE) &
      requireNamespace("grid", quietly = TRUE)) {
    warning("Package ggplot2 and grid not found;
            eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  if (graph_choice == 3 &&
      !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found;
            eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  
  ## evaluate arguments
  ## possibility to include different values of confidence as "alpha"
  exArgs <- list(...)
  alpha <- 0.05
  plot_annotations <-
    list("exist" = list(
      "title" = FALSE,
      "xlab" = FALSE,
      "ylab" = FALSE))
  plot_aes <-
    list(
      "area" = list("include" = FALSE, "color" = "grey"),
      "line" = list(
        "colors" = "black",
        "types" = NULL,
        "cri_colors" = "grey50"))
  
  plot_aes_args <- c("area_include",
                     "area_color",
                     "line_colors",
                     "line_types",
                     "line_cri_colors")
  
  cri.quantile <- TRUE
  if (length(exArgs) >= 1) {
    if (exists("cri.quantile", where = exArgs))
      cri.quantile <- exArgs$cri.quantile
    if (exists("alpha", where = exArgs)) {
      alpha <- exArgs$alpha
      if (alpha < 0 | alpha > 1) {
        warning("Argument alpha must be between 0 and 1. Reset to default value 0.95.")
        alpha <- 0.05
      }
      if (alpha > 0.80 & cri.quantile) {
        warning(
          "It is recommended adopting the normal approximation of the credible interval for high values of alpha.
            Please set the argument cri.quantile = FALSE to use the normal approximation.")
      }
    }
    # if existing, read and store title, xlab and ylab
    for (annotation in names(plot_annotations$exist)) {
      if (exists(annotation, where = exArgs)) {
        plot_annotations$exist[[annotation]] <- TRUE
        plot_annotations[[annotation]] <- exArgs[[annotation]]
      }
    }
    # if existing, read and store graphical options
    for (aes_arg in plot_aes_args) {
      if (exists(aes_arg, where = exArgs)) {
        aes_cat <- strsplit(aes_arg, "_")[[1]][1]
        aes_name <- paste0(strsplit(aes_arg, "_")[[1]][-1], collapse = "_")
        plot_aes[[aes_cat]][[aes_name]] <- exArgs[[aes_arg]]
      }
    }
  }
  
  ## if plot.cri is null, if comp=1 plot them otherwise do not (clutter!)
  if (is.null(plot.cri) & isTRUE(he$n_comparisons == 1 | is.null(comparison)))
    plot.cri <- he$n_comparisons == 1
  
  ## calculate credible intervals if necessary
  if (plot.cri)
    cri <- eib.plot.cri(he, alpha, cri.quantile)
  
  ## calculate plot vertical limits
  yl <- ifelse(rep(!isTRUE(plot.cri), 2),
               yes = range(c(he$eib)),
               no = range(c(he$eib), c(cri[, 1:2])))
  
  if (graph_choice == 1) {
    # base graphics version -----
    if (!is.null(size)) {
      if (!is.na(size)) {
        message("Option size will be ignored using base graphics.")
        size <- NULL
      }
    }
    
    if (is.numeric(alt.legend) & length(alt.legend) == 2) {
      temp <- ""
      if (alt.legend[2] == 0)
        temp <- paste0(temp, "bottom")
      else
        temp <- paste0(temp, "top")
      
      if (alt.legend[1] == 1)
        temp <- paste0(temp, "right")
      else
        temp <- paste0(temp, "left")
      
      alt.legend <- temp
      if (length(grep("^(bottom|top)(left|right)$", temp)) == 0)
        alt.legend <- FALSE
    }
    if (is.logical(alt.legend)) {
      if (!alt.legend)
        alt.legend <- "topleft"
      else
        alt.legend <- "topright"
    }
    
    if (he$n_comparisons == 1) {
      plot(
        NULL,
        ylim = yl,
        xlim = range(he$k),
        xlab = switch(
          as.numeric(plot_annotations$exist$xlab) + 1,
          "Willingness to pay",
          plot_annotations$xlab),
        ylab = switch(
          as.numeric(plot_annotations$exist$ylab) + 1,
          "EIB",
          plot_annotations$ylab),
        main = switch(
          as.numeric(plot_annotations$exist$title) + 1, 
          paste0("Expected Incremental Benefit",
                 ifelse(
                   plot.cri,
                   paste0("\nand ", format((1 - alpha)*100, digits = 4),
                          "% credible intervals"),
                   "")),
          plot_annotations$title))
      ## x axis
      abline(h = 0, col = "grey")
      ## EIB
      lines(he$k,
            he$eib,
            col = plot_aes$line$colors[1],
            lty = ifelse(is.null(plot_aes$line$types),
                         yes = 1,
                         no = plot_aes$line$types[1]))
      ## CrI
      if (plot.cri) {
        lines(he$k, cri$low, col = plot_aes$line$cri_colors[1], lty = 2)
        lines(he$k, cri$upp, col = plot_aes$line$cri_colors[1], lty = 2)
      }
      ### BEP
      if (length(he$kstar) > 0 & is.null(size)) {
        abline(v = he$kstar, col = "dark grey", lty = "dotted")
        text(he$kstar, min(yl), paste("k* = ", he$kstar ,sep = ""))
      }
      
      if (he$change_comp) {
        legend(
          alt.legend,
          paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp]),
          cex = 0.7, bty = "n", lwd = 1,
          lty = ifelse(is.null(plot_aes$line$types),
                       yes = 1,
                       no = plot_aes$line$types[1]))
      }
    } else if (he$n_comparisons > 1 & is.null(comparison)) {
      lwd <- ifelse(he$n_comparisons > 6, 1.5, 1)
      plot(
        NULL,
        ylim = yl,
        xlim = range(he$k),
        xlab = switch(
          as.numeric(plot_annotations$exist$xlab) + 1,
          "Willingness to pay",
          plot_annotations$xlab),
        ylab = switch(
          as.numeric(plot_annotations$exist$ylab) + 1,
          "EIB",
          plot_annotations$ylab),
        main = switch(
          as.numeric(plot_annotations$exist$title) + 1, 
          paste0("Expected Incremental Benefit",
                 ifelse(
                   plot.cri,
                   paste0("\nand ", format((1 - alpha)*100, digits = 4),
                          "% credible intervals"), "")),
          plot_annotations$title))
      
      abline(h = 0, col = "grey")
      
      if (is.null(plot_aes$line$types))
        plot_aes$line$types <- 1:he$n_comparisons
      
      for (j in seq_len(he$n_comparisons)) {
        lines(he$k, he$eib[, j],
              lty = plot_aes$line$types[min(j, length(plot_aes$line$types))], 
              lwd = ifelse(plot.cri, lwd + 1, lwd), 
              col = plot_aes$line$colors[min(j, length(plot_aes$line$colors))])
        if (plot.cri) {
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
      }
      if (length(he$kstar) > 0 & is.null(size)) {
        abline(v = he$kstar, col = "dark grey", lty = "dotted")
        text(he$kstar, min(yl), paste("k* = ", he$kstar, sep = ""))
      }
      text <- paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
      legend(
        alt.legend,
        text,
        cex = 0.7,
        bty = "n",
        lty = plot_aes$line$types,
        lwd = ifelse(plot.cri, lwd + 1, lwd))
    } else if (he$n_comparisons > 1 &
               !is.null(comparison)) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta_e <- he$delta_e[, comparison]
      he$delta_c <- he$delta_c[, comparison]
      he$n_comparators <- length(comparison) + 1
      he$n_comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[,,sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$change_comp <- TRUE
      
      eib.plot(
        he,
        pos = alt.legend,
        graph = "base",
        size = size,
        comparison = NULL,
        plot.cri = plot.cri,
        alpha = alpha,
        cri.quantile = cri.quantile,
        ...)
    }
  } else if (graph_choice == 2) {
    
    if (!(requireNamespace("ggplot2", quietly = TRUE) &
          requireNamespace("grid", quietly = TRUE))) {
      message("falling back to base graphics\n")
      eib.plot(he, pos = alt.legend, graph = "base")
      return(invisible(NULL))
    }
    
    ### no visible binding note
    k <- kstar <- low <- upp <- NA_real_
    
    if (is.null(size))
      size <- rel(3.5)
    
    opt.theme <- theme()
    for (obj in exArgs)
      if (is.theme(obj))
        opt.theme <- opt.theme + obj
    
    if (he$n_comparisons == 1) {
      
      data.psa <- data.frame(
        k = he$k,
        eib = c(he$eib), 
        comparison = as.factor(
          rep(1:he$n_comparison, each = length(he$k)))
      )
      
      if (plot.cri) 
        data.psa <- cbind(data.psa, cri)
      
      if (is.null(plot_aes$line$types))
        plot_aes$line$types <- 1:he$n_comparisons
      
      eib <-
        ggplot(data.psa, aes(x = .data$k, y = .data$eib)) +
        theme_bw() +
        geom_hline(aes(yintercept = 0), colour = "grey")
      
      if (!he$change_comp) {
        eib <-
          eib +
          geom_line(
            linetype = plot_aes$line$types[1],
            colour = plot_aes$line$colors[1])
      } else {
        eib <-
          eib + 
          geom_line(linetype = plot_aes$line$types[1],
                    colour = plot_aes$line$colors[1]) +
          scale_linetype_manual(
            "",
            values = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]),
            labels = with(he,paste0(interventions[ref], " vs ", interventions[comp])))
      }
      
      if (!length(he$kstar) == 0 & !is.na(size)) {
        label <- paste0("k* = ", format(he$kstar, digits = 6))
        eib <-
          eib +
          geom_vline(
            aes(xintercept = .data$kstar),
            data = data.frame("kstar" = he$kstar),
            colour = "grey50",
            linetype = 2,
            size = 0.5) +
          annotate(
            "text",
            label = label,
            x = he$kstar,
            y = min(yl),
            hjust = ifelse((max(he$k) - he$kstar) / max(he$k) > 1 / 6,
                           yes = -0.1,
                           no = 1.1),
            size = size)
      }
      if (plot.cri) {
        eib <- eib +
          geom_line(aes(y = .data$low),
                    colour = plot_aes$line$cri_colors[1],
                    lty = 2) +
          geom_line(aes(y = .data$upp),
                    colour = plot_aes$line$cri_colors[1],
                    lty = 2)
      }
    } else if (he$n_comparisons > 1 &
               is.null(comparison)) {
      data.psa <-
        data.frame(
          k = c(he$k),
          eib = c(he$eib),
          comparison = as.factor(
            rep(1:he$n_comparison, each = length(he$k)))
        )
      if (plot.cri)
        data.psa <- cbind(data.psa, cri)
      comparisons.label <-
        paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
      
      # linetype is the indicator of the comparison.
      # 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
      if (is.null(plot_aes$line$types))
        plot_aes$line$types <- rep(c(1,2,3,4,5,6), ceiling(he$n_comparisons/6))[1:he$n_comparisons]
      
      if (length(plot_aes$line$types) < length(comparisons.label))
        plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
      
      if (length(plot_aes$line$colors) < length(comparisons.label))
        plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
      
      eib <- 
        ggplot(
          data.psa,
          aes(x = .data$k, y = .data$eib, linetype = .data$comparison, colour = .data$comparison)) + 
        geom_hline(yintercept = 0, linetype = 1, color = "grey") + 
        theme_bw() +
        geom_line(lwd = ifelse(!plot.cri, 0.5, 0.75)) +
        scale_colour_manual(
          "",
          labels = comparisons.label, 
          values = plot_aes$line$colors) +
        scale_linetype_manual(
          "",
          labels = comparisons.label, 
          values = plot_aes$line$types)
      
      if (!length(he$kstar) == 0 & !is.na(size)) {
        label <- paste0("k* = ", format(he$kstar, digits = 6))
        eib <-
          eib +
          geom_vline(
            aes(xintercept = .data$kstar),
            data = data.frame("kstar" = he$kstar),
            colour = "grey50",
            linetype = 2,
            size = 0.5) +
          annotate(
            "text",
            label = label,
            x = he$kstar,
            y = min(yl),
            hjust = ifelse((max(he$k) - he$kstar) / max(he$k) > 1 / 6, -0.1, 1.1),
            size = size,
            vjust = 1)
      }
      
      if (plot.cri) {
        eib <-
          eib +
          geom_line(aes(y = .data$low),
                    colour = plot_aes$line$cri_colors,
                    show.legend = FALSE) +
          geom_line(aes(y = .data$upp),
                    colour = plot_aes$line$cri_colors,
                    show.legend = FALSE)
      }
    } else if (he$n_comparisons > 1 & !is.null(comparison)) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta_e <- he$delta_e[, comparison]
      he$delta_c <- he$delta_c[, comparison]
      he$n_comparators <- length(comparison)+1
      he$n_comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$change_comp <- TRUE
      
      return(
        eib.plot(
          he,
          pos = alt.legend,
          graph = "ggplot2",
          size = size,
          comparison = NULL,
          plot.cri = plot.cri,
          alpha = alpha,
          cri.quantile = cri.quantile,
          ...))
    }
    
    eib <- eib + 
      ggplot2::labs(
        x = switch(
          as.numeric(plot_annotations$exist$xlab) + 1,
          "Willingness to pay",
          plot_annotations$xlab),
        y = switch(
          as.numeric(plot_annotations$exist$ylab) + 1,
          "EIB",
          plot_annotations$ylab),
        title = switch(
          as.numeric(plot_annotations$exist$title) + 1, 
          paste0("Expected Incremental Benefit", ifelse(
            plot.cri,
            paste0("\nand ", format((1 - alpha)*100, digits = 4),
                   "% credible intervals"), "")),
          plot_annotations$title))
    
    jus <- NULL
    if (length(alt.legend) == 1 && alt.legend)  {
      alt.legend <- "bottom"
      eib <- eib + theme(legend.direction = "vertical")
    } else {
      if (is.character(alt.legend)) {
        choices <- c("left", "right", "bottom", "top")
        alt.legend <- choices[pmatch(alt.legend, choices)]
        jus <- "center"
        if (is.na(alt.legend))
          alt.legend <- FALSE
      }
      if (length(alt.legend) > 1)
        jus <- alt.legend
      if (length(alt.legend) == 1 & !is.character(alt.legend)) {
        alt.legend <- c(1, 0)
        jus <- alt.legend
      }
    }
    eib <-
      eib + 
      theme(
        legend.position = alt.legend,
        legend.justification = jus,
        legend.title = element_blank(),
        legend.background = element_blank(),
        text = element_text(size = 11),
        legend.key.size = grid::unit(0.66, "lines"),
        legend.spacing = grid::unit(-1.25, "line"),
        panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.text.align = 0,
        plot.title = element_text(
          lineheight = 1.05,
          face = "bold",
          size = 14.3,
          hjust = 0.5)) + opt.theme
    
    return(eib)
  } else if (graph_choice == 3) {
    # plotly version -----
    if (!is.null(size) && !is.na(size)) {
      message("Option size will be ignored using plotly.")
      size <- NULL
    }
    
    if (he$n_comparisons > 1 & !is.null(comparison)) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta_e <- he$delta_e[, comparison]
      he$delta_c <- he$delta_c[, comparison]
      he$n_comparators <- length(comparison) + 1
      he$n_comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$change_comp <- TRUE
      
      return(
        eib.plot(
          he,
          pos = alt.legend,
          graph = "plotly",
          size = size,
          comparison = NULL,
          plot.cri = plot.cri,
          alpha = alpha,
          cri.quantile = cri.quantile,
          ...))
    }
    
    if (is.null(plot_aes$line$types))
      plot_aes$line$types <- rep(c(1:6), ceiling(he$n_comparisons/6))[1:he$n_comparisons]
    comparisons.label <- with(he,paste0(interventions[ref], " vs ", interventions[comp]))
    
    if (length(plot_aes$line$types) < length(comparisons.label))
      plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
    
    if (length(plot_aes$line$colors) < length(comparisons.label))
      plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
    # opacities
    plot_aes$line$cri_colors <-
      vapply(plot_aes$line$cri_colors,
             function(x) 
               ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
    plot_aes$area$color <- vapply(plot_aes$area$color, function(x)
      ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
    
    data.psa <-
      data.frame(
        k = he$k,
        eib = c(he$eib),
        comparison = as.factor(c(
          vapply(1:he$n_comparisons, function(x) rep(x, length(he$k)))
        )),
        label = as.factor(c(
          vapply(comparisons.label, function(x) rep(x, length(he$k)))
        )))
    if (plot.cri)
      data.psa <- cbind(data.psa, cri)
    eib <- plotly::plot_ly(data.psa, x = ~k)
    eib <-
      plotly::add_trace(
        eib,
        y = ~eib,
        type = "scatter",
        mode = "lines",
        fill = ifelse(plot_aes$area$include, "tozeroy", "none"),
        name = ~label,
        fillcolor = plot_aes$area$color,
        color = ~comparison,
        colors = plot_aes$line$colors,
        linetype = ~comparison,
        linetypes = plot_aes$line$types,
        legendgroup = ~comparison)
    # NB: decision change points not included - hover functionality is sufficient
    if (plot.cri) {
      if (he$n_comparisons == 1) {
        eib <- plotly::add_ribbons(
          eib,
          name = paste0(100 * (1 - alpha), "% CrI"),
          ymin = ~low,
          ymax = ~upp,
          color = NA,
          fillcolor = ~plot_aes$line$cri_colors[comparison])
      } else {
        eib <- plotly::add_ribbons(
          eib,
          name = ~label,
          ymin = ~low,
          ymax = ~upp,
          line = list(color = plot_aes$line$cri_colors[1]),
          # for transparency, use plotly::toRGB("blue", alpha = 0.5)
          legendgroup = ~comparison,
          fillcolor = "rgba(1, 1, 1, 0)",
          linetype = ~comparison,
          linetypes = plot_aes$line$types,
          showlegend = FALSE)
      }
    }
    
    # legend positioning not great - must be customized case by case
    legend_list <- list(orientation = "h", xanchor = "center", x = 0.5)
    if (is.character(alt.legend))
      legend_list = switch(
        alt.legend,
        "left" = list(orientation = "v", x = 0, y = 0.5),
        "right" = list(orientation = "v", x = 0, y = 0.5),
        "bottom" = list(orienation = "h", x = 0.5, y = 0, xanchor = "center"),
        "top" = list(orientation = "h", x = 0.5, y = 100, xanchor = "center"))
    
    eib <-
      plotly::layout(
        eib,
        title = switch(
          as.numeric(plot_annotations$exist$title) + 1, 
          paste0("Expected Incremental Benefit", ifelse(
            plot.cri,
            paste0("\nand ", format((1 - alpha)*100, digits = 4), "% credible intervals"),
            "")),
          plot_annotations$title),
        xaxis = list(
          hoverformat = ".2f",
          title = switch(
            as.numeric(plot_annotations$exist$xlab) + 1,
            "Willingness to pay",
            plot_annotations$xlab)),
        yaxis = list(
          hoverformat = ".2f",
          title = switch(
            as.numeric(plot_annotations$exist$xlab) + 1,
            "EIB",
            plot_annotations$ylab)),
        showlegend = TRUE, 
        legend = legend_list)
    
    eib <- plotly::config(eib, displayModeBar = FALSE)
    return(eib)
  }
}


#' Expected Incremental Benefit (EIB) Plot
#' 
#' Produces a plot of the Expected Incremental Benefit (EIB) as a function of
#' the willingness to pay.
#' 
#' @template args-he
#' @template args-comparison
#' @param pos Parameter to set the position of the legend; for a single
#' comparison plot, the ICER legend position. Can be given in form of a string
#' \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with \code{FALSE}
#' indicating to use the default position and \code{TRUE} to place it on the
#' bottom of the plot. Default value is \code{c(1,0)}, that is the bottomright
#' corner inside the plot area.
#' @param size Value (in millimetres) of the size of the willingness to pay
#' label. Used only if \code{graph="ggplot2"}, otherwise it will be ignored
#' with a message. If set to \code{NA}, the break-even point line(s) and
#' label(s) are suppressed, with both base graphics and ggplot2.
#' @param plot.cri Logical value. Should the credible intervals be plotted
#' along with the expected incremental benefit? Default as \code{NULL} draws
#' the 95\% credible intervals if only one comparison is selected, and does not
#' include them for multiple comparisons.  Setting \code{plot.cri=TRUE} or
#' \code{plot.cri=FALSE} forces the function to add the intervals or not. The
#' level of the intervals can be also set, see \ldots{} for more details.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the three options \code{"base"},
#' \code{"ggplot2"} or \code{"plotly"}. Default value is \code{"base"}.
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional arguments:
#'  \itemize{
#'   \item \code{alpha} can be used to set the CrI level when \code{plot.cri=TRUE},
#'   with a default value of \code{alpha=0.05}.
#'   \item \code{cri.quantile} controls the the method of calculation of the credible
#'   intervals. The default value \code{cri.quantile=TRUE} defines the CrI as the
#'   interval between the \code{alpha/2}-th and \code{1-alpha/2}-th quantiles of
#'   the IB distribution. Setting \code{cri.quantile=FALSE} will use a normal
#'   approximation on the IB distribution to calculate the intervals.
#'   \item \code{line_colors}: specifies the line colour(s) - all graph types.
#'   \item \code{line_types}: specifies the line type(s) as lty numeric values - all graph types.
#'   \item \code{area_include}: include area under the EIB curve - plotly only.
#'   \item \code{area_color}: specifies the AUC curve - plotly only.}
#'   
#' @return \item{eib}{ If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#' a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#' the default.} The function produces a plot of the
#' Expected Incremental Benefit as a function of the discrete grid
#' approximation of the willingness to pay parameter. The break even point
#' (i.e. the point in which the EIB = 0, i.e. when the optimal decision changes
#' from one intervention to another) is also showed by default. The value `k*` is
#' the discrete grid approximation of the ICER.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}},
#'          \code{\link{ib.plot}},
#'          \code{\link{ceplane.plot}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' @keywords "Health economic evaluation" "Expected Incremental Benefit"
#' @import ggplot2
#' @importFrom grid unit
#' @export
#' 
#' @examples
#' data(Vaccine)
#'  
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(
#'       e=e,
#'       c=c,                  # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0, Kmax)
#'       plot=TRUE             # plots the results
#' )
#' eib.plot(m)
#' eib.plot(m, graph = "ggplot2") + ggplot2::theme_linedraw()
#' 
eib.plot <- function(he, ...) {
  UseMethod('eib.plot', he)
}

