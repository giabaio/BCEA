
#' @rdname contour
#' @importFrom stats sd
#' @importFrom graphics par
#' 
#' @export
#' 
contour.bcea <- function(he,
                         comparison = 1,
                         scale = 0.5,
                         nlevels = 4,
                         levels = NULL,
                         pos = c(1, 0),
                         xlim = NULL,
                         ylim = NULL,
                         graph = c("base", "ggplot2"),
                         ...) {
  
  # comparison selects which plot should be made
  # by default it is the first possible
  
  # Additional/optional arguments
  extra_args <- list(...)
  if (!exists("xlab", where = extra_args)) {
    xlab <- "Effectiveness differential"
  } else {
    xlab <- extra_args$xlab
  }
  
  if (!exists("ylab", where = extra_args)) {
    ylab <- "Cost differential"
  } else {
    ylab <- extra_args$ylab
  }
  
  if (!exists("title", where = extra_args)) {
    title <-
      paste(
        "Cost effectiveness plane contour plot\n",
        he$interventions[he$ref],
        " vs ",
        he$interventions[he$comp],
        sep = "")
  } else {
    title <- extra_args$title
  }
  
  alt.legend <- pos
  base.graphics <- all(pmatch(graph, c("base", "ggplot2")) != 2)
  
  if (base.graphics) {
    if (is.null(comparison) | length(comparison) > 1) {
      message(
        "The first available comparison will be selected.
        To plot multiple comparisons together please use the ggplot2 version.
        Please see ?contour.bcea for additional details.")
      comparison <- 1
    }
    
    if (he$n_comparisons == 1) {
      density <- MASS::kde2d(as.matrix(he$delta_e),
                             as.matrix(he$delta_c),
                             n = 300,
                             h = c(sd(as.matrix(he$delta_e)) / scale,
                                   sd(as.matrix(he$delta_c)) / scale))
      offset <- 1.0
      
      p.ne <- sum(he$delta_e > 0 & he$delta_c > 0) / he$n_sim
      p.nw <- sum(he$delta_e <= 0 & he$delta_c > 0) / he$n_sim
      p.sw <- sum(he$delta_e <= 0 & he$delta_c <= 0) / he$n_sim
      p.se <- sum(he$delta_e > 0 & he$delta_c <= 0) / he$n_sim
      
      m.c <- range(he$delta_c)[1]
      M.c <- range(he$delta_c)[2]
      m.e <- range(he$delta_e)[1]
      M.e <- range(he$delta_e)[2]
      
      ##TODO: this isnt how to use ifelse(); refactor
      
      # Changes the range so that the plot always shows the x and y axes
      ch1 <- ifelse(m.e > 0,
                    m.e <- -m.e,
                    m.e <- m.e)
      ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
      ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
      ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
      
      # If the user has specified the range of the graph, use those values
      if (!is.null(xlim)) {
        m.e <- xlim[1]
        M.e <- xlim[2]
      }
      if (!is.null(ylim)) {
        m.c <- ylim[1]
        M.c <- ylim[2]
      }
      
      plot(
        as.matrix(he$delta_e),
        as.matrix(he$delta_c),
        pch = 20,
        cex = 0.3,
        col = "dark grey",
        xlab = xlab,
        ylab = ylab,
        main = title,
        xlim = c(m.e, M.e),
        ylim = c(m.c, M.c))
      abline(h = 0, v = 0, col = "dark grey")
      
      if (!any(is.na(density$z))) {
        if (!is.null(levels)) {
          # normalise the density and use levels in the contour
          density$z <-
            (density$z - min(density$z)) / (max(density$z) - min(density$z))
          
          graphics::contour(
            density$x,
            density$y,
            density$z,
            add = TRUE,
            levels = levels,
            drawlabels = TRUE)
        }
        if (is.null(levels)) {
          graphics::contour(
            density$x,
            density$y,
            density$z,
            add = TRUE,
            nlevels = nlevels,
            drawlabels = FALSE)
        }
      }
      
      t1 <-
        paste("Pr(Delta[e]>0, Delta[c]>0)==",
              format(p.ne, digits = 4, nsmall = 3),
              sep = "")
      
      text(offset * M.e,
           offset * M.c,
           parse(text = t1),
           cex = 0.8,
           pos = 2)
      
      t2 <-
        paste("Pr(Delta[e]<=0, Delta[c]>0)==",
              format(p.nw, digits = 4, nsmall = 3),
              sep = "")
      
      text(offset * m.e,
           offset * M.c,
           parse(text = t2),
           cex = 0.8,
           pos = 4)
      
      t3 <-
        paste("Pr(Delta[e]<=0, Delta[c]<=0)==",
              format(p.sw, digits = 4, nsmall = 3),
              sep = "")
      
      text(offset * m.e,
           offset * m.c,
           parse(text = t3),
           cex = 0.8,
           pos = 4)
      
      t4 <-
        paste("Pr(Delta[e]>0, Delta[c]<=0)==",
              format(p.se, digits = 4, nsmall = 3),
              sep = "")
      
      text(offset * M.e,
           offset * m.c,
           parse(text = t4),
           cex = 0.8,
           pos = 2)
    }
    
    if (he$n_comparisons > 1) {
      if (!exists("title", where = extra_args)) {
        title <-
          paste(
            "Cost effectiveness plane contour plot \n",
            he$interventions[he$ref],
            " vs ",
            he$interventions[he$comp[comparison]],
            sep = "")
      }
      else {
        title <- extra_args$title
      }
      
      density <-
        MASS::kde2d(as.matrix(he$delta_e[, comparison]),
                    as.matrix(he$delta_c[, comparison]),
                    n = 300,
                    h = c(sd(as.matrix(he$delta_e[, comparison])) / scale,
                          sd(as.matrix(he$delta_c[, comparison])) / scale))
      offset <- 1.0
      
      p.ne <-
        sum(he$delta_e[, comparison] > 0 &
              he$delta_c[, comparison] > 0) / he$n_sim
      p.nw <-
        sum(he$delta_e[, comparison] <= 0 &
              he$delta_c[, comparison] > 0) / he$n_sim
      p.sw <-
        sum(he$delta_e[, comparison] <= 0 &
              he$delta_c[, comparison] <= 0) / he$n_sim
      p.se <-
        sum(he$delta_e[, comparison] > 0 &
              he$delta_c[, comparison] <= 0) / he$n_sim
      
      m.c <- range(he$delta_c[, comparison])[1]
      M.c <- range(he$delta_c[, comparison])[2]
      m.e <- range(he$delta_e[, comparison])[1]
      M.e <- range(he$delta_e[, comparison])[2]
      
      # Changes the range so that the plot always shows the x and y axes
      ch1 <- ifelse(m.e > 0, m.e <- -m.e, m.e <- m.e)
      ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
      ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
      ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
      
      # If the user has specified the range of the graph, use those values
      if (!is.null(xlim)) {
        m.e <- xlim[1]
        M.e <- xlim[2]
      }
      if (!is.null(ylim)) {
        m.c <- ylim[1]
        M.c <- ylim[2]
      }
      
      plot(
        as.matrix(he$delta_e[, comparison]),
        as.matrix(he$delta_c[, comparison]),
        pch = 20,
        cex = .3,
        col = "dark grey",
        xlab = xlab,
        ylab = ylab,
        main = title,
        xlim = c(m.e, M.e),
        ylim = c(m.c, M.c)
      )
      abline(h = 0, v = 0, col = "dark grey")
      
      if (!any(is.na(density$z))) {
        graphics::contour(
          density$x,
          density$y,
          density$z,
          add = TRUE,
          drawlabels = TRUE)
        if (!is.null(levels)) {
          # Normalise the density and use levels in the contour
          density$z <-
            (density$z - min(density$z)) / (max(density$z) - min(density$z))
          graphics::contour(
            density$x,
            density$y,
            density$z,
            add = TRUE,
            levels = levels,
            drawlabels = TRUE
          )
        }
        if (is.null(levels)) {
          graphics::contour(
            density$x,
            density$y,
            density$z,
            add = TRUE,
            nlevels = nlevels,
            drawlabels = FALSE
          )
        }
      }
      t1 <-
        paste("Pr(Delta[e]>0, Delta[c]>0)==",
              format(p.ne, digits = 4, nsmall = 3),
              sep = "")
      text(offset * M.e,
           offset * M.c,
           parse(text = t1),
           cex = 0.8,
           pos = 2)
      t2 <-
        paste("Pr(Delta[e]<=0, Delta[c]>0)==",
              format(p.nw, digits = 4, nsmall = 3),
              sep = "")
      text(offset * m.e,
           offset * M.c,
           parse(text = t2),
           cex = 0.8,
           pos = 4)
      t3 <-
        paste("Pr(Delta[e]<=0, Delta[c]<=0)==",
              format(p.sw, digits = 4, nsmall = 3),
              sep = "")
      text(offset * m.e,
           offset * m.c,
           parse(text = t3),
           cex = 0.8,
           pos = 4)
      t4 <-
        paste("Pr(Delta[e]>0, Delta[c]<=0)==",
              format(p.se, digits = 4, nsmall = 3),
              sep = "")
      text(offset * M.e,
           offset * m.c,
           parse(text = t4),
           cex = 0.8,
           pos = 2)
    }
  } # if base.graphics
  else{
    if (!requireNamespace("ggplot2", quietly = TRUE) &
        requireNamespace("grid", quietly = TRUE)) {
      message("Falling back to base graphics\n")
      contour(
        he,
        comparison = comparison,
        scale = scale,
        nlevels = nlevels,
        pos = alt.legend,
        levels = levels,
        graph = "base",
        ...)
      return(invisible(NULL))
    }
    
    if (!is.null(levels))
      message("Option level will be ignored using ggplot2 graphics")
    
    # no visible binding note
    delta_e <- delta_c <- e <- z <- y <- hjust <- label <- NULL
    
    if (!is.null(nlevels)) {
      nlevels <- round(nlevels)
      if (nlevels < 0)
        nlevels <- 10
      if (nlevels == 0)
        nlevels <- 1
    }
    
    if (he$n_comparisons == 1) {
      kd <- data.frame(e = unlist(he$delta_e),
                       c = unlist(he$delta_c))
      
      # for scale_x_continuous(oob=)
      do.nothing <- function(x, limits)
        return(x)
      
      # plot limits
      range.e <- range(kd$e)
      range.c <- range(kd$c)
      range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
      range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
      
      # labels
      p.ne <- sum(he$delta_e > 0 & he$delta_c > 0) / he$n_sim
      p.ne <-
        paste0("Pr(Delta[e]>0, Delta[c]>0)==",
               format(p.ne, digits = 4, nsmall = 3))
      p.nw <- sum(he$delta_e <= 0 & he$delta_c > 0) / he$n_sim
      p.nw <-
        paste0("Pr(Delta[e]<=0, Delta[c]>0)==",
               format(p.nw, digits = 4, nsmall = 3))
      p.sw <- sum(he$delta_e <= 0 & he$delta_c <= 0) / he$n_sim
      p.sw <-
        paste0("Pr(Delta[e]<=0, Delta[c]<=0)==",
               format(p.sw, digits = 4, nsmall = 3))
      p.se <- sum(he$delta_e > 0 & he$delta_c <= 0) / he$n_sim
      p.se <-
        paste0("Pr(Delta[e]>0, Delta[c]<=0)==",
               format(p.se, digits = 4, nsmall = 3))
      
      # labels dataframe
      labels.df <- data.frame(
        he = c(range.e[2], range.e[1], range.e[1], range.e[2]),
        y = c(rep(range.c[2], 2), rep(range.c[1], 2)),
        label = c(p.ne, p.nw, p.sw, p.se),
        hjust = as.factor(c(1, 0, 0, 1))
      )
      
      # actual plot
      points.colour <- "grey"
      if (nlevels == 1)
        points.colour <- "black"
      
      ceplane <-
        ggplot(kd, aes(.data$e, .data$c)) +
        geom_hline(aes(yintercept = 0), colour = "grey") +
        geom_vline(aes(xintercept = 0), colour = "grey") +
        theme_bw() +
        geom_point(size = 1, color = points.colour) +
        scale_x_continuous(limits =
                             range.e, oob = do.nothing) +
        scale_y_continuous(limits = range.c, oob = do.nothing)
      
      if (!is.null(scale) & requireNamespace("MASS", quietly = TRUE)) {
        density <-
          MASS::kde2d(as.matrix(he$delta_e),
                      as.matrix(he$delta_c),
                      n = 300,
                      h = c(sd(as.matrix(he$delta_e)) / scale,
                            sd(as.matrix(he$delta_c)) / scale)
          )
        
        density <-
          data.frame(expand.grid(e = density$x,
                                 c = density$y),
                     z = as.vector(density$z))
        ceplane <-
          ceplane +
          geom_contour(
            aes(z = .data$z),
            data = density,
            colour = "black",
            bins = nlevels)
      }
      else{
        ceplane <- ceplane + stat_density2d(color = "black")
      }
      
      ceplane <- ceplane +
        geom_text(
          data = labels.df,
          aes(
            x = .data$he,
            y = .data$y,
            hjust = hjust,
            label = label),
          parse = TRUE,
          size = rel(3.5))
    }
    if (he$n_comparisons > 1 & is.null(comparison)) {
      kd <- data.frame(
        delta_e = c(as.matrix(he$delta_e)),
        delta_c = c(as.matrix(he$delta_c)),
        comparison =
          as.factor(sort(
            rep(1:he$n_comparisons, dim(as.matrix(he$delta_e))[1])))
      )
      
      # vector of values for colour, take out white, get integer values
      colors.label <-
        paste0("gray",
               round(seq(0, 100,
                         length.out = (he$n_comparisons + 1))[-(he$n_comparisons + 1)]))
      comparisons.label <-
        paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
      
      do.nothing <- function(x, limits) x

      # plot limits
      range.e <- range(kd$delta_e)
      range.c <- range(kd$delta_c)
      range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
      range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
      
      ceplane <-
        ggplot(kd,
               aes(x = .data$delta_e, y = .data$delta_c, col = .data$comparison)) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_vline(xintercept = 0, colour = "grey") +
        theme_bw() +
        geom_point(size = 1) +
        scale_color_manual(label = comparisons.label,
                           values = colors.label,
                           na.value = "black") +
        scale_x_continuous(limits = range.e, oob = do.nothing) +
        scale_y_continuous(limits = range.c, oob = do.nothing)
      
      if (!is.null(scale) & requireNamespace("MASS", quietly = TRUE)) {
        densitydf <- data.frame()
        for (i in 1:he$n.comparison) {
          temp <-
            MASS::kde2d(
              as.matrix(he$delta_e)[, i],
              as.matrix(he$delta_c)[, i],
              n = 300,
              h = c(sd(as.matrix(he$delta_e)[, i]) / scale, sd(as.matrix(he$delta_c)[, i]) / scale))
          temp <-
            data.frame(expand.grid(e = temp$x,
                                   c = temp$y),
                       z = as.vector(temp$z))
          densitydf <-
            rbind(densitydf, cbind(temp, rep(i, dim(temp)[[1]])))
        }
        names(densitydf) <- c("delta_e", "delta_c", "z", "comparison")
        densitydf$comparison <- as.factor(densitydf$comparison)
        ceplane <-
          ceplane +
          geom_contour(aes(z = .data$z, colour = .data$comparison),
                       data = densitydf,
                       bins = nlevels) +
          guides(colour = guide_legend(override.aes =
                                         list(linetype = 0)))
      }
      else{
        ceplane <-
          ceplane +
          stat_density2d() +
          guides(colour = guide_legend(override.aes =
                                         list(linetype = 0)))
      }
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
      he$mod <- TRUE
      
      return(
        contour(
          he,
          scale = scale,
          pos = alt.legend,
          nlevels = nlevels,
          graph = "ggplot2",
          comparison = NULL
        )
      )
    }
    
    if (!exists("title", where = extra_args)) {
      labs.title <- "Cost-Effectiveness Plane"
      labs.title <-
        paste0(labs.title,
               ifelse(
                 he$n_comparisons == 1,
                 paste0("\n", he$interventions[he$ref], " vs ", he$interventions[-he$ref]),
                 paste0(ifelse(
                   isTRUE(he$mod),
                   paste0(
                     "\n",
                     he$interventions[he$ref],
                     " vs ",
                     paste0(he$interventions[he$comp], collapse = ", ")
                   ),
                   ""))
               ))
    } else {
      labs.title <- extra_args$title
    }
    
    ceplane <-
      ceplane + labs(title = labs.title, x = xlab, y = ylab)
    
    jus <- NULL
    if (all(alt.legend)) {
      alt.legend <- "bottom"
      ceplane <-
        ceplane + theme(legend.direction = "vertical")
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
    
    ceplane <-
      ceplane +
      theme(
        legend.position = alt.legend,
        legend.justification = jus,
        legend.title = element_blank(),
        legend.background = element_blank(),
        text = element_text(size = 11),
        legend.key.size = grid::unit(.66, "lines"),
        legend.spacing = grid::unit(-1.25, "line"),
        panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.text.align = 0,
        plot.title = element_text(
          lineheight = 1.05,
          face = "bold",
          size = 14.3,
          hjust = 0.5)
      )
    return(ceplane)
  }
}


#' Contour Plots for the Cost-Effectiveness Plane
#'
#' Contour method for objects in the class \code{bcea}.
#' Produces a scatterplot of the cost-effectiveness plane, with a contour-plot
#' of the bivariate density of the differentials of cost (y-axis) and
#' effectiveness (x-axis).
#'
#' @template args-he
#' @param comparison In case of more than 2 interventions being analysed,
#' selects which plot should be made. By default the first comparison among
#' the possible ones will be plotted. If \code{graph="ggplot2"} any subset of
#' the possible comparisons can be selected, and \code{comparison=NULL} will
#' yield a plot of all the possible comparisons together.
#' @param scale Scales the plot as a function of the observed standard
#' deviation.
#' @param levels Numeric vector of levels at which to draw contour lines. Will
#' be ignored using \code{graph="ggplot2"}.
#' @param nlevels Number of levels to be plotted in the contour.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom}, \code{top}, \code{left} or \code{right} for ggplot2. It can
#' be a two-elements vector, which specifies the relative position on the x and
#' y axis respectively, or alternatively it can be in form of a logical
#' variable, with \code{FALSE} indicating to use the default position and
#' \code{TRUE} to place the legend on the bottom of the plot. Default value is
#' \code{c(1,0)}, that is the bottomright corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param xlim The range of the plot along the x-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta_e}
#' @param ylim The range of the plot along the y-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta_c}
#' @param ...  Additional arguments to 'plot.window', 'title', 'Axis' and
#' 'box', typically graphical parameters such as 'cex.axis'. Will be ignored if
#' \code{graph="ggplot2"}.
#' 
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if \code{graph="ggplot2"}. } Plots the cost-effectiveness plane with a
#' scatterplot of all the simulated values from the (posterior) bivariate
#' distribution of (Delta_e, Delta_c), the differentials of effectiveness and
#' costs; superimposes a contour of the distribution and prints the estimated
#' value of the probability of each quadrant (combination of positive/negative
#' values for both Delta_e and Delta_c)
#' @author Gianluca Baio, Andrea Berardi
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#'
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' @seealso \code{\link{bcea}},
#'          \code{\link{ceplane.plot}},
#'          \code{\link{contour2}}
#' @keywords "Health economic evaluation" "Bayesian model"
#' 
#' @import ggplot2
#' @importFrom MASS kde2d
#' @importFrom grid unit
#' 
#' @examples
#' data(Vaccine)
#'
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=e,
#'           c=c,              # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e,c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0,Kmax)
#'       plot=TRUE             # plots the results
#' )
#' 
#' contour(m)
#' contour(m, graph = "ggplot2")
#' 
#' # Plots the contour and scatterplot of the bivariate 
#' # distribution of (Delta_e, Delta_c)
#' contour(m,          # uses the results of the economic evaluation 
#'                     #  (a "bcea" object)
#'       comparison=1, # if more than 2 interventions, selects the 
#'                     #  pairwise comparison 
#'       nlevels=4,    # selects the number of levels to be 
#'                     #  plotted (default=4)
#'       levels=NULL,  # specifies the actual levels to be plotted 
#'                     #  (default=NULL, so that R will decide)
#'       scale=0.5,    # scales the bandwidths for both x- and 
#'                     #  y-axis (default=0.5)
#'       graph="base"  # uses base graphics to produce the plot
#' )
#' 
#' @rdname contour
#' @export
#' 
contour <- function(he, ...) {
  UseMethod('contour', he)
}

