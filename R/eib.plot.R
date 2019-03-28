# eib.plot -----

#' Expected Incremental Benefit (EIB) plot
#' 
#' Produces a plot of the Expected Incremental Benefit (EIB) as a function of
#' the willingness to pay
#' 
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param comparison Selects the comparator, in case of more than two
#' interventions being analysed.  Default as \code{NULL} plots all the
#' comparisons together. Any subset of the possible comparisons can be selected
#' (e.g., \code{comparison=c(1,3)} or \code{comparison=2}).
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
#' @return \item{eib}{ If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#' a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#' the default.} The function produces a plot of the
#' Expected Incremental Benefit as a function of the discrete grid
#' approximation of the willingness to pay parameter. The break even point
#' (i.e. the point in which the EIB=0, ie when the optimal decision changes
#' from one intervention to another) is also showed by default. The value k* is
#' the discrete grid approximation of the ICER.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{ib.plot}},
#' \code{\link{ceplane.plot}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Expected Incremental Benefit
#' @export eib.plot
eib.plot <- function(he,comparison=NULL,pos=c(1,0),size=NULL,plot.cri=NULL,graph=c("base","ggplot2","plotly"),...) {
  
  options(scipen=10)
  alt.legend <- pos
  # choose graphical engine
  if (is.null(graph) || is.na(graph)) graph = "base"
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  
  if (graph_choice == 2 && !requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)) {
    warning("Package ggplot2 and grid not found; eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  if (graph_choice == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found; eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  
  ### evaluate arguments. possibility to include different values of confidence as "alpha"
  exArgs <- list(...)
  alpha <- 0.05
  plot_annotations <- list("exist" = list("title" = FALSE, "xlab" = FALSE, "ylab" = FALSE))
  plot_aes <- list("area" = list("include" = FALSE, "color" = "grey"),
                   "line" = list("colors" = "black", "types" = NULL, "cri_colors" = "grey50"))
  plot_aes_args = c("area_include", "area_color", "line_colors", "line_types", "line_cri_colors")
  cri.quantile <- TRUE
  if (length(exArgs) >= 1) {
    if (exists("cri.quantile", where = exArgs))
      cri.quantile <- exArgs$cri.quantile
    if (exists("alpha", where = exArgs)) {
      alpha <- exArgs$alpha
      if (alpha < 0 | alpha > 1) {
        warning("Argument alpha must be between 0 and 1. Reset to default value 0.95")
        alpha <- 0.05
      }
      if (alpha > 0.80 & cri.quantile) {
        warning("It is recommended adopting the normal approximation of the credible interval for high values of alpha. Please set the argument cri.quantile=FALSE to use the normal approsimation.")
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
  
  ### function to calculate the credible intervals
  eib.plot.cri <- function(he,alpha,cri.quantile) {
    if (alpha < 0 | alpha > 1) {
      warning("Argument alpha must be between 0 and 1. Reset to default at 0.95")
      alpha <- 0.05
    }
    margin <- 1
    if (he$n.comparison > 1) margin <- c(1,3)
    cri <- data.frame(
      "low" = c(apply(he$ib,margin,
                    function(x) ifelse(cri.quantile, quantile(x,(alpha)/2),
                                       mean(x) - qnorm((alpha)/2)*sd(x)))),
      "upp" = c(apply(he$ib,margin,function(x) ifelse(cri.quantile, quantile(x,1 - (alpha)/2),
                                                      mean(x) - qnorm(1 - (alpha)/2)*sd(x)))),
      "comp" = as.factor(c(
        sapply(1:he$n.comparisons, function(x) rep(x, length(he$k)))
      )))
    return(cri)
  }
  ### if plot.cri is null, if comp=1 plot them otherwise do not (clutter!)
  if(is.null(plot.cri) & isTRUE(he$n.comparisons==1 | is.null(comparison)))
    plot.cri <- ifelse(he$n.comparisons==1,TRUE,FALSE)
  
  ### calculate credible intervals if necessary
  if(isTRUE(plot.cri))
    cri <- eib.plot.cri(he,alpha,cri.quantile)
  
  ### calculate plot vertical limits
  yl <- ifelse(rep(!isTRUE(plot.cri),2),
               range(c(he$eib)),
               range(c(he$eib),c(cri[,1:2])))
  
  if(graph_choice == 1) {
    # base graphics version -----
    if(!is.null(size)){
      if(!is.na(size)){
        message("Option size will be ignored using base graphics.")
        size <- NULL
      }
    }
    
    if(is.numeric(alt.legend)&length(alt.legend)==2){
      temp <- ""
      if(alt.legend[2]==0)
        temp <- paste0(temp,"bottom")
      else
        temp <- paste0(temp,"top")
      if(alt.legend[1]==1)
        temp <- paste0(temp,"right")
      else
        temp <- paste0(temp,"left")
      alt.legend <- temp
      if(length(grep("^(bottom|top)(left|right)$",temp))==0)
        alt.legend <- FALSE
    }
    if(is.logical(alt.legend)){
      if(!alt.legend)
        alt.legend="topleft"
      else
        alt.legend="topright"
    }
    
    if(he$n.comparisons==1) {
      plot(
        NULL,
        ylim = yl, xlim = range(he$k),
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
          paste0("Expected Incremental Benefit", ifelse(
            plot.cri,
            paste0("\nand ", format((1 - alpha)*100, digits = 4), "% credible intervals"),
            "")),
          plot_annotations$title))
      ### x axis
      abline(h = 0, col = "grey")
      ### EIB
      lines(he$k, he$eib, col = plot_aes$line$colors[1],
            lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]))
      ### CRI
      if (plot.cri) {
        lines(he$k, cri$low, col = plot_aes$line$cri_colors[1], lty = 2)
        lines(he$k, cri$upp, col = plot_aes$line$cri_colors[1], lty = 2)
      }
      ### BEP
      if (length(he$kstar) > 0 & is.null(size)) {
        abline(v = he$kstar, col = "dark grey", lty = "dotted")
        text(he$kstar, min(yl), paste("k* = ", he$kstar ,sep = ""))
      }
      if(isTRUE(he$mod))
        legend(
          alt.legend,
          paste0(he$interventions[he$ref]," vs ",he$interventions[he$comp]),
          cex = .7, bty = "n", lwd = 1,
          lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]))
    } else if (he$n.comparisons > 1 & is.null(comparison)) {
      lwd <- ifelse(he$n.comparisons > 6, 1.5, 1)
      plot(
        NULL, ylim = yl, xlim = range(he$k),
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
          paste0("Expected Incremental Benefit", ifelse(
            plot.cri,
            paste0("\nand ", format((1 - alpha)*100, digits = 4), "% credible intervals"),
            "")),
          plot_annotations$title))
      abline(h = 0, col = "grey")
      if (is.null(plot_aes$line$types)) plot_aes$line$types = 1:he$n.comparisons
      for (j in 1:he$n.comparisons) {
        lines(he$k, he$eib[,j],
              lty = plot_aes$line$types[min(j, length(plot_aes$line$types))], 
              lwd = ifelse(plot.cri, lwd + 1, lwd), 
              col = plot_aes$line$colors[min(j, length(plot_aes$line$colors))])
        if (plot.cri) {
          lines(he$k, cri$low[cri$comp == j], lwd = lwd, 
                col = plot_aes$line$cri_colors[min(j, length(plot_aes$line$cri_colors))],
                lty = plot_aes$line$types[min(j, length(plot_aes$line$types))])
          lines(he$k, cri$upp[cri$comp == j], lwd = lwd, 
                col = plot_aes$line$cri_colors[min(j, length(plot_aes$line$cri_colors))],
                lty = plot_aes$line$types[min(j, length(plot_aes$line$types))])
        }
      }
      if (length(he$kstar) > 0 & is.null(size)) {
        abline(v = he$kstar, col = "dark grey", lty = "dotted")
        text(he$kstar, min(yl), paste("k* = ", he$kstar, sep = ""))
      }
      text <- paste0(he$interventions[he$ref]," vs ",he$interventions[he$comp])
      legend(
        alt.legend, text, cex = .7, bty = "n",
        lty = plot_aes$line$types,
        lwd = ifelse(plot.cri,lwd + 1, lwd))
    } else if(he$n.comparisons>1&!is.null(comparison)) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[,comparison]
      he$delta.c <- he$delta.c[,comparison]
      he$n.comparators=length(comparison)+1
      he$n.comparisons=length(comparison)
      he$interventions=he$interventions[sort(c(he$ref,he$comp))]
      he$ICER=he$ICER[comparison]
      he$ib=he$ib[,,comparison]
      he$eib=he$eib[,comparison]
      he$U=he$U[,,sort(c(he$ref,comparison+1))]
      he$ceac=he$ceac[,comparison]
      he$ref=rank(c(he$ref,he$comp))[1]
      he$comp=rank(c(he$ref,he$comp))[-1]
      he$mod <- TRUE #
      
      eib.plot(he,pos=alt.legend,graph="base",size=size,comparison=NULL,plot.cri=plot.cri,alpha=alpha,cri.quantile=cri.quantile,...)
    }
  } else if (graph_choice == 2) {
    # ggplot2 version -----
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))){
      message("falling back to base graphics\n")
      eib.plot(he,pos=alt.legend,graph="base"); return(invisible(NULL))
    }
    
    ### no visible binding note
    k <- kstar <- low <- upp <- NA_real_
    
    if(is.null(size))
      size=ggplot2::rel(3.5)
    
    opt.theme <- ggplot2::theme()
    for(obj in exArgs)
      if(ggplot2::is.theme(obj))
        opt.theme <- opt.theme + obj
    
    if (he$n.comparisons == 1) {
      # data frame
      data.psa <- data.frame(
        "k" = he$k, "eib" = he$eib, 
        "comparison" = as.factor(c(
          sapply(1:he$n.comparisons, function(x) rep(x, length(he$k)))
        )))
      if (plot.cri) 
        data.psa <- cbind(data.psa,cri)
      if (is.null(plot_aes$line$types))
        plot_aes$line$types = 1:he$n.comparisons
      eib <- ggplot2::ggplot(data.psa, ggplot2::aes(k, eib)) +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "grey")
      if (!isTRUE(he$mod)) {
        eib <- eib +
          ggplot2::geom_line(
            colour = plot_aes$line$colors[1],
            linetype = plot_aes$line$types[1])
      }
      else{
        eib <- eib + 
          ggplot2::geom_line(linetype = plot_aes$line$types[1], colour = plot_aes$line$colors[1]) +
          ggplot2::scale_linetype_manual(
            "", values = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]),
            labels = with(he,paste0(interventions[ref]," vs ",interventions[comp])))
      }
      
      if (!length(he$kstar) == 0 & !is.na(size)) {
        # label
        label <- paste0("k* = ", format(he$kstar, digits = 6))
        eib <- eib +
          ggplot2::geom_vline(ggplot2::aes(xintercept=kstar),data=data.frame("kstar"=he$kstar),colour="grey50",linetype=2,size=.5) +
          ggplot2::annotate("text",label=label,x=he$kstar,y=min(yl),hjust=ifelse((max(he$k)-he$kstar)/max(he$k)>1/6,-.1,1.1),size=size)
      }
      if (plot.cri) {
        eib <- eib +
          ggplot2::geom_line(ggplot2::aes(y = low), colour = plot_aes$line$cri_colors[1], lty = 2) +
          ggplot2::geom_line(ggplot2::aes(y = upp), colour = plot_aes$line$cri_colors[1], lty = 2)
      }
    } else if (he$n.comparisons > 1 & is.null(comparison) == TRUE) {
      data.psa <- data.frame(
        "k" = c(he$k), "eib" = c(he$eib),
        "comparison" = as.factor(c(
          sapply(1:he$n.comparisons, function(x) rep(x, length(he$k)))
        )))
      if (plot.cri)
        data.psa <- cbind(data.psa,cri)
      # labels for legend
      comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
      
      # linetype is the indicator of the comparison.
      # 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
      if (is.null(plot_aes$line$types))
        plot_aes$line$types <- rep(c(1,2,3,4,5,6),ceiling(he$n.comparisons/6))[1:he$n.comparisons]
      if (length(plot_aes$line$types) < length(comparisons.label))
        plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
      if (length(plot_aes$line$colors) < length(comparisons.label))
        plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
      
      eib <- 
        ggplot2::ggplot(
          data.psa,
          ggplot2::aes(x = k, y = eib, linetype = comparison, colour = comparison)) + 
        ggplot2::geom_hline(yintercept = 0, linetype = 1, color = "grey") + 
        ggplot2::theme_bw() +
        ggplot2::geom_line(lwd = ifelse(!plot.cri,0.5,0.75)) +
        ggplot2::scale_colour_manual(
          "", labels = comparisons.label, 
          values = plot_aes$line$colors) +
        ggplot2::scale_linetype_manual(
          "", labels = comparisons.label, 
          values = plot_aes$line$types)
      
      if(!length(he$kstar) == 0 & !is.na(size)) {
        # label
        label <- paste0("k* = ",format(he$kstar,digits=6))
        eib <- eib +
          ggplot2::geom_vline(ggplot2::aes(xintercept=kstar),data=data.frame("kstar"=he$kstar),colour="grey50",linetype=2,size=.5) + 
          ggplot2::annotate("text",label=label,x=he$kstar,y=min(yl),hjust=ifelse((max(he$k)-he$kstar)/max(he$k)>1/6,-.1,1.1),size=size,vjust=1)
      }
      
      if (plot.cri) {
        eib <- eib +
          ggplot2::geom_line(ggplot2::aes(y = low), colour = plot_aes$line$cri_colors, show.legend = F) +
          ggplot2::geom_line(ggplot2::aes(y = upp), colour = plot_aes$line$cri_colors, show.legend = F)
      }
    } else if (he$n.comparisons > 1 & is.null(comparison) == FALSE) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[,comparison]
      he$delta.c <- he$delta.c[,comparison]
      he$n.comparators=length(comparison)+1
      he$n.comparisons=length(comparison)
      he$interventions=he$interventions[sort(c(he$ref,he$comp))]
      he$ICER=he$ICER[comparison]
      he$ib=he$ib[,,comparison]
      he$eib=he$eib[,comparison]
      he$U=he$U[,,sort(c(he$ref,comparison+1))]
      he$ceac=he$ceac[,comparison]
      he$ref=rank(c(he$ref,he$comp))[1]
      he$comp=rank(c(he$ref,he$comp))[-1]
      he$mod <- TRUE #
      
      return(eib.plot(he,pos=alt.legend,graph="ggplot2",size=size,comparison=NULL,plot.cri=plot.cri,alpha=alpha,cri.quantile=cri.quantile,...))
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
            paste0("\nand ", format((1 - alpha)*100, digits = 4), "% credible intervals"),
            "")),
          plot_annotations$title))
    jus <- NULL
    if(isTRUE(alt.legend)) {
      alt.legend="bottom"
      eib <- eib + ggplot2::theme(legend.direction="vertical")
    }
    else{
      if(is.character(alt.legend)) {
        choices <- c("left", "right", "bottom", "top")
        alt.legend <- choices[pmatch(alt.legend,choices)]
        jus="center"
        if(is.na(alt.legend))
          alt.legend=FALSE
      }
      if(length(alt.legend)>1)
        jus <- alt.legend
      if(length(alt.legend)==1 & !is.character(alt.legend)) {
        alt.legend <- c(1,0)
        jus <- alt.legend
      }
    }
    eib <- eib + 
      ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                     legend.background=ggplot2::element_blank(),text=ggplot2::element_text(size=11),
                     legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),
                     panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank(),legend.text.align=0,
                     plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5)) +
      opt.theme
    
    return(eib)
  } else if (graph_choice == 3) {
    # plotly version -----
    if (!is.null(size) && !is.na(size)) {
        message("Option size will be ignored using plotly."); size <- NULL
    }
    
    if(he$n.comparisons > 1& is.null(comparison) == FALSE) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[,comparison]
      he$delta.c <- he$delta.c[,comparison]
      he$n.comparators=length(comparison)+1
      he$n.comparisons=length(comparison)
      he$interventions=he$interventions[sort(c(he$ref,he$comp))]
      he$ICER=he$ICER[comparison]
      he$ib=he$ib[,,comparison]
      he$eib=he$eib[,comparison]
      he$U=he$U[,,sort(c(he$ref,comparison+1))]
      he$ceac=he$ceac[,comparison]
      he$ref=rank(c(he$ref,he$comp))[1]
      he$comp=rank(c(he$ref,he$comp))[-1]
      he$mod <- TRUE #
      
      return(eib.plot(he,pos=alt.legend,graph="plotly",size=size,comparison=NULL,plot.cri=plot.cri,alpha=alpha,cri.quantile=cri.quantile,...))
    }
    
    if (is.null(plot_aes$line$types))
      plot_aes$line$types <- rep(c(1:6),ceiling(he$n.comparisons/6))[1:he$n.comparisons]
    comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
    if (length(plot_aes$line$types) < length(comparisons.label))
      plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
    if (length(plot_aes$line$colors) < length(comparisons.label))
      plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
    # opacities
    plot_aes$line$cri_colors <- sapply(plot_aes$line$cri_colors, function(x) 
      ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
    plot_aes$area$color <- sapply(plot_aes$area$color, function(x)
      ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
    # data frame
    data.psa <- data.frame(
      "k" = he$k, "eib" = c(he$eib),
      "comparison" = as.factor(c(
        sapply(1:he$n.comparisons, function(x) rep(x, length(he$k)))
      )),
      "label" = as.factor(c(
        sapply(comparisons.label, function(x) rep(x, length(he$k)))
      )))
    if (plot.cri)
      data.psa <- cbind(data.psa, cri)
    eib <- plotly::plot_ly(data.psa, x = ~k)
    eib <- plotly::add_trace(
      eib,
      y = ~eib, type = "scatter", mode = "lines",
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
      if (he$n.comparisons == 1) {
        eib <- plotly::add_ribbons(
          eib,
          name = paste0(100 * (1 - alpha), "% CrI"),
          ymin = ~low, ymax = ~upp,
          color = NA,
          fillcolor = ~plot_aes$line$cri_colors[comparison])
      } else {
        eib <- plotly::add_ribbons(
          eib,
          name = ~label,
          ymin = ~low, ymax = ~upp,
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
    legend_list = list(orientation = "h", xanchor = "center", x = 0.5)
    if (is.character(alt.legend))
      legend_list = switch(
        alt.legend,
        "left" = list(orientation = "v", x = 0, y = 0.5),
        "right" = list(orientation = "v", x = 0, y = 0.5),
        "bottom" = list(orienation = "h", x = .5, y = 0, xanchor = "center"),
        "top" = list(orientation = "h", x = .5, y = 100, xanchor = "center"))
    
    eib <- plotly::layout(
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
          plot_annotations$xlab
        )),
      yaxis = list(
        hoverformat = ".2f",
        title = switch(
          as.numeric(plot_annotations$exist$xlab) + 1,
          "EIB",
          plot_annotations$ylab
        )),
      showlegend = TRUE, 
      legend = legend_list)
    eib <- plotly::config(eib, displayModeBar = FALSE)
    return(eib)
  }
}
