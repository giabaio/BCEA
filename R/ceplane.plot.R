# ceplane.plot -----

#' Cost-effectiveness plane plot
#' 
#' Produces a scatter plot of the cost-effectiveness plane, together with the
#' sustainability area, as a function of the selected willingness to pay
#' threshold
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#'   modelling and the economic evaluation.
#' @param comparison Selects the comparator, in case of more than two
#'   interventions being analysed. Default as \code{NULL} plots all the
#'   comparisons together. Any subset of the possible comparisons can be selected
#'   (e.g., \code{comparison=c(1,3)} or \code{comparison=2}).
#' @param wtp The value of the willingness to pay parameter. Not used iff
#'   \code{graph="base"} for multiple comparisons.
#' @param pos Parameter to set the position of the legend; for a single
#'   comparison plot, the ICER legend position. Can be given in form of a string
#'   \code{(bottom|top)(right|left)} for base graphics and
#'   \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#'   which specifies the relative position on the x and y axis respectively, or
#'   alternatively it can be in form of a logical variable, with \code{FALSE}
#'   indicating to use the default position and \code{TRUE} to place it on the
#'   bottom of the plot. Default value is \code{c(1,1)}, that is the topright
#'   corner inside the plot area.
#' @param size Value (in millimetres) of the size of the willingness to pay
#'   label. Used only if \code{graph="ggplot2"}, otherwise is ignored with a
#'   message.
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-)match the two options \code{"base"} or
#'   \code{"ggplot2"}. Default value is \code{"base"}.
#' @param xlim The range of the plot along the x-axis. If NULL (default) it is
#'   determined by the range of the simulated values for \code{delta.e}
#' @param ylim The range of the plot along the y-axis. If NULL (default) it is
#'   determined by the range of the simulated values for \code{delta.c}
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional graphical arguments:
#'  \itemize{
#'   \item \code{label.pos=FALSE} will place the willingness to pay label in a different 
#'   position at the bottom of the graph - base and ggplot2 only (no label in plotly);
#'   \item \code{point_colors}: a vector of colours specifying the colour(s) associated to 
#'   the cloud of points. Should be of length 1 or equal to the number of comparisons.
#'   \item \code{point_sizes}: a vector of colours specifying the size(s) of the points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{ICER_colors}: a vector of colours specifying the colour(s) of the ICER points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{ICER_sizes}:  a vector of colours specifying the size(s) of the ICER points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{area_include}: logical, include or exclude the cost-effectiveness 
#'   acceptability area (default is TRUE).
#'   \item \code{area_color}: a color specifying the colour of the cost-effectiveness acceptability area
#'  }
#' @return \item{ceplane}{ If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#'   the default.} 
#'   The function produces a plot of the
#'   cost-effectiveness plane. Grey dots show the simulated values for the joint
#'   distribution of the effectiveness and cost differentials.  The larger red
#'   dot shows the ICER and the grey area identifies the sustainability area,
#'   i.e. the part of the plan for which the simulated values are below the
#'   willingness to pay threshold. The proportion of points in the sustainability
#'   area effectively represents the CEAC for a given value of the willingness to
#'   pay. If the comparators are more than 2 and no pairwise comparison is
#'   specified, all scatterplots are graphed using different colors.
#' @details In the plotly version, point_colors, ICER_colors and area_color can also be specified
#'   as rgba colours using either the \code{\link[plotly]{toRGB}{plotly::toRGB}} function or
#'   a rgba colour string, e.g. \code{'rgba(1, 1, 1, 1)'}.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Cost Effectiveness Plane
#' @examples
#' 
#' ### create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e,c,ref=4,Kmax=500,interventions=treats)
#' ### produce the plot
#' ceplane.plot(m,wtp=200,graph="base")
#' ### select only one comparator
#' ceplane.plot(m,wtp=200,graph="base",comparator=3)
#' ### or use ggplot2 instead
#' if(requireNamespace("ggplot2")){
#' ceplane.plot(m,wtp=200,pos="right",ICER_sizes=2,graph="ggplot2")
#' }
#' 
#' @export ceplane.plot
ceplane.plot <- function(he,
                         comparison = NULL,
                         wtp = 25000,
                         pos = c(1, 1),
                         size = NULL,
                         graph = c("base", "ggplot2"),
                         xlim = NULL,
                         ylim = NULL,
                         ...) {
  # Forces R to avoid scientific format for graphs labels
  options(scipen = 10)
  ### hidden options for ggplot2 ###
  # ICER.size =                    # changes ICER point size
  # label.pos = FALSE              # uses alternate position for wtp label (old specification)
  alt.legend <- pos
  # choose graphical engine
  if (is.null(graph) || is.na(graph)) graph = "base"
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  # check feasibility
  if (graph_choice == 2 && !requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)) {
    warning("Package ggplot2 and grid not found; eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  if (graph_choice == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found; eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  # evaluate additional arguments -----
  exArgs <- list(...)
  plot_annotations <- list("exist" = list("title" = FALSE, "xlab" = FALSE, "ylab" = FALSE))
  plot_aes <- list("area" = list("include" = TRUE, "color" = "light gray", "line_color" = "black"),
                   "point" = list("colors" = "black", "sizes" = 4),
                   "ICER" = list("colors" = "red", "sizes" = 8),
                   "exist" = list("area" = list("include" = FALSE, "color" = FALSE, "line_color" = FALSE),
                                  "point" = list("colors" = FALSE, "sizes" = FALSE),
                                  "ICER" = list("colors" = FALSE, "sizes" = FALSE)))
  plot_aes_args = c("area_include", "area_color", "area_line_color",
                    "point_colors", "point_sizes",
                    "ICER_colors", "ICER_sizes")
  if (length(exArgs) >= 1) {
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
        plot_aes$exist[[aes_cat]][[aes_name]] <- TRUE
      }
    }
  }
  # Args compatibility
  if (exists("ICER.size", where = exArgs)) {
    if (plot_aes$exist$ICER$sizes) {
      warning("Both ICER.size and ICER_sizes arguments specified. ICER_sizes will be used.")
    } else {
      warning("ICER.size is softly deprecated. Please use ICER_sizes instead.")
      plot_aes$exist$ICER$sizes <- TRUE
      plot_aes$ICER$sizes <- exArgs$ICER.size
    }
  }
  if (exists("col", where = exArgs)) {
    if (plot_aes$exist$point$colors) {
      warning("Both col and point_colors arguments specified. point_colors will be used.")
    } else {
      warning("col argument is softly deprecated. Please use point_colors instead.")
      plot_aes$exist$point$colors <- TRUE
      plot_aes$point$colors <- exArgs$col
    }
  }
  # set default colour scheme
  if (!plot_aes$exist$point$colors) {
    if (he$n.comparisons > 1 & (is.null(comparison) || length(comparison) > 1)) {
      plot_aes$point$colors <- colors()[floor(seq(262, 340, length.out = he$n.comparisons))]
    } else {
      plot_aes$point$colors <- "grey55"
    }
  }
  # default plot annotations -----
  if (!plot_annotations$exist$title)
    plot_annotations$title <- with(he, paste0(
      "Cost-Effectiveness Plane",
      ifelse(
        n.comparisons == 1 | (n.comparisons > 1 & (!is.null(comparison) && length(comparison) == 1)),
        paste0("\n", interventions[ref], " vs ", interventions[-ref]),
        paste0(ifelse(
          isTRUE(he$mod),
          paste0(
            "\n",
            interventions[ref],
            " vs ",
            paste0(interventions[comp], collapse = ", ")
          ),
          ""
        ))
      )
    ))
  if (!plot_annotations$exist$xlab)
    plot_annotations$xlab = "Effectiveness differential"
  if (!plot_annotations$exist$ylab)
    plot_annotations$ylab = "Cost differential"
  
  if (graph_choice == 1) {
    # base graphics version -----
    if(!is.null(size))
      message("option size will be ignored using base graphics")
    if(is.numeric(alt.legend)&length(alt.legend)==2){
      temp <- ""
      if(alt.legend[2]==0)
        temp <- paste0(temp,"bottom")
      else
        temp <- paste0(temp,"top")
      if(alt.legend[1]==0)
        temp <- paste0(temp,"left")
      else
        temp <- paste0(temp,"right")
      alt.legend <- temp
      if(length(grep("^(bottom|top)(left|right)$",temp))==0)
        alt.legend <- FALSE
    }
    if(is.logical(alt.legend)){
      if(!alt.legend)
        alt.legend="topright"
      else
        alt.legend="topleft"
    }
    
    # Encodes characters so that the graph can be saved as ps or pdf
    ps.options(encoding="CP1250")
    pdf.options(encoding="CP1250")
    
    if(he$n.comparisons==1) {
      m.e <- range(he$delta.e)[1]
      M.e <- range(he$delta.e)[2]
      m.c <- range(he$delta.c)[1]
      M.c <- range(he$delta.c)[2]
      step <- (M.e-m.e)/10
      m.e <- ifelse(m.e<0,m.e,-m.e)
      m.c <- ifelse(m.c<0,m.c,-m.c)
      x.pt <- .95*m.e
      y.pt <- ifelse(x.pt*wtp<m.c,m.c,x.pt*wtp)
      xx <- seq(100*m.c/wtp,100*M.c/wtp,step)
      yy <- xx*wtp
      xx[1] <- ifelse(min(xx)<m.e,xx[1],2*m.e)
      yy[1] <- ifelse(min(yy)<m.c,yy[1],2*m.c)
      xx[length(xx)] <- ifelse(xx[length(xx)]<M.e,1.5*M.e,xx[length(xx)])
      if(!is.null(xlim)) {m.e <- xlim[1]; M.e <- xlim[2]}
      if(!is.null(ylim)) {m.c <- ylim[1]; M.c <- ylim[2]}
      plot(
        xx, yy, col = "white", axes = F,
        xlim = c(m.e, M.e), ylim = c(m.c, M.c),
        xlab = plot_annotations$xlab,
        ylab = plot_annotations$ylab,
        main = plot_annotations$title
      )
      polygon(c(min(xx),seq(min(xx),max(xx),step),max(xx)),
              c(min(yy),wtp*seq(min(xx),max(xx),step),min(yy)),
              # Note: default area colour light gray is too dark for base graphics
              col = ifelse(!plot_aes$exist$area$color, "grey95", plot_aes$area$color),
              border = ifelse(!plot_aes$exist$area$line_color, "black", plot_aes$area$line_color))
      axis(1); axis(2); box()
      points(
        he$delta.e,he$delta.c,pch=20,
        cex = ifelse(
          !plot_aes$exist$point$sizes,
          .35,
          plot_aes$point$sizes[1]),
        col = plot_aes$point$colors[1])
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      text(
        M.e,M.c,paste("\U2022"," ICER=",format(he$ICER,digits=6,nsmall=2),sep=""),cex=.95,pos=2,
        col = ifelse(
          !plot_aes$exist$ICER$colors,
          "red",
          plot_aes$ICER$colors[1]))
      points(
        mean(he$delta.e),mean(he$delta.c),pch=20,
        col = ifelse(
          !plot_aes$exist$ICER$colors,
          "red",
          plot_aes$ICER$colors[1]),
        cex = ifelse(
          !plot_aes$exist$ICER$sizes,
          1,
          plot_aes$ICER$sizes[1]))
      t1 <- paste("k==",format(wtp,digits=3,nsmall=2,scientific=F),sep="")
      text(x.pt,y.pt,parse(text=t1),cex=.8,pos=4)
    } else if(he$n.comparisons>1 & is.null(comparison)==TRUE) {
      if(is.null(xlim)) {xlim <- range(he$delta.e)}
      if(is.null(ylim)) {ylim <- range(he$delta.c)}
      plot(
        he$delta.e[, 1],
        he$delta.c[, 1],
        pch = 20,
        cex = ifelse(
          !plot_aes$exist$point$sizes,
          .35,
          plot_aes$point$sizes[1]),
        col = plot_aes$point$colors[1],
        xlim = xlim,
        ylim = ylim,
        xlab = plot_annotations$xlab,
        ylab = plot_annotations$ylab,
        main = plot_annotations$title
      )
      for (i in 2:he$n.comparisons) {
        points(
          he$delta.e[,i],he$delta.c[,i],pch=20,
          cex = ifelse(
            !plot_aes$exist$point$sizes,
            .35,
            plot_aes$point$sizes[i]),
          col = plot_aes$point$colors[i])
      }
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      text <- paste(he$interventions[he$ref]," vs ",he$interventions[he$comp])
      legend(alt.legend,text,col=plot_aes$point$colors,cex=.7,bty="n",lty=1)
    } else if(he$n.comparisons>1 & is.null(comparison)==FALSE & length(comparison)==1) {
      m.e <- range(he$delta.e[,comparison])[1]
      M.e <- range(he$delta.e[,comparison])[2]
      m.c <- range(he$delta.c[,comparison])[1]
      M.c <- range(he$delta.c[,comparison])[2]
      step <- (M.e-m.e)/10
      m.e <- ifelse(m.e<0,m.e,-m.e)
      m.c <- ifelse(m.c<0,m.c,-m.c)
      x.pt <- .95*m.e
      y.pt <- ifelse(x.pt*wtp<m.c,m.c,x.pt*wtp)
      xx <- seq(100*m.c/wtp,100*M.c/wtp,step)
      yy <- xx*wtp
      xx[1] <- ifelse(min(xx)<m.e,xx[1],2*m.e)
      yy[1] <- ifelse(min(yy)<m.c,yy[1],2*m.c)
      xx[length(xx)] <- ifelse(xx[length(xx)]<M.e,1.5*M.e,xx[length(xx)])
      if(!is.null(xlim)) {m.e <- xlim[1]; M.e <- xlim[2]}
      if(!is.null(ylim)) {m.c <- ylim[1]; M.c <- ylim[2]}
      plot(
        xx, yy, col = "white", axes = F,
        xlim = c(m.e, M.e), ylim = c(m.c, M.c),
        xlab = plot_annotations$xlab,
        ylab = plot_annotations$ylab,
        main = plot_annotations$title
      )
      polygon(
        c(min(xx),seq(min(xx),max(xx),step),max(xx)),
        c(min(yy),wtp*seq(min(xx),max(xx),step),min(yy)),
        # Note: default area colour light gray is too dark for base graphics
        col = ifelse(is.null(plot_aes$area$color), "grey95", plot_aes$area$color),
        border = ifelse(!plot_aes$exist$area$line_color, "black", plot_aes$area$line_color))
      axis(1); axis(2); box()
      points(
        he$delta.e[,comparison],he$delta.c[,comparison],pch=20,
        cex = ifelse(
          !plot_aes$exist$point$sizes,
          .35,
          plot_aes$point$sizes[1]),
        col = plot_aes$point$colors[1])
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      text(
        M.e,M.c,paste("\U2022"," ICER=",format(he$ICER[comparison],digits=6,nsmall=2),sep=""),cex=.95,pos=2,
        col = ifelse(
          !plot_aes$exist$ICER$colors,
          "red",
          plot_aes$ICER$colors[1]))
      points(
        mean(he$delta.e[,comparison]),mean(he$delta.c[,comparison]),pch=20,
        col = ifelse(
          !plot_aes$exist$ICER$colors,
          "red",
          plot_aes$ICER$colors[1]),
        cex = ifelse(
          !plot_aes$exist$ICER$sizes,
          1,
          plot_aes$ICER$sizes[1]))
      t1 <- paste("k==",format(wtp,digits=3,nsmall=2,scientific=F),sep="")
      text(x.pt,y.pt,parse(text=t1),cex=.8,pos=4)
    } else if(he$n.comparisons>1&is.null(comparison)==FALSE&length(comparison)!=1) {
      stopifnot(all(comparison %in% 1:he$n.comparisons))
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
      return(ceplane.plot(he,wtp=wtp,pos=alt.legend,graph="base",size=size,...))
    }
  } else if (graph_choice == 2) {
    # ggplot2 version -----
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE) & requireNamespace("grid",quietly=TRUE))){
      message("Falling back to base graphics\n")
      ceplane.plot(he,comparison=comparison,wtp=wtp,pos=alt.legend,graph="base"); return(invisible(NULL))
    }
    # no visible binding note
    delta.e <- delta.c <- lambda.e <- lambda.c <- NULL
    if (is.null(size))
      size <- ggplot2::rel(3.5)
    label.pos <- TRUE
    opt.theme <- ggplot2::theme()
    if (!plot_aes$exist$ICER$sizes)
      plot_aes$ICER$sizes <- ifelse(he$n.comparisons == 1, 2, 0)
    if (length(exArgs) >= 1) {
      if (exists("label.pos", where = exArgs))
        if (is.logical(exArgs$label.pos))
          label.pos <- exArgs$label.pos
        for (obj in exArgs)
          if (ggplot2::is.theme(obj))
            opt.theme <- opt.theme + obj
    }
    if (he$n.comparisons == 1) {
      kd <- data.frame(he$delta.e,he$delta.c)
      names(kd) <- c("delta.e","delta.c")
      # for scale_x_continuous(oob=)
      do.nothing=function(x,limits) return(x)
      # plot limits
      range.e <- range(kd$delta.e)
      range.c <- range(kd$delta.c)
      range.e[1] <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
      range.c[1] <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
      # ce plane data
      x1 <- range.e[1]-2*abs(diff(range.e))
      x2 <- range.e[2]+2*abs(diff(range.e))
      x3 <- x2
      x <- c(x1,x2,x3)
      y <- x*wtp; y[3] <- x1*wtp
      plane <- data.frame(x=x,y=y)
      # build a trapezoidal plane instead of a triangle if the y value is less than the minimum difference on costs
      if(y[1]>1.2*range.c[1]) {
        plane <- rbind(plane,
                       c(x2,2*range.c[1]), #new bottom-right vertex
                       c(x1,2*range.c[1])) #new bottom-left vertex
      }
      # actual plot
      ceplane <- ggplot2::ggplot(kd, ggplot2::aes(delta.e,delta.c)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(limits=range.e,oob=do.nothing) + 
        ggplot2::scale_y_continuous(limits=range.c,oob=do.nothing) +
        ggplot2::scale_color_manual(
          "",labels=paste0("ICER = ",format(he$ICER,digits=6,nsmall=2),"  "),
          values = ifelse(!plot_aes$exist$ICER$colors, "red", plot_aes$ICER$colors[1])) +     
        ggplot2::geom_line(
          data = plane[1:2,], ggplot2::aes(x = x, y = y),
          color = ifelse(!plot_aes$exist$area$line_color, "black", plot_aes$area$line_color),
          linetype = 1) +
        ggplot2::geom_polygon(
          data = plane,ggplot2::aes(x = x, y = y),
          fill = ifelse(is.null(plot_aes$area$color), "light gray", plot_aes$area$color),
          alpha = .3) +
        ggplot2::geom_hline(ggplot2::aes(yintercept=0),colour="grey") + 
        ggplot2::geom_vline(ggplot2::aes(xintercept=0),colour="grey") +
        ggplot2::geom_point(
          size = ifelse(!plot_aes$exist$point$sizes, 1, plot_aes$point$sizes[1]),
          colour = plot_aes$point$colors[1]) +
        ggplot2::geom_point(
          ggplot2::aes(
            mean(delta.e),mean(delta.c),
            color = as.factor(1)),
          size = plot_aes$ICER$sizes[1])
      if(!label.pos) {
        # moves the wtp label depending on whether the line crosses the y-axis
        ceplane <- ceplane +
          ggplot2::annotate(
            geom = "text",
            x = ifelse(range.c[1] / wtp > range.e[1], range.c[1] / wtp, range.e[1]),
            y = range.c[1],
            label = paste0("k = ", format(wtp, digits = 6)),
            hjust = -.15,
            size = size)
      }
      else{
        m.e <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
        m.c <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
        x.pt <- .95*m.e
        y.pt <- ifelse(x.pt*wtp<m.c,m.c,x.pt*wtp)
        ceplane <- ceplane + 
          ggplot2::annotate(
            geom = "text",
            x = x.pt,
            y = y.pt,
            label = paste0("k = ", format(wtp, digits = 6)),
            hjust = .15,
            size = size)
      }
    } else if(he$n.comparisons>1&is.null(comparison)==TRUE) {
      # create dataframe for plotting
      kd <- with(he,data.frame("delta.e" = c(delta.e), "delta.c" = c(delta.c)))
      kd$comparison <- as.factor(sort(rep(1:he$n.comparisons,dim(he$delta.e)[1])))
      # dataset for ICERs
      means <- matrix(NA_real_,nrow=he$n.comparisons,ncol=2)
      for (i in 1:he$n.comparisons)
        means[i,] <- colMeans(kd[kd$comparison == i, -3])
      means <- data.frame(means)
      means$comparison <- factor(1:he$n.comparisons)
      names(means) <- c("lambda.e","lambda.c","comparison")
      # labels for legend
      comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
      # polygon
      do.nothing = function(x,limits) return(x)
      # plot limits
      range.e <- range(kd$delta.e)
      range.c <- range(kd$delta.c)
      range.e[1] <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
      range.c[1] <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
      # ce plane data
      x1 <- range.e[1]-2*abs(diff(range.e))
      x2 <- range.e[2]+2*abs(diff(range.e))
      x3 <- x2
      x <- c(x1,x2,x3)
      y <- x*wtp; y[3] <- x1*wtp
      plane <- data.frame(x=x,y=y,comparison=factor(rep(he$n.comparisons+1,3)))
      # build a trapezoidal plane instead of a triangle if the y value is less than the minimum difference on costs
      if(y[1]>min(kd$delta.c)) {
        plane <- rbind(plane,
                       c(x2,2*min(kd$delta.c),he$n.comparisons+1), #new bottom-right vertex
                       c(x1,2*min(kd$delta.c),he$n.comparisons+1)) #new bottom-left vertex
      }
      ceplane <-
        ggplot2::ggplot(kd,ggplot2::aes(x=delta.e,y=delta.c,col=comparison)) +
        ggplot2::theme_bw() +
        ggplot2::scale_color_manual(
          labels = comparisons.label, 
          values = plot_aes$point$colors,
          na.value = "black") +
        ggplot2::scale_size_manual(
          labels = comparisons.label,
          values = if(!plot_aes$exist$point$sizes)
            rep_len(1, length(comparisons.label)) else
              rep_len(plot_aes$point$sizes, length(comparisons.label)),
          na.value = 1) +
        ggplot2::scale_x_continuous(limits=range.e,oob=do.nothing) +
        ggplot2::scale_y_continuous(limits=range.c,oob=do.nothing) +
        ggplot2::annotate(
          "line",
          x = plane[1:2,1], y = plane[1:2,2],
          color = ifelse(!plot_aes$exist$area$line_color, "black", plot_aes$area$line_color)) +
        ggplot2::annotate(
          "polygon",
          plane$x, plane$y,
          fill = ifelse(is.null(plot_aes$area$color), "light gray", plot_aes$area$color),
          alpha = .3) +
        ggplot2::geom_hline(ggplot2::aes(yintercept=0),colour="grey") + ggplot2::geom_vline(ggplot2::aes(xintercept=0),colour="grey") +
        ggplot2::geom_point(
          ggplot2::aes(size = comparison))
      if (!all(plot_aes$ICER$sizes <= 0)) {
        ceplane <- ceplane +
          ggplot2::geom_point(
            data = means,
            ggplot2::aes(x = lambda.e, y = lambda.c),
            colour = plot_aes$ICER$colors,
            size = plot_aes$ICER$sizes)
      }
      # wtp label
      if (!label.pos) {
        ceplane <- ceplane +
          ggplot2::annotate(geom="text",
                            x=ifelse(range.c[1]/wtp>range.e[1],range.c[1]/wtp,range.e[1]),
                            y=range.c[1],
                            label=paste0("k = ",format(wtp,digits=6),"  "),hjust=.15,size=size
          )
      } else {
        m.e <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
        m.c <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
        x.pt <- .95*m.e
        y.pt <- ifelse(x.pt*wtp<m.c,m.c,x.pt*wtp)
        ceplane <- ceplane +
          ggplot2::annotate(
            geom = "text",
            x = x.pt,
            y = y.pt,
            label = paste0("k = ", format(wtp, digits = 6)),
            hjust = .15,
            size = size)
      }
    } else if (he$n.comparisons > 1 & is.null(comparison) == FALSE) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[, comparison]
      he$delta.c <- he$delta.c[, comparison]
      he$n.comparators <- length(comparison) + 1
      he$n.comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$mod <- TRUE #
      return(ceplane.plot(he,wtp=wtp,pos=alt.legend,graph="ggplot2",size=size,...))
    }
    ceplane <- ceplane + 
      ggplot2::labs(
        title = plot_annotations$title,
        x = plot_annotations$xlab,
        y = plot_annotations$ylab)
    jus <- NULL
    if (isTRUE(alt.legend)) {
      alt.legend="bottom"
      ceplane <- ceplane + ggplot2::theme(legend.direction="vertical")
    } else {
      if (is.character(alt.legend)) {
        choices <- c("left", "right", "bottom", "top")
        alt.legend <- choices[pmatch(alt.legend,choices)]
        jus="center"
        if (is.na(alt.legend))
          alt.legend = FALSE
      }
      if (length(alt.legend) > 1)
        jus <- alt.legend
      if (length(alt.legend) == 1 & !is.character(alt.legend)) {
        alt.legend <- c(1,1)
        jus <- alt.legend
      }
    }
    ceplane <- ceplane + 
      ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),legend.background=ggplot2::element_blank()) +
      ggplot2::theme(text=ggplot2::element_text(size=11),legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank(),legend.text.align=0) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
    if (he$n.comparisons == 1)
      ceplane  <- ceplane + ggplot2::theme(legend.key.size=grid::unit(.1,"lines"))
    ceplane <- ceplane + opt.theme
    return(ceplane)
  } else if (graph_choice == 3) {
    # plotly version -----
    if (he$n.comparisons > 1 & is.null(comparison) == FALSE) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[, comparison]
      he$delta.c <- he$delta.c[, comparison]
      he$n.comparators <- length(comparison) + 1
      he$n.comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$mod <- TRUE #
      return(ceplane.plot(he, wtp = wtp, pos = alt.legend, graph = "plotly", ...))
    }
    if (exists("ICER.size", where = exArgs)) {
      ICER.size <- exArgs$ICER.size
    } else {
      ICER.size <- ifelse(he$n.comparisons == 1, 8, 0)
    }
    # plot labels
    comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
    kd <- data.frame(
      "delta.e" = c(he$delta.e), "delta.c" = c(he$delta.c),
      "comparison" = as.factor(c(
        sapply(1:he$n.comparisons, function(x) rep(x, nrow(as.matrix(he$delta.e))))
      )),
      "label" = as.factor(c(
        sapply(comparisons.label, function(x) rep(x, nrow(as.matrix(he$delta.e))))
      )))
    if (length(plot_aes$point$colors) != length(comparisons.label))
      plot_aes$point$colors <- rep_len(plot_aes$point$colors, length(comparisons.label))
    if (length(plot_aes$point$sizes) != length(comparisons.label))
      plot_aes$point$sizes <- rep_len(plot_aes$point$sizes, length(comparisons.label))
    if (length(plot_aes$ICER$colors) != length(comparisons.label))
      plot_aes$ICER$colors <- rep_len(plot_aes$ICER$colors, length(comparisons.label))
    if (length(plot_aes$ICER$sizes) != length(comparisons.label))
      plot_aes$ICER$sizes <- rep_len(plot_aes$ICER$sizes, length(comparisons.label))
    # plot limits
    range.e <- range(kd$delta.e)
    range.c <- range(kd$delta.c)
    range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
    range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
    # ce plane data
    x1 <- range.e[1] - 2*abs(diff(range.e))
    x2 <- range.e[2] + 2*abs(diff(range.e))
    x = c(x1, x2, x2)
    y = c(x1*wtp, x2*wtp, x1*wtp)
    plane <- data.frame(x = x, y = y)
    # build a trapezoidal plane instead of a triangle if
    # the y value is less than the minimum difference on costs
    if (y[1] > 1.2*range.c[1])
      plane <- rbind(plane,
                     c(x2,2*range.c[1]), #new bottom-right vertex
                     c(x1,2*range.c[1])) #new bottom-left vertex
    xrng = c(ifelse(prod(range.e) < 0,
                    range.e[1]*1.1,
                    ifelse(range.e[1] < 0,
                           range.e[1]*1.1,
                           -(range.e[2] - range.e[1])*0.1)),
             ifelse(prod(range.e) < 0, range.e[2]*1.1,
                    ifelse(range.e[2] > 0,
                           range.e[2]*1.1,
                           (range.e[2] - range.e[1])*0.1)))
    yrng = c(ifelse(prod(range.c) < 0,
                    range.c[1]*1.1,
                    ifelse(range.c[1] < 0,
                           range.c[1]*1.1,
                           -(range.c[2] - range.c[1])*0.1)),
             ifelse(prod(range.c) < 0,
                    range.c[2]*1.1,
                    ifelse(range.c[2] > 0,
                           range.c[2]*1.1,
                           (range.c[2] - range.c[1])*0.1)))
    # Calculates dataset for ICERs from bcea object
    # @param he A BCEA object
    # @param comparisons.label Optional vector of strings with comparison labels
    # @return A data.frame object including mean outcomes, comparison identifier,
    #   comparison label and associated ICER
    tabulate_means = function(he, comparisons.label = NULL) {
      if (is.null(comparisons.label))
        comparisons.label <- 1:he$n.comparisons
      data.frame(
        "lambda.e" = sapply(1:he$n.comparisons, function(x) mean(as.matrix(he$delta.e)[,x])),
        "lambda.c" = sapply(1:he$n.comparisons, function(x) mean(as.matrix(he$delta.c)[,x])),
        "comparison" = as.factor(1:he$n.comparisons),
        "label" = comparisons.label,
        "ICER" = he$ICER
      )
    }
    # actual plot
    ceplane <- plotly::plot_ly()
    # CEA area
    if (plot_aes$area$include)
      ceplane <- plotly::add_trace(
        ceplane,
        type = "scatter", mode = "lines",
        data = plane,
        x = ~x, y = ~y,
        fill = "tonext",
        fillcolor = ifelse(
          grepl(pattern = "^rgba\\(", x = plot_aes$area$color),
          plot_aes$area$color,
          plotly::toRGB(plot_aes$area$color, 0.5)),
        line = list(color = ifelse(
          grepl(pattern = "^rgba\\(", x = plot_aes$area$line_color),
          plot_aes$area$line_color,
          plotly::toRGB(plot_aes$area$line_color, 1))),
        name = "CEA area")
    # cloud
    for (comp in 1:he$n.comparisons) {
      ceplane <- plotly::add_trace(
        ceplane,
        type = "scatter", mode = "markers",
        data = kd[kd$comparison == levels(kd$comparison)[comp],],
        y = ~delta.c,
        x = ~delta.e,
        marker = list(
          color = ifelse(
            grepl(pattern = "^rgba\\(", x = plot_aes$point$colors[comp]),
            plot_aes$point$colors[comp],
            plotly::toRGB(plot_aes$point$colors[comp])),
          size = plot_aes$point$sizes[comp]
        ),
        hoverinfo = "name+x+y",
        name = ~label)
    }
    # ICER
    if (!all(plot_aes$ICER$sizes <= 0)) {
      means_table = tabulate_means(he, comparisons.label)
      for (comp in 1:he$n.comparisons) {
        ceplane <- plotly::add_trace(
          ceplane,
          type = "scatter", mode = "markers",
          data = means_table[comp,],
          x = ~lambda.e,
          y = ~lambda.c,
          marker = list(
            color = plot_aes$ICER$colors[comp],
            size = plot_aes$ICER$sizes[comp]
          ),
          name = ~paste(
            ifelse(he$n.comparisons > 1, as.character(label), ""),
            "ICER:",
            prettyNum(round(ICER,2), big.mark = ","))
        )
      }
    }
    # layout
    legend_list = list(orientation = "h", xanchor = "center", x = 0.5)
    ceplane <- plotly::layout(
      ceplane,
      title = plot_annotations$title,
      xaxis = list(
        hoverformat = ".2f", range = xrng,
        title = plot_annotations$xlab
      ),
      yaxis = list(
        hoverformat = ".2f", range = yrng,
        title = plot_annotations$ylab
      ),
      showlegend = TRUE,
      legend = legend_list
    )
    ceplane <- plotly::config(ceplane, displayModeBar = FALSE)
    return(ceplane)
  }
}

