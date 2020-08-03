
ceplane_plot_ggplot <- function() {
  
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
    plot_aes$ICER$sizes <- ifelse(he$n_comparisons == 1, 2, 0)
  if (length(exArgs) >= 1) {
    if (exists("label.pos", where = exArgs))
      if (is.logical(exArgs$label.pos))
        label.pos <- exArgs$label.pos
      for (obj in exArgs)
        if (ggplot2::is.theme(obj))
          opt.theme <- opt.theme + obj
  }
  if (he$n_comparisons == 1) {
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
  } else if(he$n_comparisons > 1 & is.null(comparison)) {
    # create dataframe for plotting
    kd <- with(he,data.frame("delta.e" = c(delta.e), "delta.c" = c(delta.c)))
    kd$comparison <- as.factor(sort(rep(1:he$n_comparisons,dim(he$delta.e)[1])))
    # dataset for ICERs
    means <- matrix(NA_real_,nrow=he$n_comparisons,ncol=2)
    for (i in 1:he$n_comparisons)
      means[i,] <- colMeans(kd[kd$comparison == i, -3])
    means <- data.frame(means)
    means$comparison <- factor(1:he$n_comparisons)
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
    plane <- data.frame(x=x,y=y,comparison=factor(rep(he$n_comparisons+1,3)))
    # build a trapezoidal plane instead of a triangle if the y value is less than the minimum difference on costs
    if(y[1]>min(kd$delta.c)) {
      plane <- rbind(plane,
                     c(x2,2*min(kd$delta.c),he$n_comparisons+1), #new bottom-right vertex
                     c(x1,2*min(kd$delta.c),he$n_comparisons+1)) #new bottom-left vertex
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
  } else if (he$n_comparisons > 1 & is.null(comparison) == FALSE) {
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta.e <- he$delta.e[, comparison]
    he$delta.c <- he$delta.c[, comparison]
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
  if (he$n_comparisons == 1)
    ceplane  <- ceplane + ggplot2::theme(legend.key.size=grid::unit(.1,"lines"))
  ceplane + opt.theme
}
