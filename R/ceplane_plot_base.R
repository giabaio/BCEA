
ceplane_plot_base <- function() {
  
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
  
  if(he$n_comparisons==1) {
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
  } else if(he$n_comparisons > 1 & is.null(comparison)) {
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
    for (i in 2:he$n_comparisons) {
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
  } else if(he$n_comparisons > 1 & !is.null(comparison) & length(comparison) == 1) {
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
  } else if(he$n_comparisons>1&is.null(comparison)==FALSE&length(comparison)!=1) {
    stopifnot(all(comparison %in% 1:he$n_comparisons))
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta.e <- he$delta.e[,comparison]
    he$delta.c <- he$delta.c[,comparison]
    he$n_comparators=length(comparison)+1
    he$n_comparisons=length(comparison)
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
  
}
