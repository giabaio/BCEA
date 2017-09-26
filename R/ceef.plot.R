###cost-effectiveness efficiency frontier#############################################
# tan(theta)=e/c
# theta=atan(e/c)
# if theta_1<theta_2, take 1



#' Cost-Effectiveness Efficiency Frontier (CEAF) plot
#' 
#' Produces a plot of the Cost-Effectiveness Efficiency Frontier (CEEF)
#' 
#' The \code{bcea} objects did not include the generating \code{e} and \code{c}
#' matrices in BCEA versions <2.1-0. This function is not compatible with
#' objects created with previous versions. The matrices can be appended to
#' \code{bcea} objects obtained using previous versions, making sure that the
#' class of the object remains unaltered.
#' 
#' The argument \code{print.summary} allows for printing a brief summary of the
#' efficiency frontier, with default to \code{TRUE}. Two tables are plotted,
#' one for the interventions included in the frontier and one for the dominated
#' interventions. The average costs and clinical benefits are included for each
#' intervention. The frontier table includes the slope for the increase in the
#' frontier and the non-frontier table displays the dominance type of each
#' dominated intervention. Please note that the slopes are defined as the
#' increment in the costs for a unit increment in the benefits even if
#' \code{flip = TRUE} for consistency with the ICER definition. The angle of
#' increase is in radians and depends on the definition of the axes, i.e. on
#' the value given to the \code{flip} argument.
#' 
#' If the argument \code{relative} is set to \code{TRUE}, the graph will not
#' display the absolute measures of costs and benefits. Instead the axes will
#' represent differential costs and benefits compared to the reference
#' intervention (indexed by \code{ref} in the \code{\link{bcea}} function).
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation. The list needs to include the
#' \code{e} and \code{c} matrices used to generate the object; see Details.
#' @param comparators Vector specifying the comparators to be included in the
#' frontier analysis. Must be of length > 1. Default as \code{NULL} includes
#' all the available comparators.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom}, \code{top}, \code{left} or \code{right} for ggplot2. It can
#' be a two-elements vector, which specifies the relative position on the x and
#' y axis respectively, or alternatively it can be in form of a logical
#' variable, with \code{FALSE} indicating to use the default position and
#' \code{TRUE} to place it on the bottom of the plot. Default value is
#' \code{c(1,1)}, that is the topright corner inside the plot area.
#' @param start.from.origins Logical. Should the frontier start from the
#' origins of the axes? The argument is reset to \code{FALSE} if the average
#' effectiveness and/or costs of at least one comparator are negative.
#' @param threshold Specifies if the efficiency should be defined based on a
#' willingness-to-pay threshold value. If set to \code{NULL} (the default), no
#' conditions are included on the slope increase. If a positive value is passed
#' as argument, to be efficient an intervention also requires to have an ICER
#' for the comparison versus the last efficient strategy not greater than the
#' specified threshold value. A negative value will be ignored with a warning.
#' @param flip Logical. Should the axes of the plane be inverted?
#' @param dominance Logical. Should the dominance regions be included in the
#' plot?
#' @param relative Logical. Should the plot display the absolute measures (the
#' default as \code{FALSE}) or the differential outcomes versus the reference
#' comparator?
#' @param print.summary Logical. Should the efficiency frontier summary be
#' printed along with the graph?  See Details for additional information.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param \dots If \code{graph="ggplot2"} and a named theme object is supplied,
#' it will be added to the ggplot object. Ignored if \code{graph="base"}.
#' Setting the optional argument \code{include.ICER} to \code{TRUE} will print
#' the ICERs in the summary tables, if produced.
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if \code{graph="ggplot2"}. } The function produces a plot of the
#' cost-effectiveness efficiency frontier. The dots show the simulated values
#' for the intervention-specific distributions of the effectiveness and costs.
#' The circles indicate the average of each bivariate distribution, with the
#' numbers referring to each included intervention. The numbers inside the
#' circles are black if the intervention is included in the frontier and grey
#' otherwise. If the option \code{dominance} is set to \code{TRUE}, the
#' dominance regions are plotted, indicating the areas of dominance.
#' Interventions in the areas between the dominance region and the frontier are
#' in a situation of extended dominance.
#' @author Andrea Berardi, Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references Baio G. (2012). Bayesian Methods in Health Economics.
#' CRC/Chapman Hall, London.
#' 
#' IQWIG (2009). General methods for the Assessment of the Relation of Benefits
#' to Cost, Version 1.0. IQWIG, November 2009.
#' @keywords Health economic evaluation Multiple comparisons
#' @examples
#' 
#' ### create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e,c,ref=4,Kmax=500,interventions=treats)
#' ### produce the plot
#' ceef.plot(m,graph="base")
#' \donttest{
#' ### tweak the options
#' ceef.plot(m,flip=TRUE,dominance=FALSE,start.from.origins=FALSE,
#'           print.summary=FALSE,graph="base")
#' ### or use ggplot2 instead
#' if(require(ggplot2)){
#' ceef.plot(m,dominance=TRUE,start.from.origins=FALSE,pos=TRUE,
#'           print.summary=FALSE,graph="ggplot2")
#' }
#' }
#' 
#' @export ceef.plot
ceef.plot <- function(he, comparators=NULL, pos=c(1,1), start.from.origins=TRUE, threshold=NULL, flip=FALSE, dominance=TRUE, relative=FALSE, print.summary=TRUE, graph=c("base", "ggplot2"), ...) {
   ### plots the cost-effectiveness efficiency frontier, together with the scatter plot of the simulations and optionally the dominance areas
   if(is.null(he$c) | is.null(he$e)) stop("Please use the bcea() function from BCEA version >=2.1-0 or attach the vectors e and c to the bcea object. Please see ?ceef.plot for additional details.")
   
   ### if threshold is NULL, then bound to pi/2, which is atan(Inf); else iff positive, bound to the increase angle given the slope
   if(is.null(threshold))
      threshold  <- base::pi/2
   else{
      if(threshold<=0){
         warning("The value of the cost-effectiveness threshold should be positive. The argument will be ignored.")
         threshold  <- base::pi/2
      }
      else
         threshold <- atan(threshold)
   }
   # Gives you the possibility of suppressing the plot (if 'print.plot' set to FALSE)
   exArgs=list(...)
   if(exists("print.plot",exArgs)){print.plot=exArgs$print.plot} else {print.plot=TRUE}
   
   ### selects the comparators. No need for recursion
   if(!is.null(comparators)){
      stopifnot(all(comparators %in% 1:he$n.comparators))
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparators]
      he$n.comparators=length(comparators)
      he$n.comparisons=length(comparators)-1
      he$interventions=he$interventions[comparators]
      he$ref=rank(c(he$ref,he$comp))[1]
      he$comp=rank(c(he$ref,he$comp))[-1]
      he$mod <- TRUE #
      ### bceanew
      he$e <- he$e[,comparators]
      he$c <- he$c[,comparators]
   }
   
   ### If the incremental analysis (relative to the reference) is required, needs to modify the BCEA object
   if(relative) {
      temp <- he
      temp$e <- temp$c <- matrix(NA,he$n.sim,he$n.comparators)
      temp$e[,he$ref] <- temp$c[,he$ref] <- rep(0,he$n.sim)
      temp$e[,-he$ref] <- -he$delta.e
      temp$c[,-he$ref] <- -he$delta.c
      he <- temp
   }
   
   stopifnot(he$n.comparators>=2)
   
   base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE)
   
   ### no visible binding note
   c.avg <- e.avg <- x <- y <- e <- e.orig <- c.orig <- NA_real_
   
   ### if the effectiveness is negative or !start.from.origins, rescale
   ec.min <- with(he,c(min(apply(e,2,mean)),
                       apply(c,2,mean)[which.min(apply(e,2,mean))],
                       which.min(apply(e,2,mean))))
   e.neg <- ec.min[1]<0
   c.neg <- any(apply(he$c,2,mean)<0)
   
   if(e.neg & !c.neg & start.from.origins){
      message("Benefits are negative, the frontier will not start from the origins")
      start.from.origins <- FALSE
   }
   if(!e.neg & c.neg & start.from.origins){
      message("Costs are negative, the frontier will not start from the origins")
      start.from.origins <- FALSE
   }
   if(e.neg & c.neg & start.from.origins){
      message("Costs and benefits are negative, the frontier will not start from the origins")
      start.from.origins <- FALSE
   }
   e.neg <- ifelse(start.from.origins,e.neg,TRUE)
   
   ### frontier calculation
   data.avg <- data.frame(
      "e.avg"=apply(he$e,2,mean)-ifelse(!e.neg,0,ec.min[1]),
      "c.avg"=apply(he$c,2,mean)-ifelse(!e.neg,0,ec.min[2]))
   data.avg <- cbind(data.avg,data.avg,as.factor(c(1:dim(data.avg)[1])))
   names(data.avg)[3:5] <- c("e.orig","c.orig","comp")
   orig.avg <- data.avg[,3:5]
   ### check for interventions with zero costs and effectiveness
   comp <- ifelse(any(apply(data.avg[,1:2],1,function(x) isTRUE(sum(x) == 0 & prod(x) == 0))),
                  which(apply(data.avg[,1:2],1,sum)==0 & apply(data.avg[,1:2],1,prod)==0),0)
   ### contains the points connecting the frontier. Always starts from the origins
   ceef.points <- data.frame(
      "x"=0,
      "y"=0,
      "comp"=comp)
   repeat{
      if(prod(dim(data.avg))==0) break
      theta <- with(data.avg,atan(c.avg/e.avg))
      theta.min <- min(theta,na.rm=TRUE)
      if(theta.min>threshold) break
      index <- which(theta==theta.min)
      if(length(index)>1)
         index=index[which.min(data.avg$e.avg[index])]
      ceef.points <- with(data.avg,rbind(ceef.points,c(e.orig[index],c.orig[index],comp[index])))
      data.avg[,1:2] <- data.avg[,3:4]-matrix(rep(as.numeric(data.avg[index,3:4]),dim(data.avg)[1]),ncol=2,byrow=TRUE)
      data.avg <- subset(subset(data.avg,c.avg*e.avg>0),c.avg+e.avg>0)
   }
   ceef.points$comp <- factor(ceef.points$comp)
   
   ceef.points$slope <- NA
   ### calculate slopes
   for(i in 2:dim(ceef.points)[1])
      ceef.points$slope[i] <- with(ceef.points,(y[i]-y[i-1])/(x[i]-x[i-1]))
   
   ### workaround for start.from.origins == FALSE: remove first row if slope is negative
   while(dim(ceef.points)[1]>1 & ceef.points$slope[2]<0){
      ceef.points <- ceef.points[-1,]
      ceef.points$slope[1] <- NA
   }
   
   ### set data.frame for points
   scatter.data <- data.frame(
      "e"=c(he$e),#-ifelse(!e.neg,0,ec.min[1]),
      "c"=c(he$c),#-ifelse(!e.neg,0,ec.min[2]),
      "comp"=as.factor(sort(rep(1:he$n.comparators,he$n.sim))))
   
   ### re-adjustment of data sets
   ceef.points[,1] <- ceef.points[,1]+ifelse(!e.neg,0,ec.min[1])
   ceef.points[,2] <- ceef.points[,2]+ifelse(!e.neg,0,ec.min[2])
   orig.avg[,1] <- orig.avg[,1]+ifelse(!e.neg,0,ec.min[1])
   orig.avg[,2] <- orig.avg[,2]+ifelse(!e.neg,0,ec.min[2])
   
   ### Summary table function
   ceef.summary <- function(he,ceef.points,orig.avg,include.ICER=FALSE,...){
      ## Tables adaptation and formatting
      no.ceef <- which(!1:he$n.comparators %in% ceef.points$comp)
      ## Interventions included
      if(ceef.points$comp[1]==0)
         ceef.points <- ceef.points[-1,]
      rownames(ceef.points) <- he$interventions[as.numeric(levels(ceef.points$comp)[ceef.points$comp])]
      
      if(!include.ICER){
         ceef.points[,5] <- atan(ceef.points[,4]^(1*ifelse(!flip,1,-1)))
         ceef.points <- ceef.points[,-3]
         colnames(ceef.points) <- c("Effectiveness","Costs","Increase slope","Increase angle")
      }
      else{
         ICERs <- numeric(dim(ceef.points)[1])
         index <- as.numeric(levels(ceef.points$comp)[ceef.points$comp])
         for(i in 1:length(ICERs)){
            if(ceef.points$comp[i]==he$ref)
               ICERs[i] <- NA_real_
            else
               ICERs[i] <- he$ICER[index[i]+ifelse(index[i]<he$ref,0,-1)]
         }
         ceef.points[,3] <- ICERs
         ceef.points[,5] <- atan(ceef.points[,4]^(1*ifelse(!flip,1,-1)))
         colnames(ceef.points) <- c("Effectiveness","Costs",paste0("ICER ",he$interventions[he$ref]," vs."),"Increase slope","Increase angle")
      }
      if(flip) colnames(ceef.points)[1:2] <- colnames(ceef.points[2:1])
      
      ## Interventions not included
      if(length(no.ceef)>0){
         noceef.points <- data.frame(matrix(NA_real_,ncol=4,nrow=length(no.ceef)))
         noceef.points[,1:2] <- orig.avg[no.ceef,-3]
         
         if(!include.ICER){
            noceef.points <- noceef.points[,-3]
            colnames(noceef.points) <- c("Effectiveness","Costs","Dominance type")
         }
         else{
            ICERs <- numeric(dim(noceef.points)[1])
            for(i in 1:length(ICERs)){
               if(no.ceef[i]==he$ref)
                  ICERs[i] <- NA_real_
               else
                  ICERs[i] <- he$ICER[no.ceef[i]+ifelse(no.ceef[i]<he$ref,0,-1)]
            }
            noceef.points[,3] <- ICERs
            colnames(noceef.points) <- c("Effectiveness","Costs",paste0("ICER ",he$interventions[he$ref]," vs."),"Dominance type")
         }
         
         how.dominated <- rep("Extended dominance",length(no.ceef))
         for(i in 1:length(no.ceef))
            for(j in 1:dim(ceef.points)[1]){
               ### if the product of the deltas is negative it is dominated: cannot be dominant since not on the frontier 
               if((noceef.points[i,1]-ceef.points[j,1])*(noceef.points[i,2]-ceef.points[j,2])<0){
                  how.dominated[i] <- "Absolute dominance"
                  ### alternative:
                  # how.dominated[i] <- paste0("Dominated by ",rownames(ceef.points)[j])
                  break
               }
            }
         noceef.points[,ifelse(!include.ICER,3,4)] <- how.dominated
         rownames(noceef.points) <- he$interventions[no.ceef]
         if(flip) colnames(noceef.points)[1:2] <- colnames(noceef.points)[2:1]
      }
      
      ### Print the summary table
      cat("\nCost-effectiveness efficiency frontier summary \n\n")
      cat("Interventions on the efficiency frontier:\n")
      print(ceef.points,quote=F,digits=5,justify="center")
      cat("\n")
      if(length(no.ceef)>0){
         cat("Interventions not on the efficiency frontier:\n")
         print(noceef.points,quote=F,digits=5,justify="center")
      }
   }
   if(print.summary)
      ceef.summary(he,ceef.points,orig.avg,...)
   
   ### plots
   ### colours
   colour <- colours()[floor(seq(262,340,length.out=he$n.comparators))]  # gray scale
   ##### ***** base graphics ***** #####
   if(base.graphics){
      if(print.plot){
         ### legend positioning
         if(is.numeric(pos)&length(pos)==2){
            temp <- ""
            if(pos[2]==0)
               temp <- paste0(temp,"bottom")
            else
               temp <- paste0(temp,"top")
            if(pos[1]==0)
               temp <- paste0(temp,"left")
            else
               temp <- paste0(temp,"right")
            pos <- temp
            if(length(grep("^(bottom|top)(left|right)$",temp))==0)
               pos <- FALSE
         }
         if(is.logical(pos)){
            if(!pos)
               pos="topright"
            else
               pos="topleft"
         }
         
         if(flip){
            temp <- scatter.data$e
            scatter.data$e <- scatter.data$c
            scatter.data$c <- temp
            
            temp <- ceef.points$x
            ceef.points$x <- ceef.points$y
            ceef.points$y <- temp
            
            temp <- orig.avg$e.orig
            orig.avg$e.orig <- orig.avg$c.orig
            orig.avg$c.orig <- temp
            
            rm(temp)
         }
         
         ### set up plot window
         xlab=ifelse((!flip & !relative),"Effectiveness",
                     ifelse((!flip & relative),"Differential effectiveness",
                            ifelse((flip & !relative),"Cost","Differential cost")))
         ylab=ifelse((!flip & !relative),"Cost",
                     ifelse((!flip & relative),"Differential cost",
                            ifelse((flip & !relative),"Effectiveness","Differential effectiveness")))
         plot(NULL,
              xlim=c(min(range(scatter.data$e)[1],0),max(range(scatter.data$e)[2],0)),
              ylim=c(min(range(scatter.data$c)[1],0),max(range(scatter.data$c)[2],0)),
              main="Cost-effectiveness efficiency frontier",
              xlab=xlab,ylab=ylab)
         
         if(dominance){
            ### add dominance regions
            for(i in 1:dim(ceef.points)[1]){
               rect(col="grey95",border=NA,
                    xleft=ifelse(!flip,-1,1)*2*max(abs(range(scatter.data$e))),xright=ceef.points$x[i],
                    ybottom=ceef.points$y[i],ytop=ifelse(!flip,1,-1)*2*max(abs(range(scatter.data$c))))
            }
            if(dim(ceef.points)[1]>1)
               for(i in 2:dim(ceef.points)[1]){
                  rect(col="grey85",border=NA,
                       xleft=ifelse(!flip,-1,1)*2*max(abs(range(scatter.data$e))),xright=ceef.points$x[ifelse(!flip,i-1,i)],
                       ybottom=ceef.points$y[ifelse(!flip,i,i-1)],ytop=ifelse(!flip,1,-1)*2*max(abs(range(scatter.data$c))))
               }
         }
         
         ### plot the axes
         abline(h=0,col="grey")
         abline(v=0,col="grey")
         
         ### plot the scatter
         for(i in 1:he$n.comparators)
            with(scatter.data,points(subset(scatter.data,comp==i)[,-3],type="p",pch=20,cex=.35,col=colour[i]))
         
         ### plot the frontier
         points(ceef.points[,1:2],type="l",lwd=2)
         ### add circles
         points(orig.avg[,-3],pch=21,cex=2,bg="white",col="black")
         ### add text; grey if not on the frontier
         for(i in 1:he$n.comparators){
            text(orig.avg[i,-3],labels=orig.avg[i,3],col=ifelse(i %in% ceef.points$comp,"black","grey60"),cex=.75)
         }
         ### legend text
         text <- paste(1:he$n.comparators,":",he$interventions)
         legend(pos,text,col=colour,cex=.7,bty="n",lty=1)
         
         ### needed for dominance areas overwriting the outer box
         box()
      }
   }
   ##### ***** ggplot2 ***** #####
   else{
      if(print.plot){
         if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))){
            message("Falling back to base graphics\n")
            ceef.plot(he,flip=flip,comparators=comparators,pos=pos,start.from.origins=start.from.origins,graph="base")
            return(invisible(NULL))
         }
         
         opt.theme <- ggplot2::theme()
         exArgs <- list(...)
         if(length(exArgs)>=1){
            for(obj in exArgs)
               if(ggplot2::is.theme(obj))
                  opt.theme <- opt.theme + obj
         }
         
         ceplane <- ggplot2::ggplot(ceef.points,ggplot2::aes(x=x,y=y))
         
         if(dominance){
            ### add dominance regions
            ceplane <- ceplane +
               ggplot2::geom_rect(data=ceef.points,ggplot2::aes(xmax=x,ymin=y),
                                  ymax=2*max(abs(range(scatter.data$c))),xmin=-2*max(abs(range(scatter.data$e))),
                                  alpha=.35,fill="grey75")
         }
         
         ceplane <- ceplane +
            ### draw axes
            ggplot2::geom_hline(yintercept=0,colour="grey")+ggplot2::geom_vline(xintercept=0,colour="grey")+
            ### add scatter points
            ggplot2::geom_point(data=scatter.data,ggplot2::aes(x=e,y=c,colour=comp),size=1)
         ### add frontier
         if(dim(ceef.points)[1]>1)
            ceplane <- ceplane + ggplot2::geom_path()
         ### add circles
         xlab=ifelse(!relative,"Effectiveness","Effectiveness differential")
         ylab=ifelse(!relative,"Cost","Cost differential")
         ceplane <- ceplane +
            ggplot2::geom_point(data=orig.avg,ggplot2::aes(x=e.orig,y=c.orig),size=5.5,colour="black")+
            ggplot2::geom_point(data=orig.avg,ggplot2::aes(x=e.orig,y=c.orig),size=4.5,colour="white")+
            ### set graphical parameters
            ggplot2::scale_colour_manual("",labels=paste0(1:he$n.comparators,": ",he$interventions),values=colour,na.value="black")+
            ggplot2::labs(title="Cost-effectiveness efficiency frontier",x=xlab,y=ylab)+
            ggplot2::theme_bw()
         ### add text into circles
         for(i in 1:he$n.comparators){
            ceplane <- ceplane + 
               ggplot2::geom_text(data=orig.avg[i,],ggplot2::aes(x=e.orig,y=c.orig,label=comp),size=3.5,
                                  colour=ifelse(i %in% ceef.points$comp, "black", "grey60"))
         }
         
         jus <- NULL
         if(isTRUE(pos)) {
            pos="bottom"
            ceplane <- ceplane + ggplot2::theme(legend.direction="vertical")
         }
         else{
            if(is.character(pos)) {
               choices <- c("left", "right", "bottom", "top")
               pos <- choices[pmatch(pos,choices)]
               jus="center"
               if(is.na(pos))
                  pos=FALSE
            }
            if(length(pos)>1)
               jus <- pos
            if(length(pos)==1 & !is.character(pos)) {
               pos <- c(1,1)
               jus <- pos
            }
         }
         
         ceplane <- ceplane + 
            ggplot2::theme(legend.position=pos,legend.justification=jus,legend.title=ggplot2::element_blank(),
                           legend.background=ggplot2::element_blank(),text=ggplot2::element_text(size=11),
                           legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),
                           panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank(),legend.text.align=0,
                           plot.title = ggplot2::element_text(hjust=0.5,face="bold",lineheight=1.05,size=14.3)) +
            opt.theme
         
         if(flip) ceplane  <- ceplane + ggplot2::coord_flip()
         print(ceplane)
      }
   }
}

