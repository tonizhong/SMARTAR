#' @importFrom graphics barplot boxplot title par text
#seqplot() output the plot for sequence-specific descriptive statistics

seqplots<-function(data,family="gaussian",color,title,ylab,
                   xlab,xtext,reference){
        D<-as.data.frame(data)
        if (is.null(D$O1)) {Base<-0} else {Base<-1}

        N<-nrow(D)
        Nstage<-nstage(data=D)
        Smat<-seqscan(data=D)
        ns<-nrow(Smat)

        SEQ<-rep(NA,N)
        #assign sequence index for each subject in input dataset
        D<-cbind(D,SEQ)

        if (Nstage==1 && Base==0) {
          for (j in seq(ns)) {
            D$SEQ[which(D$A1==Smat[j,2])]<-j}
            lab<-paste("(",Smat$A1,")")
            ti<-"Primary outcome by sequence (A1)"
            oldbar<-par(mar=c(4,4,4,2))
            on.exit(par(oldbar))
            if (is.null(xtext)) {lab<-lab} else {lab<-xtext}
            if (is.null(ylab)) {ylab<-"Primary Outcome"} else {ylab<-ylab}
            if (family=="binomial") {
              counts<-table(D$Y,D$SEQ)
              range<-c(0,max(table(D$SEQ))*3/2)
              par(mar=c(6.4,5,4,4))
              barplot(counts,col=color,
                      legend=rownames(counts),
                      ylim=range,horiz = FALSE,xlab="",ylab="",
                      cex.axis=0.8,
                      names.arg=lab,las=2)
              if (is.null(title)) {ti<-ti} else {ti<-title}
              title(main = ti,ylab = ylab)
              title(xlab=xlab,cex=0.8,line = 2.5)
            } else
              if (family=="gaussian") {
                mean_y<-mean(D$Y)
                ran<-max(D$Y)-min(D$Y)
                range<-c(min(D$Y)-ran/4,max(D$Y)+ran/4)
                par(mar=c(6.4,5,4,4))
                boxplot(D$Y~D$SEQ,horizontal=FALSE,
                        names=lab,las=2,pch=19,cex=0.8,ylim=range,
                        col=color[1],cex.axis=0.8,xlab="",ylab="")
                if (is.null(title)) {ti<-ti} else {ti<-title}
                title(main = ti,ylab=ylab)
                title(xlab=xlab,cex=0.8,line = 2.5)
                if(reference==TRUE){abline(h=mean_y,lty=3)}
              } else
                stop("invalid enter for argument 'family'")
            } else
        if (Nstage==1 && Base==1) {
          for (j in seq(ns)) {
            D$SEQ[which(D$O1==Smat[j,2] &
        D$A1==Smat[j,3])]<-j}
        lab<-paste("(",Smat$O1,Smat$A1,")")
        ti<-"Primary outcome by sequence (O1,A1)"
        oldbar<-par(mar=c(4,4,4,2))
        on.exit(par(oldbar))
        if (is.null(xtext)) {lab<-lab} else {lab<-xtext}
        if (is.null(ylab)) {ylab<-"Primary Outcome"} else {ylab<-ylab}
        if (family=="binomial") {
          counts<-table(D$Y,D$SEQ)
          range<-c(0,max(table(D$SEQ))*3/2)
          par(mar=c(6.4,5,4,4))
          barplot(counts,col=color,
                  legend=rownames(counts),
                  ylim=range,horiz = FALSE,xlab="",ylab="",cex.axis=0.8,
                  names.arg=lab,las=2)
          if (is.null(title)) {ti<-ti} else {ti<-title}
          title(main = ti,ylab = ylab)
          title(xlab=xlab,cex=0.8,line = 3)
        } else
          if (family=="gaussian") {
            mean_y<-mean(D$Y)
            ran<-max(D$Y)-min(D$Y)
            range<-c(min(D$Y)-ran/4,max(D$Y)+ran/4)
            par(mar=c(6.4,5,4,4))
            boxplot(D$Y~D$SEQ,horizontal=FALSE,
                    names=lab,las=2,pch=19,cex=0.8,ylim=range,col=color[1],
                    cex.axis=0.8,xlab="",ylab="")
            if (is.null(title)) {ti<-ti} else {ti<-title}
            title(main = ti,ylab=ylab)
            title(xlab=xlab,cex=0.8,line = 3)
            if(reference==TRUE){abline(h=mean_y,lty=3)}
          }else
            stop("invalid enter for argument 'family'")
        }
        else
        if (Nstage==2 && Base==0) {
          for (j in seq(ns)) {
            D$SEQ[which(D$A1==Smat[j,2] & D$O2==Smat[j,3] &
                          D$A2==Smat[j,4])]<-j}
            lab<-paste("(",Smat$A1,Smat$O2,Smat$A2,")")
            ti<-"Primary outcome by treatment sequence (A1,O2,A2)"
            oldbar<-par(mar=c(4,4,4,2))
            on.exit(par(oldbar))
            if (is.null(xtext)) {lab<-lab} else {lab<-xtext}
            if (is.null(ylab)) {ylab<-"Primary Outcome"} else {ylab<-ylab}
            if (family=="binomial") {
              counts<-table(D$Y,D$SEQ)
              range<-c(0,max(table(D$SEQ))*3/2)
              par(mar=c(6.4,5,4,4))
              barplot(counts,col=color,
                      legend=rownames(counts),
                      ylim=range,horiz = FALSE,xlab="",ylab="",
                      cex.axis=0.8,
                      names.arg=lab,las=2)
              if (is.null(title)) {ti<-ti} else {ti<-title}
              title(main = ti,ylab = ylab)
              title(xlab=xlab,cex=0.8,line = 4)
            } else
              if (family=="gaussian") {
                mean_y<-mean(D$Y)
                ran<-max(D$Y)-min(D$Y)
                range<-c(min(D$Y)-ran/4,max(D$Y)+ran/4)
                par(mar=c(6.4,5,4,4))
                boxplot(D$Y~D$SEQ,horizontal=FALSE,
                        names=lab,las=2,pch=19,cex=0.8,ylim=range,col=color[1],
                        cex.axis=0.8,xlab="",ylab="")
                if (is.null(title)) {ti<-ti} else {ti<-title}
                title(main = ti,ylab=ylab)
                title(xlab=xlab,cex=0.8,line = 4)
                if(reference==TRUE){abline(h=mean_y,lty=3)}
              }else
                stop("invalid enter for argument 'family'")

            } else
        if (Nstage==2 && Base==1) {
          for (j in seq(ns)) {
            D$SEQ[which(D$O1==Smat[j,2] & D$A1==Smat[j,3] &
                          D$O2==Smat[j,4] & D$A2==Smat[j,5])]<-j}
            lab<-paste("(",Smat$O1,Smat$A1,Smat$O2,Smat$A2,")")
            ti<-"Primary outcome by treatment sequence (O1,A1,O2,A2)"
            oldbar<-par(mar=c(4,4,4,2))
            on.exit(par(oldbar))
            if (is.null(xtext)) {lab<-lab} else {lab<-xtext}
            if (is.null(ylab)) {ylab<-"Primary Outcome"} else {ylab<-ylab}
            if (family=="binomial") {
              counts<-table(D$Y,D$SEQ)
              range<-c(0,max(table(D$SEQ))*3/2)
              par(mar=c(6.4,5,4,4))
              barplot(counts,col=color,
                      legend=rownames(counts),
                      ylim=range,horiz = FALSE,xlab="",ylab="",
                      cex.axis=0.8,
                      names.arg=lab,las=2)
              if (is.null(title)) {ti<-ti} else {ti<-title}
              title(main = ti,ylab = ylab)
              title(xlab=xlab,cex=0.8,line = 4)
            } else
              if (family=="gaussian") {
                mean_y<-mean(D$Y)
                ran<-max(D$Y)-min(D$Y)
                range<-c(min(D$Y)-ran/4,max(D$Y)+ran/4)
                par(mar=c(6.4,5,4,4))
                boxplot(D$Y~D$SEQ,horizontal=FALSE,
                        names=lab,las=2,pch=19,cex=0.8,ylim=range,col=color[1],
                        cex.axis=0.8,xlab="",ylab="")
                if (is.null(title)) {ti<-ti} else {ti<-title}
                title(main = ti,ylab=ylab)
                title(xlab=xlab,cex=0.8,line = 4)
                if(reference==TRUE){abline(h=mean_y,lty=3)}
              }else
                stop("invalid enter for argument 'family'")

            } else
        if (Nstage==3 && Base==0) {
          for (j in seq(ns)) {
            D$SEQ[which(D$A1==Smat[j,2] & D$O2==Smat[j,3] &
                          D$A2==Smat[j,4] & D$O3==Smat[j,5] &
                          D$A3==Smat[j,6])]<-j}
            lab<-paste("(",Smat$A1,Smat$O2,Smat$A2,Smat$O3,Smat$A3,")")
            ti<-"Primary outcome by treatment sequence (A1,O2,A2,O2,A3)"
            oldbar<-par(mar=c(4,4,4,2))
            on.exit(par(oldbar))
            if (is.null(xtext)) {lab<-lab} else {lab<-xtext}
            if (is.null(ylab)) {ylab<-"Primary Outcome"} else {ylab<-ylab}
            if (family=="binomial") {
              counts<-table(D$Y,D$SEQ)
              range<-c(0,max(table(D$SEQ))*3/2)
              par(mar=c(6.4,5,4,4))
              barplot(counts,col=color,
                      legend=rownames(counts),
                      ylim=range,horiz = FALSE,xlab="",ylab="",
                      cex.axis=0.8,
                      names.arg=lab,las=2)
              if (is.null(title)) {ti<-ti} else {ti<-title}
              title(main = ti,ylab = ylab)
              title(xlab=xlab,cex=0.8,line = 5)
            } else
              if (family=="gaussian") {
                mean_y<-mean(D$Y)
                ran<-max(D$Y)-min(D$Y)
                range<-c(min(D$Y)-ran/4,max(D$Y)+ran/4)
                par(mar=c(6.4,5,4,4))
                boxplot(D$Y~D$SEQ,horizontal=FALSE,
                        names=lab,las=2,pch=19,cex=0.8,ylim=range,col=color[1],
                        cex.axis=0.8,xlab="",ylab="")
                if (is.null(title)) {ti<-ti} else {ti<-title}
                title(main = ti,ylab=ylab)
                title(xlab=xlab,cex=0.8,line = 5)
                if(reference==TRUE){abline(h=mean_y,lty=3)}
              }else
                stop("invalid enter for argument 'family'")

            } 
 

}
