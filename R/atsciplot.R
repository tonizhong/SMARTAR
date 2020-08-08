#' @importFrom graphics par plot title axis lines

#atsciplot() plot the PEs and (1-a)% CIs of all strategy values
atsciplot<-function(uimat,
                   col,title,nstage,baseline=0,
                   lim=NULL,cex,lwd,ylab=ylab,xlab=xlab,
                   pch,xtext=xtext,mar,cex.axis,line){
    UImat<-uimat
    Nstage<-nstage
    Base<-baseline
    COL<-col
    L<-lim
    CEX<-cex
    LWD<-lwd
    ti<-title
    G<-nrow(uimat)

  if (Nstage==1 && Base==0) {
    lab<-paste("(",uimat$d0,")")
    range<-max(uimat$upper)-min(uimat$lower)
    mi<-min(uimat$lower)-range/2
    ma<-max(uimat$upper)+range/2
    if (is.null(L)) {L<-c(mi,ma)}
    else {L<-lim}
    if (is.null(xtext)) {lab<-lab}
    else {lab<-xtext}
    if (is.null(mar)) {mar<-c(6.4,5,4,4)}
    else{mar<-mar}
    if (is.null(line)) {line<-2.5}
    else{line<-line}
    par(mar=mar)
    plot(x=1:G,y=uimat$value,xlim=c(0,G+1),
         pch=pch,col=COL,cex=CEX,xaxt="n",xlab="",ylab="",ylim=L)
    title(xlab=xlab,line = line)
    title(main=ti,
          ylab=ylab)
    axis(side=1,1:G,labels=lab,las=2,cex.axis=cex.axis)
    CI<-as.matrix(cbind(UImat$lower,UImat$upper))
    for (i in 1:G){lines(x=c(i,i),y=c(CI[i,1],CI[i,2]),lwd=LWD,col=COL)}
    }
  if (Nstage==1 && Base==1) {
    lab<-paste("(",uimat$d0,uimat$d1,")")
    range<-max(uimat$upper)-min(uimat$lower)
    mi<-min(uimat$lower)-range/2
    ma<-max(uimat$upper)+range/2
    
    if (is.null(L)) {L<-c(mi,ma)} 
    else {L<-lim}
    if (is.null(xtext)) {lab<-lab} 
    else {lab<-xtext}
    if (is.null(mar)) {mar<-c(6.4,5,4,4)}
    else{mar<-mar}
    if (is.null(line)) {line<-3}
    else{line<-line}
    par(mar=mar)
    plot(x=1:G,y=uimat$value,xlim=c(0,G+1),
         pch=pch,col=COL,cex=CEX,xaxt="n",xlab="",ylab="",ylim=L)
    title(xlab=xlab,line = line)
    title(main=ti,
          ylab=ylab)
    axis(side=1,1:G,labels=lab,las=2,cex.axis=cex.axis)
    CI<-as.matrix(cbind(UImat$lower,UImat$upper))
    for (i in 1:G){lines(x=c(i,i),y=c(CI[i,1],CI[i,2]),lwd=LWD,col=COL)}
    }
  if (Nstage==2 && Base==0) {
    lab<-paste("(",uimat$d0,
                                          uimat$d00,uimat$d01,")")
    range<-max(uimat$upper)-min(uimat$lower)
    mi<-min(uimat$lower)-range/2
    ma<-max(uimat$upper)+range/2
    
    if (is.null(L)) {L<-c(mi,ma)}
    else {L<-lim}
    if (is.null(xtext)) {lab<-lab} 
    else {lab<-xtext}
    if (is.null(mar)) {mar<-c(6.4,5,4,4)}
    else{mar<-mar}
    if (is.null(line)) {line<-4}
    else{line<-line}
    par(mar=mar)
    plot(x=1:G,y=uimat$value,xlim=c(0,G+1),
         pch=pch,col=COL,cex=CEX,xaxt="n",xlab="",ylab="",ylim=L)
    title(xlab=xlab,line = line)
    title(main=ti,
          ylab=ylab)
    axis(side=1,1:G,labels=lab,las=2,cex.axis=cex.axis)
    CI<-as.matrix(cbind(UImat$lower,UImat$upper))
    for (i in 1:G){lines(x=c(i,i),y=c(CI[i,1],CI[i,2]),lwd=LWD,col=COL)}
    }
  if (Nstage==2 && Base==1) {
    lab<-paste("(",uimat$d0, uimat$d1,
    uimat$d00,uimat$d01,uimat$d10,uimat$d11,")")
    range<-max(uimat$upper)-min(uimat$lower)
    mi<-min(uimat$lower)-range/2
    ma<-max(uimat$upper)+range/2
    
    if (is.null(L)) {L<-c(mi,ma)} 
    else {L<-lim}
    if (is.null(xtext)) {lab<-lab}
    else {lab<-xtext}
    if (is.null(mar)) {mar<-c(6.4,5,4,4)}
    else{mar<-mar}
    if (is.null(line)) {line<-4}
    else{line<-line}
    par(mar=mar)
    plot(x=1:G,y=uimat$value,xlim=c(0,G+1),
         pch=pch,col=COL,cex=CEX,xaxt="n",xlab="",ylab="",ylim=L)
    title(xlab=xlab,line = line)
    title(main=ti,
          ylab=ylab)
    axis(side=1,1:G,labels=lab,las=2,cex.axis=cex.axis)
    CI<-as.matrix(cbind(UImat$lower,UImat$upper))
    for (i in 1:G){lines(x=c(i,i),y=c(CI[i,1],CI[i,2]),lwd=LWD,col=COL)}
    }
  if (Nstage==3 && Base==0) {
    lab<-paste("(",uimat$d0,
    uimat$d00,uimat$d01,
    uimat$d000,uimat$d001,uimat$d010,uimat$d011,")")
    range<-max(uimat$upper)-min(uimat$lower)
    mi<-min(uimat$lower)-range/2
    ma<-max(uimat$upper)+range/2
    
    if (is.null(L)) {L<-c(mi,ma)} 
    else {L<-lim}
    if (is.null(xtext)) {lab<-lab} 
    else {lab<-xtext}
    if (is.null(mar)) {mar<-c(8,5,4,4)}
    else{mar<-mar}
    if (is.null(line)) {line<-7}
    else{line<-line}
    par(mar=mar)
    plot(x=1:G,y=uimat$value,xlim=c(0,G+1),
         pch=pch,col=COL,cex=CEX,xaxt="n",xlab="",ylab="",ylim=L)
    title(xlab=xlab,line = line)
    title(main=ti,
          ylab=ylab)
    axis(side=1,1:G,labels=lab,las=2,cex.axis=cex.axis)
    CI<-as.matrix(cbind(UImat$lower,UImat$upper))
    for (i in 1:G){lines(x=c(i,i),y=c(CI[i,1],CI[i,2]),lwd=LWD,col=COL)}
    }
    

 
}
