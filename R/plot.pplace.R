plot.pplace <-
function(x,type="precise",simplify=FALSE,main="",N=NULL,transfo=NULL,legend=TRUE,stl=FALSE,asb=FALSE,edge.width=1,max_width=10,cex.number=0.5,cex.text=0.8,transp=80,add=FALSE,color=NULL,discrete_col=FALSE,pch=16,run_id=NULL,...){

  if(!is.null(run_id)){
    x <- sub_pplace(x,run_id=run_id)
  }

  if(is.null(N)){
    x$multiclass$N <- 1
  }
  if(!is.null(N)){
    x$multiclass$N <- N
  }
  
  if(simplify){
    x$placement_positions <- x$placement_positions[order(x$placement_positions$ml_ratio,decreasing=TRUE),]
    x$placement_positions <- x$placement_positions[match(unique(x$placement_positions$placement_id),x$placement_positions$placement_id),]
    x$placement_positions$ml_ratio <- 1
  }
  
  if(type!="precise"){
    placement_N <- aggregate(x$multiclass$N,list(x$multiclass$placement_id),sum)
    br_sum <- aggregate(placement_N[match(x$placement_positions$placement_id,placement_N[,1]),2]*x$placement_positions$ml_ratio,list(branch=x$placement_positions$location),sum)

    if(type=="number"){
      plot(x$arbre,edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
      text(0,0,main,cex=cex.text,pos=4)
      edgelabels(round(br_sum[,2]),br_sum[,1],cex=cex.number)
      if(asb) add.scale.bar()
    }
    
    if(type=="fattree"){
      vwidth <- rep(0.1,nrow(x$arbre$edge))
      vwidth[br_sum[,1]] <- ceiling(max_width*br_sum[,2]/max(br_sum[,2]))
      plot(x$arbre,edge.width=vwidth,show.tip.label=stl,no.margin=TRUE)
      text(0,0,main,cex=cex.text,pos=4)
      if(asb) add.scale.bar()
    }

    if(type=="color"){
      col_palette=rgb(colorRamp(c("blue","green","yellow","red"))(seq(0,1,length=100)), maxColorValue = 255)
      vcol <- rep("grey",nrow(x$arbre$edge))
	  coln <- ceiling(100*br_sum[,2]/max(br_sum[,2]))
	  coln[coln>100] <- 100
      vcol[br_sum[,1]] <- col_palette[coln]
      if(!legend){
	plot(x$arbre,edge.color=vcol,edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
	text(0,0,main,cex=cex.text,pos=4)
	if(asb) add.scale.bar()
      }
     if(legend){
	layout(matrix(c(rep(1,6),2),ncol=1))
	plot(x$arbre,edge.color=vcol,edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
	text(0,0,main,cex=cex.text,pos=4)
	if(asb) add.scale.bar()
	par(mar=c(3,15,3,15))
	image(1:length(col_palette),1:1,matrix(1:length(col_palette),ncol=1),col=col_palette,xaxt="n",yaxt="n",xlab="",ylab="")
	axis(3,at=c(1,length(col_palette)),labels=c(0,round(max(br_sum[,2]),0)))
	text(0,0,main,cex=cex.text,pos=4)
      }
    }
  }

  if(type=="precise"){
    if(legend) layout(matrix(c(rep(1,6),2,rep(1,6),3),ncol=2))
    if(!add){
      plot(x$arbre,edge.color="black",edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
      text(0,0,main,cex=cex.text,pos=4)
    }
    if(asb) add.scale.bar()
    pos_phylo <- get_edge()
    xpos <- pos_phylo[x$placement_positions$location,1] + x$placement_positions$distal_bl
    ypos <- pos_phylo[x$placement_positions$location,2]
    
    if(is.null(color)) color <- c("blue","green","yellow","red")
    
    col_palette <- rgb(colorRamp(color)(seq(0,1,length=1000)), maxColorValue = 255)

    if(discrete_col){
      col_palette <- rep(NA,1000)
      color <- col2rgb(color)
      color <- apply(color,2,function(X){rgb(X[1],X[2],X[3], maxColorValue=255)})
      pos_col <- round(seq(1,1000,length.out=length(color)+1))
      for(i in 1:length(color)){
	col_palette[pos_col[i]:pos_col[i+1]] <- color[i]
      }
    }
    vcol <- col_palette[ceiling((x$placement_positions$pendant_bl/max(x$placement_positions$pendant_bl))*1000)]
    
    placement_N <- aggregate(x$multiclass$N,list(x$multiclass$placement_id),sum)
    pos_id <- match(placement_N[,1],x$placement_positions$placement_id)
    cex_placement <- placement_N[match(x$placement_positions$placement_id,placement_N[,1]),2]*x$placement_positions$ml_ratio
    cex_placement[is.na(cex_placement)] <- 0

	if(!is.null(transfo)) cex_placement <- transfo(cex_placement)
      
    order_ploting <- order(cex_placement,decreasing=TRUE)
    points(xpos[order_ploting],ypos[order_ploting],col=paste(vcol[order_ploting],transp,sep=""),cex=cex_placement[order_ploting],pch=pch)
    
    if(legend){
	par(mar=c(3,7,3,3.5))
	image(1:length(col_palette),1:1,matrix(1:length(col_palette),ncol=1),col=col_palette,xaxt="n",yaxt="n",xlab="",ylab="",main="pendant branch length",cex.main=1,font.main=1)
	axis(3,at=c(1,length(col_palette)),labels=c(0,round(max(x$placement_positions$pendant_bl),2)))
	par(mar=c(1,3.5,1,7))
	plot.new()
	cex_legend <- seq(0,ceiling(max(cex_placement)),length.out=5)
	points(seq(0.1,0.9,by=0.2),y=rep(0.3,5),pch=16,cex=cex_legend)
	text(seq(0.1,0.9,by=0.2),y=rep(0.5,5),cex_legend,pos=3)
	text(0.5,0.9,"placement size",cex=1)
    }
  }
}

plot.jplace <- function(x,...){
	if(ncol(x$placement_positions)==7) colnames(x$placement_positions)[1:7] <- c("placement_id","location","ml_ratio","log_like","distal_bl","pendant_bl","tax_id")
	if(ncol(x$placement_positions)==6) colnames(x$placement_positions)[1:6] <- c("placement_id","location","ml_ratio","log_like","distal_bl","pendant_bl")	
	plot.pplace(x,...)
}
