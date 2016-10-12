pplace_tree <-
function(pplace,type="precise",main="",N=NULL,transfo=NULL,legend=TRUE,stl=FALSE,asb=FALSE,edge.width=1,cex.number=0.5,cex.text=0.8,transp=80){
  if(is.null(N)){
    pplace$multiclass$N <- 1
  }
  if(!is.null(N)){
    pplace$multiclass$N <- N
  }
  if(type!="precise"){
    placement_N <- aggregate(pplace$multiclass$N,list(pplace$multiclass$placement_id),sum)
    br_sum <- aggregate(placement_N[match(pplace$placement$placement_id,placement_N[,1]),2]*pplace$placement$ml_ratio,list(branch=pplace$placement$location),sum)

    if(type=="number"){
      plot(pplace$arbre,edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
      text(0,0,main,cex=cex.text,pos=4)
      edgelabels(round(br_sum[,2]),br_sum[,1],cex=cex.number)
      if(asb) add.scale.bar()
    }

    if(type=="color"){
      col_palette=rgb(colorRamp(c("blue","green","yellow","red"))(seq(0,1,length=1000)), maxColorValue = 255)
      vcol <- rep("grey",nrow(pplace$arbre$edge))
      vcol[br_sum[,1]] <- col_palette[ceiling(1000*br_sum[,2]/max(br_sum[,2]))]
      if(!legend){
	plot(pplace$arbre,edge.color=vcol,edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
	text(0,0,main,cex=cex.text,pos=4)
	if(asb) add.scale.bar()
      }
     if(legend){
	layout(matrix(c(rep(1,6),2),ncol=1))
	plot(pplace$arbre,edge.color=vcol,edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
	text(0,0,main,cex=cex.text,pos=4)
	if(asb) add.scale.bar()
	par(mar=c(3,15,3,15))
	image(1:length(col_palette),1:1,matrix(1:length(col_palette),ncol=1),col=col_palette,xaxt="n",yaxt="n",xlab="",ylab="")
	axis(3,at=c(1,length(col_palette)),labels=c(1,max(br_sum)))
      }
    }
  }

  if(type=="precise"){
    if(!legend) layout(1)
    if(legend) layout(matrix(c(rep(1,6),2,rep(1,6),3),ncol=2))
    plot(pplace$arbre,edge.color="black",edge.width=edge.width,show.tip.label=stl,no.margin=TRUE)
    text(0,0,main,cex=cex.text,pos=4)
    if(asb) add.scale.bar()
    pos_phylo <- get_edge()
    x <- pos_phylo[pplace$placement$location,1] + pplace$placement$distal_bl
    y <- pos_phylo[pplace$placement$location,2]

    col_palette=rgb(colorRamp(c("blue","green","yellow","red"))(seq(0,1,length=1000)), maxColorValue = 255)
    vcol <- col_palette[ceiling((pplace$placement$pendant_bl/max(pplace$placement$pendant_bl))*1000)]
    placement_N <- aggregate(pplace$multiclass$N,list(pplace$multiclass$placement_id),sum)
    cex_placement <- placement_N[match(pplace$placement$placement_id,placement_N[,1]),2]*pplace$placement$ml_ratio
    
    text_legend <- seq(0,ceiling(max(cex_placement)),length.out=5)
    if(!is.null(transfo)) cex_placement <- transfo(cex_placement)
    order_ploting <- order(cex_placement,decreasing=TRUE)
    points(x[order_ploting],y[order_ploting],col=paste(vcol[order_ploting],transp,sep=""),cex=cex_placement[order_ploting],pch=16)
    
    if(legend){
	par(mar=c(3,7,3,3.5))
	image(1:length(col_palette),1:1,matrix(1:length(col_palette),ncol=1),col=col_palette,xaxt="n",yaxt="n",xlab="",ylab="")
	axis(3,at=c(1,length(col_palette)),labels=c(0,round(max(pplace$placement$pendant_bl),2)))
	par(mar=c(1,3.5,1,7))
	plot.new()
	cex_legend <- text_legend
	if(!is.null(transfo)) cex_legend <- transfo(cex_legend)
	points(seq(0.1,0.9,by=0.2),y=rep(0.5,5),pch=16,cex=cex_legend)
	text(seq(0.1,0.9,by=0.2),y=rep(0.7,5),round(text_legend),pos=3)
    }
  }
}
