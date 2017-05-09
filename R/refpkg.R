refpkg <- function(refpkg_path,type="summary",rank_tree="species",rank_pie=c("phylum","class","order","family","genus"),scale_pie=TRUE,alpha_order=TRUE,cex.text=0.7,cex.legend=1,asb=TRUE,rotate_label=TRUE){
  # read the file in the refpkg
  here <- getwd()
  setwd(refpkg_path)
  content <- fromJSON("CONTENTS.json")
  taxo <- read.csv(content$files$taxonomy,colClasses="character")
  tree <- read.tree(content$files$tree)
  info <- read.csv(content$file$seq_info,colClasses="character")
  info <- info[match(tree$t,info$seqname),]
  setwd(here)
  
  # subsetting the taxonomy table with only the parents of sequence from the refpkg
  taxid <- NULL
  for(i in 1:nrow(info)){
    pid <- info$tax_id[i]
    while(pid[1]!=1){
      pid <- c(taxo$parent_id[taxo$tax_id==pid[1]],pid)
   }
    taxid <- c(taxid,pid)  
  }
  taxid <- unique(taxid)
  taxo <- taxo[taxo$tax_id%in%taxid,,drop=FALSE]
  
  if(type=="summary"){
    cat("### Reference package summary\n\n")
    cat(paste("Path:",refpkg_path,"\n\n",sep=""))
    cat(paste("Tree with ",length(tree$t)," tips ",tree$Nnode," nodes\n\n",sep=""))
    trank <- table(taxo$rank)
    pos <- match(colnames(taxo),names(trank))
    trank <- trank[pos[!is.na(pos)]]
    cat("Classification:\n")
    for(i in 1:length(trank)){cat(paste(names(trank)[i],trank[i],"\n"))}
  }
  
  if(type=="taxonomy"){
    return(taxo)
  }
  
  if(type=="tree"){
    desc <- Descendants(tree,(length(tree$t)+1):(length(tree$t)+tree$Nnode),type="tips")
    info$classif <- taxo[,rank_tree][match(info$tax_id,taxo$tax_id)]
    info$classif[info$classif==""] <- "not available"
    desc2 <- c(info$classif[match(tree$t,info[,1])],sapply(desc,function(X,tree,info){a <- unique(info$classif[match(tree$t[X],info[,1])]);out <- "multiple";if(length(a)==1) out <- a;out},tree,info))
    
    pos <- match(desc2,taxo[,1])
    tax_name <- taxo$tax_name[pos[!is.na(pos)]]
    desc2[!is.na(pos)] <- tax_name

    colv <- c(rainbow(length(unique(tax_name))),"black","grey")
    if(alpha_order){
      names(colv) <- c(sort(unique(tax_name)),"multiple","not available")
    }
    if(!alpha_order){
      names(colv) <- c(unique(tax_name),"multiple","not available")
    }

    layout(matrix(c(rep(1,70),rep(2,30)),ncol=10))
    plot(tree,edge.color=colv[desc2[tree$edge[,2]]],tip.color=colv[desc2],cex=cex.text,no.margin=TRUE)
    if(asb) add.scale.bar()
    plot.new()
    names_id <- names(colv)
    pos <- match(names(colv),taxo[,1])
    names_id[!is.na(pos)] <- taxo$tax_name[pos[!is.na(pos)]]
    if(alpha_order){
      text(0,seq(1,0,length.out=length(colv)),names_id,col=colv,cex=cex.legend,pos=4)
    }
    if(!alpha_order){
      text(0,seq(0,1,length.out=length(colv)),names_id,col=colv,cex=cex.legend,pos=4)
    }

  }
  
  if(type=="pie"){
    taxo2 <- taxo[taxo$rank==rev(rank_pie)[1],][,-(1:4)]
    taxo2 <- taxo2[,apply(taxo2=="",2,sum)!=nrow(taxo2)]
    taxo2 <- taxo2[,colnames(taxo2)%in%rank_pie,drop=FALSE]

    N <- 1
    if(scale_pie){
      Nrank <- table(taxo[,rev(rank_pie)[1]][match(info$tax_id,taxo$tax_id)])
      N <- as.numeric(Nrank[taxo2[,rev(rank_pie)[1]]])
    }
    
    taxo3 <- taxo2
    taxid <- unique(unlist(taxo2))
    for(i in 1:length(taxid)){
      if(taxid[i]!="") taxo3[taxo2==taxid[i]] <- taxo$tax_name[taxo$tax_id==taxid[i]]
    }
    taxo3$N <- N
    if(ncol(taxo3)>2) taxo3 <- taxo3[do.call(order,taxo3[,-ncol(taxo3)]),]
    if(ncol(taxo3)==2) taxo3 <- taxo3[order(taxo3[,1]),]    
    par(mar=c(0,0,0,0))
    plot.new()
    rset <- seq(0.15,0.45,length.out=ncol(taxo3)-1)
    rset2 <- rset - 2/3*(rset[2] - rset[1])
	rset2[1] <- 0.05
    if(ncol(taxo3)==2){
      rset <- 0.45
      rset2 <- 0.25
    }
    if(length(table(taxo3[,1]))==1) rset2[1] <- 0
    for(i in (ncol(taxo3)-1):1){
      aggtaxo <- aggregate(taxo3$N,list(taxo3[,i]),sum)
      aggtaxo2 <- aggtaxo$x
      names(aggtaxo2) <- aggtaxo[,1]
      aggtaxo2 <- aggtaxo2[order(match(names(aggtaxo2),taxo3[,i]))]
      bisect.angles <- floating.pie(0.5,0.5,as.numeric(aggtaxo2),radius=rset[i],startpos=pi/2)
      for(j in 1:length(bisect.angles)){
	if(rotate_label){
	  srti <- bisect.angles[j]*180/pi
	  pie.labels(0.5,0.5,bisect.angles[j],names(aggtaxo2)[j],radius=rset2[i],cex=cex.text,srt=srti,pos=4)
	}
	if(!rotate_label) pie.labels(0.5,0.5,bisect.angles[j],names(aggtaxo2)[j],radius=rset2[i],cex=cex.text)
      }
    }
  }
}

