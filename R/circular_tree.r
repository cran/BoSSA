
draw_space <- function(size){
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,size),ylim=c(0,size),bty="n",main="",xlab="",ylab="",xaxt="n",yaxt="n")
}

horiz <- function(pos1,pos2,N,b,ratio,def,size){
  x <- size/2
  y <- size/2
  r <- size/2
  phi <- seq(pi/2, (2 * pi)+pi/2, length = def)
  V1 <- phi[ceiling((min(c(pos1,pos2))/N)*def):ceiling((max(c(pos1,pos2))/N)*def)]
  lines(x + (r-(r*b)*ratio)* (-cos(V1)), y + (r-(r*b)*ratio)* sin(V1), type = "l",lwd=1)
}

edge <- function(pos,N,b1,b2,ratio,def,size){
  x <- size/2
  y <- size/2
  r <- size/2
  phi <- seq(pi/2, (2 * pi)+pi/2, length = def)
  cx1 <- x + (r-(r*b1)*ratio) * (-cos(phi[ceiling(pos*def/N)]))
  cx2 <- x + (r-(r*b2)*ratio) * (-cos(phi[ceiling(pos*def/N)]))
  cy1 <- y + (r-(r*b1)*ratio) * (sin(phi[ceiling(pos*def/N)]))
  cy2 <- y + (r-(r*b2)*ratio) * (sin(phi[ceiling(pos*def/N)]))
  lines(c(cx1,cx2),c(cy1,cy2))
  return(c(cx1,cx2,cy1,cy2))
}

get_root <- function(arbre){
  depth <- node.depth(arbre)
  (1:length(depth))[depth==max(depth)]
}

get_descent <- function(arbre,nb=FALSE){
  out <- list()
  all <- mrca(arbre)
  tip <- length(arbre$tip.label)
  for (i in 1:arbre$Nnode){
    if(!nb) out[[i]] <- rownames(all)[apply(all==(tip+i),1,sum)>0]
    if(nb) out[[i]] <- match(rownames(all)[apply(all==(tip+i),1,sum)>0],arbre$tip.label)
  }
  out
}

get_branch <- function(arbre){
  brl <- arbre$edge.length
  brd <- arbre$edge
  desc <- get_descent(arbre)
  nodeorder <- unlist(lapply(desc,length))
  names(nodeorder) <- (length(arbre$tip.label)+1):(length(arbre$tip.label)+arbre$Nnode)
  nodeorder <- sort(nodeorder,TRUE)
  supernode <- c(as.numeric(names(nodeorder)),1:length(arbre$tip.label))
  out <- data.frame(node1=brd[,1],node2=brd[,2],brl1=rep(0,length(brd[,1])),brl2=brl)
  for(i in 2:length(supernode)){
    a <- match(supernode[i],brd[,2])
    if(!is.na(a)) out$brl1[a] <- out$brl2[a] + out$brl1[a]
    a2 <- (1:length(brd[,1]))[brd[,1]==supernode[i]]
    if(!is.na(a)) out$brl1[a2] <- out$brl1[a2] + out$brl1[a]
  }
  out$b1 <- out[,3]/max(out[,3])
  out$b2 <- (out[,3] - out[,4])/max(out[,3])
  out
}
 
desc_nb <- function(arbre){
  out <- c(rep(1,length(arbre$tip.label)),unlist(lapply(get_descent(arbre),length)))
  names(out) <- 1:length(out)
  out
} 

circular_tree <- function(phy,ratio=0.5,def=1000,pos_out=FALSE,tip_labels=TRUE,cex_tips=0.5){
  size <- 300
  branch <- get_branch(phy)
  dn <- desc_nb(phy)
  N <- length(phy$tip.label)
  pos_vector <- unique(unlist(get_descent(phy,nb=TRUE)))
  descnb <- get_descent(phy,nb=T)
  tipnb <- list()
  for(i in 1:length(pos_vector)) tipnb[i] <- pos_vector[i]
  pos_vector <- c(tipnb,descnb)
  draw_space(size)
  out <- list()
  for(i in 1:length(branch[,1])){
    out[[i]] <- c(branch[i,1:2],edge(mean(c(min(pos_vector[[branch[i,2]]]),max(pos_vector[[branch[i,2]]]))),N,branch$b2[i],branch$b1[i],ratio,def,size))
  }
  internal_node <- unique(branch[,1])
  for(i in 1:length(internal_node)){
    sub <- branch[branch[,1]==internal_node[i],]
    subtip <- pos_vector[sub[,2]]
    s1 <- (1:length(subtip))[unlist(lapply(subtip,min))==min(unlist(lapply(subtip,min)))]
    s2 <- (1:length(subtip))[unlist(lapply(subtip,max))==max(unlist(lapply(subtip,max)))]
    horiz(mean(c(min(subtip[[s1]]),max(subtip[[s1]]))),mean(c(min(subtip[[s2]]),max(subtip[[s2]]))),N,sub$b2[1],ratio,def,size)
  }
  out <- do.call(rbind,out)
  colnames(out) <- c("node1","node2","x1","x2","y1","y2")
  if(tip_labels){
      postips <- out[out[,2]%in%(1:length(phy$t)),c(4,6)]
      text(postips[,1],postips[,2],phy$t,cex_tips)
  }
  if(pos_out){
    return(out)
  }
}





