read_jplace <-
function(jplace_file,full=TRUE){
  json_data <- fromJSON(jplace_file)
  arbre <- json_data$tree
  arbre2 <- gsub("}","",gsub("{","#",arbre,fixed=TRUE),fixed=TRUE)
  arbre3 <- read.tree(text=gsub("#","",gsub(":[0-9].[0-9]+#",":",gsub("e-","",arbre2))))
  edge_key <- rbind(1:nrow(arbre3$edge),arbre3$edge.l)
  arbre <- gsub("}","#",gsub("{","#",arbre,fixed=TRUE),fixed=TRUE)
  arbre <- read.tree(text=gsub("#[0-9]+#","",arbre))
  out <- list(arbre,edge_key,json_data$tree)
  names(out) <- c("arbre","edge_key","original_tree")
  if(full){
    a <- json_data$placements[,1]
    for(i in 1:length(a)){
      a[[i]] <- cbind(rep(i,nrow(a[[i]])),a[[i]])
    }
    a <- as.data.frame(do.call(rbind,a),stringsAsFactors=FALSE)

    colnames(a) <- c("placement_id",json_data$fields)
    col_num = c(1,3:ncol(a))
    a[,col_num] <- apply(a[,col_num],2,as.numeric)
    out$placement_positions <- a
    
    b <- json_data$placements[,2]
    if(class(b[[1]])=="matrix"){
      for(i in 1:length(b)){
	b[[i]] <- cbind(rep(i,nrow(b[[i]])),b[[i]])
      }
    b2 <- as.data.frame(do.call(rbind,b),stringsAsFactors=FALSE)
    colnames(b2) <- c("placement_id","name","nm")
    b2[,1] <- as.numeric(b2[,1])
    b2[,3] <- as.numeric(b2[,3])
    }
    
    if(class(b[[1]])!="matrix"){
      for(i in 1:length(b)){
	b[[i]] <- cbind(rep(i,length(b[[i]])),b[[i]])
      }
    b2 <- as.data.frame(do.call(rbind,b),stringsAsFactors=FALSE)
    colnames(b2) <- c("placement_id","name")
    b2[,1] <- as.numeric(b2[,1])
    }

    out$multiclass <- b2
    
    pplacer_branch_id <- out$placement_positions$edge_num
    out$placement_positions$edge_num <- out$edge_key[1,match(pplacer_branch_id,out$edge_key[2,])]
    if(ncol(out$placement_positions)==7) out$placement_positions <- out$placement_positions[,c(1,4,5,6,3,7,2)]
    if(ncol(out$placement_positions)==6) out$placement_positions <- out$placement_positions[,c(1,2,4,3,5,6)]
    #out$edge_key <- NULL
    
    out$run <- as.character(json_data$metadata$invocation[1])
    
    class(out) <- "jplace"
  }
  out
}
