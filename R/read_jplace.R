read_jplace <-
function(jplace_file,full=TRUE){
  json_data <- fromJSON(jplace_file)
  arbre <- json_data[[1]]
  arbre2 <- gsub("}","",gsub("{","#",arbre,fixed=TRUE),fixed=TRUE)
  arbre3 <- read.tree(text=gsub("#","",gsub(":[0-9].[0-9]+#",":",gsub("e-","",arbre2))))
  edge_key <- rbind(1:nrow(arbre3$edge),arbre3$edge.l)
  arbre <- gsub("}","#",gsub("{","#",arbre,fixed=TRUE),fixed=TRUE)
  arbre <- read.tree(text=gsub("#[0-9]+#","",arbre))
  out <- list(arbre,edge_key)
  names(out) <- c("arbre","edge_key")
  if(full){
    a <- json_data[[2]][,1]
    for(i in 1:length(a)){
      a[[i]] <- cbind(rep(i,nrow(a[[i]])),a[[i]])
    }
    a <- as.data.frame(do.call(rbind,a),stringsAsFactors=FALSE)
    colnames(a) <- c("placement_id","tax_id","distal_bl","location","ml_ratio","log_lik","pendant_bl")
    col_num = c(1,3:7)
    a[,col_num] <- apply(a[,col_num],2,as.numeric)
    out$placement <- a
    
    b <- json_data[[2]][,2]
    for(i in 1:length(b)){
      b[[i]] <- cbind(rep(i,nrow(b[[i]])),b[[i]])
    }
    b <- as.data.frame(do.call(rbind,b),stringsAsFactors=FALSE)
    colnames(b) <- c("placement_id","name","nm")
    col_num = c(1,3)
    b[,col_num] <- apply(b[,col_num],2,as.numeric)
    out$multiclass <- b
    
    pplacer_branch_id <- out$placement$location
    out$placement$location <- out$edge_key[1,match(pplacer_branch_id,out$edge_key[2,])]
    out$edge_key <- NULL
    
    out$run <- as.character(json_data[[3]][1])
    
    class(out) <- "jplace"
  }
  out
}
