pplace_to_table <-
function(pplace,type="full"){
  out <- NULL
  if(nrow(pplace$multiclass)>0){
    if(type=="full"){
      pos <- lapply(pplace$multiclass$placement_id,function(X,Y){(1:length(Y))[Y==X]},pplace$placement$placement_id)
      out <- cbind(pplace$multiclass[ rep(1:length(pos),times=sapply(pos,length)),],pplace$placement[unlist(pos),])[,-7]
    }
    if(type=="best"){
      pos <- match(pplace$multiclass$placement_id,pplace$placement$placement_id)
      out <- cbind(pplace$multiclass,pplace$placement[pos,])[,-7]
    }
    rownames(out) <- NULL
  }
  out
}
