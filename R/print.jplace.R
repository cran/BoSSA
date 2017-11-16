print.jplace <-
function(x,...){
  cat("jplace object\n")
  cat(paste("call run: ",x$run,"\n",sep=""))  
  cat(paste("Placement on a phylogenetic tree with ",length(x[[1]]$tip.label)," tips and ",x[[1]]$Nnode," internal nodes.\n",sep=""))
  cat(paste("sequence nb: ",length(unique(x$multiclass$name)),"\n",sep=""))  
  cat(paste("placement nb: ",max(x$placement$placement_id),"\n",sep=""))  
}
