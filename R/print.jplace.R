print.jplace <-
function(x,...){
  cat("jplace object\n")
  cat(paste("call run: ",x$run,"\n",sep=""))  
  cat(paste("Placement on a phylogenetic tree with ",length(x$arbre$tip.label)," tips and ",x$arbre$Nnode," internal nodes.\n",sep=""))
  cat(paste("sequence nb: ",length(unique(x$multiclass$name)),"\n",sep=""))  
  cat(paste("placement nb: ",length(unique(x$placement_positions$placement_id)),"\n",sep=""))  
}
