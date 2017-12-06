print.pplace <-
function(x,...){
  cat("pplace object\n")
  cat(paste("run: ",nrow(x$run),"\n",sep=""))
  cat(paste("call run 1: ",x$run[1,2],"\n",sep=""))
  cat(paste("Placement on a phylogenetic tree with ",length(x$arbre$tip.label)," tips and ",x$arbre$Nnode," internal nodes.\n",sep=""))
  cat(paste("sequence nb: ",length(unique(x$multiclass$name)),"\n",sep=""))    
  cat(paste("placement nb: ",length(unique(x$placement_positions$placement_id)),"\n",sep=""))  
}
