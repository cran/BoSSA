print.pplace <-
function(x,...){
  cat("pplace object\n")
  cat(paste("run: ",nrow(x$run),"\n",sep=""))
  cat(paste("call run 1: ",x$run[1,2],"\n",sep=""))  
  cat(paste("sequence nb: ",nrow(x$multiclass),"\n",sep=""))    
  cat(paste("placement nb: ",length(unique(x$placement$placement_id)),"\n",sep=""))  
}
