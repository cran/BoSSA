print.protdb <-
function(x,...){
  cat("protdb object\n")
  cat(paste(gsub(" +"," ",x$header),"\n",sep=""))
}
