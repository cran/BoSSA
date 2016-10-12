pplace_to_matrix <-
function(pplace,sample_info,N=NULL,tax_name=FALSE){
  out <- NULL
  if(nrow(pplace$multiclass)>0){
    if(is.null(N)){
      N <- rep(1,length(sample_info))
    }
    sample_id <- unique(sample_info)
    tax_id <- unique(pplace$multiclass$tax_id)
    out <- matrix(0,ncol=length(tax_id),nrow=length(sample_id),dimnames=list(sample_id,tax_id))
    for(i in 1:nrow(pplace$multiclass)){
      out[sample_info[i],pplace$multiclass$tax_id[i]] <- out[sample_info[i],pplace$multiclass$tax_id[i]] + N[i]
    }
    if(tax_name) colnames(out) <- pplace$taxo[match(tax_id,pplace$taxo[,1]),2]
  }
  out
}
