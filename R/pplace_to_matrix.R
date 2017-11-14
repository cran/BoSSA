pplace_to_matrix <-
function(pplace,sample_info,N=NULL,tax_name=FALSE){
  out <- NULL
  if(nrow(pplace$multiclass)>0){
    sample_id <- unique(unlist(sample_info))
    tax_id <- unique(pplace$multiclass$tax_id)
    out <- matrix(0,ncol=length(tax_id),nrow=length(sample_id),dimnames=list(sample_id,tax_id))
    if(class(sample_info)!="list"){
      if(is.null(N)){
	N <- rep(1,length(sample_info))
      }
      for(i in 1:nrow(pplace$multiclass)){
	out[sample_info[i],pplace$multiclass$tax_id[i]] <- out[sample_info[i],pplace$multiclass$tax_id[i]] + N[i]
      }
    }
    if(class(sample_info)=="list"){
      if(!is.null(N) & class(N)!="list"){
	stop("Sample_info and N should be list objects")
      }
      for(i in 1:length(sample_info)){
	for(j in 1:length(sample_info[[i]])){
	  if(is.null(N)){
	    Nij <- rep(1,length(sample_info))
	  }
  	  if(!is.null(N)){
	    Nij <- N[[i]][j]
	  }
	  out[sample_info[[i]][j],pplace$multiclass$tax_id[i]] <- out[sample_info[[i]][j],pplace$multiclass$tax_id[i]] + Nij
	}
      }
    }
    if(tax_name) colnames(out) <- pplace$taxo[match(tax_id,pplace$taxo[,1]),2]
  }
  out
}

