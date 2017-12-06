pplace_to_matrix <-
function(pplace,sample_info,N=NULL,tax_name=FALSE,run_id=NULL){
  if(class(pplace)!="pplace"){
    return("ERROR: the input is not an object of class pplace")
  }
  if(class(pplace)=="pplace"){
    if(!is.null(run_id)){
      pplace <- sub_pplace(pplace,run_id=run_id)
    }
    if(!is.null(N) & length(N)!=nrow(pplace$multiclass)) stop("N should have the a number of entry equal to the number of line of the \"multiclass\"")
    if(!is.null(sample_info) & length(sample_info)!=nrow(pplace$multiclass)) stop("sample_info should have the a number of entry equal to the number of line of the \"multiclass\"")    
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
      if(tax_name) colnames(out) <- pplace$taxa[match(tax_id,pplace$taxa[,1]),2]
    }
    return(out)
  }
}
