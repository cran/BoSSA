
write_jplace <- function(x,outfile){
    if(class(x)!="pplace" & class(x)!="jplace"){
      stop("ERROR: the input is not an object of class pplace or jplace")
    }

    if(class(x)=="pplace"){
      if(nrow(x$run)>1){
	x <- sub_pplace(x,run_id=x$run[1,1])
	warning("Only the placements from the first run were exported")
      }
    }
    
    if(nrow(x$placement_positions)>0){
      if(class(x)=="pplace"){
	bid <- x$placement_positions$location
	x$placement_positions$location <- x$edge_key[2,match(bid,x$edge_key[1,])]
      }
      if(class(x)=="jplace"){
	bid <- x$placement_positions$edge_num
	x$placement_positions$edge_num <- x$edge_key[2,match(bid,x$edge_key[1,])]
      }
    }    
    write("{\"tree\":",outfile)
    tree_string <- x$original_tree
    write(paste("\"",tree_string,"\",",sep=""),outfile,append=TRUE)
    write("\"placements\":\n\t[",outfile,append=TRUE)
    pid <- unique(x$placement_positions[,1])
    for(i in 1:length(pid)){
	nmi <- NULL
	placei <- x$placement_positions[x$placement_positions[,1]==pid[i],]
	
	if(class(x)=="jplace" & !is.null(x$multiclass$nm)) nmi <- x$multiclass$nm[x$multiclass$placement_id==pid[i]]
	if(class(x)=="jplace") namei <- x$multiclass$name[x$multiclass$placement_id==pid[i]]
	if(class(x)=="pplace") namei <- x$placement_names$name[x$placement_names$placement_id==pid[i]]	
	if(class(x)=="pplace") nmi <- x$placement_names$mass[x$placement_names$placement_id==pid[i]]
	if(class(x)=="pplace") colnames(placei)[2:7] <- c("edge_num","like_weight_ratio","likelihood","distal_length","pendant_length","classification")
        
        write_placement(placei,namei,nmi,outfile)
        if(i!=length(pid)){
          write(",",outfile,append=TRUE)
	}
    }
    if(class(x)=="jplace") runinfo <- x$run[1]
    if(class(x)=="pplace") runinfo <- x$run$params[1]
    if(is.null(x$placement_positions$tax_id) & is.null(x$placement_positions$classification)) write(paste("],\n\"metadata\":\n{\"invocation\":\"",runinfo,"\"},\n\"version\": 3,\n\"fields\":[\"distal_length\",\"edge_num\",\"like_weight_ratio\",\"likelihood\",\"pendant_length\"]\n}",sep=""),outfile,append=TRUE)
    if(!is.null(x$placement_positions$tax_id) | !is.null(x$placement_positions$classification)) write(paste("],\n\"metadata\":\n{\"invocation\":\"",runinfo,"\"},\n\"version\": 3,\n\"fields\":[\"classification\",\"distal_length\",\"edge_num\",\"like_weight_ratio\",\"likelihood\",\"pendant_length\"]\n}",sep=""),outfile,append=TRUE)    
}

write_placement <- function(placei,namei,nmi,outfile){
    write("{\"p\":[",outfile,append=TRUE)
    for(i in 1:nrow(placei)){
      comma <- ","
      if(i==nrow(placei)) comma <- ""
      if(is.null(placei$classification)) write(paste("\t[",placei$distal_length[i],",",placei$edge_num[i],",",placei$like_weight_ratio[i],",",placei$likelihood[i],",",placei$pendant_length[i],"]",comma,sep=""),outfile,append=TRUE)
      if(!is.null(placei$classification)) write(paste("\t[\"",placei$classification[i],"\",",placei$distal_length[i],",",placei$edge_num[i],",",placei$like_weight_ratio[i],",",placei$likelihood[i],",",placei$pendant_length[i],"]",comma,sep=""),outfile,append=TRUE)
    }
    
    if(is.null(nmi) & length(namei)==1) write(paste("],\n\"n\":[\"",namei,"\"]\n}",sep=""),outfile,append=TRUE)
    
    if(!is.null(nmi) & length(namei)==1) write(paste("],\n\"nm\":[\n\t[\"",namei,"\",",nmi,"]\n]\n}",sep=""),outfile,append=TRUE)    
    
    if(is.null(nmi) & length(namei)>1){
      write("],\n\"n\":[",outfile,append=TRUE)
      for(i in 1:length(namei)){
        comma <- ","
	if(i==length(namei)) comma <- ""
	write(paste("[\"",namei[i],"\"]",comma,sep=""),outfile,append=TRUE)
      }
      write("]\n}",outfile,append=TRUE)      
    }
    
    if(!is.null(nmi) & length(namei)>1){
      write("],\n\"nm\":[",outfile,append=TRUE)
      for(i in 1:length(namei)){
        comma <- ","
	if(i==length(namei)) comma <- ""
	write(paste("[\"",namei[i],"\",",nmi[i],"]",comma,sep=""),outfile,append=TRUE)
      }
      write("]\n}",outfile,append=TRUE)      
    }
}
