refpkg <- function (refpkg_path, type = "summary", rank_tree = "species", 
    rank_pie = c("phylum", "class", "order", "family", "genus"), 
    scale_pie = TRUE, alpha_order = TRUE, cex.text = 0.7, cex.legend = 1, 
    asb = TRUE, rotate_label = TRUE, out_krona="for_krona.txt",text2krona=NULL) 
{
    here <- getwd()
    setwd(refpkg_path)
    content <- fromJSON("CONTENTS.json")
    taxo <- read.csv(content$files$taxonomy, colClasses = "character")
    tree <- read.tree(content$files$tree)
    info <- read.csv(content$file$seq_info, colClasses = "character")
    info <- info[match(tree$t, info$seqname), ]
    setwd(here)

    taxid <- unique(unlist(taxo[taxo$tax_id%in%info$tax_id,-(1:4)]))
    taxid <- taxid[taxid!=""]
    taxo <- taxo[taxo$tax_id %in% taxid, , drop = FALSE]
    
    if (type == "summary") {
        cat("### Reference package summary\n\n")
        cat(paste("Path:", refpkg_path, "\n\n", sep = ""))
        cat(paste("Tree with ", length(tree$t), " tips ", tree$Nnode, 
            " nodes\n\n", sep = ""))
        trank <- table(taxo$rank)
        pos <- match(colnames(taxo),names(trank))
        trank <- trank[pos[!is.na(pos)]]
        cat("Classification:\n")
        for (i in 1:length(trank)) {
            cat(paste(names(trank)[i], trank[i], "\n"))
        }
    }
    if (type == "taxonomy") {
        return(taxo)
    }
 	if (type == "info") {
        return(info)
    }
    if (type == "tree") {
        desc <- Descendants(tree, (length(tree$t) + 1):(length(tree$t) + 
            tree$Nnode), type = "tips")
        info$classif <- taxo[, rank_tree][match(info$tax_id, 
            taxo$tax_id)]
        info$classif[info$classif == ""] <- "not available"
        desc2 <- c(info$classif[match(tree$t, info[, 1])], sapply(desc, 
            function(X, tree, info) {
                a <- unique(info$classif[match(tree$t[X], info[, 
                  1])])
                out <- "multiple"
                if (length(a) == 1) out <- a
                out
            }, tree, info))
        pos <- match(desc2, taxo[, 1])
        tax_name <- taxo$tax_name[pos[!is.na(pos)]]
        desc2[!is.na(pos)] <- tax_name
        colv <- c(rainbow(length(unique(tax_name))), "black", 
            "grey")
        if (alpha_order) {
            names(colv) <- c(sort(unique(tax_name)), "multiple", 
                "not available")
        }
        if (!alpha_order) {
            names(colv) <- c(unique(tax_name), "multiple", "not available")
        }
        layout(matrix(c(rep(1, 70), rep(2, 30)), ncol = 10))
        plot(tree, edge.color = colv[desc2[tree$edge[, 2]]], 
            tip.color = colv[desc2], cex = cex.text, no.margin = TRUE)
        if (asb) 
            add.scale.bar()
        plot.new()
        names_id <- names(colv)
        pos <- match(names(colv), taxo[, 1])
        names_id[!is.na(pos)] <- taxo$tax_name[pos[!is.na(pos)]]
        if (alpha_order) {
            text(0, seq(1, 0, length.out = length(colv)), names_id, 
                col = colv, cex = cex.legend, pos = 4)
        }
        if (!alpha_order) {
            text(0, seq(0, 1, length.out = length(colv)), names_id, 
                col = colv, cex = cex.legend, pos = 4)
        }
    }
    if (type == "pie" | type=="krona") {

	if(type=="pie") taxo2 <- cbind(taxo[,1,drop=FALSE],taxo[, colnames(taxo) %in% rank_pie, drop = FALSE])
	if(type=="krona") taxo2 <- taxo[,-(2:4),drop=FALSE]
        taxo3 <- taxo2[match(info$tax_id,taxo2$tax_id),-1,drop=FALSE]
        taxid <- unique(unlist(taxo3))
        for (i in 1:length(taxid)) {
	  if (taxid[i] != "" & taxid[i]%in%taxo$tax_id) 
                taxo3[taxo3 == taxid[i]] <- taxo$tax_name[taxo$tax_id == taxid[i]]
        }
        taxo3[taxo3==""] <- "NotAvailable"
        
        taxo4 <- apply(taxo3,1,paste,collapse="_")
        N <- table(taxo4)
        taxo5 <- taxo3[match(names(N),taxo4),]
        taxo5 <- as.data.frame(taxo5)
        taxo5$N <- as.numeric(N)
               
        if(type=="krona"){
	  write.table(taxo5[,c(ncol(taxo5),1:(ncol(taxo5)-1))],out_krona,col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t")
	  if(!is.null(text2krona)){
	      system(paste("perl",text2krona,out_krona))
	      system(paste("rm",out_krona))
	    }
        }
        
        if(type=="pie"){
	  if(!scale_pie) taxo5$N <- 1
	  
	  if (ncol(taxo5) > 2) 
	      taxo5 <- taxo5[do.call(order, taxo5[, -ncol(taxo5)]), 
		  ]
	  if (ncol(taxo5) == 2) 
	      taxo5 <- taxo5[order(taxo5[, 1]), ]
	  par(mar = c(0, 0, 0, 0))
	  plot.new()
	  rset <- seq(0.15, 0.45, length.out = ncol(taxo5) - 1)
	  rset2 <- rset - 2/3 * (rset[2] - rset[1])
	  rset2[1] <- 0.05
	  if (ncol(taxo5) == 2) {
	      rset <- 0.45
	      rset2 <- 0.25
	  }
	  if (length(table(taxo5[, 1])) == 1) 
	      rset2[1] <- 0
	  for (i in (ncol(taxo5) - 1):1) {
	      aggtaxo <- aggregate(taxo5$N, list(taxo5[, i]), sum)
	      aggtaxo2 <- aggtaxo$x
	      names(aggtaxo2) <- aggtaxo[, 1]
	      aggtaxo2 <- aggtaxo2[order(match(names(aggtaxo2), 
		  taxo5[, i]))]
	      bisect.angles <- floating.pie(0.5, 0.5, as.numeric(aggtaxo2), 
		  radius = rset[i], startpos = pi/2)
	      for (j in 1:length(bisect.angles)) {
		  if (rotate_label) {
		    srti <- bisect.angles[j] * 180/pi
		    pie.labels(0.5, 0.5, bisect.angles[j], names(aggtaxo2)[j], 
		      radius = rset2[i], cex = cex.text, srt = srti, 
		      pos = 4)
		  }
		  if (!rotate_label) 
		    pie.labels(0.5, 0.5, bisect.angles[j], names(aggtaxo2)[j], 
		      radius = rset2[i], cex = cex.text)
	      }
	  }
      }
   }
}
