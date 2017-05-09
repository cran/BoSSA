## ----style, echo = FALSE, results = 'asis'-------------------------------
BiocStyle::markdown()

## ------------------------------------------------------------------------
library("rentrez")
library("XML")
library("ape")
library("BoSSA")

## ------------------------------------------------------------------------
r_search <- entrez_search(db="taxonomy", term="polerovirus")
r_search$ids

## ------------------------------------------------------------------------
r_search <- entrez_search(db="nucleotide", term="txid119164[orgn] NOT srcdb_refseq[PROP]")
r_search$count

## ------------------------------------------------------------------------
r_search <- entrez_search(db="nucleotide", term="txid119164[orgn] NOT srcdb_refseq[PROP] AND 5000:6500[slen]")
r_search$count

## ------------------------------------------------------------------------
r_search <- entrez_search(db="nucleotide", term="txid119164[orgn] NOT srcdb_refseq[PROP] AND 5000:6500[slen]",retmax=500)
gi <- r_search$ids
gi[1:10]

## ------------------------------------------------------------------------
all_recs <- entrez_fetch(db="nuccore", id=gi, rettype="gbc",retmode="xml",parsed=TRUE)
rec_list <- xmlToList(all_recs)

## ------------------------------------------------------------------------
write(sapply(rec_list,function(X){paste(">",X$INSDSeq_locus,"\n",X$INSDSeq_sequence,sep="")}),"polerovirus_from_genbank.fasta")

## ------------------------------------------------------------------------
source_name <- t(sapply(rec_list,function(X){c(X$INSDSeq_locus,X$INSDSeq_organism)}))

## ------------------------------------------------------------------------
tree <- read.tree(paste(find.package("BoSSA"),"/extdata/polerovirus_from_genbank_MAFFT.tre",sep=""))
plot(tree,cex=0.4,no.margin=TRUE)
nodelabels(cex=0.8)

## ------------------------------------------------------------------------
root_tree <- root(tree,node=231,resolve=TRUE)
plot(root_tree,cex=0.4,no.margin=TRUE)

## ------------------------------------------------------------------------
write.tree(root_tree,"polerovirus_ROOTED.tre")

## ------------------------------------------------------------------------
tax_ids <-  entrez_link(dbfrom='nuccore', id=gi, db='taxonomy')
tax_ids <- tax_ids$links$nuccore_taxonomy
write(tax_ids,"taxonomy_polerovirus.id")

## ------------------------------------------------------------------------
taxo <- read.csv(paste(find.package("BoSSA"),"/extdata/polerovirus_taxonomy.csv",sep=""))
tax_id <- taxo$tax_id[match(source_name[,2],taxo$tax_name)]
info <- data.frame(seqname=source_name[,1],accession=source_name[,1],tax_id=tax_id,species=source_name[,2],is_type=rep("no",nrow(source_name)))
write.table(info,"polerovirus_info.csv",sep=",",row.names=FALSE)

## ------------------------------------------------------------------------
refpkg_path <- paste(find.package("BoSSA"),"/extdata/polerovirus.refpkg",sep="")

refpkg(refpkg_path)

## ----tree1, fig.width=5, rank_tree="species",fig.height=5----------------
refpkg(refpkg_path,type="tree",cex.text=0.3)

## ----pie1, fig.width=5, fig.height=5-------------------------------------
refpkg(refpkg_path,type="pie",rank_pie="species",cex.text=0.6)

## ------------------------------------------------------------------------
d <- cophenetic.phylo(root_tree)

ids <- NULL
for(usp in unique(info$species)){
  sub <- info[as.character(info$species)==usp,]
  if(nrow(sub)<=10){
    ids <- c(ids,as.character(sub[,1]))
  }
  if(nrow(sub)>10){
    acc <- as.character(sub[,1])
    # the following code line prevent the code from crashing
    # i.e. new sequences not available in the example may be uploaded when you will run the code
    acc <- acc[!is.na(match(acc,colnames(d)))]
    h <- hclust(as.dist(d[acc,acc]))
    grp <- cutree(h,k=10)
    ids <- c(ids,names(grp)[match(1:10,grp)])
  }
}

to_remove <- root_tree$tip.label[!root_tree$tip.label%in%ids]

## ------------------------------------------------------------------------
root_tree2 <- drop.tip(root_tree,to_remove)
write.tree(root_tree2,"polerovirus_ROOTED_SUBSAMPLED.tre")

## ------------------------------------------------------------------------
citation("BoSSA")

