`InfoGB` <-
function(X,tsleep=3)
{
	Organism <- ""
	DateSub <- ""
	DateEch <- ""
	Host <- ""
	Source <- ""
	Location <- ""
	GPS <- ""
	Authors <- ""
	Titre <- ""
	Journal <- ""
	pubmedURL <- ""
	
	GBID <- X
	GBFile <- scan(paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=",GBID,"&rettype=gb",sep=""),what="",sep="\n",quiet=TRUE)
	
	if (length(grep("Submitted",GBFile))>0)
	{
		Organism <- strsplit(GBFile[grep("ORGANISM",GBFile)],"ORGANISM",fixed=TRUE)[[1]][2]
	}

	#date de soumission	
	if (length(grep("Submitted",GBFile))>0)
	{	
		DateSub2 <- strsplit(GBFile[grep("Submitted",GBFile)],"(",fixed=TRUE)[[1]]
		DateSub <- strsplit(DateSub2[2],")",fixed=TRUE)[[1]][1]
	}
	
	#date d'echantillonage
	if (length(grep("collection_date",GBFile))>0) DateEch <- strsplit(GBFile[grep("collection_date",GBFile)],"collection_date=")[[1]][2]
	
	#localisation
	if (length(grep("/country=",GBFile))>0)
	{	
		Location <- strsplit(GBFile[grep("/country=",GBFile)],"country=",fixed=TRUE)[[1]][2]
	}
	
	#coordonn�e GPS
	if (length(grep("lat_lon",GBFile))>0) GPS <- strsplit(GBFile[grep("lat_lon",GBFile)],"lat_lon=")[[1]][2]

	#auteur and title
	debut <- grep("AUTHORS",GBFile)[1]
	milieu <- grep("TITLE",GBFile)[1]
	fin <- grep("JOURNAL",GBFile)[1]
	
	Authors <- paste(unlist(strsplit(GBFile[debut:(milieu-1)],"AUTHORS"))[-1],collapse="")
	Authors <- gsub(",","",Authors)
	Authors <- gsub(" ","",Authors)
	
	Titre <- paste(unlist(strsplit(GBFile[milieu:(fin-1)],"TITLE"))[-1],collapse="")
	Titre <- gsub(",","",Titre)
	
	Journal <- strsplit(GBFile[fin],"JOURNAL")[[1]][2]
	Journal <- gsub(",",";",Journal)
	
	#date d'echantillonage
	if (length(grep("PUBMED",GBFile))>0)
	{
		Pubmed <- strsplit(GBFile[grep("PUBMED",GBFile)],"PUBMED")[[1]][2]
		Pubmed <- as.numeric(as.character(Pubmed))
		pubmedURL <- paste("http://www.ncbi.nlm.nih.gov/pubmed/",Pubmed,sep="")
	}

	#host
	if (length(grep("/host=",GBFile))>0)
	{	
		Host <- strsplit(GBFile[grep("/host=",GBFile)],"host=",fixed=TRUE)[[1]][2]
	}

	#source
	if (length(grep("/isolation_source=",GBFile))>0)
	{	
		Source <- strsplit(GBFile[grep("/isolation_source=",GBFile)],"isolation_source=",fixed=TRUE)[[1]][2]
	}
	
	Sys.sleep(tsleep)
			
	out <- paste(X,Organism,DateSub,DateEch,Host,Source,Location,GPS,Authors,Titre,Journal,pubmedURL,sep=",")
	out <- gsub("\"","",out)
	out <- gsub("  "," ",out)
	out <- gsub("  "," ",out)
	out <- gsub("  "," ",out)
	out <- gsub(", ",",",out)
	out
}

