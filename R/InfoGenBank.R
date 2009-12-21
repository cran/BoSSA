`InfoGenBank` <-
function(X,tsleep=3)
{	
	gbout <- paste("AccNb","Organism","Isolate","Taxonomy","DateSub","DateEch","Host","Source","Location","GPS","Authors","Title","Journal","PubmedURL",sep="\t")
	for(i in 1:length(X))
	{
		gbi <- InfoGB(X[i],tsleep)
		gbout <- c(gbout,gbi)
	}
gbout
}

