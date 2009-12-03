`InfoGenBank` <-
function(X,tsleep=3)
{	
	machin <- NULL
	for(i in 1:length(X))
	{
		truc <- InfoGB(X[i],tsleep)
		machin <- c(machin,truc)
	}
machin
}

