\name{blast}
\alias{blast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ use NCBI blast }
\description{
  This function use the BLAST URL-API and query either DNA sequences store in object of class "DNAbin" or accession numbers
}
\usage{
blast(X, program = "blastn", database = "nr", entrezquery = "none",
      nb = 5, oot = 35)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ either an object of class "DNAbin" or a vector of accession numbers }
  \item{program}{ specify the BLAST program to use.
                  either "blastn" (default), "blastp", "blastx", "tblastn", "tblastx" }
  \item{database}{ specify the database to use. See the reference section }
  \item{entrezquery}{ add an entrez query }
  \item{nb}{ maximal number of hit to display }
  \item{oot}{ out of time parameter. Default correpsond to a maximum of 2 minutes search }
}
\value{
   the output is a list where each compartment correspond to a blast search. If the query was a "DNAbin" object, there is as many compartment as sequence in the object. If the query was an accession numbers vector, there is as many compartment as accession numbers in the vector. Each blast result is a data.frame with the accession number of the hit, the definition of the hit (i.e. the name), the score and the evalue as colomns.
}
\references{
BLAST URL-API : http://www.ncbi.nlm.nih.gov/blast/Doc/urlapi.html

BLAST homepage : http://blast.ncbi.nlm.nih.gov/Blast.cgi

Entrez query : http://www.ncbi.nlm.nih.gov/BLAST/blastcgihelp.shtml
}
\author{ Pierre Lefeuvre }
\seealso{ \code{\link[ape]{as.DNAbin}}}
\examples{
#require internet connexion
#require R4X package, available at http://r-forge.r-project.org/projects/r4x/
#blast("aj865339")
#blast("aj865339",entrezquery="viridiplantae[organism]")
}


