\name{InfoGenBank}
\alias{InfoGenBank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Download sequence information from GenBank }
\description{
  This function is designed to work with virus accession number. It download from genbank a gbfile and parse it for different information, such as the year of isolation, host, sampling location...
}
\usage{
InfoGenBank(X,tsleep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a vector of accession numbers }
  \item{tsleep}{ the time between two query to genbank. set to 3 (the unit is the second) as asked by GenBank }
}
\value{
  The output is a comma separated table ready to be write on the disk. It include 12 colomns (comma separated) with the Organism, Submission date of the sequence, the sampling date, the host, the source, the sampling location, the GPS coordiantes of the sampling location, the authors, the title, the journal in which it was published and a pubmed URL to the publication. Note that if the information is missing, the cell is left empty.
}
\author{ Pierre Lefeuvre }
\seealso{ \code{\link[ape]{read.GenBank}} }
\examples{
#require internet connexion
#accnb <- c("AJ86539","AJ865337")
#InfoGenBank(accnb)
}


