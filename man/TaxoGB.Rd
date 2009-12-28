\name{TaxoGB}
\alias{TaxoGB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Search for taxonomy information in genbank from a text query }
\description{
  This function used the ESearch and ESpell tool from Eutils (http://eutils.ncbi.nlm.nih.gov/) to obtain taxonomy information from a text query. In case no hit is obtain, a mispell search is done.
}
\usage{
TaxoGB(X,tsleep=3,organism="viridiplantae")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a vector of accession numbers }
  \item{tsleep}{ the time between two query to genbank. set to 3 (the unit is the second) as asked by GenBank }
  \item{organism}{ a GenBank search query on organism. set to "viridiplantae" by default.
                   See http://www.ncbi.nlm.nih.gov/BLAST/blastcgihelp.shtml for details }
}
\details{
  Require the R4X package (available at http://r-forge.r-project.org/projects/r4x/) and internet connexion.
}
\value{
  The output is a character string with value tab separated. It includes the query, the taxonomy ID from GenBank, the Family, species and subspecies and a proposition of correction in case of possible mispell.
}
\author{ Pierre Lefeuvre }
\seealso{ \code{\link[ape]{read.GenBank}} }
\examples{
#require internet connexion
#require R4X package, available at http://r-forge.r-project.org/projects/r4x/
#TaxoGB("tomato")
#TaxoGB("tomata")
}


