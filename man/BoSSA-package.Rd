\name{BoSSA-package}
\alias{BoSSA-package}
\alias{BoSSA}
\docType{package}
\title{
\packageTitle{BoSSA}
}
\description{
\packageDescription{BoSSA}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{BoSSA}
\packageIndices{BoSSA}
BoSSA offers somes functions to read and plots placements files obtained using pplacer and guppy. For now it only works using the sqlite output from guppy. Placement can then be plotted in different fashion. There one function to read pdb files... the package still have something (a little) to do with structure.
Previously BoSSA integrate NCBI BLAST fonctions and access to NCBI CGI through R.
All these functions have been removed. For BLAST, a good alternative is to use biopython. Although it's not in R, the biopython cookbook gives really good exemples making it quite easy to use. For access to the NCBI CGI, other packages such as rentrez are really well done and very handy.
}
\author{
\packageAuthor{BoSSA}
Maintainer: \packageMaintainer{BoSSA}
}
\references{
- pplacer and guppy
http://matsen.fhcrc.org/pplacer/
http://matsen.github.io/pplacer/
- protein data bank
http://www.rcsb.org/pdb/home/home.do
}
