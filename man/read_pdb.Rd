\name{read_pdb}
\alias{read_pdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read Protein Data Bank (PDB) file 
}
\description{
read Protein Data Bank (PDB) file 
}
\usage{
read_pdb(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The path/name of a pdb file.
}
}
\value{
The output is a list of objects	
  \item{header}{the header of the pdb file}
  \item{compound}{a data frame summarizing the CMPND part of the pdb file. This include the molecule ID, the molecule name and the chain ID}
  \item{atom}{a data frame with the atom type, the amino acid, the amino acid number, the chain and the euclidian X, Y, Z cooridnates of the atom}
  \item{sequence}{a list with the numbering of the amino acid and the amino acid sequence for each chain}
}
\references{
http://www.rcsb.org/pdb/home/home.do
}
\author{
pierre lefeuvre
}
\examples{
pdb_file <- system.file("extdata", "1L2M.pdb", package = "BoSSA")
pdb <- read_pdb(pdb_file)
}
