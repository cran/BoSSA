\name{read_protdb}
\alias{read_protdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read Protein Data Bank (PDB) file 
}
\description{
Read Protein Data Bank (PDB) file 
}
\usage{
read_protdb(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The path/name of a pdb file.
}
}
\value{
The output is a list of objects	
  \item{header}{The header of the pdb file}
  \item{compound}{A data frame summarizing the CMPND part of the pdb file. This include the molecule ID, the molecule name and the chain ID}
  \item{atom}{A data frame with the atom type, the amino acid, the amino acid number, the chain and the euclidian X, Y, Z cooridnates of the atom}
  \item{sequence}{A list with the numbering of the amino acid and the amino acid sequence for each chain}
}
\references{
http://www.rcsb.org/pdb/home/home.do
}
\author{
pierre lefeuvre
}
\examples{
pdb_file <- system.file("extdata", "1L2M.pdb", package = "BoSSA")
pdb <- read_protdb(pdb_file)
pdb
}
