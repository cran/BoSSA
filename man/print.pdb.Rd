\name{print.protdb}
\alias{print.protdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compact display of protdb object
}
\description{
Function to print the header section of the protdb object on the console.
}
\usage{
\method{print}{protdb}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a protdb class object
}
  \item{...}{
further arguments passed to or from other methods
}
}
\author{
pierre lefeuvre
}

\examples{
pdb_file <- system.file("extdata", "1L2M.pdb", package = "BoSSA")
pdb <- read_protdb(pdb_file)
print(pdb)
}
