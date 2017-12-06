\name{pplace_to_taxonomy}
\alias{pplace_to_taxonomy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a pplace object to a taxonomy table
}
\description{
Convert a pplace object to a taxonomy table
}
\usage{
pplace_to_taxonomy(pplace,taxonomy,
rank=c("phylum","class","order","family","genus","species"),
tax_name=TRUE,run_id=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pplace}{
A pplace object
}
  \item{taxonomy}{
The taxonomy table as obtained using the refseq fonction with type set to taxonomy
}
  \item{rank}{
The desired rank for the taxonomy table
}
  \item{tax_name}{
Wether to use taxonomy names (default) or tax_id number
}
  \item{run_id}{
A vector of run_id to subset
}
}
\value{
A matrix with taxonomic ranks for each sequence
}
\author{
pierre lefeuvre
}
