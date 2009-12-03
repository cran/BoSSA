\name{FastaSorter}
\alias{FastaSorter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Rename and get information from fasta sequences set download from GenBank }
\description{
  The function read a fatsa alignment download from Genbank, rename the sequence with the accession number and write a file with information about the sequences.
}
\usage{
FastaSorter(Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{The path/name of a sequence set in fasta format download from genbank }
}
\details{
  This function is handfull to go through datasets download from GenBank/TaxBrowser allowing to replace the composite GenBank sequence name with the accession number.
}
\value{
  The output is (i) a sequence set in fatsa format with names of sequence being replace by the accession number and (ii) a comma separated file with the information about the sequence.
}
\author{ Pierre Lefeuvre }

\examples{
##obtain a fasta file the GenBank
##copy the fasta file to your current directory
##FastaSorter("sequence.fasta")}
