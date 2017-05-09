\name{pplace_to_matrix}
\alias{pplace_to_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pplace to contingency matrix
}
\description{
Convert the pplace object into a contingency matrix OTUs / sample
}
\usage{
pplace_to_matrix(pplace, sample_info, N = NULL, tax_name = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pplace}{
A pplace object
}
  \item{sample_info}{
A vector specifying the association between placement (in the multiclass table) and sample
}
  \item{N}{
An optionnal vector with a number of occurence associated to each placed sequence
}
  \item{tax_name}{
Either the tax ids (default) or the tax names are used as column names. the tax names are obtained form the "taxo" table of the pplace object
}
}
\value{
A contingency matrix with OTUs / species in rows and samples in columns
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

### simple example
pplace_to_matrix(pplace,c(rep("sample1",7),rep("sample2",10)))

### using the N option to specify the number of sequence each placement represents
Npplace <- sample(1:20,17,replace=TRUE)
pplace_to_matrix(pplace,c(rep("sample1",7),rep("sample2",10)),N=Npplace)

### with tax_name=TRUE
pplace_to_matrix(pplace,c(rep("sample1",7),rep("sample2",10)),tax_name=TRUE)
}

