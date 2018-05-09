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
pplace_to_matrix(pplace, sample_info, N = NULL, tax_name = FALSE
				 ,run_id=NULL,round_type=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pplace}{
A pplace object
}
  \item{sample_info}{
A vector or list specifying the association between placement (in the multiclass table) and sample. In the case of a list, multiple sample can be associated with a single placement.
}
  \item{N}{
An optionnal vector or list with a number of occurence associated to each placed sequence. If "sample_info" is a list, "N" must also be a list. 
}
  \item{tax_name}{
Either the tax ids (when set to FALSE, default) or the tax names (when set to TRUE) are used as column names. The tax names are obtained form the "taxo" table of the pplace object.
}
  \item{run_id}{
A vector of run_id to subset
}
  \item{round_type}{
The name of the rounding fonction to apply to the product of the number of individuals classified in a given category and the likelihood ratio of this classification.
Should be set to NULL (no rounding) or one of "trunc", "round", "ceiling" or "floor".
}

}
\value{
A contingency matrix with OTUs / species in rows and samples in columns.
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

### simple example
pplace_to_matrix(pplace,c(rep("sample1",27),rep("sample2",50),rep("sample3",23)))

### using the N option to specify the number of sequence each placement represents
Npplace <- sample(1:20,100,replace=TRUE)
pplace_to_matrix(pplace,c(rep("sample1",27),rep("sample2",50),rep("sample3",23)),N=Npplace)

### with tax_name=TRUE
pplace_to_matrix(pplace,c(rep("sample1",27),rep("sample2",50),rep("sample3",23)),tax_name=TRUE)
}

