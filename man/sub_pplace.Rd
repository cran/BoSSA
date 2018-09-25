\name{sub_pplace}
\alias{sub_pplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subsets a pplace object
}
\description{
Subsets a pplace or jplace object based on the placement_id, the name of the placement or a regular expression of the name of the placement
}
\usage{
sub_pplace(x, placement_id = NULL, ech_id = NULL, ech_regexp = NULL, run_id = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The pplace or jplace object to subset
}
  \item{placement_id}{
A vector of the placement_id to subset
}
  \item{ech_id}{
A vector of the names of the placement to subset
}
  \item{ech_regexp}{
A regular expression of the name of the placement to subset
}
  \item{run_id}{
A vector of run_id to subset
}
}
\details{
When using placement_id, the subset is performed based on the placement_id column of the multiclass, placements, placement_positions, placement_names, placement_classifications, placement_evidence, placement_median_identities and placement_nbc data frames.
When using ech_id and ech_regexp, the subset is performed from the multiclass$name column.
When using run_id, the subset is performed based on the placements$run_id column.
}
\value{
A pplace object
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

### subsetting using placement ids. Here placements 1 to 5
sub1 <- sub_pplace(pplace,placement_id=1:5)
sub1

### subsetting using sequenes ids
id <- c("GWZHISEQ01:514:HMCLFBCXX:2:1108:1739:60356_90",
"GWZHISEQ01:514:HMCLFBCXX:2:1114:13665:31277_80")
sub2 <- sub_pplace(pplace,ech_id=id)
sub2

### subsetting using a regular expression of sequence ids
sub3 <- sub_pplace(pplace,ech_regexp="^HWI")
sub3
}
