\name{pplace_to_table}
\alias{pplace_to_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge the multiclass and the placement table of pplace object
}
\description{
Merge the multiclass and the placement table of pplace object
}
\usage{
pplace_to_table(pplace, type = "full")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pplace}{
a pplace object
}
  \item{type}{
the placement type to consider
}
}
\details{
For the type argument, either "full" or "best" are accepted. Whereas for the "full" type, all the placements are considered, only the best placement for each placed sequence is considered for the "best" type.
}
\value{
a data frame with the same colomn names as the mutliclass and placements tables
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

### with every placement
pplace_to_table(pplace)

### keeping only the best placement for each sequence
pplace_to_table(pplace,type="best")
}
