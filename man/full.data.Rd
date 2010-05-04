\name{full.data}
\alias{full.data}
\docType{data}
\title{
This is my title
}
\description{
This is my description
}
\usage{data(full.data)}
\format{
  A data frame with 68 observations on the following 4 variables. Only Sequence and ICD9 are used by the function.
  \describe{
    \item{\code{YearDeath}}{a numeric vector representing the year of death}
    \item{\code{Sequence}}{a numeric vector representing patient identifier, need not be numeric}
    \item{\code{ICD9}}{a numeric vector, corresponding ICD9 codes}
    \item{\code{icd9}}{a numeric vector, corresponding ICD9 codes}
  }
}
\details{
The function requires you pass it a patient id vector and icd9 vector
}
\source{
Sample data is provided based on the NHANES92 dataset.
}
\examples{
data(full.data)
table(full.data)
x<-charlson(full.data$Sequence,full.data$icd9)
x
}
\keyword{datasets}
