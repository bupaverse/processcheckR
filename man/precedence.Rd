% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/precedence.R
\name{precedence}
\alias{precedence}
\title{Precedence}
\usage{
precedence(activity_a, activity_b)
}
\arguments{
\item{activity_a}{\code{\link{character}}: Activity A. This should be an activity of the log supplied to \code{\link{check_rule}}.}

\item{activity_b}{\code{\link{character}}: Activity B. This should be an activity of the log supplied to \code{\link{check_rule}}.}
}
\description{
Check for precedence between two activities.

If \code{activity_b} occured, it should be preceded by \code{activity_a} in the same case, i.e., if \code{B} was executed, it could not
have been executed before \code{A} was executed. For example, the trace \verb{[A,C,B,B,A]} satisfies the \code{precedence} relation.
}
\examples{
library(bupaR)
library(eventdataR)

# A MRI Scan should be preceeded by a Blood test.

patients \%>\%
 check_rule(precedence("Blood test","MRI SCAN"))

}
\seealso{
Other Ordering rules: 
\code{\link{ends}()},
\code{\link{responded_existence}()},
\code{\link{response}()},
\code{\link{starts}()},
\code{\link{succession}()}
}
\concept{Ordering rules}
