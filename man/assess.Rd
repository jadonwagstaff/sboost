% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assess.R
\name{assess}
\alias{assess}
\title{sboost Assessment Function}
\usage{
assess(object, features, outcomes, include_scores = FALSE)
}
\arguments{
\item{object}{\emph{sboost_classifier} S3 object output from sboost.}

\item{features}{feature set data.frame.}

\item{outcomes}{outcomes corresponding to the features.}

\item{include_scores}{if true feature_scores are included in output.}
}
\value{
An \emph{sboost_assessment} S3 object containing:
\describe{
  \item{\emph{performance}}{Last row of cumulative statistics (i.e. when all stumps are included in assessment).}
  \item{\emph{cumulative_statistics}}{\emph{stump} - the index of the last decision stump added to the assessment.\cr
    \emph{true_positive} - number of true positive predictions.\cr
    \emph{false_negative} - number of false negative predictions.\cr
    \emph{true_negative} - number of true negative predictions.\cr
    \emph{false_positive} - number of false positive predictions.\cr
    \emph{prevalence} - true positive / total.\cr
    \emph{accuracy} - correct predictions / total.\cr
    \emph{sensitivity} - correct predicted positive / true positive.\cr
    \emph{specificity} - correct predicted negative / true negative.\cr
    \emph{ppv} - correct predicted positive / predicted positive.\cr
    \emph{npv} - correct predicted negative / predicted negative.\cr
    \emph{f1} - harmonic mean of sensitivity and ppv.}
  \item{\emph{feature_scores}}{If include_scores is TRUE, for each feature in the classifier lists scores for each row in the feature set.}
  \item{\emph{classifier}}{sboost \emph{sboost_classifier} object used for assessment.}
  \item{\emph{outcomes}}{Shows which outcome was considered as positive and which negative.}
  \item{\emph{call}}{Shows the parameters that were used for assessment.}
}
}
\description{
Assesses how well an sboost classifier classifies the data.
}
\examples{
# malware
malware_classifier <- sboost(malware[-1], malware[1], iterations = 5, positive = 1)
assess(malware_classifier, malware[-1], malware[1])

# mushrooms
mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 5, positive = "p")
assess(mushroom_classifier, mushrooms[-1], mushrooms[1])
}
\seealso{
\code{\link{sboost}} documentation.
}
