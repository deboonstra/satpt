% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/satpt.R
\name{satpt}
\alias{satpt}
\title{Saturation point analysis}
\usage{
satpt(
  y,
  by,
  exclude = c(NA, NaN),
  alpha = 0.05,
  threshold = 0.025,
  dimnames = NULL,
  ...
)
}
\arguments{
\item{y}{Multinomial responses collected and being examined for saturation.
See the \emph{Details} section for valid \emph{R} data objects and how
they are handled.}

\item{by}{Values indicating when the multinomial responses (\code{y}) were
collected. See the \emph{Details} section for when to specify this argument.}

\item{exclude}{Vector of values that should be excluded in \code{y} and
\code{by}. Generally, this should be used to denote missing values. Default
is \code{NA} and \code{NaN}.}

\item{alpha}{Significance level for test for independence by \code{y} and \code{by}.
Default is \code{0.05}.}

\item{threshold}{Saturation threshold applied to the maximum standard error
of the sample proportions. Default is \code{0.025} and the threshold must be less
or equal to 0.25.}

\item{dimnames}{Character vector of names for \code{y} and \code{by} when
displaying the contingency table, sample proportions, and standard error
matrices. When \code{dimnames} is an unnamed vector the first entry should be name
of \code{y} variable and the second entry should be name of \code{by} variable. If
\code{dimnames} is a named vector, then order of the values does NOT matter, as
long the elements are named with \code{"y"} and \code{"by"}. Default is \code{NULL}.}

\item{...}{Additional arguments passed to \code{\link[stats:chisq.test]{stats::chisq.test()}} or
\code{\link[stats:fisher.test]{stats::fisher.test()}} for more control over the test for independence.
\code{x} and \code{y} arguments from \code{\link[stats:chisq.test]{stats::chisq.test()}} or \code{\link[stats:fisher.test]{stats::fisher.test()}}
should \strong{not} be spectified because the \code{by} and \code{y} from arguments above
will create a contingency matrix to perform the test of independence on.}
}
\value{
An object with \code{S3} class \code{"satpt"} containing 12 elements. The
return elements in a \code{"satpt"} object are based on the response item that
had the largest standard error. The \code{which_saturation} returned value
indicates which response item had the largest standard error. This nature
of \code{satpt} is of most important when determining saturation for select all
apply questions.
\describe{
\item{\code{threshold}}{Saturation threshold applied to the standard errors of
thesample proportions.}
\item{\code{saturation}}{A logical value indicating whether response item
with the largest standard error has achieved saturation given the defined
\code{threshold}. The value of \code{TRUE} indicates that saturation has been achieved
while a value of \code{FALSE} indicates that saturation was not achieved and more
data is needed to achieve saturation.}
\item{\code{which_saturation}}{A character value indicating which collection of
responses within \code{y} determined saturation achievement. Generally, this is
only of importance when examining select all apply questions. For
multiple choice type of question, the returned value should just be the
object name.}
\item{\code{counts}}{A \code{matrix} object containing the observed cell
counts of the contigency table created by \code{y} and \code{by} if provided.}
\item{\code{phat}}{A \code{matrix} object containing the row-wise sample
proportions for the observed contigency table (\code{counts}).}
\item{\code{se}}{A \code{matrix} object containing the standard errors for
the calculated sample proportions (\code{phat}).}
\item{\code{pooled_se}}{A logical value indicating whether pooled standard errors
were calculated due to the presence of response bias.}
\item{\code{alpha}}{Significance level for the test for independence.}
\item{\code{test}}{A \code{htest} object produced by \code{\link[stats:chisq.test]{stats::chisq.test()}}
or \code{\link[stats:fisher.test]{stats::fisher.test()}} containing the results from the test for
independence.}
\item{\code{n}}{Total number of observations with a response provided.}
\item{\code{total}}{A \code{data.frame} object with 4 variables describing
the overall collected sample. The \code{categories} variable provides the unique
categories listed in \code{y}. While \code{counts}, \code{phat}, and \code{se} provide the
overall cell counts, sample proportions, and standard errors for the
categories, respectively. The standard errors reported for the overall
sample proportions are calculated based on the presence of response bias,
which is detailed above.}
\item{\code{hindex}}{A vector of heterogeneity index values for the sample
proportions calculated by mean absolute deviation.}
}
}
\description{
Saturation point analysis of multinomial responses from a
survey using standard errors of the sample proportions for the responses.
}
\details{
The \code{by} argument should be specified when the responses collected
in \code{y} are a by-product of the data collection mechanism defined by the \code{by}
variable. When there is no a priori data collection mechanism defined, the
\code{by} argument should not be defined. Generally, when the data is collected
randomly or collected during the first data collection period, there is "no
a priori data collection mechanism."

The parameters \code{y} and \code{by} maybe a \code{vector}, \code{factor}, \code{matrix},
\code{data.frame}, \code{data.table}, \code{tibble}, or \code{list}. When the parameters are a
\code{list} object, each element of the object should be of equal length.
Otherwise missing values will be appended to the end of each element based on
the element that has the longest length. If \code{y} or \code{by} are \code{factor}s the
underlying order of the factor will be ignored when the contingency table is
created. The \code{\link[=char_matrix]{char_matrix()}} function is used within \code{satpt()} to
coerce the values of \code{y} and \code{by} to be character values to have consistency
of value types.

Generally, \code{y} should only have more than 1 column when select all apply
questions are being examined. See the \code{select-all-apply} vignette for more
information.

Functionality of \code{satpt()} depends on the limited use of \code{\link[stats:ftable]{stats::ftable()}}.
More specifically, \code{y}, \code{by}, and \code{exclude} are directly used with
\code{\link[stats:ftable]{stats::ftable()}} to create the contingency table of the collected data. The
contingency table created by \code{\link[stats:ftable]{stats::ftable()}} is converted to a \code{matrix} for
easier use with other functions. When the \code{dimnames} argument is used, it
manipulates the attributes of the created \code{ftable}, which impacts the
dimension names of the resulting \code{matrix}.

Specification of the \code{by} argument automatically calls for a test for
independence and the heterogeneity index of the sample proportions to be
calculated. The test for independence is conducted to determine if response
bias is present within the responses in \code{y} are due to the data collection
periods (\code{by}). Fisher's Exact Test or the \eqn{I \times J} variant is
implemented when the more than 20\% of the expected cell counts are less than
5. Otherwise Pearsons' \eqn{\chi^2} Test for Independence is implemented.

When response bias is present the pooled standard errors of the overall
sample proportions for each response item is reported, the pooled
standard errors account for the response bias. The heterogeneity index is
defined as the mean absoluted difference of the sample proportions for each
response item within each data collection period (\code{by}) relative to the
overall sample proportions for each response item. This index reflects
the average deviation of the data collection period proportions from the
overall sample proportions. Smaller values indicate the sample proportions of
the data collection periods are less dissimilar. This measure is of
importance when response bias is present. When \code{by} is not specified the test
for independence will not be conducted and the heterogeneity index will not
be calculated. \code{satpt} assumes response bias is only possible when \code{by} is
specified. Thus, there is no need to check for response bias when \code{by} is
not specified.

Determination of saturation is based on the the response item and/or
collection of responses that has the largest standard error. If this largest
standard error achieves saturation then all other categories or responses
will achieve saturation. For select all apply questions, the collection of
responses that have the largest standard error (i.e., a sample proportion
closest to 0.5) will be used to determine saturation of all responses.
}
\note{
The returned \code{satpt} object will contain \code{NULL} values for \code{test} and
\code{hindex} when \code{by} is not specified. This is done because \code{satpt} assumes
\code{by} is only specified when the data is collected in intervals.
}
\examples{
data(ein)

# Assuming response bias is not a possiblity
satpt::satpt(y = ein$q2)

# Examining saturation given data collected at different times and
# response bias is possible. For this example, response bias is not present,
# so the standard errors will be the same.
satpt::satpt(y = ein$q2, by = ein$wave)

# Creating an example, where response bias is present.

## Simulating data
prob <- matrix(
  data = c(0.4, 0.4, 0.2, 0.1, 0.1, 0.8),
  nrow = 2, ncol = 3, byrow = TRUE
)
catg <- LETTERS[1:3]
set.seed(123)
dat <- satpt::simulate(
  n = 1, size = c(250, 100), prob = prob, categories = catg
)

## Determining saturation with response bias
res <- satpt::satpt(y = dat$responses1, by = dat$period)
summary(res)

}
\seealso{
\code{\link[stats:ftable]{stats::ftable()}} \code{\link[stats:chisq.test]{stats::chisq.test()}}
}
