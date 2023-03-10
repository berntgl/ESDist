\name{ot_dat_raw}
\alias{ot_dat_raw}
\docType{data}
\title{
Data from oxytocin administration studies
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ot_dat_raw")}
\format{
  A data frame with 616 observations on the following 22 variables.
  \describe{
    \item{\code{meta_analysis}}{a character vector}
    \item{\code{meta_analysis_doi}}{a character vector}
    \item{\code{meta_analysis_year}}{a numeric vector}
    \item{\code{study}}{a character vector}
    \item{\code{study_doi}}{a character vector}
    \item{\code{study_year}}{a numeric vector}
    \item{\code{yi}}{a numeric vector}
    \item{\code{lower}}{a numeric vector}
    \item{\code{upper}}{a numeric vector}
    \item{\code{sei}}{a numeric vector}
    \item{\code{group}}{a character vector}
    \item{\code{group_secondary}}{a character vector}
    \item{\code{favours_oxytocin}}{a character vector}
    \item{\code{doses}}{a character vector}
    \item{\code{meta_analysis_pop_ind}}{a character vector}
    \item{\code{design}}{a character vector}
    \item{\code{n1}}{a numeric vector}
    \item{\code{n2}}{a numeric vector}
    \item{\code{n_total}}{a numeric vector}
    \item{\code{es_type}}{a character vector}
    \item{\code{raw_es}}{a numeric vector}
    \item{\code{ID}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Yang et al., 2021; Sabe et al., 2021; Peled-Avron et al., 2018;
Ooi et al., 2017; Leslie et al., 2018; Leppanen et al., 2018; Leppanen et
al., 2017; Keech et al., 2018; Kang et al., 2023; Huang et al., 2021; Chen
et al., 2021; Bakermans-Kranenburg et al., 2018.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ot_dat_raw)
## maybe str(ot_dat_raw) ; plot(ot_dat_raw) ...
}
\keyword{datasets}
