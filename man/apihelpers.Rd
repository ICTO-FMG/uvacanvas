% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uva_utils.R
\name{apihelpers}
\alias{apihelpers}
\alias{set_canvas_token}
\alias{set_canvas_domain}
\alias{check_token}
\title{Canvas API helpers}
\usage{
set_canvas_token(token)

set_canvas_domain(domain)

check_token()
}
\arguments{
\item{token}{your API token}

\item{domain}{Canvas domain}
}
\description{
These functinos set your Canvas API token, as well as the Canvas base URL.
These functions are necessary for \code{rcanvas} to run.
}
\examples{
set_canvas_token("abc123")
set_canvas_domain("https://canvas.upenn.edu")
}
