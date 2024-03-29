% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_functions (conflicted copy 2021-04-14
%   155304).R, R/simple_functions.R
\name{create_page}
\alias{create_page}
\title{Create page}
\usage{
create_page(
  course_id,
  page_title = NULL,
  page_body = NULL,
  published = T,
  front_page = NULL
)

create_page(
  course_id,
  page_title = NULL,
  page_body = NULL,
  published = T,
  front_page = NULL
)
}
\arguments{
\item{course_id}{the canvas id of the course (integer)}

\item{page_title}{new page title (character)}

\item{page_body}{new text written in html (character)}

\item{published}{is page published (boolean)}

\item{front_page}{make this page front page (boolean)}
}
\value{
server response. Either 200 status code if everything went correctly or a specific http status warning.

server response. Either 200 status code if everything went correctly or a specific http status warning.
}
\description{
This page updates the page content of a page. Content must be written in html.

This page updates the page content of a page. Content must be written in html.
}
