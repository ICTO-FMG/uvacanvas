% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_functions (conflicted copy 2021-04-14
%   155304).R, R/simple_functions.R
\name{delete_enrollment}
\alias{delete_enrollment}
\title{Delete enrollment}
\usage{
delete_enrollment(course_id, task = NULL, user_id)

delete_enrollment(course_id, task = NULL, user_id)
}
\arguments{
\item{course_id}{the canvas id of the course (integer)}

\item{task}{action to take on enrollment. Allowed values: conclude, delete, inactivate, deactivate. Default is delete (string)}

\item{user_id}{canvas user id}
}
\value{
server response. Either 200 status code if everything went correctly or a specific http status warning.

server response. Either 200 status code if everything went correctly or a specific http status warning.
}
\description{
Conclude deactivate or delete an enrollment

Conclude deactivate or delete an enrollment
}
