% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_functions (conflicted copy 2021-04-14
%   155304).R, R/simple_functions.R
\name{delete_section_enrollments}
\alias{delete_section_enrollments}
\title{Delete section enrollments}
\usage{
delete_section_enrollments(course_id, enrollment_id, task)

delete_section_enrollments(course_id, enrollment_id, task)
}
\arguments{
\item{course_id}{the canvas id of the course (integer)}

\item{enrollment_id}{the canvas user_id of the section enrollment (integer)}

\item{task}{action to take on the enrollment "conclude", "delete", "inactivate" or "deactive" (character)}
}
\value{
server response. Either 200 status code if everything went correctly or a specific http status warning.

server response. Either 200 status code if everything went correctly or a specific http status warning.
}
\description{
Deletes a specified section_enrollment from a section in a course. This does not mean a user will be 
unenrolled from the course, only from the section. You are able to either conclude, delete, inactivate or
deactivate the enrollment.

Deletes a specified section_enrollment from a section in a course. This does not mean a user will be 
unenrolled from the course, only from the section. You are able to either conclude, delete, inactivate or
deactivate the enrollment.
}
\examples{

 delete_section_enrollments(6348, 12345, "delete")

 delete_section_enrollments(6348, 12345, "delete")
}
