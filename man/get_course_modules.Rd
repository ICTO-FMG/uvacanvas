% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_functions.R
\name{get_course_modules}
\alias{get_course_modules}
\title{get_course_modules}
\usage{
get_course_modules(course_id)
}
\arguments{
\item{course_id}{the canvas id of the course (integer)}
}
\value{
a dataframe containing all the course modules
}
\description{
This function gets all the modules from a specific course.
}
\examples{

get_course_modules(6824)

}
