% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refresh_blueprint_courses_after_course_copy.R
\name{refresh_blueprint_course_after_course_copy}
\alias{refresh_blueprint_course_after_course_copy}
\title{Refresh blueprint courses after course copy}
\usage{
refresh_blueprint_course_after_course_copy(blueprint_id)
}
\arguments{
\item{blueprint_id}{the canvas course id of the blueprint course (integer)}
}
\value{

}
\description{
This function refreshes courses after course copy of blueprint content. After course copy, blueprint associations with
new created courses will not exist. However, the course content is copied. Associating the newly created courses again with the blueprint
will results in duplicated course pages and faulty internal course links. This script deletes all blueprint course content from associated pages
and then pushes content again. The results are unique blueprint content in the copied courses.
}
\examples{

refresh_blueprint_courses_after_course_copy(675)

}
