#' Delete sections from a course
#' 
#' This function allows to completely delete user created sections from a course. SIS created section will remain intact.
#' The function first empties a section by deleting all section enrollments and then removing the section. 
#' Please note that this will not delete a user from a course.
#' The function allows to either delete all sections from a course by default, or a specified id's 
#' 
#' @param course_id the canvas id of the course (integer)
#' @param section_ids all by default or specify by entering canvas ids of the sections
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
#' 
#' delete_sections(6348)


delete_sections <- function(course_id, section_ids = "all"){
  
  course_sections <- NULL

if(section_ids == "all"){  
  course_sections <- get_course_sections(course_id = course_id) %>%
  filter(!complete.cases(sis_section_id)) 
  course_sections <- course_sections$id
} else {
  course_sections <-  section_ids
  }

for(i in seq_along(course_sections)){
  
  section_enrollments <- get_section_enrollments(course_sections[i]) 
  section_enrollments <- section_enrollments$id
  
  lapply(section_enrollments,delete_section_enrollments, course_id = course_id, task = "delete")
  
  delete_course_section(course_sections[i])
  }
}
