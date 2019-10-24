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
  
  section_enrollments <- section_enrollments$id
  
  lapply(section_enrollments,delete_section_enrollments, course_id = course_id, task = "delete")
  
  delete_section(course_sections[i])
  }
}
