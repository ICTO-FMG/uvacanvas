#' Download Grades
#' 
#' This functions downloads grades on assignments from the gradebook of a specific course
#' 
#' @param course_id the canvas id of the course (integer)
#' @param csv_export indicate if you want a csv export to be saved (boolean)
#'
#' @return a dataframe and/or csv export of the gradebook
#' @export
#'
#' @examples
#' 
#' download_grades(6348, csv_export = F)
#' 

download_grades <- function(course_id, csv_export = T){ 
  
  students_data <- get_users_in_course(course_id = course_id, enrollment_type = "StudentEnrollment") %>%
    select(user_id, sis_user_id, user.sortable_name) %>%
    unique()
  
  assignments <- get_assignments_in_course(course_id)
  
  duplicate_names <- which(duplicated(assignments$name))
  
  if(length(duplicate_names) > 0){
    for(j in 1:length(duplicate_names)){
     assignments$name[duplicate_names[j]] <- paste(assignments$name[duplicate_names[j]],j, sep = "_") 
    }
  }
  
 for(i in 1 : nrow(assignments)){
    grade_data <- get_assignment_submissions(course_id, assignments$id[i]) %>%
      select(user_id,grade)
    students_data <- left_join(students_data,grade_data, by = "user_id")
    names(students_data)[3+i] <- assignments$name[i]
    message(paste0("Downloading gradebook ", floor(i/nrow(assignments)*100), "%"))
  }
  
  message("Downloading complete")
  
if(csv_export == T){
    write.csv(students_data, file = paste0(getwd(),paste("/gradebook", course_id, sys.date(), sep = "_"),".csv"))
  }

  return(students_data)

   
}