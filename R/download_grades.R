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

download_grades <- function(course_id, csv_export = F){
  
  students_data <- get_users_in_course(course_id = course_id, enrollment_type = "Student")
  
  if(purrr::is_empty(students_data)){
    warning(paste0("There are no students in course ",course_id))
    
  } else{
    students_data <- students_data %>% 
      dplyr::select(id, sis_user_id, sortable_name) %>%
      unique()
    
    assignments <- get_assignments_in_course(course_id)
    
    duplicate_names <- which(duplicated(assignments$name))
    
    if(length(duplicate_names) > 0){
      for(j in 1:length(duplicate_names)){
        assignments$name[duplicate_names[j]] <- paste(assignments$name[duplicate_names[j]],j, sep = "_") 
      }
    }
    
    
    for(i in 1 : nrow(assignments)){
      grade_data <- get_assignment_submissions(course_id, assignments$id[i])
      if(purrr::is_empty(grade_data)){
        cat("Assignment ", assignments$name[i]," doesn't contain any grades or submissions. Skipping the assignment. \n")
        students_data[,3+i] <- NA
        names(students_data)[3+i] <- assignments$name[i]
        message(cat(paste0("Downloading gradebook ", floor(i/nrow(assignments)*100), "%")))} else{
          grade_data <- dplyr::select(grade_data,user_id,grade)
          students_data <- left_join(students_data,grade_data, by = c("id" = "user_id"))
          names(students_data)[3+i] <- assignments$name[i]
          message(cat(paste0("Downloading gradebook ", floor(i/nrow(assignments)*100), "%")))
        }
    }
  }
  message("Downloading complete")
  return(students_data)
  if(csv_export == T){
    write.csv(students_data, file = paste0(getwd(),paste("/gradebook", course_id, Sys.Date(), sep = "_"),".csv"))
  }
}
