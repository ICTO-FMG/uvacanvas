hand_in_date_check <- function(course_id){
  
  users <- get_users_in_course(course_id = course_id)
  users <- users[,grep("^sis_user_id|^id|^name",names(users))]
  colnames(users)[1] <- 'user_id'
  
  data <- get_assignments_in_course(course_id = course_id)
  print(data$name)
  assignment_number <- as.integer(readline("Select the assignment you would like data from: "))
  cat("You have selected:", data$name[assignment_number])
  assignment_data <- get_assignment_submissions(course_id = course_id, assignment_id = data$id[assignment_number])
  assignment_data <- assignment_data[,grep(c("user_id|submitted_at"),names(assignment_data))]
  
  assignment_data <- dplyr::left_join(assignment_data,users, by = "user_id")
  assignment_data <- dplyr::select(assignment_data, c("sis_user_id", "name", "submitted_at"))
  
  return(assignment_data)
}