#' Download Grades
#' 
#' This functions downloads results from a specific quiz and specific course
#' 
#' @param course_id the canvas id of the course (integer)
#' @param quiz_id the canvas id of the quiz (integer)
#' @param csv_export indicate if you want a csv export to be saved (boolean)
#'
#' @return a dataframe and/or csv export of the gradebook
#' @export
#'
#' @examples
#' 
#' download_grades(6348, csv_export = F)
#' 

download_quiz_results <- function(course_id, quiz_id, csv_export = F){
  
  students_data <- get_users_in_course(course_id = course_id, enrollment_type = "Student")
  
  if(purrr::is_empty(students_data)){
    warning(paste0("There are no students in course ",course_id))
    
  } else {
    
    # Select only the columns we need to retrieve studnet numbers and merge
    students_data <- students_data %>% 
      dplyr::select(id, sis_user_id, sortable_name) %>%
      unique()
    
    # Get the quiz results based on the course and quiz id
    quizResults <- process_response(sprintf("%scourses/%s/quizzes/%s/submissions",
                                            canvas_url, course_id, quiz_id), 
                                    list(per_page = 100, `include[]` = NULL))
    
    # Merge the students data with the quiz results data
    students_data <- left_join(students_data,grade_data, by = c("id" = "user_id"))
    
  }
  message("Downloading complete")
  return(students_data)
  if(csv_export == T){
    write.csv(students_data, file = paste0(getwd(),paste("/gradebook", course_id, Sys.Date(), sep = "_"),".csv"))
  }
}
