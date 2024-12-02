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
#' download_grades(6348, 1234, csv_export = F)
#' 

download_quiz_results <- function(course_id, quiz_id, csv_export = F){
  
  # Get student ids and names from the course to merge with the quiz results
  students_data <- get_users_in_course(course_id = course_id, enrollment_type = "Student")
  
  # Check if there are students in the course
  if(purrr::is_empty(students_data)){
    warning(paste0("There are no students in course ",course_id))
    
  } else {
    
    # Select only the columns we need to retrieve studnet numbers and merge
    students_data <- students_data %>% 
      dplyr::select(id, sis_user_id, sortable_name) %>%
      unique()
    
    # Get the quiz results based on the course and quiz id
    quizResults <- process_response(sprintf("%scourses/%s/quizzes/%s/submissions",
                                            canvas_url(), course_id, quiz_id), 
                                    list(per_page = 100, `include[]` = NULL))
    
    # Merge the students ids and names with the quiz results data
    students_data <- left_join(students_data, quizResults, by = c("id" = "user_id"))
    
    # Get rid of the columns wit irrelevant ids, tokens and urls
    quizResults <- quizResults[ , c(-1,-4,-5,-6,-7,-20,-26,-27)]
    
  }
  message("Downloading complete")
  return(students_data)
  if(csv_export == T){
    write.csv(students_data, file = paste0(getwd(),paste("/quiz", course_id, quiz_id, Sys.Date(), sep = "_"),".csv"))
  }
}
