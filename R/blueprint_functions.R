#' Get Blueprint Information
#' 
#' Get information about the blueprint page
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param template_id Using 'default' as the template_id should suffice for the current implmentation (as there should be only one template per course). However, using specific template ids may become necessary in the future
#' 
#' @export
#' @return a dataframe containing information about the Blueprint
#' 
#' @example 

get_blueprint_info <- function(course_id, template_id = "default"){
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, "blueprint_templates", template_id, sep = "/"))
  
  args <- list()
  
  blueprint_info <- canvas_query(url,args)
  
  return(blueprint_info)
}

#' Get Blueprint Associated courses
#' 
#' Get information about the blueprint associated courses
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param template_id Using 'default' as the template_id should suffice for the current implmentation (as there should be only one template per course). However, using specific template ids may become necessary in the future
#' 
#' @export
#' @return a dataframe containing information about the Blueprint
#' 
#' @example 

get_blueprint_associated_courses <- function(course_id, template_id = "default"){
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, "blueprint_templates", template_id, "associated_courses",sep = "/"))
  
  args <- list(per_page = 100)
  
  blueprint_associated_courses <- process_response(url,args)
  
  return(blueprint_associated_courses)
}
