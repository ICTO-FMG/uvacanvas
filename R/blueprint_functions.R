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

#' Update associated blueprint courses
#' 
#' Updates blueprint associations
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param template_id template id of the blueprint use "default" as template_id
#' @param course_ids_to_add course id's to add to associations (vector containing integers)
#' @param course_ids_to_remove course id's to remove from associations (vector containing integers)
#' 
#' @export
#' @return server response
#' 
#' @example 
#' update_blueprint_associations(12345, "default", course_ids_to_add = c(123456,654321))

update_blueprint_associations <- function(course_id, template_id = "default", course_ids_to_add = NULL, course_ids_to_remove = NULL){
  url <- paste0(canvas_url(),
                paste("courses", course_id, "blueprint_templates", template_id,"update_associations", sep = "/"))
  
  args <- list(`course_ids_to_add` = course_ids_to_add,
               `course_ids_to_remove` = course_ids_to_remove)
  
  blueprint_update <- canvas_query(url,args, "PUT")
  
  return(blueprint_update)
}

#' Push Blueprint
#' 
#' Push blueprint pages to associated courses
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param template_id template id of the blueprint use "default" as template_id
#' @param comment an optional comment to be included in the sync history (string)
#' @param send_notification Send a notification to the calling user when the sync completes (boolean)
#' @param copy_settings Whether course settings should be copied over to associated courses. Defaults to true for newly associated courses (boolean)
#' @param publish_after_initial_sync If set, newly associated courses will be automatically published after the sync completes (boolean)
#' 
#' @export
#' @return server response
#' 
#' @example 
#' push_blueprint(12345, "default", comment = "Changed home page content.", send_notification = F, copy_settings = T, publish_afther_initial_sync = F)

push_blueprint <- function(course_id, template_id = "default", comment = NULL, send_notification = F, copy_settings = F, publish_after_initial_sync = F){
  url <- paste0(canvas_url(),
                paste("courses", course_id, "blueprint_templates", template_id,"migrations", sep = "/"))
  
  args <- list(`comment` = comment,
               `send_notification` = send_notification,
               `copy_settings` = copy_settings,
               `publish_after_initial_sync` = publish_after_initial_sync)
  
  push_done <- canvas_query(url,args, "POST")
  
  return(push_done)
}