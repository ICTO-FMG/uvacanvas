#' Refresh blueprint courses after course copy
#' 
#' This function refreshes courses after course copy of blueprint content. After course copy, blueprint associations with
#' new created courses will not exist. However, the course content is copied. Associating the newly created courses again with the blueprint
#' will results in duplicated course pages and faulty internal course links. This script deletes all blueprint course content from associated pages
#' and then pushes content again. The results are unique blueprint content in the copied courses.
#' FIRST ADD THE NEW COURSES TO THE BLUEPRINT AND THEN RUNN THIS FUNCTION
#' 
#' @param blueprint_id the canvas course id of the blueprint course (integer)
#'
#' @return 
#' @export
#'
#' @examples
#' 
#' refresh_blueprint_courses_after_course_copy(675)
#' 


refresh_blueprint_course_after_course_copy <- function(blueprint_id){ #get information about all associated courses
  
  info <- get_blueprint_associated_courses(blueprint_id)
  
  # get all information about pages in the blueprint
  
  blueprint_pages <- get_course_pages(blueprint_id)
  
  # start removing double pages
  
  for(i in 1:nrow(info)){
    # remove course as associated course to allow for page removal
    update_blueprint_associations(course_id = blueprint_id, template_id = "default", course_ids_to_remove = c(info$id[i]))
    create_page(course_id = info$id[i],
                page_title = "Under Construction", 
                page_body = "<h3>This page is under maintenance and therefor temporarily unavailable. Course content under Modules is still accessible.</h3>",
                front_page = T)
    
    # get all page info from course to select which page to remove
    course_pages <- get_course_pages(info$id[i])
    
    for(j in 1:nrow(blueprint_pages)){
      
      # select which pages have to be deleted
      pages_to_delete <- which(grepl(paste0("^",blueprint_pages$title[j]),course_pages$title))
      
      # start deleting pages
      for(k in pages_to_delete){
        resp <- delete_course_page(info$id[i],course_pages$url[k])
      }
    }
    
    # add course as associated course
    update_blueprint_associations(course_id = blueprint_id, template_id = "default", course_ids_to_add = c(info$id[i]))
  }
  
  Sys.sleep(60)
  
  push_blueprint(course_id = blueprint_id, 
                 template_id = "default", 
                 copy_settings = T, 
                 comment = "Content reset after course copy", 
                 publish_after_initial_sync = F, 
                 send_notification = F)
  
  for(l in 1:nrow(info)){
    set_front_page(course_id = info$id[1], link = "homepage",front_page = T)
    delete_course_page(info$id[l],"Under Construction")}

  } 

