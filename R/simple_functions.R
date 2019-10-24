#' `API Calls`
#' The functions here are all close to the basic calls in canvas. They can be easily recycled in more complicated functions later down the line.
#' The plan is to keep extending these functions over time.
#' @md
#' @name `API Calls`
NULL



#' Get all Courses in an Account
#' 
#' This functions gets all the courses in a specific account. It includes information such
#' as course id, sis id and the id of the term. You can get extra information like syllabus_body, term, course_progress, storage_quota_used_mb, total_students, teachers, account_name
#' via the include parameter.
#'
#' @param account_id the acount id (integer)
#'
#' @return a data frame of active courses in account
#' @export
#'
#' @examples
#' get_courses_in_account(123)
#' 
get_courses_in_account <- function(account_id, include = NULL) {
  
  url <-
    paste0(canvas_url(),
           paste("accounts", account_id, "courses", sep = "/"))
  
  # See the canvas api documentation for allowed values.
  args <- list(per_page = 100, `include[]` = include)
  
  dat <- process_response(url, args)
  return(dat)
}

#' Update a Course
#' 
#' This function allows for changes to all important settings of a course.
#'
#' @param course_id the canvas id of the course
#' @param syllabus_body 
#'
#' @return 
#' @export
#'
#' @examples
update_course <- function(course_id,
                          syllabus_body = NULL) {
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, sep = "/"))
  
  # Stop before posting an empty page!?
  stopifnot(!is.null(syllabus_body))

  args <- list(`course[syllabus_body]` = syllabus_body)

  dat <- canvas_query(url, args, type = "PUT")
  return(dat)
}

#' course_copy
#' 
#' This function creates a standard course copy from one course to another.
#' 
#' 
#' @param course_id The course id
#' @param source_course_id The source course id
#'
#' @return a content migration.
#' @export
#'
#' @examples
course_copy <- function(course_id,
                        source_course_id) {
  
  url <-
    paste0(canvas_url(),
           paste("courses", course_id, "content_migrations", sep = "/"))

  args <- list(migration_type = "course_copy_importer",
               `settings[source_course_id]` = source_course_id)
  
  res <- canvas_query(url, args, "POST")
  return(res)
}

#' get_course_modules
#' 
#' This function gets all the modules from a specific course.
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return a dataframe containing all the course modules
#' @export
#'
#' @examples
#' 
#' get_course_modules(6824)
#' 
get_course_modules <- function(course_id) {
  
  url <-
    paste0(canvas_url(),
           paste("courses", course_id, "modules", sep = "/"))

  args <- list(per_page = 100)

  modules <- process_response(url, args)

  return(modules)
}

#' delete_course_module
#' Deletes a specified module in a course.
#'
#' @param course_id the canvas id of the course (integer)
#' @param module_id the canvas id of the module (integer)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' delete_course_module(course_id = 6824, module_id = 412)
delete_course_module <- function(course_id, module_id) {
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, "modules", module_id, sep = "/"))

  canvas_query(url, args = NULL, "DELETE")
}

#' get_course_pages
#' Returns a data frame with all the pages in a course.
#' 
#' @param course_id the canvas id of the course (integer)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' get_course_pages(course_id = 6824)
get_course_pages <- function(course_id) {
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, "pages", sep = "/"))

  args <- list(per_page = 100)
  
  pages <-  process_response(url, args)

  return(pages)
}

#' delete_course_page
#' Deletes a specific page in a course. Does not delete pages pushed by blue_print courses
#' 
#' @param course_id
#' @param html_url the html_url of the page. Can be found in the get_course_pages return.
#'
#' @return
#' @export
#'
#' @examples
delete_course_page <- function(course_id,
                               html_url) {
  delete_page_url <- paste0(canvas_url(),
                            paste("courses", course_id, "pages", html_url, sep = "/"))

  args <- list(per_page = 100)
  res <- canvas_query(delete_page_url, args, "DELETE")
  return(res)
}


#' create_course_group_category
#'
#' @param course_id the canvas id of the course (integer)
#' @param groupcat_name the name of the group category (character/string)
#' @param self_signup allow self signup by studentes (boolean)
#' @param auto_leader automatically assign leader (boolean)
#' @param group_limit set group limit (integer)
#' @param create_group_count
#'
#' @return
#' @export
#'
#' @examples
#' 
#' create_course_group_category(course_id = 6824, groupcat_name = "Werkgroepen")
create_course_group_category <- function(course_id,
                                         groupcat_name,
                                         self_signup = NULL,
                                         auto_leader = NULL,
                                         group_limit = NULL,
                                         create_group_count = NULL) {
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, "group_categories", sep = "/"))

  args <- list(name = groupcat_name,
               self_signup = self_signup,
               auto_leader = auto_leader,
               group_limit = group_limit,
               create_group_count = create_group_count)

  sc(args)

  res <- canvas_query(url, args, "POST")
  return(res)
}

#' get_course_sections
#' Get sections information in course
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' get_course_sections(course_id = 6824)
get_course_sections <- function(course_id) {
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, "sections", sep = "/"))

  args <- list(per_page = 100)

  sections <-  process_response(url, args)

  return(sections)
}

#' create_course_group_in_category
#' 
#' Creates a group in a predefined Canvas group category
#'
#' @param groupcat_id the canvas id of the group category (integer)
#' @param name the name of the group(string/character)
#' @param description a description of the group (string/character)
#' @param is_public make the group public (boolean)
#' @param join_level
#' @param storage_quota_mb storage quota of the group page in megabytes (integer)
#' @param sis_group_id the sis id of the group (integer)
#'
#' @return
#' @export
#'
#' @examples
create_course_group_in_category <- function(groupcat_id,
                                    name,
                                    description = NULL,
                                    is_public = NULL,
                                    join_level = NULL,
                                    storage_quota_mb = NULL,
                                    sis_group_id = NULL) {
  url <-  paste0(canvas_url(),
                 paste("group_categories", groupcat_id, "groups", sep = "/"))
  
  args <- list(name = name,
               description = description,
               is_public = is_public,
               join_level = join_level,
               storage_quota_mb = storage_quota_mb,
               sis_group_id = sis_group_id)
  sc(args)
  res <- canvas_query(url, args, "POST")
  return(res)
}

#' get_section_enrollments
#'
#'This function gets all the enrollments for a specific section in a course.
#'
#' @param section_id the canvas id of the section (integer)
#'
#' @return a dataframe containing all the section enrollments 
#' @export
#'
#' @examples
#' 
#' get_section_enrollments(17234)
#' 
get_section_enrollments <- function(section_id){
  url <-  paste0(canvas_url(),
                 paste("sections", section_id, "enrollments", sep = "/"))
  
  args <- list(per_page = 100)

  res <- process_response(url, args)

  return(res)
}


#' create_group_membership
#'
#'This function is used to create a group membership for a specific user in 
#'a specific course.
#'
#' @param group_id the group id (integer)
#' @param user_id the canvas id of the user (string). Alternatively, the sis_id can
#' be used by setting "sis_login_id:" in front of the sis_id.
#'
#' @return nothing or statuscode 200 if correct
#' @export
#'
#' @examples
#' 
#' create_group_membership(group_id = 4095, user_id = "sis_login_id: pbosman")
#' 
create_group_membership <- function(group_id,
                                    user_id){
  url <-  paste0(canvas_url(),
                 paste("groups", group_id, "memberships", sep = "/"))
  
  args <- list(user_id = user_id)

  res <- canvas_query(url, args, "POST")

  return(res)
}


#' Enroll a User in a Course
#' 
#' This function is used to enroll a user in a specific course.
#' Enrollment-settings can be altered, such as silent or invited enrollment and
#' teacher or student enrollment. 
#'
#' @param course_id the course id (integer)
#' 
#' @param user_id the canvas id of the user (string). Alternatively, the sis_id can
#' be used by setting "sis_login_id:" in front of the sis_id.
#' 
#' @param enrollment_type the type of the enrollment (string). Allowed values are "StudentEnrolment",
#' "TeacherEnrollment", "TaEnrollment", ObserverEnrollment" and "DesignerEnrollment".
#' 
#' @param enrollment_state the state of the enrollment (string). Allowed values are "active", "invited"
#' and "inactive". Default is invited. N.B. if this parameter is incorrectly specified,
#' the default is used. 
#' 
#' @param course_section_id the id of the section for the enrollment. Can be located in the URL
#' when visiting the section page in the browser. 
#'
#' @return nothing if everything went correct
#' @export
#'
#' @examples
#' 
#' enroll_user_in_course(course_id = 1867, user_id = "sis_login_id:pbosman", enrollment_type = "TeacherEnrollment", enrollment_state = "inactive")
#' 
enroll_user_in_course <- function(course_id,
                                  user_id,
                                  enrollment_type = NULL,
                                  enrollment_state = NULL,
                                  course_section_id = NULL,
                                  interaction_limit = FALSE) {
  url <- paste0(canvas_url(),
                paste("courses", course_id, "enrollments", sep = "/"))

  args <- list(`enrollment[user_id]` = user_id,
               `enrollment[type]` = enrollment_type,
               `enrollment[enrollment_state]` = enrollment_state,
               `enrollment[course_section_id]` = course_section_id,
               `enrollment[limit_privileges_to_course_section] = interaction_limit`)

  res <- canvas_query(urlx = url, args = args, type = "POST")

  return(res)
}


#' Create a section in a course
#' 
#' This function creates a section in a Canvas course
#'
#' @param course_id the canvas id of the course (integer) 
#' @param name the name of the section (string/character)
#' @param sis_section_id the sis id of the section (integer)
#' @param integration_id 
#' @param start_at start date of the section
#' @param end_at end date of the section
#' @param restrict_enrollments_to_section_dates restrict enrollments to a specific date 
#' @param enable_sis_reactivation 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' create_section_in_course(course_id = 6824, name = "Werkgroep_1")
create_section_in_course <- function(course_id,
                                            name,
                                            sis_section_id = NULL,
                                            integration_id = NULL,
                                            start_at = NULL,
                                            end_at = NULL,
                                            restrict_enrollments_to_section_dates = NULL,
                                            enable_sis_reactivation = NULL) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "sections", sep = "/"))
  
  args <- list(`course_section[name]` = name,
               `course_section[sis_section_id]` = sis_section_id,
               `course_section[integration_id]` = integration_id,
               `course_section[start_at]` = start_at,
               `course_section[end_at]` = end_at,
               `course_section[restrict_enrollments_to_section_dates]` = restrict_enrollments_to_section_dates,
               enable_sis_reactivation = enable_sis_reactivation)
  sc(args)
  res <- canvas_query(url, args, "POST")
  return(res)
}

#' Get assignment analytics
#' 
#' Get assignment statistics in a course
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' get_assignments_stats_in_course(6824)
get_assignment_stats_in_course <- function(course_id) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id,"analytics", "assignments", sep = "/"))

  args <- list(per_page = 100)
  
  assignments <-  process_response(url, args)
  
  return(assignments)
}

#' Get assignments in course
#' 
#' Get all assignments information in Course
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' get_assignments_in_course(6824)
get_assignments_in_course <- function(course_id) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "assignments", sep = "/"))
  
  args <- list(per_page = 100)
  
  assignments <-  process_response(url, args)
  
  return(assignments)
}


#' Get pages in course
#' 
#' Get all page information in course
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' get_pages_in_course(6824)
get_pages_in_course <- function(course_id) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "pages", sep = "/"))
  
  args <- list(per_page = 100)
  
  pages <-  process_response(url, args)
  
  return(pages)
}

#' Show page content of a course
#'
#' @param course_id the canvas id of the course (integer)
#' @param link 
#'
#' @return
#' @export
#'
#' @examples
show_page_content_in_course <- function(course_id,link) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "pages", link, sep = "/"))
  
  args <- list(per_page = 100)
  
  pages <-  process_response(url, args)
  
  return(pages)
}

#' Update page content
#' 
#' This page updates the page content of a page. Content must be written in html.
#'
#' @param course_id the canvas id of the course (integer)
#' @param link
#' @param page_body new text written in html
#'
#' @return
#' @export
#'
#' @examples
update_page_body <- function(course_id,
                                     link,page_body) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "pages", link, sep = "/"))
  
  args <- list(`wiki_page[body]` = page_body)
  sc(args)
  resp <- httr::POST(url,
                     httr::user_agent("Ze adam - https://github.com/icto-psy"),
                     httr::add_headers(Authorization = paste("Bearer", rcanvas:::check_token())),
                     body = args)
  return(resp)
}


#' Get single assignment in course
#' 
#' Get the information of a single assignment in a course
#'
#' @param course_id the canvas id of the course (integer)
#' @param assignment_id the canvas id of the assignment (integer)
#'
#' @return
#' @export
#'
#' @examples
get_single_assignment_in_course <- function(course_id, assignment_id) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "assignments", assignment_id, sep = "/"))
  
  args <- list(per_page = 100)
  
  single_assignment <-  process_response(url, args)
  
  return(single_assignment)
}

#' Get groups in course
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return
#' @export
#'
#' @examples
get_groups_in_course <- function(course_id) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "groups", sep = "/"))
  
  args <- list(per_page = 100)
  
  groups <-  process_response(url, args)
  
  return(groups)
}

#' Get members in group
#' 
#' This functions gets user info of a group membership
#'
#' @param group_id the canvas id of the group (integer)
#'
#' @return
#' @export
#'
#' @examples
get_members_in_group <- function(group_id) {
  url <-  paste0(canvas_url(),
                 paste("groups", group_id, "memberships", sep = "/"))
  
  args <- list(per_page = 100)
  
  groups <-  process_response(url, args)
  
  return(groups)
}

#' delete_course_section
#' Deletes a specified module in a course.
#'
#' @param section_id the canvas id of the section (integer)
#'
#' @return
#' @export
#'
#' @examples
delete_section <- function(section_id) {
  
  url <- paste0(canvas_url(),
                paste("sections", section_id, sep = "/"))
  
  canvas_query(url, args = NULL, "DELETE")
}

#' delete_section_enrollments
#' 
#' Deletes a specified section_enrollment from a section in a course. This does not mean a user will be 
#' unenrolled from the course, only from the section. You are able to either conclude, delete, inactivate or
#' deactivate the enrollment.
#' 
#' @param course_id the canvas id of the course (integer)
#' @param section_id the canvas id of the section (integer)
#' @param task action to take on the enrollment
#'
#' @return
#' @export
#'
#' @examples
delete_section_enrollments <- function(course_id, enrollment_id, task) {
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, "enrollments", enrollment_id, sep = "/"))
  
  args <- list(`task` = task)
  
  canvas_query(url, args, "DELETE")
}

#' Crosslist section
#' 
#' This functions crosslist a section to a new course
#'
#' @param section_id the canvas id of the section (integer)
#' @param new_course_id the canvas id of the new course (integer)
#' 
#' @return
#' @export
#'
#' @examples
crosslist_section_to_course <- function(section_id,
                                     new_course_id) {
  url <-  paste0(canvas_url(),
                 paste("sections", sections_id, "crosslist", new_course_id, sep = "/"))
  
  args <- list(per_page = 100)
  sc(args)
  res <- canvas_query(url, args, "POST")
  return(res)
}

#' Update course settings
#' 
#' This functions update the settings of a course.
#'
#' @param course_id the canvas id of the course (integer)
#' @param allow_student_discussion_topics allow students to create discussion topics (boolean)
#' @param allow_student_forum_attachments allow students to attach files to forums (boolean)
#' @param allow_student_discussion_editing allow students to edit discussions (boolean)
#' @param allow_student_organized_groups allow students to create organize groups (boolean)
#' @param filter_speed_grader_by_student_group filter speed graders by student group (boolean)
#' @param hide_final_grades hide grade totals from students (boolean)
#' @param restrict_student_past_view Restrict students from viewing courses after end date (boolean)
#' @param restrict_student_future_view Restrict students from viewing courses before start date (boolean)
#' @param show_announcements_on_home_page show announcements on homepage (boolean)
#' @param home_page_announcement_limit set announcements limit on homepage announcements (integer)
#' 
#'
#' @return
#' @export
#'
#' @examples
update_course_settings <- function(course_id,
                                   allow_student_discussion_topics = NULL,
                                   allow_student_forum_attachments = NULL,
                                   allow_student_discussion_editing = NULL,
                                   allow_student_organized_groups = NULL,
                                   filter_speed_grader_by_student_group = NULL,
                                   hide_final_grades = NULL,
                                   hide_distribution_graphs = NULL,
                                   lock_all_announcements = NULL,
                                   restrict_student_past_view = NULL,
                                   restrict_student_future_view = NULL,
                                   show_announcements_on_home_page = NULL,
                                   home_page_announcement_limit = NULL) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "settings", sep = "/"))
  
  args <- list(`allow_student_discussion_topics` = allow_student_discussion_topics,
               `allow_student_forum_attachments` = allow_student_forum_attachments,
               `allow_student_discussion_editing` = allow_student_discussion_editing,
               `allow_student_organized_groups` = allow_student_organized_groups,
               `filter_speed_grader_by_student_group` = filter_speed_grader_by_student_group,
               `hide_final_grades` = hide_final_grades,
               `hide_distribution_graphs` = hide_distribution_graphs,
               `lock_all_announcements` = lock_all_announcements,
               `restrict_student_past_view` = restrict_student_past_view,
               `restrict_student_future_view` = restrict_student_future_view,
               `show_announcements_on_home_page` = show_announcements_on_home_page,
               `home_page_announcement_limit` = home_page_announcement_limit)
  sc(args)
  resp <- canvas_query(url,args,"PUT")
  return(resp)
}


#' Get users in account
#'
#' @param account_id the canvas id of the account (integer)
#' @param enrollment_type the canvas enrollment type. User either "student", "teacher", "ta", "observer" or "designer" (character)
#'
#' @return
#' @export
#'
#' @examples
get_users_in_account <- function(account_id, enrollment_type = NULL) {
  url <-  paste0(canvas_url(),
                 paste("accounts", account_id, "users", sep = "/"))
  
  args <- list(`enrollment_type` = enrollment_type,
               per_page = 100)
  
  users <-  process_response(url, args)
  
  return(users)
}



