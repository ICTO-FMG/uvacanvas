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
#' @param account_id the account id (integer)
#' @param include  "syllabus_body", "term", "course_progress", "storage_quota_used_mb", "total_students", "teachers", "account_name" (character)
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
#' This function allows for changes to the syllabus body.
#'
#' @param course_id the canvas id of the course (integer)
#' @param syllabus_body the syllabus text body written in html (character)
#' @param default_view change default view "modules" or "wiki"(character)
#'
#' @return 
#' @export
#'
#' @examples
update_course <- function(course_id,
                          syllabus_body = NULL,
                          default_view = NULL) {
  
  url <- paste0(canvas_url(),
                paste("courses", course_id, sep = "/"))
  
  # Stop before posting an empty page!?

  args <- list(`course[syllabus_body]` = syllabus_body,
               `course[default_view]` = default_view)

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
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
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
#' @return A dataframe containing course page information.
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
#' @param course_id the canvas id of the course (integer)
#' @param html_url the html_url of the page. Can be found in the get_course_pages return. (character)
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
delete_course_page <- function(course_id,
                               html_url) {
  url <- paste0(canvas_url(),
                            paste("courses", course_id, "pages", html_url, sep = "/"))

  args <- list(per_page = 100)
  res <- canvas_query(url, args, "DELETE")
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
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
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
#' @return A dataframe containing information about section in the specified course.
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
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
#' 
#' create_course_group_in_category(6348, "Werkgroepen_1")
#' create_course_group_in_category(groupcat_id = 6348, name = "Werkgroepen_1", is_public = T, storage_quota_mb = 20)
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
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
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
#' @param interaction_limit limit interaction with users to section only.
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
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
               `enrollment[limit_privileges_to_course_section]` = interaction_limit)

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
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
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
#' @return A dataframe containing assignment statistics
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
#' @return A dataframe containing assignment information of all assignments from the specified course
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


#' Show page content of a course
#' 
#' Gets page content of a course.
#'
#' @param course_id the canvas id of the course (integer)
#' @param link  name of the page (character)
#'
#' @return A dataframe containing page content in html format.
#' @export
#'
#' @examples
#' 
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
#' @param link url link/ name of the page (character)
#' @param page_body new text written in html (character)
#' @param page_title new text written in html (character)
#' @param front_page make this page front page (boolean)
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
update_page <- function(course_id,
                        link = NULL,
                        page_title = NULL,
                        page_body = NULL,
                        front_page = NULL) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "pages", link, sep = "/"))
  
  args <- list(`wiki_page[body]` = page_body,
               `wiki_page[title]` = page_title,
               `wiki_page[front_page]` = front_page)
  sc(args)
  resp <- canvas_query(url, args, "PUT")
  return(resp)
}


#' Get single assignment in course
#' 
#' Get the information of a single assignment in a course
#'
#' @param course_id the canvas id of the course (integer)
#' @param assignment_id the canvas id of the assignment (integer)
#'
#' @return A dataframe containing information about a specified assignment from a specified course.
#' @export
#'
#' @examples
#' 
#' get_single_assingment_in_course(6348, 10234)
get_single_assignment_in_course <- function(course_id, assignment_id) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "assignments", assignment_id, sep = "/"))
  
  args <- list(per_page = 100)
  
  single_assignment <-  process_response(url, args)
  
  return(single_assignment)
}

#' Get groups in course
#'
#' This function returns data about all groups in a specified course
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return A dataframe containing information about all groups in a course
#' @export
#'
#' @examples
#' 
#' get_groups_in_course(6348)
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
#' @return A dataframe with information about members of a specified group
#' @export
#'
#' @examples
#' 
#' get_members_in_group(11267)
get_members_in_group <- function(group_id) {
  url <-  paste0(canvas_url(),
                 paste("groups", group_id, "memberships", sep = "/"))
  
  args <- list(per_page = 100)
  
  groups <-  process_response(url, args)
  
  return(groups)
}

#' Delete course section
#' 
#' Deletes a specified section in a course.
#'  
#' PLEASE NOTE: To delete a course section, the section needs to be empty, This function does not empty the section. Use delete_sections for this task.
#' 
#' @param section_id the canvas id of the section (integer)
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
#' 
#' delete_course_section(12345)
delete_course_section <- function(section_id) {
  
  url <- paste0(canvas_url(),
                paste("sections", section_id, sep = "/"))
  
  canvas_query(url, args = NULL, "DELETE")
}

#' Delete section enrollments
#' 
#' Deletes a specified section_enrollment from a section in a course. This does not mean a user will be 
#' unenrolled from the course, only from the section. You are able to either conclude, delete, inactivate or
#' deactivate the enrollment.
#' 
#' @param course_id the canvas id of the course (integer)
#' @param enrollment_id the canvas user_id of the section enrollment (integer)
#' @param task action to take on the enrollment "conclude", "delete", "inactivate" or "deactive" (character)
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
#' 
#'  delete_section_enrollments(6348, 12345, "delete")
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
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
#' 
#' crosslist_section_to_course(12345, 6348)
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
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
#' 
#' update_course_settings(6348, hide_final_grades = T, hide_distribution_graphs = T)
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
#' Gets all user information from a specified account. You can use the argument enrollment_type to set a specific filter to user type, to save some time.
#'
#' @param account_id the canvas id of the account (integer)
#' @param enrollment_type the canvas enrollment type. Use either "student", "teacher", "ta", "observer" or "designer" (character)
#'
#' @return A dataframe containing information about users in the specified subaccount
#' @export
#'
#' @examples
#' 
#' get_users_in_account(123, "teacher")
get_users_in_account <- function(account_id, enrollment_type = NULL) {
  url <-  paste0(canvas_url(),
                 paste("accounts", account_id, "users", sep = "/"))
  
  args <- list(`enrollment_type` = enrollment_type,
               per_page = 100)
  
  users <-  process_response(url, args)
  
  return(users)
}

#' Get users in course
#' 
#' Gets all user information from a specified course. You can use the argument enrollment_type to set a specific filter to user type 
#' or the argument enrollment_state for a specific enrollment state of the user, to save some time.
#'
#' @param course_id the canvas id of the course (integer)
#' @param enrollment_type the canvas enrollment type to return. Use either "StudentEnrollment", "TeacherEnrollment". Default returns all roles. (character)
#' @param enrollment_state include users with a specific state. Use either "active", "invited", "deleted" or "completed". Default is "active"  and "invited" (character)
#' @return A dataframe containing information about users in the specified course.
#' @export
#'
#' @examples
#'
#' get_users_in_course(6348, enrollment_type = "student", enrollment_state = "active")
get_users_in_course <- function(course_id, enrollment_type = NULL, enrollment_state = NULL, include = "enrollments") {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "users", sep = "/"))
  
  args <- list(`enrollment_type[]` = enrollment_type,
               `enrollment_state[]` = enrollment_state,
               `include[]`= include)
  
  users <-  process_response(url, args)
  
  return(users)
}

#' Set courses dates
#' 
#' This function sets course starting date and end date
#' 
#' @param course_id the canvas id of the course (integer)
#' @param start_date starting date in ISO DateTime format or use NA for no date (character)
#' @param end_date end date in ISO DateTime format or use NA for no date (character)
#' 
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' 
#' @export
#' 
#' @examples 
#' 
#' set_course_dates(404, start_date = "2019-01-01T00:00Z")
#' set_course_dates(404, start_date = NA, end_date = NA )
set_course_dates <- function(course_id, start_date = NULL, end_date = NULL){
  url <- paste0(canvas_url(),
                paste("courses", course_id, sep = "/"))
  
  args <- list(`course[start_at]` = start_date,
               `course[end_at]` = end_date)
  
  settings <- canvas_query(url,args, "PUT")
  
  return(settings)
}

#' Set course name
#' 
#' This function sets the course name
#' 
#' @param course_id the canvas id of the course (integer)
#' @param name the new name of the course (character)
#' 
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' 
#' @export
#' 
#' @examples 
#' 
#' set_course_name(404, name = "My course name")

set_course_name <- function(course_id, name){
  url <- paste0(canvas_url(),
                paste("courses", course_id, sep = "/"))
  
  args <- list(`course[name]` = name)
  
  settings <- canvas_query(url,args, "PUT")
  
  return(settings)
}

#' Get user details
#' 
#' Get information about a user
#' 
#' @param user_id The canvas id of the user(integer) 
#' @param include Include arguments: "email"
#' 
#' @export
#' @return
#' 
#' @example 
get_user_details <- function(user_id, include = NULL){
  url <- paste0(canvas_url(),
               paste("users", user_id, sep = "/"))
  
  args <- list(`include[]` = include, 
               per_page = 100)
  
  details <- process_response(url, args)
  
  return(details)
}

#' List course files
#' 
#' List all files in a course
#' 
#' @param course_id The canvas id of the course (integer)
#' @param include Include user information to the data. (character) 
#' 
#' @export
#' @return A dataframe with information about all files in a course.
#' 
#' @example list_course_files(6348, "user")
list_course_files <- function(course_id, include = NULL){
  url <- paste0(canvas_url(),
                paste("courses", course_id,"files", sep = "/"))
  
  args <- list(`include[]` = include,
               per_page = 100)
  
  files <- process_response(url,args)
  
  return(files)
}


#' Get assignment submissions
#' 
#' Get all submission info on a assignment
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param assignment_id the canvas id of the assignment (integer)
#' @param include rubric_assessment
#' 
#' @export
#' @return a paginated list 
#' 
#' @example 
get_assignment_submissions <- function(course_id, assignment_id, include = NULL){
  url <- paste0(canvas_url(),
                paste("courses", course_id, "assignments", assignment_id, "submissions", sep = "/"))
  
  args <- list(per_page = 100,
               `include[]`= include)
  
  submissions <- process_response(url, args)
  
  return(submissions)
}

#' Get course tabs
#' 
#' Get tabs from course
#' 
#' @param course_id the canvas id of the course_id (integer)
#' 
#' @export
#' @return a paginated list 
#' 
#' @example 
get_course_tabs <- function(course_id){
  url <- paste0(canvas_url(),
                paste("courses", course_id, "tabs", sep = "/"))
  
  args <- list(per_page = 100)
  
  tabs <- process_response(url, args)
  
  return(tabs)
}

#' Update course tabs
#' 
#' Update tabs from course
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param tab_id the canvas id of the tab (integer)
#' @param position the position of the tab in the course (integer)
#' @param hidden make tab hidden or not (boolean)
#' 
#' @export
#' @return a paginated list 
#' 
#' @example 
update_course_tabs <- function(course_id, tab_id, position, hidden){
  url <- paste0(canvas_url(),
                paste("courses", course_id, "tabs", tab_id, sep = "/"))
  
  args <- list(`position` = position,
               `hidden`= hidden)
  
  tabs_up <- canvas_query(url, args, "PUT")
  
  return(tabs_up)
}

#' Grade Assignment
#' 
#' Grade Assignment
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param assignment_id the canvas id of the assignment id (integer)
#' @param user_id the canvas id of the user (integer)
#' @param grade the grade example "complete" (string)
#' @param excuse excuse 
#' 
#' @export
#' @return a server status report
#' 
#' @example 
grade_assignment <- function(course_id, assignment_id, user_id, grade = NULL, excuse = F){
  url <- paste0(canvas_url(),
                paste("courses", course_id, "assignments", assignment_id, "submissions", user_id, sep = "/"))
  
  args <- list(`submission[posted_grade]` = grade,
               `submission[excuse]` = excuse)
  
  grades <- canvas_query(url,args, "PUT")
  
  return(grades)
}

#' Get Course Rubric
#' 
#'Get information about a rubric
#' 
#' @param course_id the canvas id of the course_id (integer)
#' @param rubric_id the canvas id of the assignment id (integer)
#' 
#' @export
#' @return dataframe with course info
#' 
#' @example 
#' get_course_rubric(12345,435)

get_course_rubric <- function(course_id, rubric_id){
  url <- paste0(canvas_url(),
                paste("courses", course_id,"rubrics",rubric_id, sep = "/"))
  
  args <- list(per_page = 100)
  
  rubrics <- process_response(url,args)
  
  return(rubrics)
}

#' List Rubrics in course
#' 
#' List all rubrics in a course
#' 
#' @param course_id the canvas id of the course_id (integer)
#' 
#' @export
#' @return dataframe with rubrics info
#' 
#' @example 
#' list_course_rubrics(12345)

list_course_rubrics <- function(course_id){
  url <- paste0(canvas_url(),
                paste("courses", course_id,"rubrics", sep = "/"))
  
  args <- list(per_page = 100)
  
  rubrics_list <- process_response(url,args)
  
  return(rubrics_list)
}



#' Create page
#' 
#' This page updates the page content of a page. Content must be written in html.
#'
#' @param course_id the canvas id of the course (integer)
#' @param page_body new text written in html (character)
#' @param page_title new page title (character)
#' @param front_page make this page front page (boolean)
#' @param published is page published (boolean)
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
create_page <- function(course_id,
                        page_title = NULL,
                        page_body = NULL,
                        published = T,
                        front_page = NULL) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "pages", sep = "/"))
  
  args <- list(`wiki_page[body]` = page_body,
               `wiki_page[title]` = page_title,
               `wiki_page[published]` = published,
               `wiki_page[front_page]` = front_page)
  
  resp <- canvas_query(url, args, "POST")
  return(resp)
}

#' Set front page
#' 
#' Set front page
#'
#' @param course_id the canvas id of the course (integer)
#' @param link url link/ name of the page (character)
#' @param front_page make this page front page (boolean)
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
set_front_page <- function(course_id,
                        link = NULL,
                        front_page = NULL) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "pages", link, sep = "/"))
  
  args <- list(`wiki_page[front_page]` = front_page)
  
  resp <- canvas_query(url, args, "PUT")
  return(resp)
}

#' Delete enrollment
#' 
#' Conclude deactivate or delete an enrollment
#'
#' @param course_id the canvas id of the course (integer)
#' @param task action to take on enrollment. Allowed values: conclude, delete, inactivate, deactivate. Default is delete (string)
#' @param user_id canvas user id
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
delete_enrollment <- function(course_id, task = NULL, user_id){
  url <- paste0(canvas_url(),
                paste("courses", course_id, "enrollments", user_id, sep = "/"))
  
  args <- list(`task`= task)
  
  resp <- canvas_query(url, args, "DELETE")
  return(resp)
}

#' Create course
#' 
#' Create a new course
#'
#' @param account_id the id of the account in which you want to create the course
#' @param course_name the name of the new (string)
#' @param course_code the course code/reference code for the course (string)
#' @param restrict_enrollments_to_course_dates set to true to restrict user enrollments to the start and end dates of the course
#' @param term_id the ID of the term to create the course in (integer)
#'
#' @return server response. Either 200 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
create_course <- function(account_id, 
                          course_name, 
                          course_code, 
                          restrict_enrollments_to_course_dates = TRUE, 
                          term_id){
  url <- paste0(canvas_url(),
                paste("accounts", account_id, "courses", sep = "/"))
  
  args <- list(`course[name]`= course_name,
               `course[course_code]`= course_code,
               `course[restrict_enrollments_to_course_dates]`= restrict_enrollments_to_course_dates,
               `course[term_id]`= term_id)
  
  resp <- canvas_query(url, args, "POST")
  return(resp)
}

#' Get enrollment terms
#' 
#' List all enrollment terms
#' 
#' @param account_id the canvas id of the course_id (integer)
#' 
#' @export
#' @return dataframe with enrollment term info
#' 
#' @example get_enrollment_terms()
get_enrollment_terms <- function(account_id = 1){
  url <- paste0(canvas_url(),
                paste("accounts", account_id,"terms", sep = "/"))
  
  args <- list(per_page = 100)
  
  terms_list <- process_response(url,args)
  
  return(terms_list)
}

#' Get assignment groups in course
#' 
#' Get all assignment groups in Course
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return A dataframe containing the assignment group information from the specified course
#' @export
#'
#' @example get_assignmentgroups_in_course(6824)
get_assignmentgroups_in_course <- function(course_id) {
  url <-  paste0(canvas_url(),
                 paste("courses", course_id, "assignment_groups", sep = "/"))
  
  args <- list(per_page = 100)
  
  assignments <-  process_response(url, args)
  
  return(assignments)
}

#' Get course information
#' 
#' Get course information
#'
#' @param course_id the canvas id of the course (integer)
#'
#' @return A dataframe containing information from the specified course
#' @export
#'
#' @examples
#' 
#' get_course_info(6824)
get_course_info <- function(course_id, include = NULL){
  url <- paste0(canvas_url(),
                paste("courses", course_id, sep = "/"))
  
  args <- list(per_page = 100, `include[]` = include)
  
  settings <- process_response(url,args)
  
  return(settings)
}

#' Create assignment
#' 
#' Create a new assignment in course
#'
#' @param course_id canvas id of the course (integer)
#' @param assignment_name name of the assignment (string)
#' @param position position of the assignment in the list of assignments. Default to 0 -> end of the list (integer)
#' @param submission_type online_quiz/none/on_paper/discussion_topic/external_tool (string)
#' @param notify_of_update notifies student of changes if TRUE (boolean)
#' @param points_possible max points possible, default is 10 (integer)
#' @param grading_type pass_fail/percent/letter_grade/gpa_scale/points/not_graded (string)
#' @param published is the assignment published, default is False (boolean)
#' @return server response. Either 201 status code if everything went correctly or a specific http status warning.
#' @export
#'
#' @examples
create_assignment <- function(course_id,
                          assignment_name, 
                          position = NULL, 
                          submission_type = "none", 
                          notify_of_update = F,
                          points_possible = 10,
                          grading_type = "points",
                          published = F){
  url <- paste0(canvas_url(),
                paste("courses",course_id,"assignments", sep = "/"))
  
  args <- list(`assignment[name]`= assignment_name,
               `assignment[position]`= position,
               `assignment[submission_types][]`= submission_type,
               `assignment[notify_of_update]`= notify_of_update,
               `assignment[points_possible]` = points_possible,
               `assignment[grading_type]` = grading_type,
               `assignment[published]` = published)
  
  resp <- canvas_query(url, args, "POST")
  return(resp)
}
