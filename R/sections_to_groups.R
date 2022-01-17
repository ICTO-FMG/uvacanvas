#' Sections to Groups
#' 
#' @description This function turns sections in to group in a Canvas course
#' 
#' @param course_id the canvas id of the course (integer)
#' @param groupcat_name the name of the group category (string)
#' @param skip_section_ids the canvas id's of sections to skip in copying
#'
#' @return a paginated list with responses 
#'
#' @examples
#' sections_to_groups(course_id = 12345, groupcat_name = "Werkgroepen", skip_section_ids = c(123123, 123124, 123125))



sections_to_groups <- function(course_id, groupcat_name, skip_section_ids = NULL) {
# Create a group category.
  res <- create_course_group_category(course_id, groupcat_name = groupcat_name)

  res <- httr::content(res)
  groupcat_id <- res$id


  # Get all sections
  sections_df <- get_course_sections(course_id)
  
  sections_df <- sections_df[!sections_df$id %in% skip_section_ids,]

  for (i in seq_along(sections_df$id)) {
    sec_id <- sections_df$id[i]
    sec_name <- sections_df$name[i]
    # Create a group for given section
    res <-
      create_course_group_in_category(groupcat_id = groupcat_id,
                                      name = sec_name)
    res <- httr::content(res)
    group_id <- res$id
    #Get all the students in given section
    enrollments <- get_section_enrollments(sec_id)

    # Enrol students into groups
    if(length(enrollments) != 0 & any(enrollments$type == "StudentEnrollment")){
      enrollments <- enrollments[enrollments$type == "StudentEnrollment", ]

      for(j in seq_along(enrollments$id)) {
        create_group_membership(group_id = group_id, user_id = enrollments$user_id[j])
        print(enrollments$user_id[j])
      }
    } else {print("No students in section")
      }
  }
}
