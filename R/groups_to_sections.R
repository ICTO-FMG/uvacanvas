groups_to_sections <- function(course_id){

  #get groupinformation from course including id's and names
  
  groups_in_course <- get_groups_in_course(course_id = course_id)
  
  group_ids <- groups_in_course$id
  
  group_names <- groups_in_course$name
  
  sections_to_exclude <- get_course_sections(course_id = course_id)
  
  sections_to_exclude_ids <- sections_to_exclude$id
  
  
  #create sections based on group name
  
  res2 <- lapply(group_names, create_section_in_course, course_id = course_id)
  
  #get section_ids and remove insignificant sections
  
  sections <- get_course_sections(course_id = course_id)
  section_ids <- sections$id
  section_ids <- section_ids[section_ids!=sections_to_exclude_ids]
  
  # for the number of groups, get group members and enroll to corresponding section
  
  for(i in seq_along(group_ids)){
    
    #get user_ids from the group
    
    group_members <- get_members_in_group(group_id = group_ids[i])
    group_member_ids <- group_members$user_id
    
    if(length(group_member_ids)!=0){
    
      res3 <- lapply(group_member_ids,
                     enroll_user_in_course,
                     course_id = course_id,
                     enrollment_type = "StudentEnrollment",
                     enrollment_state = "active",
                     course_section_id = section_ids[i]) 
      message(paste("Group",groups_in_course[groups_in_course$id == group_ids[i],2],"has been converted to a section.", sep = " "))
      
    } else { warning(paste("Group",groups_in_course[groups_in_course$id == group_ids[i],2],"has no students.", sep = " "))  
      }
  }
}