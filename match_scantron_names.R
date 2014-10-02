match_scantron_names = function(scantrons, grades, col_names_sc, col_names_gr) {
  require("stringr")
  
  # scantron dataframe
  # grade dataframe
  # column names scantron: a named list with fields: "name" and "score"-- each element gives column name(s) in scantron file
  # column names grades:   a named list with fields: "name" and "score"-- each element gives column name(s) in grades file
  
  scantron_names = scantrons[ , col_names_sc[["name"]] ]
  grades_names =   grades[ , col_names_sc[["name"]] ]
  
  scantrons$FormattedName = NA
  
  for (i in seq_along(grades_names)) {
    
    # Get each student's full name, split into first and last name:
    # the grades downloaded from the CMS should be comma separated: last, first
    this_student = grades_names[i]
    this_student_spl = unlist( str_split(this_student, ",") )
    last_name   = str_trim(this_student_spl[1])
    first_names = str_trim(this_student_spl[2])
    
    # Find which scantron name corresponds to the full name:
    ind_match_ln = grep(last_name, scantron_names, ignore.case= TRUE) # where in vector of scantron names does last name appear?
    ind_match_fn = grep(first_names, scantron_names, ignore.case= TRUE) # where in vector of scantron names does first name appear?
    ind_match = intersect(ind_match_ln, ind_match_fn)
    
    if (length(ind_match) == 1) {
      # if there was one match for it, add full name to scantron dataframe
      scantrons$FormattedName[ind_match] = this_student
    } else {
      cat("\nWarning: No matches found for", this_student, ".")
      cat("\nYou will have to enter grade for this student manually.")
    }
    
  }
  
  # Create a dataframe with formatted name and correct score:
  to_merge = scantrons
  cols_to_keep = c(col_names_sc[["score"]], "FormattedName")
  to_merge = to_merge[,cols_to_keep]
  colnames(to_merge)[colnames(to_merge) == col_names_sc[["score"]] ] = "ScoreToUpdate"
  
  # Merge this dataframe with the "grades" dataframe, format score for proper output:
  out = merge(grades, to_merge, by.x = col_names_sc[["name"]], by.y= "FormattedName", all.x = TRUE)
  out[ , col_names_gr[["score"]] ] = out$ScoreToUpdate
  all_but_one_col = colnames(out)[colnames(out) != "ScoreToUpdate"]
  out = out[,all_but_one_col]
  
  # Output CSV
  path_out = paste0(getwd(), "/to_upload.csv")
  write.csv(out, path_out, na="", row.names= FALSE)
  cat("\nCSV to upload was written to", path_out)
  
  # Output Names:
  path_name_check = paste0(getwd(), "/name_check.csv")
  write.csv(scantrons, path_name_check, na="", row.names= FALSE)
  cat("\nCSV with scantron names and gradesheet names was written to", path_name_check)
  cat("\nPlease check this file to make sure no mismatches occurred.")
  
  return( out )

}