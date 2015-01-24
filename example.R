source("/Users/jacobdink/Documents/Projects/helpful-grading-scripts/match_scantron_names.R")
scantron_filepath = file.choose()
setwd(dir = dirname(scantron_filepath))
grade_filepath = file.choose()


col_names_sc = c("name" = "Student", "score" = "Total")
col_names_gr = c("name" = "Student", "score" = "Multiple Choice")

output = match_scantron_names(scantron_filepath, grade_filepath, col_names_sc, col_names_gr, 
                              name_separator = ",") # can be changed to = " " (for space separated), or other separators
