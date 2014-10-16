source("/Users/jacobdink/Documents/Projects/helpful-grading-scripts/match_scantron_names.R")
scantron_filepath = file.choose()
grade_filepath = file.choose()
setwd(dir = dirname(grade_filepath))

col_names_sc = c("name" = "Student", "score" = "Total")
col_names_gr = c("name" = "Student", "score" = "Quiz 3 (41041)")

output = match_scantron_names(scantron_filepath, grade_filepath, col_names_sc, col_names_gr, name_separator = ",")
