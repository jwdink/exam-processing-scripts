source("/Users/jacobdink/Documents/Projects/helpful-grading-scripts/match_scantron_names.R")
scantron_path = file.choose()
grade_path = file.choose()
setwd(dir = dirname(grade_path))
scantrons = read.csv(scantron_path, stringsAsFactors=FALSE)
grades = read.csv(grade_path, stringsAsFactors=FALSE)
grades = grades[2:nrow(grades),] # remove the "points possible" column

col_names_sc = list("name" = "Student", "score" = "Total")
col_names_gr = list("name" = "Student", "score" = "Quiz.1..41039.")

output = match_scantron_names(scantrons, grades, col_names_sc, col_names_gr)
