scantrons = read.csv("/Users/jacobdink/Documents/Courses/TA STATS/grouping_test/scores.csv", stringsAsFactors=FALSE)
grades = read.csv("/Users/jacobdink/Documents/Courses/TA STATS/grades_from_canvas.csv", stringsAsFactors=FALSE)
grades = grades[2:nrow(grades),] # remove the "points possible" column

col_names_sc = list("name" = "Student", "score" = "Total")
col_names_gr = list("name" = "Student", "score" = "Quiz.1..41039.")

output = match_scantron_names(scantrons, grades, col_names_sc, col_names_gr)
