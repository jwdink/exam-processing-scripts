## Some parameters (please set):
source("./read_multiple_choice.R")
filepath = "/Users/jacobdink/Documents/Courses/TA/exam3/exam3.txt"
number_options = 4
number_questions = 30

create_scrambled_exam( filepath, number_options, number_questions, "Latin1" )
