create_scrambled_exam = function(filepath, 
                                 number_options, 
                                 number_questions, 
                                 encoding,
                                 scramble_number = rep(number_options, number_questions)) {
  text_source = scan(filepath, what='character',sep="\n",fileEncoding= encoding)
  
  result = read_mc_to_df(text_source, scramble_number, number_options, number_questions, remove_answers=remove_answers)
  mc_frame = result$mc_frame
  answer_key = result$answer_key
  
  result = scramble_exam(frame = mc_frame, answer_key = answer_key, number_options, number_questions, scramble_number)
  scrambled_df = result$mc_frame
  scrambled_answer_key = result$answer_key
  
  output = format_exam(frame = scrambled_df, number_options, number_questions)
  
  dir_key = "./answer_key.csv"
  write.csv(scrambled_answer_key,dir_key) 
  
  dir_exam = "./output.txt"
  fileConn = file(dir_exam, encoding= encoding)
  writeLines(output, fileConn)
  close(fileConn)
}

read_mc_to_df = function(text_source, scramble_number, number_options, number_questions, remove_answers=TRUE) {
  require("stringr")
  
  ### HELPERS:
    insert_into_frame = function(frame, current_item, item_type, question_counter, answer_counter = NA) {
    current_item = paste(current_item, collapse = "\n")
    current_item = str_trim(current_item)
    
    if ( grepl('question', item_type) ) {
      
      current_item = str_replace( string = current_item, pattern = "^\\([0-9]+\\)", replacement = "")
      current_item = str_replace( string = current_item, pattern = "^[0-9]+\\.",    replacement = "")
      current_item = str_trim(current_item)
      
      frame[question_counter,1] = current_item
      return(frame)
    } 
    if ( grepl('answer', item_type) ) {
      
      current_item = str_replace( string = current_item, pattern = "^\\([abcdefgABCDEFG]+\\)", replacement = "")
      current_item = str_replace( string = current_item, pattern = "^[abcdefgABCDEFG]+\\.",    replacement = "")
      current_item = str_trim(current_item)
      
      if ( str_detect(current_item, pattern = "^\\([xX]+\\)") ) {
        # this is an answer, with an (X)
        
        if (remove_answers) {
          current_item = str_replace(current_item, pattern = "^\\([xX]+\\)", replacement = "")
          current_item = str_trim(current_item)
        }
        
      }
      
      frame[question_counter,answer_counter + 1] = current_item
      return(frame)
    } 
    
    stop("found item not recognized as question or answer")
  }
  
  update_answer_key = function(answer_key, this_answer_option, question_counter, answer_counter) {
    
    this_answer_option = paste(this_answer_option, collapse = "\n")
    this_answer_option = str_trim(this_answer_option)
    
    if ( str_detect(this_answer_option, pattern = "\\([xX]+\\)") ){
      answer_key[question_counter,answer_counter] = 1
    }
    return(answer_key)
    
  }
  
  check_line_type = function(this_line, prev_line_type) {
    
    this_line = str_trim(this_line)
    
    # Is the start of a question?
    is_question = 
      str_detect( this_line, "^\\([0-9]+\\)") |
      str_detect( this_line, "^[0-9]+\\.")
    if (is_question) return( "question_start" )
    
    # Is the start of an answer?
    is_answer = 
      str_detect( this_line, "^\\([abcdefgABCDEFG]+\\)") |
      str_detect( this_line, "^[abcdefgABCDEFG]+\\.")
    if (is_answer) return( "answer_start" )
    
    # Is neither?
    if (grepl("answer", prev_line_type)) return( "answer" )
    if (grepl("question", prev_line_type)) return( "question" )
    cat("\n",this_line)
    stop("Line type was not recognized!")
    
  }
  
  ### MAIN:
  col_names = c("Q","(A)","(B)","(C)","(D)","(E)","(F)","(G)")
  ncols = number_options + 1
  
  frame = data.frame(matrix(NA, ncol = ncols, nrow = number_questions))
  names(frame) = col_names[1:(number_options+1)]
  
  answer_key = matrix(0, ncol = number_options, nrow = number_questions)
  
  ## Loop thru text, parse into data-frame:
  line_type = NA
  current_question = c()
  current_answer   = c()
  question_counter = 0
  answer_counter = 0
  
  for(this_line in text_source) {
    
    prev_line_type = line_type
    
    line_type = check_line_type(this_line, prev_line_type)

    # if question
    if ( grepl('question', line_type) ) {
      if (line_type == 'question_start') {
        # insert previous question/answer into frame, start new question
        frame = insert_into_frame(frame, current_question, 'question', question_counter) 
        frame = insert_into_frame(frame, current_answer, 'answer', question_counter, answer_counter)
        answer_key = update_answer_key(answer_key, current_answer, question_counter, answer_counter)
        
        current_question = this_line
        question_counter = question_counter + 1
        answer_counter = 0
      } else {
        line_without_spaces = gsub(pattern = " ", replacement = "", this_line, fixed= TRUE)
        if (line_without_spaces != "") current_question = c(current_question, this_line) # check if an empty line
      }
    } 
    
    # check if answer
    if ( grepl('answer', line_type) ) {
      if (line_type == 'answer_start') {
        # insert prev, start new answer
        if (answer_counter>0) {
          frame = insert_into_frame(frame, current_answer, 'answer', question_counter, answer_counter) 
          answer_key = update_answer_key(answer_key, current_answer, question_counter, answer_counter)
        }
        
        current_answer = this_line
        answer_counter = answer_counter + 1
        
      } else {
        line_without_spaces = gsub(pattern = " ", replacement = "", this_line, fixed = TRUE)
        if (line_without_spaces != "") current_answer = c(current_answer, this_line) # check if an empty line
      }
    } 
    
  }
  
  # insert previous question/answer into frame
  frame = insert_into_frame(frame, current_question, 'question', question_counter) 
  frame = insert_into_frame(frame, current_answer, 'answer', question_counter, answer_counter)
  answer_key = update_answer_key(answer_key, current_answer, question_counter, answer_counter)
  
  return(list(mc_frame = frame, answer_key = answer_key))
}

scramble_exam = function(frame, answer_key, number_options, number_questions, scramble_number) {
  ncols = number_options + 1
  
  ## Randomize answers to each question:
  for (i in 1:number_questions) {
    if (frame[i,ncols]=="12341"){
      if (scramble_number[i] == number_options)
        scramble_number[i] = number_options-1
    }
    seed = sample(scramble_number[i])
    scr_indices = c(1,seed+1)
    scr_indices_padded = 1:ncols
    scr_indices_padded[1:length(scr_indices)] = scr_indices
    frame[i,] = frame[i,scr_indices_padded]
    
    scr_indices_padded_key = scr_indices_padded[2:length(scr_indices_padded)] - 1
    answer_key[i,] = answer_key[i,scr_indices_padded_key]
  }
  
  ## Randomize questions:
  question_reorder = sample(number_questions)
  frame = frame[question_reorder,]
  answer_key = answer_key[question_reorder,]
  
  ## Create Answer Key:
  answer_key2 = data.frame(answer_key)
  opts = names(frame)[2:ncols]
  for (i in 1:number_questions){
    ind = as.logical(answer_key[i,])
    answer_key2[i,ind] = opts[ind]
  }
  
  return(list(mc_frame = frame, answer_key = answer_key2))
  
}

format_exam = function(frame, number_options, number_questions) {
  ## Format Exam for Exporting:
  ncols = number_options + 1
  last = (ncols) * number_questions
  output = c()
  for (i in 1:last){
    row = ceiling(i/ncols)
    col = i%%ncols+((i%%ncols)==0)*ncols # creates the pattern "1, 2, 3, 4, 5, 1, 2, 3, 4, 5..."
    if (col==1){
      to_insert = 
        paste("\n","\n",
              row,
              ". ",
              frame[row,col],
              "\n",
              sep="")
    }else{
      to_insert = 
        paste(names(frame)[col],
              frame[row,col],
              sep=" ")
    }
    if (!frame[row,col]=="12341"){
      output[i] = to_insert
    }
  }
  output=output[!is.na(output)]
  return(output)
}

