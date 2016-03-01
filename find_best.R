best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcome_name <- c("heart attack", "heart failure", "pneumonia")
        outcome_index <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
          
        state_valid <- state%in%data$State      
        # state_valid <- state%in%as.character(data$State)
        outcome_valid <- outcome%in%outcome_name
        if(!state_valid) stop("invalid state")
        if(!outcome_valid) stop("invalid outcome")
        
        my_data <- data[, c(2, 7, outcome_index[outcome])]
        clean_data <- na.omit(my_data)
        final_data <- subset(clean_data, State == state)
        best_row <- which.min(as.double(unlist(final_data[3])))
        final_data[best_row, 1]
        
}