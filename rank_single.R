rankhospital <- function(state, outcome, num = "best") {
        library(dplyr)
        data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcome_name <- c("heart attack", "heart failure", "pneumonia")
        outcome_index <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        table_name <- c("hospital", "State", "rate")
        
        state_valid <- state %in% data$State
        outcome_valid <- outcome %in% outcome_name
        if(!state_valid) stop("invalid state")
        if(!outcome_valid) stop("invalid outcome")
        
        my_data <- data[, c(2, 7, outcome_index[outcome])]
        colnames(my_data) <- table_name
        # clean_data <- na.omit(my_data)
        # final_data <- subset(clean_data, State == state)
        # rank_data <- final_data[order(final_data[3], final_data[1]), ]
        
        rank_data <- my_data %>%
                        na.omit %>%
                        filter(State == state) %>%
                        arrange(rate, hospital)
        
        # if(num == "best") result = unlist(rank_data[1], use.names = FALSE)[1]
        # if(num == "worst") result = unlist(rank_data[1], use.names = FALSE)[nrow(rank_data)]
        if(num == "best") num <- 1
        if(num == "worst") num <- nrow(rank_data)
        if(is.numeric(num)) result = unlist(rank_data[1], use.names = FALSE)[num]
        
        result
        
}