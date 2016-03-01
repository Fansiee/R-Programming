rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        outcome_name <- c("heart attack", "heart failure", "pneumonia")
        outcome_index <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        table_name <- c("hospital", "state", "rate")
        final_name <- c("hospital", "state")
        
        outcome_valid <- outcome %in% outcome_name
        if(!outcome_valid) stop("invalid outcome")
        
        my_data <- data[, c(2, 7, outcome_index[outcome])]
        clean_data <- na.omit(my_data)
        colnames(clean_data) <- table_name
        
        rank_list <- lapply(split(clean_data, clean_data$state), function(data) arrange(data, rate, hospital))
        if(num == "best") {
                num <- 1
                result_list <- lapply(rank_list, function(data) data[num, c(1, 2)])
        }
        else if(num == "worst") {
                result_list <- lapply(rank_list, function(data) data[nrow(data), c(1, 2)])
        }
        else if(is.numeric(num)) {
                result_list <- lapply(rank_list, function(data) data[num, c(1, 2)])
        }

        result_matrix <- do.call(rbind, result_list)
        result <- mutate(result_matrix, state = rownames(result_matrix))
        # rownames(result) <- rownames(result_matrix)

        print(head(result_matrix))
        result
        
}