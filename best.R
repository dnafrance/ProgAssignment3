best <- function(state, outcome) {
        
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        ## Check that state and outcome are valid
        allstates <- unique(df$State)
        if (! state %in% allstates)
        {
                stop("invalid state")
        }
        
        alloutcomes <- c("heart attack","heart failure","pneumonia")
        if (! outcome %in% alloutcomes)
        {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if(outcome == alloutcomes[1])
        {
                #Exclude hospitals without data
                # "Heart Attack" outcome in Column 11
                df <- df[df[,11] !="Not Available",] 
                df[,11] <- as.numeric(df[,11]) #convert to numeric
                df <- df[df$State==state,c(2,11)] #subset by state & outcome
                df <- df[!is.na(df),] #remove NAs
                
                #Rank by ascending outcome - column 2, 
                #then by ascending hospital name - column 1
                sorted <- df[order(df[,2],df[,1],na.last = NA),] 
        }
        else if(outcome == alloutcomes[2])
        {
                #Exclude hospitals without data
                # "Heart Failure" outcome in Column 17
                df <- df[df[,17] !="Not Available",]
                df[,17] <- as.numeric(df[,17]) #convert to numeric
                df <- df[df$State==state,c(2,17)] #subset by state & outcome
                df <- df[!is.na(df),] #remove NAs
                
                #Rank by ascending outcome - column 2, 
                #then by ascending hospital name - column 1
                sorted <- df[order(df[,2],df[,1],na.last = NA),]
        }
        else 
        {
                #Exclude hospitals without data
                # "Pneumonia" outcome in Column 23
                df <- df[df[,23] !="Not Available",]
                df[,23] <- as.numeric(df[,23]) #convert to numeric
                df <- df[df$State==state,c(2,23)] #subset by state & outcome
                df <- df[!is.na(df),] #remove NAs
                
                #Rank by ascending outcome - column 2, 
                #then by ascending hospital name - column 1
                sorted <- df[order(df[,2],df[,1],na.last = NA),]
        }
        
        # Return first record with hospital name - column 1
        return(head(sorted[,1],1))
}