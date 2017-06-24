sortedhospitals <- function(df,state,colnum) {
        #Exclude hospitals without data of outcome in column colnum
        df <- df[df[,colnum] !="Not Available",] 
        df[,colnum] <- as.numeric(df[,colnum]) #convert to numeric
        df <- df[df$State==state,c(2,colnum)] #subset by state & outcome
        df <- df[!is.na(df),] #remove NAs
        #Rank by ascending outcome - column 2, 
        #then by ascending hospital name - column 1
        sorted <- df[order(df[,2],df[,1],na.last = NA),] 
        return(sorted)
}

rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(outcome==alloutcomes[1]){
                colnum <- 11
        }
        else if(outcome==alloutcomes[2]){
                colnum <- 17
        }
        else {
                colnum <- 23
        }
        sorted <- sortedhospitals(df,state,colnum)
        numhosp <- length((sorted[,1]))
        
        if(num == 'best'){
                hosp <- head(sorted[,1],1)
        }
        else if (num == 'worst'){
                hosp <- tail(sorted[,1],1)
        }
        else if (num > numhosp){
                hosp <- NA
        }
        else {
                hosp <- tail(head(sorted[,1],num),1)
        }
        return(hosp)
}

