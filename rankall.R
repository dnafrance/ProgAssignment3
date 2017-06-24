sortedhospitals <- function(df,state,colnum) {
        #Exclude hospitals without data of outcome in column colnum
        df <- df[df[,colnum] !="Not Available",] 
        df[,colnum] <- as.numeric(df[,colnum]) #convert to numeric
        df <- df[df$State==state,c(2,7,colnum)] #subset by state & outcome
        #df <- df[!is.na(df),] #remove NAs
        #Rank by ascending outcome - column 3, 
        #then by ascending hospital name - column 1
        sorted <- df[order(df[,3],df[,1]),] 
        return(sorted)
}

rankhosp <- function(sorted,num){
        numhosp <- length((sorted[,1]))
        if(num == 'best'){
                hosp <- head(sorted[,1:2],1)
        }
        else if (num == 'worst'){
                hosp <- tail(sorted[,1:2],1)
        }
        else if (num > numhosp){
                hosp <- head(sorted[,1:2],1)
                hosp[,1] <- NA
        }
        else {
                hosp <- tail(head(sorted[,1:2],num),1)
        }
        return(hosp)
}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character")
        
        ## Check that outcome is valid
         alloutcomes <- c("heart attack","heart failure","pneumonia")
        if (! outcome %in% alloutcomes)
        {
                stop("invalid outcome")
        }
        if(outcome==alloutcomes[1]){
                colnum <- 11
        }
        else if(outcome==alloutcomes[2]){
                colnum <- 17
        }
        else {
                colnum <- 23
        }

        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        listhosp <- data.frame(matrix(ncol = 2, nrow = 0)) 
        colnames(listhosp) <- c(names(df)[2],names(df)[7])

        allstates <- unique(df$State)

        for (state in allstates){
                sorted <- sortedhospitals(df,state,colnum)
                ranked <- rankhosp(sorted,num)
                listhosp <- rbind(listhosp,ranked)
        }
        listhosp <- listhosp[order(listhosp[,2]),]
        colnames(listhosp) <- c('hospital','state')
        return(listhosp)
}
