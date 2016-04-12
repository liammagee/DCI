source("R/utils.R", FALSE)



# Main columns used
rawCols <- function() {
  d <- c("DCI.ID", "Indicator...Variable", "Indicator..Options...Constraints", "Indicator..Response.Type")
  return (d)
}
cleanCols <- function() {
  d <- c("DCI.ID", "Name", "Options", "Responses")
  return (d)
}


# Obtain a list of valid indicators
validIndicators <- function() {
  
  library(stringr)
  indicators = read.csv("data/Indicators.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))

  indicator = na.omit(indicators[,"Indicator...Variable"])
  
  rawCols <- rawCols()
  cleanCols <- cleanCols()
  d <- indicators[ which(indicators$Survey.Candidate.Question == "1"), rawCols]
  names(d)[names(d)==rawCols[2]] <- cleanCols[2]
  names(d)[names(d)==rawCols[3]] <- cleanCols[3]
  names(d)[names(d)==rawCols[4]] <- cleanCols[4]
  
  d$DCI.ID <- trim.leading(d$DCI.ID)
  
  return (d)
}

# Version of length which returns -1 instead of NA
lengthNoNA <- function(v) {
  l <- length(v)
  return (if (is.na(l)) -1 else l)
}


# Unrolls the variables to include options with responses as subvariables
generateExpandedVariableSet_Looped = function() {

  validIndicators <- validIndicators()
  
  countViS <- str_count(as.character(validIndicators$Options), "\n") + 1
  countViR <- str_count(as.character(validIndicators$Responses), ";") + 1
  validIndicators <- cbind(validIndicators, countViS, countViR)
  
  # Obtain indicators that have both options and responses, and therefore require subindicators
  subIndicators <- validIndicators[which(!is.na(validIndicators$countViS) & validIndicators$countViS > 1 & !is.na(validIndicators$countViR) & validIndicators$countViR > 1) , ]
  
  # Obtain standalone indicators (the inverse of those indicators that have subindicators)
  standaloneIndicators <- validIndicators[!(validIndicators$DCI.ID %in% subIndicators$DCI.ID), ]
  standaloneIndicators$Label <- standaloneIndicators$Name

  # New data set with both variables and subvariables
  extendedData <- as.matrix(standaloneIndicators)
  
  # Obtain the length of subindicators
  L <- length(subIndicators[,1])
  for (i in 1:L) {
    sourceRow <- subIndicators[i,]
    options <- data.frame(strsplit(as.character(sourceRow$Options), "\n"))
    
    # Use mapply
    newRows <- mapply( modifyRow, options[,1], 1:length(options[,1]),  MoreArgs = list(row = sourceRow) )

    # Rbind transposed results
    extendedData <- rbind(extendedData, t(newRows))
  }

  extendedData <- as.data.frame(extendedData)
  row.names(extendedData) <- extendedData$DCI.ID
  
  return (extendedData)
}


# Add a new row to a matrix
modifyRow <- function(option, id, row) {
    # Add a unique identifier
    newID <- paste(row$DCI.ID, as.character(id), sep = ".")
    row$DCI.ID <- newID
          
    # Add the option to the variable name, for a new variable name
    row$Label <- paste(option, sep = "")
          
    # Add Simplified name
    row$Name <- paste(row$DCI.ID, row$Name, option, sep = " - ")
    
    # Copy these values as strings not factors
    row$Options <- paste(row$Options,  "", sep = "")
    row$Responses <- paste(row$Responses,  "", sep = "")
    
    return (row) 
}


# Generates a data frame with columns corresponding to the variable IDs, and rows containing sample data
samplesForExpandedIndicators = function(N = 20) {
  
  # Obtain the expanded list of indicators
  expandedIndicators <- generateExpandedVariableSet_Looped()
  
  # Transpose with just the IDs as column names
  transposedIndicators <- data.frame(t(expandedIndicators)[FALSE,])
  
  # Extract the row names
  rows <- row.names(expandedIndicators)
  
  # Create a single dummy column in a matrix with 2000 rows
  df <- cbind(1:N)
  
  cols <- cleanCols()
  
  for (i in 1:length(rows)) {
    rowName <- rows[i]
    
    # Test if this is a grouping variable
    countPeriods <- str_count(rowName, fixed("."))
    id <- rowName
    
    indicator <- expandedIndicators[which(expandedIndicators$DCI.ID == as.character(id)), cols]
    ViS <- strsplit(as.character(indicator$Options), "\n")
    ViR <- strsplit(as.character(indicator$Responses), ";")
    lenViS <- str_count(as.character(indicator$Options), "\n") + 1
    lenViR <- str_count(as.character(indicator$Responses), ";") + 1

    if (countPeriods == 1) {
      if (length(indicator$Response) > 0 && !is.na(lenViR) && lenViR > 0) {
        s <- sample(1:lenViR, N, replace = TRUE)
        df <- cbind(df, rowName = s)
      }
      else {
        s <- sample(-1, N, replace = TRUE)
        df <- cbind(df, rowName = s)
      }
    }
    # Multiple options, zero responses
    else if (length(indicator$Options) > 0 && !is.na(lenViS) && 
               ( length(indicator$Responses) == 0 || is.na(lenViR) )) {
      s <- sample(1:lenViS, N, replace = TRUE)
      df <- cbind(df, rowName = s)
    }
    # Multiple responses, zero options
    else if (length(indicator$Response) > 0 && !is.na(lenViR) ) {
      s <- sample(1:lenViR, N, replace = TRUE)
      df <- cbind(df, rowName = s)
    }
    # For all other cases
    else {
      if (id == "10") {
        s <- sample(12:65, N, replace = TRUE)
        df <- cbind(df, rowName = s)
      }
      else {
        s <- sample(-1, N, replace = TRUE)
        df <- cbind(df, rowName = s)
      }
    }
  }
  
  # Get rid of the first dummy column
  df <- df[,2:length(df[1,])]
  
  df <- data.frame(df)
  names(df) <- paste("X", rows, sep = ".")
  
  return (df)
}



