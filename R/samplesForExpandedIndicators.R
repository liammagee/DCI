

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
  names(d)[names(d)==cols[2]] <- cleanCols[2]
  names(d)[names(d)==cols[3]] <- cleanCols[3]
  names(d)[names(d)==cols[4]] <- cleanCols[4]
  
  d$DCI.ID <- trim.leading(d$DCI.ID)
  
  return (d)
}


generateExpandedVariableSet_Looped = function() {

  validIndicators <- validIndicators()
  
  ViS <- strsplit(as.character(validIndicators$Options), "\n")
  ViR <- strsplit(as.character(validIndicators$Responses), ";")
  
  # 'j' is the number of substrings in ViS
  lenViS <- str_count(as.character(validIndicators$Options), "\n") + 1
  
  # 'k' is the number of values in ViR
  lenViR <- str_count(as.character(validIndicators$Responses), ";") + 1
  
  lengthOfVariableSet <- length(validIndicators[,1])
  extendedData <- as.matrix(validIndicators, stringsAsFactors=FALSE)
  
  for (i in 1:lengthOfVariableSet) {
    
    options <- data.frame(ViS[i])
    lenOptions <- lenViS[i]
    lenResponses <- lenViR[i]
    
    if (!is.na(lenOptions)) {
      
      for (j in 1:lenOptions) {
        
        currentRow <- c(validIndicators[i,])
        option <- options[j,1]
        
        if (!is.na(option) && !is.na(lenResponses) && lenResponses > 0) {
          
          # Add a unique identifier
          newID <- paste(currentRow$DCI.ID, as.character(j), sep = ".")
          currentRow$DCI.ID <- newID
          
          # Add the option to the variable name, for a new variable name
          currentRow$Name <- paste(currentRow$Name,  option, sep = " - ")
          
          # Bind the new row to the extended data set
          extendedData <- rbind(extendedData, currentRow)
        }
      }
    }
  }
  extendedData <- as.data.frame(extendedData)
  row.names(extendedData) <- extendedData$DCI.ID
  
  return (extendedData)
}


# Generates a data frame with columns corresponding to the variable IDs, and rows containing sample data
samplesForExpandedIndicators = function(N = 2000) {
  
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
    id <- lapply(strsplit(rowName, ".", fixed = TRUE), "[[", 1)
    
    indicator <- expandedIndicators[which(expandedIndicators$DCI.ID == as.character(id)), cols]
    ViS <- strsplit(as.character(indicator$Options), "\n")
    ViR <- strsplit(as.character(indicator$Responses), ";")
    lenViS <- str_count(as.character(indicator$Options), "\n") + 1
    lenViR <- str_count(as.character(indicator$Responses), ";") + 1
    
    # Dealing with a sub indicator
    if (countPeriods == 1) {
      if (length(indicator$Response) > 0 && !is.na(lenViR)) {
        if ( lenViR > 0) {
          s <- sample(1:lenViR, N, replace = TRUE)
          df <- cbind(df, rowName = s)
        }
      }
      else {
        s <- sample(-1, N, replace = TRUE)
        df <- cbind(df, rowName = s)
      }
    }
    else if (length(indicator$Options) > 0 && !is.na(lenViS) && 
               ( length(indicator$Responses) == 0 || is.na(lenViR) )) {
      s <- sample(1:lenViS, N, replace = TRUE)
      df <- cbind(df, rowName = s)
    }
    else {
      s <- sample(-1, N, replace = TRUE)
      df <- cbind(df, rowName = s)
    }
  }
  
  # Get rid of the first dummy column
  df <- df[,2:length(df[1,])]
  
  df <- data.frame(df)
  names(df) <- paste("X", rows, sep = ".")
  
  return (df)
}



