
getValidIndicators <- function() {
  
  # install.packages("stringr")
  library(stringr)
  
  
  indicators = read.csv("data/Indicators.csv", header = TRUE, na.strings = c("", " "))
  names(indicators)
  
  indicator = na.omit(indicators[,"Indicator...Variable"])
  table(indicator)
  
  validIndicators <- indicators[ which(indicators$Survey.Candidate.Question == "1"), c("DCI.ID", "Indicator...Variable", "Indicator..Options...Constraints", "Indicator..Response.Type")]
  
  return (validIndicators)
}

generateExpandedVariableSet_Looped = function(indicators) {
 
  ViS <- strsplit(as.character(indicators$Indicator..Options...Constraints), "\n")
  ViR <- strsplit(as.character(indicators$Indicator..Response.Type), ";")
  
  # 'j' is the number of substrings in ViS
  lenViS <- str_count(as.character(indicators$Indicator..Options...Constraints), "\n") + 1
  
  # 'k' is the number of values in ViR
  lenViR <- str_count(as.character(indicators$Indicator..Response.Type), ";") + 1
  
  names(newdata)
  sampleAnswer <- vector(, length(lenViS))

  lengthOfVariableSet <- length(indicators[,1])
  extendedData <- as.matrix(newdata, stringsAsFactors=FALSE)
  
  for (i in 1:lengthOfVariableSet) {
  
    options <- data.frame(ViS[i])
    lenOptions <- lenViS[i]
  
    if (!is.na(lenOptions)) {
      
      for (j in 1:lenOptions) {
        
        currentRow <- c(indicators[i,])
        option <- options[j,1]
        
        if (!is.na(option)) {
          
          # Add a unique identifier
          newID <- 100 * as.numeric(currentRow$DCI.ID) + j
          currentRow$DCI.ID <- as.character(newID)
          
          # Add the option to the variable name, for a new variable name
          currentRow$Indicator...Variable <- paste(currentRow$Indicator...Variable,  option, sep = " - ")
          
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


generateExpandedVariableSet_Looped = function(indicators) {
  
  ViS <- strsplit(as.character(indicators$Indicator..Options...Constraints), "\n")
  ViR <- strsplit(as.character(indicators$Indicator..Response.Type), ";")
  
  # 'j' is the number of substrings in ViS
  lenViS <- str_count(as.character(indicators$Indicator..Options...Constraints), "\n") + 1
  
  # 'k' is the number of values in ViR
  lenViR <- str_count(as.character(indicators$Indicator..Response.Type), ";") + 1
  
  names(newdata)
  sampleAnswer <- vector(, length(lenViS))
  
  lengthOfVariableSet <- length(indicators[,1])
  extendedData <- as.matrix(newdata, stringsAsFactors=FALSE)
  
  for (i in 1:lengthOfVariableSet) {
    
    options <- data.frame(ViS[i])
    lenOptions <- lenViS[i]
    lenResponses <- lenViR[i]
    
    if (!is.na(lenOptions)) {
      
      for (j in 1:lenOptions) {
        
        currentRow <- c(indicators[i,])
        option <- options[j,1]
        
        if (!is.na(option) && !is.na(lenResponses) && lenResponses > 0) {
          
          # Add a unique identifier
          newID <- paste(currentRow$DCI.ID, as.character(j), sep = ".")
          currentRow$DCI.ID <- newID
          
          # Add the option to the variable name, for a new variable name
          currentRow$Indicator...Variable <- paste(currentRow$Indicator...Variable,  option, sep = " - ")
          
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


indicatorsAsFrame = function(expandedIndicators) {

  # Transpose with just the IDs as column names
  transposedIndicators <- data.frame(t(expandedIndicators)[FALSE,])
  
  # Extract the row names
  rows <- row.names(expandedIndicators)
  
  # Create a single dummy column in a matrix with 2000 rows
  df <- cbind(1:2000)
  
  
  for (i in 1:length(rows)) {
    rowName <- rows[i]
    
    # Test if this is a grouping variable
    countPeriods <- str_count(rowName, fixed("."))
    id <- lapply(strsplit(rowName, ".", fixed = TRUE), "[[", 1)
    indicator <- expandedIndicators[which(expandedIndicators$DCI.ID == as.character(id)), c("DCI.ID", "Indicator...Variable", "Indicator..Options...Constraints", "Indicator..Response.Type")]
    ViS <- strsplit(as.character(indicator$Indicator..Options...Constraints), "\n")
    ViR <- strsplit(as.character(indicator$Indicator..Response.Type), ";")
    lenViS <- str_count(as.character(indicator$Indicator..Options...Constraints), "\n") + 1
    lenViR <- str_count(as.character(indicator$Indicator..Response.Type), ";") + 1
    
    # Dealing with a sub indicator
    if (countPeriods == 1) {
      if (length(indicator$Indicator..Response.Type) > 0 && !is.na(lenViR)) {
        if ( lenViR > 0) {
          s <- sample(1:lenViR, 2000, replace = TRUE)
          df <- cbind(df, rowName = s)
        }
      }
      else {
        s <- sample(0, 2000, replace = TRUE)
        df <- cbind(df, rowName = s)
      }
    }
    else if (length(indicator$Indicator..Options...Constraints) > 0 && !is.na(lenViS) && 
               ( length(indicator$Indicator..Response.Type) == 0 || is.na(lenViR) )) {
      s <- sample(1:lenViS, 2000, replace = TRUE)
      df <- cbind(df, rowName = s)
    }
    else {
      s <- sample(1:5, 2000, replace = TRUE)
      df <- cbind(df, rowName = s)
    }
  }
  
  # Get rid of the first dummy column
  df <- df[,2:length(df[1,])]
  
  df <- data.frame(df)
  names(df) <- paste("X", rows, sep = ".")
  
  return (df)
}



validIndicators <- getValidIndicators()

expandedIndicators <- generateExpandedVariableSet_Looped(validIndicators)

print("Length of initial variable set:")
print(length(validIndicators[,1]))

print("Length of expanded variable set:")
print(length(expandedIndicators[,1]))

df <- indicatorsAsFrame(expandedIndicators)
#print(names(df))
