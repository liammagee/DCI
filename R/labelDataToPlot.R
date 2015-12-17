source("R/replacing-1Columns.R")


getDimensions <- function() {
  dimensions <- c("Subject.ID", "Age", "Children in care", "Children ages", "Gender", "Location (Postcode)", "Language", "Language 2", "Education", "Home education status", "Main Activities")
  return (dimensions)
}

addLabelsToSampleValues = function(N = 20) {
  
  # Expand valid indicators to include options, as sub-indicators
  expandedIndicators <- generateExpandedVariableSet_Looped()
  
  # Generate sample data for the expanded list of indicators
  df <- samplesForExpandedIndicators(N)
  
  # Strip X's from the sample data columns
  idsWithoutXs <- sub("X.", "", colnames(df))
  
  # Obtain the indicators that match these ids
  indicators <- expandedIndicators[expandedIndicators$DCI.ID == idsWithoutXs,]
  
  # Set the response values
  sampleSubindicators <- df[str_count(colnames(df), fixed(".")) == 2,]
  responses <- str_split(indicators[indicators$DCI.ID == sub("X.", "", colnames(sampleSubindicators)),]$Responses, ";")
  
  # Keep a copy of the IDs
  idCols <- colnames(df)
  
  # Set the column names
  colnames(df) <- indicators$Name
  
  
  # VERY EXPENSIVE - ONLY FOR TABLEAU
  dimensions <- getDimensions()
  for (i in 1:length(df[1,])) {
    indicator <- df[,i]
    col <- colnames(df)[i]
    id <- idCols[i]
    
    if (!(mean(indicator) == -1 || (col %in% dimensions))) {
      ind <- indicators[indicators$DCI.ID == sub("X.", "", id),]
      options <- unlist(str_split(ind$Options, "\n"))
      responses <- unlist(str_split(ind$Responses, ";"))
      for (j in 1:length(df[,1])) {
        z <- as.numeric(df[j, i])
        if (length(responses) <= 1) {
          o <- options[z]
          df[j, i] <- paste(df[j, i], o, sep = ": ")
        }
        else {
          r <- responses[z]
          df[j, i] <- paste(df[j, i], r, sep = ": ")
        }
      }
    }
  }
  
  df <- addDemographicData(df)
  
  return (df)
}

# Create tall & thin version of labelled sample data
reshapeSample <- function(N = 20) {
  
  df <- addLabelsToSampleValues(N)
  
  df$Subject.ID <- row.names(df)
  # dimensions <- c("Subject.ID", "Age", "Children in care", "Children ages", "Gender", "Location (Postcode)", "Latitude", "Longitude", "Language", "Language 2", "Education", "Home education status", "Main Activities")
  dimensions <- c("Subject.ID", "Age", "Children in care", "Gender", "Location (Postcode)", "Latitude", "Longitude", "Language", "Language 2", "Education", "Home education status", "Main Activities")
  melted.df <- melt(df, dimensions)
  melted.df <- melted.df[order(melted.df$Subject.ID),]
  names(melted.df)[1 + length(dimensions)] <- "Question"
  names(melted.df)[2 + length(dimensions)] <- "Response"
  
  return (melted.df)
  
}
