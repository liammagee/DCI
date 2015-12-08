getDimensions <- function() {
  dimensions <- c("Subject.ID", "Age", "Children in care", "Children ages", "Gender", "Location (Postcode)", "Language", "Language 2", "Education", "Home education status", "Main Activities")
  return (dimensions)
}

addLabelsToSampleValues = function() {
  
  # Expand valid indicators to include options, as sub-indicators
  expandedIndicators <- generateExpandedVariableSet_Looped()
  
  # Generate sample data for the expanded list of indicators
  df <- samplesForExpandedIndicators()
  
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
      responses <- unlist(str_split(indicators[indicators$DCI.ID == sub("X.", "", id),]$Responses, ";"))
      for (j in 1:length(df[,1])) {
        r <- responses[as.numeric(df[j, i])]
        df[j, i] <- paste(df[j, i], r, sep = ": ")
      }
    }
  }
  
  source("R/replacing-1Columns.R")
  addDemographicData(df)
  
  return (df)
}

# Create tall & thin version of labelled sample data
reshapeSample <- function() {
  
  df <- addLabelsToSampleValues()
  
  df$Subject.ID <- row.names(df)
  dimensions <- c("Subject.ID", "Age", "Children in care", "Children ages", "Gender", "Location (Postcode)", "Language", "Language 2", "Education", "Home education status", "Main Activities")
  melted.df <- melt(df, dimensions)
  melted.df <- melted.df[order(melted.df$Subject.ID),]
  names(melted.df)[1 + length(dimensions)] <- "Question"
  names(melted.df)[2 + length(dimensions)] <- "Response"
  
  return (melted.df)
  
}
