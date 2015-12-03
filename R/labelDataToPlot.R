labelDataToPlot = function() {
  
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

  
  
  # VERY EXPENSIVE - ONLY FOR TABLEAU
  for (i in 1:length(df[1,])) {
    indicator <- df[,i]
    col <- colnames(df)[i]
    
    if (mean(indicator) > -1) {
      responses <- unlist(str_split(indicators[indicators$DCI.ID == sub("X.", "", col),]$Responses, ";"))
      for (j in 1:length(df[,1])) {
        r <- responses[as.numeric(df[j, i])]
        df[j, i] <- paste(df[j, i], r, sep = ": ")
      }
    }
  }
    
  
  # Set the column names
  colnames(df) <- indicators$Name
  
  write.csv(df, "output/ttest.csv")
  
  return (df)
}

