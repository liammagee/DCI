labelDataToPlot = function(df, expandedIndicators) {
  
  idsWithoutXs <- sub("X.", "", colnames(df))
  properNames <- expandedIndicators[expandedIndicators$DCI.ID == idsWithoutXs,]$Name
  colnames(df) <- paste(idsWithoutXs, properNames, sep = ": ")
  
  write.csv(df, "output/ttest.csv")
  
}

