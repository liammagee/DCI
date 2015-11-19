printFirstPriority = function() {
  
  #install.packages("stringr")
  library(stringr)
  
  
  indicators = read.csv("data/Indicators.csv", header = TRUE, na.strings = c("", " "))
  names(indicators)
  
  indicator = na.omit(indicators[,"Indicator...Variable"])
  table(indicator)
  
  newdata <- indicators[ which(indicators$Survey.Candidate.Question == "1"), c("Indicator...Variable", "Indicator..Options...Constraints", "Indicator..Response.Type")]
  ViS <- strsplit(as.character(indicators$Indicator..Options...Constraints), "\n")
  ViR <- strsplit(as.character(indicators$Indicator..Response.Type), ";")
  
  # 'j' is the number of substrings in ViS
  j <- str_count(as.character(indicators$Indicator..Options...Constraints), "\n") + 1
  
  # 'k' is the number of values in ViR
  k <- str_count(as.character(indicators$Indicator..Response.Type), ";") + 1
  
  for (i in 1:2000) {
    
    sampleAnswer[i] = sample(if(!is.na(k[i]))0:k[i], j, replace = TRUE)
    
  }
  
}