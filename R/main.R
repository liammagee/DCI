# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/samplesForExpandedIndicators.R", FALSE)
source("R/pureProfile.R", FALSE)

# Install dependencies, if they are not available
installDeps()

# Imports
library(gdata)
library(reshape2)

print("Loading DCI data")

indicators <- loadIndicators()
# Expand valid indicators to include options, as sub-indicators
expandedIndicators <- generateExpandedVariableSet_Looped()

results <- loadSurveyResults()

# Show column names
# print(colnames(results))

# Q74_2 = Watched video clips (e.g. on YouTube)
watchedVideoClipsAge <- results[,c("Q10_159", "Q74_2")]
watchedVideoClipsAge$decades <- floor(watchedVideoClipsAge$Q10_159 / 10.0)
watchedVideoClipsAgeMean <- with(watchedVideoClipsAge, aggregate(Q74_2, by = list(decades), FUN=mean))

watchedVideoClipsMetadata <- expandedIndicators[which(expandedIndicators$DCI.ID == "74.2"),]

chartVariableByAge <- function(data, filename, metadata, labelsY) {
	p <- standardBarChart(data, 
					filename, 
					paste(metadata$Name, " by Age"), 
					"Age by Decade", 
					metadata$Name,
					labelsY
					)
	comment <- paste("Printed graph of ", metadata$Name, " to ./figs/", filename, ".png", sep="")
	print(comment)
	comment <- paste("Type 'open ./figs/", filename, ".png' from the terminal to view the file.", sep="")
	print(comment)
	return (p)
}

p <- chartVariableByAge(watchedVideoClipsAgeMean, 
							"watchedVideoClipsAgeMean", 
							watchedVideoClipsMetadata, 
							frequencyLabels)
print(p)

