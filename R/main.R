# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/pureProfile.R", FALSE)

# Install dependencies, if they are not available
installDeps()

# Imports
library(gdata)
library(reshape2)

print("Loading DCI data")

indicators <- loadIndicators()
results <- loadSurveyResults()

# Show column names
# print(colnames(results))

# Q74_2 = Watched video clips (e.g. on YouTube)
watchedVideoClipsAge <- results[,c("Q10_159", "Q74_2")]
watchedVideoClipsAge$decades <- floor(watchedVideoClipsAge$Q10_159 / 10.0)
watchedVideoClipsAgeMean <- with(watchedVideoClipsAge, aggregate(Q74_2, by = list(decades), FUN=mean))


standardBarChart(watchedVideoClipsAgeMean, 
				"watchedVideoClipsAgeMean", 
				"Watched Video Clips by Age", 
				"Age by Decade", 
				"Watched Video Clips",
				frequencyLabels
				)
