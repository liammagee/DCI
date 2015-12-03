# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/samplesForExpandedIndicators.R", FALSE)
source("R/labelDataToPlot.R", FALSE)


# Imports
library(gdata)
library(reshape2)

installDeps()


print("Generating sample data")

# Expand valid indicators to include options, as sub-indicators
expandedIndicators <- generateExpandedVariableSet_Looped()

# For debugging only...
print("Length of initial variable set:")
print(length(validIndicators()[,1]))

print("Length of expanded variable set:")
print(length(expandedIndicators[,1]))

# Show the variable IDs
print(names(df))

# Test melt

# Generate sample data for the expanded list of indicators
df <- samplesForExpandedIndicators()



# Some preliminary testing...

# Should be some positive value
print(mean(df$X.289.6))

# Print a graph
df <- labelDataToPlot()

# Completed the work
print("Done")


