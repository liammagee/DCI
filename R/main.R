# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/samplesForExpandedIndicators.R", FALSE)

print("Generating sample data")

# Expand valid indicators to include options, as sub-indicators
expandedIndicators <- generateExpandedVariableSet_Looped()

# For debugging only...
print("Length of initial variable set:")
print(length(validIndicators()[,1]))

print("Length of expanded variable set:")
print(length(expandedIndicators[,1]))

# Generate sample data for the expanded list of indicators
df <- samplesForExpandedIndicators()

# Show the variable IDs
print(names(df))


# Some preliminary testing...

# Should be -1
print(mean(df$"X.9"))

# Should be some positive value
print(mean(df$X.289.6))


# Completed the work
print("Done")

