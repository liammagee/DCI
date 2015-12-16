# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/samplesForExpandedIndicators.R", FALSE)
source("R/labelDataToPlot.R", FALSE)
source("R/replacing-1Columns.R", FALSE)
source("R/postcodeMapping.R", FALSE)


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


df <- samplesForExpandedIndicators()

# Show the variable IDs
print(names(df))

# Some preliminary testing...

# Should be some positive value
print(mean(df$X.289.4))


# Write the sample to file
write.csv(df, "output/sample_values_raw.csv", row.names = FALSE)

# Write the labelled sample to file
df.labelled <- addLabelsToSampleValues()
df.labelled <- addDemographicData(df.labelled)

write.csv(df.labelled, "output/sample_values_labelled.csv", row.names = FALSE)

# Write the melted sample to file
df.melted <- reshapeSample()
write.csv(df.melted, "output/sample_values_melted.csv", row.names = FALSE)

# Completed the work
print("Done")




# Test postcode mapping
loadPostcodeMappings()
obtainSA4(822)
