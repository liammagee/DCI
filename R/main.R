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
df.melted <- reshapeSample(20)


sa4 <- function(postcode) {  return (obtainGeoArea(postcode, "SA4")[1]) }
sa3 <- function(postcode) {  return (obtainGeoArea(postcode, "SA3")[1]) }
sa2 <- function(postcode) {  return (obtainGeoArea(postcode, "SA2")[1]) }
sa1 <- function(postcode) {  return (obtainGeoArea(postcode, "SLA")[1]) }
lga <- function(postcode) {  return (obtainGeoArea(postcode, "LGA")[1]) }

df.melted$sa4 <- sapply(df.melted$"Location (Postcode)", sa4)
df.melted$sa3 <- sapply(df.melted$"Location (Postcode)", sa3)
df.melted$sa2 <- sapply(df.melted$"Location (Postcode)", sa2)
df.melted$sla <- sapply(df.melted$"Location (Postcode)", sa1)
df.melted$lga <- sapply(df.melted$"Location (Postcode)", lga)


write.csv(df.melted, "output/sample_values_melted.csv", row.names = FALSE)



# Completed the work
print("Done")




# Test postcode mapping
loadPostcodeMappings()
obtainGeoArea("2148", "SA4")
obtainGeoArea("2148", "SA3")
obtainGeoArea("2148", "SA2")
obtainGeoArea("2148", "SLA")
obtainGeoArea("2148", "LGA")



