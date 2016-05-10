#install.packages("stringr")
library(stringr)

melted = read.csv("output/sample_values_melted.csv")

### Tabulating Melted Values for Plotting ###

# Frequency table of melted
x = data.frame(xtabs(~ Question + Response, data = melted))

# Sort descending by Freq
xsorted = x[order(-x$Freq), ]

# Remove all rows with Freq 0 or 2000 (none or all)
xnonzero = subset(xsorted, Freq !=0)
xnonzero = subset(xnonzero, Freq !=2000)

# Order by question, omit NA's
x = na.omit(xnonzero[order(xnonzero$Question),])

# Calculate percentage frequency
x$Percent = (x$Freq/2000)*100

# Count the number of possible responses for each question
count = data.frame(table(x$Question))

# To merge this data frame with the main melted data frame we
# must have matching columns (Sub.Question) named the same in both data frames
# and all other columns named uniquely
names(count) = c("Question", "Count")

#Merge counted number of possbile responses with tabulated data by Sub.Question
table = merge(x, count)



### Labelling Likert Questions ###

# Likert.csv is data extracted brom the likert gantt bar chart in Tableau
likert = read.csv("data/Likert.csv")

# Get distinct sub questions in the likert file
likertqs = data.frame(names(table(likert$Sub.Question)))

# Label them "Likert"
likertqs$Type = "Likert"

# To merge this data frame with the main melted data frame we
# must have matching columns (Sub.Question) named the same in both data frames
# and all other columns named uniquely
names(likertqs) = c("Sub.Question", "Type")

# Extract sub question from question column
table$Sub.Question = str_trim(str_split_fixed(table$Question, "-", n=3)[,3])

# Merge the main table with type column, and keep all table rows
table = merge(table, likertqs, all.x = TRUE)

# Extract tabulated data only for likert questions
tableLikert = table[which(table$Type=="Likert"),]

# See that questions with 2 or 3 responses are falsely labelled likert questions
table(tableLikert$Count)
# Remove those from table of likert questions
tableLikert = subset(tableLikert, Count !=2)
tableLikert = subset(tableLikert, Count !=3)

# Write table to data folder
write.csv(tableLikert, "data/sample_likert_summary.csv")

