tableLikert = read.csv("data/sample_likert_summary.csv")
melted = read.csv("output/sample_values_melted.csv")
names(tableLikert)

# This needs each question to be a different column
# and to have the same number of possible answers
#install.packages("devtools")
library(devtools)
install_github('likert', 'jbryer')
require(likert)

# To reference one level at a time...
level4 = tableLikert[which(tableLikert$Count == 4), ]
level5 = tableLikert[which(tableLikert$Count == 5), ]
level6 = tableLikert[which(tableLikert$Count == 6), ]
level7 = tableLikert[which(tableLikert$Count == 7), ]

### To get data into required format for likert package... ###

likert4 = data.frame(names(table(level4$Sub.Question)))
likert4 = data.frame(t(likert4))
names(likert4) = names(table(level4$Sub.Question))

expanded4 = data.frame(rep(level4$Response, level4$Freq))
expanded4$Group = rep(1:(dim(expanded4)[1]/2000), each=2000)
expanded4 = unstack(expanded4)
expanded4 = data.frame(lapply(expanded4, as.factor))
names(expanded4) = gsub( "\\." , "" , names(table(likert4$Sub.Question)))


likert5 = data.frame(names(table(level5$Sub.Question)))
likert5 = data.frame(t(likert5))
names(likert5) = names(table(level5$Sub.Question))

expanded5 = data.frame(rep(level5$Response, level5$Freq))
expanded5$Group = rep(1:(dim(expanded5)[1]/2000), each=2000)
expanded5 = unstack(expanded5)
names(expanded5) = names(table(level5$Sub.Question))
expanded5 = data.frame(lapply(expanded5, as.factor))
names(expanded5) = gsub( "\\." , "" , names(likert5))


likert6 = data.frame(names(table(level6$Sub.Question)))
likert6 = data.frame(t(likert6))
names(likert6) = names(table(level6$Sub.Question))

expanded6 = data.frame(rep(level6$Response, level6$Freq))
expanded6$Group = rep(1:(dim(expanded6)[1]/2000), each=2000)
expanded6 = unstack(expanded6)
names(expanded6) = names(table(level6$Sub.Question))
expanded6 = data.frame(lapply(expanded6, as.factor))
names(expanded6) = gsub( "\\." , "" , names(likert6))


likert7 = data.frame(names(table(level7$Sub.Question)))
likert7 = data.frame(t(likert7))
names(likert7) = names(table(level7$Sub.Question))

expanded7 = data.frame(rep(level7$Response, level7$Freq))
expanded7$Group = rep(1:(dim(expanded7)[1]/2000), each=2000)
expanded7 = unstack(expanded7)
names(expanded7) = names(table(level7$Sub.Question))
expanded7 = data.frame(lapply(expanded7, as.factor))
names(expanded7) = gsub( "\\." , "" , names(likert7))

png("figs/RLikert.png")
plot(likert(expanded4))
dev.off()

plot(likert(expanded5))
plot(likert(expanded6))
plot(likert(expanded7))

# Must split by each set of responses, not just how many responses!

indicators = read.csv("data/Indicators.csv", na.strings=c(""," "))
indicators = indicators[which(indicators$Survey.Candidate.Question == 1),]
names = c("Subject.ID", "Age", "Children.in.care", "Gender", "Location..Postcode.", 
                   "Latitude", "Longitude", "Language", "Language.2", "Education", 
                   "Home.education.status", "Main.Activities",
                   "sa4", "sa3", "sa2", "sla", "lga")
library(reshape2)

#Crashes!
casted = dcast(  melted, 
                 Age
               + Children.in.care
               + Gender
               + Location..Postcode.
               + Latitude
               + Longitude
               + Language
               + Language.2
               + Education
               + Home.education.status
               + Main.Activities
               + sa4
               + sa3
               + sa2
               + sla
               + lga
               ~ Question, value.var = melted$Response )
casted = dcast(melted, Subject.ID ~ Question, value.var = melted$Response)
casted = dcast(melted, names ~ Question, value.var = melted$Response)

fix(tableLikert)
