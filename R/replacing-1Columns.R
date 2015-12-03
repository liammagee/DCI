# Columns containing just -1

test = ttestL

test$Age
test$Children.in.care
test$Children.ages
test$Type.of.Connection...Bandwidth...Speed

# Add data to columns

set.seed(4)

a = 12:65
age = sample(a, 2000, replace=T)

b = 0:10
cic = sample(b, 2000, replace=T)

c = 0:18 #Counting months?
ca = sample(c, 2000, replace=T)

d = c("1: Mobile 3G", "2: Mobile 4G", "3: ADSL", "4: ADSL2", "5: Cable", 
      "6: National Broadband Network (NBN)", "7: Other (please specify)")
toc = sample(d, 2000, replace=T)

test$Age = age
test$Children.in.care = cic
test$Children.ages = ca
test$Type.of.Connection...Bandwidth...Speed = toc

# Replace NA's in Gender and other columns
## Where are "Male", "Female", "Other (specify)", "Refused"?



# Stack questions, leave demographics
attach(test)
demographics = c("X", "Age", "Children.in.care", 
                 "Children.ages", "Gender", "Location..Postcode.",
                 "Language", "Language.2", "Education", 
                 "Home.education.status")
test.dem = data.frame(test[,demographics])
questions = names(test)[-which(names(test) %in% demographics)]
test.que = data.frame(test[,questions])
fix(test.que)
stack = stack(test.que)
fix(stack)
