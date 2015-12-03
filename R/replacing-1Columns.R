# Columns containing just -1

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
