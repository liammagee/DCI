a = 12:65
age = sample(a, 2000, replace=T)

g = c("Male", "Female", "Other (specify)", "Refused")
gender = sample(g, 2000, replace=T, prob = c(49, 49, 1, 1))

dci = data.frame(age, gender)

write.csv(dci, "./output/dci.csv")
