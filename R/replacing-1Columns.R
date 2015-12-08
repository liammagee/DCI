# Add data to columns
names(df.labelled)

addDemographicData = function(data) {
  
a = 12:65
age = sample(a, dim(data)[1], replace=T)

b = 0:10
cic = sample(b, dim(data)[1], replace=T)

d = c("1: Mobile 3G", "2: Mobile 4G", "3: ADSL", "4: ADSL2", "5: Cable", 
      "6: National Broadband Network (NBN)", "7: Other (please specify)")
toc = sample(d, dim(data)[1], replace=T)

data$Subject.ID = 1:dim(data)[1]
data$Age = age
data$"Children in care" = cic
data$"Type of Connection / Bandwidth / Speed" = toc

# Children ages depends on number of children in care

c = 0:18
ca = sample(c, dim(data)[1], replace=T)

# Replace the rest of the demographic variables


g = c("Male", "Female", "Other (specify)", "Refused")
gender = sample(g, dim(data)[1], replace=T, prob = c(49, 49, 1, 1))

############
# Postcode section
all = sprintf("%04d", 1:9999) 

AU = "(0[289][0-9]{2})|([1345689][0-9]{3})|(2[0-8][0-9]{2})|(290[0-9])|(291[0-4])|(7[0-4][0-9]{2})|(7[8-9][0-9]{2})"

# Extract valid postcodes from all
x = regexpr(AU, all)
pc = substring(all, x, x + attr(x, "match.length") - 1)
pc[pc == ""] <- NA
pc = na.omit(pc)
pcs = sample(pc, dim(data)[1], replace=TRUE)
############

lan = c("Mandarin", 
        "Spanish", 
        "English", 
        "Hindi", 
        "Arabic", 
        "Portuguese", 
        "Bengali", 
        "Russian", 
        "Japanese", 
        "Punjabi")

lan2 = lan

edu = c("Less than High School", 
        "High School / GED", 
        "Some College", 
        "2-year College Degree", 
        "4-year College Degree", 
        "Masters Degree", 
        "Doctoral Degree", 
        "Professional Degree (JD, MD)")

hes = c("No formal education",
        "Completed or partially completed primary school",
        "Completed or partially completed junior high school",
        "Completed or partially completed senior high school",
        "Certificate or diploma",
        "Degree",
        "Post Graduate Diploma, Masters or PhD",
        "Don't know",
        "Refused")

ma = c("Full-time work greater than or equal to 30 hours paid employment per week", 
       "Part-time work less than 30 hours paid employment per week", 
       "Unemployed/looking for work", 
       "Home duties", 
       "Have a job but not at work due to illness, vacation etc", 
       "Not working and currently receiving sickness allowance/disability support pension.", 
       "Volunteer work", 
       "Student attending school", 
       "Student attending university, TAFE or other tertiary education provider", 
       "Don't know", 
       "Refused")

data$Gender = gender
data$"Location (Postcode)" = pcs
data$Language = sample(lan, dim(data)[1], replace=TRUE)
data$"Language 2" = sample(lan2, dim(data)[1], replace=TRUE)
data$Education = sample(edu, dim(data)[1], replace=TRUE)
data$"Home education status" = sample(hes, dim(data)[1], replace=TRUE)
data$"Main Activities" = sample(ma, dim(data)[1], replace=TRUE)

return(data)

}