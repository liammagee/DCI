melted = read.csv("output/sample_values_melted.csv")

library(stringr)
category = data.frame(str_split_fixed(melted$Question, "-", n=3)[13:277,])
id = 1:dim(category)[1]
category = cbind(id, category)
names(category) = c("id", "DCI.ID", "Source", "Target")
category$DCI.ID = str_trim(category$DCI.ID)
category$Category = str_trim(category$Category)
category$Question = str_trim(category$Question)
write.csv(category, "output/category_question.csv")

