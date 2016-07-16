# http://www.r-bloggers.com/social-science-goes-r-weighted-survey-data/
# http://www.r-bloggers.com/survey-computing-your-own-post-stratification-weights-in-r/
#
# http://www.ats.ucla.edu/stat/r/faq/svy_r_post.htm

source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

library(reshape2)
library(dplyr)
library(survey)

loadWeightedData <- function() {

  sample <- as.data.frame(with(augmented.data, table(age.breaks, gender)))
  pop <- read.csv('data/abs-age-gender-june-2015.csv')

  totals.age <- rowSums(cbind(pop$Male, pop$Female, pop$Other))
  rel.age <- totals.age / sum(totals.age)
  totals.gender <- colSums(cbind(pop$Male, pop$Female, pop$Other))
  rel.gender <- totals.gender / sum(totals.gender)
  gender.names <- colnames(pop[2:4])
  age.names <- as.character(pop$age.breaks)
  age.dist <- data.frame(age.breaks = age.names, Freq = rel.age)
  gender.dist <- data.frame(gender = gender.names, Freq = rel.gender)
  test.data <- augmented.data[,c("age.breaks", "gender", vars.all)]
  unweighted.data <- svydesign(ids=~1, data = test.data)
  raked.data <- rake(design = unweighted.data,
      sample.margins = list(~age.breaks, ~gender),
      population.margin = list(age.dist, gender.dist))
  return (raked.data)

}

stackColumns <- function(col) {
  name <- paste("~", col, sep = "")
  a <- svytable(as.formula(name), raked.data)
  b <- melt(a)
  b$variable <- as.factor(colnames(b)[1])
  b$rel.freq <- b$value
  b$value <- b[, c(col)]
  b[, c(col)] <- NULL
  return (b)
}

# Add dummy column, to allow melt to work with single columns
compareWeights <- function() {
  data$dummy <- NA
  vars <- vars.all
  vars <- c(vars, "dummy")
  # Obtain a melted, long version of the column data
  m <- melt(data[,vars], id.vars = c(), na.rm = TRUE)
  # Generate counts of the item data
  cm.old <- plyr::count(m, c("variable", "value"))
  # Relative frequencies (Q10_159 is age)
  cm.old$rel.freq <- cm.old$freq / length(data$Q10_159)

  # ALTERNATIVE - USE WEIGHTED VALUES
  vars <- vars[vars != "dummy"]
  cm <- do.call("rbind", lapply(vars, stackColumns))

  cmm <- merge(cm.old, cm, by=c("variable", "value"))
  cmm$unweighted <- cmm$rel.freq.x * 100
  cmm$weighted <- cmm$rel.freq.y * 100
  cmm$diff <- cmm$unweighted - cmm$weighted
  cmm$rel.freq.x <- NULL
  cmm$rel.freq.y <- NULL
  write.table(cmm, "data/compare-unweighted-weighted.csv", row.names = F, sep = ",")
}


#do.call("rbind", lapply(vars, stackColumns))

#
# new.scores.1 <- raked.data$postStrata[[1]][[1]]
# new.scores.2 <- raked.data$postStrata[[1]][[2]]
# svytable(~Q74_1, raked.data)
# svymean(~Q74_1, unweighted.data)
# svymean(~Q74_1, raked.data)
# svymean(~Q74_2, unweighted.data)
# svymean(~Q74_2, raked.data)
#
#
#
# table(augmented.data$age.breaks)
# table(augmented.data$gender)
# age.names
# gender.names
#
# rake
#
#
# load(url("http://knutur.at/wsmt/R/RData/small.RData"))
# summary(small)
# table(small$sex)
# small.svy.unweighted <- svydesign(ids=~1, data=small)
#
