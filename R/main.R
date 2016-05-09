# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/samplesForExpandedIndicators.R", FALSE)
source("R/histograms.R", FALSE)
source("R/indexFunctions.R", FALSE)
source("R/multivariateAnalysis.R", FALSE)

# Install dependencies, if they are not available
installDeps()

# Imports
library(gdata)
library(reshape2)
library(plyr)

print("Loading DCI data")

# Settings
PRINTING <- TRUE

# Name variables

vars <- c(

	"Q74_1", "Q74_2", "Q74_3", "Q74_4", "Q74_5", "Q74_6", "Q74_7", "Q74_8", "Q74_9", "Q74_10", "Q74_11", "Q74_12", "Q74_13", "Q74_14", "Q74_15",
			"Q431_16", "Q431_17", "Q431_18", "Q431_19", "Q431_20", "Q431_21", "Q431_22", "Q431_23", "Q431_24", "Q431_25", "Q431_26", "Q431_27", "Q431_28", "Q431_29", "Q431_30", "Q431_31", "Q431_32", "Q431_33", "Q431_34", "Q431_35", "Q431_36", "Q431_37", "Q431_38", "Q431_39", "Q431_40", "Q431_41", "Q431_42",
			"Q437_43", "Q437_44", "Q437_45", "Q437_46", "Q437_47", "Q437_49", "Q437_50", "Q437_51", "Q437_52", "Q437_53", "Q437_54",
			"Q341_55", "Q341_56", "Q341_57", "Q341_58", "Q341_59", "Q341_60", "Q341_61",
			"Q352_62", "Q352_63", "Q352_64", "Q352_65", "Q352_66", "Q352_67", "Q352_68", "Q352_69", "Q352_70", "Q352_71",
			"Q353_72", "Q353_73",
			"Q430_74", "Q430_75", "Q430_76", "Q430_77", "Q430_78", "Q430_79", "Q430_80", "Q430_81", "Q430_82", "Q430_83", "Q430_84", "Q430_85", "Q430_86", "Q430_87",
			"Q434_88", "Q434_89", "Q434_90", "Q434_91", "Q434_92", "Q434_93", "Q434_94", "Q434_95", "Q434_96", "Q434_97", "Q434_98",
			"Q435_99", "Q435_100", "Q435_101", "Q435_102", "Q435_103", "Q435_104", "Q435_105",
			"Q428_106", "Q428_107", "Q428_108", "Q428_109", "Q428_110", "Q428_111",
			"Q277_112", "Q277_113", "Q277_114", "Q277_115", "Q277_116", "Q277_117", "Q277_118", "Q277_119", "Q277_120", "Q277_121", "Q277_122", "Q277_123",
			"Q280_124", "Q280_125", "Q280_126", "Q280_127", "Q280_128", "Q280_129", "Q280_130", "Q280_131", "Q280_132", "Q280_133", "Q280_134", "Q280_135", "Q280_136",
			"Q287_137", "Q287_138", "Q287_139", "Q287_140", "Q287_141", "Q287_142",
			"Q343_143", "Q343_144", "Q343_145", "Q343_146", "Q343_147", "Q343_148", "Q343_149", "Q343_150",
			"Q429_151", "Q429_152", "Q429_153", "Q429_154", "Q429_155", "Q429_156", "Q429_157", "Q429_158", "Q429_159",
			"Q24_160", "Q24_161", "Q24_162", "Q24_163", "Q24_164", "Q24_165", "Q24_166", "Q24_167", "Q24_168", "Q24_169", "Q24_170",
			"Q30_170", "Q30_171",
			"Q30DK",
			"Q45",
			"Q425",
			"Q427",
			"Q436",
			"Q15A_1", "Q15A_2", "Q15A_3", "Q15A_4", "Q15A_5", "Q15A_6", "Q15A_7", "Q15A_8",
			"Q15Y_1", "Q15Y_2", "Q15Y_3", "Q15Y_4", "Q15Y_5", "Q15Y_6", "Q15Y_7", "Q15Y_8", "Q15Y_9", "Q15Y_10", "Q15Y_11", "Q15Y_12", "Q15Y_13", "Q15Y_14", "Q15Y_15", "Q15Y_16",
			"Q8",
			"Q9_212", "Q9_213", "Q9_214", "Q9_215", "Q9_216", "Q9_217", "Q9_218", "Q9_219", "Q9_220"
)

vars.competencies.online.activities.74 <- c("Q74_1", "Q74_2", "Q74_3", "Q74_4", "Q74_5", "Q74_6", "Q74_7", "Q74_8", "Q74_9", "Q74_10", "Q74_11", "Q74_12", "Q74_13", "Q74_14", "Q74_15")
vars.connectedness.maintenance.287 <- c("Q287_137", "Q287_138", "Q287_139", "Q287_140", "Q287_141", "Q287_142")
vars.interests.difference.seeking.341 <- c("Q341_55", "Q341_56", "Q341_57", "Q341_58", "Q341_59", "Q341_60", "Q341_61")
vars.connectedness.events.343 <- c("Q343_143", "Q343_144", "Q343_145", "Q343_146", "Q343_147", "Q343_148", "Q343_149", "Q343_150")
vars.interests.fitness.352 <- c("Q352_62", "Q352_63", "Q352_64", "Q352_65", "Q352_66", "Q352_67", "Q352_68", "Q352_69", "Q352_70", "Q352_71")
vars.interests.health.improvement.353 <- c("Q353_72", "Q353_73")
vars.resilience.engage.with.others.428 <- c("Q428_106", "Q428_107", "Q428_108", "Q428_109", "Q428_110", "Q428_111")
vars.connectedness.tech.attitudes.429 <- c("Q429_151", "Q429_152", "Q429_153", "Q429_154", "Q429_155", "Q429_156", "Q429_157", "Q429_158", "Q429_159")
vars.interests.keeping.in.touch.430 <- c("Q430_74", "Q430_75", "Q430_76", "Q430_77", "Q430_78", "Q430_79", "Q430_80", "Q430_81", "Q430_82", "Q430_83", "Q430_84", "Q430_85", "Q430_86", "Q430_87")
vars.competencies.431 <- c("Q431_16", "Q431_17", "Q431_18", "Q431_19", "Q431_20", "Q431_21", "Q431_22", "Q431_23", "Q431_24", "Q431_25", "Q431_26", "Q431_27", "Q431_28", "Q431_29", "Q431_30", "Q431_31", "Q431_32", "Q431_33", "Q431_34", "Q431_35", "Q431_36", "Q431_37", "Q431_38", "Q431_39", "Q431_40", "Q431_41", "Q431_42")
vars.resilience.harm.events.434 <- c("Q434_88", "Q434_89", "Q434_90", "Q434_91", "Q434_92", "Q434_93", "Q434_94", "Q434_95", "Q434_96", "Q434_97", "Q434_98")
vars.resilience.harms.agreement.435 <- c("Q435_99", "Q435_100", "Q435_101", "Q435_102", "Q435_103", "Q435_104", "Q435_105")
vars.interests.general.437 <- c("Q437_43", "Q437_44", "Q437_45", "Q437_46", "Q437_47", "Q437_49", "Q437_50", "Q437_51", "Q437_52", "Q437_53", "Q437_54")

# Binary 
vars.connectedness.helping.others.277 <- c("Q277_112", "Q277_113", "Q277_114", "Q277_115", "Q277_116", "Q277_117", "Q277_118", "Q277_119", "Q277_120", "Q277_121", "Q277_122", "Q277_123")
vars.connectedness.sought.help.from.others.280 <- c("Q280_124", "Q280_125", "Q280_126", "Q280_127", "Q280_128", "Q280_129", "Q280_130", "Q280_131", "Q280_132", "Q280_133", "Q280_134", "Q280_135", "Q280_136")


vars.competencies <- c(
	vars.competencies.online.activities.74,
	vars.competencies.431
)
vars.interest <- c(
	vars.interests.general.437,
	vars.interests.difference.seeking.341,
	vars.interests.fitness.352,
	vars.interests.health.improvement.353,
	vars.interests.keeping.in.touch.430
)
vars.resilience <- c(
	vars.resilience.engage.with.others.428,
	vars.resilience.harm.events.434,
	vars.resilience.harms.agreement.435
)
vars.connectedness <- c(
	vars.connectedness.helping.others.277,
	vars.connectedness.sought.help.from.others.280,
	vars.connectedness.maintenance.287,
	vars.connectedness.events.343,
	vars.connectedness.tech.attitudes.429
)

vars.index <- c(
	vars.competencies,
	vars.interest,
	vars.resilience,
	vars.connectedness
)


# Group variables by type
vars.frequency <- c(
	vars.competencies.online.activities.74
)

vars.frequency.months <- c(
	vars.interests.general.437,
	vars.interests.fitness.352,
	vars.resilience.harm.events.434,
	vars.connectedness.events.343
)

vars.ease <- c(
	vars.competencies.431
)
vars.agreement <- c(
	vars.interests.difference.seeking.341,
	vars.interests.health.improvement.353,
	vars.resilience.harms.agreement.435,
	vars.resilience.engage.with.others.428,
	vars.connectedness.tech.attitudes.429
)

vars.importance <- c(
	vars.interests.keeping.in.touch.430,
	vars.connectedness.maintenance.287	
)
vars.yes_no <- c(
	vars.connectedness.helping.others.277,
	vars.connectedness.sought.help.from.others.280	
)

vars.all <- c(
	vars.frequency,
	vars.frequency.months,
	vars.ease,
	vars.agreement,
	vars.importance,
	vars.yes_no
)

vars.competency.totals <- c(
	"total.74",
	"total.431"
)
vars.interests.totals <- c(
	"total.437",
	"total.341",
	"total.352",
	"total.353",
	"total.430"
)
vars.resilience.totals <- c(
	"total.434",
	"total.435",
	"total.428"
)
vars.connectedness.totals <- c(
	"total.277",
	"total.280",
	"total.287",
	"total.343",
	"total.429"
)
vars.totals <- c(
	vars.competency.totals,
	vars.interests.totals,
	vars.resilience.totals,
	vars.connectedness.totals
)



indicators <- loadIndicators()

# Expand valid indicators to include options, as sub-indicators
expandedIndicators <- generateExpandedVariableSet_Looped()

results <- loadSurveyResults()

# Show column names
# print(colnames(results))

age <- "Q10_159"
gender <- "Q1" # 193 = "Male", 194 = "Female", 195 = "Other", 196 = "Refused"
state <- "STATE1"
location <- "LOCATION1"
pressure.fin <- "Q45"
pressure.digital.access <- "Q425"
current.neighbourhood <- "Q427"
level.education <- "Q436"
main.activities <- "Q8"
child.consent <- "Q5A"
child.age <- "Q5B"
augmented.data <- results[,c(age, 
								gender, 
								state, 
								location, 
								pressure.fin, 
								pressure.digital.access, 
								current.neighbourhood, 
								level.education, 
								main.activities,
								child.consent,
								child.age,
								vars)]

# Recode variables
augmented.data$age <- augmented.data$Q10_159
augmented.data$age[augmented.data$Q5A == 1 & !is.na(augmented.data$Q5A)] <- augmented.data$Q5B[augmented.data$Q5A == 1 & !is.na(augmented.data$Q5A)] + 11
# Find a better way, e.g. http://www.kkuniyuk.com/RTutorial1.pdf
augmented.data$age.breaks <- floor(augmented.data$Q10_159 / 5.0)
augmented.data$age.breaks <- NA
augmented.data$age.breaks[augmented.data$age >= 12 & augmented.data$age <= 17] <- "12 - 17"
augmented.data$age.breaks[augmented.data$age >= 18 & augmented.data$age <= 34] <- "18 - 34"
augmented.data$age.breaks[augmented.data$age >= 35 & augmented.data$age <= 49] <- "35 - 49"
augmented.data$age.breaks[augmented.data$age >= 50 & augmented.data$age <= 64] <- "50 - 64"
augmented.data$age.breaks[augmented.data$age >= 65] <- "65+"

augmented.data$gender <- NA
augmented.data$gender[augmented.data$Q1 == 193] <- "Male"
augmented.data$gender[augmented.data$Q1 == 194] <- "Female"
augmented.data$gender[augmented.data$Q1 == 195] <- "Other"
augmented.data$gender[augmented.data$Q1 == 196] <- "Refused"
augmented.data$state[augmented.data$STATE1 == 1] <- "ACT"
augmented.data$state[augmented.data$STATE1 == 2] <- "NSW"
augmented.data$state[augmented.data$STATE1 == 3] <- "VIC"
augmented.data$state[augmented.data$STATE1 == 4] <- "QLD"
augmented.data$state[augmented.data$STATE1 == 5] <- "SA"
augmented.data$state[augmented.data$STATE1 == 6] <- "WA"
augmented.data$state[augmented.data$STATE1 == 7] <- "NT"
augmented.data$state[augmented.data$STATE1 == 8] <- "TAS"
augmented.data$location[augmented.data$LOCATION1 == 1] <- "Urban"
augmented.data$location[augmented.data$LOCATION1 == 2] <- "Regional/Rural"
augmented.data$pressure.financial <- NA
augmented.data$pressure.financial[augmented.data$Q45 == 171] <- "No pressure - my household rarely or never considers the costs of basic necessities."
augmented.data$pressure.financial[augmented.data$Q45 == 172] <- "Some pressure - my household monitors its budget closely, though is generally able to afford the costs of basic necessit"
augmented.data$pressure.financial[augmented.data$Q45 == 173] <- "Considerable pressure - my household monitors its budget closely and sometimes struggles to cover the costs of basic nec"
augmented.data$pressure.financial[augmented.data$Q45 == 174] <- "Extreme pressure - my household regularly struggles to cover the costs of basic necessities."
augmented.data$pressure.digital.access <- NA
augmented.data$pressure.digital.access[augmented.data$Q425 == 175] <- "No pressure at all."
augmented.data$pressure.digital.access[augmented.data$Q425 == 176] <- "Some pressure - my household notices the financial costs of our digital life."
augmented.data$pressure.digital.access[augmented.data$Q425 == 177] <- "Considerable pressure - my household monitor mobile phone usage, download limits and electricity use closely."
augmented.data$pressure.digital.access[augmented.data$Q425 == 178] <- "Extreme pressure - my household sometimes sacrifices other necessities to pay mobile phone or broadband expenses."
augmented.data$current.neighbourhood <- NA
augmented.data$current.neighbourhood[augmented.data$Q427 == 179] <- "Less than one week."
augmented.data$current.neighbourhood[augmented.data$Q427 == 180] <- "Less than one month."
augmented.data$current.neighbourhood[augmented.data$Q427 == 181] <- "Less than six months."
augmented.data$current.neighbourhood[augmented.data$Q427 == 182] <- "Less than three years."
augmented.data$current.neighbourhood[augmented.data$Q427 == 183] <- "More than three years."
augmented.data$level.education <- NA
augmented.data$level.education[augmented.data$Q427 == 179] <- "No school."
augmented.data$level.education[augmented.data$Q427 == 180] <- "Primary school."
augmented.data$level.education[augmented.data$Q427 == 181] <- "Some secondary school."
augmented.data$level.education[augmented.data$Q427 == 182] <- "Finished secondary school."
augmented.data$level.education[augmented.data$Q427 == 183] <- "Trade training."
augmented.data$level.education[augmented.data$Q427 == 184] <- "University or college (undergraduate)."
augmented.data$level.education[augmented.data$Q427 == 185] <- "University or college (postgraduate)."
augmented.data$level.education[augmented.data$Q427 == 186] <- "Don't know."
augmented.data$level.education[augmented.data$Q427 == 187] <- "Refused."
augmented.data$main.activities <- NA
augmented.data$main.activities[augmented.data$Q427 == 200] <- "Full-time work greater than or equal to 30 hours paid employment per week"
augmented.data$main.activities[augmented.data$Q427 == 201] <- "Part-time work less than 30 hours paid employment per week"
augmented.data$main.activities[augmented.data$Q427 == 202] <- "Unemployed/looking for work"
augmented.data$main.activities[augmented.data$Q427 == 203] <- "Home duties"
augmented.data$main.activities[augmented.data$Q427 == 204] <- "Have a job but not at work due to illness, vacation etc"
augmented.data$main.activities[augmented.data$Q427 == 205] <- "Not working and currently receiving sickness allowance/disability support pension."
augmented.data$main.activities[augmented.data$Q427 == 206] <- "Volunteer work"
augmented.data$main.activities[augmented.data$Q427 == 207] <- "Student attending school"
augmented.data$main.activities[augmented.data$Q427 == 208] <- "Student attending university, TAFE or other tertiary education provider"
augmented.data$main.activities[augmented.data$Q427 == 209] <- "Other"
augmented.data$main.activities[augmented.data$Q427 == 210] <- "Don't know"
augmented.data$main.activities[augmented.data$Q427 == 211] <- "Refused"


# Create sums
augmented.data$Q74 <- round(rowMeans(augmented.data[,vars.competencies.online.activities.74])	)
augmented.data$Q287 <- round(rowMeans(augmented.data[,vars.connectedness.maintenance.287]))
augmented.data$Q341 <- round(rowMeans(augmented.data[,vars.interests.difference.seeking.341]))
augmented.data$Q343 <- round(rowMeans(augmented.data[,vars.connectedness.events.343]))
augmented.data$Q352 <- round(rowMeans(augmented.data[,vars.interests.fitness.352]))
augmented.data$Q353 <- round(rowMeans(augmented.data[,vars.interests.health.improvement.353]))
augmented.data$Q428 <- round(rowMeans(augmented.data[,vars.resilience.engage.with.others.428]))
augmented.data$Q429 <- round(rowMeans(augmented.data[,vars.connectedness.tech.attitudes.429]))
augmented.data$Q430 <- round(rowMeans(augmented.data[,vars.interests.keeping.in.touch.430]))
augmented.data$Q431 <- round(rowMeans(augmented.data[,vars.competencies.431]))
augmented.data$Q434 <- round(rowMeans(augmented.data[,vars.resilience.harm.events.434]))
augmented.data$Q435 <- round(rowMeans(augmented.data[,vars.resilience.harms.agreement.435]))
augmented.data$Q437 <- round(rowMeans(augmented.data[,vars.interests.general.437]))

## Generate totals
sumIndex()

# Shorthand
data <- augmented.data

# Generate scaled versions of data
data.scaled <- as.data.frame(scale(data[,vars.index]))
data.totals.scaled <- as.data.frame(scale(data[,vars.totals]))



## Demographcs

sampleSize <- function() {
	return (length(augmented.data[,1]))
}


ageBreakdown <- function() {
	return (length(augmented.data[,1]))
}

generateFrequencies <- function() {
	generateAgeFrequencies(vars.frequency, frequencyLabels)
	generateAgeFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateAgeFrequencies(vars.ease, easeLabels)
	generateAgeFrequencies(vars.agreement, agreementLabels)
	generateAgeFrequencies(vars.importance, importanceLabels)
	generateAgeFrequencies(vars.yes_no, yesNoLabels)

	generateGenderFrequencies(vars.frequency, frequencyLabels)
	generateGenderFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateGenderFrequencies(vars.ease, easeLabels)
	generateGenderFrequencies(vars.agreement, agreementLabels)
	generateGenderFrequencies(vars.importance, importanceLabels)
	generateGenderFrequencies(vars.yes_no, yesNoLabels)

	generateStateFrequencies(vars.frequency, frequencyLabels)
	generateStateFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateStateFrequencies(vars.ease, easeLabels)
	generateStateFrequencies(vars.agreement, agreementLabels)
	generateStateFrequencies(vars.importance, importanceLabels)
	generateStateFrequencies(vars.yes_no, yesNoLabels)

	generateLocationFrequencies(vars.frequency, frequencyLabels)
	generateLocationFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateLocationFrequencies(vars.ease, easeLabels)
	generateLocationFrequencies(vars.agreement, agreementLabels)
	generateLocationFrequencies(vars.importance, importanceLabels)
	generateLocationFrequencies(vars.yes_no, yesNoLabels)

}


generateAgeFrequenciesForAggregate <- function() {
	p <- generateSingleAgeFrequency(0, c("Q74"), frequencyLabels)
	p <- generateSingleAgeFrequency(0, c("Q287"), importanceLabels)
	p <- generateSingleAgeFrequency(0, c("Q341"), agreementLabels)
	p <- generateSingleAgeFrequency(0, c("Q343"), frequencyMonthLabels)
	p <- generateSingleAgeFrequency(0, c("Q352"), frequencyMonthLabels)
	p <- generateSingleAgeFrequency(0, c("Q353"), agreementLabels)
	p <- generateSingleAgeFrequency(0, c("Q428"), agreementLabels)
	p <- generateSingleAgeFrequency(0, c("Q429"), agreementLabels)
	p <- generateSingleAgeFrequency(0, c("Q430"), importanceLabels)
	p <- generateSingleAgeFrequency(0, c("Q431"), easeLabels)
	p <- generateSingleAgeFrequency(0, c("Q434"), frequencyMonthLabels)
	p <- generateSingleAgeFrequency(0, c("Q435"), agreementLabels)
	p <- generateSingleAgeFrequency(0, c("Q437"), frequencyMonthLabels)
	return (p)
}

generateGenderFrequenciesForAggregate <- function() {
	p <- generateSingleGenderFrequency(0, c("Q74"), frequencyLabels)
	p <- generateSingleGenderFrequency(0, c("Q287"), importanceLabels)
	p <- generateSingleGenderFrequency(0, c("Q341"), agreementLabels)
	p <- generateSingleGenderFrequency(0, c("Q343"), frequencyMonthLabels)
	p <- generateSingleGenderFrequency(0, c("Q352"), frequencyMonthLabels)
	p <- generateSingleGenderFrequency(0, c("Q353"), agreementLabels)
	p <- generateSingleGenderFrequency(0, c("Q428"), agreementLabels)
	p <- generateSingleGenderFrequency(0, c("Q429"), agreementLabels)
	p <- generateSingleGenderFrequency(0, c("Q430"), importanceLabels)
	p <- generateSingleGenderFrequency(0, c("Q431"), easeLabels)
	p <- generateSingleGenderFrequency(0, c("Q434"), frequencyMonthLabels)
	p <- generateSingleGenderFrequency(0, c("Q435"), agreementLabels)
	p <- generateSingleGenderFrequency(0, c("Q437"), frequencyMonthLabels)
	return (p)
}

generateStateFrequenciesForAggregate <- function() {
	p <- generateSingleStateFrequency(0, c("Q74"), frequencyLabels)
	p <- generateSingleStateFrequency(0, c("Q287"), importanceLabels)
	p <- generateSingleStateFrequency(0, c("Q341"), agreementLabels)
	p <- generateSingleStateFrequency(0, c("Q343"), frequencyMonthLabels)
	p <- generateSingleStateFrequency(0, c("Q352"), frequencyMonthLabels)
	p <- generateSingleStateFrequency(0, c("Q353"), agreementLabels)
	p <- generateSingleStateFrequency(0, c("Q428"), agreementLabels)
	p <- generateSingleStateFrequency(0, c("Q429"), agreementLabels)
	p <- generateSingleStateFrequency(0, c("Q430"), importanceLabels)
	p <- generateSingleStateFrequency(0, c("Q431"), easeLabels)
	p <- generateSingleStateFrequency(0, c("Q434"), frequencyMonthLabels)
	p <- generateSingleStateFrequency(0, c("Q435"), agreementLabels)
	p <- generateSingleStateFrequency(0, c("Q437"), frequencyMonthLabels)
	return (p)
}

generateLocationFrequenciesForAggregate <- function() {
	p <- generateSingleLocationFrequency(0, c("Q74"), frequencyLabels)
	p <- generateSingleLocationFrequency(0, c("Q287"), importanceLabels)
	p <- generateSingleLocationFrequency(0, c("Q341"), agreementLabels)
	p <- generateSingleLocationFrequency(0, c("Q343"), frequencyMonthLabels)
	p <- generateSingleLocationFrequency(0, c("Q352"), frequencyMonthLabels)
	p <- generateSingleLocationFrequency(0, c("Q353"), agreementLabels)
	p <- generateSingleLocationFrequency(0, c("Q428"), agreementLabels)
	p <- generateSingleLocationFrequency(0, c("Q429"), agreementLabels)
	p <- generateSingleLocationFrequency(0, c("Q430"), importanceLabels)
	p <- generateSingleLocationFrequency(0, c("Q431"), easeLabels)
	p <- generateSingleLocationFrequency(0, c("Q434"), frequencyMonthLabels)
	p <- generateSingleLocationFrequency(0, c("Q435"), agreementLabels)
	p <- generateSingleLocationFrequency(0, c("Q437"), frequencyMonthLabels)
	return (p)
}

generateAggregateFrequences <- function() {
	generateAgeFrequenciesForAggregate()
	generateGenderFrequenciesForAggregate()
	generateStateFrequenciesForAggregate()
	generateLocationFrequenciesForAggregate()
}


# Generate all subquestion charts
generateSubQuestionCharts <- function() {
	graphSubQuestionFrequencies(vars.competencies.online.activities.74, "Frequency", frequencyLabels, "online-activities-74")
	graphSubQuestionFrequencies(vars.competencies.431, "Ease", easeLabels, "competencies-ease-of-tasks-431")

	graphSubQuestionFrequencies(vars.interests.difference.seeking.341, "Agreement", agreementLabels, "interests-difference-seeking-341")
	graphSubQuestionFrequencies(vars.interests.fitness.352, "Frequency", frequencyMonthLabels, "interests-fitness-352")
	graphSubQuestionFrequencies(vars.interests.health.improvement.353, "Agreement", agreementLabels, "interests-health-improvement-353")
	graphSubQuestionFrequencies(vars.interests.general.437, "Frequency", frequencyMonthLabels, "interests-general-437")
	graphSubQuestionFrequencies(vars.interests.keeping.in.touch.430, "Importance", importanceLabels, "keeping-in-touch-430")

	graphSubQuestionFrequencies(vars.resilience.engage.with.others.428, "Agreement", agreementLabels, "resilience-engage-with-others-428")
	graphSubQuestionFrequencies(vars.resilience.harm.events.434, "Frequency", frequencyMonthLabels, "resilience-harm-events-434")
	graphSubQuestionFrequencies(vars.resilience.harms.agreement.435, "Agreement", agreementLabels, "resilience-harms-agreement-435")

	graphSubQuestionFrequencies(vars.connectedness.tech.attitudes.429, "Agreement", agreementLabels, "connectedness-tech-attitudes-429")
	graphSubQuestionFrequencies(vars.connectedness.maintenance.287, "Importance", importanceLabels, "maintaining-connections-287")
	graphSubQuestionFrequencies(vars.connectedness.events.343, "Frequency", frequencyMonthLabels, "connectedness-events-343")
}
