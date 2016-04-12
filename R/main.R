# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/samplesForExpandedIndicators.R", FALSE)
source("R/pureProfile.R", FALSE)

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
vars.resilience.totals <- c(
	"total.277",
	"total.280",
	"total.287",
	"total.343",
	"total.420"
)
vars.totals <- c(
	vars.competency.totals,
	vars.interests.totals,
	vars.resilience.totals,
	vars.resilience.totals
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

# Shorthand
data <- augmented.data


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

generateIndexForCompetencies <- function() {
	# Competencies are correctly coded: low values mean higher competencies
	vars.competencies.for.index <- vars.competencies
	histogram(vars.competencies.for.index, 'Competencies', 'gen/index-competencies')
}


generateIndexForInterests <- function() {
	# Interests are correctly coded: low values mean higher competencies
	vars.interest.for.index <- vars.interest
	histogram(vars.interest.for.index, 'Interests', 'gen/index-interests')
}

generateIndexForResilience <- function() {
	# Resilience is more complex; high scores on some responses can be 
	# interpreted as more resilient, others as less resilient

	# Reverse harm  results - these are interpreted as meaning *less* resilient
	vars.resilience.harm.events.434.for.index <- c()
	for (i in 1:length(vars.resilience.harm.events.434)) {
		var.name <- vars.resilience.harm.events.434[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		d <- replace(d, d == -1, 0)
		vars.resilience.harm.events.434.for.index <- c(vars.resilience.harm.events.434.for.index, var.name)
		augmented.data[new.var] <<- d
	}

	vars.resilience.for.index <- c(
		vars.resilience.harm.events.434.for.index,
		vars.resilience.harms.agreement.435
		# TODO: Determine what this means in terms of resilience
		# vars.resilience.engage.with.others.428
	)
	histogram(vars.resilience.for.index, 'Resilience', 'gen/index-resilience')
}

generateIndexForConnectedness <- function() {

	# Similar to vars.connectedness
	# TODO: remove or reverse responses to 'tech attitudes'
	vars.connectedness.for.index <- c(
		vars.connectedness.helping.others.277,
		vars.connectedness.sought.help.from.others.280,
		vars.connectedness.maintenance.287,
		vars.connectedness.events.343,
		vars.connectedness.tech.attitudes.429
	)
	histogram(vars.connectedness.for.index, 'Connectedness', 'gen/index-connectedness')
}

generateIndexForAll <- function() {
	vars.all.for.index <- vars.index
	histogram(vars.all.for.index, 'All Measures', 'gen/index-all')
}

generateIndexes <- function() {
	generateIndexForInterests()
	generateIndexForCompetencies()
	generateIndexForResilience()
	generateIndexForConnectedness()
	generateIndexForAll()
}

generateAll <- function() {

	# Specific question frequences by age, gender, state, location
	generateFrequencies()
	# Aggregate frequences by age, gender, state, location
	generateAggregateFrequences()

	# Sub question frequences
	generateSubQuestionCharts()

	# Index functions
	generateIndexes()

}

sumCompetencies <- function() {
	# Competencies are correctly coded: low values mean higher competencies
	vars.competencies.for.index <- c()

	# Totals
	total.74 <- 0
	total.431 <- 0

	# Score frequency
	for (i in 1:length(vars.competencies.online.activities.74)) {
		var.name <- vars.competencies.online.activities.74[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Less often" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.competencies.for.index <- c(vars.competencies.for.index, new.var)
		augmented.data[new.var] <<- d
		total.74 <- total.74 + d
	}
	# Score ease
	for (i in 1:length(vars.competencies.431)) {
		var.name <- vars.competencies.431[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither easy nor difficult" - conservative guess
		# Convert "Not applicable" to "Neither easy nor difficult" - conservative guess
		d <- replace(d, d == -1, 2)
		d <- replace(d, d == -2, 2)
		vars.competencies.for.index <- c(vars.competencies.for.index, new.var)
		augmented.data[new.var] <<- d
		total.431 <- total.431 + d
	}

	augmented.data$total.74 <<- total.74
	augmented.data$total.431 <<- total.431

	# Max value: (Q74) +  (Q431)
	# Max value: 15 * 4 + 27 * 4
	divisor <- 168
	augmented.data$competencies.index <<- 100 * rowSums(augmented.data[,vars.competencies.for.index]) / divisor
	return(augmented.data$competencies.index)
}

sumInterests <- function() {
	# Score frequency
	vars.interests.for.index <- c()

	# Totals
	total.437 <- 0
	total.341 <- 0
	total.352 <- 0
	total.353 <- 0
	total.430 <- 0

	for (i in 1:length(vars.interests.general.437)) {
		var.name <- vars.interests.general.437[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.437 <- total.437 + d
	}
	for (i in 1:length(vars.interests.difference.seeking.341)) {
		var.name <- vars.interests.difference.seeking.341[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.341 <- total.341 + d
	}
	for (i in 1:length(vars.interests.fitness.352)) {
		var.name <- vars.interests.fitness.352[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.352 <- total.352 + d
	}
	for (i in 1:length(vars.interests.health.improvement.353)) {
		var.name <- vars.interests.health.improvement.353[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.353 <- total.353 + d
	}
	for (i in 1:length(vars.interests.keeping.in.touch.430)) {
		var.name <- vars.interests.keeping.in.touch.430[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 4 - c
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.430 <- total.430 + d
	}

	augmented.data$total.437 <<- total.437
	augmented.data$total.341 <<- total.341
	augmented.data$total.352 <<- total.352
	augmented.data$total.353 <<- total.353
	augmented.data$total.430 <<- total.430

	# Max value: (Q437) + (Q341) + (Q352) + (Q353) + (Q341)
	# Max value: 11 * 5 + 7 * 4  + 10 * 5  + 2 * 4 + 14 * 3
	divisor <- 183
	augmented.data$interests.index <<- 100 * rowSums(augmented.data[,vars.interests.for.index]) / divisor
	return(augmented.data$interests.index)
	
}

sumResilience <- function() {
	# Score frequency
	vars.resilience.for.index <- c()

	total.434 <- 0
	total.435 <- 0
	total.428 <- 0

	for (i in 1:length(vars.resilience.harm.events.434)) {
		var.name <- vars.resilience.harm.events.434[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		# d <- 6 - c
		d <- c - 1
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
		augmented.data[new.var] <<- d
		total.434 <- total.434 + d
	}
	for (i in 1:length(vars.resilience.harms.agreement.435)) {
		var.name <- vars.resilience.harms.agreement.435[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
		augmented.data[new.var] <<- d
		total.435 <- total.435 + d
	}

	# TODO: Meaning of 106-109 for resilience? Maybe connectedness?

	# QUESTION: When I am going through a difficult time, I go online less often
	var.name <- "Q428_110"
	new.var <- paste(var.name, ".for.index", sep = "")
	c <- augmented.data[,var.name]
	# Normalise - Strongly agree means *less* resilient
	d <- c - 1
	# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == 5, 2)
	# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == 6, 2)
	vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
	augmented.data[new.var] <<- d
	total.428 <- total.428 + d

	# QUESTION: When I am going through a difficult time, going online makes me feel better
	var.name <- "Q428_111"
	new.var <- paste(var.name, ".for.index", sep = "")
	c <- augmented.data[,var.name]
	# Normalise - Strongly agree means *more* resilient
	d <- 5 - c
	# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == -2, 2)
	# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == -1, 2)
	vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
	augmented.data[new.var] <<- d
	total.428 <- total.428 + d

	augmented.data$total.434 <<- total.434
	augmented.data$total.435 <<- total.435
	augmented.data$total.428 <<- total.428

	# Max value: (Q434) + (Q435) + (Q428)
	# Max value: 11 * 5 + 7 * 4  + 2 * 4
	divisor <- 91
	augmented.data$resilience.index <<- 100 * rowSums(augmented.data[,vars.resilience.for.index]) / divisor
	return(augmented.data$resilience.index)
	
}

sumConnectedness <- function() {
	# Score frequency
	vars.connectedness.for.index <- c()

	total.277 <- 0
	total.280 <- 0
	total.287 <- 0
	total.343 <- 0
	total.420 <- 0
	
	for (i in 1:length(vars.connectedness.helping.others.277)) {
		var.name <- vars.connectedness.helping.others.277[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- c - 1
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.277 <- total.277 + d
	}
	for (i in 1:length(vars.connectedness.sought.help.from.others.280)) {
		var.name <- vars.connectedness.sought.help.from.others.280[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- c - 1
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.280 <- total.280 + d
	}
	for (i in 1:length(vars.connectedness.maintenance.287)) {
		var.name <- vars.connectedness.maintenance.287[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 3 - c
		# Convert "Don't know" to "Not important at all" - conservative guess
		d <- replace(d, d == -1, 0)
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.287 <- total.287 + d
	}
	for (i in 1:length(vars.connectedness.events.343)) {
		var.name <- vars.connectedness.events.343[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.343 <- total.343 + d
	}
	for (i in 1:length(vars.connectedness.tech.attitudes.429)) {
		var.name <- vars.connectedness.tech.attitudes.429[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.420 <- total.420 + d
	}

	augmented.data$total.277 <<- total.277
	augmented.data$total.280 <<- total.280
	augmented.data$total.287 <<- total.287
	augmented.data$total.343 <<- total.343
	augmented.data$total.420 <<- total.420

	# Max value: (Q277) + (Q280) + (Q287) + (Q343) + (Q429)
	# Max value: 13 * 1 + 13 * 1 + 6 * 2  + 8 * 5  + 9 * 4 
	divisor <- 114
	augmented.data$connectedness.index <<- 100 * rowSums(augmented.data[,vars.connectedness.for.index]) / divisor
	return(augmented.data$connectedness.index)
	
}

sumIndex <- function() {
	sct <- sumCompetencies()
	sin <- sumInterests()
	sre <- sumResilience()
	scn <- sumConnectedness()
	augmented.data$combined.index <<- ( sct + sin + sre + scn ) / 4
	return ( augmented.data$combined.index )
}

generateIndexChart <- function() {

	sc <- data.frame(sort(sumCompetencies()))
	si <- data.frame(sort(sumInterests()))
	sr <- data.frame(sort(sumResilience()))
	ss <- data.frame(sort(sumConnectedness()))
	sa <- data.frame(sort(sumIndex()))
	sall <- cbind(sc, si, sr, ss, sa)
	colnames(sall)[1] <- "Competencies"
	colnames(sall)[2] <- "Interests"
	colnames(sall)[3] <- "Resilience"
	colnames(sall)[4] <- "Connectedness"
	colnames(sall)[5] <- "Combined"
	sall$id <- seq(1:length(sall$Competencies))

	msc <- melt(sall, id = "id")

	p <- ggplot(data=msc, aes(x = variable, y = value, colour=value)) + 
		geom_bar(stat="identity", position = "fill") + 
	    scale_colour_gradientn(colours=yawcrcPaletteAggregate) +
	    coord_flip() 
    return (p)
}

randomFunctions <- function() {
  sumIndex()
  
	data <- augmented.data
	# Correlations for interest columns
	cor(data[,vars.interests.totals])

	# Pairwise scatterplot for interest columns
	pairs(~total.437+total.341+total.352+total.353+total.430,data=data, 
      main="Simple Scatterplot Matrix")

	# Scatterplot of 2 variables
	p <- plot(data$total.437, data$total.341, main="Scatterplot Example", 
  		xlab="Gen Interests", ylab="Difference Seeking", pch=19)

	# Add fit lines
	abline(lm(data$total.437~data$total.341), col="red") # regression line (y~x) 
	lines(lowess(data$total.341,data$total), col="blue") # lowess line (x,y)
	p

	# ggplot variant
	# http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/
	ggplot(data, aes(x=total.437, y=total.341)) +
	    geom_point(shape=1) +    # Use hollow circles
	    geom_smooth(method=lm)   # Add linear regression line 
	                             #  (by default includes 95% confidence region)	

	# http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/#set-colorshape-by-another-variable
	ggplot(data, aes(x=total.437, y=total.341, color=gender)) +
	    geom_point(shape=1) +    # Use hollow circles
	    geom_smooth(method=lm)   # Add linear regression line 

}

# generateSingleAgeFrequency(1, vars.ease, easeLabels)
# generateAll()s