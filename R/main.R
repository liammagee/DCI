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


vars.competencies <- c(
	vars.competencies.online.activities.74,
	vars.competencies.431
)
vars.interest <- c(
	vars.interests.difference.seeking.341,
	vars.interests.fitness.352,
	vars.interests.health.improvement.353,
	vars.interests.keeping.in.touch.430,
	vars.interests.general.437
)
vars.resilience <- c(
	vars.resilience.engage.with.others.428,
	vars.resilience.harm.events.434,
	vars.resilience.harms.agreement.435
)
vars.connectedness <- c(
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
vars.all <- c(
	vars.frequency,
	vars.frequency.months,
	vars.ease,
	vars.agreement,
	vars.importance
)



indicators <- loadIndicators()

# Expand valid indicators to include options, as sub-indicators
expandedIndicators <- generateExpandedVariableSet_Looped()

results <- loadSurveyResults()

# Show column names
# print(colnames(results))

age <- "Q10_159"
gender <- "Q1" # 193 = "Male", 194 = "Female"
augmented.data <- results[,c(age, gender, vars)]

# Recode variables
augmented.data$age <- augmented.data$Q10_159
# Find a better way, e.g. http://www.kkuniyuk.com/RTutorial1.pdf
augmented.data$age.breaks <- floor(augmented.data$Q10_159 / 5.0)
augmented.data$gender <- NA
augmented.data$gender[augmented.data$Q1 == 193] <- "Male"
augmented.data$gender[augmented.data$Q1 == 194] <- "Female"
augmented.data$gender[augmented.data$Q1 == 195] <- "Other"
augmented.data$gender[augmented.data$Q1 == 196] <- "Refused"

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

chartVariableByAge <- function(data, filename, metadata, labelsY) {
	p <- standardBarChart(data,
					filename,
					paste(metadata$Name, " by Age"),
					"Age by Decade",
					metadata$Name,
					labelsY
					)
	comment <- paste("Printed graph of ", metadata$Name, " to ./figs/", filename, ".png", sep="")
	if (PRINTING) {
		print(comment)
	}
	comment <- paste("Type 'open ./figs/", filename, ".png' from the terminal to view the file.", sep="")
	if (PRINTING) {
		print(comment)
	}
	return (p)
}

chartFrequencies <- function(data, filename, metadata, labelsY, desc1, desc2, use.years = TRUE) {
	p <- freqDistChart(data,
					filename,
					paste(metadata$Name, desc1),
					desc2,
					metadata$Name,
					labelsY,
					use.years
					)
	comment <- paste("Printed graph of ", metadata$Name, " to ./figs/", filename, ".png", sep="")
	if (PRINTING == TRUE) {
		print(comment)
	}
	# comment <- paste("Type 'open ./figs/", filename, ".png' from the terminal to view the file.", sep="")
	# print(comment)
	return (p)
}




# Obtain indicator names
# vars.names <- gsub("Q([[:digit:]]*)_([[:digit:]]*)", "\\1.\\2", vars)

generateChartsForVariable <- function(var) {
	var.name <- gsub("Q", "", var)
	var.parts <- unlist(strsplit(c(var.name), "_"))
	v1 <- var.parts[1]
	v2 <- var.parts[2]
	var.name <- gsub("Q([[:digit:]]*)_([[:digit:]]*)", "\\1.\\2", var)
	if (PRINTING) {
		print(var.name)
	}
}



obtainIndicatorName <- function(var) {
	var.name <- gsub("Q", "", var)
	var.parts <- unlist(strsplit(c(var.name), "_"))
	v1 <- as.integer(var.parts[1])
	v2 <- as.integer(var.parts[2])
	if (!is.na(v1)) {
		ind.name <- paste(v1, ".", v2, sep = "")
	}
	else {
		ind.name <- v1
	}
	return (ind.name)		
}

obtainIndicatorNames <- function(vars) {
	v1.last <- 0
	v.counter <- 0
	ind.names <- c()
	for (i in 1:length(vars)) {
		var <- vars[i]
		var.name <- gsub("Q", "", var)
		var.parts <- unlist(strsplit(c(var.name), "_"))
		v1 <- as.integer(var.parts[1])
		v2 <- as.integer(var.parts[2])
		if (!is.na(v1)) {
			if (v1 != v1.last) {
				v.counter <- 1
			}
			v2 <- v.counter
			ind.name <- paste(v1, ".", v2, sep = "")
			v1.last <- v1
		}
		else {
			v.counter <- 1
			ind.name <- var.name
		}
		ind.names <- c(ind.names, ind.name)
		v.counter <- v.counter + 1
	}

	return(ind.names)
}



generateSingleAgeFrequency <- function(x, vars, func) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$age.breaks)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)
	
		freqs <- table(augmented.data[,var.name], augmented.data$age.breaks)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}

	p <- chartFrequencies(freqs,
							paste("age/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by Age",
							"Age",
							TRUE)
	return (p)
}

generateAgeFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleAgeFrequency, vars, func )
}


generateSingleGenderFrequency <- function(x, vars, func) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$gender)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)
	
		freqs <- table(augmented.data[,var.name], augmented.data$gender)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}
	p <- chartFrequencies(freqs,
							paste("gender/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by Gender",
							"Gender",
							FALSE)
	return (p)
}

generateGenderFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleGenderFrequency, vars, func)
}


sumVariable <- function() {
	vars.of.interest = vars.competencies.online.activities.74
	means <- sapply(vars.of.interest, function(x) { 
		mean(augmented.data[,x])
	} )
	print(mean(means))
}

## Demographcs

sampleSize <- function() {
	return (length(augmented.data[,1]))
}


ageBreakdown <- function() {
	return (length(augmented.data[,1]))
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


# Generate all subquestion charts
generateSubQuestionCharts <- function() {
	graphSubQuestionFrequencies(vars.competencies.online.activities.74, "Frequency", frequencyLabels, "online-activities-74")
	graphSubQuestionFrequencies(vars.connectedness.maintenance.287, "Importance", importanceLabels, "maintaining-connections-287")
	graphSubQuestionFrequencies(vars.interests.difference.seeking.341, "Agreement", agreementLabels, "interests-difference-seeking-341")
	graphSubQuestionFrequencies(vars.connectedness.events.343, "Frequency", frequencyMonthLabels, "connectedness-events-343")
	graphSubQuestionFrequencies(vars.interests.fitness.352, "Frequency", frequencyMonthLabels, "interests-fitness-352")
	graphSubQuestionFrequencies(vars.interests.health.improvement.353, "Agreement", agreementLabels, "interests-health-improvement-353")
	graphSubQuestionFrequencies(vars.resilience.engage.with.others.428, "Agreement", agreementLabels, "resilience-engage-with-others-428")
	graphSubQuestionFrequencies(vars.connectedness.tech.attitudes.429, "Agreement", agreementLabels, "connectedness-tech-attitudes-429")
	graphSubQuestionFrequencies(vars.interests.keeping.in.touch.430, "Importance", importanceLabels, "keeping-in-touch-430")
	graphSubQuestionFrequencies(vars.competencies.431, "Ease", easeLabels, "competencies-ease-of-tasks-431")
	graphSubQuestionFrequencies(vars.resilience.harm.events.434, "Frequency", frequencyMonthLabels, "resilience-harm-events-434")
	graphSubQuestionFrequencies(vars.resilience.harms.agreement.435, "Agreement", agreementLabels, "resilience-harms-agreement-435")
	graphSubQuestionFrequencies(vars.interests.general.437, "Frequency", frequencyMonthLabels, "interests-general-437")
}

generateInterests <- function() {
	histogram(vars.interest, 'interests', 'gen/interests')
}

generateCompetencies <- function() {
	histogram(vars.competencies, 'competencies', 'gen/competencies')
}

generateResilience <- function() {
	histogram(vars.resilience, 'resilience', 'gen/resilience')
}

generateConnectedness <- function() {
	histogram(vars.connectedness, 'connectedness', 'gen/connectedness')
}

generateIndex <- function() {
	histogram(vars.index, 'index', 'gen/index')
}


generateAll <- function() {
	generateAgeFrequencies(vars.frequency, frequencyLabels)
	generateAgeFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateAgeFrequencies(vars.ease, easeLabels)
	generateAgeFrequencies(vars.agreement, agreementLabels)
	generateAgeFrequencies(vars.importance, importanceLabels)

	generateGenderFrequencies(vars.frequency, frequencyLabels)
	generateGenderFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateGenderFrequencies(vars.ease, easeLabels)
	generateGenderFrequencies(vars.agreement, agreementLabels)
	generateGenderFrequencies(vars.importance, importanceLabels)

	generateAgeFrequenciesForAggregate()
	generateGenderFrequenciesForAggregate()

	# Sub question frequences
	generateSubQuestionCharts()

	# Aggregate functions
	generateInterests()
	generateCompetencies()
	generateResilience()
	generateConnectedness()
	generateIndex()

}


# generateSingleAgeFrequency(1, vars.ease, easeLabels)
# generateAll()s