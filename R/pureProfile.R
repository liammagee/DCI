
# Libraries
library(ggplot2)
library(scales)
library(reshape2)
library(ggthemes)


# Global variables for graphs
title.size <- 0.8
character.size <- 0.6
graph.title.size <- 18
axis.title.size <- 12
axis.text.size <- 10
x.axis.vjust <- -0.25
y.axis.vjust <- 0.5
png.width = 8
png.height = 6


## DECLARE COMMON COLOURS AND PALETTES 

# Set colours
# Quasi-BTE
# background <- '#F0D2AF'
# foreground <- '#D08728'
# text.color <- '#888888'

# Experiment 1
# background.color <- '#F6FFC7'
# foreground.color <- '#668E39'
# title.color <- '#667566'
# text.color <- '#202020'

# Experiment 2
# background.color <- '#FFFBE3'
# foreground.color <- '#FF7260'
# title.color <- '#129793'
# text.color <- '#202020'

background.color <- '#FFFFFF'
foreground.color <- '#FFFFFF'
title.color <- '#000000'
text.color <- '#000000'

# Colour-friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# blackPalette <- c("#000000")
yawcrcPalette <- c("#139DEA", "#56CDFF", "#949494", "#F07899", "#EA3568", "#FFFFFF", "#000000")



# Generates year breaks to use on X axis
yearBreaks <- function(years) {
  
  # Every year
  years <- years

  return (years)

}


# Generates financial year labels for a series of years, e.g. "66/67"
yearLabels <- function(years) {

  years <- yearBreaks(years)
  
  # Formats the year into financial year form, e.g. "66/67"
  output <- paste(formatC((years) * 5, width = 2, format = "d", flag = "0"),
                  " - ",
                  formatC((years + 1) * 5 - 1, width = 2, format = "d", flag = "0"), sep = "")
  
  return (output)

}

# Frequency labels
frequencyLabels <- function() {

	labels <- c(
		"Several times each day",
		"Daily or almost daily",
		"At least every week",
		"Less often",
		"Never",
		"Don't know"
	)

	return (labels)

}
# Frequency labels (with months)
frequencyMonthLabels <- function() {

	labels <- c(
		"Several times per day",
		"Daily",
		"Weekly",
		"Monthly",
		"Less than Once a Month",
		"Never",
		"Don't know"
	)

	return (labels)

}

# Ease labels
easeLabels <- function() {

	labels <- c(
		"Very easy",
		"Easy",
		"Neither easy nor difficult",
		"Difficult",
		"Very difficult",
		"Don't know",
		"Not Applicable"
	)

	return (labels)

}


# Agreement labels
agreementLabels <- function() {

	labels <- c(
		"Strongly agree",
		"Agree",
		"Neither agree nor disagree",
		"Disagree",
		"Strongly disagree",
		"Not applicable (NA)",
		"Don't know"
	)

	return (labels)

}

# Importance labels
importanceLabels <- function() {

	labels <- c(
		"Extremely important",
		"Fairly important",
		"A little important",
		"Not important at all"
	)

	return (labels)

}




# Load indicator list, for mapping DCI.IDs to variable names
loadIndicators <- function() {
	
	results <- read.csv("data/Indicators.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))

	return (results)

}


# Load survey results supplied by Pure Profile
loadSurveyResults <- function() {
	
	results <- read.csv("data/DCI_DATA.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))

	return (results)

}


## Provides a single function for generating bar charts
standardBarChart <- function(data, file.name, title, x.label, y.label, func, use.years = TRUE) {

  # Ensure the order remains the same
  if (use.years==FALSE) {
    data$Group.1 <- factor(data$Group.1, as.character(data$Group.1))
  }

  # Change the X axis for time series
  if (use.years==TRUE) {
    x.scale <- scale_x_continuous(name = x.label,
                                  breaks = yearBreaks(data$Group.1),
                                  labels = yearLabels(data$Group.1))
  } else {
    x.scale <- xlab(x.label)
  }

  y.scale <- scale_y_continuous(name=y.label, 
  								breaks=c(1, 2, 3, 4, 5, 6),
  								labels=func())

  p <- ggplot(data, aes(x=Group.1, y = x)) +
          geom_bar(width=0.75, stat="identity")

	p <- p +
	  ggtitle(title) + 
	  x.scale +
	  y.scale

	p <- augmentChart(p, prop.freq.melted$Var1, func())

  p +
      theme_tufte() +    # FOR CONSISTENT LOOK AND FEEL
	  scale_fill_manual(values = yawcrcPalette) +
      ggtitle(title) + 
      x.scale +
      y.scale +
      
      theme(
        # GRID
        panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.major.y = element_line(colour = foreground.color),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),

        # BACKGROUND
        panel.background = element_rect(fill = background.color, colour = foreground.color),
        
        # TITLE
        # plot.title = element_text(colour = title.color, lineheight=1.0, face="bold", size=graph.title.size),
        axis.title = element_text(color=title.color, lineheight=1.0, size = axis.title.size),
        # axis.title = element_text(lineheight=1.0, size = axis.title.size),
        axis.title.x = element_text(size = axis.title.size, vjust = x.axis.vjust),
        axis.title.y = element_text(size = axis.title.size, vjust = y.axis.vjust),
        
        # LINE
        axis.line = element_line(colour = "black"),

        # TEXT
        axis.text.x = element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
        axis.text.y = element_text(color=text.color, angle=45, size = axis.text.size)
        # axis.text.x = element_text(angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
        # axis.text.y = element_text(angle=45, size = axis.text.size)
    )


  # Save the plot
  ggsave(file=paste("./figs/", file.name, ".png", sep=""),
      width = png.width,
      height = png.height
    )

  return (p)

}

## Augment chart with standard look and feel 
augmentChart <- function(p, breaks, labels) {
	p <- p +
		scale_fill_manual(name="Frequency",
				 values=yawcrcPalette,
                 breaks=unique(breaks),
                 labels=labels) +
	  theme(
	    # GRID
	    panel.grid.minor.y = element_blank(),
	    # panel.grid.major.y = element_blank(),
	    # panel.grid.major.y = element_line(colour = foreground.color),
	    panel.grid.minor.x = element_blank(),
	    panel.grid.major.x = element_blank(),

	    # BACKGROUND
	    # panel.background = element_rect(fill = background.color, colour = foreground.color),
	    
	    # TITLE
	    # plot.title = element_text(colour = title.color, lineheight=1.0, face="bold", size=graph.title.size),
	    axis.title = element_text(color=title.color, lineheight=1.0, size = axis.title.size),
	    # axis.title = element_text(lineheight=1.0, size = axis.title.size),
	    axis.title.x = element_text(size = axis.title.size, vjust = x.axis.vjust),
	    axis.title.y = element_text(size = axis.title.size, vjust = y.axis.vjust),
	    
	    # LINE
	    axis.line = element_line(colour = "black"),

	    # TEXT
	    axis.text.x = element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = axis.text.size * 0.8),
	    axis.text.y = element_text(color=text.color, angle=0, vjust=1.0, hjust=1.0, size = axis.text.size * 0.8)
	    # axis.text.x = element_text(angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
	    # axis.text.y = element_text(angle=45, size = axis.text.size)

	)



	return (p)
}

## Provides a single function for generating frequency distribution bar charts
freqDistChart <- function(freq.table, file.name, title, x.label, y.label, func, use.years = TRUE) {

	# Assume the columns contain decade breaks
	prop.freq.table <- prop.table(freq.table, 2)

	# Melt data for graphing
	prop.freq.melted <- melt(prop.freq.table)

	# Change the X axis for time series
	if (use.years==TRUE) {
		years <- yearBreaks(unique(prop.freq.melted$Var2))
		x.scale <- scale_x_continuous(name = x.label,
	                              breaks = years,
	                              labels = yearLabels(years))
	} else {
		x.scale <- xlab(x.label)
	}

	y.scale <- scale_y_continuous(name = y.label,
									breaks = seq(0.0, 1.0, by = 0.2), 
									labels = paste(seq(0, 100, by = 20), "%", sep = ""))

	# Graph so that x = 2nd variable (age.breaks), y = data, fill = responses
	p <- ggplot(data = prop.freq.melted, 
		aes(x = Var2, y = value, fill = factor(Var1))) + 
	    geom_bar(width=0.75, stat="identity") + 
	    coord_flip() 

	p <- p +
	  ggtitle(title) + 
	  x.scale +
	  y.scale

	p <- augmentChart(p, prop.freq.melted$Var1, func())

	full.file <- paste("./figs/", file.name, ".png", sep="") 

	# Save the plot
	ggsave(file = full.file,
	  width = png.width,
	  height = png.height
	)

	# Open the file if on Mac - TODO: check other OS'es
	if (Sys.info()['sysname'] == "Darwin") {
		#system2("open", full.file)
	}

	return (p)

}



ageChart <- function(augmented.data) {

	age.breaks <- sort(unique(augmented.data$age.breaks))
	age.breaks.labels <- sort(unique(augmented.data$age.breaks * 5))

	x.scale <- scale_x_continuous(name = "Ages",
									breaks = age.breaks, 
									labels = age.breaks.labels)

	p <- ggplot(augmented.data, aes(x=age.breaks)) +
				geom_histogram(binwidth=.5) + 
				x.scale + 
	    		coord_flip() 

	p <- augmentChart(p, augmented.data$age.breaks, unique(augmented.data$age.breaks * 5))

	return (p)
}


genderChart <- function(augmented.data) {

	p <- ggplot(augmented.data, aes(x=gender)) +
				geom_bar(width=0.5) + 
				# x.scale + 
	    		coord_flip() 

	p <- augmentChart(p, augmented.data$gender, unique(augmented.data$gender))

	return (p)
}


genderAndAgeChart <- function(augmented.data) {

	age.breaks <- sort(unique(augmented.data$age.breaks))
	age.breaks.labels <- sort(unique(augmented.data$age.breaks * 5))
	x.scale <- scale_x_continuous(name = "age.breaks",
									breaks = age.breaks, 
									labels = age.breaks.labels)

	ad <- table(augmented.data$gender, augmented.data$age.breaks)
	ad <- melt(ad)

	p <- ggplot(ad, aes(x=Var2, y=value, group=Var1, fill=Var1)) +
				geom_bar(width=0.5, stat="identity", position="dodge") + 
				x.scale + 
	    		coord_flip() 

	p <- augmentChart(p, augmented.data$gender, unique(augmented.data$gender))

	return (p)
}

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


histogram <- function(cols, name, file.name) {
	aggMeans <- data.frame(round(rowSums(augmented.data[,cols])))
	colnames(aggMeans)[1] <- name
	x.scale <- scale_x_continuous(name = name)
	p <- ggplot(data = aggMeans, aes(x = aggMeans[1])) + 
		geom_histogram(binwidth = 0.5) +
		geom_density(aes(y=1.0*..count..), colour="red", adjust=4) +
		x.scale

	full.file <- paste("./figs/", file.name, ".png", sep="") 

	# Save the plot
	ggsave(file = full.file,
	  width = png.width,
	  height = png.height
	)

	return (p)
}

histogram <- function(cols, name, file.name) {
	aggMeans <- data.frame(round(rowSums(augmented.data[,cols])))
	colnames(aggMeans)[1] <- name
	x.scale <- scale_x_continuous(name = name)
	p <- ggplot(data = aggMeans, aes(x = aggMeans[1])) + 
		geom_histogram(binwidth = 0.5) +
		geom_density(aes(y=1.0*..count..), colour="red", adjust=4) +
		x.scale

	full.file <- paste("./figs/", file.name, ".png", sep="") 

	# Save the plot
	ggsave(file = full.file,
	  width = png.width,
	  height = png.height
	)

	return (p)
}

# vars.connectedness.maintenance.287, "Importance", importanceLabels, "maintaining-connections-287"
# Generic function that returns relative frequencies of sub questions (items) as a graph
graphSubQuestionFrequencies  <- function(vars, legend.name, legendBreakFunc, file.name) {
	# Obtain a melted, long version of the column data
	m <- melt(data[,vars], id.vars = c())
	# Generate counts of the item data
	cm <- count(m, c("variable", "value"))
	# Relative frequencies (Q10_159 is age)
	cm$rel.freq <- cm$freq / length(data$Q10_159)
	# Rename for simplicity - TODO: push up to the global variable
	ex <- expandedIndicators
	# Complicated code that loads truncated item names into the melted data
	var.names = as.character(unlist(sapply(obtainIndicatorNames(unique(cm$variable)), function(lbl) {
		v <- ex[which(ex$DCI.ID == lbl),2]
		# Assume a name like "74 - 1 - XXX"
		v <- unlist(strsplit(as.character(v), "-"))[3]
		# Truncate if too long
		v <- substring(v, 1, 30)
		# Add back the ID, for reference
		v <- paste(lbl, v, sep = " ")
		return (v)
	})))
	# Construct scales
	x.scale <- scale_x_discrete(name = "Questions",
									breaks = unique(cm$variable), 
									labels = var.names)
	y.scale <- scale_y_continuous(name = "Percentage",
									breaks = seq(0.0, 1.0, by = 0.2), 
									labels = paste(seq(0, 100, by = 20), "%", sep = ""))
	fill.scale <- scale_fill_manual(name=legend.name,
				 values=yawcrcPalette,
                 breaks=seq(1:length(legendBreakFunc())),
                 labels=legendBreakFunc())
	# Generate plot
	p <- ggplot(data = cm, 
		aes(x = variable, y = rel.freq, fill = factor(value))) + 
	    geom_bar(width = 0.5, stat = "identity") + 
	    coord_flip() +
	    x.scale +
	    y.scale +
	    fill.scale
	# p <- p + theme(
	# 	axis.text.y = element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = axis.text.size * 0.8)
	# )

	full.file <- paste("./figs/gen/", file.name, ".png", sep="") 

	# Save the plot
	ggsave(file = full.file,
	  width = png.width,
	  height = png.height
	)

	return (p)
}
