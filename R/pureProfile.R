
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
background.color <- '#FFFBE3'
foreground.color <- '#FF7260'
title.color <- '#129793'
text.color <- '#202020'

# Colour-friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# The palette with grey:
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# blackPalette <- c("#000000")



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
  output <- paste(formatC((years) * 10, width = 2, format = "d", flag = "0"),
                  " - ",
                  formatC((years + 1) * 10 - 1, width = 2, format = "d", flag = "0"), sep = "")
  
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


  p +
      theme_tufte() +    # FOR CONSISTENT LOOK AND FEEL
	  scale_fill_manual(values = cbPalette) +
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

