##Editing and Making Publication Ready Plots
#Student Presentation: By Shea Wales

# ======================= Class outline =======================
#Today we are going to be discussing different ways to modify and customize your figures. 

#(1) Let's talk: TYPES OF PLOTS
#We will briefly review some types of plots that you can make in r
  ## Bar Plots / Histograms 
  ## Scatter Plots
  ## Line Plots
  ## Box Plots
  ## Violin Plots 

#(2) Let's talk: PRE-SETS
#I'll show you how to store/save a plot, and introduce you to some pre##set themes already available in ggplot2: 
  ## theme_gray
  ## theme_bw
  ## theme_linedraw
  ## theme_light
  ## theme_dark
  ## theme_minimal
  ## theme_classic
  ## theme_void
  ## theme_test

#(3) Let's talk: EDITING ANYTHING & EVERYTHING
#We will use histograms to demonstrate how you can alter almost anything within a ggplot to make it look the way you want it. We will specifically touch on:
  ## bin-width 
  ## plot title
  ## axis and legend labels
  ## How to add a theme to, and remove a theme from, a stored plot
  ## How use color palettes, and make your own. 
  ## How to use alter default themes: 
      ## Text 
      ## Background color
      ## Grid lines
      ## Tick Marks
      ## Legend location 
  ## Storing plots 
  ## Setting and defining themes for later use (AKA: Your personal publication style plots!) 

# ======================= Class outline ======================= #

#### Setup: Load Libraries & Import Data ####
#Read in Libraries 
    #install.packages("ggplot2")
    library(ggplot2)
    #install.packages("plyr")
    library(plyr) 
    #install.packages("RColorBrewer")
    library(RColorBrewer)
    #install.packages("tidyr")
    library(tidyr)

# write out all the offending strings
na_strings <- c("#DIV/0!", "", " ", "NA")

# read in the data
data2 <- read.csv("data/LTREB_classPRACdata2.csv", header = TRUE, na = na_strings, check.names = FALSE)

# Let's look at the data
str(data2)

names(data2)
names(data2) <- c("count","stand","plot","tree_id","species","DBH","2017","2016", "2015","2014","2013","2012","2011", "2010", "2009", "2008", "2007", "2006","avg_rgr","species_long","a","b","DBH_range_cm")

# Side Note: If you want to rename groups within an observation you can uses plyr's revalue() function. If I wanted to make stand names more descriptive, for example...
#summary(data2$stand)
#data2$stand <- revalue(data2$stand, c("BP36" = "Cut&Burn_1936", "BP80" = "Cut&Burn_1980"))

#### Let's talk: TYPES OF PLOTS ####
    # --> Use arrows to open up sections!
# Bar Plot / histograms #########
    # x: must be numerical 
    # fill: what you want to color code by

ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram()

#Pick better value with `binwidth`? Okay, let's change that by doing this:
ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 3)

#What if you want to zoom in or out on the graph? You can change the axis limits like this:
ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 3) + xlim(0,150)

#Here is another example of this type of graph, using other data (in this case DBH vs. stand)
ggplot(data2, aes(x = DBH, fill = stand)) + geom_histogram(binwidth = 3)

# Scatter plot #########
    # x: must be numerical 
    # y: must be numerical 
    # color: what you want to color code by
        ## When you have points use "color" when you have a bar chart use "fill"
ggplot(data2, aes(x = DBH, y = avg_rgr)) + geom_point()

# Here is an example of how to color code by a catagorical variable - species.
ggplot(data2, aes(x = DBH, y = avg_rgr, color = species)) + geom_point()

# Need to adjust axis lengths? (zoom in or out?) Change it like this:
ggplot(data2, aes(x = DBH, y = avg_rgr, color = species)) + geom_point() + xlim(-500,500) + ylim(-500,500)

# Want to visualize your data by another catagorical variable? Here are a few ways to do so:
ggplot(data2, aes(x = DBH, y = avg_rgr, color = species)) + geom_point() + facet_wrap(~stand)

ggplot(data2, aes(x = DBH, y = avg_rgr, color = species)) + geom_point() + facet_grid(~stand)
ggplot(data2, aes(x = DBH, y = avg_rgr, color = species)) + geom_point() + facet_grid(stand ~ .)


# Line Plot  #########
ggplot(data2, aes(x = DBH, y = avg_rgr, color = species)) + geom_line()
ggplot(data2, aes(x = DBH, y = avg_rgr, color = species)) + geom_line(size=3)

# Box Plots  #########
    # fill: what you want to color code by
    # adjust: Default is 1, lower means finer resolution (less smooth)
    # Don't want a legend? No problem. Use + guides(fill = FALSE)

#Here is a box plot of one continous varaible - average relative growth rate (RGR)
ggplot(data2, aes(x = species, y = avg_rgr)) + geom_boxplot()

#Here is a box plot of one continous varaible - - average relative growth rate (RGR) - now colorcoded by stand
ggplot(data2, aes(x = species, y = avg_rgr, fill = species)) + geom_boxplot()
# and now the x and y axes have been flipped
ggplot(data2, aes(x = species, y = avg_rgr, fill = species)) + geom_boxplot() + coord_flip()

# Violin Plots  #########
    # fill: what you want to color code by
    # adjust: Default is 1, lower means finer resolution (less smooth)
    # Don't want a legend? No problem. Use + guides(fill = FALSE)
ggplot(data2, aes(x = species, y = avg_rgr)) + geom_violin()
ggplot(data2, aes(x = species, y = avg_rgr, fill = species)) + geom_violin()

# Lets swich to graphing these by stand 
ggplot(data2, aes(x = stand, y = avg_rgr, fill = stand)) + geom_violin()

# Want to change "adjust" the smoothness??
ggplot(data2, aes(x = stand, y = avg_rgr, fill = stand)) + geom_violin(adjust = 100)
ggplot(data2, aes(x = stand, y = avg_rgr, fill = stand)) + geom_violin(adjust = 0.5)

# Need to remove the legend? Use guides(fill = FALSE)
ggplot(data2, aes(x = stand, y = avg_rgr, fill = stand)) + geom_violin() + guides(fill = FALSE)

# You can log-scale any numerical axis on any plot
ggplot(data2, aes(x = stand, y = avg_rgr, fill = stand)) + geom_violin() + scale_y_log10()

# Want to log-scale both the x and y axis? We need another continous variable. Let's use DBH. 
ggplot(data2, aes(x = DBH, y = avg_rgr, fill = stand)) + geom_violin()
ggplot(data2, aes(x = DBH, y = avg_rgr, fill = stand)) + geom_violin() + scale_y_log10() 
ggplot(data2, aes(x = DBH, y = avg_rgr, fill = stand)) + geom_violin() + scale_y_log10() + scale_x_log10() 

#### Let's talk: PRE-SETS ####

# Store the plot. This makes it easier to minpulate features!
basic <- ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 2) + labs(x = "Stem Diameter at Breast Height (DBH)", y = "Count", fill = "Species")

# Basic (AKA: a normal plot). Now call it (so we can look at it)!
basic

#The default theme in basicplot2 is theme_gray. There are a set of pre-made themes you can use which are cohesive themes that don’t require modifying individual elements.
basic + theme_gray()
basic + theme_bw()
basic + theme_linedraw()
basic + theme_light()
basic + theme_dark()
basic + theme_minimal()
basic + theme_classic()
basic + theme_void()
basic + theme_test()

#Need to take a closer look to see the diffrence between them? Try facet_wrap!
basic + theme_gray() + facet_wrap(~ stand)
basic + theme_bw() + facet_wrap(~ stand) # diffrence between theme_bw and theme_light is more apparent here
basic + theme_light() + facet_wrap(~ stand) # diffrence between theme_bw and theme_light is more apparent here
basic + theme_dark() + facet_wrap(~ stand)

#### Let's talk: EDITING ANYTHING & EVERYTHING ####
# The basics  #########
# Let's use a bar graph/histogram as an example 
ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram()

# Again we can change the binwidth!
ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 50)
ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 2)

# Add a plot title
ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 2) + labs(title = "DBH Distribution")

# Change axis (x and y = "") and legend (fill = "") labels
ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 2) + labs(x = "Stem Diameter at Breast Height (DBH)", y = "Count", fill = "Species")

# Don't know what species abbreviations mean, or want to change how ggplot2 is color coding? (fill = species --> species_long)
ggplot(data2, aes(x = DBH, fill = species_long)) + geom_histogram(binwidth = 2) + labs(x = "Stem Diameter at Breast Height (DBH)", y = "Count", fill = "Species")

# Let's store the plot
basic <- ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 2) + labs(x = "Stem Diameter at Breast Height (DBH)", y = "Count", fill = "Species")

# Running basic will display the plot
basic

# Color palettes  #########
display.brewer.all()

basic + scale_fill_brewer(palette="Set1")
basic + scale_fill_brewer(palette="Spectral")
basic + scale_fill_brewer(palette="Paired")
basic + scale_fill_brewer(palette="Pastel1")
basic + scale_fill_brewer(palette="YlOrRd")

# Try one on your own: 
display.brewer.all()
basic + scale_fill_brewer(palette="")

# We can also change the colors manually. See a list below:
colors()

# For example, I chose 9 colors to plot my 9 species:
basic + scale_fill_manual(values = c("peru","palegreen3","tomato", "sienna1", "salmon4", "maroon", "lightslategrey", "peachpuff1", "palegreen4"))

# Here is another example...
species_plot <- ggplot(data2, aes(x = species, fill = stand)) + geom_bar()
species_plot

# And let's say we did't like rainbow (because lack of contrast reduces readability etc.) we might realize that some of the other color palettes are too short:
species_plot + scale_fill_brewer()
species_plot + scale_fill_brewer(palette = "Dark2")

# But that's okay because we can make our own!
display.brewer.all()

# to get the colors from a palette:
palette1 <- brewer.pal(9,"YlOrRd")
palette1

palette2 <- brewer.pal(8,"Greens")
palette2

palette3 <- brewer.pal(9,"Blues")
palette3

# We can use a quick pie chart to see the colors:
pie(rep(1, length(palette1)), col = palette1)
pie(rep(1, length(palette2)), col = palette2)
pie(rep(1, length(palette3)), col = palette3)

# Now let's combine those color pallettes we made
big_palette <- c(palette1, palette2, palette3)
big_palette

# Pie chart of all the colors together:
pie(rep(1,length(big_palette)),col=big_palette)

#Let's plot species_plot again using the new color palette - big_palette
species_plot + scale_fill_manual(values = big_palette)

#You may have notice it used the colors in order (1-3)
# To shuffle the colors use values = sample()
species_plot + scale_fill_manual(values = sample(big_palette))

# use set.seed() to resuffle the colors randomly. Keep using set.seed() untill you find a combo you like!
set.seed(5)
# use different numbers until you find your favorite colors
species_plot + scale_fill_manual(values = sample(big_palette))
# This is possible, because "random" numbers from a computer aren't really random


# Modifying everything else #######
# Let's make sure our plots are set to the defult theme - theme_gray():
theme_set(theme_gray())

# We are going to use the same saved plot, basic to easily minipulate plot attributes:
basic <- ggplot(data2, aes(x = DBH, fill = species)) + geom_histogram(binwidth = 2) + labs(x = "Stem Diameter at Breast Height (DBH)", y = "Count", fill = "Species")

# Basic, normal plot:
basic

# Here is how to change fonts and font sizes for everything in the plot all at once - within a theme.
## According to one site, there are only three fonts that are guaranteed to work everywhere: “sans” (the default), “serif”, or “mono”... so heads up. 
basic + theme_gray(base_size = 15) # base_family = "sans" is default 
basic + theme_gray(base_size = 15, base_family = "American Typewriter")
basic + theme_gray(base_size = 15, base_family = "serif")
basic + theme_gray(base_size = 15, base_family = "Times New Roman")

# What if we wanted to change things seperatly:
basic
basic + theme(axis.text=element_text(size=20)) # numbers on axes
basic + theme(axis.title=element_text(size=20)) # titles on axes
basic + theme(legend.title=element_text(size=20)) # legend title
basic + theme(legend.text=element_text(size=20)) # legend category labels

# Changing things all at once:
basic
basic + theme(
  legend.title=element_text(size=15, family = "mono"),
  legend.text=element_text(size=12, family = "mono"),
  axis.title=element_text(size=15, family = "mono"),
  axis.text=element_text(size=12, family = "mono")
)

# We can change background color
basic + theme(panel.background = element_rect(fill="pink"))
basic + theme(panel.background = element_rect(fill="white"))

# We can change grid-lines
basic + theme(panel.grid.major = element_line(color = "black"), panel.grid.minor = element_line(color = "red"))

# We can remove all gridlines:
basic + theme(panel.grid.major = element_line(NA), 
              panel.grid.minor = element_line(NA))

# Thin black major gridlines on y-axis, the others are removed
basic + theme(panel.grid.major.y = element_line(color = "black", size = 0.2), 
              panel.grid.major.x = element_line(NA),
              panel.grid.minor = element_line(NA), 
              panel.background = element_rect(fill = "white"))

# Change tick-marks sepratly (for visualization's sake)
basic # normal ticks
basic + theme(axis.ticks = element_line(size = 2))
basic + theme(axis.ticks = element_line(NA))
basic + theme(axis.ticks = element_line(color = "blue",size = 1))

# Change tick-marks (multiple at once)
basic + theme(axis.ticks = element_line(size = 2), # affects both x and y
              axis.ticks.x = element_line(color = "blue"), # x only
              axis.ticks.y = element_line(color = "red"))  # y only

# Change location of the legend
basic + theme(legend.position = "top")
basic + theme(legend.position = "bottom")

#Think of the plot/figure as a coordinate system. You can change the position of attributes if you tell it where to go using this cord-system. 
basic + theme(legend.position = c(0,0.5)) # bottom left
basic + theme(legend.position = c(0,0)) # bottom left
basic + theme(legend.position = c(1,1)) # top right
basic + theme(legend.position = c(0.9,0.8)) # near the top right

# Remove legend title
basic + labs(fill = "")

# Remove legend completely
basic + guides(fill=FALSE)

# Let's talk: Making/storing your own "PUBLICATION STYLE" theme  ####

# Ususally for publication we need: 
      # clear background
      # axis lines but no box
      # no grid lines
      # basic colors 
      # a legend
            ## no legend?? No problem! Add "+ guides(fill = FALSE)" to your pot.

#Let's define a theme (AKA: What we type into the theme() is how the graph will look...)
publication_style <- basic +
  theme(
    axis.line = element_line(size=0.5), # x and y axis lines 
    panel.background = element_rect(fill = NA), # no background fill (, size = rel(20))
    panel.grid.minor = element_line(color = NA), # no minor lines
    axis.text = element_text(size = 16), # axis text = size 16
    axis.title = element_text(size = 18), # axis title text = size 18
    legend.text = element_text(size = 13), # legend text = size 13
    legend.title = element_text(size = 16), # legend title text = size 18
    legend.position = c(0.9,0.8)) + # where on the "grid" the legend is
  scale_y_continuous(expand=c(0,0)) # to stop the bars from floating above the x-axis
 
publication_style

basic

# Let's set the theme with all these changes. That way we can apply them to all the future plots.
theme_set(theme_gray(base_family = "mono") + 
            theme(
              axis.line = element_line(size=0.5),
              panel.background = element_rect(fill = NA),
              panel.grid.minor = element_line(color = NA),
              axis.text = element_text(size = 16),
              axis.title = element_text(size = 18),
              legend.text = element_text(size = 13), 
              legend.title = element_text(size = 16), 
              legend.position = c(0.9,0.8))
)

basic

# These tweaks aren't part of the theme, so you will still have to add them separately to each plot
# Prevent the bars from floating above the x-axis
basic + scale_y_continuous(expand = c(0, 0))
# Change position of the legend 
basic + theme(legend.position = "bottom") + scale_y_continuous(expand = c(0, 0))
# remove the legend entirely 
basic + theme() + scale_y_continuous(expand = c(0, 0)) + guides(fill = FALSE)

#Going back to the colors: We can always change them
display.brewer.all()
basic + theme(legend.position = "bottom") + scale_y_continuous(expand = c(0,0)) + scale_fill_brewer(palette = "Spectral")

# But wait, remember this guy? 
pie(rep(1, length(big_palette)),col = big_palette)
#We can sill use that "big_palet"
basic + theme() + scale_y_continuous(expand = c(0, 0)) + scale_fill_manual(values = sample(big_palette))
set.seed(5)
basic + theme() + scale_y_continuous(expand = c(0,0)) + scale_fill_manual(values = sample(big_palette))

# and you can always reset to defaults with:
theme_set(theme_gray())
basic

