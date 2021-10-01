# Histogram-Anova-v3.4
# Author: Niels Asmussen
# Date: August 2021
##########################################
####################################################################################
#
####################################################################################
# system check & management
#install.packages("sfsmisc")
#R.Version()
#
####################################################################################
# manage R
# install.packages("installr");
# updateR()
#
# load needed libraries
#library(tidyverse)
library(ggplot2)
library(reshape2)
library(plyr)
library(ggsignif)
library(multcompView)
library(gridExtra)
library(egg)
library(grid)
library(outliers)
library(sfsmisc)
library(magrittr)
#
#investigate:
#ggplot function ggsave
#library(ggthemes)
#
####################################################################################
# v3.2 status
# ADDED: 
# - ability to handle multiple Group2 ids for data partitioning and display,
# - can now run stats on each Group2 individually or on the entire dataset
# - can also chart data based on Group2 subgroupings
# - new variables added to config file: Colors; Stats Anova Group2; Facet Split
# - for groups / comparisons with no data the script should warn and gracefully deal with
#     eg all 0's should simply produce '--' for each stats letter that no stats can be run...
# - does a best guess at the Stat Letters Offset value based on the YMax value for export sizes of 1600x831
# - add small notes to the bottom automatically that CAN be cropped detailing the stats that were run
#     also any 1 or 2 tailed outlier removal, etc...
# v3.2.1 status
# ADDED:
# - check that there are enough values to perform outlier checks (> 2)
# v3.2.2 status
# ADDED:
# - ability to define the distance between the axis title and the tick values
# - ability to set the stats caption text size in the config file
# - updated init_vars to set default values
# v3.2.3 status
# ADDED:
# - option to control the figure's ratio enabling one to produce a 'square' or various rectangles...
# v3.2.4 status
# ADDED:
# - option to auto control the ratio (SQUARE is now an input) -> largely DEPRECATED with gsave()
# - ability to rotate the X-axis values AND consistent sizing between axis & statistical lettering
# - ability to manipulate the values (eg divide all by 1,000) and label or use scientific notation
# v3.3 status
# ADDED:
# - statistical test options (t-test options, student & wilcox paired) with ability to set the tails (two.sided, greater, less)
# - data transform: ability to generate treatment/ control figures & 'mute' select groups (eg for treatment/ control)
# - edited 2.4 to allow files with only one group - edits done in the stats call to ignore importing Group2 from the data
# - time course transformation added -> Group1 sets the time, Group2 is the group -> impacts spacing of bars
# - streamlined the config files - many options are now defaulted in the R script and can be changed in config if desired
# v3.4 status
# ADDED:
# - ability to generate a figure without running any statistical tests
# - run t-tests POST transformation on data again (may NOT be necessary... investigate)
# - Ability to have individual data points to histogram
# - Incorporate multiple HLine's (split up a comma string)
# - handle special characters in the config file for titles
# - option for 'global settings' function that allows one to over-ride numerous settings for batch figure generation
# - options to specify saved file sizes and dimensions
# To Do:
# - include option for other post hoc tests
# - special characters in the legends & title are buggy - sometimes unicode doesn't display correctly:
#       generate_figure has attempts at working on this, main issue is mixing strings & expressions...
# - accept a '-' in the x-axis labels
# - Better display / sizing of the figure for export & saving of output (eg R shiny)
# - auto guess the YMax value (using upper in the summary table) and the Stat Offset for variable letter sizes
#
####################################################################################
# FUNCTIONS TO LOAD FOR LATER USE...
#
####################################################################################
# INIT VARS
#
# reset the variables used to load and generate another figure
#
init_vars <- function() {

  if (!exists("Override")) { assign("Override", FALSE, envir = .GlobalEnv) }
  if (isFALSE(Override)) { 
    ################ Label Size and Appearance (OPT) ################
    assign("Fig.Title.Size", 32, envir = .GlobalEnv)
    assign("Fig.Axis.LabelSize", 26, envir = .GlobalEnv)
    assign("Fig.Axis.LabelSep", 20, envir = .GlobalEnv)
    assign("Fig.Axis.ValueSize", 26, envir = .GlobalEnv)
    assign("Fig.Convert", TRUE, envir = .GlobalEnv)
    assign("Fig.Font", "sans", envir = .GlobalEnv)
  
    ################ Display of the Axis & Plot (OPT) ################
    assign("Fig.X.Angle", 45, envir = .GlobalEnv)
    # .Ratio for when a number is entered in config
    assign("Fig.Coord.Fixed", TRUE, envir = .GlobalEnv)
    assign("Fig.Coord.Fixed.Ratio", "SQUARE", envir = .GlobalEnv)  
    
    ################ Colors and Display Individual Points (OPT) ################
    assign("Fig.Colors", "", envir = .GlobalEnv)
    assign("Fig.Scatter.Disp", TRUE, envir = .GlobalEnv)
  
    ################ Stats Labels (OPT) ################
    assign("Stats.Letters.Offset", FALSE, envir = .GlobalEnv)
    assign("Stats.Letters.Size", 18, envir = .GlobalEnv)
    assign("Stats.Caption.Display", TRUE, envir = .GlobalEnv)  
    assign("Stats.Caption.Size", 6, envir = .GlobalEnv)
    
    ################ Figure Save (OPT) ################
    assign("Fig.Save.Width", 8, envir = .GlobalEnv)
    assign("Fig.Save.Height", 8.5, envir = .GlobalEnv)
    assign("Fig.Save.DPI", 320, envir = .GlobalEnv)
    assign("Fig.Save.Units", "in", envir = .GlobalEnv)
    assign("Fig.Save.Type", "jpg", envir = .GlobalEnv)
  }

  ################ Title & Axis Labels (REQ) ################
  assign("Fig.Title", "", envir = .GlobalEnv)
  assign("Fig.Title.tmp", "", envir = .GlobalEnv)
  assign("Fig.X", "", envir = .GlobalEnv)
  assign("Fig.Y", "", envir = .GlobalEnv)

  ################ Height of Y-axis and Horizontal Line/s (REQ) ################
  assign("Fig.Y.Max", "", envir = .GlobalEnv)
  assign("Fig.Y.Interval", "", envir = .GlobalEnv)
  assign("HLine", FALSE, envir = .GlobalEnv)
  
  ################ Alter the Axis (REQ) ################
  assign("Fig.Y.Rig", FALSE, envir = .GlobalEnv)
  assign("Fig.Y.Rig.Newline", FALSE, envir = .GlobalEnv)
  # if there is additional info from Y manipulation store here, not editable in CONFIG
  assign("Fig.Y.Supp", "", envir = .GlobalEnv)
  
  ################ Stats Labels (OPT) ################
  # set by the script - RESET per run:
  assign("Notes.Stats.Method", "", envir = .GlobalEnv)
  assign("Notes.Stats.Outlier", "", envir = .GlobalEnv)

  ################ Stats Tests (REQ) ################
  assign("Stats.Test", c(), envir = .GlobalEnv)
  assign("Stats.STTest.Pairs", data.frame(), envir = .GlobalEnv)
  assign("Stats.PTTest.Pairs", data.frame(), envir = .GlobalEnv)

  ################ Stats Transformation or Outlier (REQ) ################
  assign("Stats.Transform", FALSE, envir = .GlobalEnv)
  assign("Stats.Transform.Treatment", "", envir = .GlobalEnv)
  assign("Stats.Outlier", "ONE", envir = .GlobalEnv)
  assign("group1Mute", FALSE, envir=.GlobalEnv)

  ################ Split on Group 2? (REQ) ################
  assign("Stats.Anova.Group2", FALSE, envir = .GlobalEnv)
  assign("Fig.Facet.Split", TRUE, envir = .GlobalEnv)
  
  # clear out all the variables generated in the run_stats_prep()
  assign("raw", "", envir = .GlobalEnv)
  assign("raw.multi", "", envir = .GlobalEnv)
  assign("raw.anova.multi", "", envir = .GlobalEnv)
  assign("raw.aov.multi", "", envir = .GlobalEnv)
  assign("raw.aov.tukey.multi", "", envir = .GlobalEnv)
  assign("raw.summary", "", envir = .GlobalEnv)
  assign("raw.summary.multi", "", envir = .GlobalEnv)
  
  # clear out ANOVA generated values
  assign("Tukey.levels", "", envir = .GlobalEnv)
  
  # clear out plot generated values
  assign("plot.labels", "", envir = .GlobalEnv)
}
#
####################################################################################
# LOAD FILE HEAD
# Functions to pull in and analyze the data
#
# pull in the data contained in the header
#
load_file_head = function() {
  
  message(sprintf("---- Load config (file: %s)", Location.file))

  getwd()
  setwd(Location.dir)
  getwd()
  
  # forced reset
  init_vars()
  
  # set the override placeholder to NULL
  Override.tmp = NULL
  Fig.Y.tmp = NULL
  
  # read in the comments and set any specified variables (title, legend, etc)
  CON = file(Location.file, open = "r")
  l = readLines(CON, 1)
  while(substring(l, 1, 1) == '#') {
    #message(sprintf("---- Load data (line: %s)", l))

    # pull out the information
    lA = strsplit(substring(l, 2), "\t");

    # if the line is a comment skip to the next iteration
    if (substring(l, 2, 2) == '#') { 
      l = readLines(CON, 1);
      next 
    } else {
      l = readLines(CON, 1);
    }
    
    # maintain backwards compatibility - simply move this to a new line for analysis...
    if (lA[[1]][1] == "Stats STTest Pairs") { lA[[1]] = c("Stats Test", "STTest", lA[[1]][-1]) }
    else if (lA[[1]][1] == "Stats PTTest Pairs") { lA[[1]] = c("Stats Test", "PTTest", lA[[1]][-1]) }
      
    ################ OVERRIDE? ################
    if (lA[[1]][1] == "Override") {
      if (lA[[1]][2] %in% c("TRUE", "True", "true", "1")) { 
        if (isTRUE(Override)) { message("turning override ON AND overwriting previous override!!") }
        else { message("turning override ON!!") }

        # set override to true from here on out
        Override.tmp = TRUE 
        # turn off override protection if it was set
        assign("Override", FALSE, envir = .GlobalEnv)
      }
      else if (lA[[1]][2] %in% c("FALSE", "False", "false", "0")) {
        message("turning override OFF!")
        assign("Override", FALSE, envir = .GlobalEnv)
      }
    }
    ## OPTIONAL / OVERRIDEABLE SETTINGS ##
    # Override will be set to false in init_vars() (called earlier) IF it doesn't exist
    if (isFALSE(Override)) {
      ################ Label Size and Appearance (OPT) ################
      if (lA[[1]][1] == "Title Size") { assign("Fig.Title.Size", lA[[1]][2], envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Axis Label Size") { assign("Fig.Axis.LabelSize", lA[[1]][2], envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Axis Label Sep") { assign("Fig.Axis.LabelSep", lA[[1]][2], envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Axis Value Size") { assign("Fig.Axis.ValueSize", lA[[1]][2], envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Text Convert") { 
        if (lA[[1]][2] %in% c("TRUE", "True", "true", "1")) { assign("Fig.Convert", TRUE, envir = .GlobalEnv)
        } else { assign("Fig.Convert", FALSE, envir = .GlobalEnv) }
      }
      else if (lA[[1]][1] == "Text Font") { 
        if (lA[[1]][2] %in% c("serif", "sans", "mono")) { assign("Fig.Font", lA[[1]][2], envir = .GlobalEnv) }
      }
      ################ Display of the Axis & Plot (OPT) ################
      else if (lA[[1]][1] == "X Value Angle") { assign("Fig.X.Angle", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Coord Fixed Ratio") { 
        if (lA[[1]][2] %in% c("FALSE", "False", "false")) {
          assign("Fig.Coord.Fixed", FALSE, envir = .GlobalEnv) 
          assign("Fig.Coord.Fixed.Ratio", "", envir = .GlobalEnv)
        } else if (lA[[1]][2] %in% c("SQUARE", "Square", "square")) {
          assign("Fig.Coord.Fixed", TRUE, envir = .GlobalEnv) 
          assign("Fig.Coord.Fixed.Ratio", "SQUARE", envir = .GlobalEnv)
        } else {
          assign("Fig.Coord.Fixed", TRUE, envir = .GlobalEnv) 
          if (grepl("/", lA[[1]][2], fixed=TRUE)) { assign("Fig.Coord.Fixed.Ratio", sapply(strsplit(lA[[1]][2], "/"), function(x) { x <- as.numeric(x); x[1] / x[2]}), envir = .GlobalEnv) }
          else { assign("Fig.Coord.Fixed.Ratio", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
        }
      }
      ################ Colors and Display Individual Points (OPT) ################
      # can handle "," and " " for splitting ([, ]+ looks for "," or " " multiple times for splitting)
      else if (lA[[1]][1] == "Colors") { assign("Fig.Colors", strsplit(lA[[1]][2], "[, ]+")[[1]], envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Scatter Display") { 
        if (lA[[1]][2] %in% c("FALSE", "False", "false", 0)) { assign("Fig.Scatter.Disp", FALSE, envir = .GlobalEnv) }
        else { assign("Fig.Scatter.Disp", TRUE, envir = .GlobalEnv) }
      }
      ################ Stats Labels (OPT) ################
      else if (lA[[1]][1] == "Stat Offset") { 
        if (lA[[1]][2] %in% c("FALSE", "False", "false")) { assign("Stats.Letters.Offset", FALSE, envir = .GlobalEnv) }
        else { assign("Stats.Letters.Offset", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
      }
      else if (lA[[1]][1] == "Stat Letter Size") { assign("Stats.Letters.Size", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Stat Caption Display") { 
        if (lA[[1]][2] %in% c("FALSE", "False", "false", 0)) { assign("Stats.Caption.Display", FALSE, envir = .GlobalEnv) }
        else { assign("Stats.Caption.Display", TRUE, envir = .GlobalEnv) }
      }
      else if (lA[[1]][1] == "Stat Caption Size") { assign("Stats.Caption.Size", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
      ################ Figure Save (OPT) ################
      else if (lA[[1]][1] == "Save Width") { assign("Fig.Save.Width", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Save Height") { assign("Fig.Save.Height", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Save DPI") { assign("Fig.Save.DPI", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
      else if (lA[[1]][1] == "Save Units") { 
        if (tolower(lA[[1]][2]) %in% c("in", "cm", "mm", "px")) { assign("Fig.Save.Units", tolower(lA[[1]][2]), envir = .GlobalEnv) }
        else { assign("Fig.Save.Units", "in", envir = .GlobalEnv) }
      }
      else if (lA[[1]][1] == "Save Type") { 
        if (tolower(lA[[1]][2]) %in% c("eps", "ps", "tex", "pdf", "jpg", "jpeg", "tiff", "png", "bmp", "svg", "wmf")) { assign("Fig.Save.Type", tolower(lA[[1]][2]), envir = .GlobalEnv) }
        else { assign("Fig.Save.Type", "jpg", envir = .GlobalEnv) }
      }
    }

    ################ Title & Axis Labels (REQ) ################
    if (lA[[1]][1] == "Title Main") { Fig.Title.tmp = lA[[1]][2] }
    else if (lA[[1]][1] == "X Leg") { Fig.X.tmp = lA[[1]][2] }
    else if (lA[[1]][1] == "Y Leg") { Fig.Y.tmp = lA[[1]][2] }
    ################ Height of Y-axis and Horizontal Line/s (REQ) ################
    else if (lA[[1]][1] == "Y Max") { assign("Fig.Y.Max", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
    else if (lA[[1]][1] == "Y Interval") { assign("Fig.Y.Interval", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
    # can handle comma delimited array of horizontal lines
    else if (lA[[1]][1] == "HLine") { 
      assign("HLine", unlist(lapply(strsplit(lA[[1]][2], ","), function(x) { x = as.numeric(x)})), envir = .GlobalEnv)
    }
    ################ Alter the Axis (REQ) ################
    else if (lA[[1]][1] == "Y Value Rig") { 
      if (lA[[1]][2] %in% c("FALSE", "False", "false", "0")) { assign("Fig.Y.Rig", FALSE, envir = .GlobalEnv)
      } else if (lA[[1]][2] %in% c("SCI", "Sci", "sci")) { assign("Fig.Y.Rig", "SCI", envir = .GlobalEnv) 
      } else { assign("Fig.Y.Rig", as.numeric(lA[[1]][2]), envir = .GlobalEnv) }
    }
    else if (lA[[1]][1] == "Y Value Rig Newline") {
      if (lA[[1]][2] %in% c("TRUE", "True", "true", "1")) { assign("Fig.Y.Rig.Newline", TRUE, envir = .GlobalEnv)
      } else { assign("Fig.Y.Rig.Newline", FALSE, envir = .GlobalEnv) }
    }
    ################ Stats Tests (REQ) ################
    else if (lA[[1]][1] == "Stats Test") {
      # set the default value to FALSE whenever a user assigns any specific test
      Stats.Test[1] = FALSE
      if (lA[[1]][2] %in% c("ANOVA", "anova", "Anova")) { Stats.Test = c(Stats.Test, "ANOVA") }
      else if (lA[[1]][2] %in% c("STTest", "sttest", "STtest")) { 
        if (!"STTest" %in% Stats.Test) { Stats.Test = c(Stats.Test, "STTest") }
        # for a TTtest the comparison groups need to be submitted
        # set default test
        STTest.tails = "two.sided"
        # check to see if a test is request AND if it is workable...
        if (length(lA[[1]]) > 5) { 
          if (lA[[1]][6] %in% c("two.sided", "greater", "less")) { STTest.tails = lA[[1]][6] }
          else { warning(sprintf("---- Argument in STTest (%s) NOT VALID, using default (%s) instead", lA[[1]][6], STTest.tails)) }
        }
        # retain backwards compatability when the config file had sep lines for test & parings...
        if (length(lA[[1]]) > 2) {
          assign("Stats.STTest.Pairs", rbind(Stats.STTest.Pairs, data.frame(g1 = lA[[1]][3], g2 = lA[[1]][4], l = lA[[1]][5], alt = STTest.tails, ftest = I(vector(mode="list", length=1)), sttest = I(vector(mode="list", length=1))) ), envir = .GlobalEnv)
        }
      }
      else if (lA[[1]][2] %in% c("PTTest", "pttest", "PTtest", "Pttest")) { 
        if (!"PTTest" %in% Stats.Test) { Stats.Test = c(Stats.Test, "PTTest") }
        # for a TTtest the comparison groups need to be submitted
        # set default test
        PTTest.tails = "two.sided"
        if (length(lA[[1]]) > 5) { 
          if (lA[[1]][6] %in% c("two.sided", "greater", "less")) { PTTest.tails = lA[[1]][6] }
          else { warning(sprintf("---- Argument in PTTest (%s) NOT VALID, using default (%s) instead", lA[[1]][6], PTTest.tails)) }
        }
        # retain backwards compatability when the config file had sep lines for test & parings...
        if (length(lA[[1]]) > 2) {
          assign("Stats.PTTest.Pairs", rbind(Stats.PTTest.Pairs, data.frame(g1 = lA[[1]][3], g2 = lA[[1]][4], l = lA[[1]][5], alt = PTTest.tails, pttest = I(vector(mode="list", length=1))) ), envir = .GlobalEnv)
        }
      }
      assign("Stats.Test", Stats.Test, envir = .GlobalEnv)
    }
    ################ Stats Transformation or Outlier (REQ) ################
    else if (lA[[1]][1] == "Stats Transform") { 
      if (lA[[1]][2] %in% c("TreatmentControl", "TREATMENTCONTROL")) {
        # a specified treatment is required, if it isn't there, leave it set at FALSE
        if (!is.na(lA[[1]][3])) {
          assign("Stats.Transform", "ToverC", envir = .GlobalEnv) 
          tc = c(lA[[1]][3])
          if (!is.na(lA[[1]][4])) { tc = c(lA[[1]][3], lA[[1]][4]) }
          assign("Stats.Transform.Treatment", tc, envir = .GlobalEnv)
        }
      } else if (lA[[1]][2] %in% c("TimeCourse", "TIMECOURSE")) {
          assign("Stats.Transform", "TimeCourse", envir = .GlobalEnv) 
      }
    }
    else if (lA[[1]][1] == "Stats Outlier") { 
      if (lA[[1]][2] %in% c("ONE", "One", "one", 1)) { assign("Stats.Outlier", "ONE", envir = .GlobalEnv) }
      else if (lA[[1]][2] %in% c("TWO", "Two", "two", 2)) { assign("Stats.Outlier", "TWO", envir = .GlobalEnv) }
      else { assign("Stats.Outlier", FALSE, envir = .GlobalEnv) }
    }
    ################ Split on Group 2? (REQ) ################
    else if (lA[[1]][1] == "Stats Anova Group2") { 
      if (lA[[1]][2] %in% c("TRUE", "True", "true", 1)) { assign("Stats.Anova.Group2", TRUE, envir = .GlobalEnv) }
      else { assign("Stats.Anova.Group2", FALSE, envir = .GlobalEnv) }
    }
    else if (lA[[1]][1] == "Facet Split") { 
      if (lA[[1]][2] %in% c("FALSE", "False", "false", 0)) { assign("Fig.Facet.Split", FALSE, envir = .GlobalEnv) }
      else { assign("Fig.Facet.Split", TRUE, envir = .GlobalEnv) }
    }
  }
  close(CON)
  
  if (isTRUE(Override)) { message("OVERRIDE ON - Optional config settings skipped") }

  if (!is.null(Override.tmp)) { assign("Override", Override.tmp, envir = .GlobalEnv) }
  
  if (exists("Fig.Title.Replace")) {
    Fig.Title.tmp = Fig.Title.Replace
    rm(Fig.Title.Replace, envir = .GlobalEnv)
  }
  if (exists("Fig.Y.Replace")) { 
    Fig.Y.tmp = Fig.Y.Replace 
    rm(Fig.Y.Replace, envir = .GlobalEnv)
  }
  if (exists("Fig.X.Replace")) { 
    Fig.X.tmp = Fig.X.Replace 
    rm(Fig.X.Replace, envir = .GlobalEnv)
  }
  if (Fig.Convert) {
    Fig.Title.tmp = convert_text(Fig.Title.tmp)
    Fig.Y.tmp = convert_text(Fig.Y.tmp)
    Fig.X.tmp = convert_text(Fig.X.tmp)
  }
  assign("Fig.Title", Fig.Title.tmp, envir = .GlobalEnv)
  assign("Fig.Y", Fig.Y.tmp, envir = .GlobalEnv)
  assign("Fig.X", Fig.X.tmp, envir = .GlobalEnv)
}
####################################################################################
# CONVERT TEXT
#
# do a simple swap of html based characters for special symbols
# needed in titles & labels
#
convert_text <- function (label) {
  
  # convert
  # set conversion table
  replaceDict = list(
    "&Alpha;" = "\U0391",
    # Beta is not always reliable...
    "&Beta;" = "\U0392",
    "&Gamma;" = "\U0393",
    "&Delta;" = "\U0394",
    "&Epsilon;" = "\U0395",
    "&Pi;" = "\U03A0",
    "&Sigma;" = "\U03A3",
    "&Tau;" = "\U03A4",
    "&Phi;" = "\U03A6",
    "&Omega;" = "\U03A9",
    "&alpha;" = "\U03B1",
    "&beta;" = "\U03B2",
    "&gamma;" = "\U03B3",
    "&delta;" = "\U03B4",
    "&epsilon;" = "\U03B5",
    "&pi;" = "\U03C0",
    "&sigma;" = "\U03C3",
    "&tau;" = "\U03C4",
    "&phi;" = "\U03C6",
    "&omega;" = "\U03C9",
    "&bull;" = "\U2022",
    "&isin;" = "\U2208",
    "&notin;" = "\U2209",
    "&radic;" = "\U221A",
    "&infin;" = "\U221E",
    "&asymp;" = "\U2248",
    "&micro;" = "\U00B5"
  )
  # run through the above list and replace code on left with unicode on right
  for (html in names(replaceDict)) { label = gsub(html, replaceDict[[html]], label) }
  # search for \n in input string that gets converted to \\n by R and revert it to \n
  label = gsub("\\\\n", "\n", label)

  return(label)
}
####################################################################################
# LOAD DATA
#
# load the data in from the file and perform any global manipulations
#
load_data <- function () {

  message(sprintf("---- Load data (file: %s)", Location.file))
  # read in the data
  raw = read.table(Location.file, sep="\t", header=TRUE, comment.char = '#', check.names = FALSE)
  
  #
  # address any value manipulations - this can apply a standard division to ALL data values (eg divide by 1,000)
  # prepare the Y axis label supplement that contains details on what was done to the data
  if (is.numeric(Fig.Y.Rig)) {
    warning(sprintf("MODIFYING VALUES: DIVIDING ALL BY %s (file: %s)", Fig.Y.Rig, Location.file))
    raw['Value'] = raw['Value']/Fig.Y.Rig
    assign("Fig.Y.Max", Fig.Y.Max/Fig.Y.Rig, envir = .GlobalEnv)
    assign("Fig.Y.Interval", Fig.Y.Interval/Fig.Y.Rig, envir = .GlobalEnv)
    # update the HLine values
    assign("HLine", sapply(HLine, function(x) x/Fig.Y.Rig), envir = .GlobalEnv)
    if (Fig.Y.Rig > 100) {
      Fig.Y.Supp = pretty10exp(Fig.Y.Rig, drop.1=TRUE)
    } else {
      Fig.Y.Supp = paste("x ", Fig.Y.Rig, sep="")
    }
    assign("Fig.Y.Supp", Fig.Y.Supp, envir = .GlobalEnv)
  }
  
  # set the levels to be in the same order as the file... 
  # this also controls the order in which they display in the final figure...
  raw$Group1 = factor(raw$Group1, levels = unique(raw$Group1))
  if (!is(raw$Group2, "NULL")) {
    raw$Group2 = factor(raw$Group2, levels = unique(raw$Group2))
  }
  
  # create a list  of group IDs for use in the analysis - either a combination of 1 & 2 or simply 1
  if (!is(raw$Group2, "NULL")) {
    raw$statGroups = factor(with(raw, paste(Group1, Group2, sep="_")))
    raw$statGroups = factor(raw$statGroups, levels = unique(raw$statGroups))
  } else {  
    raw$statGroups = factor(raw$Group1, levels = unique(raw$Group1))
  }
  assign("raw", raw, envir = .GlobalEnv)
  message(sprintf("%s final Group1_Group2 (statGroups - should be unique!) ids:\n\t%s", length(levels(raw$statGroups)), paste("", levels(raw$statGroups), collapse="")))
  
}
####################################################################################
# RUN STATS
#
# calculate the stats (outlier removal, anova, TUKEY post hoc, summary table)
# this function reads in the raw data and then calls the appropriate functions
# depending on what tests were specified in the config file...
#
run_stats_prep <- function () {

  message("---- Prep stats overview")
  # Create a subset of raw (raw.multi) that stores the data in raw broken down by Group2 for future analysis... 
  # no stats are being run on raw.multi, simply used for defining the different groups AND for defining letters for statistical significance.....
  # if TRUE run stats WITHIN each group2
  if (Stats.Anova.Group2) {
    n = 1
    raw.multi = vector(mode="list", length = length(levels(raw$Group2)))
    for (l in levels(raw[,'Group2'])) {
      raw.multi[[n]] = droplevels(raw[raw[,'Group2'] %in% c(l),])
      n = n + 1
    }
    assign("raw.multi", raw.multi, envir = .GlobalEnv)
  }

  # REQUIRED for error bars AND placing statistical letters!
  # following from: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  # IN BRIEF:
  # ddply is applying a function to each subset of values in the data frame 'raw' as split into groups by 'Group1'
  # and summarising the data for the groups
  # sum(!is.na(Value)) counting the values in each group that haven't been set to 'NA' eg by remove outlier function above...
  # mean(Value, na.rm=TRUE) calculates the mean of each group dropping any 'NA' values (reducing N for that mean)
  # sd(Value, na.rm=TRUE) runs the standard dev function also dropping any 'NA' values
  # sd/sqrt(N) determines the stadard error based on the above standard dev...
  # summary of all data is assigned to raw.summary, subsets (when needed) to raw.summary.multi
  #
  # keep this around even with multi since the entire dataset is sent to gplot for making the figure...
  if (is.null(raw$Group2)) {
    assign("raw.summary", ddply(raw, c('statGroups'), summarise, Group1=unique(Group1), N=sum(!is.na(Value)), mean=mean(Value, na.rm=TRUE),sd=sd(Value, na.rm=TRUE),se=sd/sqrt(N)), envir=.GlobalEnv)
  } else {
    assign("raw.summary", ddply(raw, c('statGroups'), summarise, Group1=unique(Group1), Group2=unique(Group2), N=sum(!is.na(Value)), mean=mean(Value, na.rm=TRUE),sd=sd(Value, na.rm=TRUE),se=sd/sqrt(N)), envir=.GlobalEnv)
  }
  
  #raw.summary.multi
  #
  # if TRUE only run stats WITHIN each group2
  if (Stats.Anova.Group2) {
    n = 1
    raw.summary.multi = vector(mode="list", length = length(levels(raw$Group2)))
    for (l in levels(raw[,'Group2'])) {
      raw.summary.multi[[n]] = ddply(raw[raw[,'Group2'] %in% c(l),], c('statGroups'), summarise, Group1=unique(Group1), Group2=unique(Group2), N=sum(!is.na(Value)), mean=mean(Value, na.rm=TRUE),sd=sd(Value, na.rm=TRUE),se=sd/sqrt(N),upper=mean+se)
      n = n + 1
    }    
    # if FALSE run stats on entire dataset regardless...
  } else {
    if(is.null(raw$Group2)) {
      raw.summary.multi = ddply(raw, c('statGroups'), summarise, Group1=unique(Group1), N=sum(!is.na(Value)), mean=mean(Value, na.rm=TRUE),sd=sd(Value, na.rm=TRUE),se=sd/sqrt(N),upper=mean+se)
    } else {
      raw.summary.multi = ddply(raw, c('statGroups'), summarise, Group1=unique(Group1), Group2=unique(Group2), N=sum(!is.na(Value)), mean=mean(Value, na.rm=TRUE),sd=sd(Value, na.rm=TRUE),se=sd/sqrt(N),upper=mean+se)
    }  
  }
  assign("raw.summary.multi", raw.summary.multi, envir = .GlobalEnv)
  # beginning of auto setting YMax - find the upper most values...
  #max(raw.summary.multi[[2]]['upper'])  

}
####################################################################################
# 
# remove any outliers if requested
#
run_outlier <- function() {

  message("---- Outlier checking")
  # determined by the header in the data file...
  # not impacted by the Group2 setting as each Group1_Group2 should create a unique ID combination
  if ((Stats.Outlier == "ONE") || (Stats.Outlier == "TWO")) {
    if (Stats.Outlier == "ONE") { Notes.Stats.Outlier = paste(Notes.Stats.Outlier, "Outlier Check (one tailed removal)", sep=" ") }
    else if (Stats.Outlier == "TWO") { Notes.Stats.Outlier = paste(Notes.Stats.Outlier, "Outlier Check (two tailed removal)", sep=" ") }
    outlier.list = ""
    outlier.skip = ""
    
    # cycle through the raw data frame based on the groupings in 'Group1'
    for (l in levels(raw[,'statGroups'])) {
      #message(sprintf("checking: %s", l))
      
      # first check to ensure that the list is not simply the same values
      # if all of the values are the same (eg 0s) then the outlier test will FAIL 
      # and end the script
      # ALSO check to ensure that there are AT LEAST 3 actual numerical values in order to run a outlier test
      if ((length(unique(raw[raw[,'statGroups'] %in% c(l),][,'Value'])) > 1) && (length(na.omit(raw[raw[,'statGroups'] %in% c(l),][,'Value'])) > 2)) {
        
        # there are multiple types of tests to run w/ grubbs https://cran.r-project.org/web/packages/outliers/outliers.pdf
        # type 10: looks for one outlier; type 11: checks highest & lowest value; type 20: checks for two outliers on SAME tail
        # make a list of the data in the Value column of the raw data frame 
        # for all entries that match c(l) (from above) in the 'Group1' column
        # [raw[,'Group1'] %in% c(l),][,'Value']
        #
        # one tailed check can throw a warning message: "In sqrt(s) : NaNs produced" though script 
        # continues running and so far no issue has been discovered; double check if concerned
        #message(sprintf("VALUES: %s", raw[raw[,'statGroups'] %in% c(l),][,'Value']))
        #message(sprintf("1 P: %s", grubbs.test(raw[raw[,'statGroups'] %in% c(l),][,'Value'], type=10)$p.value))
        #message(sprintf("2 P: %s", grubbs.test(raw[raw[,'statGroups'] %in% c(l),][,'Value'], type=11)$p.value))
        if (Stats.Outlier == "ONE") { 
          if (grubbs.test(raw[raw[,'statGroups'] %in% c(l),][,'Value'], type=10)$p.value <= 0.05) {
            warning(sprintf("ONE TAILED REMOVAL on group %s (file: %s)", l, Location.file))
            if (outlier.list == "") { outlier.list = l }
            else { outlier.list = paste(outlier.list, sprintf(", %s", l), sep="") }
            
            raw[raw[,'statGroups'] %in% c(l),][,'Value'] = append(rm.outlier(raw[raw[,'statGroups'] %in% c(l),][,'Value'], fill=FALSE), NA)
          }
          # check for outliers on both tails
        } else if (Stats.Outlier == "TWO") {
          if (grubbs.test(raw[raw[,'statGroups'] %in% c(l),][,'Value'], type=11)$p.value <= 0.05) {
            warning(sprintf("TWO TAILED REMOVAL on group %s (file: %s)", l, Location.file))
            if (outlier.list == "") { outlier.list = l }
            else { outlier.list = paste(outlier.list, sprintf(", %s", l), sep="") }
            
            # overwrites the existing group *** NOT NECESSARILY IN THE SAME ORDER! ***
            # runs rm.outlier function to remove outlier and appends 'NA' in its place
            
            raw[raw[,'statGroups'] %in% c(l),][,'Value'] = append(rm.outlier(raw[raw[,'statGroups'] %in% c(l),][,'Value'], fill=FALSE), NA)
          }
        }
        # write out a message if the test is skipped
      } else {
        warning(sprintf("NOT ENOUGH VALUES to perform outlier check on group %s (file: %s)", l, Location.file))
        if (outlier.skip == "") { outlier.skip = l }
        else { outlier.skip = paste(outlier.skip, sprintf(", %s", l), sep="") }
      }
    }
    # remove any 'NA' rows entered during outlier search as they confuse the ordering of
    # letters during figure creation, this will also remove any 'NA' rows in the initial file
    raw = na.omit(raw)
    if (outlier.list == "") { outlier.list = "None Found" }
    if (outlier.skip != "") { outlier.list = paste(outlier.list, "\nToo few values to check outliers: ", outlier.skip) }
    Notes.Stats.Outlier = paste(Notes.Stats.Outlier, outlier.list, sep=" ")
    assign("raw", raw, envir = .GlobalEnv)
    
  } else {
    Notes.Stats.Outlier = paste(Notes.Stats.Outlier, "No Outlier Check Performed", sep=" ")
  }
  assign("Notes.Stats.Outlier", Notes.Stats.Outlier, envir = .GlobalEnv)
  
}
####################################################################################
# 
# calculate the anova values
#
run_anova <- function() {
  
  message("---- ANOVA w/ Tukeys Post Hoc")
  ##########################################
  # anova on the data
  #
  # if TRUE only run stats WITHIN each group2
  if (Stats.Anova.Group2) {
    n = 1
    raw.anova.multi = vector(mode="list", length = length(levels(raw$Group2)))
    for (l in levels(raw[,'Group2'])) {
      raw.anova.multi[[n]] = lm(Value ~ statGroups, data = raw[raw[,'Group2'] %in% c(l),], na.action=na.exclude)
      n = n + 1
    }    
  # if FALSE run stats on entire dataset regardless...
  } else {
    raw.anova.multi = lm(Value ~ statGroups, data = raw, na.action=na.exclude)
  }
  assign("raw.anova.multi", raw.anova.multi, envir = .GlobalEnv)
  #summary(raw.anova.multi[[1]])
  #anova(raw.anova.multi[[1]])
  
  
  ##########################################
  # anova again using aov()
  #
  # if TRUE only run stats WITHIN each group2
  if (Stats.Anova.Group2) {
    n = 1
    raw.aov.multi = vector(mode="list", length = length(levels(raw$Group2)))
    for (l in levels(raw[,'Group2'])) {
      raw.aov.multi[[n]] = aov(Value ~ statGroups, data = raw[raw[,'Group2'] %in% c(l),])
      n = n + 1
    }    
  # if FALSE run stats on entire dataset regardless...
  } else {
    raw.aov.multi = aov(Value ~ statGroups, data = raw)
  }
  assign("raw.aov.multi", raw.aov.multi, envir = .GlobalEnv)
  # get some useful data out of the aov...
  #raw.aov.multi[[1]]$residuals
  #str(summary(raw.aov.multi[[1]]))
  #summary(raw.aov.multi[[1]])[[1]][[1,'Pr(>F)']]
  
  
  ##########################################
  # TUKEY post hoc stats...
  #
  # if TRUE only run stats WITHIN each group2
  if (Stats.Anova.Group2) {
    n = 1
    raw.aov.tukey.multi = vector(mode="list", length = length(levels(raw$Group2)))
    for (l in levels(raw[,'Group2'])) {
      raw.aov.tukey.multi[[n]] = TukeyHSD(raw.aov.multi[[n]])
      n = n + 1
    }    
  # if FALSE run stats on entire dataset regardless...
  } else {
    raw.aov.tukey.multi = TukeyHSD(raw.aov.multi)
  }
  assign("raw.aov.tukey.multi", raw.aov.tukey.multi, envir = .GlobalEnv)
  #raw.aov.tukey.multi[[1]]

}
####################################################################################
# RUN TTEST
#
run_ttest <- function(paired) {
  
  if (paired) {
    message("---- Paired Wilcox T-Test")
    for (i in rownames(Stats.PTTest.Pairs)) {
      i = as.numeric(i)
      Stats.PTTest.Pairs[i,]$pttest[[1]] = wilcox.test(
        raw[raw[,'statGroups'] %in% Stats.PTTest.Pairs[i,"g1"],]$Value,
        raw[raw[,'statGroups'] %in% Stats.PTTest.Pairs[i,"g2"],]$Value,
        alternative=as.character(Stats.PTTest.Pairs$alt)[i],
        paired=TRUE,
        conf.level=0.95)
      message(sprintf("ran paired wilcox t-test on group1 %s to group2 %s, with tail: %s, p-value: %s", Stats.PTTest.Pairs[i,"g1"], Stats.PTTest.Pairs[i,"g2"], Stats.PTTest.Pairs[i,"alt"], Stats.PTTest.Pairs$pttest[[i]]$p.value))
    }
    assign("Stats.PTTest.Pairs", Stats.PTTest.Pairs, envir = .GlobalEnv)
  } else {
    i = 1
    message("---- Student T-Test")
    for (i in rownames(Stats.STTest.Pairs)) {
      i = as.numeric(i)
      # check homogeneity of the variances of the two groups before running students ttest
      # a p > 0.05 inidicates that the variances ARE homogeous and can proceed, if < 0.05 throw a warning
      Stats.STTest.Pairs$ftest[[i]][1] = list(var.test(
        raw[raw[,'statGroups'] %in% Stats.STTest.Pairs[i,"g1"],]$Value,
        raw[raw[,'statGroups'] %in% Stats.STTest.Pairs[i,"g2"],]$Value ))
      if (Stats.STTest.Pairs$ftest[[i]][[1]]$p.value > 0.05) {
        Stats.STTest.Pairs$ftest[[i]][2] = "Homogenous"
        message(sprintf("variances between group1 %s & group2 %s ARE homogenous with a p-value: %s", Stats.STTest.Pairs[i,"g1"], Stats.STTest.Pairs[i,"g2"], Stats.STTest.Pairs$ftest[[i]][[1]]$p.value))
      } else {
        Stats.STTest.Pairs$ftest[[i]][2] = "NOT Homogenous - CHECK DATA"
        message(sprintf("** CHECK DATA - variances between group1 %s & group2 %s ARE NOT homogenous with a p-value: %s", Stats.STTest.Pairs[i,"g1"], Stats.STTest.Pairs[i,"g2"], Stats.STTest.Pairs$ftest[[i]][[1]]$p.value))
        warning(sprintf("** CHECK DATA - variances between group1 %s & group2 %s ARE NOT homogenous with a p-value: %s (file: %s)", Stats.STTest.Pairs[i,"g1"], Stats.STTest.Pairs[i,"g2"], Stats.STTest.Pairs$ftest[[i]][[1]]$p.value, Location.file))
      }
      
      Stats.STTest.Pairs[i,]$sttest[[1]] = t.test(
        raw[raw[,'statGroups'] %in% Stats.STTest.Pairs[i,"g1"],]$Value,
        raw[raw[,'statGroups'] %in% Stats.STTest.Pairs[i,"g2"],]$Value,
        alternative=as.character(Stats.STTest.Pairs$alt)[i],
        paired=FALSE,
        conf.level=0.95)
      message(sprintf("ran students t-test on group1 %s to group2 %s, with tail: %s, p-value: %s", Stats.STTest.Pairs[i,"g1"], Stats.STTest.Pairs[i,"g2"], Stats.STTest.Pairs[i,"alt"], Stats.STTest.Pairs$sttest[[i]]$p.value))
    }
    assign("Stats.STTest.Pairs", Stats.STTest.Pairs, envir = .GlobalEnv)    
  }
}
####################################################################################
# RUN TRANSFORM
#
run_transform <- function() {
  
  if (Stats.Transform == "ToverC") {
    
    message("---- Transform: Treatment OVER Control")
    warning(sprintf("Transforming data to generate treatment over control figure (file: %s)", Location.file))
    
    # we want to divide all groups by the transform control group AND drop that group from the figure as it should all be set to 1
    trans = ""
    if (is.na(Stats.Transform.Treatment[2])) { trans = Stats.Transform.Treatment[1]
    } else { trans = paste(Stats.Transform.Treatment[1], Stats.Transform.Treatment[2], sep="_") }
    
    for (l in levels(raw[,'statGroups'])) {
      # don't want to run treat over treat since then everything will be divided by 1...
      if (l != trans) {
        raw[raw[,'statGroups'] %in% c(l),][,'Value'] = (raw[raw[,'statGroups'] %in% c(l),][,'Value'] / raw[raw[,'statGroups'] %in% c(trans),][,'Value'])
      }
    }
    raw[raw[,'statGroups'] %in% c(trans),][,'Value'] = (raw[raw[,'statGroups'] %in% c(trans),][,'Value'] / raw[raw[,'statGroups'] %in% c(trans),][,'Value'])    
    assign("raw", raw, envir = .GlobalEnv)
  }
  
}
####################################################################################
# ACTUAL CALLS:
# data = generate_label_df(raw.multi[[n]], raw.aov.multi[[n]], raw.aov.tukey.multi[[n]], raw.summary.multi[[n]], Value ~ statGroups, 'statGroups', Stats.Letters.Offset)
# data = generate_label_df(raw, raw.aov.multi, raw.aov.tukey.multi, raw.summary.multi, Value ~ statGroups, 'statGroups', Stats.Letters.Offset)
# 
# generate_label_df <- function(
#   HSDdata     -> is a global variable already
#   HSDaov      -> is a global variable already
#   HSDtukey    -> is a global variable already
#   HSDsummary  -> is a global variable already
#   comparison  -> is set & defined in the run_anova function and would have to be changed their as well... pretty static
#   flev        -> is set & defined throughout the script, would require a near global redesign... 
#   yOff        -> is a global variable already
# )
# moving to:
# data = generate_label_df(n)
# 0 denotes the 'entire dataset' ie raw, raw.aov.multi
# > 0 denotes that section of the dataset ie: raw.multi[n], raw.aov.multi[[n]]
#
####################################################################################
# GENERATE LABEL DF
#
# generate the data frame containing the letters displayed in the figure
#
# warning - changing headers or variable names needs to be reflected in this function!
# AUTOMATED significant letter group asignment
# FROM: https://stackoverflow.com/questions/18771516/is-there-a-function-to-add-aov-post-hoc-testing-results-to-ggplot2-boxplot
# call used: generate_label_df(raw, raw.aov.tukey.multi, raw.summary.multi, Value ~ statGroups, 'statGroups', Stats.Letters.Offset)
# OR: generate_label_df(raw.multi[[n]], raw.aov.multi[[n]], raw.aov.tukey.multi[[n]], raw.summary.multi[[n]], Value ~ statGroups, 'statGroups', Stats.Letters.Offset)
# generate_label_df <- function(HSDdata, HSDaov, HSDtukey, HSDsummary, comparison, flev, yOff){
generate_label_df <- function(n){
  
  ##########################################
  # build the variables required in this function - some are redundant and could/should be cleaned up...
  comparison = Value ~ statGroups
  flev = 'statGroups'
  yOff = Stats.Letters.Offset
  alpha = 0.05
  
  if (n > 0) { 
    #n = 1
    HSDdata = raw.multi[[n]]
    HSDsummary = raw.summary.multi[[n]]
  } else { 
    HSDdata = raw 
    HSDsummary = raw.summary.multi
  }
  #
  ##########################################
  
  ##########################################
  # generate the 'basic' table with an empty labels column...
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  # $mean and $se are referring to an external column IN HSDsummary - a calculated summary OF HSD
  # HSDsummary is from:
  # ddply(traw, c("variable"), summarise, N=sum(!is.na(value)), mean=mean(value, na.rm=TRUE),sd=sd(value, na.rm=TRUE),se=sd/sqrt(N))
  # ORIG: boxplot.df <- ddply(traw, flev, function (x) max(fivenum(x$value)) + .02)
  boxplot.df = ddply(HSDsummary, flev, function (x) (x$mean) + (x$se) + yOff)
  
  # add the corresponding summary data 'back' in...
  labels.df = merge(boxplot.df, raw.summary, by.x = flev, by.y = flev, sort = FALSE)
  
  main.labels = data.frame(row.names = labels.df$statGroups, plot.labels = labels.df$statGroups, stringsAsFactors = FALSE)
  anova.labels = data.frame(row.names = labels.df$statGroups, plot.labels = labels.df$statGroups, stringsAsFactors = FALSE)
  anova.labels['labels'] = ""
  sttest.labels = data.frame(row.names = labels.df$statGroups, plot.labels = labels.df$statGroups, stringsAsFactors = FALSE)
  sttest.labels['labels'] = ""
  pttest.labels = data.frame(row.names = labels.df$statGroups, plot.labels = labels.df$statGroups, stringsAsFactors = FALSE)
  pttest.labels['labels'] = ""
  
  # build the letters that will be used from each test, they will be combined into one table later
  # what tests are being carried out and need statistics reporting?
  if ("ANOVA" %in% Stats.Test) { 
    ##########################################
    # ANOVA variables:
    if (n > 0) { 
      HSDaov = raw.aov.multi[[n]]
      HSDtukey = raw.aov.tukey.multi[[n]]
    } else {
      HSDaov = raw.aov.multi
      HSDtukey = raw.aov.tukey.multi
    }
    #
    ##########################################
    
    ##########################################
    # build the ANOVA letters
    
    # Extract labels and factor levels from Tukey post-hoc 
    # aka: raw.aov.tukey.multi[,"p adj"]
    Tukey.levels <- HSDtukey[[flev]][,4]
    
    # IF all results for Tukey.levels are identical (eg 0) then multcompLetters2 will fail as there is nothing to do
    # check to ensure that there is something to work with OTHERWISE throw a warning
    # and assign 'A' to everything
    if (length(unique(Tukey.levels[!is.na(1)])) == 1) {
      anova.labels = data.frame(labels = rep('--', 6), plot.labels = raw.summary.multi[[2]]$statGroups)
      warning(sprintf("UNABLE to determine significance based on the tukey results! (%s) \n\tAll groups assigned as having NO SIGNIFICANT DIFFERENCE! (file: %s)", paste("", Tukey.levels, collapse=""), Location.file ))
      
      # get the work done 'by hand' 
    } else {
      
      # letters but NO order to them...
      # Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
      # ordered by the mean, descending...
      # *** DEFINE STAT NOTE BELOW IF CHANGING APPLIED STATS ***
      Tukey.labels <- multcompLetters2(comparison, Tukey.levels, HSDdata, Letters=c(LETTERS), threshold=alpha)
      names(Tukey.labels[['Letters']])
      plot.labels <- names(Tukey.labels[['Letters']])
      
      # Create a data frame out of the factor levels and Tukey's homogenous group letters
      # this has the letter description and the statGroup name in the data frame...
      anova.labels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']], stringsAsFactors = FALSE)
      data.frame(plot.labels, labels = Tukey.labels[['Letters']], stringsAsFactors = FALSE)
    }
    # for figure notes - currently using one-way ANOVA w/ Tukey Post-Hoc Test with a threshold of alpha = 0.05
    assign("Notes.Stats.Method", paste(Notes.Stats.Method, sprintf("one-way ANOVA (F(%s,%s) = %s, p = %s) with Tukey's HSD post hoc test (alpha = %s)", summary(HSDaov)[[1]][[1,'Df']], summary(HSDaov)[[1]][[2,'Df']], signif(summary(HSDaov)[[1]][[1,'F value']], digits=4), signif(summary(HSDaov)[[1]][[1,'Pr(>F)']], digits=4), alpha), sep=" "), envir = .GlobalEnv)

  }
  if ("STTest" %in% Stats.Test) { 
    # t test letters / symbols / whatever should be contained in the sttest data frame
    # and simply need to be assembled into a simplified table
    # from Stats.STTest.Pairs data frame: g1 is group 1 being compared to by g2, group 2
    # l is the symbol that gets placed above g2 denoting significance when compared to g1
    sttest.notes = "Student T-Test:"
    for (i in rownames(Stats.STTest.Pairs)) {
      i = as.numeric(i)
      if ((Stats.STTest.Pairs$sttest[[i]]$p.value < alpha) && (Stats.STTest.Pairs$g2[[i]] %in% HSDsummary$statGroups)) {
        sttest.labels[as.character(Stats.STTest.Pairs[i,'g2']),'labels'] = as.character(Stats.STTest.Pairs[i,'l'])
        #sttest.notes = sprintf("Fisher F-test with p of %s (%s); Student T-Test:", round(Stats.STTest.Pairs$ftest[[i]][[1]]$p.value, digits=4), Stats.STTest.Pairs$ftest[[i]][[2]])
        sttest.notes = paste(sttest.notes, sprintf("\'%s\' significant to %s with p of %s (F-Test: %s: %s);", Stats.STTest.Pairs[i,'l'], Stats.STTest.Pairs[i, 'g1'], round(Stats.STTest.Pairs$sttest[[i]]$p.value, digits=4), round(Stats.STTest.Pairs$ftest[[i]][[1]]$p.value, digits=4), Stats.STTest.Pairs$ftest[[i]][[2]]), sep=" ")
      }
    }
    if (substr(sttest.notes, nchar(sttest.notes), nchar(sttest.notes)) == ":") { sttest.notes = paste(sttest.notes, "nothing significant found or nothing checked", sep = " ") }
    assign("Notes.Stats.Method", paste(Notes.Stats.Method, sttest.notes), envir = .GlobalEnv)
  }
  if ("PTTest" %in% Stats.Test) { 
    # t test letters / symbols / whatever should be contained in the pttest data frame
    # and simply need to be assembled into a simplified table
    # from Stats.PTTest.Pairs data frame: g1 is group 1 being compared to by g2, group 2
    # l is the symbol that gets placed above g2 denoting significance when compared to g1
    pttest.notes = ""
    for (i in rownames(Stats.PTTest.Pairs)) {
      i = as.numeric(i)
      if ((Stats.PTTest.Pairs$pttest[[i]]$p.value < alpha) && (Stats.PTTest.Pairs$g2[[i]] %in% HSDsummary$statGroups)) {
        pttest.labels[as.character(Stats.PTTest.Pairs[i,'g2']),'labels'] = as.character(Stats.PTTest.Pairs[i,'l'])
        if (pttest.notes == "") { pttest.notes = "Paired Wilcox:" }
        pttest.notes = paste(pttest.notes, sprintf("\'%s\' significant to %s with p of %s;", Stats.PTTest.Pairs[i,'l'], Stats.PTTest.Pairs[i, 'g1'], round(Stats.PTTest.Pairs$pttest[[i]]$p.value, digits=4)), sep=" ")
      }
    }
    if (pttest.notes == "") { pttest.notes = "Paired Wilcox: nothing significant found or nothing checked"}
    assign("Notes.Stats.Method", paste(Notes.Stats.Method, pttest.notes), envir = .GlobalEnv)
  }

  # merge the stats results into one data frame for use in displaying the results
  main.labels = merge(anova.labels, pttest.labels, by='plot.labels', all=TRUE)
  main.labels = mutate(main.labels, labels = ifelse(labels.x == '', labels.y, ifelse(labels.y == '', labels.x, paste(labels.x, labels.y, sep = ' '))))
  main.labels$labels.x = NULL
  main.labels$labels.y = NULL
  main.labels
  main.labels = merge(main.labels, sttest.labels, by='plot.labels', all=TRUE)
  main.labels = mutate(main.labels, labels = ifelse(labels.x == '', labels.y, ifelse(labels.y == '', labels.x, paste(labels.x, labels.y, sep = ' '))))
  main.labels$labels.x = NULL
  main.labels$labels.y = NULL

  labels.df = merge(labels.df, main.labels, by.x = flev, by.y = 'plot.labels', sort = FALSE)
  
  return(labels.df)
}
#
####################################################################################
# FIGURE TIME!!
####################################################################################
# BUILD HISTO
#
# CURRENT FUNCTIONING FIGURE!
#
# basic histogram using above created summary and adding error bars!
build_histo <- function(){

  # Determine if any calculations are to be done for the Stat Letter Offset
  if (! isTRUE(Stats.Letters.Offset)) {
    assign("Stats.Letters.Offset", Fig.Y.Max / 25, envir = .GlobalEnv)
  }
  
  if (exists("Fig.Colors") == FALSE) { 
    assign("Fig.Colors", c("#000000", "#606060", "#00c000", "#f71480", "#0000ff", "#ff0000", "#000000", "#606060", "#00c000", "#f71480", "#0000ff", "#ff0000"), envir = .GlobalEnv)
  }
  mynamestheme = theme(plot.title = element_text(face = "bold", size = (18)))

  ##########################################
  # Time Course Figure
  ##########################################
  if (Stats.Transform == "TimeCourse") {
    #gplot = ggplot(raw.summary, aes(y=mean, label = Group2, x = Group1, fill=Group2, ymin=mean-se, ymax=mean+se)) +
    gplot = ggplot(raw.summary, aes(y=mean, label = Group2, x = Group1, fill=Group2, width=.85)) +
      geom_bar(aes(fill=Group2),
              alpha= 1,
              color= "black",
              size= 1,
              position=position_dodge(), 
              stat='identity') +
      # turn off the legend...
      #guides(fill = 'none') +
      #guides(size = 'none') +
      #guides(color = 'none') +
    
      scale_fill_manual(values=Fig.Colors, 
              #element_text(color="black", size=16, face="bold"),
              name="")

  ##########################################
  # 'Standard' Figure  
  # ggplot(df, aes(x, y, <other aesthetics>))
  ##########################################
  } else {
    gplot = ggplot(raw.summary, aes(Group1, mean, label = Group1)) +
      geom_bar(aes(fill=statGroups),
              alpha= 1,
              color= "white",
              size= 0,
              width= 0.7,
              position=position_dodge(), 
              stat='identity') +
      # turn off the legend...
      guides(fill = 'none') +
      guides(size = 'none') +
      guides(color = 'none') +
    
      #scale_fill_manual(values=wes_palette(n=6, name="GrandBudapest")) +
      scale_fill_manual(values=Fig.Colors, 
              #element_text(color="black", size=16, face="bold"),
              name="Treatment Groups")
      #labels=c("a","b")) +
  }
  ##########################################
  # Common Settings
  ##########################################
  # set the main titles for the figure
  gplot = gplot + labs(
            title=Fig.Title, 
            y=Fig.Y, 
            #caption = paste(Notes.Stats.Method, Notes.Stats.Outlier, sep="\n"),
            x=Fig.X)
  gplot = gplot + theme(
            text=element_text(family=Fig.Font),
            plot.title = element_text(lineheight=0.8, size=Fig.Title.Size, face="bold", hjust=0.5, vjust=0, margin=margin(b=0, unit = "pt")),
            plot.caption = element_text(color="black", size=Stats.Caption.Size, face="italic"),
            axis.title = element_text(color="black", size=Fig.Axis.LabelSize, face="bold"),
            axis.title.y = element_text(margin = margin(t = 0, r = Fig.Axis.LabelSep, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = Fig.Axis.LabelSep, r = 0, b = 0, l = 0)),
            axis.line = element_line(color="black", size=1.4),
            axis.text = element_text(color="black", size=Fig.Axis.ValueSize, face="bold"),
            # CUSTOMIZE
            #plot.title = element_text(lineheight=0.8, size=Fig.Title.Size, face="bold", hjust=0.5, vjust=0, margin=margin(b=5, unit = "pt")),
            #axis.text.y = element_text(color="black", size=Fig.Axis.ValueSize, face="bold"),
            #axis.text.x = element_text(color="black", size=50, face="bold"),
            #axis.text.x = element_text(angle=0, hjust=0),
            #axis.ticks.y = element_line(color='black', size=2),
            #axis.ticks.length = unit(.15, "cm"),
            plot.background = element_rect(fill = 'white', colour = 'white'),
            panel.background = element_rect(fill = 'white', colour = 'white'),
            panel.grid.major = element_line(colour = "white", size = 0),
            panel.grid.minor.y = element_line(size=3),
            legend.title = element_text(colour="black", size=16, face="bold"),
            legend.text = element_text(colour="black", size = 16, face = "bold"),
            strip.text.x = element_text(size = Fig.Axis.LabelSize, colour = "black", face = "bold"),
            strip.background = element_rect(colour="white", fill="white", size=1.5, linetype="solid")
          )
  # add text into the figure.... 
  #annotate("text", label = "Mean = 5", x = 4.5, y = -1, color = "red") +

  #########################################################
  # Y axis - add labels to any horizontal lines being added to the figure
  # the labels are going on the secondary y-axis
  if (HLine[1] != FALSE) {
    #gplot = gplot + scale_y_continuous(sec.axis = sec_axis(~ . * 1 , breaks = c(29,30), labels = c(29,30)))
    #gplot = gplot + scale_y_continuous(sec.axis = sec_axis(~ . * 1 , breaks = HLine, labels = HLine))
    gplot = gplot + scale_y_continuous(
      expand = c(0, 0),
      limits=c(0,Fig.Y.Max), 
      breaks = seq(0, Fig.Y.Max, by = Fig.Y.Interval),
      sec.axis = sec_axis(~ . * 1 , breaks = HLine, labels = HLine)
    )
  } else {
    gplot = gplot + scale_y_continuous(
      expand = c(0, 0),
      limits=c(0,Fig.Y.Max), 
      breaks = seq(0, Fig.Y.Max, by = Fig.Y.Interval)
    )
  }
  #########################################################
  # add scatter plot into the figure
  # 
  if (Fig.Scatter.Disp) {
    if (Stats.Transform == "TimeCourse") {
      gplot = gplot + geom_point(data=raw,
              aes(x=Group1, y=Value, group=statGroups), 
              #inherit.aes = FALSE,
              position = position_dodge(width=0.9),
              stat='identity',
              color="#FFD700", # gold
              shape=4, 
              size=1.8, 
              stroke=2
      ) + 
      # makes the scatter points placed in the legend match the group color
      guides(fill = guide_legend(override.aes = list(shape = NA)))
    } else {
      gplot = gplot + geom_point(data=raw,
              aes(x=as.numeric(Group1) + 0.0, y=Value), 
              #position=position_nudge(x=.3),
              #pch=21,
              position=position_dodge2(width=.5,padding=0.1),
              #position=position_dodge(width=3, preserve=c("single")),
              #color="#EEDB1B", # yellow
              #color="#EC910F", # orange
              color="#FFD700", # gold
              #color = "#000000",
              shape=4, 
              size=1.8, 
              stroke=2
      )
    }
  }
  
  #########################################################
  # add error bars to the figure 
  #
  # keep this after the scatter plot to ensure the bars
  # aren't covered by the scatter...
  if (Stats.Transform == "TimeCourse") {
    gplot = gplot + geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
              colour="black", 
              width=0.3, 
              size = 1,
              position = position_dodge(width=0.9))
  } else {
    gplot = gplot + geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
              width=.3,
              size=1,
              position=position_dodge(.9))
  }
  ##########################################
  # Modify the Figure based on file settings...
  #
  # swap Y axis labels to scientific notation...
  if (Fig.Y.Rig == "SCI") {
    message(sprintf("setting y-axis to use scientific notation, replacing existing scale_y_continuous..."))
    gplot = gplot + scale_y_continuous(labels = function(x) format(x, scientific = TRUE), expand = c(0, 0), limits=c(0,Fig.Y.Max), breaks = seq(0, Fig.Y.Max, by = Fig.Y.Interval))
  }
  #
  # modify x axis labels to put them at an angle
  if (Fig.X.Angle != 0) {
    gplot = gplot + theme(axis.text.x = element_text(angle=Fig.X.Angle, hjust=1))
  }
  #
  # break the figure into two separate ones for each secondary grouping
  if ((Fig.Facet.Split) && (Stats.Transform != "TimeCourse")) {
    gplot = gplot + facet_grid(~ Group2, scales="fixed", space="fixed")
  }
  #
  # determine if the fig shape should be modified:
  if (Fig.Coord.Fixed) {
    # determine the ratio for any auto requests
    if (Fig.Coord.Fixed.Ratio == "SQUARE") {
      assign("Fig.Coord.Fixed.Ratio", 1/(Fig.Y.Max / length(raw.summary[['statGroups']])), envir = .GlobalEnv)
    }
    gplot = gplot + coord_fixed(ratio = Fig.Coord.Fixed.Ratio)
  }
  #
  # determine where and how the stat letters are applied to each figure
  #
  # if TRUE only run stats WITHIN each group2
  n = 0
  if (Stats.Anova.Group2) {
    for (l in levels(raw[,'Group2'])) {
      n = n + 1
      s = "\n"
      if (Notes.Stats.Method == "") { s = "" }
      assign("Notes.Stats.Method", paste(Notes.Stats.Method, sprintf("For group %s: ", l), sep=s), envir = .GlobalEnv)
      #gplot = gplot + geom_text(data = generate_label_df(raw.multi[[n]], raw.aov.multi[[n]], raw.aov.tukey.multi[[n]], raw.summary.multi[[n]], Value ~ statGroups, 'statGroups', Stats.Letters.Offset), size = (Stats.Letters.Size / 2.834645669), fontface="bold", aes(x = Group1, y = V1, label = labels))
      gplot = gplot + geom_text(data = generate_label_df(n), size = (Stats.Letters.Size / 2.834645669), fontface="bold", aes(y = V1, label = labels), position = position_dodge(0.9))
    }
    # add border lines between / around the facet grids in order to make it clear that the stats are separate...
    gplot = gplot + theme(panel.spacing=unit(.05, "lines"),
                          panel.border = element_rect(color = "black", fill = NA, size = 1))
  # if FALSE run stats on entire dataset regardless...    
  } else {
    s = "\n"
    if (Notes.Stats.Method == "") { s = "" }
    assign("Notes.Stats.Method", paste(Notes.Stats.Method, "Statistical test: ", sep=s), envir = .GlobalEnv)
    #gplot = gplot + geom_text(data = generate_label_df(raw, raw.aov.multi, raw.aov.tukey.multi, raw.summary.multi, Value ~ statGroups, 'statGroups', Stats.Letters.Offset), size = (Stats.Letters.Size / 2.834645669), fontface="bold", aes(x = Group1, y = V1, label = labels))
    gplot = gplot + geom_text(data = generate_label_df(n), size = (Stats.Letters.Size / 2.834645669), fontface="bold", aes(y = V1, label = labels), position = position_dodge(0.9))
  }

  if (isTRUE(Stats.Caption.Display)) {
    if (Notes.Stats.Method == "Statistical test: ") { assign("Notes.Stats.Method", paste(Notes.Stats.Method, "----"), envir = .GlobalEnv) }
    gplot = gplot + labs(caption = paste(Notes.Stats.Method, Notes.Stats.Outlier, sep="\n"))  
  }
  
  # is it possible to create figures with the exact same internal size?
  #grid.arrange(grobs=lapply(list(gplot), set_panel_size, height=unit(18, "cm"), width=unit(18, "cm")))

  # make the plot accessible...
  assign("gplot", gplot, envir = .GlobalEnv)  
  # display the plot...
  gplot

  #n = 2
  #generate_label_df(raw.multi[[n]], raw.aov.tukey.multi[[n]], raw.summary.multi[[n]], Value ~ statGroups, 'statGroups', Stats.Letters.Offset)
}
#
# FINISHED WITH THE FIGURE!!!
####################################################################################
#
####################################################################################
# putting it all together...
####################################################################################
# GENERATE FIGURE
#
generate_figure <- function() {

  message("----------------  ----------------  ----------------")
  message("-------- Prep & Load config settings and data --------")

  # prep & load config info / data
  init_vars()
  load_file_head()
  load_data()

  message("-------- Statistical Analysis --------")
  
  # move onto stats analysis
  if (Stats.Outlier != FALSE) { run_outlier() }
  run_stats_prep()
  
  # run actual tests
  if ("ANOVA" %in% Stats.Test) { run_anova() }
  if ("STTest" %in% Stats.Test) { run_ttest(FALSE) }
  if ("PTTest" %in% Stats.Test) { run_ttest(TRUE) }

  # if a transformation is being conducted (eg treatment over control)
  # ** After a group is removed the ANOVA stats (if requested) are run again for ToverC **
  if (Stats.Transform == "ToverC") {
    run_transform()
    # run the stats prep again to set the summary tables to the new values
    run_stats_prep()

    # remove the treatment group (it will be indicated by a line at 1)
    group1Mute = Stats.Transform.Treatment[1]
    HLine = 1
  }   

  # if the Y-values were adjusted (eg all divided by 1,000 - in run_stats_prep()) this will append the modification
  # to the end of your y-axis label - you can select one or two lines...
  if (is.numeric(Fig.Y.Rig)) {
    if (Fig.Y.Rig.Newline) {
      # Two Lines
      Fig.Y = bquote(bold(atop(.(Fig.Y), "(" * .(Fig.Y.Supp[[1]]) * ")")))
    } else {
      # Single Line
      Fig.Y = bquote(bold(.(Fig.Y)~"("*.(Fig.Y.Supp[[1]])*")"))
    }
    # general math expression:
    #Fig.Y = bquote(.(Fig.Y)~"He"~r[xy]==~B^2)

    # following will successfully store the expression...
    #r = do.call(substitute, as.list(str2expression("r[xy]")))
    # error prone BUT it will parse a expression stored in a string...
    #Fig.Y = bquote(.(parse(text=Fig.Y)))
    # FULL list of current html4
    #"&Alpha;~&Beta;~&Gamma;~&Delta;~&Epsilon;~&Pi;~&Sigma;~&Tau;~&Phi;~&Omega;~&alpha;~&beta;~&gamma;~&delta;~&epsilon;~&pi;~&sigma;~&tau;~&phi;~&omega;~&bull;~&isin;~&notin;~&radic;~&infin;~&asymp;~&micro;"
    
    assign("Fig.Y", Fig.Y, envir = .GlobalEnv)
  }
  
  # this may not be 100% necessary if the
  # the t-tests should be run on the remaining groups POST transformation
  # but BEFORE removal of the control group
  if (Stats.Transform == "ToverC") {
    if ("STTest" %in% Stats.Test) { run_ttest(FALSE) }
    if ("PTTest" %in% Stats.Test) { run_ttest(TRUE) }
  }

  # remove a group from being displayed (eg for treatment / control figures)
  if ((group1Mute != FALSE) && (Stats.Anova.Group2 == FALSE)) {
    
    warning(sprintf("group1Mute is set to %s, attempting to remove this group! (file: %s)", group1Mute, Location.file))
    
    raw = subset(raw, Group1!=group1Mute)
    raw[] = lapply(raw, function(x) if(is.factor(x)) factor(x) else x)
    assign("raw", raw, envir = .GlobalEnv)
    
    raw.summary = subset(raw.summary, Group1!=group1Mute)
    raw.summary[] = lapply(raw.summary, function(x) if(is.factor(x)) factor(x) else x)
    assign("raw.summary", raw.summary, envir = .GlobalEnv)
    
    raw.summary.multi = subset(raw.summary.multi, Group1!=group1Mute)
    raw.summary.multi[] = lapply(raw.summary.multi, function(x) if(is.factor(x)) factor(x) else x)
    assign("raw.summary.multi", raw.summary.multi, envir = .GlobalEnv)
  }
  
  # the ANOVA test should be run on the remaining groups POST transformation
  if (Stats.Transform == "ToverC") {
    if ("ANOVA" %in% Stats.Test) { run_anova() }
    # following vars are set in run_anova() (raw.anova.multi, raw.aov.multi, raw.aov.tukey.multi)
  }
  
  message("-------- Build Histogram --------")
  build_histo()

  # add a line to the figure...
  if (HLine[1] != FALSE) {
    for (HL in HLine) {
      message(sprintf("adding a horizontal line to the figure at: \'%s\'", HL))
      gplot = gplot + geom_hline(yintercept=HL, linetype="solid", color="black", size=1)        
    }
  }
  gplot
  
  print(gplot)

  # save the image to the working directory using the modified txt filename - this WILL
  # overwrite an existing image...
  Location.image = sub("txt", "jpg", Location.file)
  message(sprintf("saving your new figure to: \'%s\'", Location.image))
  message("-------- SAVE Histogram --------")
  ggsave(Location.image, width = Fig.Save.Width, height = Fig.Save.Height, dpi = Fig.Save.DPI, units = Fig.Save.Units, device = Fig.Save.Type)
  message("----------------  ----------------  ----------------")
}
#
#
####################################################################################
# FINISHED LOADING NEEDED FUNCTIONS... 
####################################################################################
#
#
####################################################################################
# Automate figures...
####################################################################################
# GENERATE FIGURE X
#
generate_figure_X <- function() {
  
  Location.file = "Figure X/abcde.txt"
  Fig.Title.Replace = expression(bold(atop("Break Down", "Long Title")))
  Fig.Y.Replace = expression(bold(paste("Treatment/ Control (", mu, "g/ mL)")))
  group1Mute = "NT"
  HLine = 1
  assign("Location.file", Location.file, envir = .GlobalEnv)
  assign("Fig.Title.Replace", Fig.Title.Replace, envir = .GlobalEnv)
  assign("Fig.Y.Replace", Fig.Y.Replace, envir = .GlobalEnv)
  assign("group1Mute", group1Mute, envir = .GlobalEnv)
  assign("HLine", HLine, envir = .GlobalEnv)
  generate_figure()

}
#
#
####################################################################################
# define some labels and details for the current figure
##########################################
# File Directory (convert \ to /; does not end with /):
Location.dir = "W:/Boyan Group/LMRI-Grad, Techs, PIs/Niels Asmussen/Statistical Analysis"

# File name
Location.file = "test.txt"
Location.file = "test-1_groups.txt"
Location.file = "test-2_groups.txt"
Location.file = "test-ToverC.txt"
Location.file = "test-TimeCourse.txt"

# generate & save the figure!
generate_figure()

# batch figure generation...
generate_figure_X()

gplot

####################################################################################
# take quick look:

raw # raw data
# stats summary:
raw.summary
levels(raw.summary[,'Group1'])
summary(raw.anova.multi)
raw.aov.tukey.multi

##########################################
####################################################################################