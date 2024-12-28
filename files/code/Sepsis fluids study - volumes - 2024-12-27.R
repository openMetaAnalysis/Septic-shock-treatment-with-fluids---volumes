# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-12-21

file.filter   <- matrix(c("Cancel this","ZZZZZ.txt"),byrow=TRUE,ncol=2)
choose.files(filters= file.filter, caption="Cancel. This is a work around to avoid trouble with new version of R Studio")

library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

#== Startup ======
#* Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
getwd()

#* Troubleshooting -----
#options(error = NULL)   # Default
#options(warn = 2)       # Converts warnings into errors
options(warn = 2, error = browser)
options(error = recover) # This will provide a menu showing the call stack, and you can choose which environment to inspect.
typeof('bob')
class('bob') # Better
capabilities("tcltk")
# database contents:
#sapply(regdat, class)
# browser()
# Key Commands in browser()
# n: Next
#Executes the next line of code and stays in the debugging mode. This is similar to "step over" in other debuggers.
# s: Step into
# If the next line is a function call, s steps into that function, allowing you to debug inside the function. If it's not a function, it behaves like n.
# c: Continue
# Continues execution until the next breakpoint or until the script completes.
# Q: Quit
# Exits the browser and stops the debugging session.

# Global variables -----
Pb <- NULL  # For function_progress
Pallette_RoyGBiv <- c('red','orange','yellow', 'green', 'blue', '#4B0082', 'violet')
Palette_KU <- c("KUBlue" = "#0022B4", "KUSkyBlue" = "#6DC6E7", "KUCrimson" = "#e8000d", "KUYellow" = "#ffc82d", "CarolinaBlue" = "#56A0D3")
# Access: Palette_KU["KU Blue"]

##* Footnotes ===========
# https://www.unicodepedia.com/groups/general-punctuation/
# Dagger  \u2020
# Double dagger  \u2021
# Section symbol \u00A7
# Double Vertical Line \u2016 "\u2016\u2016"
# Para    \B6 or \u0086 or \u204a or \u204b

## Functions -----
`%notin%` <- Negate(`%in%`)
`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
`%==na%` <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2)))

function_progress <- function(progress, titletext = "testing") {
  # Check if Tcl/Tk is available
  if (!capabilities("tcltk")) {
    message("Tcl/Tk not available; cannot display a progress bar.")
    return(invisible(NULL))
  }
  
  # If Pb does not exist or got removed, create a new progress bar at 0
  if (!exists("Pb", envir = .GlobalEnv)) {
    Pb <<- tkProgressBar(title = titletext, label = "", min = 0, max = 100, initial = 0)
  }
  
  info <- sprintf("%d%% done", round(progress))
  
  # Attempt to set the progress bar; if it fails, try recreating it
  tryCatch({
    setTkProgressBar(Pb, value = progress, 
                     title = paste(titletext, sprintf("(%s)", info)), 
                     label = info)
  }, error = function(e) {
    # If there's an error, try to close and remove Pb, then recreate
    if (exists("Pb", envir = .GlobalEnv)) {
      try(close(Pb), silent = TRUE)
      rm(Pb, envir = .GlobalEnv)
    }
    # Recreate the progress bar and update
    Pb <<- tkProgressBar(title = titletext, label = "", min = 0, max = 100, initial = 0)
    setTkProgressBar(Pb, value = progress, 
                     title = paste(titletext, sprintf("(%s)", info)), 
                     label = info)
  })
  
  # If progress reached 100%, close and remove the progress bar
  if (progress == 100) {
    close(Pb)
    rm(Pb, envir = .GlobalEnv)
  }
}


function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())), 
                   repos = "https://cloud.r-project.org/",
                   #type = "binary"
  )
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
  #tk_messageBox(type = "ok", paste(packages, collapse="\n"), title="Packages installed")
}

current.date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}  

#* Functions to show data -----
function_plot_print <- function (plotname, plotwidth, plotheight){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  (current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.tif',sep=''),
    format = "tiff", width = plotwidth, height = plotheight)
}

# Packages/libraries -----
function_progress(0,'Libraries')
#* Essential -----
packages_essential <- c("tcltk", # Helps troublshooting
                        'rstudioapi', # function_plot_print
                        'stringr', 'openxlsx','readr')
function_libraries_install(packages_essential)

#* Meta-analysis and positive deviance----
packages_meta <- c("metafor", #
                   'meta',   # Meta-analysis
                   'boot',   # inv.logit to identify deviants
                   'grid',   # Forest and blobbogram
                   'gemtc',  # Blobbogram
                   'esc'     # David Wilson's Campbell collection individual study TEs
)
function_libraries_install(packages_meta)
function_progress(50, "Working")

# Intentionally running next line when Pb not displayed in order to troubleshoot stability
function_progress(100, "Done")

# ____________________________________ ----
# CLEAN ENVIRONMENT -----------------------
# Remove all but functions from the environment
#rm(list = ls()[sapply(ls(), function(x) {!is.function(get(x))})])
# OR PRESERVE GLOBAL VALUES:
rm(list = ls()[sapply(ls(), function(x) {
  var <- get(x)
  !is.function(var) && !(is.character(var) || is.numeric(var) || is.factor(var))
})])

### Data grab ===================================
file.filter   <- matrix(c("Text","*.txt","Spreadsheets","*.csv;*.xls;*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 2,multi=FALSE)
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
data.import <- NULL
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  wb.temp <- loadWorkbook(filename)
  data.import <- read.xlsx (wb.temp, sheet = 1, startRow = 1, colNames = TRUE, na.strings = "NA", detectDates = TRUE)
}
head(data.import)

data_sepsis <- data.import

## Meta-analysis ------
meta1 <- metabin(experimental.events, experimental.total, control.events, experimental.total, 
                 subgroup = Design,
                 data = data_sepsis, sm = "RR", hakn = TRUE, tau.common = FALSE, method="Inverse", level = 0.95, incr = "TA",  digits=2,digits.se=2, allstudies = TRUE, studlab=paste(Study,", ", Year, sep=""))
(meta1.summary <- summary(meta1))
(summary(inv.logit(meta1$TE)))
meta1$data$control.rate <- paste(sprintf(100*meta1$data$.event.c/meta1$data$.n.c, fmt='%#.1f'),'%', sep="") 

##* Forest plot ------
dev.off()
par(mar=c(5.1, 4.1, 4.1, 1)) # (bottom, left, top, right)

analyticmethod = "Random effects model (Hartung-Knapp)"
meta::forest (meta1, sortvalue = data_sepsis$Year, 
              #xlim=c(0,100), 
              leftcols = c("studlab", "Liberal.fluids","Restrictive.fluids", "control.rate"),
              leftlabs = c("Study"," Fluids\nLiberal","(ml/kg) \nRestrictive", "Mortality\n(Restrictive group)"),
              print.subgroup.name = FALSE,
              col.diamond="blue", col.diamond.lines="blue", title = "Sepsis", 
              fixed = FALSE, common = FALSE, random = TRUE, method.random.ci = "HK",
              #resid.hetstat = TRUE, 
              addrows = 2,
              print.I2.ci=TRUE, print.tau2=FALSE, print.p=FALSE, 
              label.left="Favors liberal", label.right="Favors restrictive",
              text.random=analyticmethod, fs.random=12, ff.random = 1, ff.hetstat=2, fs.hetstat=12)
grid.text("Forest plot – More liberal versus more restrictive fluids for sepsis with fluid refractory hypotension", 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))

# Copyright statement -----
if (1==2){
grid.text('Notes:', 
          0.02, 0.125, hjust=0, gp=gpar(cex=1, font=2))
#grid.text(paste0("Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)"),
#          0.95,  0.1,  hjust=1, gp=gpar(cex=1, font=1))
#grid.text(paste0("Version: ", current.date(), ". Code and data available at https://openmetaanalysis.github.io/sepsis"),
#          0.95,  0.05, hjust=1, gp=gpar(cex=1, font=1))
grid.text(paste0("Version: ", current.date(), ". Code and data available at https://openmetaanalysis.github.io/sepsis"),
        0.95,  0.1, hjust=1, gp=gpar(cex=1, font=1))
}

#* Print -----
plotname <- paste0("forest_plot_sepsis_-_")
function_plot_print (plotname, 1000,500)

# ____________________________________ ----
# Arrows plot ------
#Tips for Fine-Tuning
#angle: Default is 30. Lower values like 15 or 20 create a slimmer arrowhead.
#length: If the arrowhead is too long or short, raise or lower the fraction in unit(..., "inches").
#lineend & linejoin: Setting them to "round" in geom_segment() ensures smoother corners along the lines and arrow transitions.

# If you haven't installed ggtext yet, run:
# install.packages("ggtext")

library(ggplot2)
library(ggtext)

#* Data grab -----
data_arrows <- data.import

#* Data process -----
data_arrows$duration <- 1

data_arrows$Study.name <- data_arrows$Study
# data_arrows$Study.name <-  <- paste(data_arrows$Study,", ", data_arrows$Year, sep="")
data_arrows$control_biomarker_post <- data_arrows$Restrictive.fluids
data_arrows$exp_biomarker_post     <- data_arrows$Liberal.fluids

data_arrows$Event_rate_exp <- 100 * data_arrows$experimental.events / data_arrows$experimental.total
data_arrows$Event_rate_con <- 100 * data_arrows$control.events / data_arrows$control.total
data_arrows$surrogate_diff_p_rx <- data_arrows$control_biomarker_post - data_arrows$exp_biomarker_post
data_arrows$OR <- (data_arrows$experimental.events * (data_arrows$control.total - data_arrows$control.events)) / 
  ((data_arrows$experimental.total - data_arrows$experimental.events) * data_arrows$control.events)

data_arrows$x2  <- data_arrows$exp_biomarker_post; 
data_arrows$y2  <- data_arrows$Event_rate_exp/data_arrows$duration; 
data_arrows$x  <- data_arrows$control_biomarker_post; 
data_arrows$y  <- data_arrows$Event_rate_con/data_arrows$duration
#data_arrows=cbind(data_arrows,OR)
attach(data_arrows)

#* Plot ----------

# -- Calculate Odds Ratio and define arrow colors ------------------------------------
data_arrows$OR <- (data_arrows$experimental.events * (data_arrows$control.total - data_arrows$control.events)) / 
  ((data_arrows$experimental.total - data_arrows$experimental.events) * data_arrows$control.events)

data_arrows$arrow_color <- "black"
#data_arrows$arrow_color <- ifelse(data_arrows$OR > 1, "red", "darkgreen")

# -- Compute "Size" and derive line thickness based on it ---------------------------
data_arrows$Size <- data_arrows$control.total + data_arrows$experimental.total
data_arrows$linewidth <- (data_arrows$Size^1.5) / max(data_arrows$Size^1.5)

# -- Define your HTML-based footnotes as a single string with line breaks & styling --
footnotes <- paste0(
  "**Notes:**<br/>",
  "1. For each study, the arrow starts with the results of the control group (restrictive)<br/>",
  "and ends with the results of the treatment group (liberal).<br/>",
  "2. Width of arrow shafts reflect study size.<br/>",
  #"3. <span style='color:darkgreen;'>Green</span> arrows suggest benefit and ",
  #"<span style='color:red;'>red</span> arrows suggest harm.<br/>",
  "4. Dashed arrow shafts indicate nonsignificant results.<br/>",
  "Code and data available at https: //openmetaanalysis.github.io/sepsis."
)

ggplot(
  data_arrows,
  aes(
    x    = control_biomarker_post,
    y    = Event_rate_con / duration,
    xend = exp_biomarker_post,
    yend = Event_rate_exp / duration
  )
) +
  # 1) Draw arrowed segments with variable thickness
  geom_segment(
    aes(
      color    = arrow_color,  # "red" or "darkgreen"
      linetype = linetype,     # numeric code: 1=solid, 2=dashed
      size     = linewidth     # thickness based on study size
    ),
    lineend = "round",    # round corners
    linejoin = "round",   # round joins
    arrow = arrow(
      angle  = 20,                  # narrower arrowhead angle
      length = unit(0.12, "inches"),
      type   = "closed"             # solid (filled) triangular arrowhead
    )
  ) +
  
  # 2) Label each study to the left of the arrow start
  geom_text(
    aes(label = Study.name),
    # Position text so its right edge is ~1 char to the left of arrow's start
    nudge_x = -1,     
    hjust = 1,        
    color = "black",
    size = 3
  ) +
  
  # 3) Use identity for color and linetype, set the displayed thickness range
  scale_color_identity() +
  scale_linetype_identity() +
  scale_size_continuous(
    range = c(0.5, 1),  # map [min(linewidth), max(linewidth)] to [0.5, 1] on screen
    guide = FALSE       # hide size legend
  ) +
  
  # 4) X/Y-axis scales
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 30, 10),  limits = c(0, 30)) +
  
  # 5) Title, axes, footnotes (using ggtext’s element_markdown)
  labs(
    title   = "Arrows plot: liberal versus restrictive fluids for septic shock",
    x       = "Treatment (fluids ml/kg)",
    y       = "Outcome (overall mortality)",
    caption = footnotes
  ) +
  theme_bw() +
  theme(
    plot.caption = element_markdown(hjust = 0)  # left-align footnotes
  )

plotname <- paste0("arrows_plot_sepsis_-_", Sys.Date(),".tiff")
ggsave(filename = plotname, path = getwd(), width = 6, height = 4, units = 'in', device='tiff', dpi=800)

