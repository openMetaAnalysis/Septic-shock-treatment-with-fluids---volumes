# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-12-13

file.filter   <- matrix(c("Cancel this","ZZZZZ.txt"),byrow=TRUE,ncol=2)
choose.files(filters= file.filter, caption="Cancel. This is a work around to avoid trouble with new version of R Studio")

library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

#== Startup ======
#* Troubleshooting -----
#options(error = NULL)   # Default
#options(warn = 2)       # Converts warnings into errors
options(warn = 2, error = browser)
options(error = recover) # This will provide a menu showing the call stack, and you can choose which environment to inspect.
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

# Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
#getwd()
# Did the script load? -----
#tk_messageBox(type = "ok", paste('1. ', 'R has loaded.\n\nWorking directory:\n', getwd(), sepo=''), caption = "Hello")

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

par(mar=c(5.1, 4.1, 4.1, 1)) # (bottom, left, top, right)

analyticmethod = "Random effects model (Hartung-Knapp)"
meta::forest (meta1, sortvalue = data_sepsis$Year, 
              #xlim=c(0,100), 
              leftcols = c("studlab", "Restrictive.fluids", "control.rate"),
              leftlabs = c("Study","Restrictive fluids\n(ml/kg)", "Mortality\n(control group)"),
              print.subgroup.name = FALSE,
              col.diamond="blue", col.diamond.lines="blue", title = "Sepsis", 
              fixed = FALSE, common = FALSE, random = TRUE, method.random.ci = "HK",
              #resid.hetstat = TRUE, 
              addrows = 2,
              print.I2.ci=TRUE, print.tau2=FALSE, print.p=FALSE, 
              label.left="Favors liberal", label.right="Favors restrictive",
              text.random=analyticmethod, fs.random=12, ff.random = 1, ff.hetstat=2, fs.hetstat=12)
grid.text("Forest plot: liberal versus restrictive fluids for septic shock", 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))

# Copyright statement -----
grid.text('Notes:', 
          0.02, 0.125, hjust=0, gp=gpar(cex=1, font=2))
grid.text(paste0("Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)"),
          0.95,  0.1,    hjust=1, gp=gpar(cex=1, font=1))
grid.text(paste0("Version: ", current.date(), ". Code and data available at https://openmetaanalysis.github.io/sepsis"),
          0.95,  0.05,    hjust=1, gp=gpar(cex=1, font=1))

#* Print -----
plotname <- paste0("forest_plot_sepsis_-_")
function_plot_print (plotname, 1000,500)

# ____________________________________ ----
# Arrows plot ------

data_arrows <- data.import

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
par(mfrow=c(1,1), oma = c(4, 0, 0, 0))

topic = "Arrows plot: liberal versus restrictive fluids for septic shock"
outcome = "overall mortality"
biomarker = "Fluids ml/kg"
timeframe = "90 days"

par(yaxt = "n")
#Annualized
plot(data_arrows$control_biomarker_post, data_arrows$Event_rate_con/data_arrows$duration,
     type = "n",
     axes = FALSE, xlab = "", ylab = "", 
     main=topic,
     ylim=c(0,30), xlim=c(0,100), 
     )
par(yaxt = "s")
#mtext(side=3,line=3,"Aggressive treatment of diabetes.", cex=1.2,font=2, adj=0)
axis(2,seq(0, 50, by = 10), tick = TRUE,c("0%","10%","20%","30%","40%","50%"))
axis(1,seq(0, 100, by = 10), tick = TRUE,c(seq(0, 100, by = 10)))
box(which="plot")

##Grid lines
i = 0
for(i in 0:10) # Horizontal
{
  abline(h=i*10, v=0, col = "gray90")
}

for(i in seq(0, 100, by = 10)) # Vertical
{
  abline(h=0, v=i, col = "gray90")
}
mtext(side=2,line=3,paste("Clinical outcome (", outcome, ")", sep=""), font=2)
mtext(side=2,line=2,timeframe, font=1)
# "Biomarker"
#mtext(side=1,line=2, paste("Biomarker (", biomarker, ")", sep=""), font=2)
# "Treatment"
mtext(side=1,line=2, paste("Treatment (", biomarker, ")", sep=""), font=2)

##Study labels
#default
for(i in 1: nrow(data_arrows))
{
  data_arrows$placement[i] <- 3
  if (data_arrows$Study.name[i] == "EXAMINE"){data_arrows$placement[i] <- 1} 
}

for(i in 1: nrow(data_arrows))
{  
  #Upper right adj=c(0,0)
  #Lower right adj=c(0,1)
  #Upper left adj=c(1,0)
  #Lower left adj=c(1,1)
  if (data_arrows$placement[i] == 1)
  {text(x=control_biomarker_post[i], y=Event_rate_con[i]/duration[i] - 0, labels=paste(Study.name[i], sep=" "), cex=0.65, pos=4, adj=c(0,0), font=1,col='black')}
  if (data_arrows$placement[i] == 2)
  {text(x=control_biomarker_post[i], y=Event_rate_con[i]/duration[i] - 0, labels=paste(Study.name[i], sep=" "), cex=0.65, pos=3, adj=c(0,1), font=1,col='black')}
  if (data_arrows$placement[i] == 2.5)
  {text(x=control_biomarker_post[i], y=Event_rate_con[i]/duration[i] - 0, labels=paste(Study.name[i], sep=" "), cex=0.65, pos=4, adj=c(0,1), font=1,col='black')}
  if (data_arrows$placement[i] == 3)
  {text(x=control_biomarker_post[i], y=Event_rate_con[i]/duration[i] - 0, labels=paste(Study.name[i], sep=" "), cex=0.65, pos=2, adj=c(1,0), font=1,col='black')}
  if (data_arrows$placement[i] == 4)
  {text(x=control_biomarker_post[i], y=Event_rate_con[i]/duration[i] - 0, labels=paste(Study.name[i], sep=" "), cex=0.65, pos=2, adj=c(1,1), font=1,col='black')}
}  

#* draw arrows from point to point -----
#s <- seq(length(x)+1)

lower_CI=meta1.summary$lower[]
upper_CI=meta1.summary$upper
data_arrows$Size <- data_arrows$control.total + data_arrows$experimental.total
data_arrows$significant= (lower_CI>0 | upper_CI<0)
n_study=nrow(data_arrows)
data_arrows$linetype    = 1
data_arrows$linewidth   = 1
data_arrows$segmenttype = 0
# 1 =solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
for(i in 1: nrow(data_arrows))
  {
  data_arrows$linewidth[i] = 
    (data_arrows$Size[i]^1.5)/(max(data_arrows$Size^1.5))
  if(significant[i] == TRUE)
    {
    data_arrows$linetype[i]= 1 
    data_arrows$segmenttype[i]= 1
    }
  else 
  {
    data_arrows$linetype[i]= 2 
    data_arrows$segmenttype[i]= 3
  }
}
data_arrows$linewidth

# Draw the lines
data_arrows$linewidth <- data_arrows$linewidth * 3
data_arrows$linewidth <- ceiling(data_arrows$linewidth)
for (i in 1:nrow(data_arrows))
  {
  if (data_arrows$OR[i] > 1)
    {
    # Studies suggesting harm
    # Sig manifested in data_arrows$linetype[i]
    #arrows(x2[s_OR_greater_than_one_data], y2[s_OR_greater_than_one_data],x[s_OR_greater_than_one_data],y[s_OR_greater_than_one_data], lwd=linewidth,length=0.1,col='red',lty=linetype[s_OR_greater_than_one_data])
    segments(x2[i], y2[i],x[i],y[i], 
             lwd=data_arrows$linewidth[i],col="red", lty=data_arrows$linetype[i])
    arrows(
      x[i] + (x2[i] - x[i]) * 0.98,
      y[i] + (y2[i] - y[i]) * 0.98,
      x2[i], y2[i],
      lwd = data_arrows$linewidth[i],
      length = 0.1,
      col = "red",
      lty = 1
    )
  }
  else
    {
    # Studies suggesting benefit
    # Sig manifested in data_arrows$linetype[i]
    #arrows(x2[s_OR_less_than_one_data], y2[s_OR_less_than_one_data],x[s_OR_less_than_one_data],y[s_OR_less_than_one_data], lwd=linewidth,length=0.1,col="darkgreen", lty=linetype[s_OR_less_than_one_data])
    segments(x2[i], y2[i],x[i],y[i], 
           lwd=data_arrows$linewidth[i],col="darkgreen", lty=data_arrows$linetype[i])
      arrows(
        x[i] + (x2[i] - x[i]) * 0.98,
        y[i] + (y2[i] - y[i]) * 0.98,
        x2[i], y2[i],
        lwd = data_arrows$linewidth[i],
        length = 0.1,
        col = "darkgreen",
        lty = 1
      )
      print(data_arrows$linewidth[i])
      }
  }
# 1 =solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash

#Notes ------
mtext(side=1,line=3,cex=0.9,adj=0,"Notes:", font=2)
mtext(side=1,line=5,cex=0.9,adj=0, "1. For each study, the arrow starts with the results of the control group (restrictive) and\nends with the results of the treatment group (liberal).", font=1)
mtext(side=1,line=6,cex=0.9,adj=0, "2. Width of arrows reflect study size.", font=1)
mtext(side=1,line=7,cex=0.9,adj=0, "3. Solid arrows indicate statistically significant change in clinical outcome.", font=1)

#mtext(side=1,line=8,cex=0.9,adj=0, "2. Green arrows suggest benefit and red arrows suggest harm.", font=1)
# Write the beginning of the sentence
mtext(side=1, line=8, cex=0.9, adj=0, font=1, "4. ")
# Add the word "Green" in green
mtext(side=1, line=8, cex=0.9, font=1, "Green", col="darkgreen", adj=0.03)
# Add the text after "Green"
mtext(side=1, line=8, cex=0.9, font=1, " arrows suggest benefit and ", adj=0.15)
# Add the word "red" in red
mtext(side=1, line=8, cex=0.9, font=1, "red", col="red", adj=0.44)
# Finish the sentence
mtext(side=1, line=8, cex=0.9, font=1, " arrows suggest harm.", adj=0.62)

#mtext(side=1,line=8,cex=0.9,adj=0,"3. P-best is the p-value assuming a relative risk reduction from treatment of 0.1. P-worst is \nthe P-value assuming that the\nrelative benefit of treat is 1.0", font=1)

#* Print -----
plotname <- paste0("arrows_plot_sepsis_-_")
function_plot_print (plotname, 680,500)

