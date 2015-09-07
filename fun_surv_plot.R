

#################################################### Skalspecifika parametrar #####################################################

# Xlabs: x-axis label                                                                       Default = "År efter diagnos"
# Ylabs: y-axis label                                                                       Default = "Överlevnad"  
# Xlims: Vektor med min,max på xaxelns breaks                                               Default = c(0,5)
# Ylims: Vektor med min,max på yaxelns breaks                                               Default = c(0,1)
# Timeby: Sätt tidens intervall på x-axis (numerisk)                                        Default = 1
# Base_size: Ändrar textstorlek på figuren (Ej tabell)                                      Default = 14
# Linesize: Tjockleken på överlevnadskurvan                                                 Default = 1

###################################################################################################################################

#################################################### Legendspecifika parametrar ###################################################

# Legend: Skall en legend inkluderas? (logical)                                             Default = FALSE
# Legendposition: Positionen för legenden, c(x,y)                                           Default = c(.25, .25)
# Legendtitle: Legendtitel                                                                  Default = ""
# Ystratalabs: Vektor för stratum levels                                                    Default = levels(summary(sfit)$strata)

###################################################################################################################################

####################################### Tabell med antal i risk specifika parametrar ##############################################

# Table: Skapa tabell under figur med antal i risk per år (logical)                         Default = TRUE
# Tabsize: Justerar storleken på elementen i tabellen                                       Default = 4
# Tabadjust1: Avstånd mellan rad titel och antal i risk år 0                                Default = 1
# Tabadjust2: Justering av hela tabellen i förhållande till grafen                          Default = 1

###################################################################################################################################

#################################################### Övriga parametrar ############################################################

# Main: Huvudtitel                                                                          Default = ""
# Marks: Markering för censorering (logical)                                                Default = FALSE
# Shape: Formen på markering för censorering                                                Default = Ett streck
# CI: Skall konfidensintervall inkluderas? (logical)                                        Default = FALSE

###################################################################################################################################

surv_plot <- function(sfit,
                    table = TRUE,
                    xlabs = "År efter diagnos",
                    ylabs = "Överlevnad",
                    tablabs = "Antal i risk",
                    xlims = c(0,5),
                    ylims = c(0,1),
                    ystratalabs = NULL,
                    timeby = 1,
                    main = "",
                    marks = FALSE,
                    shape = 3,
                    legend = F,
                    legend.position = c( .25, .25),
                    CI = F,
                    tabadjust1 = 1,
                    tabadjust2 = 1,
                    linesize = 1,
                    base_size=14,
                    legendtitle = "",
                    tabsize = 4,
                    ...) {
  

  #############
  # libraries #
  #############
  
  #Check if the following packages have been installed. If not, install them
  if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
  if (!"survival" %in% installed.packages()) install.packages("survival")
  if (!"gridExtra" %in% installed.packages()) install.packages("gridExtra")
  if (!"reshape" %in% installed.packages()) install.packages("reshape")
  library(scales)
  suppressPackageStartupMessages(library(ggplot2, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(survival, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(gridExtra, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(reshape, warn.conflicts=FALSE))
  

  #################################
  #         sortering             #
  #################################
  fit <- sfit
  
  times <- seq(0, max(fit$time), by = timeby)
    if(length(levels(summary(fit)$strata)) == 0) {
      subs1 <- 1
      subs2 <- 1:length(summary(fit,censored=T)$time)
      subs3 <- 1:length(summary(fit,times = times,extend = TRUE)$time)
    } else {
      subs1 <- 1:length(levels(summary(fit)$strata))
      subs2 <- 1:length(summary(fit,censored=T)$strata)
      subs3 <- 1:length(summary(fit,times = times,extend = TRUE)$strata)
    }
  
  
  ##################################
  # data manipulation för plot    #
  ##################################
  
  if(length(levels(summary(fit)$strata)) == 0) {
    #[subs1]
    if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","","All"))
  } else {
    #[subs1]
    if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","",names(fit$strata)))
  }
  
ystrataname <- "Strata"
  m <- max(nchar(ystratalabs))
  times <- seq(0, max(fit$time), by = timeby)
  
  if(length(levels(summary(fit)$strata)) == 0) {
    Factor <- factor(rep("All",length(subs2)))
  } else {
    Factor <- factor(summary(fit, censored = T)$strata[subs2])
  }
  
  #Data to be used in the survival plot
  .df <- data.frame(
    time = fit$time[subs2],
    n.risk = fit$n.risk[subs2],
    n.event = fit$n.event[subs2],
    n.censor = fit$n.censor[subs2],
    surv = fit$surv[subs2],
    strata = Factor,
    upper = fit$upper[subs2],
    lower = fit$lower[subs2]
  )
  
  #Final changes to data for survival plot
  levels(.df$strata) <- ystratalabs
  zeros <- data.frame(time = 0, surv = 1,
                      strata = factor(ystratalabs, levels=levels(.df$strata)),
                      upper = 1, lower = 1)
  .df <- rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))
  
  ###################################
  # specifying plot parameteres etc #
  ###################################
  
  # Sätt HC färger8085e9
#   colors_hc <- c("#7CB5EC", "#434348", "#90ed7d",
#                  "#f7a35c", "#AAEEEE", "#f15c80",
#                  "#e4d354", "#2b908f", "#f45b5b",
#                  "#91e8e1", "#7798BF", "#8085e9")
  
  colors_hc <- c("#00b3f6","#ffb117", "#434348", "#90ed7d","#AAEEEE", "#f15c80",
                 "#e4d354", "#2b908f", "#f45b5b",
                 "#91e8e1", "#7798BF", "#8085e9")
  
  p <- ggplot( .df, aes(time, surv)) +
    geom_step(aes(colour = strata), size = linesize) +
    theme_bw(base_size=base_size) +
    theme(axis.title.x = element_text(vjust = 0.5)) +
    scale_x_continuous(xlabs, breaks = times, limits = xlims) +
    scale_y_continuous(ylabs,limits = ylims, labels=percent) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = legend.position) +
    theme(legend.key = element_rect(colour = NA)) +
    theme(legend.text = element_text(size = base_size)) +
    theme(legend.title = element_blank()) +    
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.y  = element_line(color='gray', size = .3)) +
    labs(linetype = ystrataname) +
    theme(plot.margin = unit(c(0, 1, .5,ifelse(m < 10, 1.5, 2.5)),"lines")) +
    ggtitle(main) +
    scale_colour_manual(values=colors_hc)



 
  if(legendtitle !="") { 
    p <- p + scale_colour_manual(name  = legendtitle, values=colors_hc) +
      theme(legend.title = element_text(face = "bold")) }

  
  

  #Removes the legend: 
  if(legend == FALSE) 
    p <- p + theme(legend.position="none")
  
  # CI
  p <- if(CI == T ) {
    p + geom_step(aes(y = upper, col = strata), linetype="dashed") +
      geom_step(aes(y = lower, col = strata), linetype="dashed")
  } else (p)
  
  
  #Add censoring marks to the line:
  if(marks == TRUE)
    p <- p + geom_point(data = subset(.df, n.censor >= 1), aes(x = time, y = surv), shape = shape)
  

  ## Create a blank plot for place-holding
  blank.pic <- ggplot(.df, aes(time, surv)) +
    geom_blank() + theme_bw() +
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),panel.border = element_blank())
  
  ###################################################
  # Create table graphic to include at-risk numbers #
  ###################################################
  
  if(length(levels(summary(fit)$strata)) == 0) {
    Factor <- factor(rep("All",length(subs3)))
  } else {
    Factor <- factor(summary(fit,times = times,extend = TRUE)$strata[subs3])
  }
  
  if(table == TRUE) {
    risk.data <- data.frame(
      strata = Factor,
      time = summary(fit,times = times,extend = TRUE)$time[subs3],
      n.risk = summary(fit,times = times,extend = TRUE)$n.risk[subs3]
    )
    risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))
    
    data.table <- ggplot(risk.data,aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
      geom_text(size = tabsize) + theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                       labels = rev(ystratalabs)) +
      scale_x_continuous(tablabs, limits = xlims) +
      theme(axis.title.x = element_text(size = base_size, vjust = 1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),axis.text.x = element_blank(),
            axis.ticks = element_blank(),axis.text.y = element_text(face = "bold",hjust = 1, size =tabsize*3))
    
    data.table <- data.table +
      theme(legend.position = "none") + xlab(NULL) + ylab(NULL)
    
    # ADJUST POSITION OF TABLE FOR AT RISK
    data.table <- data.table +
      theme(plot.margin = unit(c(-1.5, 1*tabadjust1, 0.1, ifelse(m < 10, 2.5*tabadjust2, 3.5*tabadjust2) - 0.15 * m), "lines"))
    
    #######################
    # Plotting the graphs #
    #######################
    
    grid.arrange(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                 ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))
    
    if(table == FALSE) {
      a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                       ncol = 1, heights = unit(c(2, .1, .25), c("null", "null", "null")))
      return(a)
    }#if
  } else {
    if(table == FALSE) return(p)
  }#else
}