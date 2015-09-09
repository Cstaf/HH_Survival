

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

###################################################################################################################################

####################################### Tabell med antal i risk specifika parametrar ##############################################

# Table: Skapa tabell under figur med antal i risk per år (logical)                         Default = TRUE
# Tabsize: Justerar storleken på elementen i tabellen                                       Default = 4
# Tabadjust1: Avstånd mellan rad titel och antal i risk år 0                                Default = 1
# Tabadjust2: Justering av hela tabellen i förhållande till grafen                          Default = 1

###################################################################################################################################

#################################################### Övriga parametrar ############################################################

# Main: Huvudtitel                                                                          Default = ""
# censoring_shape: Form på markering för censorering                                                Default = NULL
# CI: Skall konfidensintervall inkluderas? (logical)                                        Default = FALSE

###################################################################################################################################

summary_incasurv <- function(sfit) {
  
  # Data to be used in the survival plot
  df <- as.data.frame(unclass(sfit)[c("time", "n.risk", "n.event", "n.censor", "surv", "upper", "lower")])
  df$strata <- if (is.null(sfit$strata)) "All" else as.character(survival:::summary.survfit(sfit, censored = T)$strata)
 
  # Add initial start values for time = 0
  zeros <- data.frame(
    strata = unique(df$strata),
    time   = 0, surv = 1,
    upper  = 1, lower = 1, 
    stringsAsFactors = FALSE)
  df <- plyr::rbind.fill(zeros, df)

  df
}
  




surv_plot <- function(sfit,
                      table = TRUE,
                      xlabs = "År efter diagnos",
                      ylabs = "Överlevnad",
                      tablabs = "Antal i risk; överlevnad i % (95 % CI för överlevnad)",
                      xlims = c(0,5),
                      ylims = c(0,1),
                      timeby = 1,
                      main = "",
                      censoring_shape = NULL,
                      legend.position = c( 0.05, 0.025),
                      CI = FALSE,
                      tabadjust1 = 1,
                      tabadjust2 = 1,
                      linesize = 1,
                      base_size = 12,
                      legendtitle = "",
                      tabsize = 3
                      ) {
  
  library(ggplot2)
  
  #################################
  #         Hjälpvariabler        #
  #################################
  
  df                 <- summary_incasurv(sfit)
  times              <- seq(0, max(sfit$time), by = timeby)
  
  surv_by_strata     <- survival:::summary.survfit(sfit, times = times, extend = TRUE)
  no.strata          <- max(1, length(unique(surv_by_strata$strata)))
  strata_name_length <- max(nchar(df$strata))
  
  colors_hc          <- c("#00b3f6","#ffb117", "#434348", 
                          "#90ed7d","#AAEEEE", "#f15c80",
                          "#e4d354", "#2b908f", "#f45b5b",
                          "#7798BF", "#8085e9")
  
  
  ###################################
  # specifying plot parameteres etc #
  ###################################
  
  # Kaplan-Meier-kurva/kurvor
  p <- ggplot( df, aes(time, surv)) +
    geom_step(aes(colour = strata), size = linesize) +
    theme_bw(base_size = base_size) +
    scale_x_continuous(xlabs, breaks = times, limits = xlims) +
    scale_y_continuous(ylabs, limits = ylims, labels = scales::percent) +
    theme(
        axis.title.x         = element_text(vjust = 0.5),
        panel.grid.minor     = element_blank(),
        legend.justification = c(0,0),
        legend.position      = legend.position,
        legend.key           = element_rect(colour = NA),
        legend.text          = element_text(size = base_size),
        legend.title         = element_blank(),
        panel.border         = element_blank(),
        legend.background    = element_rect(fill = scales::alpha('white', 0)),
        panel.grid.major.y   = element_line(color = 'gray', size = .3),
        plot.margin          = grid::unit(c(0, 1, .5, if (strata_name_length < 10) 1.5 else 2.5),"lines")) +
    ggtitle(main) +
    scale_colour_manual(values = colors_hc)

 
  # Control the legend - use only of more than one strata!
  if (no.strata <= 1) {
    p <- p + 
      theme(legend.position = "none")
  } else if (legendtitle != "") { 
    p <- p + 
      scale_colour_manual(name  = legendtitle, values = colors_hc) +
      theme(legend.title = element_text(face = "bold")) 
  }
    
  
  # CI
  if (CI) {
    p <- p + 
      geom_step(aes(y = upper, col = strata), linetype = "dashed") +
      geom_step(aes(y = lower, col = strata), linetype = "dashed")
  } 
  
  
  # Add censoring marks to the line:
  if (!is.null(censoring_shape)) {
    p <- p + geom_point(data = subset(df, n.censor >= 1), 
                        aes(x = time, y = surv), 
                        shape = censoring_shape)
  }

  ## Create a blank plot for place-holding
  blank.pic <- 
    ggplot(df, aes(time, surv)) +
    geom_blank() + 
    theme_bw() +
    theme(axis.text.x       = element_blank(), 
          axis.text.y       = element_blank(),
          axis.title.x      = element_blank(),
          axis.title.y      = element_blank(),
          axis.ticks        = element_blank(),
          panel.grid.major  = element_blank(),
          panel.border      = element_blank()
    )
  
  ###################################################
  # Create table graphic to include at-risk numbers #
  ###################################################
  
  if (!table) {
    return(p)
  } else {
    
    
    # Table with data to present
    risk.data <- 
      with(surv_by_strata,
        data.frame(
        strata = if (no.strata <= 1) "All" else strata,
        time   = time,
        n.risk = n.risk,
        cell_text = ifelse(time != 0,
                      paste0(n.risk, "; ", 
                           round(surv * 100), 
                           " (", round(lower * 100), "-", round(upper * 100), ")"),
                      n.risk)
      )
    )

    data.table <- 
      ggplot(risk.data, 
             aes(x = time, y = rev(strata), label = format(cell_text, justify = "left"))) +
      geom_text(size = tabsize) + 
      theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                       labels = rev(levels(risk.data$strata))) +
      scale_x_continuous(tablabs, limits = xlims) +
      theme(axis.title.x     = element_text(size = tabsize * 3, vjust = 1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border     = element_blank(),
            axis.text.x      = element_blank(),
            axis.ticks       = element_blank(),
            axis.text.y      = element_text(face = "bold", hjust = 1, size = tabsize * 3),
            legend.position  = "none",
            plot.margin      = grid::unit(x = c(
                                      -1.5,                                           # top
                                      tabadjust1,                                     # right
                                      0.1,                                            # bottom
                                      (if (strata_name_length < 10) 2.5 else 3.5 )
                                         * tabadjust2 - 0.15 * strata_name_length),   # left 
                                    units = "lines")
      ) +
      xlab(NULL) + 
      ylab(NULL)
    
    # Make plot and table
    gridExtra::grid.arrange(
                p, 
                blank.pic, 
                data.table, 
                clip = FALSE, nrow = 3, ncol = 1, 
                heights = grid::unit(c(2, .1, .5), c("null", "null", "null"))
    )
  } 
}