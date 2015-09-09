
# x ska vara ett survfit-objekt
surv_table <-  function(x, times = 0:5, ...) {
 
  x <- summary(x, times, ...)
  
    with(x, 
      data.frame(
        strata = strata,
        tid = time,
        n.risk = n.risk,
        n.event = n.event,
        surv = round(surv * 100),
        `CI 95 %` = paste0("(", round(lower * 100), " - ", round(upper * 100), ")"),
        
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
}