#' print given message string into konsole as log 
#'
#' @param msg message string to be printed to stdout
#' @param INIT boolean variable indicating whether this is start of initial setup
#' @param FINIT boolean variable indicating whether this is end of initial setup
#' @return  return zero value, if function ran smoothly
#' @keywords internal
#' 
print_log <- function (msg = NULL,
                       INIT = FALSE,
                       FINIT = FALSE,
                       PRINT_LOGS = TRUE) {
  # suppress logs --------------------------------------------------------------
  if (!PRINT_LOGS) return(0)
  
  # get system time ------------------------------------------------------------
  systime <- Sys.time()
  
  # start-up message -----------------------------------------------------------
  if (INIT) {
    message(
      paste ("\n--------------------------------------------------------------------------------\n",
             as.character(systime),
             "New session just started!",
             "\n--------------------------------------------------------------------------------\n"))
  }
  
  # end of start-up message ----------------------------------------------------
  if (FINIT) {
    message(
      paste ("\n--------------------------------------------------------------------------------\n",
             as.character(systime),
             "Initial setup was completed!",
             "\n--------------------------------------------------------------------------------\n"))
  }
  
  # print any other message to standard output ---------------------------------
  message (paste(as.character(systime),
                 signif(as.numeric(systime) - floor(as.numeric(systime)), 3),
                 msg, 
                 "\t"
                 ))
  
  # return zero for having run smoothly ----------------------------------------
  return(0)
  
}