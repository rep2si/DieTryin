#' Build a single allocation evaluation file for DieTryinSR in enhanced mode
#'
#' @param path Path to RICH folder.
#' @param pid ID code of participant.
#' @param aid ID of alter (about whom reputational eval is being made)
#' @param seed Number to use in seeding the randomization process if used.
#' @param gid_size Number of terms in hashcodes for the game IDs.
#' @export

build_subset_allocation <- function (path, subdir = "SubsetContributions", pid = NULL, aid = NULL,  seed = 123, gid_size = 4, condition = "", endowment = 100, opt_out_keep = 0, ask_opt_out = FALSE) {

  # # Set random number generator seed to make repeatable game IDS
  # if(!is.na(seed)){
  #   set.seed(seed)
  # }

  GID  = toupper(random_string(1, gid_size))

  ## Build csv
  ask_string <- ifelse(ask_opt_out, "true", "false")
  
  output = cbind(
    c("RID", "ID", "GID", "AID", "Condition", "Endowment", "amtKept", "amtGiven", "loadTime", "saveTime", "optedOut", "optOutKeep", "askOptOut"),
    c("",    pid,  GID,   aid,   condition,   endowment,   "",        "",       "",          "",         "",        opt_out_keep  , ask_string)
  )

  # Write csv (creating directory if necessary)
  dir <- paste0(path, "/", subdir)

  if (!dir.exists(dir)) {
    dir.create(dir)
    }
  
  colnames(output) = c("Variable", "Data")
  write.csv(output, paste0(dir, "/", GID, ".csv"), row.names = FALSE)

  ## And parse to JSON
  LB = length(output[,1])
  billy = c()
  for(i in 1:(LB-1)){
    billy = paste0(billy, paste0("'",output[i,1],"':'", output[i,2],"',"))
  }
  billy = paste0(billy, paste0("'",output[LB,1],"':'", output[LB,2],"'"))
  billy = paste0("{",billy,"}")

  write(billy, paste0(dir, "/", GID,".json"))

}

