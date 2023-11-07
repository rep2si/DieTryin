#' Build a single payout file for DieTryinSR in enhanced mode
#'
#' @param path Path to RICH folder.
#' @param pid ID code of participant.
#' @param aid ID of alter 
#' @param pid_role Set to "receive" if this was an amount given to pid, "keep" if this was an amount kept by pid.
#' @param amt Amount given or kept
#' @param anonoymous Set to TRUE to hide alter's photograph (in practice should only be used with pid_role == receive)
#' @param opted_out Set to TRUE if giver opted out. Only useful for pid_role == "keep". amt still needs to be accurate
#' @param seed Number to use in seeding the randomization process if used.
#' @param gid_size Number of terms in hashcodes for the game IDs.

build_subset_payout <- function (path, subdir = "SubsetPayouts", pid = NULL, kept = 0, received = 0) {

  ## Build csv
  output = cbind(
    c("RID", "ID", "amtKept", "amtReceived", "loadTime", "saveTime" ),
    c("",    pid,  kept,      received,      "",         ""         )
  )

  # Write csv (creating directory if necessary)
  dir <- paste0(path, "/", subdir)

  if (!dir.exists(dir)) {
    dir.create(dir)
    }
  
  colnames(output) = c("Variable", "Data")
  write.csv(output, paste0(dir, "/", pid, ".csv"), row.names = FALSE)

  ## And parse to JSON
  LB = length(output[,1])
  billy = c()
  for(i in 1:(LB-1)){
    billy = paste0(billy, paste0("'",output[i,1],"':'", output[i,2],"',"))
  }
  billy = paste0(billy, paste0("'",output[LB,1],"':'", output[LB,2],"'"))
  billy = paste0("{",billy,"}")

  write(billy, paste0(dir, "/", pid,".json"))
}
