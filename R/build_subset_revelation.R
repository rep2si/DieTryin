#' Build a single payoff file for DieTryinSR in enhanced mode
#'
#' @param path Path to RICH folder.
#' @param pid ID code of receiver (who will be presented the payout made by giver)
#' @param aid ID of giver (whose offer is already known)
#' @param offer Offer that was made by giver to receiver.
#' @param seed Number to use in seeding the randomization process if used.
#' @param gid_size Number of terms in hashcodes for the game IDs.
#' @export


build_subset_revelation <- function (path, subdir = "SubsetRevelations", pid = NULL, aid = NULL, offer = NA, likert_levels = c("Nope", "Meh", "Kinda", "Yeah", "Mega"), dont_know_text = "dunno", anonymous = FALSE, seed = 123, gid_size = 4) {

  # # Set random number generator seed to make repeatable game IDS
  # if(!is.na(seed)){
  #   set.seed(seed)
  # }

  GID  = toupper(random_string(1, gid_size))

  # Create dir if necessary
  dir <- paste0(path, "/", subdir)

  if (!dir.exists(dir)) {
    dir.create(dir)
    }

  anon_string <- ifelse(anonymous, "true", "false")

  n_levels <- length(likert_levels)

  ## Build csv
  output = cbind(
    c("RID", "ID", "GID", "AID", "Given", "anonymous", "allocEval", "NlikertLevels", "dontKnowText"),
    c(NA,    pid,   GID,   aid,   offer,   anon_string, "",          n_levels,       dont_know_text)
  )

  ll <- cbind(
    paste0("likertLevel", seq_along(likert_levels)),
    likert_levels
  )

  time_stamps <- cbind(
    c("loadTime", "saveTime"),
    c("",         "")
  )

  output = rbind(output, ll, time_stamps)
  
  colnames(output) = c("Variable", "Data")
  write.csv(output, paste0(path, "/", subdir , "/", GID, ".csv"), row.names = FALSE)

  ## And parse to JSON
  LB = length(output[,1])
  billy = c()
  for(i in 1:(LB-1)){
    billy = paste0(billy, paste0('"',output[i,1],'":"', output[i,2],'",'))
  }
  billy = paste0(billy, paste0('"',output[LB,1],'":"', output[LB,2],'"'))
  billy = paste0("{",billy,"}")

  write(billy, paste0(path, "/", subdir , "/",  GID,".json"))

}
