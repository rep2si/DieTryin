#' Build all payout files for DieTryinSR in enhanced mode
#'
#' This is a simple loop that takes game results from an initial set of dicator games (played in "enhanced" mode) and generates files that can be used for the payout functionality in DieTryinSR. This should only be used after the SubsetContributions-SummaryTable.csv file is created, using compile_subset_survey_data().
#'
#' @param path Path to RICH folder.
#' @param keep_anonymous Set to TRUE if you want to generate files for the games that were played in the "anonymous" condition. Defaults to FALSE.
#' @export

build_all_revelations <- function(path, remove_optouts = TRUE, gid_size = 4) {
  res_file <- paste0(path, "/Results/SubsetContributions-SummaryTable.csv")

  if (!file.exists(res_file)) {
    stop("SubsetContributions-SummaryTable.csv not found. Have you run compile_subset_survey_data()?")
  }

  res <- read.csv(res_file)


  if (remove_optouts) {
    res <- res[res$optedOut != "true", ]
  }
  
  # Create dir if necessary
  dir <- paste0(path, "/SubsetRevelations")

  if (!dir.exists(dir)) {
    dir.create(dir)
    }

  for (r in seq_len(nrow(res))) {
    receiver <- res[r, "AID"]
    giver <- res[r, "ID"]
    amount_offered <- res[r, "amtGiven"]
    anonymous <- ifelse(res[r, "Condition"] == "anonymous", TRUE, FALSE)
    build_subset_expectation(path = path, subdir = "SubsetRevelations", pid = receiver, aid = giver, offer = amount_offered, anonymous = anonymous, gid_size = gid_size)
  }
}
