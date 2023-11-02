#' Build all payout files for DieTryinSR in enhanced mode
#'
#' This is a simple loop that takes game results from an initial set of dicator games (played in "enhanced" mode) and generates files that can be used for the payout functionality in DieTryinSR. This should only be used after the SubsetContributions-SummaryTable.csv file is created, using compile_subset_survey_data().
#'
#' @param path Path to RICH folder.
#' @param keep_anonymous Set to TRUE if you want to generate files for the games that were played in the "anonymous" condition. Defaults to FALSE.

build_all_payouts <- function(path, keep_anonymous = TRUE, keep_optin = TRUE) {
  res_file <- paste0(path, "/Results/SubsetContributions-SummaryTable.csv")

  if (!file.exists(res_file)) {
    stop("SubsetContributions-SummaryTable.csv not found. Have you run compile_subset_survey_data()?")
  }

  res <- read.csv(res_file)

  if (!keep_anonymous) {
    res <- res[res$Condition != "anonymous", ]
  }

  if (!keep_anonymous) {
    res <- res[res$Condition != "optin", ]
  }

  for (r in seq_len(dim(res)[1])) {
    receiver <- res[r, "AID2"]
    giver <- res[r, "ID"]
    amount_offered <- res[r, "Offer2"]
    build_subset_payout(path = path, pid = receiver, aid = giver, offer = amount_offered)
  }
}
