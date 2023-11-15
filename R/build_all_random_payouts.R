#' Select random payouts and generate corresponding
#'
#' Randomly select a number of allocations made by and to each player,
#' and generate the json files required for the payout activity in
#' DieTryinSR.
#'
#' This function requires the results of the allocations to have been
#' compiled with compile_subset_survey_data().
#'
#' @param n_alloc_keep Number of 'outgoing' allocations to select:
#' will implement the amount 'kept' by the player. If there are fewer
#' allocations than this number, implement the closest possible figure.
#' @param n_alloc_receive Number of 'incoming' allocations to select:
#' will implement the amount 'give' to the player. If there are fewer
#' allocations than this number, implement the closest possible figure.
#' @param guess_margin the margin within which an expected allocation
#' will be rewarded. E.g. set to 10 to reward all guesses within ±10
#' of actual allocation.
#' @param guess_payout_amt The amount given per correct guess.

build_all_random_payouts <- function(path, n_alloc_keep = 1,
                                     n_alloc_receive = 1,
                                     guess_margin = 0,
                                     guess_payout_amt = 0) {

  res_file <- paste0(
    path,
    "/Results/SubsetContributions-SummaryTable.csv"
  )

  if (!file.exists(res_file)) {
    stop(
      "SubsetContributions-SummaryTable.csv not found.
       Have you run compile_subset_survey_data()?"
    )
  }

  results <- read.csv(res_file)

  all_ids <- results$ID
  all_aids <- results$AID
  all_players <- unique(c(all_ids, all_aids))

  tmp <- lapply(all_players, function(p) {
    # sample allocations kept
    allocs_kept <- results[results$ID == p, "amtKept"]
    n_max <- length(allocs_kept)
    n_sampled_keep <- ifelse(n_max > n_alloc_keep, n_alloc_keep, n_max)
    keep_these <- sample(allocs_kept, size = n_sampled_keep)
    # sample allocations received, removing any optouts. Note AID!
    allocs_received <- results[results$AID == p & (is.na(results$optedOut) | results$optedOut == "false"), "amtGiven"]
    n_max <- length(allocs_received)
    n_sampled_received <- ifelse(n_max > n_alloc_receive, n_alloc_receive, n_max)
    receive_these <- sample(allocs_received, size = n_sampled_received)
    tot_kept = sum(keep_these)
    tot_received = sum(receive_these)

    build_subset_payout(path             = path,
                        pid              = p,
                        amt_kept         = tot_kept,
                        amt_received     = tot_received,
                        n_kept           = n_sampled_keep,
                        n_received       = n_sampled_received,
                        guess_margin     = guess_margin,
                        guess_payout_amt = guess_payout_amt
                        )

  ## In case we want to export a df with all the planned payouts.
  #   d_payouts <- data.frame(
  #       id = p,
  #       kept = tot_kept,
  #       received = tot_received
  #     )
  #   return(d_payouts)
   })

}
