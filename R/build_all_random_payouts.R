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
#' @export

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


  dir <- paste0(path, "/SubsetPayouts")

  if (!dir.exists(dir)) {
    dir.create(dir)
    }

  results <- read.csv(res_file)

  all_ids <- results$ID
  all_aids <- results$AID
  all_players <- unique(c(all_ids, all_aids))

  all_payouts <- vector("list", length(all_players))

  for (p in seq_along(all_players)) {
          player <- all_players[[p]]

          # sample allocations kept
          df_allocs_kept <- results[results$ID == player, ]
          n_max <- nrow(df_allocs_kept)
          n_sampled_keep <- ifelse(n_max > n_alloc_keep, n_alloc_keep, n_max)
          df_allocs_kept <- df_allocs_kept[sample(nrow(df_allocs_kept), n_sampled_keep), ]
          df_allocs_kept$whichPaidOut = "amtKept"

          # sample allocations received, removing any optouts. Note AID!
          df_allocs_received <- results[results$AID == player & (results$optedOut == "false"), ]
          n_max <- nrow(df_allocs_received)
          n_sampled_received <- ifelse(n_max > n_alloc_receive, n_alloc_receive, n_max)
          df_allocs_received <- df_allocs_received[sample(nrow(df_allocs_received), n_sampled_received), ]
          df_allocs_received$whichPaidOut = "amtGiven"

          # dump all the implemented allocations in a df
          all_payouts[[p]] <- rbind(df_allocs_kept, df_allocs_received)

          # calculate total received and given
          keep_these <- df_allocs_kept$amtKept
          receive_these <- df_allocs_received$amtGiven
          tot_kept = sum(keep_these)
          tot_received = sum(receive_these)

          # and build jsons using these
          build_subset_payout(
                  path = path,
                  pid = player,
                  amt_kept = tot_kept,
                  amt_received = tot_received,
                  n_kept = n_sampled_keep,
                  n_received = n_sampled_received,
                  guess_margin = guess_margin,
                  guess_payout_amt = guess_payout_amt
          )
  }
  # Create df of all payouts and write it to csv
  df_all_payouts <- do.call("rbind", all_payouts)
  write.csv(df_all_payouts, paste0(path, "SubsetPayouts/all_payouts.csv"))
  message("Json and csv files generated in 'SubsetPayouts'")
  message("CSV file 'all_payouts.csv' also in 'SubsetPayouts'")
}
