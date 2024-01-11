#' Build all revelation files for DieTryinSR in enhanced mode
#'
#' This is a simple loop that takes game results from an initial set of dictator games (played in "enhanced" mode) and generates files that can be used for the revelations functionality in DieTryinSR. To construct the data frame passed as "allocation_df", you will most likely want compile SubsetContributions-SummaryTable.csv, using compile_subset_survey_data(), then read this is with read.csv().
#'
#' @param path Path to RICH folder.
#' @param remove_optouts Defaults to TRUE. Removes all allocation decisions in which the allocator opted out. There is most likely no reason to ever set this to FALSE.
#' @param gid_size Number of terms in hashcodes for the game IDs.
#' @param allocations_df The data frame of allocations to use when constructing revelations. See remarks above.
#' @param likert_levels a vector of strings, of max length 5, e.g. c("not enough", "enough", "more than enough"). This should contain the levels in the likert scale presented to the participant. Note that the display of this likert scale is optional and can be controlled in settings.json, with the option "likertInRevelations", which can take the following values: "true", "false", "revealedOnly".
#' @param dont_know_text A string, displayed as the "don't know" option with the likert scale.
#' @export

build_all_revelations <- function(path, remove_optouts = TRUE, gid_size = 4, allocations_df = NA, likert_levels = c("Nope", "Meh", "Kinda", "Yeah", "Mega"), dont_know_text = "dunno")  {

  res <- allocations_df

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
    build_subset_revelation(path = path, subdir = "SubsetRevelations", pid = receiver, aid = giver, offer = amount_offered, anonymous = anonymous, likert_levels = likert_levels, dont_know_text = dont_know_text , gid_size = gid_size)
  }
}
