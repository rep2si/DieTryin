#' Build a single reputational evaluation file for DieTryinSR in enhanced mode
#'
#' @param path Path to RICH folder.
#' @param pid ID code of participant.
#' @param aid ID of alter (about whom reputational eval is being made)
#' @param seed Number to use in seeding the randomization process if used.
#' @param gid_size Number of terms in hashcodes for the game IDs.
#' @export

build_subset_rep_eval <- function (path, subdir = "SubsetRep1", pid = NULL, aid = NULL,  seed = 123, gid_size = 4, questions = c("question1", "question2", "question3"), likert_levels = c("Nope", "Meh", "Kinda", "Yeah", "Mega"), dont_know_text = "dunno") {

  # # Set random number generator seed to make repeatable game IDS
  # if(!is.na(seed)){
  #   set.seed(seed)
  # }

  GID  = toupper(random_string(1, gid_size))

  n <- length(questions)
  n_levels <- length(likert_levels)

  ## Build csv
  output = cbind(
    c("RID", "TimeStamp", "ID", "GID", "AID", "Nquestions", "dontKnowText", "NlikertLevels"),
    c(NA, NA, pid, GID, aid, n, dont_know_text, n_levels)
  )

  # add one entry per likert level

  ll <- cbind(
    paste0("likertLevel", seq_along(likert_levels)),
    likert_levels
  )
  
  # add one entry per question
  ans <- cbind(
    paste0("q", seq_len(n)),
    rep("", n)
  )

  # add question text (needed in app)
  text <- cbind(
    paste0("text", seq_len(n)),
    questions
  )

  # add save and loadtimes
  time_stamps <- cbind(
          c(paste0("loadTime", seq_len(n)), paste0("saveTime", seq_len(n))),
          rep("", 2 * n)
  )
  
  output = rbind(output, ll, ans, text, time_stamps)

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
    billy = paste0(billy, paste0('"',output[i,1],'":"', output[i,2],'",'))
  }
  billy = paste0(billy, paste0('"',output[LB,1],'":"', output[LB,2],'"'))
  billy = paste0("{",billy,"}")

  write(billy, paste0(dir, "/", GID,".json"))

}

