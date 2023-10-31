#' Build a single reputational evaluation file for DieTryinSR in enhanced mode
#'
#' @param path Path to RICH folder.
#' @param pid ID code of participant.
#' @param aid ID of alter (about whom reputational eval is being made)
#' @param seed Number to use in seeding the randomization process if used.
#' @param gid_size Number of terms in hashcodes for the game IDs.

build_subset_rep_eval <- function (path, subdir = "SubsetRep1", pid = NULL, aid = NULL,  seed = 123, gid_size = 4, questions = c("question1", "question2", "question3")) {

  # # Set random number generator seed to make repeatable game IDS
  # if(!is.na(seed)){
  #   set.seed(seed)
  # }

  GID  = toupper(random_string(1, gid_size))

  ## Build csv
  output = cbind(
    c("RID", "TimeStamp", "ID", "GID", "AID"),
    c(NA, NA, pid, GID, aid)
  )

  n <- length(questions)
  
  # add one entry per question
  ans <- cbind(
    paste0("q", seq_len(n)),
    rep(NA, n)
  )

  # add question text (needed in app)

  text <- cbind(
    paste0("text", seq_len(n)),
    questions
  )
  
  output = rbind(output, ans, text)

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

