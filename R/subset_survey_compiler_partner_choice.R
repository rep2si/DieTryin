#' Build RICH subset surveys (e.g., for partner choice or PG games.)
#' 
#' This is a small helper function to create suveys (PDFs) to collect data for PGG contributions and similar games.
#'
#' @param path Path to RICH folder.
#' @param RID ID of researcher.
#' @param day Day of interview.
#' @param month Month of interview.
#' @param year Year of interview.
#' @param game_name Used to label PDF surveys.
#' @param set_size Size of set of possible alters.
#' @param token_color "color" of tokens placed. If data was entered manually, place whatever value was stored 
#'  to indiciate a tie: e.g., "1"
#' @param pattern Should "JPG" be "jpg" be used to load photos?
#' @param height Size of PDF output.
#' @param width Size of PDF output.
#' @param seed Number to use in seeding randomizer. Not applicable with choice data.
#' @param gid_size Number of characters in hashcodes for the game IDs.
#' @param condition Condition for DieTryinSR (one of "anonymous", "revealed", "optin")
#' @return A file folder, SubsetSurveys, filled with PDFs of sub-surveys to run, and a second folder, SubsetContributions, 
#'  filled with CSV files that will be used to record survey results.
#' @export
#' @examples
#' \dontrun{
#' subset_survey_compiler_partner_choice(path, pattern = ".jpg", token_color="navyblue", set_size=4, 
#'                                  height=8.5, width=11, seed=123, gid_size=4)
#' }
#'
subset_survey_compiler_partner_choice = function(path, pattern = ".jpg", token_color="navyblue", 
                                                 set_size=4, height=8.5, width=11, seed=123, 
                                                 gid_size=4, game_name="Choice",
                                                 RID="CR", day=1, month=3, year=2023, condition = "anonymous"){
    # Set random number generator seed to make repeatable game IDS
      if(!is.na(seed)){
       set.seed(seed)
       }

    # Load all partner choice data from RICH folder
      files = list.files(paste0(path, "/SubsetData"), full.names = TRUE)
      files_short = list.files(paste0(path, "/SubsetData"))

    # Loop over each CSV file, and build a PDF survey and a CSV for data entry  
      for (i in 1:length(files)) {
        bob = read.csv(files[i])

        # First get the IDs we need
        PID = bob[6,2]
        AIDs = bob$AlterID[which(bob$CoinsPlaced == token_color)]
        GID  = toupper(random_string(1, gid_size))

        if(length(AIDs) != set_size) stop("Token count does not match set_size.")

        PID2 = paste0(PID, pattern)
        AIDs2 = paste0(AIDs, pattern) 

        IDs = c(PID, AIDs)
        IDs2 = c(PID2, AIDs2)

        # Now load the photos into R
        path_imgs_small = paste0(path, "/StandardizedPhotos/")
        
        to_read = list.files(path_imgs_small, full.names=TRUE)
        to_read_short = list.files(path_imgs_small)
        
        photoset = list()

        for(j in 1:(set_size+1)){
         photoset[[j]] = imager::load.image(to_read[which(to_read_short==IDs2[j])])
        }

        ###### Now build PDF
        pdf(paste0(path, "/SubsetSurveys/", game_name, "_", PID, ".pdf"), height=height, width=width)
        par(mfrow=c(2,(set_size+1)),mar=c(0.5, 0.5, 0.2, 0.2), oma = c(2.7, 0.2, 2.7, 0.2))

        for(j in 1:(set_size+1)){
         plot(photoset[[j]], axes=FALSE)
          }

        for(j in 1:(set_size+1)){
           plot(1,1, ylab="", yaxt="n", xlab="", xaxt="n", type="n", ylim=c(0,1))
           text(1,0.75, IDs[j],cex=2.5)
           text(1,0.25, "Offer:",cex=1.75)
         }

        mtext(paste0("HHID:______ RID:_______ Day:_______ Month:_______ Year:_______ Name:_________________ Game: ", game_name," GID: ", GID),                
           side = 1,
           line = 1,
           cex=1,
           outer = TRUE)

        mtext(paste0("Game: ", game_name, "     GID: ", GID),                
           side = 3,
           line = 1,
           cex=1,
           outer = TRUE)

        dev.off()

        ###### Now Build csv
         Full_N = 8
         header = cbind(c("HHID", "RID", "Day", "Month", "Year", "Name", "ID", "Game", "Order", "Seed", "GID", "Condition"), c(c('',RID,day,month,year,''), PID, game_name , '', seed, GID, condition))
         header2 = cbind(c(paste0("Offer", c(1:(Full_N+1))),paste0("AID", c(1:(Full_N+1)))), c(rep('', Full_N +1),IDs,rep("BLANK",Full_N-set_size)))

         output = rbind(header,header2) 
         colnames(output) = c("Variable","Data")

         write.csv(output, paste0(path, "/SubsetContributions/", GID,".csv"),row.names = FALSE)
        
        ####### And parse to JSON 
         LB = length(output[,1])
         billy = c()
         for(i in 1:(LB-1)){
         billy = paste0(billy, paste0("'",output[i,1],"':'", output[i,2],"',"))
         }
         billy = paste0(billy, paste0("'",output[LB,1],"':'", output[LB,2],"'"))
         billy = paste0("{",billy,"}")

         write(billy, paste0(path, "/SubsetContributions/", GID,".json"))


       }

}




