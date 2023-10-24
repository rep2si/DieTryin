#' Build a map between GIDs and PIDs
#' 
#' This is a small helper function to create suveys JSON files that guide the app behavior.
#'
#' @param path Path to RICH folder.
#' @param photoMode For games like the PGG or SnowDrift, where the focal persons can either give or not, set to "onlyfocal". For games were the focal
#' can give to anyone on the roster, set to "fullset".
#' @param entryMode Set to "permanent" so that data cant be changed once saved. Set to "temporary" if saved data are allowed to be overwritten.
#' @param gameMode Used to determine the mode in which DieTryinSR launches. Set to "og" for the classic layout, similar to generated pdfs. Set to "enhanced" to use interactive dictator game mode, with additional game conditions.
#' @param photoNumber The app allows up to 9 total targets when set to "threelines", and 6 targets when set to "twolines". Three people per line.
#' @param quietMode Set to "fifty" for some inspirational 50 cents quotes on start-up. Set to "dull" if you wanna be boring.
#' @export
#'
compile_android_settings = function(path, photoMode="onlyfocal", entryMode="permanent", photoNumber="twolines", gameMode = "enhanced", quietMode="fifty"){
   ################################### PGG style
         billy = c(paste0("'","photoMode","':'", photoMode,"',"))
         bob = c(paste0("'","entryMode","':'", entryMode,"',"))
         dole = c(paste0("'","photoNumber","':'", photoNumber,"',"))
         gm = c(paste0("'","gameMode","':'", gameMode,"',"))
         fifty = c(paste0("'","quietMode","':'", quietMode,"'"))

         billy = paste0("{",billy,bob,dole,gm,fifty,"}")

         print(billy)

         write(billy, paste0(path, "/SubsetContributions/GIDsByPID/", "settings.json"))
       
    
     }
     
