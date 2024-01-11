#' Deploy a DieTryin datebase to all connected Android devices. 
#'
#' This is a helper function that will push a created JSON database to all connected Android phones. 
#' Later, the data from each Android can be merged to a single unified database.
#' 
#' @param 
#' main_directory Path to the directory containing the RICH folder.
#' @param 
#' build_tar Boolean. Does the database need to be compressed with tar? Set to TRUE unless a valid tar object already exists.
#' @param 
#' auto_untar Boolean. Should DieTryin initiate tar decompression upon uploading to each device? Its simplest to set to TRUE. However, it may be faster to set 'auto_untar=FALSE' here and untar directly in the Android app using the RAR GUI.
#' @param 
#' remove_tar Boolean. Should the temporary .tar file be removed after the database is decompressed. It is best to keep the Androids clean, and set to TRUE.
#' @return 
#' Nothing is returned by this function. However, the full RICH database is pushed up to all connected Android devices.
#' @export

deploy_to_android_devices = function(main_directory, build_tar=TRUE, auto_untar=TRUE, remove_tar=TRUE){
 ############################################### Process directory info   
     current_wd = getwd()
     setwd(main_directory) # Not good to setwd in functions. Naughty Naughty. But, no other clear way to have the tar function work

 ################################################# Compile tar of Build 
  if(build_tar){
     Sys.sleep(1)
     message("Building payload as a .tar file. ")  
     message("Please be patient. For large databases, this may take several minutes. ") 

     Sys.sleep(3)
     files2tar = dir("RICH", full.names = TRUE, recursive=TRUE)
     ends_csv = grepl(".csv", files2tar, fixed = TRUE)
     utils::tar(tarfile = "DieTryinPayload.tar", files = files2tar[!ends_csv])

     if(file.exists("DieTryinPayload.tar")){
        message("Successfully built the .tar database. ")
        message("  ") 
     } else{
        warning("The .tar database could not be built. ")
     }
   }

 ################################################# Find Android Devices
     dev_set = find_android_devices(main_directory)
     N = dev_set$N
     nums = dev_set$nums
     devices_clean = dev_set$devices_clean

 ################################################# Push build out to the Androids
    if(file.exists("DieTryinPayload.tar")){
   message("Pushing the .tar payload to all connected devices.")
   message("  ") 

     for(i in 1:N){
     message(paste0("Attempting to push payload to device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
     system(paste0("adb -s ", devices_clean[i], " push DieTryinPayload.tar /sdcard/DieTryinPayload.tar"))
                  }
     message("  ")  
     message("The payload was successfully transfered to all connected devices.")
     message("  ")  
     
     } else{
      stop("The .tar payload could not be found. Please set 'build_tar=TRUE'.")   
     }
  ################################################# Untar the to the Androids  
    if(auto_untar){
    message("Preparing to untar the payload.")
    message("Please be patient. For large databases, this may take several minutes per device. ") 
    message("It may be faster to set 'auto_untar=FALSE' here and untar directly in the Android app using RAR.") 
    message("The RAR app, by RARLAB, is avaible for free on the play store.") 
    message("  ") 
    Sys.sleep(3)

     for(i in 1:N){
     message(paste0("Attempting to untar payload on device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
     system(paste0("adb -s ", devices_clean[i], " shell tar -xf /sdcard/DieTryinPayload.tar -C /sdcard"))
                  }
     message("  ")  
     message("The payload has been successfully unpacked across all connected devices.")

     if(remove_tar){
      for(i in 1:N){
     message(paste0("Attempting to delete tarfile on device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
     system(paste0("adb -s ", devices_clean[i], " shell rm /sdcard/DieTryinPayload.tar"))
                  }
      message("  ")  
      message("Temporary .tar file deleted successfully.")


     }

     message("Goodluck with the RICH games.")
     message("  ")  
     }

   ################################################# Cleanup 
    setwd(current_wd) # restore inital wd
    }