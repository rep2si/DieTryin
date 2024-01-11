#' A helper function to clean the Androids of database files
#'
#' This is a helper function to clean up the Androids. Three kinds of files can be removed. The payload "DieTryinPayload.tar" file used to send the database to each Android. The database directory itself. And, the "RICHdata.tar" file, used to send data back to R.
#' Be careful when using, as data loss is permanent. The user must set the arguments purposefully. A second set of prompts is used to comfirm, before deletion occurs.
#' @param 
#' main_directory Path to the directory containing the RICH folder.
#' @param 
#' wipe_RICH Should the RICH database directory be deleted? Be very careful with this one! Data is permanently deleted, and this may be the only copy!
#' @param 
#' wipe_tar_payload Should the payload DieTryinPayload.tar file, used to send the database to each Android, be deleted?
#' @param 
#' wipe_tar_database Should the RICHdat.tar file, used to send data back to R, be deleted?
#' @return 
#' This function does not return any objects.
#' @export

wipe_RICH_files_from_android_devices = function(main_directory, wipe_RICH=FALSE, wipe_tar_payload=FALSE, wipe_tar_database=FALSE){
 ############################################### Process directory info   
     current_wd = getwd()
     setwd(main_directory) # Not good to setwd in functions. Naughty Naughty. But, no other clear way to have the tar function work

 ################################################# Find Android Devices
     dev_set = find_android_devices(main_directory)
     N = dev_set$N
     nums = dev_set$nums
     devices_clean = dev_set$devices_clean

    ################################################# Wipe Data from Android Devices
    if(wipe_RICH){
       checker =  menu(c("Yes", "No"), title="Are you sure you want to wipe the entire RICH directory? Data will be permanantly deleted.")
        if(checker==1){
      for(i in 1:N){
     message(paste0("Attempting to delete RICH folder on device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
     system(paste0("adb -s ", devices_clean[i], " shell rm -rfv /sdcard/RICH"))
                  }
      message("  ")  
      message("RICH folders deleted successfully.")
     }
     }

    if(wipe_tar_payload){
       checker =  menu(c("Yes", "No"), title="Are you sure you want to wipe the .tar payload file? Data will be permanantly deleted.")
        if(checker==1){
      for(i in 1:N){
     message(paste0("Attempting to delete tarfile on device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
     system(paste0("adb -s ", devices_clean[i], " shell rm /sdcard/DieTryinPayload.tar"))
                  }
      message("  ")  
      message("Temporary .tar file deleted successfully.")
     }
     }

    if(wipe_tar_database){
       checker =  menu(c("Yes", "No"), title="Are you sure you want to wipe the .tar database file? Data will be permanantly deleted.")
        if(checker==1){
      for(i in 1:N){
     message(paste0("Attempting to delete tarfile on device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
     system(paste0("adb -s ", devices_clean[i], " shell rm /sdcard/RICHdata.tar"))
                  }
      message("  ")  
      message("Temporary .tar file deleted successfully.")
     }
     }

   ################################################# Cleanup 
    setwd(current_wd) # restore inital wd
}