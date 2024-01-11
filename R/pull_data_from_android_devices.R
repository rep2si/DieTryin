#' A helper function to pull .tar data objects from all connected Androids
#'
#' This is a helper function to pull data from all connected Androids, saving each version as an independent .tar file, indexed by the Android's device ID.
#' Multiple database files can be automatically merged using other functions in DieTryin.
#' 
#' @param 
#' main_directory Path to the directory containing the RICH folder.
#' @param 
#' build_tar Should a new .tar file be created on the Android? This is almost always needed, unless the user has used a third-party app to create the .tar compressed version of the RICH database, with name RICHdata.tar.
#' @param 
#' remove_tar Should the RICHdat.tar file, used to send data back to R, be deleted after the data is pulled? Recommened to keep the Android clean.
#' @param 
#' sub_directory Please supply a name for the new subdirectory where the .tar files will be saved. For example: sub_directory="Day1Backup". We recommend backing up all devices each day, and saving all copies.
#' @return 
#' This function does not return any objects.
#' @export

pull_data_from_android_devices = function(main_directory, build_tar=TRUE, remove_tar=TRUE, sub_directory=NULL){
     ############################################### Process directory info   
     current_wd = getwd()
     setwd(main_directory) # Not good to setwd in functions. Naughty Naughty. But, no other clear way to have the tar function work

    ################################################# Create a directory
      if(is.null(sub_directory)){
        stop("Please provide a valid sub-directory for saving files.")
      }

      if(dir.exists(sub_directory)){
        stop("To avoid data loss, please create an new sub-directory, by supplying an unused name.")
        } else{
      dir.create(sub_directory)
        }

    ################################################# Find Android Devices
     dev_set = find_android_devices(main_directory)
     N = dev_set$N
     nums = dev_set$nums
     devices_clean = dev_set$devices_clean
 
    ################################################# Compile tar of Build 
     Sys.sleep(1)
     message("Building payloads as a .tar files. ")  
     message("Please be patient. For large databases, this may take several minutes per device. ") 
     
     for(i in 1:N){
     Sys.sleep(2)
     if(build_tar){
      system(paste0("adb -s ", devices_clean[i], " shell tar -cvf sdcard/RICHdata.tar sdcard/RICH"))
       }

     system(paste0("adb -s ", devices_clean[i], " pull sdcard/RICHdata.tar ", paste0(sub_directory,"/RICH_Data_",devices_clean[i],".tar")))

     if(remove_tar){
      system(paste0("adb -s ", devices_clean[i], " shell rm /sdcard/RICHdata.tar"))
      }
     }

     for(i in 1:N){
     if(file.exists(paste0(sub_directory,"/RICH_Data_",devices_clean[i],".tar"))){
        message(paste0("Successfully pulled the .tar database for device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
        message("  ") 
        } else{
        warning("The .tar database could not be pulled. ")
        }
      }

   ################################################# Cleanup 
    setwd(current_wd) # restore inital wd
}
