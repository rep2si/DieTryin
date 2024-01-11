#' Deploy a file to all connected Android devices. 
#' 
#' A small function to send a file to all connected Androids. 
#' Useful for pushing the DieTryinSR and DieTryinCam APK files to new devices.
#'
#' @param 
#' main_directory Path to the directory containing the RICH folder.
#' @param 
#' file_path Path to file.
#' @return 
#' Nothing is returned by this function. However, the full RICH database is pushed up to all connected Android devices.
#' @export

push_file_to_android_devices = function(main_directory, file_path){
 ############################################### Process directory info   
     current_wd = getwd()
     setwd(main_directory) # Not good to setwd in functions. Naughty Naughty. But, no other clear way to have the tar function work

 ################################################# Find Android Devices
     dev_set = find_android_devices(main_directory)
     N = dev_set$N
     nums = dev_set$nums
     devices_clean = dev_set$devices_clean

 ################################################# Push build out to the Androids
    if(file.exists(file_path)){
   message("Pushing the file to all connected devices.")
   message("  ") 

     for(i in 1:N){
     message(paste0("Attempting to push file to device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
     system(paste0("adb -s ", devices_clean[i], " push ", file_path, " ", "/sdcard/"))
                  }
     message("  ")  
     message("The file was successfully transfered to all connected devices.")
     message("  ")  
     
     } else{
      stop("The indicated file could not be found. Please check the file path.")   
     }

   ################################################# Cleanup 
    setwd(current_wd) # restore inital wd
    }

         


