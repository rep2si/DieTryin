#' Find all connected Android devices
#'
#' This is a helper function used to find Android devices connected to the computer, check that permissions are granted, and pull device names.
#'
#' @param 
#' main_directory Path to the directory containing the RICH folder.
#' @return 
#' A list containing basic details about the connected Android devices.
#' @export

find_android_devices = function(main_directory){
 ############################################### Process directory info   
     current_wd = getwd()
     setwd(main_directory) # Not good to setwd in functions. Naughty Naughty. But, no other clear way to have the tar function work

 ################################################# Find Android Devices
     message("Searching for Android devices with authenticated credentials.")
     devices = shell("adb devices",intern=TRUE)
     devices = devices[-length(devices)]
     devices = devices[-1]

     N = length(devices)
     nums = c(1:N)
     devices_clean = rep(NA, N)
     devices_dirty = rep(NA, N)

     for(i in 1:N){
      devices_clean[i] = gsub('\tdevice','',devices[i])  
      devices_dirty[i] = gsub('\tunauthorized','',devices[i])  
     }

     unauthenticated = grepl("\tunauthorized", devices_clean, fixed = TRUE)

     if(sum(unauthenticated)>0){
       warning("At least one Android device is not authenticated.
       Please activate developer mode, turn on USB debugging, 
       and 'Allow USB debugging' after ticking the 'Always allow  
       from this computer' checkbox.")

       for(i in 1:N){
        message(paste0("Device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
         } 
        stop("Please correct authentication issues before proceeding.") 
        }
     if(sum(unauthenticated)==0){
       message(paste0("Successfully detected ", N, " devices. "))
       for(i in 1:N){
        message(paste0("Device: ", nums[i], " of ", N,". Serial number ", devices_clean[i], "."))
        }
        message("  ")  
       }

   ################################################# Cleanup 
    setwd(current_wd) # restore inital wd
    return(list(N=N, nums=nums, devices_clean=devices_clean))
  }