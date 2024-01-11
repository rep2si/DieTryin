#' A helper function to merge DieTryin databases collected on different devices.
#'
#' This is a helper function to merge DieTryin databases collected on different devices. The user supplies a sub-directory where a set of .tar files are stored. This
#' function then creates a single unified database, by pulling the updated data from each Android device. Checks are performed to validate the data structure.\
#'
#' @param 
#' main_directory Path to the directory containing the RICH folder.
#' @param 
#' sub_directory Which sub-directory will supply the .tar databases to be merged? 
#' @param 
#' android_device_names What are the device names of the Androids used to collect these data? Use find_android_devices(main_directory)$devices_clean to find out, if devices are connected. 
#' @param 
#' temp_directory What name should be used to hold temporary files? Doesn't really matter. 
#' @return 
#' This function dose not return any objects, but it creates a clone of the RICH database called RICH_Merged inside of main_directory.
#' @export

build_unified_database = function(main_directory, sub_directory="Day1Backup", android_device_names, temp_directory="temporary_RICH_files"){
  ############################################### Process directory info  
     N = length(android_device_names) 
     nums = c(1:N)
     current_wd = getwd()
     setwd(main_directory) # Not good to setwd in functions. Naughty Naughty. But, no other clear way to have the tar function work

     dir.create(temp_directory)

  ############################################### Begin untaring the data objects 
     message("Unpacking the compressed database files.")

     for(i in 1:N){
     message(paste0("Unpacking tarfile from device: ", nums[i], " of ", N,". Serial number ", android_device_names[i], "."))
     untar(paste0(sub_directory,"/RICH_Data_", android_device_names[i], ".tar"), exdir=paste0(temp_directory,"/", android_device_names[i]))
                  }

  ############################################### Check that database is consistent across devices
     message("Checking that database structure is identical across devices.")

     file_set = vector("list",N)

    for(i in 1:N){
     message(paste0("Loading metadata from device: ", nums[i], " of ", N,". Serial number ", android_device_names[i], "."))
     file_set[[i]] = list.files(paste0(temp_directory,"/", android_device_names[i]), recursive=TRUE)
                  }

     df_file_set = do.call(cbind,file_set)
     
     N_files = length(df_file_set[,1])    
     check_match = rep(NA, N_files)         
     
     for(m in 1:N_files){
      check_match[m] = all(df_file_set[m,]==df_file_set[m,1])    
     }
     
     if(sum(check_match)==N_files){
        message("The database structure is consistent across devices. Preparing to merge.")
     } else{
        stop("The database structure is no consistent across devices. Cannot merge. Please check your workflow!")
     }

 ############################################### Check that database is consistent across devices
   Sys.sleep(4)
   message("Building new directory structure.")
    if(dir.exists("RICH_Merged")){
     stop("To avoid data loss, merge has been canceled. The folder RICH_Merged already exists in this directory. Either move or delete.")
      } 
    dir.create("RICH_Merged")  
    copydir(paste0(temp_directory,"/", android_device_names[1],"/sdcard/RICH"),"RICH_Merged")   
    dir.create("RICH_Merged/Results")     

    for(i in 1:N){
     message(paste0("Loading metadata from device: ", nums[i], " of ", N,". Serial number ", android_device_names[i], "."))
     file_set[[i]] = list.files(paste0(temp_directory,"/", android_device_names[i]), full.names=TRUE, recursive=TRUE)
                  }  
    
    Sys.sleep(4)
    message("Merging the data. This may take a few minutes.")

    for(k in 1:N_files){
       # Step 1. Any file that is not JSON, just move from the first device folder to the merged folder.
       if(substrRight(file_set[[1]][k],4) != "json"){
        file.copy(file_set[[1]][k], paste0(main_directory,"/","RICH_Merged/",sub('.*/RICH/', '', file_set[[1]][k])))
       }

       # Step 2. If file IS json, then check the character count of all versions.
       if(substrRight(file_set[[1]][k],4) == "json"){
           
           # Check how long the JSON is
           scrap = rep(NA, N)
           for(i in 1:N){       
            scrap[i] = nchar(readr::read_file(file_set[[i]][k]))
           }
            
           # If all objects are equal in length, then just copy file from device 1. 
           if(all(scrap==scrap[1])){
            file.copy(file_set[[1]][k], paste0(main_directory,"/","RICH_Merged/",sub('.*/RICH/', '', file_set[[1]][k])))
            } else{
           # Otherwise, we need to check if multiple are different, if so, then there is a collision. Otherwise, we can pass the files from the correct device.
             if(max(table(scrap)) != (N-1)){
              stop("Error cannot merge databases! Multiple different versions of a file were found. Please check that the same respodent did not provide interviews on different devices.")
             }else{
              t_scrap = table(scrap)
              updated_index = which(scrap==names(which(t_scrap==1)))
              file.copy(file_set[[updated_index]][k], paste0(main_directory,"/","RICH_Merged/",sub('.*/RICH/', '', file_set[[updated_index]][k])))
             }
           }

       }
         message(paste0("Merged file ", k, " of ", N_files,"."))
    }

  message("File shuffling completed. Now checking for consistency.")
    check_match = rep(NA, N_files)
    file_set_old = list.files(paste0(temp_directory,"/", android_device_names[1]), recursive=TRUE)
    file_set_new = list.files(paste0(main_directory,"/RICH_Merged"), recursive=TRUE)

    file_set_old_lean = sub('.*/RICH/', '', file_set_old)

    if(length(file_set_old_lean) != N_files){
        stop("The database structure is corrupted. Merge failed! Please check your workflow!")
    } 

    if(length(file_set_new) != N_files){
        stop("The database structure is corrupted. Merge failed! Please check your workflow!")
    } 

    for(k in 1:N_files){
      check_match[k] = ifelse(file_set_old_lean[k] == file_set_new[k], 1, 0)
    }

    if(sum(check_match)==N_files){
        message("Congratulations. The databases could be merged successfully.")  
    } else{
        stop("The database structure is corrupted. Merge failed! Please check your workflow!")
    }

   ################################################# Cleanup 
   message("Deleting temporary files.")      
   unlink(temp_directory,recursive = TRUE) # Removes temp files
   setwd(current_wd) # restore inital wd
}
