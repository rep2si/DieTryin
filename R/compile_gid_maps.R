#' Build a map between GIDs and PIDs
#'
#' This is a small helper function to create suveys JSON files that guide the app behavior.
#'
#' @param path Path to RICH folder.
#' @param mode For games like the PGG or SnowDrift, where the focal persons can either give or not, set to "onlyfocal". For games were the focal
#' @param what What to compile gid_maps for. Only useful for "enhanced" moded. Set to "contributions" to compile files in "SubsetContributions" folder (e.g. when using og mode, or for collecting giver data in enhanced mode). Set to "payouts" to compile files in the "SubsetPayouts" folder (when collecting payout data in enhanced mode).
#' can give to anyone on the roster, set to "fullset".
#' @export
#'
compile_gid_maps = function(path, what = "contributions", mode = "onlyfocal", subdir = "SubsetPayouts", sort_by = FALSE, sort_levels = NA, sort_randomise_levels = "false", anon_alter_first = FALSE, anon_alter_midway = FALSE) {
        if (what == "contributions") {
                ################################### PGG style
                if (mode == "onlyfocal") {
                        ## Now list games by PID
                        all_ds = list.files(paste0(path, "/SubsetContributions/"), pattern = ".csv", full.names = TRUE)
                        res_list = list()
                        Full_N = 8

                        for (i in 1:length(all_ds)) {
                                res1 = read.csv(all_ds[i])
                                res1b = res1[which(res1[, 1] %in% paste0("AID", c(1:(Full_N + 1)))), ]
                                res1b$Variable = rep(res1[which(res1[, 1] == "GID"), 2], Full_N + 1)
                                res_list[[i]] = res1b
                        }

                        res_all = do.call(rbind, res_list)
                        res_all = res_all[which(res_all$Data != "BLANK"), ]

                        ## Write json files
                        all_ids = tools::file_path_sans_ext(list.files(paste0(path, "/StandardizedPhotos/")))

                        for (i in 1:length(all_ids)) {
                                ####### And parse to JSON
                                gids_2_write = res_all$Variable[which(res_all$Data == all_ids[i])]
                                LB = length(gids_2_write)
                                billy = c(paste0("'", "Ngames", "':'", LB, "',"))
                                for (j in 1:(LB - 1)) {
                                        billy = paste0(billy, paste0("'", paste0("GIDx", j), "':'", gids_2_write[j], "',"))
                                }
                                billy = paste0(billy, paste0("'", paste0("GIDx", LB), "':'", gids_2_write[LB], "'"))
                                billy = paste0("{", billy, "}")

                                write(billy, paste0(path, "/SubsetContributions/GIDsByPID/", all_ids[i], ".json"))
                        }
                }

                ################################### RICH style
                if (mode == "fullset") {
                        ## Now list games by PID
                        all_ds = list.files(paste0(path, "/SubsetContributions/"), pattern = ".csv", full.names = TRUE)
                        res_list = list()
                        Full_N = 8

                        for (i in 1:length(all_ds)) {
                                res1 = read.csv(all_ds[i])
                                res1b = res1[which(res1[, 1] %in% paste0("AID", c(1:(Full_N + 1)))), ]
                                res1b$Variable = rep(res1[which(res1[, 1] == "GID"), 2], Full_N + 1)
                                res1b$Focal = rep(res1[which(res1[, 1] == "ID"), 2], Full_N + 1)
                                res_list[[i]] = res1b
                        }

                        res_all = do.call(rbind, res_list)
                        res_all = res_all[which(res_all$Data != "BLANK"), ]

                        res_all = res_all[which(res_all$Focal == res_all$Data), ]

                        ## Write json files
                        all_ids = tools::file_path_sans_ext(list.files(paste0(path, "/StandardizedPhotos/")))

                        for (i in 1:length(all_ids)) {
                                ####### And parse to JSON
                                gids_2_write = res_all$Variable[which(res_all$Data == all_ids[i])]
                                LB = length(gids_2_write)

                                billy = c(paste0("'", "Ngames", "':'", LB, "',"))
                                for (j in 1:(LB - 1)) {
                                        billy = paste0(billy, paste0("'", paste0("GIDx", j), "':'", gids_2_write[j], "',"))
                                }
                                billy = paste0(billy, paste0("'", paste0("GIDx", LB), "':'", gids_2_write[LB], "'"))
                                billy = paste0("{", billy, "}")

                                write(billy, paste0(path, "/SubsetContributions/GIDsByPID/", all_ids[i], ".json"))
                        }
                }
        } else if (what == "other") {

          # I can't be bothered to parse Cody's code, so I'm doing this from scratch.
          all_ds <- list.files(paste0(path, "/", subdir, "/"), pattern = ".csv", full.names = TRUE)

          all_gids <- vector("list", length(all_ds))

          # populate list with player ids and corresponding game ids
          for (i in seq_len((length(all_ds)))) {
            game <- read.csv(all_ds[i])
            id <- game[which(game[, 1] %in% "ID"), 2]
            gid <- game[which(game[, 1] %in% "GID"), 2]
            anon_alter <- game[which(game[, 1] %in% "anonymousAlter"), 2] 
            # base data frame: id, gid
            all_gids[[i]] <- data.frame(id, gid, anon_alter)         
            if (!sort_by == FALSE) {
                    # add sorter
                    all_gids[[i]]$sorter <- game[which(game[, 1] %in% sort_by), 2] # add sorter
            }
          }

          # crunch me good
          d_gids <- do.call("rbind", all_gids)

          gid_dir <- paste0(path, "/", subdir, "/GIDsByPID")
          if (!dir.exists(gid_dir)) {
              dir.create(gid_dir)
          }

          all_ids <- unique(d_gids[, 1])

          if (sort_randomise_levels == "balance") {
            all_ids <- sample(all_ids)
          }

          track_order <- vector("list", length(all_ids))

          for (i in seq_along(all_ids)) {
            id <- all_ids[i]
            # df of gids for this id
            d <- d_gids[d_gids$id == id, ] 
            # first randomise order completely
            d <- d[sample(nrow(d)),]
            # then sort by sorter
            if (!sort_by == FALSE) {
              if (sort_randomise_levels == "random") {
                sorting_order <- sample(sort_levels)
                d$sorter <- ordered(d$sorter, levels = sorting_order) # make ordered factor
              } else if (sort_randomise_levels == "balance") { ## ONLY WORKS WITH TWO CONDITIONS, FOR NOW
                if (i < (length(all_ids) / 2)) {
                  sorting_order <- sort_levels
                  d$sorter <- ordered(d$sorter, levels = sorting_order) # make ordered factor
                } else {
                  sorting_order <- rev(sort_levels)
                  d$sorter <- ordered(d$sorter, levels = sorting_order) # make ordered factor
                }
              } else {
                sorting_order <- sort_levels
                d$sorter <- ordered(d$sorter, levels = sort_levels) # make ordered factor
              }
              if (anon_alter_first) {
                d$anon_alter <- ordered(d$anon_alter, levels = c("true", "false")) # make ordered factor
                d <- d[order(d$sorter, d$anon_alter), ] # sort
              } else if (anon_alter_midway) {
                #subset two dfs, both ordered by "sorter"
                d_anon_alter <- d[d$anon_alter == "true",]
                d_anon_alter <- d_anon_alter[order(d_anon_alter$sorter), ] # sort
                d_known_alter <- d[d$anon_alter == "false",]
                d_known_alter <- d_known_alter[order(d_known_alter$sorter), ] # sort
                n_known_per_sorter_level <- nrow(d_known_alter) / length(sort_levels)
                # Only implementing for 2 sort_levels for now
                h <- floor(n_known_per_sorter_level / 2)
                t <- n_known_per_sorter_level - h
                l <- list()
                start <- 1
                for(j in seq_along(sort_levels)) {
                  #It's 1.30am in the field. This works with two sorter levels but is untested with more
                  start <- start + (j - 1) * ((n_known_per_sorter_level + nrow(d_anon_alter) / 2) - 1)
                  l[[j]] <- rbind(
                    d_known_alter[start:(start+h-1),],
                    d_anon_alter[j,], # will only work for one unknown alter per condition 
                    d_known_alter[(start+h):(start+h+t-1),]
                  )
                }
                d <- do.call(rbind,l)
              } else {
              d <- d[order(d$sorter), ] # sort
              }
            }
            gids <- d[, 2]
            Ngames <- length(gids)
            filename <- paste0(path, "/", subdir, "/GIDsByPID/", id, ".json")
            content <- paste0('{"Ngames":"',Ngames,'"')
            for (j in seq_len(Ngames)) {
              content <- paste0(content, ',"GIDx', j, '":"', gids[j],'"')
            }
            content <- paste0(content,'}')
            write(content, filename)
            if (!sort_by == FALSE) {
                    track_order[[i]] <- c(id = id, order = paste(sorting_order, collapse = ","))
            }
          }
          if (!sort_by == FALSE) {
                  df_order <- do.call(rbind, track_order)
                  write.csv(df_order, paste0(path, "/", subdir, "/GIDsByPID/", "ORDER.csv"))
                  }
          }
        }

