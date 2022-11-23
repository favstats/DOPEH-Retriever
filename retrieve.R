# Setup -----------------------------------------------------------------	


# Loading Packages	
pacman::p_load(	
  # tidyverse,	
  dplyr,
  httr,	
  glue,	
  rvest,	
  jsonlite,
  futile.logger,
  utils,
  here
)	



## fabio dirs
setwd(here::here())
datadir <- here::here("data")
scriptdir <- here::here("rscripts")
# Packages that should be installed but not loaded: 
# av, digest, OpenImageR;


# load functions separately	
source(glue("{scriptdir}/99_utils.R"))	

outputexample <- 1103135646905363 %>%
  get_ad_snapshots()

print(outputexample)


# 
# # I. Parsing Media URLS and downloading images & video --------------------------------------------------------------	
# # examplify code	
# if(F){	
#   ad_id <- "1103135646905363"
#   # df_imp %>%	
#   # filter(page_name == "Australian Labor Party") %>%	
#   # sample_n(1) %>%	
#   # pull(id)	
#   browseURL(glue("https://www.facebook.com/ads/library/?id={ad_id}")) # to browse the specific ad	
#   debugonce(get_ad_snapshots)
#   get_ad_snapshots("1103135646905363")
# }
# 
# 
# # Load some exemplary ad ids (these we got directely from the ad library)	
# # filename <- "AU_2022-02-21_2022-05-31.rds"
# filenames <- dir(glue("{datadir}/page_ids/")) %>%
#   discard(~str_detect(.x, "media|04-01_2022"))
# 
# 
# 
# filenames %>% 
#   # filenames %>% 
#   walk(~{
#     filename <- .x
#     df_imp <- read_rds(glue("{datadir}/page_ids/{filename}"))	
#     
#     # assess which data has been downloaded already	
#     # F: I think this isn't necessary since we get downloaded from below?
#     # downloaded <- read_csv(glue("{datadir}/media/hash_table.csv")) %>% 
#     #   pull(ad_id) %>% str_remove_all("adid_|_.*") %>% unique()
#     
#     # clear hash table
#     # file.remove("{datadir}/media/hash_table.csv")
#     
#     # Download Media Files -------------------------------------------------
#     # reset media_dat meta-data file
#     media_dat_filepath <- glue("{datadir}/page_ids/media_dat_{filename}")
#     
#     anew <- !file.exists(media_dat_filepath)
#     if(!anew){
#       # F: public file of already downloaded ad ids
#       downloaded <- read_lines("already_there.txt")
#       
#     } else {
#       downloaded <- c()
#       write_rds(tibble(), media_dat_filepath)
#     }
#     
#     temp <- df_imp %>%
#       tidylog::filter(!id %in% downloaded) %>%
#       sample_n(nrow(.)) %>%
#       # slice(1:10) %>%
#       # tidylog::slice(1:10000) %>%
#       mutate(
#         rowid = 1:nrow(.),
#         maxrid = max(rowid)
#       ) %>% 
#       select(id, rowid, maxrid)
#     
#     temp %>%
#       split(1:nrow(.)) %>%
#       map_dfr_progress(~{ # this version ads a progress bar;
#         # map_dfr(~{
#         # cat("\n")
#         # print(.x$id)
#         # print(.x$rowid)
#         
#         # TODO have alternative actions in cases of warnings or errors (i.e. print the ad id)
#         # try({
#         media_dat_row <<- #%>% 
#           retry(get_ad_snapshots(.x$id, download = T, hashing = T, mediadir = glue("{datadir}/media")), maxErrors = 100, sleep = 60*30) %>% 
#           dplyr::mutate_if(is.numeric, as.character) %>% 
#           dplyr::mutate_if(is.data.frame, list)  %>% 
#           mutate(rowid = .x$rowid)
#         # print(media_dat_row)
#         if(.x$rowid == 1) {
#           media_dat <<- media_dat_row 
#         } else {
#           media_dat <<- media_dat %>% bind_rows(media_dat_row)
#         }
#         
#         # all 10000 rows and in the last row, bind together and save to hardware:
#         if((.x$rowid %% 1000)== 0 | .x$rowid == .x$maxrid){
#           # print("Reached checkpoint. Caching the data...")
#           
#           to_bind <- dir(glue::glue("{datadir}/unbinding"), full.names = T) 
#           
#           media_dat_orig <- read_rds(media_dat_filepath)
#           
#           ## save data
#           intern <- to_bind %>% 
#             map(read_rds) %>% 
#             data.table::rbindlist(use.names = T, fill = T) 
#           
#           media_dat_new <- data.table::rbindlist(
#             list(intern, media_dat_orig, media_dat), 
#             use.names = T, fill = T
#           ) %>% 
#             tidylog::distinct() %>% 
#             as_tibble()
#           
#           write_rds(media_dat_new, media_dat_filepath)
#           
#           # write the already downloaded ids
#           already_there <- read_lines("already_there.txt")
#           write_lines(unique(c(media_dat_new$id, already_there)), file = "already_there.txt")
#           
#           ## remove the to be bound data
#           to_bind %>% walk(file.remove)
#           
#           # print(glue("{.x$rowid} ads have been processed 🎉"))
#           
#         } else  if((.x$rowid %% 50)== 0) {
#           # all 50 rows, save to hardware:
#           
#           create_necessary_dirs(glue::glue("{datadir}/unbinding"))
#           
#           write_rds(media_dat, glue("{datadir}/unbinding/{Sys.time()}_{min(media_dat$rowid)}_{max(media_dat$rowid)}_{filename}"))
#           
#           # write the already downloaded ids
#           write_lines(media_dat$id, file = "already_there.txt", append = T, sep = "\n")
#           
#           # reset media dat
#           media_dat <<- tibble()
#           
#           # media_dat_orig <- read_rds(media_dat_filepath)
#           # bind_rows(media_dat_orig, media_dat) %>% 
#           #   write_rds(media_dat_filepath)
#           (beepr::beep(12))
#           # print(glue("{.x$rowid} ads have been processed 🎉"))
#         }
#         # }) # try one
#       }); beepr::beep(10)
#     
#   })




# #debug
# if(F){
#   ttemp <- 1103135646905363 %>% 
#   get_ad_snapshots(download = T, hashing = T, mediadir = glue("{datadir}/media"))
# }
# 
# 
# ttemp %>% str()
# 
# # Save meta data ----------------------------------------------------------
# df_merged <- left_join(media_dat, df_imp)
# write_rds(df_merged, glue("{datadir}/page_ids/mediadf_{filename}"))
# 
# 
# 
# 
# 
# 
# # Experimental bits -------------------------------------------------------
#   ## experiments
# yo <- media_dat %>% 
#   # filter(id == "680975262989752")
#   # filter(lengths(cards)!=0) %>% 
#   slice(1) #%>% 
#   # select(cards, images, videos) #%>% 
#   # slice(7)
# 
# 
# # debugonce(extract_media_urls)
# # debugonce(download_media_int)
# download_media(yo)
# 
# # 3. Separate sound from video -----
# # 4. Slicing Video into snapshots -----
# 
# 
# 
# # 5. Sound of video -----
# # https://cloud.google.com/speech-to-text
# # Convert it into text
# # Detect the pitch of sound
# 
# 
# 
# # 6. Get only political pages ----
# df_merged$effective_authorization_category %>% table()
# 
# #
# library(inops)
# df_merged$page_categories %>% 
#   lapply(`[[`, 1) %>% unlist() %>% 
#   table() %>% as_tibble() %>% 
#   rename(., page_category = `.`) %>% 
#   mutate(
#     page_category = fct_reorder(page_category, n)
#     ) %>% 
#   ggplot(aes(x = page_category, y = n)) +
#   geom_col() +
#   theme_minimal() +
#   gghighlight::gghighlight(as.character(page_category) %in~% "Politic") + 
#   coord_flip() + 
#   labs(
#     x = "meta page category",
#     y = "number of entries"
#     )
# 
# # Create a selection for annotation with Annotinder
# media_dat$page_categories %>% 
#   lapply(`[[`, 1)
# 
# df_merged %>% 
#   rowwise() %>% 
#   mutate(
#     page_category = page_categories %>% unlist() %>% paste0(collapse=" | "),
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     is_political = str_detect(page_category,"Pol")
#   ) %>% 
#   filter(is_political) %>% 
#   pull(page_name)

