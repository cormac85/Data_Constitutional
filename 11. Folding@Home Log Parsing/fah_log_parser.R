library(stringr)
library(dplyr)
library(rex)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)

logs_path <-  "~/../AppData/Roaming/FAHClient/logs/"

read_log_with_date <- function(log_file_name, path){
  file_path <- paste0(path, log_file_name)
  
  log_df <- tibble(message = scan(file_path, what = "character", sep = "\r")) %>% 
    filter(!str_starts(message, "\\*")) 
  log_df
}

logs <- 
  tibble(log_file_name = list.files(pattern = "*.txt", path = logs_path)) %>% 
  mutate(log_df = map(log_file_name, read_log_with_date, logs_path))


parsed_log <-
  logs %>% 
  mutate(log_date = map_chr(log_file_name, 
                            function(x) str_extract(x, "\\d+")),
         log_date = as.Date(log_date, format="%Y%m%d")) %>% 
  unnest(log_df)


parsed_log <- 
  parsed_log %>% 
  mutate(log_time = str_sub(message, 1, 8),
         message = str_sub(message, 10, 10000),
         message = str_trim(message),
         log_timestamp = lubridate::ymd_hms(paste(log_date, log_time)))

parsed_log_expanded <- 
  parsed_log %>% 
  separate(col = message, into = as.character(1:13), sep = ":")

log_errors_df <- 
  parsed_log_expanded %>% 
  filter(`1` %in% c("WARNING", "ERROR")) 

log_cpu_df <- 
  parsed_log_expanded %>% 
  filter(!`1` %in% c("WARNING", "ERROR") & `2` == "FS00") %>% 
  rename(work_unit = `1`,
         folding_slot = `2`)

log_gpu_df <- 
  parsed_log_expanded %>% 
  filter(!`1` %in% c("WARNING", "ERROR") & `2` == "FS01") %>% 
  rename(work_unit = `1`,
         folding_slot = `2`)


log_errors_df %>% mutate_all(as.factor) %>% summary()
log_gpu_df %>% mutate_all(as.factor) %>% summary()


# GPU Credits
get_credits <- function(log_df){
  log_df %>%
    filter(str_detect(`3`, "Final")) %>%
    rename(credits_attributed = `3`) %>% 
    select(work_unit, folding_slot, credits_attributed, log_time, log_date, log_timestamp) %>% 
    mutate(credits_attributed = as.numeric(str_extract(credits_attributed, "\\d+")))
  
}

cpu_credits <- get_credits(log_cpu_df)
gpu_credits <- get_credits(log_gpu_df)

cpu_credits %>% 
  ggplot() +
  geom_col(aes(log_date, credits_attributed, fill = as.character(log_time)), 
           position = "stack") +
  scale_x_date(date_breaks = "1 day", date_labels ="%a %D") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

gpu_credits %>% 
  ggplot() +
  geom_col(aes(log_date, credits_attributed, fill = as.character(log_time)), 
           position = "stack") +
  scale_x_date(date_breaks = "1 day", date_labels ="%a %D") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
