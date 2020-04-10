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

# Processing Time
processing_time_df <- 
  parsed_log_expanded %>%
  filter(str_detect(`3`, "^0x"),
         str_detect(`4`, "Completed")) %>%
  rename(work_unit = `1`, folding_slot = `2`, core = `3`, progress_message = `4`) %>% 
  select(log_file_name, log_timestamp, log_date, log_time, folding_slot, work_unit, core, progress_message) %>% 
  arrange(folding_slot, work_unit, log_timestamp)

processing_time_df <- 
  processing_time_df %>%
  mutate(end_flag = str_detect(progress_message, "100\\%\\)")) %>% 
  group_by(folding_slot, work_unit) %>%
  mutate(work_id = cumsum(end_flag),
         work_id = ifelse(end_flag, work_id - 1, work_id),
         work_id = paste(folding_slot, work_unit, work_id, sep = "-")) %>%
  group_by(work_id) %>% 
  mutate(previous_step_timestamp = lag(log_timestamp, 1),
         step_time_diff = log_timestamp - previous_step_timestamp,
         step_time_diff = ifelse(step_time_diff > 1000, 0, step_time_diff))

processing_time_summary <- 
  processing_time_df %>% 
  group_by(folding_slot, work_unit, work_id) %>% 
  summarise(total_processing_time = sum(step_time_diff, na.rm = TRUE) / 3600)
  
processing_time_summary %>% 
  group_by(folding_slot) %>% 
  summarise(total_processing_time = sum(total_processing_time),
            total_work_items_count = n())
# Acquired Credits
get_credits <- function(log_df){
  log_df %>%
    filter(str_detect(`3`, "Final")) %>%
    rename(credits_attributed = `3`) %>% 
    select(work_unit, folding_slot, credits_attributed, log_time, log_date, log_timestamp) %>% 
    mutate(credits_attributed = as.numeric(str_extract(credits_attributed, "\\d+")))
}

cpu_credits <- get_credits(log_cpu_df)
gpu_credits <- get_credits(log_gpu_df)

plot_credits <- function(credits_df) {
  credits_df %>% 
    ggplot() +
    geom_col(aes(log_date, credits_attributed, fill = as.character(log_time)), 
             position = "stack") +
    scale_x_date(date_breaks = "1 day", date_labels ="%a %F") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(title = paste0(unique(credits_df$folding_slot, collapse = " - "), ": Credits Acquired Per Day"), 
         subtitle = paste0(min(credits_df$log_date), " - ", max(credits_df$log_date)),
         x = "Date", y = "Credits") +
    scale_y_continuous(labels = scales::comma_format())
}

plot_credits(cpu_credits)
plot_credits(gpu_credits)

