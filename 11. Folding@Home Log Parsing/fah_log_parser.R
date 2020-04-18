library(stringr)
library(dplyr)
library(rex)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

logs_path <-  "~/../AppData/Roaming/FAHClient/logs/old_logs/"
backup_folder_name <- "old_logs"

read_log_with_date <- function(log_file_name, path){
  file_path <- paste0(path, log_file_name)
  
  log_df <- tibble(message = scan(file_path, what = "character", sep = "\r")) %>% 
    filter(!str_starts(message, "\\*")) 
  log_df
}


# This logic at the start should backup the current "active" folder to "old_logs" 
# and then read from the full set of files in the "old_logs" folder.

# Backup Logs
# if(!isTRUE(file.info(paste0(logs_path, backup_folder_name))$isdir))
#   dir.create(paste0(logs_path, backup_folder_name))
# 
# file.copy(paste0(logs_path,  list.files(pattern = "*.txt", path = logs_path)),
#           paste0(logs_path, backup_folder_name,  "/",  list.files(pattern = "*.txt", path = logs_path)))

# Read logs & do basic parsing
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

# Split logs for various uses.
# Each subset of rows in the logs have very different formats so
# we need to split them up into more self-similar groups that can
# then be analysed.
log_errors_df <- 
  parsed_log_expanded %>% 
  filter(`1` %in% c("WARNING", "ERROR")) 

log_work_units_df <- 
  parsed_log_expanded %>% 
  filter(!`1` %in% c("WARNING", "ERROR") & str_detect(`2`, "FS\\d\\d")) %>% 
  rename(work_unit = `1`,
         folding_slot = `2`)

log_errors_df %>% mutate_all(as.factor) %>% summary()
log_work_units_df %>% mutate_all(as.factor) %>% summary()

# Processing Time
add_processing_time_cols <- function(parsed_log){
  processing_time_df <- 
    parsed_log %>%
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
  
  processing_time_df
}

get_processing_time_summary <- function(parsed_log) {
  processing_time_summary <- 
    parsed_log %>% 
    add_processing_time_cols() %>% 
    group_by(folding_slot, work_unit, work_id) %>% 
    summarise(total_processing_time = sum(step_time_diff, na.rm = TRUE) / 3600)
  
  processing_time_summary
}

get_total_log_duration <- function(parsed_log) {
  parsed_log %>% 
    group_by(log_file_name) %>% 
    summarise(start_log = min(log_timestamp),
              end_log = max(log_timestamp)) %>% 
    mutate(log_duration = (end_log - start_log)) %>% 
    ungroup() %>% 
    summarise(total_log_duration = sum(log_duration)) %>% 
    pull(total_log_duration) %>% 
    (function(x) as.integer(x) / 3600)
}

processing_time_summary <- get_processing_time_summary(parsed_log_expanded)
total_log_duration <- get_total_log_duration(parsed_log_expanded)

processing_time_summary %>% 
  group_by(folding_slot) %>% 
  summarise(total_processing_time = sum(total_processing_time),
            total_work_items_count = n()) %>% 
  mutate(proportion_time_processing = total_processing_time / total_log_duration)

processing_time_summary %>%
  ggplot(aes(total_processing_time)) +
  geom_density(size = 1.2) + 
  geom_vline(xintercept = median(processing_time_summary$total_processing_time), colour = "red") +
  theme_minimal() +
  labs(title = "Density Plot of Work Unit Processing Time",
       y = "Density", x = "Work Unit Processing Time (hours)")

# Acquired Credits
get_credits <- function(log_df){
  log_df %>%
    filter(str_detect(`3`, "Final")) %>%
    rename(credits_attributed = `3`) %>% 
    select(work_unit, folding_slot, credits_attributed, log_time, log_date, log_timestamp) %>% 
    mutate(credits_attributed = as.numeric(str_extract(credits_attributed, "\\d+")))
}

plot_credits <- function(credits_df) {
  credits_df %>% 
    ggplot() +
    geom_col(aes(log_date, credits_attributed, fill = as.character(log_time)), 
             position = "stack") +
    theme_minimal() +
    scale_x_date(date_breaks = "1 day", date_labels ="%a %F") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(title = "Credits Acquired Per Day", 
         subtitle = paste0(min(credits_df$log_date), " - ", max(credits_df$log_date)),
         x = "Date", y = "Credits") +
    scale_y_continuous(labels = scales::comma_format()) +
    facet_wrap(~folding_slot, ncol=1)
}


credits <- get_credits(log_work_units_df)

credits %>% 
  plot_credits()

credits %>% 
  filter(folding_slot == "FS01") %>% 
  plot_credits()


# Credits per Hour
folding_slot_summary <- 
  credits %>% 
  group_by(folding_slot) %>% 
  summarise(credits_attributed = sum(credits_attributed)) %>%
  left_join(
    processing_time_summary %>% 
      group_by(folding_slot) %>% 
      summarise(total_processing_time = sum(total_processing_time),
                total_work_items_count = n()) %>% 
      mutate(proportion_time_processing = total_processing_time / total_log_duration),
    by = "folding_slot"
    ) %>% 
  mutate(credits_per_processing_hour = credits_attributed / total_processing_time,
         credits_per_log_hour = credits_attributed / total_log_duration)

folding_slot_summary

# Network Usage

get_network_usage <- function(work_units_df) {
  work_units_df %>% 
    filter(str_detect(`3`, "Downloading")) %>% 
    rename(usage_mib = `3`) %>% 
    select(log_file_name, folding_slot, work_unit, log_timestamp, log_date, log_time, usage_mib) %>% 
    mutate(usage_mib = str_extract(usage_mib, "(\\d+).(\\d+)"),
           network_direction = "download") %>% 
    union_all(
      work_units_df %>% 
        filter(str_detect(`3`, "Uploading")) %>% 
        rename(usage_mib = `3`) %>% 
        select(log_file_name, folding_slot, work_unit, log_timestamp, log_date, log_time, usage_mib) %>% 
        mutate(usage_mib = str_extract(usage_mib, "(\\d+).(\\d+)"),
               network_direction = "upload")
    ) %>% 
    mutate(usage_mib = as.numeric(usage_mib))  
}

calculate_daily_network_usage <- function(network_usage_df) {
  network_usage_df %>% 
    group_by(log_date, folding_slot, network_direction) %>% 
    summarise(total_usage_mib = sum(usage_mib)) %>% 
    ungroup() %>% 
    complete(folding_slot, 
             network_direction,
             log_date = seq.Date(min(log_date), max(log_date), by = "day"), 
             fill = list(total_usage_mib = 0)) %>% 
    arrange(log_date)
}

network_usage_df <- get_network_usage(log_work_units_df)

network_usage_df %>% 
  group_by(folding_slot, network_direction) %>% 
  summarise(total_usage_mib = sum(usage_mib),
            median_usage = median(usage_mib),
            usage_quantile_25 = quantile(usage_mib, 0.25),
            usage_quantile_75 = quantile(usage_mib, 0.75),
            max_usage = max(usage_mib)) %>% 
  arrange(network_direction)

network_usage_daily_summary <- calculate_daily_network_usage(network_usage_df) 

network_usage_daily_summary %>% 
  ggplot(aes(log_date, total_usage_mib / 1024, fill = network_direction)) +
  geom_col() + 
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Network Usage per Day",
       x = "Date", y = "Cumulative Usage (GiB)")

total_usage_gib <- sum(network_usage_daily_summary$total_usage_mib) / 1024

cumulative_plot_by_slot <- 
  network_usage_daily_summary %>% 
  arrange(folding_slot, network_direction, log_date) %>% 
  group_by(folding_slot, network_direction) %>% 
  mutate(cumulative_usage_mib = cumsum(total_usage_mib)) %>%
  ggplot(aes(log_date, cumulative_usage_mib / 1024, fill = network_direction)) +
  geom_col() + 
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~folding_slot, ncol = 1) +
  labs(title = "Cumulative Network Usage by Folding Slot",
       x = "Date", y = "Cumulative Usage (GiB)") +
  ylim(c(0, total_usage_gib))

cumulative_plot <- 
  network_usage_daily_summary %>% 
  arrange(folding_slot, network_direction, log_date) %>% 
  group_by(folding_slot, network_direction) %>% 
  mutate(cumulative_usage_mib = cumsum(total_usage_mib))  %>%
  group_by(log_date) %>% 
  mutate(cumulative_usage_mib = sum(cumulative_usage_mib)) %>% 
  ggplot(aes(log_date, cumulative_usage_mib / 1024)) +
  geom_line(colour = RColorBrewer::brewer.pal(3, "Set2")[3], size = 1) + 
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Total Cumulative Network Usage", 
       subtitle = "Upload + Download",
       x = "Date", y = "Cumulative Usage (GiB)") +
  ylim(c(0, total_usage_gib))


gridExtra::grid.arrange(cumulative_plot_by_slot, cumulative_plot,
                        heights=c(2,1))

# IP Address Lookup
connections_df <- 
  log_work_units_df %>% 
  filter(str_detect(`3`, "Connecting")) %>% 
  rename(ip_address = `3`) %>% 
  select(log_file_name, log_timestamp, log_date, log_time, folding_slot, work_unit, ip_address) %>% 
  mutate(ip_address = str_extract(ip_address, "(\\d+).(\\d+).(\\d+).(\\d+)"))


ip_address_df <- tibble(ip_address = unique(connections_df$ip_address))

get_from_ip_api <- function(ip_addr) {
  api_url <- "http://ip-api.com/json/"
  api_query <- paste0(api_url, ip_addr)
  httr::GET(api_query) %>% 
    httr::content() %>% 
    unlist() %>% 
    tibble::enframe() %>%
    tidyr::pivot_wider() %>% 
    janitor::clean_names()
}

ip_address_df <- 
  ip_address_df %>% 
  mutate(ip_address_data = map(ip_address, get_from_ip_api)) %>% 
  unnest(cols = c(ip_address_data))

ip_address_df %>% 
  group_by(country, city, org, isp) %>% 
  summarise(connection_count = n()) %>% 
  arrange(desc(connection_count))

