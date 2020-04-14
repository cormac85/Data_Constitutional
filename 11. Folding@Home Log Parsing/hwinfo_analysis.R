library(tidyverse)

fah_hwinfo_log <- readr::read_csv("~/PC/fah_run_1.CSV") %>% as.data.frame() %>% as_tibble()

cpu_temperature <-
  fah_hwinfo_log %>% 
  as.data.frame() %>% 
  select(c(1, 2, 52, 
           which(colnames(fah_hwinfo_log) %>% str_detect("FANIN"))))


colnames(cpu_temperature) <- c("date", "time", 
                               "cpu_temperature_tctl_tdie",
                               "system_fan", "cpu_fan",
                               "motherboard", "aux_fan_2")

cpu_temperature_clean <- 
  cpu_temperature %>% 
  as_tibble() %>% 
  mutate(
    date = as.Date(date, format = "%d.%m.%Y"), 
    timestamp = lubridate::ymd_hms(paste(date, time))
    ) %>% 
  na.omit() %>% 
  pivot_longer(cols = system_fan:aux_fan_2,
               names_to = "fan",
               values_to = "rpm") %>% 
  mutate(rpm = as.numeric(rpm),
         cpu_temperature_tctl_tdie = as.numeric(cpu_temperature_tctl_tdie))

cpu_temperature_clean %>% 
  filter(fan == "motherboard") %>% 
  ggplot(aes(timestamp, cpu_temperature_tctl_tdie)) +
  geom_line() +
  geom_vline(xintercept = lubridate::ymd_hms("2020-04-13 21:19:00"),
             colour = "red") +
  geom_vline(xintercept = lubridate::ymd_hms("2020-04-13 21:21:00"),
             colour = "green") +
  geom_vline(xintercept = lubridate::ymd_hms("2020-04-13 22:09:00"),
             colour = "blue") +
  geom_vline(xintercept = lubridate::ymd_hms("2020-04-13 22:20:00"),
             colour = "yellow")

cpu_temperature_clean %>% 
  ggplot(aes(timestamp, rpm, colour = fan)) +
  geom_line()

  
