library(googleAnalyticsR)
library(googleAuthR)
library(dplyr)
library(ggplot2)
library(lubridate)

view <- "177200146"
release_date <- as.Date("2018-06-19")
gar_auth_service('~/.vizlab/VIZLAB-a48f4107248c.json')

# time_series <- google_analytics(viewId = view, 
#                                 date_range = c(start = release_date, end = Sys.Date()),
#                                dimensions = c("date", "hour"),
#                                metrics = c("sessions", "users"), max = -1, 
#                               anti_sample = TRUE) %>% 
#   mutate(dateTime = as.POSIXct(paste0(date, " ", hour, ":00:00")))
# ggplot(time_series, aes(x = dateTime, y = sessions)) + geom_line() +
#   scale_x_datetime(date_minor_breaks = "12 hours", 
#                    date_breaks = "24 hours", 
#                    expand = c(0,0)) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

total <- google_analytics(viewId = view, 
                          date_range = c(start = release_date, end = Sys.Date()),
                          dimensions = c("networkDomain"),
                          metrics = c("sessions", "users"), max = -1, 
                          anti_sample = TRUE) %>% 
  mutate(internal = ifelse(networkDomain == "usgs.gov", TRUE, FALSE)) %>% 
  group_by(internal) %>% summarize(n=sum(sessions))

get_platform_plot <- function(viewId, release_date) {
  platforms <- google_analytics(viewId = view, 
                                date_range = c(start = release_date, end = Sys.Date()),
                                dimensions = c("deviceCategory"),
                                metrics = c("sessions"), max = -1, 
                                anti_sample = TRUE)
  plot <- ggplot(platforms, aes(x = deviceCategory, y = sessions)) + geom_col() + 
    ylab("Sessions") + xlab("Device category")
  return(plot)
}

get_campaign_source_table <- function(viewId, release_date) {
  campaigns_sources <- google_analytics(viewId = view, 
                                        date_range = c(start = release_date, end = Sys.Date()),
                                        dimensions = c("campaign", "source", "keyword", "networkDomain"),
                                        metrics = c("sessions"), max = -1, 
                                        anti_sample = TRUE)
  campaigns_sources_classified <- campaigns_sources %>% 
    mutate(networkDomain = ifelse(networkDomain == "usgs.gov", yes = "USGS network", no = "Off network")) %>% 
    group_by(campaign, source, keyword, networkDomain) %>% summarize(n=sum(sessions))
  
  campaign_total_sessions <- campaigns_sources_classified %>% ungroup() %>% group_by(campaign, source, keyword) %>% 
    summarize(source_campaign_total = sum(n))
  campaigns_sources_joined <- left_join(campaigns_sources_classified, campaign_total_sessions, 
                                        by = c("campaign", "source", "keyword")) %>% 
    filter(networkDomain == "USGS network") %>% 
    mutate(frac_internal = n/source_campaign_total) %>% select(-networkDomain, -n)
  return(campaigns_sources_joined)
  
}
#tag a more general source for time series plot
get_time_series_plot <- function(viewId, release_date) {
  
  time_series_sources <- google_analytics(viewId = view, 
                                          date_range = c(start = release_date, end = Sys.Date()),
                                          dimensions = c("date", "hour", "campaign", "source"),
                                          metrics = c("sessions"), max = -1, 
                                          anti_sample = TRUE)
  time_series_big_source <- time_series_sources %>% 
    mutate(big_source = case_when(grepl("reddit", source) ~ "Reddit",
                                  grepl("twitter|t.co", source) ~ "Twitter",
                                  grepl("facebook|fb", source) ~ "Facebook",
                                  grepl("popsci", source) ~ "PopSci",
                                  grepl("direct", source) ~ "Direct",
                                  TRUE ~ "Other"),
           dateTime = as.POSIXct(paste0(date, " ", hour, ":00:00")),
           date = as.Date(date)) %>% 
    group_by(dateTime, big_source) %>% summarize(sessions = sum(sessions))
  
  #pad out the first day so timeseries starts at midnight - keeps axis breaks on noon/midnight
  empty_padding <- tibble(dateTime=seq(as.POSIXct("2018-06-19 00:00:00"), 
                                       time_series_big_source$dateTime[1] - hours(1), by = "hour"),
                          big_source = "Other", sessions = 0)
  time_series_big_source_padded <- bind_rows(empty_padding, time_series_big_source)
  plot <- ggplot(time_series_big_source_padded, aes(x = dateTime, y = sessions, color = big_source)) + geom_line() +
    scale_x_datetime(date_minor_breaks = "12 hours", 
                     date_breaks = "24 hours", 
                     expand = c(0,0)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot)
}


