#1 - client ID
#2 - session ID
#3 - timestamp
library(ggplot2)





view_changes <- events %>% filter(eventAction == "update view")
view_change_sessions <- length(unique(view_changes$sessionID))
no_view_changes <- total_sessions - view_change_sessions
summed_view_changes <- view_changes %>% group_by(sessionID) %>% 
  summarize(n_view_changes=n()) %>% 
  ungroup() %>% group_by(n_view_changes) %>% summarize(n_sessions=n()) %>% 
  bind_rows(data.frame(n_view_changes=0, n_sessions=no_view_changes))
ggplot(summed_view_changes, aes(x = n_view_changes, y = n_sessions)) + 
  geom_col() + scale_x_continuous(minor_breaks = 1:30, breaks = seq(0,30,5)) +
  xlab("Number of view changes") + ylab("Sessions")


cat_changes <- events %>% filter(grepl(pattern = "update category", x = eventAction)) 
no_cat_changes <- total_sessions - length(unique(cat_changes$sessionID))
summed_cat_changes <- cat_changes %>% 
  group_by(sessionID) %>% summarize(n_cat_changes = n()) %>% ungroup() %>% 
  group_by(n_cat_changes) %>% summarize(n_sessions=n()) %>% 
  bind_rows(data.frame(n_cat_changes=0, n_sessions = no_cat_changes)) %>% 
  arrange(n_cat_changes)
ggplot(summed_cat_changes, aes(x = n_cat_changes, y = n_sessions)) + 
  geom_col() + scale_x_continuous(minor_breaks = 1:30, breaks = seq(0,30,5)) +
  xlab("Number of category changes") + ylab("Sessions")

finished_rankem <- events %>% filter(eventAction == "finished state ranking")
length(unique(finished_rankem$sessionID))
