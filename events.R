#1 - client ID
#2 - session ID
#3 - timestamp
library(ggplot2)



knitr::kable(scrolls, col.names = c("Scroll point", "Sessions", "Fraction of sessions"))  
#    kable_styling(full_width = F, position = "float_left")
# knitr::kable(data.frame(Avg = "foo")) %>% kable_styling(full_width = F, position = "center")
#knitr::kable(list(scrolls, data.frame(Avg = "foo")), align = "c")



finished_rankem <- events %>% filter(eventAction == "finished state ranking")
length(unique(finished_rankem$sessionID))
