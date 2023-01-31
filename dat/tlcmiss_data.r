# packages
pacman::p_load(tidyverse, rio)
pacman::p_load(labelled)

# LOCATION
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import
tlcmiss_w <- import("./ala2e/tlcmiss.sas7bdat") %>% 
   rename_all(tolower) %>% 
   rename(w_0 = y1,
          w_1 = y2,
          w_4 = y3,
          w_6 = y4) %>% 
   mutate(trt = factor(trt, labels = c("Placebo","Active")) %>% 
             fct_rev()) %>% 
   arrange(id)

tlc_w <- import("./tlc_w.rds") 

tlcmiss_w <- left_join(
   tlcmiss_w %>% select(-w_0),
   tlc_w     %>% select(id, w_0)) %>% 
   relocate(w_0, .before = w_1)
   
   

# labels
var_label(tlcmiss_w) <- list(id = 'Study ID',
                       trt = 'Treatment',
                       w_0 = 'Week 0',
                       w_1 = 'Week 1',
                       w_4 = 'Week 4',
                       w_6 = 'Week 6')

# long
tlcmiss_l <- tlcmiss_w %>% 
   pivot_longer(cols = -c(1:2),
                names_to = c(".value", "week"),
                names_sep = '_',
                names_transform = list(week = as.integer)) %>% 
   rename(lead = w) %>% 
   group_by(id) %>% 
   mutate(base = lead[1],
          diff = lead - base) %>% 
   ungroup()

# export
export(tlcmiss_w, 'tlcmiss_w.csv')
export(tlcmiss_l, 'tlcmiss_l.csv')

export(tlcmiss_w, 'tlcmiss_w.rds')
export(tlcmiss_l, 'tlcmiss_l.rds')

# export
# same as manuscript
tlcmiss3_l <- tlcmiss_l %>% 
   filter(week != 1) %>% 
   mutate(follow = ifelse(week == 0, 0, 1)) %>% 
   mutate(across(c(id, week, follow), ~as.factor(.x)) )

export(tlcmiss3_l, 'tlcmiss3_l.csv')
export(tlcmiss3_l, 'tlcmiss3_l.rds')
export(tlcmiss3_l, 'h:/tlcmiss3_l.csv')
