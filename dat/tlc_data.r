# packages
pacman::p_load(tidyverse, rio)
pacman::p_load(labelled)

# LOCATION
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import
set.seed(123)

tlc_w <- import("./ala2e/tlc.sas7bdat") %>% 
   mutate(trt = factor(trt, labels = c("Active","Placebo"))) %>% 
   rename_with(~str_replace_all(.x, "y","w_"), .cols = starts_with("y")) %>% 
   mutate(w_0 = case_when(
      trt == 'Placebo' ~ w_0 + (0.60 + round(rnorm(50, 1.1), 1)),
      trt == 'Active'  ~ w_0))

# labels
var_label(tlc_w) <- list(id = 'Study ID',
                       trt = 'Treatment',
                       w_0 = 'Week 0',
                       w_1 = 'Week 1',
                       w_4 = 'Week 4',
                       w_6 = 'Week 6')

# long
tlc_l <- tlc_w %>% 
   pivot_longer(cols = -c(1:2),
                names_to = c(".value", "week"),
                names_sep = '_',
                names_transform = list(week = as.integer)) %>% 
   rename(lead = w) %>% 
   group_by(id) %>% 
   mutate(base = lead[1],
          diff = lead - base) %>% 
   ungroup() %>% 
   mutate(follow = ifelse(week == 0, 0, 1)) %>% 
   mutate(across(c(id, week, follow), ~as.factor(.x)) )

# export
export(tlc_w, 'tlc_w.csv')
export(tlc_l, 'tlc_l.csv')

export(tlc_w, 'tlc_w.rds')
export(tlc_l, 'tlc_l.rds')

# export
# same as manuscript
tlc3_l <- tlc_l %>% 
   filter(week != 1) %>% 
   mutate(follow = ifelse(week == 0, 0, 1)) %>% 
   mutate(across(c(id, week, follow), ~as.factor(.x)) )

export(tlc3_l, 'tlc3_l.csv')
export(tlc3_l, 'tlc3_l.rds')
export(tlc3_l, 'h:/tlc3_l.csv')
