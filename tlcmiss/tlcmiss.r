# PACKAGE
pacman::p_load(tidyverse, rio) 
pacman::p_load(labelled)
pacman::p_load(gt, gtsummary, gtreg)
pacman::p_load(mmrm, emmeans, multcomp)
pacman::p_load(broom)

# LOCATION
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# EXPORT
# knitr::purl(input = "../g1_axc.qmd", output = "tlcmiss_qmd.r", documentation = 0)

# OPTIONS
emm_options(emmeans = list(infer = c(TRUE, TRUE)),
            contrast = list(infer = c(TRUE, TRUE)),
            summary = list(adjust = 'none'))

# IMPORT
tlcmiss3_l <- import("../dat/tlcmiss3_l.rds")


# MODELS
m1a <- mmrm(lead ~ trt + base + us(week|id),     
            data = tlcmiss3_l %>% filter(week != '0'))

e1a <- emmeans(m1a, pairwise ~ trt) %>% 
   as.data.frame() 

# MODELS
m1b <- mmrm(lead ~ trt + base + week + week:trt + us(week|id),     
            data = tlcmiss3_l %>% filter(week != '0'))

e1b <- emmeans(m1b, pairwise ~ trt | week) %>% 
   as.data.frame()

# MODELS
m2a <- mmrm(lead ~ trt + follow + follow:trt + us(week|id), 
            data = tlcmiss3_l %>% mutate(follow = as.numeric(follow)-1)) 

e2a <- emmeans(m2a, pairwise ~ trt | follow) %>% 
   as.data.frame()

# MODELS
m2b <- mmrm(lead ~ trt + I(week=='4') + I(week=='6') + I(week=='4'):trt + I(week=='6'):trt + us(week|id), 
            data = tlcmiss3_l)

summary(m2b)

glht(m2b, linfct = matrix(c(1,0,0,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Placebo week 0
glht(m2b, linfct = matrix(c(1,1,0,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Active  week 0

glht(m2b, linfct = matrix(c(1,0,1,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Placebo week 4
glht(m2b, linfct = matrix(c(1,0,1,0,1,0),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Active  week 4

glht(m2b, linfct = matrix(c(1,0,0,1,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Placebo week 6
glht(m2b, linfct = matrix(c(1,1,0,1,0,1),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Active  week 6

glht(m2b, linfct = matrix(c(0,-1,0,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Diff    week 0
glht(m2b, linfct = matrix(c(0,0,0,0,-1,0),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Diff    week 4
glht(m2b, linfct = matrix(c(0,0,0,0,0,-1),1) ) %>% summary(test = adjusted("none")) %>% tidy() # Diff    week 6

d2b <- 
   bind_rows(
      glht(m2b, linfct = matrix(c(1,0,0,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Placebo week 0
      glht(m2b, linfct = matrix(c(1,1,0,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Active  week 0
      
      glht(m2b, linfct = matrix(c(1,0,1,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Placebo week 4
      glht(m2b, linfct = matrix(c(1,0,1,0,1,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Active  week 4
      
      glht(m2b, linfct = matrix(c(1,0,0,1,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Placebo week 6
      glht(m2b, linfct = matrix(c(1,1,0,1,0,1),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Active  week 6
      
      glht(m2b, linfct = matrix(c(0,-1,0,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Diff    week 0
      glht(m2b, linfct = matrix(c(0,0,0,0,-1,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Diff    week 4
      glht(m2b, linfct = matrix(c(0,0,0,0,0,-1),1) ) %>% summary(test = adjusted("none")) %>% tidy()) # Diff    week 6

e2b <- d2b %>% 
   mutate(week = c('0', '0', '4', '4', '6', '6', '0', '4', '6'), .before = 1) %>% 
   mutate(trt = c('Placebo','Active','Placebo','Active','Placebo','Active','.','.','.'), .after = 1) %>% 
   mutate(contrast = c(rep('.',6),'Placebo - Active','Placebo - Active','Placebo - Active'), .after = 2) %>% 
   dplyr::select(-null.value) %>% 
   rename(emmean = estimate,
          SE = std.error,
          t.ratio = statistic) %>% 
   mutate(lower.CL = emmean - SE * qt(.975, df = 72),
          upper.CL = emmean + SE * qt(.975, df = 72)) 

# MODELS
m2c <- mmrm(lead ~ follow + follow:trt + us(week|id), 
            data = tlcmiss3_l %>% mutate(follow = as.numeric(follow)-1))

e2f0 <- emmeans(m2c,            ~ trt | follow, at = list(follow = 0)) %>% as.data.frame()
e2f1 <- emmeans(m2c,   pairwise ~ trt | follow, at = list(follow = 1)) %>% as.data.frame()

e2c <- bind_rows(e2f0 %>% mutate(follow = '0'),
                 e2f1) 

# MODELS
m2d <- mmrm(lead ~ I(week=='4') + I(week=='6') + 
               I(week=='4' & trt == 'Active') + 
               I(week=='6' & trt == 'Active') + us(week|id), 
            data = tlcmiss3_l)

summary(m2d)

d2d <- 
   bind_rows(
      glht(m2d, linfct = matrix(c(1,0,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), #         week 0
      glht(m2d, linfct = matrix(c(1,1,0,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Placebo week 4
      glht(m2d, linfct = matrix(c(1,0,1,0,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Placebo week 6
      glht(m2d, linfct = matrix(c(1,1,0,1,0),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Active  week 4
      glht(m2d, linfct = matrix(c(1,0,1,0,1),1) ) %>% summary(test = adjusted("none")) %>% tidy(), # Active  week 6
      
      glht(m2d, linfct = matrix(c(0,0,0,1,0),1) ) %>% summary(test = adjusted("none"))%>% tidy(), # Diff    week 4
      glht(m2d, linfct = matrix(c(0,0,0,0,1),1) ) %>% summary(test = adjusted("none")) %>% tidy())# Diff    week 6

e2d <- d2d %>% 
   mutate(week = c('0', '4', '6', '4', '6', '4', '6'), .before = 1) %>% 
   mutate(trt = c('Both','Placebo','Placebo','Active','Active','.','.'), .after = 1) %>% 
   mutate(contrast = c(rep('.',5),'Placebo - Active','Placebo - Active'), .after = 2) %>% 
   dplyr::select(-null.value) %>% 
   rename(emmean = estimate,
          SE = std.error,
          t.ratio = statistic) %>% 
   mutate(lower.CL = emmean - SE * qt(.975, df = 72),
          upper.CL = emmean + SE * qt(.975, df = 72)) 

# MODELS
m3a <- mmrm(diff ~ trt + us(week|id), 
            data = tlcmiss3_l %>% filter(week != '0'))

e3a <- emmeans(m3a, pairwise ~ trt) %>% 
   as.data.frame()

# MODELS
m3b <- mmrm(diff ~ trt + week + trt:week + us(week|id), 
            data = tlcmiss3_l %>% filter(week != '0'))

e3b <- emmeans(m3b, pairwise ~ trt | week) %>% 
   as.data.frame()

# MODELS
m3c <- mmrm(diff ~ trt + base + us(week|id), 
            data = tlcmiss3_l %>% filter(week != '0'))

e3c <- emmeans(m3c, pairwise ~ trt) %>% 
   as.data.frame()

# MODELS
m3d <- mmrm(diff ~ trt + base + week + trt:week + us(week|id), 
            data = tlcmiss3_l %>% filter(week != '0'))

e3d <- emmeans(m3d, pairwise ~ trt | week) %>% 
   as.data.frame()


tlcmiss_all <- tribble(
   ~table,
   ~equation,  
   ~method,                            
   ~m,
   ~e,
   
   'Overall',
   "(1a)", 
   "Longitudinal analysis of covariance",  
   m1a,
   e1a,
   
   'Overall',
   "(2a)",                   
   "Repeated measures analysis",   
   m2a,
   e2a,
   
   'Overall',
   "(2c)", 
   "Repeated measures wo/ treatment",   
   m2c,
   e2c,
   
   'Overall',
   "(3a)",   
   "Analysis of changes (not adjusted)",  
   m3a,
   e3a,
   
   'Overall',
   "(3c)",      
   "Analysis of changes (adjusted)",  
   m3c,
   e3c,
   
   'Weekly',
   "(1b)",  
   "Longitudinal analysis of covariance",   
   m1b,
   e1b,
   
   'Weekly',
   "(2b)",                   
   "Repeated measures analysis",   
   m2b,
   e2b,
   
   'Weekly',
   "(2d)", 
   "Repeated measures wo/ treatmentt",   
   m2d,
   e2d,
   
   'Weekly',
   "(3b)",  
   "Analysis of changes (not adjusted)",   
   m3b,
   e3b,
   
   'Weekly',
   "(3d)",      
   "Analysis of changes (adjusted)",   
   m3d,
   e3d) %>% 
   rowwise() %>% 
   mutate(data  = list(m$tmb_data$full_frame),
          subset = attr(m$data,'dataname') %>% str_remove( "%>%, "),
          model = Reduce( paste, deparse(m$formula_parts$formula) ),
          call  = list(m$call),
          coef  = list(summary(m)$coefficient))

export(tlcmiss_all, 'tlcmiss_all.rds')

e_all <- tlcmiss_all %>% 
   dplyr::select(table, equation, method, e) %>% 
   unnest(cols = e) %>% 
   relocate(c(week, follow), .after = trt) %>% 
   relocate(equation, .before = trt) %>% 
   mutate(week = ifelse(!is.na(follow), ifelse(follow == '0', '0', '4/6'), week)) %>%
   mutate(week = ifelse(is.na(week), '4/6', week)) %>% 
   dplyr::select(-df, -follow)

e_all %>% 
   filter(contrast != '.') %>% 
   filter(week != '0') %>% 
   filter(table == 'Overall') %>% 
   mutate(equation = str_glue("{equation} {method}")) %>% 
   gt(groupname_col = 'equation') %>% 
   fmt_number(columns = c(emmean, lower.CL, upper.CL), decimals = 1) %>% 
   fmt_number(columns = c(SE, t.ratio), decimals = 2) %>% 
   fmt(columns = p.value, 
       fns = function(x) gtsummary::style_pvalue(x, digits = 2)) %>% 
   sub_values(columns = week,
              pattern  = "NA", replacement  = '4/6') %>% 
   cols_merge_uncert(emmean, SE) %>% 
   cols_hide(columns = c(method, contrast, table, trt)) %>% 
   cols_align_decimal() %>% 
   tab_options(row_group.as_column = TRUE)%>% 
   gtsave('tlcmiss-tbl_overall.html')

e_all %>% 
   filter(contrast != '.') %>% 
   filter(week != '0') %>% 
   filter(table == 'Weekly') %>% 
   mutate(equation = str_glue("{equation} {method}")) %>% 
   gt(groupname_col = 'equation') %>% 
   fmt_number(columns = c(emmean, lower.CL, upper.CL), decimals = 1) %>% 
   fmt_number(columns = c(SE, t.ratio), decimals = 2) %>% 
   fmt(columns = p.value, 
       fns = function(x) gtsummary::style_pvalue(x, digits = 2)) %>% 
   sub_values(columns = week,
              pattern  = "NA", replacement  = '4/6') %>% 
   cols_merge_uncert(emmean, SE) %>% 
   cols_hide(columns = c(method, contrast, table, trt)) %>% 
   cols_align_decimal() %>% 
   tab_options(row_group.as_column = TRUE) %>% 
   gtsave('tlcmiss-tbl_weekly.html')
