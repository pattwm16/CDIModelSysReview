library(tidyverse)
library(janitor)
library(googlesheets4)
library(robvis)
library(flextable)
library(ggpubr)
library(cowplot)

# INPUTS ----
# list the sheet url of the PROBAST form
sheet_url <- 'https://docs.google.com/spreadsheets/d/16uaR99p2aE_8jieP3Ai14oV9dYFC12YXVKpqminmV7M/edit?resourcekey#gid=10033098'

# name figures produced in the analysis
figure_extension <- ".tiff"
figure_dpi       <- 800
figure1_filename <- "figs/fig4" # filename for traffic light plot
figure2_filename <- "figs/figs1" # filename for traffic light plot

# table produced in the analysis
table_filename <- "tbls/tbls4.docx"

# DATA LOAD ----
# load in raw data and separate written descriptions
raw_data <- read_sheet(sheet_url)
data_descriptions <- names(raw_data)

# DEV - How did the reviewers disagree?
(raw_data[c(2,3,10,13,23,26,42,47,72,74,76)] %>% 
  arrange(`Which study are you evaluating?`) %>% 
  flextable() %>% 
  theme_zebra() %>%
  merge_v(j = c(1)))

# separate derivation and validation column names
viz_column_names_d_rob <- data_descriptions[c(8,21,40,70)]
viz_column_names_v_rob <- data_descriptions[c(9,22,41,71)]
viz_column_names_d_app <- data_descriptions[c(11,24,45)]
viz_column_names_v_app <- data_descriptions[c(12,25,46)]
viz_column_names_over <- data_descriptions[c(75,76)]

# select columns of interest for viz
viz_column_names_v <- data.frame(col_keys = c("auth", "rob_participants_val", 
                                              "rob_predictors_val", "rob_outcome_val", 
                                              "rob_analysis_val", "app_participants_val", 
                                              "app_predictors_val", "app_outcome_val", 
                                              "rob_overall", "app_overall"),
                                 grp = c("Study", "Risk of bias", 
                                         "Risk of bias", "Risk of bias", 
                                         "Risk of bias", "Applicability", 
                                         "Applicability", "Applicability", 
                                         "Overall", "Overall"),
                                 domain = c("", "Participants","Predictors",
                                            "Outcome", "Analysis",
                                            "Participants","Predictors",
                                            "Outcome", "ROB", "Applicability"),
                                 stringsAsFactors = FALSE)
                                 
# create tidied data 
tidy_data <- raw_data %>% 
  select(-c(1)) %>% #remove timestamp from google forms
  clean_names() %>%
  filter(which_reviewer_are_you == "Will") #forms should have same data for both reviewers

# data viz schemea
tidy_data <- tidy_data %>%
  mutate(auth = which_study_are_you_evaluating) %>%
  mutate(rob_participants_dev = fct_recode(risk_of_bias_introduced_by_selection_of_participants_development, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_participants_val = fct_recode(risk_of_bias_introduced_by_selection_of_participants_validation, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_predictors_dev = fct_recode(risk_of_bias_introduced_by_predictors_or_their_assessment_development, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_predictors_val = fct_recode(risk_of_bias_introduced_by_predictors_or_their_assessment_validation, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_outcome_dev = fct_recode(risk_of_bias_introduced_by_the_outcome_or_its_determination_development, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_outcome_val = fct_recode(risk_of_bias_introduced_by_the_outcome_or_its_determination_validation, 
                                           "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_analysis_dev = fct_recode(risk_of_bias_introduced_by_the_analysis_development, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_analysis_val = fct_recode(risk_of_bias_introduced_by_the_analysis_validation, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(app_participants_dev = fct_recode(concern_that_the_definition_assessment_or_timing_of_predictors_in_the_model_do_not_match_the_review_question_development, 
                                           "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(app_participants_val = fct_recode(concern_that_the_definition_assessment_or_timing_of_predictors_in_the_model_do_not_match_the_review_question_validation, 
                                           "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(app_predictors_dev = fct_recode(concern_that_the_definition_assessment_or_timing_of_predictors_in_the_model_do_not_match_the_review_question_development, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(app_predictors_val = fct_recode(concern_that_the_definition_assessment_or_timing_of_predictors_in_the_model_do_not_match_the_review_question_validation, 
                                         "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(app_outcome_dev = fct_recode(concern_that_the_outcome_its_definition_timing_or_determination_do_not_match_the_review_question_development, 
                                      "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(app_outcome_val = fct_recode(concern_that_the_outcome_its_definition_timing_or_determination_do_not_match_the_review_question_validation, 
                                      "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(rob_overall = fct_recode(overall_judgement_of_risk_of_bias,
                                  "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(app_overall = fct_recode(overall_concerns_for_applicability,
                                  "-" = "Low", "+" = "High", "?" = "Unclear")) %>%
  mutate(who = which_reviewer_are_you)

# DATA FILTERING ----
# select only the cols needed for graphing
small_tidy_data <- tidy_data %>% 
  select(auth, #study titles
         rob_participants_val, rob_predictors_val, rob_outcome_val, rob_analysis_val, #risk of bias
         app_participants_val, app_predictors_val, app_outcome_val, #concerns for applicability
         rob_overall, app_overall) #overall scores
  
# save this small tidy table for publication
# NB: This corresponds to table 12 format from Moons, Ann Intern Med. 2019
# DOI: 10.7326/M18-1377
small_tidy_data %>% 
  select(-c(2)) %>%
  flextable() %>%
  set_header_df(mapping = viz_column_names_v, key = "col_keys") %>%
  merge_h(part = "header") %>%
  fix_border_issues() %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%
  save_as_docx(path = table_filename)

# FIGURES ----
# traffic light plot for evaluating risk of bias by study
rob_summary_dat_val <- raw_data[c(2,3,10,23,42,72,74)] %>% #ROB
  filter(`Which reviewer are you?` == "Will") %>%
  select(-c(`Which reviewer are you?`))

app_summary_dat_val <- raw_data[c(2,3,13,26,47,76)] %>% #Applicability concerns
  filter(`Which reviewer are you?` == "Will") %>%
  select(-c(`Which reviewer are you?`))

# Get rid of validation column designation
colnames(rob_summary_dat_val) <- gsub("\\[Validation\\]", "",
                                  colnames(rob_summary_dat_val))
colnames(app_summary_dat_val) <- gsub("\\[Validation\\]", "",
                                      colnames(app_summary_dat_val))

# Shorten long column names for plotting
colnames(rob_summary_dat_val)[6] <- "Overall"
colnames(app_summary_dat_val)[5] <- "Overall"
colnames(rob_summary_dat_val)[1] <- "Study"
colnames(app_summary_dat_val)[1] <- "Study"


## Traffic Light Figure for PROBAST (Figure S1)
# Filename defined at top of present file
# Combined view of risk of bias (a) and concern for applicability (b)
(f1_panel_a <- rob_summary_dat_val %>% 
  rob_traffic_light(tool = "Generic") + theme(title = element_blank()))  #get rid of incorrect title, cannot add new one
#f1_panel_a %>% rob_save(file = "panel_a.tiff", dpi = figure_dpi) #save panel a

(f1_panel_b <- app_summary_dat_val %>% 
  rob_traffic_light(tool = "Generic") + theme(title = element_blank())) #get rid of incorrect title, cannot add new one
#f1_panel_b %>% rob_save(file = "panel_b.tiff", dpi = figure_dpi) #save panel b

# Combine panel a and b to form final figure
(f1 <- plot_grid(f1_panel_a, f1_panel_b, 
                labels = c('A', "B"), 
                label_size = 9))

# Save file using predefined name and .tiff extension/device
ggsave(paste0(figure1_filename, figure_extension), 
       device = "tiff", 
       dpi = figure_dpi, # DPI can be specified here per journal specification
       units = "in", 
       width = 20, # makes sure domain labels don't collide with legend
       height = 8)

# Save file using predefined name and .png extension/device
ggsave(paste0(figure1_filename, "_preview", ".png"), 
       device = "png", 
       dpi = figure_dpi, # DPI can be specified here per journal specification
       units = "in", 
       width = 20, # makes sure domain labels don't collide with legend
       height = 8)

## Summary Barchart Figure for PROBAST (Figure 4)
# Filename defined at top of present file

# Combined view of risk of bias (a) and concern for applicability (b)
(f2_panel_a <- rob_summary_dat_val %>% # panel a - risk of bias
  rob_summary(tool = "Generic", weighted = FALSE) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)))

(f2_panel_b <- app_summary_dat_val %>% # panel b - concerns for applicability
  rob_summary(tool = "Generic", weighted = FALSE) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50))) 

# Combine panel a and b to form final figure
(f2 <- plot_grid(f2_panel_a, f2_panel_b, 
                labels = c('A', "B"),
                align = "v",
                ncol = 1,
                label_size = 9))

# Save file using predefined name and .tiff extension/device
ggsave(paste0(figure2_filename, figure_extension), 
       device = "tiff", 
       dpi = figure_dpi, # DPI can be specified here per journal specification
       units = "in", 
       width = 10, # makes sure domain labels don't collide with legend
       height = 15) 

# Save file using predefined name and .png extension/device
ggsave(paste0(figure2_filename, "_preview", ".png"), 
       device = "png", 
       dpi = figure_dpi, # DPI can be specified here per journal specification
       units = "in", 
       width = 10, # makes sure domain labels don't collide with legend
       height = 15)

