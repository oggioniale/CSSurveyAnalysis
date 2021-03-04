# Data analysis
# following this guide: http://www.sthda.com/english/wiki/comparing-means-in-r
# devtools::install_github("kassambara/ggpubr")

############# Load libraries
library(dplyr)
library(plyr)
library(ggplot2)

############# load base dataset
ILTERAnswers <- readxl::read_excel("ILTER and Public Engagement_October2020_CB_AO.xlsx")
ILTERAnswers$age <- as.numeric(format(Sys.Date(), "%Y")) - ILTERAnswers$Q33
# View(ILTERAnswers)
################################################# 4a
################################################# Q8 Barriers
regions <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% # 165
  filter(!is.na(Regions)) %>% # 134
  # filter(Regions == 'LTER Europe' | Regions == 'Americas' ) %>% # 118
  select(Regions)
unique(regions)

geography_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% # 165
  filter(!is.na(Regions)) %>% # 134
  # filter(Regions == 'LTER Europe' | Regions == 'Americas' ) %>% # 118
  select(c(Q6_1:Q6_8, Q21_1:Q21_13, Q57_1:Q57_6)) %>% 
  # Q6 Reasons
  replace(.=="Very high\r\nimportance", "5") %>% 
  replace(.=="High\r\nimportance", "4") %>% 
  replace(.=="Moderate\r\nimportance", "3") %>% 
  replace(.=="Little\r\nimportance", "2") %>% 
  replace(.=="Very low\r\nimportance", "1") %>%
  # Q21 Roles
  replace(.=="Very high\r\ninvolvement", "5") %>% 
  replace(.=="High\r\ninvolvement", "4") %>% 
  replace(.=="Moderate\r\ninvolvement", "3") %>% 
  replace(.=="Very little\r\ninvolvement", "2") %>% 
  replace(.=="Not at all\r\ninvolved", "1") %>%
  # Q57 Communication Objectives
  replace(.=="Very high\r\nimportance", "5") %>% 
  replace(.=="High\r\nimportance", "4") %>% 
  replace(.=="Moderate\r\nimportance\r\n", "3") %>% 
  replace(.=="Low\r\nimportance", "2") %>% 
  replace(.=="Very low\r\nimportance", "1") %>% 
  mutate_if(is.character, as.numeric) %>% 
  tibble::add_column(regions)

#numeric bar for Reagions
plotly::plot_ly(data = as.data.frame(geography_Dataset), x = ~Regions)
ggsave("./images_paper1_Statistics/bar_responsesPerRegions.png", dpi = 400)

geography_Dataset <- geography_Dataset %>% 
  mutate(
    # Q6
    averageTraReasons = round((Q6_1 + Q6_3 + Q6_4 + Q6_5)/4, 0),
    averageExpReasons = round((Q6_2 + Q6_6 + Q6_7 + Q6_8)/4, 0),
    # Q21
    averageTraRoles = round((Q21_5 + Q21_6)/2, 0),
    averageExpRoles = round((Q21_1 + Q21_2 + Q21_2 + Q21_4 +Q21_7 + Q21_8 + Q21_9 + Q21_10 + Q21_11 + Q21_12 + Q21_13)/11, 0),
    # Q57
    averageTraObjectives = round((Q57_1 + Q57_2 + Q57_3)/3, 0),
    averageExpObjectives = round((Q57_4 + Q57_5 + Q57_6)/3, 0)
  ) %>% 
  select(28:34) %>% # 118 x 7
  reshape2::melt() %>%  # 708 x 3
  filter(!is.na(value)) %>% 
  as_tibble() %>% # 607 x 3
  dplyr::mutate(groups = paste0(variable, '_', Regions))
geography_Dataset

geography_Dataset_Summary <- geography_Dataset %>% 
  group_by(groups) %>%
  rstatix::get_summary_stats(value, type = "common")

geography_Dataset_Summary

# unpaired two-samples Wilcoxon test
geography_DatasetMeans_res_Wilcoxon <- geography_Dataset %>%
  rstatix::wilcox_test(value ~ groups, paired = F, p.adjust.method = "bonferroni") 
geography_DatasetMeans_res_Wilcoxon %>%
  dplyr::slice(1,6,17,52,22,27,36,61,39,44,51,66) # for LTER_Europe and America
