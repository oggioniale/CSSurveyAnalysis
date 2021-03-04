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
################################################# 3a - 2b
################################################# Q6 Reasons
a3b2_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q6_1:Q6_8, Q35) %>%
  # 'High importance', 'Little importance', 'Moderate importance', 'Very high importance', 'Very low importance'
  replace(.=="Very high\r\nimportance", "5") %>% 
  replace(.=="High\r\nimportance", "4") %>% 
  replace(.=="Moderate\r\nimportance", "3") %>% 
  replace(.=="Little\r\nimportance", "2") %>% 
  replace(.=="Very low\r\nimportance", "1") %>% 
  # replace(. == 'Q57_1', 'Traditional objectives') %>% 
  # replace(. == 'Q57_2', 'Traditional objectives') %>% 
  # replace(. == 'Q57_3', 'Traditional objectives') %>% 
  # replace(. == 'Q57_4', 'Expanded objectives') %>% 
  # replace(. == 'Q57_5', 'Expanded objectives') %>% 
  # replace(. == 'Q57_6', 'Expanded objectives') %>%
  mutate(Q6_1 = as.integer(Q6_1)) %>% 
  mutate(Q6_2 = as.integer(Q6_2)) %>% 
  mutate(Q6_3 = as.integer(Q6_3)) %>% 
  mutate(Q6_4 = as.integer(Q6_4)) %>% 
  mutate(Q6_5 = as.integer(Q6_5)) %>% 
  mutate(Q6_6 = as.integer(Q6_6)) %>% 
  mutate(Q6_7 = as.integer(Q6_7)) %>% 
  mutate(Q6_8 = as.integer(Q6_8)) %>% 
  mutate_at(vars(Q6_1, Q6_2, Q6_3, Q6_4, Q6_5, Q6_6, Q6_7, Q6_8), ~tidyr::replace_na(., 0))
a3b2_Dataset

a3_Dataset <- a3b2_Dataset %>% 
  mutate(
    averageTR = round((Q6_1 + Q6_3 + Q6_4 + Q6_5)/4, 0),
    averageER = round((Q6_2 + Q6_6 + Q6_7 + Q6_8)/4, 0)
  ) %>% 
  select(averageTR, averageER) %>% 
  lapply(function(x) factor(
    x,
    levels = c("1", "2", "3", "4", "5"),
    ordered = TRUE)
  )

a3b2_Dataset[1:8] <- lapply(a3b2_Dataset[1:8], function(x) factor(x, levels = c("1", "2", "3", "4", "5"),
                                                                  ordered = TRUE))
a3b2_Dataset_likertSummary <- likert::likert(as.data.frame(a3b2_Dataset[1:8]))
#   Item          1          2        3        4         5
# 1 Q6_1  3.6363636 18.7878788 39.39394 26.66667 11.515152
# 2 Q6_2  4.2944785 17.1779141 49.69325 21.47239  7.361963
# 3 Q6_3 10.9756098 14.6341463 40.24390 28.65854  5.487805
# 4 Q6_4  1.2195122  7.9268293 23.78049 43.29268 23.780488
# 5 Q6_5  0.6060606  0.6060606 11.51515 33.93939 53.333333
# 6 Q6_6  0.6060606  9.0909091 29.69697 40.60606 20.000000
# 7 Q6_7  2.4242424  3.6363636 16.36364 40.60606 36.969697
# 8 Q6_8  0.0000000  7.9268293 24.39024 37.80488 29.878049
plot(a3b2_Dataset_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_ReasonsAll.png", dpi = 400)

# an example of Data Vis for Likert Questions https://lmudge13.github.io/sample_code/likert_graphs.html
# Bar plot
### Note: for the percent numbers, 
###   responses are grouped into "low", "neutral", and "high"
# the same chart performed with ggplot https://ourcodingclub.github.io/tutorials/qualitative/
a3_likertSummary <- likert::likert(as.data.frame(a3_Dataset))
plot(a3_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_Reasons.png", dpi = 400)
# Heat plot
plot(a3_likertSummary, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)
ggsave("./images_paper1_Statistics/heatPlotLikert_Reasons.png", dpi = 400)
# Plots that treat Likert data like numeric data
### Note: Vertical lines are means for each group.
### Note: Curves are density plots, which show the distribution of values
###   similar to a histogram.
plot(a3_likertSummary,
     type = "density",
     facet = TRUE, 
     bw = 0.5)
ggsave("./images_paper1_Statistics/curvesLikert_Reasons.png", dpi = 400)

### Create a new variable which is the Likert scores as an ordered factor
a3_Dataset_bis <- a3_Dataset %>% 
  tidyr::gather(reasons, Likert, 1:2) %>% 
  mutate(Likert = as.integer(Likert)) %>% 
  as_tibble()

a3_Dataset_bis$Likert.f <- factor(a3_Dataset_bis$Likert,
                                  ordered = TRUE)

a3_Dataset_bis

str(a3_Dataset_bis)
summary(a3_Dataset_bis)

a3_Dataset_bisSummary <- FSA::Summarize(Likert ~ reasons, 
                                        data = a3_Dataset_bis, 
                                        digits = 3)
a3_Dataset_bisSummary

## Plot n by questions and color by questions
ggpubr::ggboxplot(
  a3_Dataset_bis, x = "reasons", y = "Likert", 
  color = "reasons", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Reasons"
)
## Are the data from each of the groups follow a normal distribution?
## unpaired two-samples Wilcoxon test
a3Q6_res_Wilcoxon <- a3_Dataset_bis %>% 
  rstatix::wilcox_test(Likert ~ reasons) %>%
  rstatix::add_significance()

a3Q6_res_Wilcoxon

################################################# 2b
################################################# Q35 Gender
a3b2_Dataset[1:8] <- lapply(a3b2_Dataset[1:8], function(x) as.numeric(as.character(x)))
a3Q35_Dataset <- a3b2_Dataset %>% 
  mutate_at(vars(Q35), ~tidyr::replace_na(., 'no answer')) %>%
  mutate(
    averageTR = round((Q6_1 + Q6_3 + Q6_4 + Q6_5)/4, 0),
    averageER = round((Q6_2 + Q6_6 + Q6_7 + Q6_8)/4, 0),
    averageTR_Male = ifelse(Q35 == 'Male', round((Q6_1 + Q6_3 + Q6_4 + Q6_5)/4, 0), NA),
    averageER_Male = ifelse(Q35 == 'Male', round((Q6_2 + Q6_6 + Q6_7 + Q6_8)/4, 0), NA),
    averageTR_Female = ifelse(Q35 == 'Female', round((Q6_1 + Q6_3 + Q6_4 + Q6_5)/4, 0), NA),
    averageER_Female = ifelse(Q35 == 'Female', round((Q6_2 + Q6_6 + Q6_7 + Q6_8)/4, 0), NA),
    averageTR_noAnswer = ifelse(Q35 == 'no answer', round((Q6_1 + Q6_3 + Q6_4 + Q6_5)/4, 0), NA),
    averageER_noAnswer = ifelse(Q35 == 'no answer', round((Q6_2 + Q6_6 + Q6_7 + Q6_8)/4, 0), NA)
  )
a3Q35_Dataset

a3Q35_Dataset_bis <- a3Q35_Dataset %>% 
  select(averageTR_Male:averageER_noAnswer) %>% 
  rstatix::convert_as_factor(averageTR_Male, averageER_Male, averageTR_Female, averageER_Female, averageTR_noAnswer, averageER_noAnswer) %>% 
  tidyr::gather(reasons, Likert, 1:6) %>% 
  mutate(Likert = as.integer(Likert)) %>% 
  as_tibble() %>% 
  filter(!is.na(Likert)) %>% 
  tibble::rowid_to_column("ID") %>% 
  rstatix::convert_as_factor(ID, reasons)

a3Q35_Dataset_bis

a3Q35_Dataset_bis_Summary <- a3Q35_Dataset_bis %>% 
  group_by(reasons) %>%
  rstatix::get_summary_stats(Likert, type = "common")

a3Q35_Dataset_bis_Summary

# Mann-Whitney likert (score) vs gender (https://www.datanovia.com/en/lessons/friedman-test-in-r/)
# a3Q35_res_Fried <- a3Q35_Dataset_bis %>% 
#   rstatix::friedman_test(Likert ~ reasons)

a3Q35_res_Wilcoxon <- a3Q35_Dataset_bis %>%
  rstatix::wilcox_test(Likert ~ reasons, paired = F, p.adjust.method = "bonferroni") %>% 
  rstatix::add_xy_position(x = "reasons")
a3Q35_res_Wilcoxon %>% 
  slice(1,3,8,13)


a2Q35_Dataset_ter <- a3Q35_Dataset %>% 
  select(averageTR:averageER_noAnswer) %>% 
  lapply(function(x) factor(
    x,
    levels = c("1", "2", "3", "4", "5"),
    ordered = TRUE)
  )

a3Q35_Likert <- likert::likert(
  as.data.frame(a2Q35_Dataset_ter)
)
# Bar plot
plot(a3Q35_Likert) + ggtitle('Bar plot of Traditional reasons (TR) and Expanded reasons (ER) separated per gender (Male and Female)')
ggsave("./images_paper1_Statistics/barPlotLikert_ReasonsGender.png", dpi = 400)
# Plots that treat Likert data like numeric data
### Note: Vertical lines are means for each group.
### Note: Curves are density plots, which show the distribution of values
###   similar to a histogram.
plot(a3Q35_Likert,
     type = "density",
     facet = TRUE, 
     bw = 0.5)
ggsave("./images_paper1_Statistics/curvesLikert_ReasonsGender.png", dpi = 400)
