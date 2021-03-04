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
################################################# 2a - 2b
################################################# Q57 Communication Objectives
a2b2_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q57_1:Q57_6, Q32, age, Q35) %>%
  replace(.=="Very high\r\nimportance", "5") %>% 
  replace(.=="High\r\nimportance", "4") %>% 
  replace(.=="Moderate\r\nimportance\r\n", "3") %>% 
  replace(.=="Low\r\nimportance", "2") %>% 
  replace(.=="Very low\r\nimportance", "1") %>% 
  # replace(. == 'Q57_1', 'Traditional objectives') %>% 
  # replace(. == 'Q57_2', 'Traditional objectives') %>% 
  # replace(. == 'Q57_3', 'Traditional objectives') %>% 
  # replace(. == 'Q57_4', 'Expanded objectives') %>% 
  # replace(. == 'Q57_5', 'Expanded objectives') %>% 
  # replace(. == 'Q57_6', 'Expanded objectives') %>%
  mutate(Q57_1 = as.integer(Q57_1)) %>% 
  mutate(Q57_2 = as.integer(Q57_2)) %>% 
  mutate(Q57_3 = as.integer(Q57_3)) %>% 
  mutate(Q57_4 = as.integer(Q57_4)) %>% 
  mutate(Q57_5 = as.integer(Q57_5)) %>% 
  mutate(Q57_6 = as.integer(Q57_6)) %>% 
  mutate_at(vars(Q57_1, Q57_2, Q57_3, Q57_4, Q57_5, Q57_6), ~tidyr::replace_na(., 0))

a2_Dataset <- a2b2_Dataset %>% 
  mutate(
    averageTO = round((Q57_1 + Q57_2 + Q57_3)/3, 0),
    averageEO = round((Q57_4 + Q57_5 + Q57_6)/3, 0)
  ) %>% 
  select(averageTO, averageEO) %>% 
  lapply(function(x) factor(
    x,
    levels = c("1", "2", "3", "4", "5"),
    ordered = TRUE)
  )

a2b2_Dataset[1:6] <- lapply(a2b2_Dataset[1:6], function(x) factor(x, levels = c("1", "2", "3", "4", "5"),
                                                                  ordered = TRUE))
a2b2_Dataset_likertSummary <- likert::likert(as.data.frame(a2b2_Dataset[1:6]))

#    Item         1        2        3        4        5
# 1 Q57_1 0.0000000 1.212121 12.12121 27.27273 59.39394
# 2 Q57_2 0.6060606 1.818182 29.09091 40.60606 27.87879
# 3 Q57_3 0.6097561 4.268293 14.02439 40.85366 40.24390
# 4 Q57_4 2.4390244 8.536585 34.75610 34.14634 20.12195
# 5 Q57_5 3.0487805 7.317073 22.56098 37.80488 29.26829
# 6 Q57_6 3.6585366 6.097561 23.78049 35.36585 31.09756
plot(a2b2_Dataset_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_ObjectivesAll.png", dpi = 400)

# an example of Data Vis for Likert Questions https://lmudge13.github.io/sample_code/likert_graphs.html
# Bar plot
### Note: for the percent numbers, 
###   responses are grouped into "low", "neutral", and "high"
# the same chart performed with ggplot https://ourcodingclub.github.io/tutorials/qualitative/
likertSummary <- likert::likert(as.data.frame(a2_Dataset))
plot(likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_Objectives.png", dpi = 400)
# Heat plot
plot(likertSummary, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)
ggsave("./images_paper1_Statistics/heatPlotLikert_Objectives.png", dpi = 400)
# Plots that treat Likert data like numeric data
### Note: Vertical lines are means for each group.
### Note: Curves are density plots, which show the distribution of values
###   similar to a histogram.
plot(likertSummary,
     type = "density",
     facet = TRUE, 
     bw = 0.5)
ggsave("./images_paper1_Statistics/curvesLikert_Objectives.png", dpi = 400)

### Create a new variable which is the Likert scores as an ordered factor
a2_Dataset_bis <- a2_Dataset %>% 
  as_tibble() %>% 
  tidyr::gather(objectives, Likert, 1:2) %>% 
  mutate(Likert = as.integer(Likert))

a2_Dataset_bis$Likert.f <- factor(a2_Dataset_bis$Likert,
                       ordered = TRUE)

a2_Dataset_bis

str(a2_Dataset_bis)
summary(a2_Dataset_bis)

a2_Dataset_bisSummary <- FSA::Summarize(Likert ~ objectives, 
          data = a2_Dataset_bis, 
          digits = 3)
a2_Dataset_bisSummary

## Plot n by questions and color by questions
ggpubr::ggboxplot(
  a2_Dataset_bis, x = "objectives", y = "Likert", 
  color = "objectives", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Objectives"
)
## Are the data from each of the groups follow a normal distribution?
## unpaired two-samples Wilcoxon test
a2Q57_res_Wilcoxon <- a2_Dataset_bis %>% 
  rstatix::wilcox_test(Likert ~ objectives) %>%
  rstatix::add_significance()

a2Q57_res_Wilcoxon

################################################# 2b
################################################# Q35 Gender
a2b2_Dataset[1:6] <- lapply(a2b2_Dataset[1:6], function(x) as.numeric(as.character(x)))
b2Q35_Dataset <- a2b2_Dataset %>%
  dplyr::select(Q57_1:Q57_6, Q35) %>% 
  mutate_at(vars(Q35), ~tidyr::replace_na(., 'no answer')) %>%
  mutate(
    averageTO = round((Q57_1 + Q57_2 + Q57_3)/3, 0),
    averageEO = round((Q57_4 + Q57_5 + Q57_6)/3, 0),
    averageTO_Male = ifelse(Q35 == 'Male', round((Q57_1 + Q57_2 + Q57_3)/3, 0), NA),
    averageEO_Male = ifelse(Q35 == 'Male', round((Q57_4 + Q57_5 + Q57_6)/3, 0), NA),
    averageTO_Female = ifelse(Q35 == 'Female', round((Q57_1 + Q57_2 + Q57_3)/3, 0), NA),
    averageEO_Female = ifelse(Q35 == 'Female', round((Q57_4 + Q57_5 + Q57_6)/3, 0), NA),
    averageTO_noAnswer = ifelse(Q35 == 'no answer', round((Q57_1 + Q57_2 + Q57_3)/3, 0), NA),
    averageEO_noAnswer = ifelse(Q35 == 'no answer', round((Q57_4 + Q57_5 + Q57_6)/3, 0), NA)
  ) %>% 
  select(averageTO:averageEO_noAnswer, Q35)
  
b2Q35_Dataset
#numeric bar for gender
plotly::plot_ly(data = as.data.frame(b2Q35_Dataset), x = ~Q35)
ggsave("./images_paper1_Statistics/bar_ObjectivesGender.png", dpi = 400)

b2Q35_Dataset_bis <- b2Q35_Dataset %>% 
  select(averageTO_Male:averageEO_noAnswer) %>% 
  rstatix::convert_as_factor(averageTO_Male, averageEO_Male, averageTO_Female, averageEO_Female, averageTO_noAnswer, averageEO_noAnswer) %>% 
  tidyr::gather(reasons, Likert, 1:6) %>% 
  mutate(Likert = as.integer(Likert)) %>% 
  as_tibble() %>% 
  filter(!is.na(Likert)) %>% 
  tibble::rowid_to_column("ID") %>% 
  rstatix::convert_as_factor(ID, reasons)

b2Q35_Dataset_bis

b2Q35_Dataset_bis_Summary <- b2Q35_Dataset_bis %>% 
  group_by(reasons) %>%
  rstatix::get_summary_stats(Likert, type = "common")

b2Q35_Dataset_bis_Summary

# Mann-Whitney likert (score) vs gender (https://www.datanovia.com/en/lessons/friedman-test-in-r/)
# a3Q35_res_Fried <- a3Q35_Dataset_bis %>% 
#   rstatix::friedman_test(Likert ~ reasons)

b2Q35_res_Wilcoxon <- b2Q35_Dataset_bis %>%
  rstatix::wilcox_test(Likert ~ reasons, paired = F, p.adjust.method = "bonferroni") %>% 
  rstatix::add_xy_position(x = "reasons")
b2Q35_res_Wilcoxon %>% 
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





































b2Q35_Dataset[1:8] <- factor(b2Q35_Dataset[1:8],
                               levels = c("1", "2", "3", "4", "5", NA),
                               ordered = TRUE)
b2Q35_Dataset$Q35 <- factor(b2Q35_Dataset$Q35,
                                  levels = c("Male", "Female", "no answer"),
                                  ordered = TRUE)
b2Q35_Likert <- likert::likert(as.data.frame(b2Q35_Dataset[,1:2]), grouping = b2Q35_Dataset$Q35)
# Bar plot
plot(b2Q35_Likert) + ggtitle('Bar plot of Traditional objectives (TO) and Expanded objectives (EO) separated per gender (Male and Female)')
ggsave("./images_paper1_Statistics/barPlotLikert_ObjectivesGender.png", dpi = 400)
# Plots that treat Likert data like numeric data
### Note: Vertical lines are means for each group.
### Note: Curves are density plots, which show the distribution of values
###   similar to a histogram.
plot(b2Q35_Likert,
     type = "density",
     facet = TRUE, 
     bw = 0.5)
ggsave("./images_paper1_Statistics/curvesLikert_ObjectivesGender.png", dpi = 400)

# follow this example https://www.dataindeed.io/2017/12/03/likert-scale-analysis/
b2Q35_Dataset_bis <- b2Q35_Dataset %>%
  tidyr::gather(objectives, Likert, 1:2)
# Mann-Whitney likert (score) vs gender
b2Q35_res_Wilcoxon <- b2Q35_Dataset_bis %>% 
  dplyr::mutate(Likert = as.numeric(Likert)) %>% 
  rstatix::wilcox_test(Likert ~ Q35) %>%
  rstatix::add_significance()

b2Q35_res_Wilcoxon

b2Q35_res_Kruskal <- b2Q35_Dataset_bis %>%
  dplyr::mutate(Likert = as.numeric(Likert)) %>% 
  rstatix::kruskal_test(Likert ~ Q35)
b2Q35_res_Kruskal

