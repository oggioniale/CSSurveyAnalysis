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
################################################# 7e
################################################# Q21 Roles
e7b2_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  select(Q21_1:Q21_13, Q35) %>%
  replace(.=="Very high\r\ninvolvement", "5") %>% 
  replace(.=="High\r\ninvolvement", "4") %>% 
  replace(.=="Moderate\r\ninvolvement", "3") %>% 
  replace(.=="Very little\r\ninvolvement", "2") %>% 
  replace(.=="Not at all\r\ninvolved", "1") %>% 
  mutate(Q21_1 = as.integer(Q21_1)) %>% 
  mutate(Q21_2 = as.integer(Q21_2)) %>% 
  mutate(Q21_3 = as.integer(Q21_3)) %>% 
  mutate(Q21_4 = as.integer(Q21_4)) %>% 
  mutate(Q21_5 = as.integer(Q21_5)) %>% 
  mutate(Q21_6 = as.integer(Q21_6)) %>% 
  mutate(Q21_7 = as.integer(Q21_7)) %>% 
  mutate(Q21_8 = as.integer(Q21_8)) %>% 
  mutate(Q21_9 = as.integer(Q21_9)) %>% 
  mutate(Q21_10 = as.integer(Q21_10)) %>% 
  mutate(Q21_11 = as.integer(Q21_11)) %>% 
  mutate(Q21_12 = as.integer(Q21_12)) %>% 
  mutate(Q21_13 = as.integer(Q21_13))
e7b2_Dataset

e7_Dataset <- e7b2_Dataset %>% 
  mutate(
    averageTRoles = round((Q21_5 + Q21_6)/2, 0),
    averageERoles = round((Q21_1 + Q21_2 + Q21_2 + Q21_4 +Q21_7 + Q21_8 + Q21_9 + Q21_10 + Q21_11 + Q21_12 + Q21_13)/11, 0)
  ) %>% 
  select(averageTRoles, averageERoles) %>% 
  lapply(function(x) factor(
    x,
    levels = c("1", "2", "3", "4", "5"),
    ordered = TRUE)
  )
e7_Dataset

e7b2_Dataset[1:13] <- lapply(e7b2_Dataset[1:13], function(x) factor(x, levels = c("1", "2", "3", "4", "5"),
                                                                  ordered = TRUE))
e7b2_Dataset_likertSummary <- likert::likert(as.data.frame(e7b2_Dataset[1:13]))
#      Item         1         2        3         4         5
# 1   Q21_1 36.842105 22.368421 21.05263 14.473684  5.263158
# 2   Q21_2 17.105263 22.368421 26.31579 21.052632 13.157895
# 3   Q21_3 35.064935 28.571429 29.87013  3.896104  2.597403
# 4   Q21_4 31.168831 37.662338 20.77922 10.389610  0.000000
# 5   Q21_5  5.194805  7.792208 11.68831 38.961039 36.363636
# 6   Q21_6 16.883117 31.168831 23.37662 23.376623  5.194805
# 7   Q21_7 46.666667 24.000000 18.66667  4.000000  6.666667
# 8   Q21_8 31.168831 27.272727 19.48052 18.181818  3.896104
# 9   Q21_9 57.142857 22.077922 15.58442  5.194805  0.000000
# 10 Q21_10 36.363636 28.571429 11.68831 20.779221  2.597403
# 11 Q21_11 24.675325  9.090909 22.07792 29.870130 14.285714
# 12 Q21_12 25.974026 20.779221 20.77922 28.571429  3.896104
# 13 Q21_13 14.285714 23.376623 20.77922 22.077922 19.480519

plot(e7b2_Dataset_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_RolesAll.png", dpi = 400)

# an example of Data Vis for Likert Questions https://lmudge13.github.io/sample_code/likert_graphs.html
# Bar plot
### Note: for the percent numbers, 
###   responses are grouped into "low", "neutral", and "high"
# the same chart performed with ggplot https://ourcodingclub.github.io/tutorials/qualitative/
e7_likertSummary <- likert::likert(as.data.frame(e7_Dataset))
plot(e7_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_Roles.png", dpi = 400)
# Heat plot
plot(e7_likertSummary, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)
ggsave("./images_paper1_Statistics/heatPlotLikert_Roles.png", dpi = 400)
# Plots that treat Likert data like numeric data
### Note: Vertical lines are means for each group.
### Note: Curves are density plots, which show the distribution of values
###   similar to a histogram.
plot(e7_likertSummary,
     type = "density",
     facet = TRUE, 
     bw = 0.5)
ggsave("./images_paper1_Statistics/curvesLikert_Roles.png", dpi = 400)

### Create a new variable which is the Likert scores as an ordered factor
e7_Dataset_bis <- e7_Dataset %>% 
  tidyr::gather(roles, Likert, 1:2) %>% 
  mutate(Likert = as.integer(Likert)) %>% 
  as_tibble()

e7_Dataset_bis$Likert.f <- factor(e7_Dataset_bis$Likert,
                                  ordered = TRUE)

e7_Dataset_bis

str(e7_Dataset_bis)
summary(e7_Dataset_bis)

e7_Dataset_bisSummary <- FSA::Summarize(Likert ~ roles, 
                                        data = e7_Dataset_bis, 
                                        digits = 3)
e7_Dataset_bisSummary

# roles  n nvalid  mean    sd min Q1 median Q3 max
# 1 averageERoles 77     74 2.459 0.894   1  2      2  3   4
# 2 averageTRoles 77     77 3.299 0.947   1  3      4  4   5

## Plot n by questions and color by questions
ggpubr::ggboxplot(
  e7_Dataset_bis,
  x = "roles", y = "Likert",
  width = 0.5, add = c("mean", "jitter"),
  color = "roles", palette = c("#00AFBB", "#E7B800"),
  ylab = "score", xlab = "Roles"
)

## Are the data from each of the groups follow a normal distribution?
## unpaired two-samples Wilcoxon test
e7Q21_res_Wilcoxon <- e7_Dataset_bis %>% 
  rstatix::wilcox_test(Likert ~ roles) %>%
  rstatix::add_significance()

e7Q21_res_Wilcoxon
