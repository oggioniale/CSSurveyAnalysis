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
################################################# 6a
################################################# Q29 Impacts
a6b2_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  select(Q29_1:Q29_9, Q35) %>%
  replace(.=="Strongly\r\nagree", "5") %>% 
  replace(.=="Agree", "4") %>% 
  replace(.=="Neither agree\r\nnor disagree", "3") %>% 
  replace(.=="Disagree", "2") %>% 
  replace(.=="Strongly\r\ndisagree", "1") %>% 
  mutate(Q29_1 = as.integer(Q29_1)) %>% 
  mutate(Q29_2 = as.integer(Q29_2)) %>% 
  mutate(Q29_3 = as.integer(Q29_3)) %>% 
  mutate(Q29_4 = as.integer(Q29_4)) %>% 
  mutate(Q29_5 = as.integer(Q29_5)) %>% 
  mutate(Q29_6 = as.integer(Q29_6)) %>% 
  mutate(Q29_7 = as.integer(Q29_7)) %>% 
  mutate(Q29_8 = as.integer(Q29_8)) %>% 
  mutate(Q29_9 = as.integer(Q29_9)) 
a6b2_Dataset

a6_Dataset <- a6b2_Dataset %>% 
  mutate(
    averageTraImpacts = round((Q29_1 + Q29_2)/2, 0),
    averageTraExpImpacts = round((Q29_3 + Q29_7 + Q29_8 + Q29_9)/4, 0),
    averageExpImpacts = round((Q29_4 + Q29_5 + Q29_6)/3, 0)
  ) %>% 
  select(averageTraImpacts, averageTraExpImpacts, averageExpImpacts) %>% 
  lapply(function(x) factor(
    x,
    levels = c("1", "2", "3", "4", "5"),
    ordered = TRUE)
  )
a6_Dataset

a6b2_Dataset[1:9] <- lapply(a6b2_Dataset[1:9], function(x) factor(x, levels = c("1", "2", "3", "4", "5"),
                                                                  ordered = TRUE))
a6b2_Dataset_likertSummary <- likert::likert(as.data.frame(a6b2_Dataset[1:9]))

#    Item        1         2        3        4         5
# 1 Q29_1 0.000000  5.194805 18.18182 55.84416 20.779221
# 2 Q29_2 1.298701  5.194805 10.38961 66.23377 16.883117
# 3 Q29_3 2.597403  5.194805 22.07792 41.55844 28.571429
# 4 Q29_4 0.000000  7.894737 15.78947 44.73684 31.578947
# 5 Q29_5 3.896104 10.389610 29.87013 40.25974 15.584416
# 6 Q29_6 5.405405  2.702703 29.72973 44.59459 17.567568
# 7 Q29_7 6.493506 19.480519 24.67532 40.25974  9.090909
# 8 Q29_8 6.493506 19.480519 33.76623 25.97403 14.285714
# 9 Q29_9 1.315789 10.526316 17.10526 55.26316 15.789474
plot(a6b2_Dataset_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_ImpactsAll.png", dpi = 400)

# an example of Data Vis for Likert Questions https://lmudge13.github.io/sample_code/likert_graphs.html
# Bar plot
### Note: for the percent numbers, 
###   responses are grouped into "low", "neutral", and "high"
# the same chart performed with ggplot https://ourcodingclub.github.io/tutorials/qualitative/
a6_likertSummary <- likert::likert(as.data.frame(a6_Dataset))
plot(a6_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_Impacts.png", dpi = 400)
# Heat plot
plot(a6_likertSummary, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)
ggsave("./images_paper1_Statistics/heatPlotLikert_Impacts.png", dpi = 400)
# Plots that treat Likert data like numeric data
### Note: Vertical lines are means for each group.
### Note: Curves are density plots, which show the distribution of values
###   similar to a histogram.
plot(a6_likertSummary,
     type = "density",
     facet = TRUE, 
     bw = 0.5)
ggsave("./images_paper1_Statistics/curvesLikert_Impacts.png", dpi = 400)

### Create a new variable which is the Likert scores as an ordered factor
a6_Dataset <- lapply(a6_Dataset, function(x) as.numeric(as.character(x))) 
a6_Dataset_bis <- a6_Dataset %>% 
  as_tibble() %>% 
  tidyr::gather(impacts, Likert, 1:3) %>% 
  mutate(Likert = as.integer(Likert)) 
  

a6_Dataset_bis$Likert.f <- factor(a6_Dataset_bis$Likert,
                                  ordered = TRUE)

a6_Dataset_bis

str(a6_Dataset_bis)
summary(a6_Dataset_bis)

a6_Dataset_bisSummary <- FSA::Summarize(Likert ~ impacts, 
                                        data = a6_Dataset_bis, 
                                        digits = 3)
a6_Dataset_bisSummary

## Are the data from each of the groups follow a normal distribution?
## unpaired two-samples Wilcoxon test
a6Q29_res_Wilcoxon <- a6_Dataset_bis %>% 
  rstatix::wilcox_test(Likert ~ impacts) %>%
  rstatix::add_significance()

a6Q29_res_Wilcoxon


