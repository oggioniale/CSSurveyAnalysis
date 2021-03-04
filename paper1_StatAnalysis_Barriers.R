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
a4_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q8_1:Q8_8)) %>%
  replace(.=="Strongly\r\nagree", "5") %>% 
  replace(.=="Agree", "4") %>% 
  replace(.=="Neither agree\r\nnor disagree", "3") %>% 
  replace(.=="Disagree", "2") %>% 
  replace(.=="Strongly\r\ndisagree", "1") %>% 
  mutate(Q8_1 = as.integer(Q8_1)) %>% 
  mutate(Q8_2 = as.integer(Q8_2)) %>% 
  mutate(Q8_3 = as.integer(Q8_3)) %>% 
  mutate(Q8_4 = as.integer(Q8_4)) %>% 
  mutate(Q8_5 = as.integer(Q8_5)) %>% 
  mutate(Q8_6 = as.integer(Q8_6)) %>% 
  mutate(Q8_7 = as.integer(Q8_7)) %>% 
  mutate(Q8_8 = as.integer(Q8_8))
a4_Dataset$Q8_1 <- factor(a4_Dataset$Q8_1,
                                       levels = c("1", "2", "3", "4", "5"),
                                       ordered = TRUE)
a4_Dataset$Q8_2 <- factor(a4_Dataset$Q8_2,
                                          levels = c("1", "2", "3", "4", "5"),
                                          ordered = TRUE)
a4_Dataset$Q8_3 <- factor(a4_Dataset$Q8_3,
                                       levels = c("1", "2", "3", "4", "5"),
                                       ordered = TRUE)
a4_Dataset$Q8_4 <- factor(a4_Dataset$Q8_4,
                          levels = c("1", "2", "3", "4", "5"),
                          ordered = TRUE)
a4_Dataset$Q8_5 <- factor(a4_Dataset$Q8_5,
                          levels = c("1", "2", "3", "4", "5"),
                          ordered = TRUE)
a4_Dataset$Q8_6 <- factor(a4_Dataset$Q8_6,
                          levels = c("1", "2", "3", "4", "5"),
                          ordered = TRUE)
a4_Dataset$Q8_7 <- factor(a4_Dataset$Q8_7,
                          levels = c("1", "2", "3", "4", "5"),
                          ordered = TRUE)
a4_Dataset$Q8_8 <- factor(a4_Dataset$Q8_8,
                          levels = c("1", "2", "3", "4", "5"),
                          ordered = TRUE)
# an example of Data Vis for Likert Questions https://lmudge13.github.io/sample_code/likert_graphs.html
# Bar plot
### Note: for the percent numbers, 
###   responses are grouped into "low", "neutral", and "high"
# the same chart performed with ggplot https://ourcodingclub.github.io/tutorials/qualitative/
a4_likertSummary <- likert::likert(as.data.frame(a4_Dataset)) 
# Item         1        2        3         4         5
# 1 Q8_1  9.090909 23.63636 32.12121 28.484848  6.666667
# 2 Q8_2  7.878788 29.69697 29.69697 23.030303  9.696970
# 3 Q8_3  4.268293 22.56098 31.09756 34.756098  7.317073
# 4 Q8_4 19.512195 49.39024 23.78049  4.268293  3.048780
# 5 Q8_5  4.268293 18.29268 23.17073 41.463415 12.804878
# 6 Q8_6 12.727273 36.36364 33.93939 14.545455  2.424242
# 7 Q8_7  3.067485 13.49693 23.92638 46.625767 12.883436
# 8 Q8_8  6.097561 20.73171 31.09756 25.609756 16.463415
plot(a4_likertSummary, type="bar")
ggsave("./images_paper1_Statistics/barPlotLikert_Barriers.png", dpi = 400)

### Create a new variable which is the Likert scores as an ordered factor
a4_Dataset_bis <- a4_Dataset %>% 
  tidyr::gather(barriers, Likert, 1:8) %>% 
  mutate(Likert = as.integer(Likert)) %>% 
  as_tibble()

a4_Dataset_bis$Likert.f <- factor(a4_Dataset_bis$Likert,
                                  ordered = TRUE)

a4_Dataset_bis

str(a4_Dataset_bis)
summary(a4_Dataset_bis)

a4_Dataset_bisSummary <- FSA::Summarize(Likert ~ barriers, 
                                        data = a4_Dataset_bis, 
                                        digits = 3)
a4_Dataset_bisSummary
# barriers   n nvalid  mean    sd min Q1 median Q3 max
# 1     Q8_1 165    165 3.000 1.076   1  2      3  4   5
# 2     Q8_2 165    165 2.970 1.112   1  2      3  4   5
# 3     Q8_3 165    164 3.183 1.005   1  2      3  4   5
# 4     Q8_4 165    164 2.220 0.914   1  2      2  3   5
# 5     Q8_5 165    164 3.402 1.061   1  3      4  4   5
# 6     Q8_6 165    165 2.576 0.970   1  2      3  3   5
# 7     Q8_7 165    163 3.528 0.983   1  3      4  4   5
# 8     Q8_8 165    164 3.256 1.144   1  2      3  4   5
