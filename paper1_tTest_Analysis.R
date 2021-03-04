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

################################################# 2a
################################################# Q57 Communication Objectives
a2_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q57_1:Q57_6) %>%
  tidyr::gather(questions, levelOfImportance) %>% 
  dplyr::group_by(questions, levelOfImportance) %>% 
  dplyr::count() %>% dplyr::ungroup() %>% tidyr::spread(questions, n) %>% 
  t() %>% 
  data.frame(row.names(.), ., row.names = NULL) %>% 
  `colnames<-`(c('level of importance', 'High importance', 'Low importance', 'Moderate importance', 'Very high importance', 'Very low importance', 'NA')) %>% 
  .[-1,-1] %>% .[,-6] %>% 
  as.matrix() %>% 
  `rownames<-`(c(
    'Helping to inform people about scientific issues and processes, or correcting misinformation',
    'Getting people interested or excited about science',
    'Showing the scientific community\'s ability to help solve real problems',
    'Showing that the scientific community is listening to what others think about scientific issues',
    'Demonstrating the scientific community\'s openness and transparency',
    'Showing that the scientific community cares about societal well-being'
  )) %>% 
  reshape2::melt() 

a2_Dataset_bis <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q57_1:Q57_6) %>%
  replace(.=="Very high\r\nimportance", "5") %>% 
  replace(.=="High\r\nimportance", "4") %>% 
  replace(.=="Moderate\r\nimportance\r\n", "3") %>% 
  replace(.=="Low\r\nimportance", "2") %>% 
  replace(.=="Very low\r\nimportance", "1") %>% 
  tidyr::gather(questions, levelOfImportance) %>% 
  replace(. == 'Q57_1', 'Traditional objectives') %>% 
  replace(. == 'Q57_2', 'Traditional objectives') %>% 
  replace(. == 'Q57_3', 'Traditional objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives') %>%
  mutate(levelOfImportance = as.integer(levelOfImportance)) # %>% 
# filter(levelOfImportance == 'Very high\r\nimportance' | levelOfImportance == 'High\r\nimportance')
a2_Dataset_bis
### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded objectives and Traditional objectives)
a2_Dataset_bis_Percentage <- a2_Dataset_bis %>% 
  dplyr::group_by(questions) %>% 
  dplyr::summarise(
    sumOfWeight = sum(levelOfImportance, na.rm=TRUE),
    mean = mean(levelOfImportance, na.rm = TRUE),
    sd = sd(levelOfImportance, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    percentage = (sumOfWeight / sum(sumOfWeight))*100
  )
a2_Dataset_bis_Percentage

## Plot n by questions and color by questions
ggpubr::ggboxplot(
  a2_Dataset_bis, x = "questions", y = "levelOfImportance", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Objectives"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional objectives
with(a2_Dataset_bis, shapiro.test(n[questions == "Traditional objectives"])) # p = 0.201
# Shapiro-Wilk normality test for Expanded objectives
with(a2_Dataset_bis, shapiro.test(n[questions == "Expanded objectives"])) # p = 0.4302
# the two p-values are greater than the significance level 0.05 implying that the distribution of the data are not significantly different from the normal distribution. 
# In other words, we can assume the normal distribution of the data.
# If the data are not normally distributed, it’s recommended to use the non parametric two-samples Wilcoxon rank test.

## unpaired two-samples t-test
a2_res_tTest <- t.test(levelOfImportance ~ questions, data = a2_Dataset_bis, var.equal = TRUE)
a2_res_tTest
# The p-value of the test is 0.161, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.161

## unpaired two-samples Wilcoxon test
a2_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = a2_Dataset_bis,
  exact = FALSE
)
a2_res_Wilcoxon
# The p-value of the test is 0.229, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.229.

################################################# 2b
################################################# Q31 Role
b2_Q31Role_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, Q31)) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  # filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q31)) %>% 
  replace(. == 'Q57_1', 'Traditional objectives') %>% 
  replace(. == 'Q57_2', 'Traditional objectives') %>% 
  replace(. == 'Q57_3', 'Traditional objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives') %>%
  group_by(Q31, questions) %>% 
  tally() 

################################################# Q32 Career Level
b2_Q32CareerLevel_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, Q32)) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q32)) %>% 
  replace(. == 'Q57_1', 'Traditional objectives') %>% 
  replace(. == 'Q57_2', 'Traditional objectives') %>% 
  replace(. == 'Q57_3', 'Traditional objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives') %>% 
  group_by(Q32, questions, importance) %>% 
  tally()

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded objectives and Traditional objectives)
b2_Q32CareerLevel_Summarise <- b2_Q32CareerLevel_Dataset %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  b2_Q32CareerLevel_Dataset, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Objectives"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional objectives
with(b2_Q32CareerLevel_Dataset, shapiro.test(n[questions == "Traditional objectives"])) # p = 0.005486
# Shapiro-Wilk normality test for Expanded objectives
with(b2_Q32CareerLevel_Dataset, shapiro.test(n[questions == "Expanded objectives"])) # p = 0.01569
# the two p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from the normal distribution. 
# In other words, we can assume the not normal distribution of the data.

## unpaired two-samples t-test
b2_Q32CareerLevel_res_tTest <- t.test(n ~ questions, data = b2_Q32CareerLevel_Dataset, var.equal = TRUE)
b2_Q32CareerLevel_res_tTest
# The p-value of the test is 0.9181, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.9181.

## unpaired two-samples Wilcoxon test
b2_Q32CareerLevel_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = b2_Q32CareerLevel_Dataset,
  exact = FALSE
)
b2_Q32CareerLevel_res_Wilcoxon
# The p-value of the test is 0.766, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.766
################################################# Q33 Age
b2_Q33Age_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, age)) %>% 
  mutate(decade = floor(age/10)*10) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(age)) %>% select(-age) %>%
  group_by(decade, questions, importance) %>% 
  tally() %>%
  replace(. == 'Q57_1', 'Traditional objectives') %>% 
  replace(. == 'Q57_2', 'Traditional objectives') %>% 
  replace(. == 'Q57_3', 'Traditional objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives')

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded objectives and Traditional objectives)
b2_Q33Age_Summarise <- b2_Q33Age_Dataset %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  b2_Q33Age_Dataset, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Objectives"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional objectives
with(b2_Q33Age_Dataset, shapiro.test(n[questions == "Traditional objectives"])) # p = 0.008673
# Shapiro-Wilk normality test for Expanded objectives
with(b2_Q33Age_Dataset, shapiro.test(n[questions == "Expanded objectives"])) # p = 0.002708
# the two p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from the normal distribution. 
# In other words, we can assume the not normal distribution of the data.

## unpaired two-samples t-test
b2_Q33Age_res_tTest <- t.test(n ~ questions, data = b2_Q33Age_Dataset, var.equal = TRUE)
b2_Q33Age_res_tTest
# The p-value of the test is 0.1974, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.9181.

## unpaired two-samples Wilcoxon test
b2_Q33Age_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = b2_Q33Age_Dataset,
  exact = FALSE
)
b2_Q33Age_res_Wilcoxon
# The p-value of the test is 0.2698, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.2698
################################################# Q35 Gender
b2_Q35Gender_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, Q35)) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(Q35)) %>%
  group_by(Q35, questions, importance) %>% 
  tally() %>%
  replace(. == 'Q57_1', 'Traditional objectives') %>% 
  replace(. == 'Q57_2', 'Traditional objectives') %>% 
  replace(. == 'Q57_3', 'Traditional objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives')

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded objectives and Traditional objectives)
b2_Q35Gender_Summarise <- b2_Q35Gender_Dataset %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  b2_Q35Gender_Dataset, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Objectives"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional objectives
with(b2_Q35Gender_Dataset, shapiro.test(n[questions == "Traditional objectives"])) # p = 0.03866
# Shapiro-Wilk normality test for Expanded objectives
with(b2_Q35Gender_Dataset, shapiro.test(n[questions == "Expanded objectives"])) # p = 0.116
# the two p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from the normal distribution. 
# In other words, we can assume the not normal distribution of the data.
## unpaired two-samples t-test
b2_Q35Gender_res_tTest <- t.test(n ~ questions, data = b2_Q35Gender_Dataset, var.equal = TRUE)
b2_Q35Gender_res_tTest
# The p-value of the test is 0.0805, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.0805

## unpaired two-samples Wilcoxon test
b2_Q35Gender_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = b2_Q35Gender_Dataset,
  exact = FALSE
)
b2_Q35Gender_res_Wilcoxon
# The p-value of the test is 0.068, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.068

#####
tTestTable <- purrr::map_df(
  list(
    a2_res_tTest,
    b2_Q32CareerLevel_res_tTest,
    b2_Q33Age_res_tTest,
    b2_Q35Gender_res_tTest
  ),
  broom::tidy
) %>% 
  dplyr::rename('Test Statistic' = statistic, 'p value' = p.value, DF = parameter) %>% 
  dplyr::select('Test Statistic', 'p value', DF)
tTestTable

wilcoxonTestTable <- purrr::map_df(
  list(
    a2_res_Wilcoxon,
    b2_Q32CareerLevel_res_Wilcoxon,
    b2_Q33Age_res_Wilcoxon,
    b2_Q35Gender_res_Wilcoxon
  ),
  broom::tidy
) %>% 
  dplyr::rename('p value' = p.value, DF = statistic) %>% 
  dplyr::select('p value', DF)
wilcoxonTestTable


################################################# 2c
c2_dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q34_1, Q57_1:Q57_6)) %>% 
  filter(Q34_1 == 'Slightly \r\nwilling' | Q34_1 == 'Very\r\nwilling') %>%
  filter(Q57_1 == 'Very high\r\nimportance' | Q57_1 == 'High\r\nimportance') %>%
  filter(Q57_2 == 'Very high\r\nimportance' | Q57_2 == 'High\r\nimportance') %>%
  filter(Q57_3 == 'Very high\r\nimportance' | Q57_3 == 'High\r\nimportance') %>%
  filter(Q57_4 == 'Very high\r\nimportance' | Q57_4 == 'High\r\nimportance') %>%
  filter(Q57_5 == 'Very high\r\nimportance' | Q57_5 == 'High\r\nimportance') %>%
  filter(Q57_6 == 'Very high\r\nimportance' | Q57_6 == 'High\r\nimportance') %>%
  # tidyr::gather(questions, level, c(Q34_1, Q57_1:Q57_6)) %>% 
  # replace(. == 'Q34_1', 'Citizen Science') %>% 
  # replace(. == 'Q57_1', 'Traditions objectives') %>% 
  # replace(. == 'Q57_2', 'Traditions objectives') %>% 
  # replace(. == 'Q57_3', 'Traditions objectives') %>% 
  # replace(. == 'Q57_4', 'Expanded objectives') %>% 
  # replace(. == 'Q57_5', 'Expanded objectives') %>% 
  # replace(. == 'Q57_6', 'Expanded objectives') %>% 
  group_by(Q34_1) %>% 
  tally()


################################################# 3a
################################################# Q6 Reasons
a3_Dataset <- 
  ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q6_1:Q6_8) %>%
  tidyr::gather(questions, levelOfImportance) %>% dplyr::group_by(questions, levelOfImportance) %>% dplyr::count() %>% dplyr::ungroup() %>% tidyr::spread(questions, n) %>% 
  t() %>% 
  data.frame(row.names(.), ., row.names = NULL) %>% 
  `colnames<-`(c('level of importance', 'High importance', 'Little importance', 'Moderate importance', 'Very high importance', 'Very low importance', 'NA')) %>% 
  .[-1,-1] %>% .[,-6] %>% 
  as.matrix() %>% 
  `rownames<-`(c(
    'Get help from the public by having them collect or classify data',
    'Get help from the public in ways that are not limited to data collection and classification',
    'Make a grant proposal more competitive and appealing to funders by including citizen science',
    'Educate the public on how science research is conducted',
    'Educate the public on environmental issues',
    'Bring in perspectives and ideas from the public that can inform scientific research',
    'Build relationships between scientists and the public who live and work near LTER Sites or LTSER Platforms',
    'Have greater influence on policy by collaborating with the public on scientific research'
  )) %>% 
  reshape2::melt() 

a3_Dataset_bis <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q6_1:Q6_8) %>%
  tidyr::gather(questions, levelOfImportance) %>% 
  dplyr::group_by(questions, levelOfImportance) %>% 
  dplyr::count() %>% dplyr::ungroup() %>% 
  replace(. == 'Q6_1', 'Traditional reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') %>%
  filter(levelOfImportance == 'Very high\r\nimportance' | levelOfImportance == 'High\r\nimportance') %>% 
  group_by(questions)

a3_Dataset_bis_Percentage <- a3_Dataset_bis %>% 
  dplyr::summarise(
    n = sum(n, na.rm = TRUE)
  ) %>%
  dplyr::mutate(percentage = (n / sum(n))*100)

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded objectives and Traditional objectives)
a2_Dataset_bis_Summarise <- a3_Dataset_bis %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  a3_Dataset_bis, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Objectives"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional reasons
with(a3_Dataset_bis, shapiro.test(n[questions == "Traditional reasons"])) # p = 0.9594
# Shapiro-Wilk normality test for Expanded reasons
with(a3_Dataset_bis, shapiro.test(n[questions == "Expanded reasons"])) # p = 0.1883
# the two p-values are greater than the significance level 0.05 implying that the distribution of the data are not significantly different from the normal distribution. 
# In other words, we can assume the normal distribution of the data.
# If the data are not normally distributed, it’s recommended to use the non parametric two-samples Wilcoxon rank test.

## unpaired two-samples t-test
a3_res_tTest <- t.test(n ~ questions, data = a3_Dataset_bis, var.equal = TRUE)
a3_res_tTest
# The p-value of the test is 0.8897, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional reasons median is not significantly different from Expanded reasons median with a p-value = 0.8897

## unpaired two-samples Wilcoxon test
a3_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = a3_Dataset_bis,
  exact = FALSE
)
a3_res_Wilcoxon
# The p-value of the test is 0.8747, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.8747

################################################# 3b
################################################# Q31 Role
b3_Q31Role_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, Q31)) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q31)) %>% 
  replace(. == 'Q6_1', 'Traditional reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') %>%
  group_by(Q31, questions, importance) %>% 
  tally()

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded reasons and Traditional reasons)
b3_Q31Role_Summarise <- b3_Q31Role_Dataset %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  b3_Q31Role_Dataset, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Reasons"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional reasons
with(b3_Q31Role_Dataset, shapiro.test(n[questions == "Traditional reasons"])) # p = 0.004126
# Shapiro-Wilk normality test for Expanded reasons
with(b3_Q31Role_Dataset, shapiro.test(n[questions == "Expanded reasons"])) # p = 0.01283
# the two p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from the normal distribution. 

## unpaired two-samples t-test
b3_Q31Role_res_tTest <- t.test(n ~ questions, data = b3_Q31Role_Dataset, var.equal = TRUE)
b3_Q31Role_res_tTest
# The p-value of the test is 0.0513, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional reasons median is not significantly different from Expanded reasons median with a p-value = 0.0513

## unpaired two-samples Wilcoxon test
b3_Q31Role_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = b3_Q31Role_Dataset,
  exact = FALSE
)
b3_Q31Role_res_Wilcoxon
# The p-value of the test is 0.02051, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.02051

################################################# Q32 Career Level
b3_Q32CareerLevel_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, Q32)) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q32)) %>% 
  group_by(Q32, questions, importance) %>% 
  tally() %>%
  replace(. == 'Q6_1', 'Traditional reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') 

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded reasons and Traditional reasons)
b3_Q32CareerLevel_Summarise <- b3_Q32CareerLevel_Dataset %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  b3_Q32CareerLevel_Dataset, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Reasons"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional reasons
with(b3_Q32CareerLevel_Dataset, shapiro.test(n[questions == "Traditional reasons"])) # p = 4.612e-06
# Shapiro-Wilk normality test for Expanded reasons
with(b3_Q32CareerLevel_Dataset, shapiro.test(n[questions == "Expanded reasons"])) # p = 0.002635
# the two p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from the normal distribution. 

## unpaired two-samples t-test
b3_Q32CareerLevel_res_tTest <- t.test(n ~ questions, data = b3_Q32CareerLevel_Dataset, var.equal = TRUE)
b3_Q32CareerLevel_res_tTest
# The p-value of the test is 0.67, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional reasons median is not significantly different from Expanded reasons median with a p-value = 0.67

## unpaired two-samples Wilcoxon test
b3_Q32CareerLevel_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = b3_Q32CareerLevel_Dataset,
  exact = FALSE
)
b3_Q32CareerLevel_res_Wilcoxon
# The p-value of the test is 0.8827, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.8827

################################################# Q33 Age
b3_Q33Age_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, age)) %>% 
  mutate(decade = floor(age/10)*10) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(age)) %>% select(-age) %>%
  group_by(decade, questions, importance) %>% 
  tally() %>%
  replace(. == 'Q6_1', 'Traditional reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') 

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded reasons and Traditional reasons)
b3_Q33Age_Summarise <- b3_Q33Age_Dataset %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  b3_Q33Age_Dataset, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Reasons"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional reasons
with(b3_Q33Age_Dataset, shapiro.test(n[questions == "Traditional reasons"])) # p = 0.001322
# Shapiro-Wilk normality test for Expanded reasons
with(b3_Q33Age_Dataset, shapiro.test(n[questions == "Expanded reasons"])) # p = 0.0001737
# the two p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from the normal distribution. 

## unpaired two-samples t-test
b3_Q33Age_res_tTest <- t.test(n ~ questions, data = b3_Q33Age_Dataset, var.equal = TRUE)
b3_Q33Age_res_tTest
# The p-value of the test is 0.08311, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional reasons median is not significantly different from Expanded reasons median with a p-value = 0.08311

## unpaired two-samples Wilcoxon test
b3_Q33Age_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = b3_Q33Age_Dataset,
  exact = FALSE
)
b3_Q33Age_res_Wilcoxon
# The p-value of the test is 0.06531, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.06531

################################################# Q35 Gender
b3_Q35Gender_Dataset <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, Q35)) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(Q35)) %>%
  group_by(Q35, questions, importance) %>% 
  tally() %>%
  replace(. == 'Q6_1', 'Traditional reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons')

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded reasons and Traditional reasons)
b3_Q35Gender_Summarise <- b3_Q35Gender_Dataset %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  b3_Q35Gender_Dataset, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Reasons"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional reasons
with(b3_Q35Gender_Dataset, shapiro.test(n[questions == "Traditional reasons"])) # p = 0.1743
# Shapiro-Wilk normality test for Expanded reasons
with(b3_Q35Gender_Dataset, shapiro.test(n[questions == "Expanded reasons"])) # p = 0.4308
# the two p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from the normal distribution. 

## unpaired two-samples t-test
b3_Q35Gender_res_tTest <- t.test(n ~ questions, data = b3_Q35Gender_Dataset, var.equal = TRUE)
b3_Q35Gender_res_tTest
# The p-value of the test is 0.2759, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional reasons median is not significantly different from Expanded reasons median with a p-value = 0.2759

## unpaired two-samples Wilcoxon test
b3_Q35Gender_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = b3_Q35Gender_Dataset,
  exact = FALSE
)
b3_Q35Gender_res_Wilcoxon
# The p-value of the test is 0.2969, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.2969

#####
tTestTable <- purrr::map_df(
  list(
    a3_res_tTest,
    b3_Q32CareerLevel_res_tTest,
    b3_Q33Age_res_tTest,
    b3_Q35Gender_res_tTest
  ),
  broom::tidy
) %>% 
  dplyr::rename('Test Statistic' = statistic, 'p value' = p.value, DF = parameter) %>% 
  dplyr::select('Test Statistic', 'p value', DF)
tTestTable

wilcoxonTestTable <- purrr::map_df(
  list(
    a3_res_Wilcoxon,
    b3_Q32CareerLevel_res_Wilcoxon,
    b3_Q33Age_res_Wilcoxon,
    b3_Q35Gender_res_Wilcoxon
  ),
  broom::tidy
) %>% 
  dplyr::rename('p value' = p.value, DF = statistic) %>% 
  dplyr::select('p value', DF)
wilcoxonTestTable

################################################# 7e
################################################# Q21 Roles
e7_Dataset <-
  ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(Q21_1:Q21_13) %>%
  tidyr::gather(questions, typeOfInvol) %>% dplyr::group_by(questions, typeOfInvol) %>% dplyr::count() %>% dplyr::ungroup() %>% tidyr::spread(questions, n) %>% 
  t() %>% 
  data.frame(row.names(.), ., row.names = NULL) %>% 
  `colnames<-`(c('Activity in CS', 'High involvement', 'Moderate involvement', 'Not at all involved', 'Very high involvement', 'Very little involvement', 'NA')) %>% 
  .[-1,-1] %>% .[,-6] %>% 
  as.matrix() %>% 
  `rownames<-`(c(
    'Help define research questions',
    'Help interpret data and draw conclusions',
    'Help disseminate conclusions',
    'Help translate the results into action',
    'Help discuss results and ask new questions',
    'Help gather information and resources for research',
    'Help develop hypotheses',
    'Help design data collection methodologies',
    'Help collect samples or record data',
    'Help classify data',
    'Help process samples',
    'Help validate data', 
    'Help analyze data'
  )) %>% 
  reshape2::melt() 

e7_Dataset_bis <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(Q21_1:Q21_13) %>%
  tidyr::gather(questions, levelOfInvolvement) %>% 
  dplyr::group_by(questions, levelOfInvolvement) %>% 
  dplyr::count() %>% dplyr::ungroup() %>% 
  replace(. == 'Q21_1', 'Expanded roles') %>% 
  replace(. == 'Q21_2', 'Expanded roles') %>% 
  replace(. == 'Q21_3', 'Expanded roles') %>% 
  replace(. == 'Q21_4', 'Expanded roles') %>% 
  replace(. == 'Q21_5', 'Traditional roles') %>% 
  replace(. == 'Q21_6', 'Traditional roles') %>% 
  replace(. == 'Q21_7', 'Expanded roles') %>% 
  replace(. == 'Q21_8', 'Expanded roles') %>% 
  replace(. == 'Q21_9', 'Expanded roles') %>% 
  replace(. == 'Q21_10', 'Expanded roles') %>% 
  replace(. == 'Q21_11', 'Expanded roles') %>% 
  replace(. == 'Q21_12', 'Expanded roles') %>% 
  replace(. == 'Q21_13', 'Expanded roles') %>% 
  filter(levelOfInvolvement == 'Very high\r\ninvolvement' | levelOfInvolvement == 'High\r\ninvolvement') %>% 
  group_by(questions)

e7_Dataset_bis_Percentage <- e7_Dataset_bis %>% 
  dplyr::summarise(
    n = sum(n, na.rm = TRUE)
  ) %>%
  dplyr::mutate(percentage = (n / sum(n))*100)

### Unpaired Two-Samples T-test
## Summary statistics by questions (Expanded objectives and Traditional objectives)
e7_Dataset_bis_Summarise <- e7_Dataset_bis %>% 
  group_by(questions) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )
## Plot n by questions and color by questions
ggpubr::ggboxplot(
  e7_Dataset_bis, x = "questions", y = "n", 
  color = "questions", palette = c("#00AFBB", "#E7B800"),
  ylab = "number", xlab = "Objectives"
)
## Are the data from each of the 2 groups follow a normal distribution?
# Shapiro-Wilk normality test as described at: Normality Test in R. - 
# Null hypothesis: the data are normally distributed - Alternative hypothesis: the data are not normally distributed.
# Shapiro-Wilk normality test for Traditional roles
with(e7_Dataset_bis, shapiro.test(n[questions == "Traditional roles"])) # p = 0.3091
# Shapiro-Wilk normality test for Expanded roles
with(e7_Dataset_bis, shapiro.test(n[questions == "Expanded roles"])) # p = 0.4302
# the two p-values are greater than the significance level 0.05 implying that the distribution of the data 
# are not significantly different from the normal distribution. 
# In other words, we can assume the normal distribution of the data.
# If the data are not normally distributed, it’s recommended to use the non parametric two-samples Wilcoxon rank test.

## unpaired two-samples t-test
e7_res_tTest <- t.test(n ~ questions, data = e7_Dataset_bis, var.equal = TRUE)
e7_res_tTest
# The p-value of the test is 0.01295, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional roles median is not significantly different from Expanded roles median with a p-value = 0.01295

## unpaired two-samples Wilcoxon test
e7_res_Wilcoxon <- wilcox.test(
  n ~ questions, 
  data = e7_Dataset_bis,
  exact = FALSE
)
e7_res_Wilcoxon
# The p-value of the test is 0.06188, which is more than the significance level alpha = 0.05.
# We can conclude that Traditional objectives median is not significantly different from Expanded objectives median with a p-value = 0.06188.
tTestTable <- purrr::map_df(
  list(
    e7_res_tTest
  ),
  broom::tidy
) %>% 
  dplyr::rename('Test Statistic' = statistic, 'p value' = p.value, DF = parameter) %>% 
  dplyr::select('Test Statistic', 'p value', DF)
tTestTable

wilcoxonTestTable <- purrr::map_df(
  list(
    e7_res_Wilcoxon
  ),
  broom::tidy
) %>% 
  dplyr::rename('p value' = p.value, DF = statistic) %>% 
  dplyr::select('p value', DF)
wilcoxonTestTable
