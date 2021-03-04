library(dplyr)
library(ggplot2)

## import dataset
ILTERAnswers <- readxl::read_excel("ILTER and Public Engagement_October2020_CB_AO.xlsx")
ILTERAnswers$age <- as.numeric(format(Sys.Date(), "%Y")) - ILTERAnswers$Q33
# View(ILTERAnswers)

##### 
# Survey analysis
#####
### General info
# Pool for the first part of the survey: number of answers with a completeness >= 75 % between column Q34_1-Q8_11 (first part of the survey)
# October 2020
# = 165
CSPoolFirstPart <- ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  # filter(Finished == 'True') %>% # = 136
  count()


### Research questions
# 1a e 1b Willingness - Q34_1-Q36_6
matrix1ab <- 
  ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q34_1:Q34_6) %>%
  tidyr::gather(questions, levelOfWillingness) %>% dplyr::group_by(questions, levelOfWillingness) %>% dplyr::count() %>% dplyr::ungroup() %>% tidyr::spread(questions, n) %>% 
  t() %>% 
  data.frame(row.names(.), ., row.names = NULL) %>% 
  `colnames<-`(c('level of willingness', 'Neither unwilling or willing', 'Slghtly unwilling', 'Slightly willing', 'Very unwilling', 'Very willing', 'NA')) %>% 
  .[-1,-1] %>% .[,-6] %>% 
  as.matrix() %>% 
  `rownames<-`(c(
    'Collaborations with the public on scientific research (i.e., Citizen Science)',
    'Face-to-face science discussions and activities with the public',
    'Online science discussions and activities with the public',
    'Interviews with journalists or other media professionals about science',
    'Direct interactions with government policy makers about science',
    'Any form of public engagement with science involving children or young adults (18 years or younger)'
  )) %>% 
  reshape2::melt() 

matrix1ab$Var2 <- factor(matrix1ab$Var2, levels = c('Very willing', 'Slightly willing', 'Neither unwilling or willing', 'Slghtly unwilling', 'Very unwilling'))
matrix1ab <- matrix1ab[matrix1ab$value!=0,]
matrix1ab <- matrix1ab[!is.na(matrix1ab$value),]

ggplot(matrix1ab, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = as.numeric(value))) + 
  scale_fill_gradient(low = "grey90", high = "red4", na.value = "grey10", guide = "colourbar") +
  labs(x = "Level of willingness", y = "Type of PE activities") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  labs(fill = "n of answers") +
  theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 0.3),
                          axis.text.y = element_text(size = 8),
                          plot.title = element_text(size = 11))
ggsave("./images1stPart/1a_b.png", dpi = 400)
# 1c
# Q31 Role
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q34_1:Q34_6, Q31)) %>% 
  tidyr::gather(questions, levelOfWillingness, Q34_1:Q34_6) %>% 
  filter(levelOfWillingness == 'Very\r\nwilling' | levelOfWillingness == 'Slightly \r\nwilling') %>% 
  filter(!is.na(Q31)) %>% 
  group_by(Q31, questions) %>% 
  tally() %>%
  replace(. == 'Q34_1', 'Collaborations with the public on scientific research (i.e., Citizen Science)') %>% 
  replace(. == 'Q34_2', 'Face-to-face science discussions and activities with the public') %>% 
  replace(. == 'Q34_3', 'Online science discussions and activities with the public') %>% 
  replace(. == 'Q34_4', 'Interviews with journalists or other media professionals about science') %>% 
  replace(. == 'Q34_5', 'Direct interactions with government policy makers about science') %>% 
  replace(. == 'Q34_6', 'Any form of public engagement with science involving children or young adults (18 years or younger)') %>% 
  ggplot2::ggplot(mapping = aes(x = Q31, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Role") + ggplot2::ylab("n of responces 'Very willing' + 'Slightly willing'") +
  labs(fill = "Type of PE activities") +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/1c_role.png", dpi = 400)
# Q32 Career Level
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q34_1:Q34_6, Q32)) %>% 
  tidyr::gather(questions, levelOfWillingness, Q34_1:Q34_6) %>% 
  filter(levelOfWillingness == 'Very\r\nwilling' | levelOfWillingness == 'Slightly \r\nwilling') %>% 
  filter(!is.na(Q32)) %>% 
  group_by(Q32, questions) %>% 
  tally() %>%
  replace(. == 'Q34_1', 'Collaborations with the public on scientific research (i.e., Citizen Science)') %>% 
  replace(. == 'Q34_2', 'Face-to-face science discussions and activities with the public') %>% 
  replace(. == 'Q34_3', 'Online science discussions and activities with the public') %>% 
  replace(. == 'Q34_4', 'Interviews with journalists or other media professionals about science') %>% 
  replace(. == 'Q34_5', 'Direct interactions with government policy makers about science') %>% 
  replace(. == 'Q34_6', 'Any form of public engagement with science involving children or young adults (18 years or younger)') %>% 
  ggplot2::ggplot(mapping = aes(x = Q32, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Career Level") + ggplot2::ylab("n of responces 'Very willing' + 'Slightly willing'") +
  labs(fill = "Type of PE activities") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/1c_careerLevel.png", dpi = 400)
# Q33 Age
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q34_1:Q34_6, age)) %>% 
  mutate(decade = floor(age/10)*10) %>% 
  tidyr::gather(questions, levelOfWillingness, Q34_1:Q34_6) %>% 
  filter(levelOfWillingness == 'Very\r\nwilling' | levelOfWillingness == 'Slightly \r\nwilling') %>% 
  filter(!is.na(age)) %>% select(-age) %>%
  group_by(decade, questions) %>% 
  tally() %>%
  replace(. == 'Q34_1', 'Collaborations with the public on scientific research (i.e., Citizen Science)') %>% 
  replace(. == 'Q34_2', 'Face-to-face science discussions and activities with the public') %>% 
  replace(. == 'Q34_3', 'Online science discussions and activities with the public') %>% 
  replace(. == 'Q34_4', 'Interviews with journalists or other media professionals about science') %>% 
  replace(. == 'Q34_5', 'Direct interactions with government policy makers about science') %>% 
  replace(. == 'Q34_6', 'Any form of public engagement with science involving children or young adults (18 years or younger)') %>% 
  ggplot2::ggplot(mapping = aes(x = as.character(decade), y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Age") + ggplot2::ylab("n of responces 'Very willing' + 'Slightly willing'") +
  labs(fill = "Type of PE activities") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/1c_age.png", dpi = 400)
# Q35 Gender
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q34_1:Q34_6, Q35)) %>% 
  tidyr::gather(questions, levelOfWillingness, Q34_1:Q34_6) %>% 
  filter(levelOfWillingness == 'Very\r\nwilling' | levelOfWillingness == 'Slightly \r\nwilling') %>% 
  filter(!is.na(Q35)) %>%
  group_by(Q35, questions) %>% 
  tally() %>%
  replace(. == 'Q34_1', 'Collaborations with the public on scientific research (i.e., Citizen Science)') %>% 
  replace(. == 'Q34_2', 'Face-to-face science discussions and activities with the public') %>% 
  replace(. == 'Q34_3', 'Online science discussions and activities with the public') %>% 
  replace(. == 'Q34_4', 'Interviews with journalists or other media professionals about science') %>% 
  replace(. == 'Q34_5', 'Direct interactions with government policy makers about science') %>% 
  replace(. == 'Q34_6', 'Any form of public engagement with science involving children or young adults (18 years or younger)') %>% 
  ggplot2::ggplot(mapping = aes(x = Q35, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Age") + ggplot2::ylab("n of responces 'Very willing' + 'Slightly willing'") +
  labs(fill = "Type of PE activities") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/1c_gender.png", dpi = 400)


# Q57 Communication Objectives
# 2a
matrix2a <- 
  ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(Q57_1:Q57_6) %>%
  tidyr::gather(questions, levelOfImportance) %>% dplyr::group_by(questions, levelOfImportance) %>% dplyr::count() %>% dplyr::ungroup() %>% tidyr::spread(questions, n) %>% 
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

matrix2a$Var2 <- factor(matrix2a$Var2, levels = c('Very high importance', 'High importance', 'Moderate importance', 'Low importance', 'Very low importance'))
matrix2a <- matrix2a[matrix2a$value!=0,]
matrix2a <- matrix2a[!is.na(matrix2a$value),]

ggplot(matrix2a, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = as.numeric(value))) + 
  scale_fill_gradient(low = "grey90", high = "red4", na.value = "grey10", guide = "colourbar") +
  labs(x = "Level of importance", y = "Communication objectives") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  labs(fill = "n of answers") +
  theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 0.3),
                          axis.text.y = element_text(size = 8),
                          plot.title = element_text(size = 11))
ggsave("./images1stPart/2a.png", dpi = 400)
# 2b
# Q31 Role
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, Q31)) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q31)) %>% 
  replace(. == 'Q57_1', 'Traditions objectives') %>% 
  replace(. == 'Q57_2', 'Traditions objectives') %>% 
  replace(. == 'Q57_3', 'Traditions objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives') %>%
  group_by(Q31, questions) %>% 
  tally() %>%
  ggplot2::ggplot(mapping = aes(x = Q31, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Role") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Communication objectives") +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/2b_role.png", dpi = 400)
# Q32 Career Level
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, Q32)) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q32)) %>% 
  group_by(Q32, questions) %>% 
  tally() %>%
  replace(. == 'Q57_1', 'Traditions objectives') %>% 
  replace(. == 'Q57_2', 'Traditions objectives') %>% 
  replace(. == 'Q57_3', 'Traditions objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives') %>% 
  ggplot2::ggplot(mapping = aes(x = Q32, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Career Level") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Communication objectives") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/2b_careerLevel.png", dpi = 400)
# Q33 Age
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, age)) %>% 
  mutate(decade = floor(age/10)*10) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(age)) %>% select(-age) %>%
  group_by(decade, questions) %>% 
  tally() %>%
  replace(. == 'Q57_1', 'Traditions objectives') %>% 
  replace(. == 'Q57_2', 'Traditions objectives') %>% 
  replace(. == 'Q57_3', 'Traditions objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives') %>% 
  ggplot2::ggplot(mapping = aes(x = as.character(decade), y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Age") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Communication objectives") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/2b_age.png", dpi = 400)
# Q35 Gender
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q57_1:Q57_6, Q35)) %>% 
  tidyr::gather(questions, importance, Q57_1:Q57_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(Q35)) %>%
  group_by(Q35, questions) %>% 
  tally() %>%
  replace(. == 'Q57_1', 'Traditions objectives') %>% 
  replace(. == 'Q57_2', 'Traditions objectives') %>% 
  replace(. == 'Q57_3', 'Traditions objectives') %>% 
  replace(. == 'Q57_4', 'Expanded objectives') %>% 
  replace(. == 'Q57_5', 'Expanded objectives') %>% 
  replace(. == 'Q57_6', 'Expanded objectives') %>% 
  ggplot2::ggplot(mapping = aes(x = Q35, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Age") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Communication objectives") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/2b_gender.png", dpi = 400)
#2c
ILTERAnswers %>% 
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


# Q6 Reasons
# 3a
matrix3a <- 
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

matrix3a$Var2 <- factor(matrix3a$Var2, levels = c('Very high importance', 'High importance', 'Moderate importance', 'Little importance', 'Very low importance'))
matrix3a <- matrix3a[matrix3a$value!=0,]
matrix3a <- matrix3a[!is.na(matrix3a$value),]

ggplot(matrix3a, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = as.numeric(value))) + 
  scale_fill_gradient(low = "grey90", high = "red4", na.value = "grey10", guide = "colourbar") +
  labs(x = "Level of importance", y = "Communication objectives") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  labs(fill = "n of answers") +
  theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 0.3),
                          axis.text.y = element_text(size = 8),
                          plot.title = element_text(size = 11))
ggsave("./images1stPart/3a.png", dpi = 400)
# 3b
# Q31 Role
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, Q31)) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q31)) %>% 
  replace(. == 'Q6_1', 'Traditions reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') %>%
  group_by(Q31, questions) %>% 
  tally() %>%
  ggplot2::ggplot(mapping = aes(x = Q31, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Role") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Reasons") +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/3b_role.png", dpi = 400)
# Q32 Career Level
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, Q32)) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>% 
  filter(!is.na(Q32)) %>% 
  group_by(Q32, questions) %>% 
  tally() %>%
  replace(. == 'Q6_1', 'Traditions reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') %>%
  ggplot2::ggplot(mapping = aes(x = Q32, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Career Level") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Reasons") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/3b_careerLevel.png", dpi = 400)
# Q33 Age
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, age)) %>% 
  mutate(decade = floor(age/10)*10) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(age)) %>% select(-age) %>%
  group_by(decade, questions) %>% 
  tally() %>%
  replace(. == 'Q6_1', 'Traditions reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') %>%
  ggplot2::ggplot(mapping = aes(x = as.character(decade), y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Age") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Reasons") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/3b_age.png", dpi = 400)
# Q35 Gender
ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q6_1:Q6_6, Q35)) %>% 
  tidyr::gather(questions, importance, Q6_1:Q6_6) %>% 
  filter(importance == 'Very high\r\nimportance' | importance == 'High\r\nimportance') %>%
  filter(!is.na(Q35)) %>%
  group_by(Q35, questions) %>% 
  tally() %>%
  replace(. == 'Q6_1', 'Traditions reasons') %>% 
  replace(. == 'Q6_2', 'Expanded reasons') %>% 
  replace(. == 'Q6_3', 'Traditional reasons') %>% 
  replace(. == 'Q6_4', 'Traditional reasons') %>% 
  replace(. == 'Q6_5', 'Traditional reasons') %>% 
  replace(. == 'Q6_6', 'Expanded reasons') %>%
  replace(. == 'Q6_7', 'Expanded reasons') %>%
  replace(. == 'Q6_8', 'Expanded reasons') %>%
  ggplot2::ggplot(mapping = aes(x = Q35, y = n, fill = questions)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::xlab("ILTER Age") + ggplot2::ylab("n of responces 'Very high importance' + 'High importance'") +
  labs(fill = "Roles") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "vertical")
ggsave("./images1stPart/3b_gender.png", dpi = 400)


# Q8 Barriers
# 4a
matrix4a <- 
  ILTERAnswers %>% 
  filter(as.numeric(ProgressFirstPart) >= 75) %>% 
  select(c(Q8_1:Q8_8)) %>%
  tidyr::gather(questions, levelOfAgreement) %>% dplyr::group_by(questions, levelOfAgreement) %>% dplyr::count() %>% dplyr::ungroup() %>% tidyr::spread(questions, n) %>% 
  t() %>% 
  data.frame(row.names(.), ., row.names = NULL) %>% 
  `colnames<-`(c('level of agreement', 'Agree', 'Disagree', 'Neither agree nor disagree', 'Strongly agree', 'Strongly disagree', 'NA')) %>% 
  .[-1,-1] %>% select(-'NA') %>% 
  as.matrix() %>% 
  `rownames<-`(c(
    'The public does not have the necessary knowledge or skills to contribute to scientific research',
    'It is too difficult or time-consuming to teach the public the necessary knowledge or skills to contribute to scientific research',
    'It is too difficult or time-consuming to validate data collected or classified by the public',
    'The public is not interested in helping with science research',
    'It is difficult to create long-term stable relationships with the public, which are necessary to conduct scientific research',
    'It is not possible to acknowledge citizen science volunteers’ contribution in grants, presentations, and publications',
    'Scientists do not have any or enough support to start and run a citizen science project',
    'Scientists do not get credit or acknowledgement for their work in citizen science'
  )) %>% 
  reshape2::melt() 

matrix4a$Var2 <- factor(matrix4a$Var2, levels = c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree'))
matrix4a <- matrix4a[matrix4a$value!=0,]
matrix4a <- matrix4a[!is.na(matrix4a$value),]

ggplot(matrix4a, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = as.numeric(value))) + 
  scale_fill_gradient(low = "grey90", high = "red4", na.value = "grey10", guide = "colourbar") +
  labs(x = "Level of importance", y = "Communication objectives") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  labs(fill = "n of answers") +
  theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 0.3),
                          axis.text.y = element_text(size = 8),
                          plot.title = element_text(size = 11))
ggsave("./images1stPart/4a.png", dpi = 400)


# 5c
# Q34_1
participationVSWillingness <- ILTERAnswers %>% 
  select(c(Q10, Q34_1)) %>% # 296
  filter(Q34_1 == 'Very\r\nwilling' | Q34_1 == 'Slightly \r\nwilling') %>% # 149
  # filter(Q10 == 0, na.rm = FALSE) %>% # 54
  # filter(Q10 > 0) %>% # 68
  # group_by(Q34_1) %>% 
  count(is.na(Q10) | Q10 == 0) %>% 
  mutate(freq = (n/sum(n))*100)

blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size = 14, face = "bold")
  )

graphics::pie(
  participationVSWillingness$freq, 
  label= c('Willingness with initiatives (57%)', 'Willingness no initiatives (44.3%)'), 
  edges = 10
)
# TRUE = Willingness no initiatives
# FALSE = Willingness with initiatives

# ggplot2::ggplot(participationVSWillingness, aes(x = "", y = freq, fill = `is.na(Q10) | Q10 == 0`)) +
#   ggplot2::geom_bar(width = 1, stat = "identity") + 
#   ggplot2::coord_polar("y", start = 0) +
#   ggplot2::scale_fill_brewer("Blues") + blank_theme +
#   ggplot2::theme(axis.text.x = element_blank()) +
#   ggplot2::geom_text(aes(y = freq/3 + c(0, cumsum(freq)[-length(freq)]), 
#                          label = round(freq, 1), size = 5))




