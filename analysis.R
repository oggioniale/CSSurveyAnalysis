library(dplyr)
library(ggplot2)

## import dataset
ILTERAnswers <- readxl::read_excel("ILTER and Public Engagement_October2020_CB_AO.xlsx")
# View(ILTERAnswers)

## import shape countries
worldCountries <- rgdal::readOGR("/Users/alessandrooggioni/Documents/Alessandro/IREA/GIS_WGS84/countries.shp") 
# worldCountries <- worldCountries@data %>% select(FIPS_CNTRY:CNTRY_NAME)
europeCountries <- rgdal::readOGR("/Users/alessandrooggioni/Documents/Alessandro/IREA/GIS_WGS84/countries_europe.shp")
# europeCountries <- europeCountries@data %>% select(NAME:FIPS_CODE)
View(worldCountries@data)
View(europeCountries@data)


##### 
# Survey analysis
#####

### General info
# Pool for the second part of the survey: number of answers with a completeness >= 75 % between column Q10-Q30 (second part of the survey)
# October 2020
# = 77
CSPool <- ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  count()


# number of answers with DEIMS-ID among them
# October 2020
# = 52
CSPoolWithDEIMSID <- ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q30 != 'NA') %>% 
  count()


# number of answers with LTER network information among them
# October 2020
# = 77
CSPoolWithLTERNetwork <- ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(!is.na('LTERNetwork')) %>% 
  count()


# number of persons who accessed the survey (not necessarily fisnished it)
# July 2020
# = 220
# October 2020
# = 296
totalAnswers <- ILTERAnswers %>% filter(Finished == 'True' | Finished == 'False') %>% count() 


# 750 e-mail in the mailing list
# after the second call (end July 2020) we don't have information about the number of reached e-mails addresses
(totalAnswers/750)*100


# number of persons who finished the survey (no information about the completness)
# July 2020
# = 117
# October 2020
# = 142
completeAnswers <- ILTERAnswers %>% filter(Finished == 'True') %>% count()


# number of answers with a completeness >= 50 %
# July 2020
# = 134
# October 2020
# = 163
halfOfAnswers <- ILTERAnswers %>% filter(as.numeric(Progress) >= 50) %>% count()


# number of answer without DEIMS-ID
# July 2020
# = 140
# October 2020
# = 201
noDEIMSAnswers <- ILTERAnswers %>% filter(Q30 == 'NA') %>% count() 


# number of answer with DEIMS-ID
# July 2020
# = 80
# October 2020
# = 95
DEIMSAnswers <- ILTERAnswers %>% filter(Q30 != 'NA') %>% count() 


# number of answers with countries information
# July 2020
# = 108
# October 2020
# = 130
aswersWithCountries <- ILTERAnswers %>% filter(CNTRY_NAME != 'NA')


# number of participants in CS initiative among respondents with a completeness >= 50 %
# July 2020
# = 73
# October 2020
# = 89
participationVSresponces <- ILTERAnswers %>% 
  filter(as.numeric(Progress) >= 50) %>%
  filter(Q10 > 0) %>% 
  count()


# number of participants in CS initiative among respondents
# October 2020
# = 97
participationVSresponcesAll <- ILTERAnswers %>% 
  filter(Q10 > 0) %>% 
  count()


# number of CS initiatives declared among respondents with a completeness >= 50 %
# July 2020
# = 354
# October 2020
# = 395
ILTERAnswers %>% 
  filter(as.numeric(Progress) >= 50) %>% 
  filter(Q10 > 0) %>% 
  select(Q10) %>% 
  summarise(sum(Q10))


### Research questions
# 5a
# July 2020
# = 54.5 % of respondents with a completeness >= 50 %
# October 2020
# = 54.6 % of respondents with a completeness >= 50 %
print(round((participationVSresponces/halfOfAnswers)*100, 1))


# 5b ############ POSTER #############
participationCSDifference <- ILTERAnswers %>% 
  filter(as.numeric(Progress) >= 50) %>% 
  # filter(Q10 > 0) %>%
  select(c(Q10, Q31:Q36)) #%>% View()
participationCSDifference$age <- as.numeric(format(Sys.Date(), "%Y")) - participationCSDifference$Q33
# Q31 ILTER Role
participationCSDifference %>% 
  group_by(Q31) %>% 
  # summarise(totalCSInitiative = sum(Q10)) %>% 
  count(Q31) %>% 
  filter(n > 1) %>%
  ggplot2::ggplot(ggplot2::aes(x = Q31, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = "orange") +
  ggplot2::xlab("") + ggplot2::ylab("Participants in CS initiatives") +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic()
ggsave("./images/5b_role.png", dpi = 400)
# Q32 Career Level
participationCSDifference %>% 
  group_by(Q32) %>% 
  # summarise(totalCSInitiative = sum(Q10)) %>% 
  count(Q32) %>% 
  filter(n > 1) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q32, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = "orange") +
  ggplot2::xlab("") + ggplot2::ylab("Participants in CS initiatives") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1.6, color = "white", size = 3.5)+
  ggplot2::theme_classic()
ggsave("./images/5b_careerLevel.png", dpi = 400)
# Q33 Age
participationCSDifference %>% 
  group_by(age) %>% 
  # summarise(totalCSInitiative = sum(Q10)) %>% 
  count(age) %>% 
  filter(n > 1) %>%
  mutate(decade = floor(age/10)*10) %>% 
  group_by(decade) %>% 
  summarize_all(sum) %>% 
  select(-age) %>% 
  ggplot2::ggplot(ggplot2::aes(x = decade, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = "orange") +
  ggplot2::xlab("Age") + ggplot2::ylab("Participants in CS initiatives") +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1.6, color = "white", size = 3.5)+
  ggplot2::theme_classic()
ggsave("./images/5b_age.png", dpi = 400)
# Q35 Gender
participationCSDifference %>% 
  group_by(Q35) %>% 
  # summarise(totalCSInitiative = sum(Q10)) %>% 
  count(Q35) %>% 
  filter(n > 1) %>%
  ggplot2::ggplot(ggplot2::aes(x = Q35, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = "orange") +
  ggplot2::xlab("Gender") + ggplot2::ylab("Participants in CS initiatives") +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1.6, color = "white", size = 3.5)+
  ggplot2::theme_classic()
ggsave("./images/5b_gender.png", dpi = 400)
# by country
ILTERAnswers %>%
  filter(as.numeric(Progress) >= 50) %>%
  select(c(Q10, CNTRY_NAME, LTERNetwork)) %>%
  count(CNTRY_NAME) %>%
  transmute(CNTRY_NAME, Percentage = round(n/sum(n)*100, 2))


# 5c
# Q34_1
participationVSWillingness <- ILTERAnswers %>% 
  select(c(Q10, Q34_1:Q34_6)) %>% # 220
  filter(Q34_1 == 'Very\r\nwilling' | Q34_1 == 'Slightly \r\nwilling') %>% # 122
  # filter(Q10 == 0, na.rm = FALSE) %>% # 54
  # filter(Q10 > 0) %>% # 68
  # group_by(Q34_1) %>% 
  count(is.na(Q10) | Q10 == 0) %>% 
  mutate(freq = (n/sum(n))*100)

blank_theme <- theme_minimal()+
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



# 6
matrix6 <- ILTERAnswers %>% 
  filter(as.numeric(Progress) >= 50) %>% 
  filter(Q10 > 0) %>% 
  select(Q29_1:Q29_9) %>%
  tidyr::gather(questions, typeOfInvol) %>% dplyr::group_by(questions, typeOfInvol) %>% dplyr::count() %>% dplyr::ungroup() %>% tidyr::spread(questions, n) %>%
  t() %>% 
  data.frame(row.names(.), ., row.names = NULL) %>% 
  `colnames<-`(c('', 'Agree', 'Disagree', 'Neither agree nor disagree', 'Strongly agree', 'Strongly disagree', 'NA')) %>% 
  .[-1,-1] %>% .[,-6] %>% 
  as.matrix() %>% 
  `rownames<-`(c(
    'My participation in CS gave me insight into the concerns that people have about science.',
    'My participation in CS gave me a better understanding of how people think about the kinds of work that scientists do.',
    'My involvement in citizen science has given me an opportunity to learn from the public in ways that are relevant to the work that I do.',
    'My involvement in citizen science has helped me improve how I communicate about my work with stakeholders.',
    'My involvement in citizen science has helped me improve how I communicate about my work with scientists outside my field.',
    'My involvement in citizen science has helped me improve how I teach and mentor students and staff.',
    'My involvement in citizen science has influenced how I ask research questions.',
    'My involvement in citizen science has influenced how I design studies, collect data, or analyze data.',
    'My involvement in citizen science has helped me place my research in a broader context.'
  )) %>%
  reshape2::melt() 

matrix6 <- matrix6[matrix6$value!=0,]
matrix6 <- matrix6[!is.na(matrix6$value),]

ggplot(matrix6, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = as.numeric(value))) + 
  scale_fill_gradient(low = "grey90", high = "red", na.value = "grey10", guide = "colourbar") +
  labs(x = "Level of agreement", y = "CS impacts", title = "Matrix") +
  labs(fill = "n of answers") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
  theme_bw() + theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
                     axis.text.y = element_text(size = 9),
                     plot.title = element_text(size = 11))

ggplot(matrix6, aes(x = Var2, y = Var1)) + 
  # geom_raster() +
  # scale_fill_gradient(low = "grey90", high = "red") +
  labs(x = "Level of agreement", y = "CS impacts", title = "Matrix") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
  geom_point(aes(size = as.numeric(value))) + #, colour = value)) +
  # scale_color_brewer(palette = "RdYlBu") +
  scale_size_continuous(
    breaks = c(5, 10, 15, 20, 25),
    labels = c('<=5', '10', '15', '20', '>25')
  ) +
  labs(size = "n of answers") +
  theme_bw() + theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
                     axis.text.y = element_text(size = 9),
                     plot.title = element_text(size = 11))

ggplot(matrix6, aes(x = Var2, y = Var1, size = as.numeric(value), label = value)) +
  labs(x = "Level of agreement", y = "CS impacts", title = "Matrix") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
  geom_text(size = as.numeric(matrix6$value), aes(colour = as.numeric(matrix6$value))) +
  scale_colour_distiller(palette = "RdYlBu") +
  labs(colour = "n of answers") +
  theme_bw()


# 7l
numOfCSInitiativePerCntry <- ILTERAnswers %>% 
  filter(as.numeric(Progress) >= 50) %>%
  select(c(Q10, CNTRY_NAME, LTERNetwork)) %>% 
  filter(Q10 > 0) %>%
  group_by(CNTRY_NAME) %>% 
  summarise(totalCSInitiative = sum(Q10)) #%>% View() 
# ILTERAnswers %>% select(c(Q10, CNTRY_NAME, LTERNetwork)) %>% View() 
# ILTERAnswers %>% filter(CNTRY_NAME == 'Italy') %>% select(c(Q10, CNTRY_NAME, LTERNetwork)) %>% View() 
worldCountriesCSInitiative <- 
  sp::merge(worldCountries, as.data.frame(numOfCSInitiativePerCntry), by = "CNTRY_NAME", all.y = FALSE)
worldCountriesCSInitiative@data[233, 'totalCSInitiative'] <- 27
worldCountriesCSInitiative@data[130, 'totalCSInitiative'] <- 5
# number of CS initiatives declared among respondents with country information
# July 2020
# = 315
# October 2020
# = 372
worldCountriesCSInitiative@data %>% 
  filter(totalCSInitiative > 0) %>% summarise(sum(totalCSInitiative)) %>% View()

# pal <- leaflet::colorNumeric(
#   palette = "Blues",
#   domain = worldCountriesCSInitiative$totalCSInitiative)
# binpal <- leaflet::colorNumeric(
#   palette = "Blues",
#   domain = worldCountriesCSInitiative$totalCSInitiative,
#   na.color = 'white'
# )
factpal <- leaflet::colorBin(
  palette = "RdYlBu",
  domain = worldCountriesCSInitiative$totalCSInitiative,
  na.color = 'white',
  bins = c(0, 1, 2, 3, 5, 9, 15, 20, 35, 45)
)
# leaflet::previewColors(factpal, unique(worldCountriesCSInitiative$totalCSInitiative))
leaflet::leaflet(worldCountriesCSInitiative) %>%
  leaflet::addTiles() %>%
  # addMouseCoordinates() %>%
  leaflet::setView(lng = 0, lat = 0, zoom = 3) %>% 
  leaflet::addPolygons(
    stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8,
    color = ~factpal(totalCSInitiative),
    label = ~paste0(totalCSInitiative)
  ) %>% 
  leaflet::addLegend(pal = factpal, values = ~totalCSInitiative, group = "circles", position = "bottomleft")


# 7a
# spatial scale ############ POSTER ############ 
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(c(Q10, Q13)) %>% # 73
  count(Q13) %>% 
  filter(n > 1) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q13, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = "green4") +
  ggplot2::xlab("") + ggplot2::ylab("Number of projects") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1.6, color = "white", size = 3.5) +
  ggplot2::theme_classic()
ggsave("./images2ndPart/7a_spatialScale.png", dpi = 400)
# temporal scale 
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(c(Q10, Q12)) %>% # 73
  count(Q12) %>% 
  filter(!(Q12 %in% NA)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q12, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::xlab("Is the project still active?") + ggplot2::ylab("Number of projects") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1.6, color = "white", size = 3.5) +
  ggplot2::theme_minimal()
# temporal scale initiative ############ POSTER #############
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(c(Q12, Q56)) %>% # 73
  group_by(Q12, Q56) %>%
  summarize(freq = n()) %>% 
  filter(!is.na(Q12)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q12, y = Q56)) +
  ggplot2::geom_point(aes(size = freq), colour = "green4") +
  ggplot2::xlab("Is the project still active?") + 
  ggplot2::ylab("Number of projects") +
  labs(size = "n of years") +
  scale_size_continuous(
    breaks = c(2, 4, 6, 8),
    labels = c('<=2', '4', '6', '=>8')
  ) +
  ggplot2::theme_bw()
ggsave("./images2ndPart/7a_temporalScale.png", dpi = 400)


# 7b ############ POSTER #############
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(c(Q10, Q14)) %>% # 73
  count(Q14) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q14, y = n)) +
  ggplot2::geom_bar(stat = "identity", fill = "green4") +
  ggplot2::xlab("Research focus") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic()
ggsave("./images2ndPart/7b.png", dpi = 400)


# 7c ############ POSTER #############
# number of participants/year
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>%  
  filter(Q10 > 0) %>% 
  select(Q10, Q17, Q18:Q18_8_TEXT, Q19:Q19_4_TEXT) %>% # 73
  group_by(Q17) %>%
  summarize(numProj = n()) %>%
  mutate(Q17 = forcats::fct_relevel(Q17, "Fewer than 25", "25-50", "51-100", "101-500", "More than 500", "NA")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q17, y = numProj)) +
  ggplot2::xlab("n participants/year") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_bar(stat = "identity", fill = "blue4") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  ggplot2::theme_classic()
ggsave("./images2ndPart/7c_numberOfParticipantPerYear.png", dpi = 400)
# type of participans
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>%  
  filter(Q10 > 0) %>% 
  select(Q10, Q17, Q18:Q18_8_TEXT, Q19:Q19_4_TEXT) %>% # 73
  group_by(Q18) %>%
  summarize(numProj = n()) %>% 
  filter(numProj > 3) %>%
  ggplot2::ggplot(ggplot2::aes(x = Q18, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "blue4") +
  ggplot2::xlab("Type of participans") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic()
ggsave("./images2ndPart/7c_typeOfParticipant.png", dpi = 400)
# undeserved communities
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(Q10, Q17, Q18:Q18_8_TEXT, Q19:Q19_4_TEXT) %>% # 73
  group_by(Q19) %>%
  summarize(numProj = n()) %>% 
  mutate(Q19 = forcats::fct_relevel(Q19, 
                                    "Volunteers who have limited financial resources", "Volunteers who live in rural areas", "No undeserved communities", "Other")) %>% 
  filter(numProj > 6) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q19, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "blue4") +
  ggplot2::xlab("Undeserved communities") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic()
ggsave("./images2ndPart/7c_undeservedCommunities.png", dpi = 400)


# 7d ############ POSTER #############
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(Q10, Q20) %>% # 73
  group_by(Q20) %>%
  summarize(numProj = n()) %>% 
  filter(!is.na(Q20)) %>% 
  mutate(Q20 = forcats::fct_relevel(Q20, 
                                    "Once", "Two to three times", "Four to six times", "More than six times")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q20, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "blue4") +
  ggplot2::xlab("Participation frequency") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  ggplot2::theme_classic()
ggsave("./images2ndPart/7d.png", dpi = 400)


# 7e ############ POSTER #############
matrix7e <- # ILTERAnswers %>% 
  # filter(as.numeric(Progress) >= 50) %>% 
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

matrix7e$Var2 <- factor(matrix7e$Var2, levels = c("Not at all involved", "Very little involvement", "Moderate involvement", "High involvement", "Very high involvement"))
matrix7e <- matrix7e[matrix7e$value!=0,]
matrix7e <- matrix7e[!is.na(matrix7e$value),]

ggplot(matrix7e, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = as.numeric(value))) + 
  scale_fill_gradient(low = "grey90", high = "red4", na.value = "grey10", guide = "colourbar") +
  labs(x = "Degree of Involvement", y = "Type of Involvement") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  labs(fill = "n of answers") +
  theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 0.3),
                     axis.text.y = element_text(size = 8),
                     plot.title = element_text(size = 11))
ggsave("./images2ndPart/7e.png", dpi = 400)

ggplot(matrix7e, aes(x = Var2, y = Var1)) + 
  # geom_raster() +
  # scale_fill_gradient(low = "grey90", high = "red") +
  geom_point(aes(size = as.numeric(value))) + #, colour = value)) +
  # scale_color_brewer(palette = "RdYlBu") +
  labs(x = "Degree of Involvement", y = "Type of Involvement", title = "Matrix") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  scale_size_continuous(
    breaks = c(5, 10, 15, 20, 25),
    labels = c('<=5', '10', '15', '20', '>25')
  ) +
  theme_classic() + theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
                     axis.text.y = element_text(size = 9),
                     plot.title = element_text(size = 11))

ggplot(matrix7e, aes(x = Var2, y = Var1, size = as.numeric(value), label = value)) +
  labs(x = "Degree of Involvement", y = "Type of Involvement", title = "Matrix") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  geom_text(size = as.numeric(matrix7e$value), aes(colour = as.numeric(matrix7e$value))) +
  labs(colour = "n of answers") +
  scale_colour_distiller(palette = "RdYlBu") +
  theme_classic()
  

# 7h ############ POSTER #############
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(Q10, Q22) %>% 
  group_by(Q22) %>%
  summarize(numProj = n()) %>% 
  filter(!is.na(Q22)) %>% 
  filter(numProj > 4) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q22, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "yellow4") +
  ggplot2::xlab("") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic() + theme(axis.text.x = element_text(size = 4, angle = 0, vjust = 0.2),
                                 axis.text.y = element_text(size = 8))
ggsave("./images2ndPart/7h.png", dpi = 400)


# 7i ############ POSTER #############
# type of data
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>%  
  filter(Q10 > 0) %>% 
  select(Q10, Q23, Q23_6_TEXT) %>% 
  group_by(Q23) %>%
  summarize(numProj = n()) %>% 
  filter(!is.na(Q23)) %>% 
  filter(numProj > 3) %>%
  ggplot2::ggplot(ggplot2::aes(x = Q23, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "blueviolet") +
  ggplot2::xlab("") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 1)) +
  ggplot2::theme_classic() + theme(axis.text.x = element_text(size = 5, angle = 0, vjust = 0.3),
                                   axis.text.y = element_text(size = 8))
  # ggplot2::theme(axis.text = element_text(size = 3))
ggsave("./images2ndPart/7i_dataType.png", dpi = 400)
# quality check
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>%  
  filter(Q10 > 0) %>% 
  select(Q10, Q25:Q25_9_TEXT) %>% 
  group_by(Q25) %>%
  summarize(numProj = n()) %>% 
  filter(!is.na(Q25)) %>% 
  filter(numProj > 3) %>%
  ggplot2::ggplot(ggplot2::aes(x = Q25, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "blueviolet") +
  ggplot2::xlab("") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic() 
ggsave("./images2ndPart/7i_qualityCheck.png", dpi = 400)


# 7j ############ POSTER #############
# shared data
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>%  
  filter(Q10 > 0) %>% 
  select(Q10, Q24, Q26:Q26_6_TEXT) %>%
  group_by(Q24) %>%
  summarize(numProj = n()) %>% 
  filter(!is.na(Q24)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Q24, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "chocolate4") +
  ggplot2::xlab("") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic() 
ggsave("./images2ndPart/7j_sharedData.png", dpi = 400)
# shared findings
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(Q10, Q24, Q26:Q26_6_TEXT) %>% 
  group_by(Q26) %>%
  summarize(numProj = n()) %>% 
  filter(!is.na(Q26)) %>% 
  filter(numProj > 3) %>%
  ggplot2::ggplot(ggplot2::aes(x = Q26, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "chocolate4") +
  ggplot2::xlab("") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic() + theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 0.3),
                                   axis.text.y = element_text(size = 8))
ggsave("./images2ndPart/7j_sharedFindings.png", dpi = 400)


# 7k ############ POSTER #############
# ILTERAnswers %>% 
#   filter(as.numeric(Progress) >= 50) %>% 
ILTERAnswers %>% 
  filter(as.numeric(ProgressCS) >= 75) %>% 
  filter(Q10 > 0) %>% 
  select(Q10, Q27) %>%
  group_by(Q27) %>%
  summarize(numProj = n()) %>% 
  filter(!is.na(Q27)) %>%
  filter(numProj > 3) %>%
  ggplot2::ggplot(ggplot2::aes(x = Q27, y = numProj)) +
  ggplot2::geom_bar(stat = "identity", fill = "chocolate4") +
  ggplot2::xlab("") + ggplot2::ylab("Number of projects") +
  ggplot2::geom_text(ggplot2::aes(label = numProj), vjust = 1.6, color = "white", size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  ggplot2::theme_classic()
ggsave("./images2ndPart/7k.png", dpi = 400)
