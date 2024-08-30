#'=================================================================================
#'Business Data Analytics - Project work 
#'Diella Pierfrancesco 
#'=================================================================================
#'Topic : Stip-compass database
#'=================================================================================
#'Note : UTF-8 Encoding (recommended by "stip.eocd.org")

#Please, set your working directory
setwd("")

#0. Rimozione oggetti e caricamento librerie -------------

rm(list = ls())

library(readr)
library(readxl)
library(tidyverse)
library(tidytext)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gghighlight)
library(patchwork)
library(cowplot)
library(RColorBrewer)
library(patchwork)

library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(tm)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(readtext)
library(topicmodels)
library(stm)
library(textplot_wordcloud)
library(Matrix)

library(igraph)
library(viridis)
library(tnet)

#1. Qualitative analysis of debates ------
  #1.1. Importing data ------

#' Importing the answers to the question: "Briefly, what are the main ongoing issues of debate around how STI policy is governed?"
debate_governance <- read_xlsx("Governance.xlsx")

#' Importing the answers to the question: "Briefly, what are the main ongoing policy debates around government support for the public research system? "
debate_public_research_system <- read_xlsx("Public research system.xlsx")

#' Importing the answers to the question: "Briefly, what are the main ongoing policy debates around government support to business innovation and innovative entrepreneurship?"
debate_innovation_in_firms <- read_xlsx("Innovation in firms and innovative entrepreneurship.xlsx")

#' Importing the answers to the question: "Briefly, what are the main ongoing policy debates around policy for science-industry knowledge transfer and sharing?"
debate_knowledge_exchange <- read_xlsx("Knowledge exchange and co-creation.xlsx")

#' Importing the answers to the question: "Briefly, what are the main ongoing policy debates around government support for human resources for research and innovation?"
debate_human_resources <- read_xlsx("Human resources for research and innovation.xlsx")

#' Importing the answers to the question: "Briefly, what are the current main policy debates around how policy for research and innovation can help address societal challenges? If applicable, please elaborate on how the Sustainable Development Goals (SDGs) are being incorporated into STI policy objectives, design and implementation"
debate_research_and_innovation_society <- read_xlsx("Research and innovation for society.xlsx")

#' Importing the answers to the question: "Briefly, what are the current main policy debates around how net zero emission targets are being incorporated into STI policy objectives, design and implementation?"
debate_net_zero_transition <- read_xlsx("Net zero transitions.xlsx")

  #1.2. Preprocessing ------

#'merging into a single dataframe
all_debate <- merge(debate_governance, debate_public_research_system, by = "Country", all = TRUE) %>%
  merge(debate_innovation_in_firms, by = "Country", all = TRUE)%>%
  merge(debate_knowledge_exchange, by = "Country", all = TRUE) %>%
  merge(debate_human_resources, by = "Country", all = TRUE) %>%
  merge(debate_research_and_innovation_society, by = "Country", all = TRUE) %>%
  merge(debate_net_zero_transition, by = "Country", all = TRUE) %>%
  mutate(Country = case_when(
    Country %in% c("Belgium - Flanders", "Belgium - Federal government", "Belgium - Brussels Capital", "Belgium - Wallonia-Brussels Federation", "Belgium - Wallonia") ~ "Belgium", TRUE ~ Country )
  )  #' merging lines related to Belgium (Belgium responses were recorded realtively to 5 different administrative regions)

#' new columns name
new_column_names <- c("Country", "Governance", "Public_research_system", "Innovation_in_firms", "Knowledge_exchange", "Human_resources", "Research_and_innovation", "Net_zero_transitions")

all_debate <- all_debate %>%
  setNames(new_column_names) %>%
  group_by(Country) %>%
  summarise_all(~ paste(unique(.), collapse = ", "))

#'transformation to long format
#'creation of a column to be used as an ID
all_debate_long <-  all_debate %>%
  pivot_longer(cols = -Country, names_to = "Policy_Area", values_to = "Description") %>%
  unite(ID, Country, Policy_Area, sep = "--", remove = FALSE)
  
  #1.3. Wordcloud of debates ------------

#' General wordcloud

debate_corp <- Corpus(VectorSource(all_debate_long$Description)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en"))

dtm_debate <- DocumentTermMatrix(debate_corp)

df_debate <- as.data.frame(as.matrix(dtm_debate))

word_freq_debate <- colSums(df_debate)

debate_wordcloud <- data.frame(word = names(word_freq_debate), freq = word_freq_debate)%>%
  filter(freq > 10)

#wordcloud(names(word_freq_debate), word_freq_debate, scale=c(5,0.3), min.freq=10, random.order=FALSE, colors=brewer.pal(8, "Dark2"), title = "Entire debates")

wordcloud2(debate_wordcloud,size=0.7)


#'wordcloud for Governance area

Governance_debate <- all_debate_long %>%
  filter(Policy_Area=='Governance')

Governance_debate_corp <- Corpus(VectorSource(Governance_debate$Description)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en"))

dtm_Governance_debate <- DocumentTermMatrix(Governance_debate_corp)

df_Governance_debate <- as.data.frame(as.matrix(dtm_Governance_debate))

word_freq_Governance_debate <- colSums(df_Governance_debate)

Governance_debate_wordcloud <- data.frame(word = names(word_freq_Governance_debate), freq = word_freq_Governance_debate) %>%
  filter(freq > 10)

wordcloud2(Governance_debate_wordcloud, size = 0.7)



#' wordlcoud for Public research system area

PubResearch_debate <- all_debate_long %>%
  filter(Policy_Area == 'Public_research_system')

PubResearch_debate_corp <- Corpus(VectorSource(PubResearch_debate$Description)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en"))

dtm_PubResearch_debate <- DocumentTermMatrix(PubResearch_debate_corp)

df_PubResearch_debate <- as.data.frame(as.matrix(dtm_PubResearch_debate))

word_freq_PubResearch_debate <- colSums(df_PubResearch_debate)

PubResearch_debate_wordcloud <- data.frame(word = names(word_freq_PubResearch_debate), freq = word_freq_PubResearch_debate) %>%
  filter(freq > 10)

wordcloud2(PubResearch_debate_wordcloud, size = 0.7)


#' wordcloud for Net zero transition area


NetZero_debate <- all_debate_long%>%
  filter(Policy_Area=='Net_zero_transitions')

NetZero_debate_corp <- Corpus(VectorSource(NetZero_debate$Description)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en"))

dtm_NetZero_debate <- DocumentTermMatrix(NetZero_debate_corp)

df_NetZero_debate <- as.data.frame(as.matrix(dtm_NetZero_debate))

word_freq_NetZero_debate <- colSums(df_NetZero_debate)


NetZero_debate_wordcloud <- data.frame(word= names(word_freq_NetZero_debate), freq = word_freq_NetZero_debate) %>%
  filter(freq >10)


wordcloud2(NetZero_debate_wordcloud, size = 0.7)


  #1.4. tf - idf -------

my_text_uni <- all_debate_long %>%
  select(ID, Description) %>%
  unnest_tokens(output = word, input = Description) %>%
  anti_join(stop_words) %>%
  mutate(word_numeric = as.numeric(word))%>%
  filter(is.na(word_numeric))%>%
  select(-word_numeric)
  
debate_words <- my_text_uni %>%
  count(ID, word, sort = TRUE) %>%
  ungroup()

total_words <- debate_words %>%
  group_by(ID) %>%
  summarize(total = sum(n)) 

debate_words <- left_join(debate_words, total_words) %>%
  filter(total > 1) %>%
  mutate(word_freq = n/total)

g_debate_words <- debate_words %>%
  filter(ID == 'Italy--Governance' | ID == 'Italy--Human_resources' | ID == 'Italy--Innovation_in_firms' | ID == 'Italy--Knowledge_exchange' | ID == 'Italy--Public_research_system' | ID == 'Italy--Research_and_innovation' | ID =='Italy--Net_zero_transitions') %>%
  ggplot(aes(word_freq, fill = as.character(ID))) +
  geom_histogram() +
  #xlim(NA, 0.05) +
  theme(legend.position = "none") +
  facet_wrap(~ID, ncol = 4) +
  xlab("N/total")

plot(g_debate_words)

debate_words_tfidf <- all_debate_long %>%
  select(ID, Description) %>%
  unnest_tokens(output = word, input = Description) %>%
  anti_join(stop_words) %>%
  mutate(word_numeric = as.numeric(word))%>%
  filter(is.na(word_numeric))%>%
  select(-word_numeric) %>%
  filter(!word %in% c("italy", "italian", "italy's")) %>%
  count(ID, word, sort = TRUE)%>%
  ungroup() %>%
  bind_tf_idf(word, ID, n)

g_tf_idf <- debate_words_tfidf %>%
  filter(ID == 'Italy--Governance' | ID == 'Italy--Human_resources' | ID == 'Italy--Innovation_in_firms' | ID == 'Italy--Knowledge_exchange' | ID == 'Italy--Public_research_system' | ID == 'Italy--Research_and_innovation' | ID =='Italy--Net_zero_transitions') %>%
  group_by(ID) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(ID = as.factor(ID), word = reorder_within(word, tf_idf, ID)) %>%
  ggplot(aes(x = tf_idf, y = word, fill = ID)) + 
  geom_col() +
  facet_wrap(~ID, nrow = 3, scales = "free_y") +
  xlab("tf-idf") +
  ylab("Words") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_reordered()


plot(g_tf_idf)

#2. Quantitative Analysis of Initiatives -------
  #2.1. Importing data ------

#permalink from "stip.oecd.org": refers to the STIP survey of 2023
url <- 'https://stip.oecd.org/assets/downloads/STIP_Survey.csv'

#downloading the dataset and saving to the working directory
download.file(url, destfile = 'stip2023.csv', mode = 'wb')

#loading the dataset into the enviroment
stip_data_raw <- read_delim('stip2023.csv', '|', escape_double = FALSE, trim_ws = TRUE)

  #2.2. Pre-processing ------
#' The dataset contains the policy initiatives on the rows and the features on the columns, 
#' in the cells the values that the features take on each initiative
#' multiple rows can contain the same initiative if a column (example: themes, tools, other...) takes 2 different values for the same initiative

#' in the first row of the dataset the cells contain column explanations.
#' Extracting this row and creating from it a dataframe called "codebook," which contains the variables and their respective explanation
codebook <- as.data.frame(t(stip_data_raw[1,])) %>%
  rownames_to_column()

names(codebook) <- c('Variable', 'Code')

#Removal of the first row, whose information was saved in the codebook, from the dataframe
stip_data_raw <- stip_data_raw[-1, ]

#' An example of a single initiative
example_data <- stip_data_raw %>%
  filter(InitiativeID == "http://stip.oecd.org/2023/data/policyInitiatives/1400") #a questa iniziativa corrispondono due "InstrumentID" per questo motivo è stata descritta in 2 righe 

#' Information on instruments does not seem so useful to the analysis at hand
#' The information contained in the "Instrument Type Label" column, in addition to those required as IDs, is sufficient

#' Elimination of the columns "Instrument type code," "F[...]," (which go too much into the specifics of instrument description)
#' Elimination of "SurveyYear," LastModifiedInSurvey," 'Acronym,' "YearlyBudgetLocalCurrency"
#' elimination of variables containing Links
#' Elimination of the "http://stip.oecd.org/2023/data/policyInitiatives/" pattern from InitiativeID 
#' Elimination of the pattern "http://stip.oecd.org/2023/data/policyInitiatives/[]/instrument/" from InstrumentID
#' replacement of multiple CountryLabel values related to Belgium with "Belgium"
#' replacement of multiple CountryCode values related to Belgium with "BEL"
stip_data_worked <- stip_data_raw[,!grepl('[F][0-9]', (names(stip_data_raw)))] %>%
  select(-InstrumentTypeCode, -SurveyYear, -LastModifiedInSurvey, -Acronym, -YearlyBudgetLocalCurrency) %>%
  select(-contains("Link")) %>%
  mutate(InitiativeID = as.numeric((gsub('http://stip.oecd.org/2023/data/policyInitiatives/', '', InitiativeID)))) %>%
  mutate(InstrumentID = as.numeric((gsub('http://stip.oecd.org/2023/data/policyInitiatives/[0-9]+/instrument/', '', InstrumentID)))) %>%
  mutate(CountryLabel = case_when(
    CountryLabel %in% c("Belgium - Flanders", "Belgium - Federal government", "Belgium - Brussels Capital", "Belgium - Wallonia-Brussels Federation", "Belgium - Wallonia") ~ "Belgium", TRUE ~ CountryLabel )
    ) %>%
  mutate(CountryCode = ifelse(CountryLabel == "Belgium", "BEL", CountryCode))

#'Long format with a column indicating the political theme
#'The explanation of the theme code is given in the codebook
stip_themes <- stip_data_worked %>%
  select(InitiativeID, contains("TH")) %>%
  pivot_longer(cols = -InitiativeID, names_to = "Variable") %>%
  filter(value == 1) %>%
  left_join(codebook, by= "Variable") %>%
  select(- Variable, -value) %>%
  rename(Theme = Code) 
  
#'df where each initiative is associated with only one line
#'useful only for counting initiatives!!! 
stip_data_grouped <- stip_data_worked %>%
  distinct(InitiativeID, .keep_all = T)

#'grouped themes 
stip_themes_grouped <- stip_data_worked %>%
  select(InitiativeID, contains("TH")) %>%
  pivot_longer(cols = -InitiativeID, names_to = "Variable") %>%
  filter(value == 1) %>%
  left_join(codebook, by= "Variable") %>%
  select(- Variable, -value) %>%
  rename(Theme = Code) %>%
  group_by(InitiativeID) %>%
  summarise(PolicyTheme = paste(Theme, collapse = ", "))

  #2.3. general statistics-------

summary(stip_data_worked)

#' Table containing the number of initiatives, 
#' number of instruments used, 
#' numbers of themes, 
#' number of countries involved, 
#' number of organizations responsible for the initiatives, 
#' number of initiatives found to be structural reforms
stat <- stip_data_worked %>%
  summarise(
    n_initiative = n_distinct(InitiativeID),
    n_instrument = n_distinct(InstrumentID),
    n_themes = n_distinct(stip_themes$Theme),
    n_country = n_distinct(CountryLabel),
    n_responsible_organisation = n_distinct(NameResponsibleOrganisation),
    n_structural_reform = sum(IsStructuralReform == 1, na.rm = T)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

#'count of the most recurring themes in the initiatives
count_theme <- stip_themes %>%
  count(Theme, sort=T)

#'count of initiatives by countries 
count_country <- stip_data_grouped %>%
  count(CountryLabel, sort = T) 

#'count of the most indicated instruments in the initiatives
count_instrument <- stip_data_worked %>%
  count(InstrumentTypeLabel, sort = T)

#'count of initiatives by responsible organization
count_responsible_organisation <- stip_data_worked %>%
  group_by(NameResponsibleOrganisation, CountryLabel) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate (NameResponsibleOrganisation = gsub("\\([^\\)]+\\)", "", NameResponsibleOrganisation))%>%
  ungroup()#%>%
#unite(ResponsibleOrganisation, NameResponsibleOrganisation, CountryLabel, sep = "-- ")

  #2.4.Iniziatives X Countries ------

#'chart of No. of initiatives by country
g_count_country <- count_country %>%
  ungroup() %>%
  mutate(CountryLabel = reorder(CountryLabel, desc(n))) %>%
  ggplot(aes(x = CountryLabel, y = n, fill = cut(n, breaks = c(-Inf, 50, 100, 200, 300, Inf),
                                                 labels = c("0-50", "50-100", "100-200", "200-300", ">300"),
                                                 include.lowest = TRUE))) +
  geom_col(color = "#cccccc", alpha = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size=16, hjust=0.5)) +
  scale_fill_manual(values = c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c"),
                    breaks = c("0-50", "50-100", "100-200", "200-300", ">300"),
                    labels = c("0-50", "50-100", "100-200", "200-300", ">300"),
                    name="") +
  ggtitle("Policy initiatives by Countries") +
  xlab(NULL) +
  ylab("N° initiatives")
  
print(g_count_country)

#' please run this portion of the script useful for continuation
stip_data_raw2019 <- read_delim('STIP_Survey-2019.csv', '|', escape_double = FALSE, trim_ws = TRUE)
codebook2019 <- as.data.frame(t(stip_data_raw2019[1,])) %>%
  rownames_to_column()
names(codebook2019) <- c('Variable', 'Code')
stip_data_raw2019 <- stip_data_raw2019[-1, ]
stip_data_worked2019 <- stip_data_raw2019[,!grepl('[F][0-9]', (names(stip_data_raw2019)))] %>%
  select(-InstrumentTypeCode, -SurveyYear, -Acronym, -YearlyBudgetLocalCurrency) %>%
  select(-contains("Link")) %>%
  mutate(InitiativeID = as.numeric((gsub('http://stip.oecd.org/2019/data/policyInitiatives/', '', InitiativeID)))) %>%
  mutate(InstrumentID = as.numeric((gsub('http://stip.oecd.org/2019/data/policyInitiatives/[0-9]+/instrument/', '', InstrumentID)))) %>%
  mutate(CountryLabel = case_when(
    CountryLabel %in% c("Belgium - Flanders", "Belgium - Federal government", "Belgium - Brussels Capital", "Belgium - Wallonia-Brussels Federation", "Belgium - Wallonia") ~ "Belgium", TRUE ~ CountryLabel )
  ) %>%
  mutate(CountryCode = ifelse(CountryLabel == "Belgium", "BEL", CountryCode))
stip_themes2019 <- stip_data_worked2019 %>%
  select(InitiativeID, contains("TH")) %>%
  pivot_longer(cols = -InitiativeID, names_to = "Variable") %>%
  filter(value == 1) %>%
  left_join(codebook2019, by= "Variable") %>%
  select(- Variable, -value) %>%
  rename(Theme = Code) #%>%
#group_by(InitiativeID) %>%
#summarise(PolicyTheme = paste(Theme, collapse = ", "))    # per raggruppare le iniziative
stip_data_grouped2019 <- stip_data_worked2019 %>%
  distinct(InitiativeID, .keep_all = T)
stat2019 <- stip_data_worked2019 %>%
  summarise(
    n_initiative = n_distinct(InitiativeID),
    n_instrument = n_distinct(InstrumentID),
    n_themes = n_distinct(stip_themes2019$Theme),
    n_country = n_distinct(CountryLabel),
    n_responsible_organisation = n_distinct(NameResponsibleOrganisation),
    n_structural_reform = sum(IsStructuralReform == 1, na.rm = T)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")
count_theme2019 <- stip_themes2019 %>%
  count(Theme, sort=T)
count_country2019 <- stip_data_grouped2019 %>%
  count(CountryLabel, sort = T) 
g_count_country2019 <- count_country2019 %>%
  ungroup() %>%
  mutate(CountryLabel = reorder(CountryLabel, desc(n))) %>%
  ggplot(aes(x = CountryLabel, y = n, fill = cut(n, breaks = c(-Inf, 50, 100, 200, 300, Inf),
                                                 labels = c("0-50", "50-100", "100-200", "200-300", ">300"),
                                                 include.lowest = TRUE))) +
  geom_col(color = "#cccccc", alpha = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.title = element_text(size=16, hjust=0.5)) +
  scale_fill_manual(values = c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c"),
                    breaks = c("0-50", "50-100", "100-200", "200-300", ">300"),
                    labels = c("0-50", "50-100", "100-200", "200-300", ">300"),
                    name="") +
  ggtitle("Policy initiatives by Countries 2019") +
  xlab("Countries") +
  ylab("NÂ° initiatives")
print(g_count_country2019)
count_instrument2019 <- stip_data_worked2019 %>%
  count(InstrumentTypeLabel, sort = T)
count_responsible_organisation2019 <- stip_data_worked2019 %>%
  group_by(NameResponsibleOrganisation, CountryLabel) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate (NameResponsibleOrganisation = gsub("\\([^\\)]+\\)", "", NameResponsibleOrganisation)) #%>%
#unite(ResponsibleOrganisation, NameResponsibleOrganisation, CountryLabel, sep = "-- ")


#' Difference between 2019 (script executed in parallel) and 2023
difference_data <- count_country %>%
  left_join(count_country2019, by = "CountryLabel", suffix = c("_2023", "_2019")) %>%
  mutate(Difference = n_2023 - n_2019) %>%
  filter(!is.na(Difference))  # Rimuovi le righe con valori mancanti

#' chart
g_difference_country <- ggplot(difference_data, aes(x = reorder(CountryLabel, -Difference), y = Difference)) +
  geom_col(fill = "skyblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  ggtitle("Initiatives [2023 vs. 2019]") +
  xlab(NULL) +
  ylab(NULL)

print(g_difference_country)

combined_g_country <- plot_grid(g_count_country, g_difference_country, nrow = 2, align = "v")
print(combined_g_country)


  #2.5. Iniziatives by ResponsableOrganitation -------

#' countries of the top n responsible organizations (n=10)
top_n_country_resposible_organisation <- count_responsible_organisation %>%
  slice_head(n=10)%>%
  pull(CountryLabel)%>%
  unique()
#' Responsible organization with at least n initiatives (n=5)
#' creation of the "percentage" column indicating the contribution of the specific organization on the total initiatives in that country
g_responsible_organisation <- count_responsible_organisation %>%
  filter(CountryLabel %in% top_n_country_resposible_organisation) %>%
  filter(count >= 5) %>%
  group_by(CountryLabel) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

# grouped bar chart
ggplot(g_responsible_organisation, aes(x = CountryLabel, y = count, fill = NameResponsibleOrganisation, color =CountryLabel )) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size =0.5) +
  theme_minimal() +
  labs(title = "Responsible Organisations distribution",
       x = NULL,
       y = "N° Initiatives",
       fill = "Organizzazione responsabile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=20)) +
  theme(legend.position="none") 

# bar chart, highlighting organizations that contributed more than 40 percent to their country's initiatives
ggplot(g_responsible_organisation, aes(x=CountryLabel, y=count, fill=NameResponsibleOrganisation))+
  geom_col()+
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(size=15, angle = 45),
        legend.text = element_text(size=15))+
  gghighlight(percentage > 40) +
  labs(title = "Responsible Organisatzation with contribution of more than 40%",
       x= NULL,
       y= "N° Initiatives",
       fill= "Responsible Organisation") +
  theme(panel.grid=element_blank())

  #2.6. Analysis of instruments-------

#'chart of the most frequently used Instruments
g_count_instrument <- count_instrument %>%
  na.omit() %>%
  ggplot(aes(x="", y=n, fill=reorder(InstrumentTypeLabel, n))) +
  geom_bar(stat="identity", width=1, color = "black") +
  geom_text(aes(label=ifelse(n>200, as.character(n), "")), position = position_stack(vjust = 0.5), color="white", size =5)+
  coord_polar(theta="y") +
  theme_minimal() +
  labs(fill="Instrument Type",
       x=NULL,
       y=NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size=15))

print(g_count_instrument)

#' types of values that InstrumentTypeLabel can take.
print(unique(stip_data_worked$InstrumentTypeLabel))


  #2.7.  Brazil case -------------

#' Initiatives and Instruments (NA omitted)
df_brazil <- stip_data_worked %>%
  filter(CountryLabel == "Brazil") %>%
  select(InitiativeID, InstrumentTypeLabel) %>%
  na.omit() 

#' types of values that InstrumentTypeLabel can take 
unique(df_brazil$InstrumentTypeLabel)

#' counting the most recurring Instruments
count_instrument_brazil <- df_brazil %>% 
  count(InitiativeID, sort = T)

#' Counting the number of times two instruments appeared in the same initiative 
#' Will be the weights of the edges of network
df_width <- df_brazil %>%
  inner_join(df_brazil, by = "InitiativeID") %>%
  filter(InstrumentTypeLabel.x != InstrumentTypeLabel.y) %>%
  mutate(pair = ifelse(InstrumentTypeLabel.x < InstrumentTypeLabel.y,
                       paste(InstrumentTypeLabel.x, InstrumentTypeLabel.y, sep = "--"),
                       paste(InstrumentTypeLabel.y, InstrumentTypeLabel.x, sep = "--"))) %>%
  group_by(pair) %>%
  summarise(count = n()) %>%
  ungroup()
  
#'graph between initiatives and instruments
g_brazil <- graph_from_data_frame(df_brazil, directed = FALSE)

plot(g_brazil)

#' elimination of "initiatives" nodes
V(g_brazil)$type <- ifelse(grepl("^\\d+$", V(g_brazil)$name), FALSE, TRUE)
g_brazil <- bipartite_projection(g_brazil, multiplicity = FALSE, which = TRUE)
plot(g_brazil)

#'Assignment of the count of times two instruments appeared in the same initiative to the weight of the edges
E(g_brazil)$width <- df_width$count
plot(g_brazil)

#'useful operations for graphical purposes only
node_degrees <- degree(g_brazil)

color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))
node_colors <- color_palette(10)[cut(node_degrees, breaks = 10)]

#' final country-related graph
#' darker nodes are those with higher degree
plot(g_brazil, 
     layout=layout.fruchterman.reingold(g_brazil),
     vertex.label = NA,
     vertex.size = 10,
     vertex.color = node_colors,
     edge.color = "lightblue", 
     bg = "lightgray")

#' Network-Level Measures
#' nodes and edges
n_nodes_g_brazil <- length(V(g_brazil))  #23
n_edges_g_brazil <- length(E(g_brazil))  #50

#'diameter
diameter_g_brazil <- diameter(g_brazil, directed = FALSE, unconnected = TRUE)  #3         

#'apl
apl_g_brazil  <- mean_distance(g_brazil, directed = FALSE, unconnected = TRUE)  #1.705882

#'densità
ed_g_density_brazil <- edge_density(g_brazil)   #0.1976285

#' No. of component
comp_g_brazil <- components(g_brazil)   #$csize 17  1  1  1  1  1  1 , $no 7 

#' bridges
bridges_g_brazil <- bridges(g_brazil)    #0

#'n4 cliques
cliques_g_brazil <- cliques(g_brazil, min = 4)                                   
numcliques_g_brazil <- count_max_cliques(g_brazil, min = 4)     #6                 

#'inclusivness
inclusiveness_g_brazil <- (vcount(g_brazil)-sum(degree(g_brazil)==0))/vcount(g_brazil)   #0.7391304

#'transitivity
transitivity_g_brazil <- transitivity(g_brazil, type = "globalundirected")    #0.5700935


  #2.8. Portugal case -----


df_portugal <- stip_data_worked %>%
  filter(CountryLabel == "Portugal") %>%
  select(InitiativeID, InstrumentTypeLabel) %>%
  na.omit() 

unique(df_portugal$InstrumentTypeLabel)

count_instrument_portugal <- df_portugal %>% 
  count(InitiativeID)

df_width_portugal <- df_portugal %>%
  inner_join(df_portugal, by = "InitiativeID") %>%
  filter(InstrumentTypeLabel.x != InstrumentTypeLabel.y) %>%
  mutate(pair = ifelse(InstrumentTypeLabel.x < InstrumentTypeLabel.y,
                       paste(InstrumentTypeLabel.x, InstrumentTypeLabel.y, sep = "--"),
                       paste(InstrumentTypeLabel.y, InstrumentTypeLabel.x, sep = "--"))) %>%
  group_by(pair) %>%
  summarise(count = n()) %>%
  ungroup()

g_portugal <- graph_from_data_frame(df_portugal, directed = FALSE)

plot(g_portugal)

V(g_portugal)$type <- ifelse(grepl("^\\d+$", V(g_portugal)$name), FALSE, TRUE)
g_portugal <- bipartite_projection(g_portugal, multiplicity = FALSE, which = TRUE)
plot(g_portugal)

E(g_portugal)$width <- df_width_portugal$count
plot(g_portugal)

node_degrees_portugal <- degree(g_portugal)

color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))
node_colors_portugal <- color_palette(10)[cut(node_degrees_portugal, breaks = 10)]

plot(g_portugal, 
     layout=layout.fruchterman.reingold(g_portugal),
     vertex.label = NA,
     vertex.size = 10,
     vertex.color = node_colors_portugal,
     edge.color = "lightblue", 
     bg = "lightgray")

n_nodes_g_portugal <- length(V(g_portugal))  #27
n_edges_g_portugal <- length(E(g_portugal))  #64

diameter_g_portugal <- diameter(g_portugal, directed = FALSE, unconnected = TRUE)  #4        

apl_g_portugal  <- mean_distance(g_portugal, directed = FALSE, unconnected = TRUE)  #2.086957

ed_g_density_portugal <- edge_density(g_portugal)   #0.1823362

comp_g_portugal <- components(g_portugal)   #$csize 23  1  1  1  1 , $no 5

bridges_g_portugal <- bridges(g_portugal)    #3

cliques_g_portugal <- cliques(g_portugal, min = 4)                                   
numcliques_g_portugal <- count_max_cliques(g_portugal, min = 4)     #11                

inclusiveness_g_portugal <- (vcount(g_portugal)-sum(degree(g_portugal)==0))/vcount(g_portugal)   #0.8518519

transitivity_g_portugal <- transitivity(g_portugal, type = "globalundirected")    #0.4358354


  #2.9. United Kingdom case -----------


df_united_kingdom <- stip_data_worked %>%
  filter(CountryLabel == "United Kingdom") %>%
  select(InitiativeID, InstrumentTypeLabel) %>%
  na.omit() 

unique(df_united_kingdom$InstrumentTypeLabel)

count_instrument_united_kingdom <- df_united_kingdom %>% 
  count(InitiativeID)

df_width_united_kingdom <- df_united_kingdom %>%
  inner_join(df_united_kingdom, by = "InitiativeID") %>%
  filter(InstrumentTypeLabel.x != InstrumentTypeLabel.y) %>%
  mutate(pair = ifelse(InstrumentTypeLabel.x < InstrumentTypeLabel.y,
                       paste(InstrumentTypeLabel.x, InstrumentTypeLabel.y, sep = "--"),
                       paste(InstrumentTypeLabel.y, InstrumentTypeLabel.x, sep = "--"))) %>%
  group_by(pair) %>%
  summarise(count = n()) %>%
  ungroup()

g_united_kingdom <- graph_from_data_frame(df_united_kingdom, directed = FALSE)

plot(g_united_kingdom)

V(g_united_kingdom)$type <- ifelse(grepl("^\\d+$", V(g_united_kingdom)$name), FALSE, TRUE)
g_united_kingdom <- bipartite_projection(g_united_kingdom, multiplicity = FALSE, which = TRUE)
plot(g_united_kingdom)

E(g_united_kingdom)$width <- df_width_united_kingdom$count
plot(g_united_kingdom)

node_degrees_united_kingdom <- degree(g_united_kingdom)

color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))
node_colors_united_kingdom <- color_palette(10)[cut(node_degrees_united_kingdom, breaks = 10)]

plot(g_united_kingdom, 
     layout=layout.fruchterman.reingold(g_united_kingdom),
     vertex.label = NA,
     vertex.size = 10,
     vertex.color = node_colors_united_kingdom,
     edge.color = "lightblue", 
     bg = "lightgray")

n_nodes_g_united_kingdom <- length(V(g_united_kingdom))  #24
n_edges_g_united_kingdom <- length(E(g_united_kingdom))  #30

diameter_g_united_kingdom <- diameter(g_united_kingdom, directed = FALSE, unconnected = TRUE)  #5         

apl_g_united_kingdom  <- mean_distance(g_united_kingdom, directed = FALSE, unconnected = TRUE)  #2.175

ed_g_density_united_kingdom <- edge_density(g_united_kingdom)   #0.1086957

comp_g_united_kingdom <- components(g_united_kingdom)   #$csize 16  1  1  1  1  1  1  1   , $no 9

bridges_g_united_kingdom <- bridges(g_united_kingdom)    #3

cliques_g_united_kingdom <- cliques(g_united_kingdom, min = 4)                                   
numcliques_g_united_kingdom <- count_max_cliques(g_united_kingdom, min = 4)     #3                 

inclusiveness_g_united_kingdom <- (vcount(g_united_kingdom)-sum(degree(g_united_kingdom)==0))/vcount(g_united_kingdom)   #0.6666667

transitivity_g_united_kingdom <- transitivity(g_united_kingdom, type = "globalundirected")    #0.4033613


  #2.10. Luxembourg case -------

df_luxembourg <- stip_data_worked %>%
  filter(CountryLabel == "Luxembourg") %>%
  select(InitiativeID, InstrumentTypeLabel) %>%
  na.omit() 

unique(df_luxembourg$InstrumentTypeLabel)

count_instrument_luxembourg <- df_luxembourg %>% 
  count(InitiativeID)

df_width_luxembourg <- df_luxembourg %>%
  inner_join(df_luxembourg, by = "InitiativeID") %>%
  filter(InstrumentTypeLabel.x != InstrumentTypeLabel.y) %>%
  mutate(pair = ifelse(InstrumentTypeLabel.x < InstrumentTypeLabel.y,
                       paste(InstrumentTypeLabel.x, InstrumentTypeLabel.y, sep = "--"),
                       paste(InstrumentTypeLabel.y, InstrumentTypeLabel.x, sep = "--"))) %>%
  group_by(pair) %>%
  summarise(count = n()) %>%
  ungroup()

g_luxembourg <- graph_from_data_frame(df_luxembourg, directed = FALSE)

plot(g_luxembourg)

V(g_luxembourg)$type <- ifelse(grepl("^\\d+$", V(g_luxembourg)$name), FALSE, TRUE)
g_luxembourg <- bipartite_projection(g_luxembourg, multiplicity = FALSE, which = TRUE)
plot(g_luxembourg)

E(g_luxembourg)$width <- df_width_luxembourg$count
plot(g_luxembourg)

node_degrees_luxembourg <- degree(g_luxembourg)

color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))
node_colors_luxembourg <- color_palette(10)[cut(node_degrees_luxembourg, breaks = 10)]

plot(g_luxembourg, 
     layout=layout.fruchterman.reingold(g_luxembourg),
     vertex.label = NA,
     vertex.size = 10,
     vertex.color = node_colors_luxembourg,
     edge.color = "lightblue", 
     bg = "lightgray")

n_nodes_g_luxembourg <- length(V(g_luxembourg))  #23
n_edges_g_luxembourg <- length(E(g_luxembourg))  #29

diameter_g_luxembourg <- diameter(g_luxembourg, directed = FALSE, unconnected = TRUE)  #5         

apl_g_luxembourg  <- mean_distance(g_luxembourg, directed = FALSE, unconnected = TRUE)  #2.291667

ed_g_density_luxembourg <- edge_density(g_luxembourg)   #0.1146245

comp_g_luxembourg <- components(g_luxembourg)   #$csize 16  1  1  1  1  1  1  1 , $no 8

bridges_g_luxembourg <- bridges(g_luxembourg)    #4

cliques_g_luxembourg <- cliques(g_luxembourg, min = 4)                                   
numcliques_g_luxembourg <- count_max_cliques(g_luxembourg, min = 4)     #2                 

inclusiveness_g_luxembourg <- (vcount(g_luxembourg)-sum(degree(g_luxembourg)==0))/vcount(g_luxembourg)   #0.6956522

transitivity_g_luxembourg <- transitivity(g_luxembourg, type = "globalundirected")    #0.5


  #2.11. Romania case -------

df_romania <- stip_data_worked %>%
  filter(CountryLabel == "Romania") %>%
  select(InitiativeID, InstrumentTypeLabel) %>%
  na.omit() 

unique(df_romania$InstrumentTypeLabel)

count_instrument_romania <- df_romania %>% 
  count(InitiativeID)

df_width_romania <- df_romania %>%
  inner_join(df_romania, by = "InitiativeID") %>%
  filter(InstrumentTypeLabel.x != InstrumentTypeLabel.y) %>%
  mutate(pair = ifelse(InstrumentTypeLabel.x < InstrumentTypeLabel.y,
                       paste(InstrumentTypeLabel.x, InstrumentTypeLabel.y, sep = "--"),
                       paste(InstrumentTypeLabel.y, InstrumentTypeLabel.x, sep = "--"))) %>%
  group_by(pair) %>%
  summarise(count = n()) %>%
  ungroup()

g_romania <- graph_from_data_frame(df_romania, directed = FALSE)

plot(g_romania)

V(g_romania)$type <- ifelse(grepl("^\\d+$", V(g_romania)$name), FALSE, TRUE)
g_romania <- bipartite_projection(g_romania, multiplicity = FALSE, which = TRUE)
plot(g_romania)

E(g_romania)$width <- df_width_romania$count
plot(g_romania)

node_degrees_romania <- degree(g_romania)

color_palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))
node_colors_romania <- color_palette(10)[cut(node_degrees_romania, breaks = 10)]

plot(g_romania, 
     layout=layout.fruchterman.reingold(g_romania),
     vertex.label = NA,
     vertex.size = 10,
     vertex.color = node_colors_romania,
     edge.color = "lightblue", 
     bg = "lightgray")


n_nodes_g_romania <- length(V(g_romania))  #15
n_edges_g_romania <- length(E(g_romania))  #15


diameter_g_romania <- diameter(g_romania, directed = FALSE, unconnected = TRUE)  #3         

apl_g_romania  <- mean_distance(g_romania, directed = FALSE, unconnected = TRUE)  #1.888889

ed_g_density_romania <- edge_density(g_romania)   #0.1428571

comp_g_romania <- components(g_romania)   #$csize 1 10  1  1  1  1 , $no 6 

bridges_g_romania <- bridges(g_romania)    #1

cliques_g_romania <- cliques(g_romania, min = 4)                                   
numcliques_g_romania <- count_max_cliques(g_romania, min = 4)     #0                 

inclusiveness_g_romania <- (vcount(g_romania)-sum(degree(g_romania)==0))/vcount(g_romania)   #0.6666667

transitivity_g_romania <- transitivity(g_romania, type = "globalundirected")    #0.3571429


  #2.12 Conclusions instruments analysis -------

# Creation of a combined dataframe for Network Level Measures for the chosen countries
net_measures <- data.frame(
  Paese = c("Portugal", "United Kingdom", "Brazil", "Luxembourg", "Romania"),
  Nodi = c(27, 24, 23, 23, 15),
  Archi = c(64, 30, 50, 29, 15),
  Diametro = c(4, 5, 3, 5, 3),
  APL = c(2.087, 2.175, 1.706, 2.292, 1.889),
  Densità = c(0.182, 0.109, 0.198, 0.115, 0.143),
  Componenti = c(5, 9, 7, 8, 6),
  Dim_Largest_Comp = c(23, 16, 17, 16, 10),
  Bridges = c(3, 3, 0, 4, 1),
  Inclusiveness = c(0.852, 0.667, 0.739, 0.696, 0.667),
  Transitivity = c(0.436, 0.403, 0.570, 0.500, 0.357)
)

net_measures$Paese <- factor(net_measures$Paese, levels = c("Portugal", "United Kingdom", "Brazil", "Luxembourg", "Romania"))


print(net_measures)


country_colors <- c("Portugal" = "lightcoral",
                    "United Kingdom" = "lightgreen",
                    "Brazil" = "gold",
                    "Luxembourg" = "skyblue",
                    "Romania" = "lightseagreen")

plot_nodes <- ggplot(net_measures, aes(x = Paese, y = as.numeric(Nodi), fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Nodes") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(plot_nodes)


plot_edges <- ggplot(net_measures, aes(x = Paese, y = as.numeric(Archi), fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Edges") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(plot_edges)

max_y <- max(max(net_measures$Nodi), max(net_measures$Archi))

combined_plot1 <- plot_grid(
  plot_nodes + ylim(0, max_y),
  plot_edges + ylim(0, max_y),
  ncol = 2, align = "v"
)


print(combined_plot1)



plot_diametro <- ggplot(net_measures, aes(x = Paese, y = Diametro, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Diameter") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot_apl <- ggplot(net_measures, aes(x = Paese, y = APL, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "APL") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot_densita <- ggplot(net_measures, aes(x = Paese, y = Densità, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Density") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

combined_plot2 <- plot_grid(
  plot_diametro + ylim(0, max(net_measures$Diametro)),
  plot_apl + ylim(0, max(net_measures$APL)),
  plot_densita + ylim(0, max(net_measures$Densità)),
  ncol = 3, align = "v"
)

print(combined_plot2)


plot_componenti <- ggplot(net_measures, aes(x = Paese, y = Componenti, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "N° Components") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot_dim_largest_comp <- ggplot(net_measures, aes(x = Paese, y = Dim_Largest_Comp, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Largest component dimension") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

combined_plot3 <- plot_grid(
  plot_componenti + ylim(0, max(net_measures$Componenti)),
  plot_dim_largest_comp + ylim(0, max(net_measures$Dim_Largest_Comp)),
  ncol = 2, align = "v"
)

print(combined_plot3)



plot_bridges <- ggplot(net_measures, aes(x = Paese, y = Bridges, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Bridges") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot_inclusiveness <- ggplot(net_measures, aes(x = Paese, y = Inclusiveness, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Inclusiveness") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot_transitivity <- ggplot(net_measures, aes(x = Paese, y = Transitivity, fill = Paese)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  labs(x = NULL, y = "Transitivity") +
  scale_fill_manual(values = country_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

combined_plot4 <- plot_grid(
  plot_bridges + ylim(0, max(net_measures$Bridges)),
  plot_inclusiveness + ylim(0, max(net_measures$Inclusiveness)),
  plot_transitivity + ylim(0, max(net_measures$Transitivity)),
  ncol = 3, align = "v"
)

print(combined_plot4)


















