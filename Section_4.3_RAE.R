#Carlota de Benito Moreno
#From (semi-)oppositional to non-oppositional middles: the case of Spanish reír(se)
#Section 4.1 (RAE corpora)
#February 11th 2021, 1.0
#August 30th 2021, 2.0

#load libraries
library(tidyverse)
library(lme4)
library(ggtext)
library(extrafont)
library(interactions)


####
#Load and clean all tables

##CDH until 1975
###load file
cdh <- read_delim("reir_CDH.csv", delim = ";")

###clean table
cdh_clean <- cdh %>% 
  filter(Cordemaforo %in% c("V", NA)) %>%
  filter(Valido == "si") %>% 
  #clean dates to have operative figures
  mutate(fecha = str_remove(FECHA, "\\[.+\\] ?"), #remove the annotations in brackets
         fecha = str_remove_all(fecha, "[a-z]"), #ignoring the notes on ante, circa, post
         fecha = str_replace(fecha, "1445-80\\(1445-1480\\)", "1445-1480"), #cleaning the single result that didn't follow the pattern
         fecha = str_remove(fecha, "\\(.+\\) ?")) %>% 
  separate(fecha, into = c("fecha1", "fecha2"), sep = "-") %>% #separating them to calculate the ranges
  mutate(fecha1 = as.double(fecha1), 
         fecha2 = as.double(fecha2),
         fecha2 = ifelse(is.na(fecha2), fecha1, fecha2), 
         fecha1 = round((fecha1 + fecha2)/2)) %>% 
  rename(fecha = fecha1) %>% 
  select(-fecha2) %>% 
  mutate(AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TITULO), AUTOR)) %>% #not all Anónimos are the same Anónimo
  select(EjemploID, AUTOR, TITULO, fecha, Valido, Reflexivo, Complemento_de, Aspecto_verbal, Sujeto, Perifrasis) %>% 
  mutate(corpus = "cdh_1975")


  
##CDH 1976-1999
###load file
cdh_modern <- read_delim("reir_cdh1976-1999_1perdoc_analizado.csv", delim = ";")

###clean table
cdh_modern_clean <- cdh_modern %>% 
  filter(Valido == "si") %>% 
  mutate(AUTOR = ifelse(is.na(AUTOR), "ANÓNIMO", AUTOR),
         AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TITULO), AUTOR)) %>% #not all Anónimos are the same Anónimo
  select(EjemploID, AUTOR, TITULO, fecha, Valido, Reflexivo, Complemento_de, Aspecto_verbal, Sujeto, Perifrasis) %>% 
  mutate(corpus = "cdh_1999")

##CREA
###load file
crea <- read_delim("reir_crea2000_1perdoc_analizado.csv", delim = ";")

###clean table
crea_clean <- crea %>% 
  filter(Valido == "si") %>% 
  rename(fecha = FECHA) %>% 
  mutate(AUTOR = ifelse(is.na(AUTOR), "ANÓNIMO", AUTOR),
         AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TITULO), AUTOR)) %>% #not all Anónimos are the same Anónimo
  select(EjemploID, AUTOR, TITULO, fecha, Valido, Reflexivo, Complemento_de, Aspecto_verbal, Sujeto, Perifrasis) %>% 
  mutate(corpus = "crea")

##CORPES
###load file
corpes <- read_delim("reir_corpes_1perdoc_analizado.csv", delim = ";")

###clean table
corpes_clean <- corpes %>% 
  filter(Valido == "si") %>% 
  rename(fecha = FECHA) %>% 
  mutate(AUTOR = ifelse(is.na(AUTOR), "ANÓNIMO", AUTOR),
         AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TITULO), AUTOR)) %>% #not all Anónimos are the same Anónimo
  select(EjemploID, AUTOR, TITULO, fecha, Valido, Reflexivo, Complemento_de, Aspecto_verbal, Sujeto, Perifrasis) %>% 
  mutate(corpus = "corpes")

##Join all RAE corpora together
rae_clean <- cdh_clean %>% 
  add_row(cdh_modern_clean) %>% 
  add_row(crea_clean) %>% 
  add_row(corpes_clean)

##Number of total examples manually analysed
nrow(cdh) + nrow(cdh_modern) + nrow(crea) + nrow(corpes)

##Number of total valid examples
rae_clean %>% 
  nrow()

##Number of transitive examples
cdh %>% 
  filter(Valido == "transitivo") %>% 
  nrow() +
  cdh_modern %>% 
     filter(Valido == "transitivo") %>% 
     nrow() + 
  crea %>% 
  filter(Valido == "transitivo") %>% 
  nrow() + 
  corpes %>% 
  filter(Valido == "transitivo") %>% 
  nrow()

##Number of ambiguous examples (marked-unmarked) 
rae_clean %>% 
  count(Reflexivo) 
###Check: they all come from potentially reflexive periphrases
rae_clean %>% 
  filter(Reflexivo == "ambiguo") %>% 
  count(Perifrasis) 

##Number and proportion of MM cases by periphrasis type
rae_clean %>% 
  filter(Reflexivo != "ambiguo") %>% 
  count(Perifrasis, Reflexivo) %>% 
  group_by(Perifrasis) %>% 
  mutate(perc_refl = n/sum(n)*100, 
         total = sum(n))


##Plot temporal evolution of "reír"
###Because some authors are overrepresented in the corpus, 
###I take a maximum of 5 examples per author and period for the plot

####set seed for random sampling
set.seed(88)
####create table with the reduced random sample
rae_historical <- rae_clean %>% 
  filter(Reflexivo != "ambiguo") %>% 
  filter(!Perifrasis %in% c("causativa", "reflexiva", "semicausativa")) %>% 
  mutate(periodo = cut_interval(fecha, 36, labels = seq(from = 1228, to = 2011, by = 22))) %>% #36 22-year periods
  group_by(periodo, AUTOR) %>% #some authors are over represented 
  slice_sample(n=5) %>% 
  ungroup() 


###some periods are missing because they have no data, they must be included in the plot
####create vector with all periods
all_periods <- seq(from = 1228, to = 2011, by = 22) %>% 
  as.factor()
####create a vector with actually attested periods
actual_periods <- rae_historical %>% 
  distinct(periodo)
####create a vector with missing periods
missing_periods <- all_periods %>% 
  as_tibble_col(column_name = "periodo") %>% 
  anti_join(actual_periods)
####create a tibble with missing periods and their data (0 occurrences of any examples)
missing_periods <- missing_periods %>% 
  add_column(no = 0, si = 0)

###summarise data and plot it
rae_historical %>% 
  count(periodo, Reflexivo) %>% #count presence and absence of the reflexive
  spread(Reflexivo, n, fill = 0) %>% #reorganise the tibble
  add_row(missing_periods) %>% 
  mutate(total = si + no, 
         freq_refl = si/total*100) %>% #calculate frequencies
  ggplot(aes(x = periodo, y = freq_refl, group = 1)) + 
  geom_line() + #create line plot
  geom_point() + #modulate size of dots by total n
  geom_text(aes(label = total), nudge_y = -5) + #add labels of total n
  ylim(-10,100) +
  labs(title = "Frequency of the middle marker in intransitive *reír(se)*", 
       caption = "(The figures indicate the total number of occurrences in each period.)", 
       x ="Period", y = "% middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown(), axis.text.x = element_text(angle = 45, hjust = 1))


##Plot frequency of prepositional (and other) objects by corpus
###Because some authors are overrepresented in the corpus, 
###I take a maximum of 5 examples per author and text for the plot

####set seed for random sampling
set.seed(999)
####create table with the reduced random sample
rae_historical_normalised <- rae_clean %>% 
  filter(Reflexivo != "ambiguo") %>% 
  filter(!Perifrasis %in% c("causativa", "reflexiva", "semicausativa")) %>% 
  group_by(TITULO, AUTOR) %>% #some authors are over represented 
  slice_sample(n=5) %>% 
  ungroup() 
###sumarise data 
rae_historical_plot <- rae_historical_normalised %>% 
  mutate(corpus = ifelse(corpus == "cdh_1975", "Nuclear CDH (until 1975)", "Other RAE corpora (from 1976)")) %>% 
  mutate(Complemento_de = str_replace(Complemento_de, "dativo", "dative"),
         Complemento_de = str_replace(Complemento_de, "no|reflexivo", "no_object"), 
         Complemento_de = str_replace(Complemento_de, "si", "de")) %>% 
  mutate(Reflexivo = ifelse(Reflexivo == "no", "No MM", "MM")) %>% 
  filter(Complemento_de != "ambiguo") %>% 
  count(corpus, Complemento_de, Reflexivo) %>% 
  group_by(corpus, Complemento_de) %>% 
  mutate(prep_perc = round(n/sum(n)*100, 1), 
         total = sum(n)) 

###plot it
rae_historical_plot %>% 
  ggplot(aes(y = n , x = reorder(Complemento_de, n), fill = Reflexivo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ corpus, scales = "free_x") +
  labs(title = "Proportion of the middle marker by object taken by *reír(se)*", 
       x ="Objects", y = "Occurrences", fill = "Middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown())


#Statistical model

##clean data
###note: there is no way of using a glmer using Author as a random factor, because it has too many levels
rae_clean %>% 
  distinct(AUTOR) %>% 
  nrow()
rae_clean %>% 
  count(AUTOR, sort = T) %>% 
  View()
###This is why I use rae_historical, which has a maximum of 5 examples per Author and period
rae_model <- rae_historical %>% 
  mutate(Reflexivo = ifelse(Reflexivo == "no", "no_MM", "MM")) %>% 
  filter(!Complemento_de %in% c("a", "ambiguo", "ante", "dativo", "por", "sobre", "en")) %>% 
  mutate(Complemento_de = str_replace(Complemento_de, "no|reflexivo", "no_object"), 
         Complemento_de = str_replace(Complemento_de, "si", "de")) %>% 
  mutate(Aspecto_verbal = ifelse(Aspecto_verbal %in% c("perfectivo", "imperfectivo"), "finite", "non_finite")) %>% 
  filter(!Sujeto == "ambiguo") %>% 
  mutate(Sujeto = str_replace(Sujeto, "ado$", "ate")) %>% 
  rename(Reflexive = Reflexivo, 
         Object = Complemento_de, 
         Verbal_form = Aspecto_verbal, 
         Subject = Sujeto, 
         Author = AUTOR, 
         Year = fecha)

##each relevant column as factors or numeric, relevel so that it is easier to interpret
rae_model$Reflexive <- factor(rae_model$Reflexive, levels = c("no_MM", "MM"))
rae_model$Year <- as.numeric(rae_model$Year)
rae_model$Object <- factor(rae_model$Object, levels = c("no_object", "de", "con"))
rae_model$Verbal_form <- factor(rae_model$Verbal_form, levels = c("finite", "non_finite"))
rae_model$Subject <- factor(rae_model$Subject, levels = c("animate", "inanimate", "impersonal"))

##maximal model
rae_glm <- glm(Reflexive ~ Object + Verbal_form + Subject + Year + 
                   Object:Year + Verbal_form:Year + Subject:Year, 
                   family = "binomial", data = rae_model)

###summary of the model
summary(rae_glm)
###tidy version of the model and round to 3 digits
rae_glm_tidy <- tidy(rae_glm, exponentiate = F, conf.int = T) %>% 
  mutate_if(is.double, round, digits = 3)

###Plotting interactions
####Interaction between type of object and time
interact_plot(model = rae_glm, pred = Year, modx = Object, interval = T) +
  labs(title = "*Reír(se)*: Interaction between type of object and time", 
       x ="Time", y = "Middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(legend.title = element_blank(), plot.title = element_markdown()) 


####Interaction between tyoe of subject and time
interact_plot(model = rae_glm, pred = Year, modx = Subject, interval = T) +
  labs(title = "*Reír(se)*: Interaction between type of the subject and time", 
       x ="Time", y = "Middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(legend.title = element_blank(), plot.title = element_markdown())

####Interaction between animacy of subject and time
interact_plot(model = rae_glm, pred = Year, modx = Verbal_form, interval = T) +
  labs(title = "*Reír(se)*: Interaction between verbal form and time", 
       x ="Time", y = "Middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(legend.title = element_blank(), plot.title = element_markdown())

