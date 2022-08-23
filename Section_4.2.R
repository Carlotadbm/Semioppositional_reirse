#Carlota de Benito Moreno
#From (semi-)oppositional to non-oppositional middles: the case of Spanish reír(se)
#Section 4.2
#February 9th 2021, 1.0
#August 27th 2021, 2.0

#load libraries
library(tidyverse)
library(ggtext)
library(extrafont)
library(lme4)
library(interactions)

####
#burlar

##load file
burlar <- read_delim("burlar.csv", delim = ";")

##clean data
burlar_clean <- burlar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  filter(!Perífrasis %in% c("causativa", "semicausativa")) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  select(CORDEMÁFORO, AUTOR, TÍTULO, FECHA, Reflexivo, Complemento_de, Aspecto_verbal, Sujeto) %>% 
  mutate(fecha = str_remove(FECHA, "\\[.+\\] ?"), 
         fecha = str_remove_all(fecha, "[a-z]\\.?"), 
         fecha = str_remove(fecha, "\\(.+\\) ?")) %>% 
  separate(fecha, into = c("fecha1", "fecha2"), sep = "-") %>% 
  mutate(fecha1 = as.double(fecha1), 
         fecha2 = as.double(fecha2),
         fecha2 = ifelse(is.na(fecha2), fecha1, fecha2), 
         fecha1 = round((fecha1 + fecha2)/2)) %>% 
  rename(fecha = fecha1) %>% 
  select(-fecha2) %>% 
  mutate(AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TÍTULO), AUTOR)) %>%  #not all Anónimos are the same Anónimo
  mutate(Verbo = "burlar")


##range of dates
range(burlar_clean$fecha) 

##Number of valid examples
nrow(burlar_clean)

##Number and proportion of MM cases in total
burlar_clean %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = n/sum(n)*100)

##Number of excluded examples
burlar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  mutate(Válido = str_replace(Válido, "recíproco", "transitivo")) %>% 
  count(Válido)

####
# escarnecer

##load file
escarnecer <- read_delim("escarnecer.csv", delim = ";")

##clean data
escarnecer_clean <- escarnecer %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  select(CORDEMÁFORO, AUTOR, TÍTULO, FECHA, Reflexivo, Complemento_de, Aspecto_verbal, Sujeto) %>% 
  mutate(fecha = str_remove(FECHA, "\\[.+\\] ?"), 
         fecha = str_remove_all(fecha, "[a-z]\\.?"), 
         fecha = str_remove(fecha, "\\(.+\\) ?")) %>% 
  separate(fecha, into = c("fecha1", "fecha2"), sep = "-") %>% 
  mutate(fecha1 = as.double(fecha1), 
         fecha2 = as.double(fecha2),
         fecha2 = ifelse(is.na(fecha2), fecha1, fecha2), 
         fecha1 = round((fecha1 + fecha2)/2)) %>% 
  rename(fecha = fecha1) %>% 
  select(-fecha2) %>% 
  mutate(AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TÍTULO), AUTOR)) %>% #not all Anónimos are the same Anónimo
  mutate(Verbo = "escarnecer")

##range of dates
range(escarnecer_clean$fecha) #1260 1596

##Number of valid examples
nrow(escarnecer_clean)

##Number and proportion of MM cases in total
escarnecer_clean %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = n/sum(n)*100)

##Number of excluded examples
escarnecer %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  mutate(Válido = str_replace(Válido, "pasiva", "transitivo")) %>% 
  count(Válido)


####
# mofar

##load file
mofar <- read_delim("mofar.csv", delim = ";")

##clean data
mofar_clean <- mofar %>% 
  filter(Válido == "si") %>% 
  filter(Reflexivo != "ambiguo") %>% 
  select(CORDEMÁFORO, AUTOR, TÍTULO, FECHA, Reflexivo, Complemento_de, Aspecto_verbal, Sujeto) %>% 
  mutate(fecha = str_remove(FECHA, "\\[.+\\] ?"), 
         fecha = str_remove_all(fecha, "[a-z]\\.?"), 
         fecha = str_remove(fecha, "\\(.+\\) ?")) %>% 
  separate(fecha, into = c("fecha1", "fecha2"), sep = "-") %>% 
  mutate(fecha1 = as.double(fecha1), 
         fecha2 = as.double(fecha2),
         fecha2 = ifelse(is.na(fecha2), fecha1, fecha2), 
         fecha1 = round((fecha1 + fecha2)/2)) %>% 
  rename(fecha = fecha1) %>% 
  select(-fecha2) %>% 
  mutate(AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TÍTULO), AUTOR)) %>% #not all Anónimos are the same Anónimo
  mutate(Verbo = "mofar")

##range of dates
range(mofar_clean$fecha) 

##Number of valid examples
nrow(mofar_clean)

##Number and proportion of MM cases in total
mofar_clean %>% 
  count(Reflexivo) %>% 
  mutate(refl_perc = n/sum(n)*100)

##Number of excluded examples (transitive)
mofar %>% 
  count(Válido)

##Number of excluded examples (ambiguous marked-unmarked)
mofar %>% 
  filter(Válido == "si") %>% 
  count(Reflexivo)


#Plot temporal evolution of all three mocking verbs

##join all tables and calculate intervals
mocking_verbs <- burlar_clean %>% 
  add_row(escarnecer_clean) %>% 
  add_row(mofar_clean) %>% 
  mutate(periodo = cut_interval(fecha, 33, labels = seq(from = 1260, to = 1975, by = 22))) #%>% #33 22-year periods


##Because some authors are overrepresented in the corpus, 
##I take a maximum of 5 examples per author, period and verb for the plot
###set seed for random sampling
set.seed(22)
###create table with the reduced random sample
mocking_verbs_normalised <- mocking_verbs %>% 
  group_by(Verbo, periodo, AUTOR) %>% 
  slice(sample(5)) %>% 
  ungroup()

##some periods are missing because they have no data, they must be included in the plot
###create vector with all periods
all_periods <- seq(from = 1260, to = 1975, by = 22) %>% 
  as.factor()
###create a vector with actually attested periods
actual_periods <- mocking_verbs %>% 
  distinct(periodo)
###create a vector with missing periods
missing_periods <- all_periods %>% 
  as_tibble_col(column_name = "periodo") %>% 
  anti_join(actual_periods)
###create a tibble with missing periods and their data (0 occurrences of any examples)
missing_periods <- missing_periods %>% 
  add_column(Verbo = "burlar", .before = T) %>% 
  mutate(Verbo = ifelse(periodo == 1678, "escarnecer", Verbo)) %>% 
  add_column(no = 0, si = 0)

##summarise data and plot it
mocking_verbs_normalised %>% 
  count(Verbo, periodo, Reflexivo) %>% #count presence and absence of the reflexive by verb
  spread(Reflexivo, n, fill = 0) %>% #reorganise the tibble
  add_row(missing_periods) %>% 
  mutate(total = si + no, 
         freq_refl = si/total*100) %>% #calculate frequencies
  ggplot(aes(x = periodo, y = freq_refl, group = 1)) + 
  geom_line() + #create line plot
  geom_point() + 
  geom_text(aes(label = total), nudge_y = 10) + #add labels with total figures
  ylim(0,130) + #adjust y axis so that the labels fit
  labs(title = "Frequency of the middle marker in intransitive mocking verbs", 
       caption = "(The figures indicate the total number of occurrences in each period.)", 
       x ="Period", y = "% middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown(), axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Verbo, nrow = 3)



#Range of transitive uses
##burlar
burlar_trans <- burlar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido %in% c("transitivo", "recíproco")) %>% 
  select(CORDEMÁFORO, AUTOR, TÍTULO, FECHA) %>% 
  mutate(fecha = str_remove(FECHA, "\\[.+\\] ?"), 
         fecha = str_remove_all(fecha, "[a-z]\\.?"), 
         fecha = str_remove(fecha, "\\(.+\\) ?")) %>% 
  separate(fecha, into = c("fecha1", "fecha2"), sep = "-") %>% 
  mutate(fecha1 = as.double(fecha1), 
         fecha2 = as.double(fecha2),
         fecha2 = ifelse(is.na(fecha2), fecha1, fecha2), 
         fecha1 = round((fecha1 + fecha2)/2)) %>% 
  rename(fecha = fecha1) %>% 
  select(-fecha2)
###range of dates
range(burlar_trans$fecha) #1400 1971

##escarnecer
escarnecer_trans <- escarnecer %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido %in% c("transitivo", "pasiva")) %>% 
  select(CORDEMÁFORO, AUTOR, TÍTULO, FECHA) %>% 
  mutate(fecha = str_remove(FECHA, "\\[.+\\] ?"), 
         fecha = str_remove_all(fecha, "[a-z]\\.?"), 
         fecha = str_remove(fecha, "\\(.+\\) ?")) %>% 
  separate(fecha, into = c("fecha1", "fecha2"), sep = "-") %>% 
  mutate(fecha1 = as.double(fecha1), 
         fecha2 = as.double(fecha2),
         fecha2 = ifelse(is.na(fecha2), fecha1, fecha2), 
         fecha1 = round((fecha1 + fecha2)/2)) %>% 
  rename(fecha = fecha1) %>% 
  select(-fecha2)
###range of dates
range(escarnecer_trans$fecha) #1260 1989

##mofar
mofar_trans <- mofar %>% 
  filter(Válido == "transitivo") %>% 
  select(CORDEMÁFORO, AUTOR, TÍTULO, FECHA) %>% 
  mutate(fecha = str_remove(FECHA, "\\[.+\\] ?"), 
         fecha = str_remove_all(fecha, "[a-z]\\.?"), 
         fecha = str_remove(fecha, "\\(.+\\) ?")) %>% 
  separate(fecha, into = c("fecha1", "fecha2"), sep = "-") %>% 
  mutate(fecha1 = as.double(fecha1), 
         fecha2 = as.double(fecha2),
         fecha2 = ifelse(is.na(fecha2), fecha1, fecha2), 
         fecha1 = round((fecha1 + fecha2)/2)) %>% 
  rename(fecha = fecha1) %>% 
  select(-fecha2)
###range of dates
range(mofar_trans$fecha) #1438 1807


#(Un)marked examples in causative and similar contexts
##burlar
burlar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  filter(Perífrasis %in% c("causativa", "semicausativa")) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  count(Perífrasis, Reflexivo)

##escarnecer
escarnecer %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  filter(Perífrasis %in% c("causativa", "semicausativa")) %>% 
  count(Perífrasis, Reflexivo)
##mofar
mofar %>% 
  filter(Válido == "si") %>% 
  filter(Perífrasis %in% c("causativa", "semicausativa")) %>% 
  count(Perífrasis, Reflexivo)

#Plot frequency of prepositional (and other) objects
##Because some authors are overrepresented in the corpus, 
##I take a maximum of 4 examples per author, text and verb for the plot
###set seed for random sampling
set.seed(55)
###create table with the reduced random sample
mocking_verbs_normalised2 <- mocking_verbs %>% 
  group_by(Verbo, TÍTULO, AUTOR) %>% 
  slice_sample(n=5) %>% 
  ungroup()

##sumarise data and plot it
mocking_verbs_normalised2 %>% 
  mutate(Complemento_de = str_remove_all(Complemento_de, "\\?"),
         Complemento_de = str_replace(Complemento_de, "dativo", "dative"),
         Complemento_de = str_replace(Complemento_de, "si", "de"),
         Complemento_de = str_replace(Complemento_de, "no", "no_object")) %>% 
  filter(!Complemento_de %in% c("ambiguo", "duda")) %>% 
  count(Verbo, Complemento_de) %>% 
  mutate(prep_perc = n/sum(n)*100) %>% 
  ggplot(aes(y = n, x = reorder(Complemento_de,n))) +
  geom_col() +
  coord_flip() +
  labs(title = "Objects taken by intransitive mocking verbs", 
       x ="Objects", y = "Occurrences") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown()) + 
  facet_wrap(~Verbo, scales = "free_x")

#Statistical model

##clean data
mocking_verbs_model <- mocking_verbs %>% 
  filter(!Complemento_de %in% c("ambiguo", "duda")) %>% 
  filter(Verbo != "escarnecer") %>% #I remove escarnecer for it shows almost no variation
  filter(Sujeto != "ambiguo") %>% 
  mutate(Complemento_de = ifelse(Complemento_de == "no", "no_object", "object")) %>% 
  mutate(Reflexivo = ifelse(Reflexivo == "si", "MM", "no_MM")) %>% 
  mutate(Aspecto_verbal = str_replace(Aspecto_verbal, "(i?m?perfectivo)", "finite")) %>% 
  mutate(Aspecto_verbal = str_replace(Aspecto_verbal, "(gerundio|infinitivo)", "non_finite")) 

##check number of authors
mocking_verbs_model %>% 
  distinct(AUTOR) %>% 
  nrow()

##each relevant column as factors or numeric, relevel so that it is easier to interpret
mocking_verbs_model$Reflexivo <- factor(mocking_verbs_model$Reflexivo, levels = c("no_MM", "MM"))
mocking_verbs_model$Verbo <- as.factor(mocking_verbs_model$Verbo)
mocking_verbs_model$fecha <- as.numeric(mocking_verbs_model$fecha)
mocking_verbs_model$Complemento_de <- factor(mocking_verbs_model$Complemento_de, levels = c("object", "no_object"))
mocking_verbs_model$Aspecto_verbal <- factor(mocking_verbs_model$Aspecto_verbal, levels = c("finite", "non_finite"))

##maximal model
###note: there is no way of using a glmer using AUTOR as a random factor, because it has too many levels
###Conflating them (for instance, grouping all authors with less than 21 examples together) does not help either
mocking_verbs_glm <- glm(Reflexivo ~ Verbo + fecha + Aspecto_verbal + Complemento_de +
                                           Verbo:fecha + Aspecto_verbal:fecha + Complemento_de:fecha, 
                                         family = "binomial", data = mocking_verbs_model)

###summary of the model
summary(mocking_verbs_glm)
###tidy version of the model and round to 3 digits
mocking_verbs_glm_tidy <- tidy(mocking_verbs_glm, exponentiate = F, conf.int = T) %>% 
  mutate_if(is.double, round, digits = 3)

#Characteristics of the subject
##burlar
burlar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  filter(!Perífrasis %in% c("causativa", "semicausativa")) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  count(Sujeto)

##mofar
mofar %>% 
  filter(Válido == "si") %>% 
  filter(!Perífrasis %in% c("causativa", "semicausativa")) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  count(Sujeto)

#Periphrases
##burlar
burlar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  filter(!Perífrasis %in% c("causativa", "semicausativa")) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  count(Perífrasis)

##mofar
mofar %>% 
  filter(Válido == "si") %>% 
  filter(!Perífrasis %in% c("causativa", "semicausativa")) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  count(Perífrasis)

###Plotting interactions
####Interaction between verb and time
interact_plot(model = mocking_verbs_glm, pred = fecha, modx = Verbo, interval = T) +
  labs(title = "Interaction between verb and time", 
       x ="Time", y = "Middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(legend.title = element_blank(), plot.title = element_markdown())


####Interaction between verb form and time
interact_plot(model = mocking_verbs_glm, pred = fecha, modx = Aspecto_verbal, interval = T) +
  labs(title = "Interaction between verb form and time", 
       x ="Time", y = "Middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(legend.title = element_blank(), plot.title = element_markdown())


####Interaction between type of object and time
interact_plot(model = mocking_verbs_glm, pred = fecha, modx = Complemento_de, interval = T) +
  labs(title = "Interaction between type of object and time", 
       x ="Time", y = "Middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(legend.title = element_blank(), plot.title = element_markdown())

