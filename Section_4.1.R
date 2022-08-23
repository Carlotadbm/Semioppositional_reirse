#Carlota de Benito Moreno
#From (semi-)oppositional to non-oppositional middles: the case of Spanish reír(se)
#Section 4.1
#February 8th 2021, 1.0
#August 26th 2021, 2.0


#load libraries
library(tidyverse)
library(ggtext)
library(extrafont)

####
#Preprocess and analysis of "alegrar"

#load file
alegrar <- read_delim("alegrar.csv", delim = ";")

#clean data
alegrar_clean <- alegrar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  filter(!Perífrasis %in% c("causativa", "semicausativa")) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  select(CORDEMÁFORO, AUTOR, TÍTULO, FECHA, Reflexivo, Complemento_de) %>% 
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
  mutate(periodo = cut_interval(fecha, 33, labels = seq(from = 1250, to = 1975, by = 22))) # #33 22-year periods
  
#Number of valid examples
nrow(alegrar_clean)

#Number and proportion of MM cases in total
alegrar_clean %>% 
  count(Reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Number of repeated and ambiguous (transitive-intransitive) examples
alegrar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  count(Válido)

#Number of ambiguous (marked-unmarked) examples
alegrar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  count(Reflexivo) 

#Plot temporal evolution
##some periods are missing because they have no data, they must be included in the plot
###create vector with all periods
all_periods <- seq(from = 1250, to = 1975, by = 22) %>% 
  as.factor()
###create a vector with actually attested periods
actual_periods <- alegrar_clean %>% 
  distinct(periodo)
###create a vector with missing periods
missing_periods <- all_periods %>% 
  as_tibble_col(column_name = "periodo") %>% 
  anti_join(actual_periods)
###create a tibble with missing periods and their data (0 occurrences of any examples)
missing_periods <- missing_periods %>% 
  add_column(no = 0, si = 0)

##summarise data and plot it
alegrar_clean %>% 
  count(periodo, Reflexivo) %>% #count presence and absence of the reflexive
  spread(Reflexivo, n, fill = 0) %>% #reorganise the tibble
  add_row(missing_periods) %>% #add the missing periods data
  mutate(total = si + no, 
         freq_refl = si/total*100) %>%  #calculate frequencies
  ggplot(aes(x = periodo, y = freq_refl, group = 1)) + 
  geom_line() + #create line plot
  geom_point() + #modulate size of dots by total n
  geom_text(aes(label = total), nudge_y = -10) + #add labels of total n
  ylim(0,100) +
  labs(title = "Frequency of the middle marker in intransitive *alegrar(se)*", 
       caption = "(The figures indicate the total number of occurrences in each period.)", 
       x ="Period", y = "% middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown(), axis.text.x = element_text(angle = 45, hjust = 1))

#(Un)marked examples in causative and similar contexts
alegrar %>% 
  filter(CORDEMÁFORO %in% c("V", "V_manual", NA)) %>% 
  filter(Válido == "si") %>% 
  filter(Perífrasis %in% c("causativa", "semicausativa")) %>% 
  count(Perífrasis, Reflexivo)

#Plot frequency of prepositional (and other) objects
alegrar_clean %>% 
  mutate(Complemento_de = str_remove_all(Complemento_de, "\\?"),
         Complemento_de = str_replace(Complemento_de, "dativo", "dative"),
         Complemento_de = str_replace(Complemento_de, "no", "no_object")) %>% 
  filter(Complemento_de != "ambiguo") %>% 
  count(Complemento_de) %>% 
  mutate(prep_perc = n/sum(n)*100) %>% 
  ggplot(aes(y = n , x = reorder(Complemento_de,n))) +
  geom_col() +
  coord_flip() +
  labs(title = "Objects taken by intransitive *alegrar(se)*", 
       x ="Objects", y = "Occurrences") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown())

####
#Preprocess and analysis of "regocijar"

#load file
regocijar <- read_delim("regocijar.csv", delim = ";")

#clean data
regocijar_clean <- regocijar %>% 
  filter(Válido == "si") %>% 
  filter(is.na(Perífrasis)) %>% 
  filter(Reflexivo != "ambiguo") %>% 
  select(AUTOR, TÍTULO, FECHA, Reflexivo, Complemento_de) %>% 
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
  #distinct(fecha) %>% View
  mutate(AUTOR = ifelse(AUTOR == "ANÓNIMO", str_c(AUTOR, "_", TÍTULO), AUTOR)) %>%  #not all Anónimos are the same Anónimo
  mutate(periodo = cut_interval(fecha, 22, labels = seq(from = 1525, to = 1996, by = 22))) 

#Number of valid examples
nrow(regocijar_clean) 

#Number and proportion of MM cases in total
regocijar_clean %>% 
  count(Reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Plot temporal evolution
##some periods are missing because they have no data, they must be included in the plot
###create vector with all periods
all_periods <- seq(from = 1525, to = 1996, by = 22) %>% 
  as.factor()
###create a vector with actually attested periods
actual_periods <- regocijar_clean %>% 
  distinct(periodo)
###create a vector with missing periods
missing_periods <- all_periods %>% 
  as_tibble_col(column_name = "periodo") %>% 
  anti_join(actual_periods)
###create a tibble with missing periods and their data (0 occurrences of any examples)
missing_periods <- missing_periods %>% 
  add_column(si = 0)

##summarise data and plot it
regocijar_clean %>% 
  count(periodo, Reflexivo) %>% #count presence and absence of the reflexive
  spread(Reflexivo, n, fill = 0) %>% #reorganise the tibble
  add_row(missing_periods) %>% 
  mutate(freq_refl = si/si*100) %>% #calculate frequencies, they're all 100%, it's pretty absurd
  ggplot(aes(x = periodo, y = freq_refl, group = 1)) + 
  geom_line() + #create line plot
  geom_point() + #modulate size of dots by total n
  geom_text(aes(label = si), nudge_y = -10) + #add labels of total n
  ylim(0,100) +
  labs(title = "Frequency of the middle marker in intransitive *regocijar(se)*", 
       caption = "(The figures indicate the total number of occurrences in each period.)", 
       x ="Period", y = "% middle marker") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown(), axis.text.x = element_text(angle = 45, hjust = 1))

#(Un)marked examples in causative and similar contexts
regocijar %>% 
  filter(Válido == "si") %>% 
  filter(Perífrasis == "causativa") %>% 
  count(Perífrasis, Reflexivo)

#Plot frequency of prepositional (and other) objects
regocijar_clean %>% 
  mutate(Complemento_de = str_replace(Complemento_de, "de \\+ dativo", "de"),
         Complemento_de = str_replace(Complemento_de, "dativo", "dative"),
         Complemento_de = str_replace(Complemento_de, "no", "no_object")) %>% 
  filter(Complemento_de != "ambiguo") %>% 
  count(Complemento_de) %>% 
  mutate(prep_perc = n/sum(n)*100) %>% 
  ggplot(aes(y = n , x = reorder(Complemento_de,n))) +
  geom_col() +
  coord_flip() +
  labs(title = "Objects taken by intransitive *regocijar(se)*", 
       x ="Objects", y = "Occurrences") +
  theme_light(base_family = "Times New Roman", base_size = 16) +
  theme(plot.title = element_markdown())

