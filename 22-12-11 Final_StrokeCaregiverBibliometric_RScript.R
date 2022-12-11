## Preamble ---------------------------
##
## Name: Dataset for Manuscript "Research on Stroke Caregiver, A bibliometric Analysis"
## Purpose: Bibliometric Analysis
## Author: Mohd Azmi Bin Suliman
## Date Created: 2022-12-11
## Email: mohd.azmi@student.usm.my; m.azmi.suliman@gmail.com
##

## Packages ---------------------------
##
## if tidyverse available, can load tidyverse.
## library(tidyverse)

require(dplyr)
require(magrittr)
require(forcats)
require(stringr)
# require(tidyr)
require(ggplot2)
require(bibliometrix)

## Data Import ---------------------------

# data available in Zenodo: https://zenodo.org/record/7421945

linkfile_1 <- "https://zenodo.org/record/7421945/files/22-12-07%20WOS_TIstcgp1.bib?download=1"
linkfile_2 <- "https://zenodo.org/record/7421945/files/22-12-07%20WOS_TIstcgp2.bib?download=1"

bibds1_wos <- convert2df(file = linkfile_1, dbsource = "wos", format = "bibtex")
bibds2_wos <- convert2df(file = linkfile_2, dbsource = "wos", format = "bibtex")

bibds_wos <- bind_rows(bibds1_wos, bibds2_wos)

## Convert to bibliometrix file type ---------------------------

bibres <- biblioAnalysis(bibds_wos)


## General Information ---------------------------

## bibliometric summary

bibres_summary <- summary(bibres)

## bibliometric summary plot
bibres_plot <- plot(bibres, k = 10)


## Influential Articles ---------------------------

# Top 10 Most Cited Articles on Stroke Caregiver Burden

tibble(Title = bibds_wos$TI,
       Author = bibds_wos$AU,
       Year = bibds_wos$PY,
       DOI = bibds_wos$DI,
       Citations = bibds_wos$TC,
       CitaPerYear = Citations/(2023-Year)) %>%
  mutate(across(.cols = c(Title, Author),
                .fns = str_to_title),
         CitaPerYear = round(CitaPerYear, 2)) %>%
  arrange(desc(Citations)) %>%
  head(n=10)

# Top 10 Most Cited per Year Article 

tibble(Title = bibds_wos$TI,
       Author = bibds_wos$AU,
       Year = bibds_wos$PY,
       DOI = bibds_wos$DI,
       Citations = bibds_wos$TC,
       CitaPerYear = Citations/(2023-Year)) %>%
  mutate(across(.cols = c(Title, Author),
                .fns = str_to_title),
         CitaPerYear = round(CitaPerYear, 2)) %>%
  arrange(desc(CitaPerYear)) %>%
  head(n=10)

## Language ---------------------------

bibds_wos %>% 
  group_by(LA) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n) * 100,
         percent = round(percent,1)) %>% 
  arrange(desc(n))

## Authors ---------------------------

# Number of authors per articles

bibds_noaufreq <- bibds_wos %>% 
  select(TI, AU, DT) %>% 
  tibble() %>% 
  mutate(no_auth = str_count(AU, pattern = ";") + 1,
         DT = fct_recode(DT,
                         "ARTICLE" = "PROCEEDINGS PAPER",
                         "ARTICLE" = "ARTICLE; PROCEEDINGS PAPER",
                         "ARTICLE" = "ARTICLE; EARLY ACCESS",
                         "REVIEW" = "REVIEW; EARLY ACCESS")) %>% 
  rename("paper" = "TI", "author" = "AU", "type" = "DT") %>% 
  group_by(no_auth, type) %>% 
  summarise(freq = n(), .groups = "drop") %>% 
  mutate(percent = freq / sum(freq) * 100,
         percent = round(percent,1))

bibds_noaufreq %>%
  mutate(type = str_to_title(type)) %>% 
  ggplot(aes(no_auth, freq, fill = type)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Number of Authors", y = "Frequency (Number of Articles)", fill = "Type") + 
  scale_x_continuous(breaks = seq(0,30,4)) +
  scale_y_continuous(breaks = seq(0,200,20)) +
  theme_bw() +
  theme(legend.position = "top")

# Most Productive Authors

bibres_aulist <- bibres$Authors

bibres_autable <- tibble(Rank = seq_along(bibres_aulist),
                         Author = rownames(bibres_aulist),
                         Np = as.integer(bibres_aulist)) %>% 
  mutate(Author = fct_reorder(Author, Rank))

bibres_autable %>% 
  rename("Number of Publications" = "Np") %>% 
  mutate(Author = stringr::str_to_title(Author),
         percent = `Number of Publications` / bibres$Articles * 100,
         percent = round(percent,1)) %>% 
  head(n = 15)

# Author Coupling Analysis

bib_AuCoupling_NetMatrix <- biblioNetwork(bibds_wos, analysis = "coupling",
                                          network = "authors", sep = ";")

bib_AuCoupling_Plot <- networkPlot(bib_AuCoupling_NetMatrix, n = 25, 
                                   cluster = "optimal", type = "auto", 
                                   size.cex = T, size = 20, remove.multiple = F,
                                   Title = "Bibliographic coupling of the authors",
                                   alpha = .7)

## Authors Keyword and WOS Keyword-Plus ---------------------------

# Top 10 Most Relevance Keyword 

cbind(Rank = 1:10,
      bibres_summary$MostRelKeywords)

# Author's keyword co-occurrence network map

bib_kwco_NetMatrix <- biblioNetwork(bibds_wos, analysis = "co-occurrences", 
                                    network = "keywords", sep = ";")

bib_kwco_Plot <- networkPlot(bib_kwco_NetMatrix, normalize = "association", 
                             n = 20, Title = "Keyword Co-occurences", 
                             cluster = "optimal", type = "fruchterman", 
                             size.cex = T, size = 20,  remove.multiple = F, 
                             edgesize = 7, labelsize = 3, label.cex = T, 
                             label.n = 20, edges.min = 10)

## Journal ---------------------------

# Bradford's Law of Scattering

bib_bradford <- bradford(bibds_wos)

bib_bradfordtable <- bib_bradford$table %>% 
  select(Zone, Freq, Rank) %>% 
  tibble() %>% 
  group_by(Zone) %>% 
  summarise(nSO = n(),
            nArt = sum(Freq),
            RankRange = str_c(min(Rank), max(Rank), sep = "-")) %>% 
  mutate(percent = nArt / sum(nArt) * 100,
         percent = round(percent,1))

bib_bradfordtable %>% 
  select(-percent) %>% 
  rename("No of Source (Journal)" = "nSO",
         "Total Article" = "nArt",
         "Source Rank" = "RankRange")

# Journals in Zone 1 of Bradford's Law

bib_bradford$table %>% 
  filter(Zone == "Zone 1") %>% 
  tibble() %>% 
  mutate(SO = stringr::str_to_title(SO)) %>% 
  rename("Journal Name" = "SO") %>% 
  relocate(Rank)

# Journal Co-citation Analysis 

bib_CRSO <- metaTagExtraction(bibds_wos, Field = "CR_SO", sep = ";")

bib_CRSO_NetMatrix <- biblioNetwork(bib_CRSO, analysis = "co-citation", 
                                    network = "sources", sep = ";")

bib_CRSO_Plot <- networkPlot(bib_CRSO_NetMatrix, n = 25, 
                             Title = "Co-citation Network", type = "auto", 
                             size.cex = T, size = 20, remove.multiple = F,
                             labelsize = 1, edgesize = 5, edges.min = 1, alpha = .7)

## Institution ---------------------------

# Most Productive Institution

bibres_instlist <- bibres$Affiliations

bibres_insttable <- tibble(Rank = seq_along(bibres_instlist),
                           InstitutionAffiliation = rownames(bibres_instlist),
                           Np = as.integer(bibres_instlist)) %>% 
  mutate(InstitutionAffiliation = fct_reorder(InstitutionAffiliation, Rank))

bibres_insttable %>% 
  rename("Number of Publications" = "Np",
         "Instituion Name" = "InstitutionAffiliation") %>% 
  mutate(`Instituion Name` = stringr::str_to_title(`Instituion Name`)) %>% 
  head(n = 10)

# Institution Collaboration Network

bib_educolab_NetMatrix <- biblioNetwork(bibds_wos, analysis = "collaboration",
                                        network = "universities", sep = ";")

bib_educolab_Plot <- networkPlot(bib_educolab_NetMatrix, n = 100, 
                                 cluster = "optimal", type = "auto",
                                 size.cex = F, size = 5, remove.multiple = F,
                                 labelsize=1, alpha = 1, edgesize = 2,
                                 edges.min = 1, remove.isolates = T, 
                                 community.repulsion = 0, label = F,
                                 Title = "Institutions collaboration")

## Country ---------------------------

bibres_countrylist <- bibres$Countries

# No of Publication

bibres_countrytable <- tibble(Rank = seq_along(bibres_countrylist),
                              Country = rownames(bibres_countrylist),
                              Np = as.integer(bibres_countrylist)) %>% 
  mutate(Country = fct_reorder(Country, Rank),
         percent = Np / sum(Np) * 100,
         percent = round(percent,1))

# Total Citation

bibressum_countrytable <- tibble(Rank = 1:10,
                                 bibres_summary$TCperCountries) %>% 
  rename("Country" = "Country     ") %>% 
  mutate(Country = str_trim(Country),
         Country = fct_reorder(Country, Rank),
         `Total Citations` = as.integer(`Total Citations`),
         `Average Article Citations` = as.double(`Average Article Citations`),
         percent = `Total Citations` / sum(`Total Citations`) * 100,
         percent = round(percent,1)) %>% 
  inner_join(x = ., y = select(bibres_countrytable, Country, Np), 
             by = "Country") %>% 
  relocate(percent, .after = `Total Citations`)

# Country Collaboration Network Map
 
bib_concolab <- metaTagExtraction(bibds_wos, Field = "AU_CO", sep = ";")

bib_concolab_NetMatrix <- biblioNetwork(bib_concolab, analysis = "collaboration",
                                        network = "countries", sep = ";")

bib_concolab_Plot <- networkPlot(bib_concolab_NetMatrix, 
                                 n = dim(bib_concolab_NetMatrix)[1], 
                                 type = "auto", Title = "Country Collaboration",
                                 size=10, size.cex=T, edgesize = 2, labelsize=1.1,
                                 edges.min = 2, remove.isolates = T, community.repulsion = 0,
                                 cluster = "none")
