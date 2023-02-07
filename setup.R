
# setup -------------------------------------------------------------------
library(ggeffects)
library(ggplot2)
library(tidyverse)
library(shiny)
library(arrow)
library(xlsx)

library(ggdist) # for shadeable density slabs
library(gghalves) # for half-half geoms
library(ggpp) # for position_dodge2nudge
library(colorspace) # for lightening color palettes
library(extrafont) # for Lato, RISE font
library(stringr)
library(EnvStats)
library(bslib)
library(formattable)
library(thematic)
library(catR)
library(Hmisc)

library(geomtextpath)
library(readxl)
library(rlang)
library(visNetwork)
library(networkD3)
library(RColorBrewer)
library(vctrs)
library(ggrepel)
library(see)
library(gt)
library(gtExtras)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

### text sizes
ax.size <- 14
title.size <- 16
legend.size <- 14

### set up color palette based on RISE guidelines
RISEprimGreen <- "#009ca6"
RISEprimRed <- "#e83c63"
RISEprimYellow <- "#ffe500"
RISEprimGreenMid <- "#8dc8c7"
RISEprimRedMid <- "#f5a9ab"
RISEprimYellowMid <- "#ffee8d"
RISEprimGreenLight <- "#ebf5f0"
RISEprimRedLight <- "#fde8df"
RISEprimYellowLight <- "#fff7dd"
RISEcompPurple <- "#482d55"
RISEcompGreenDark <- "#0e4e65"
RISEgrey1 <- "#f0f0f0"
RISEgrey2 <- "#c8c8c8"
RISEgrey3 <- "#828282"
RISEgrey4 <- "#555555"

### colors specifically for DID application
DIDcolorsOriginal <- c('#D55E00','#F0E442','#009E73','#999999')
DIDcolors <- lighten(DIDcolorsOriginal, amount = 0.1, space = "HLS")
DIDred <- "#D55E00"
DIDorange <- "#E69F00"
DIDgreen <- "#009E73"
DIDyellow <- "#F0E442"

RISEpalette1 <- colorRampPalette(colors = c("#009ca6", "#e83c63", "#ffe500"))(6)
#scales::show_col(RISEpalette1)

RISEpalette2 <- colorRampPalette(colors = c("#009ca6", "#482d55", "#e83c63", "#ffe500"))(8)
#scales::show_col(RISEpalette2)

# distance between panels in faceted ggplots
pandist <- 1


# Read info on insatser/interventions -------------------------------------

insatserMain <- read_excel("data/DIDinsatser.xls", sheet = 1)
insatserIntro <- read_excel("data/DIDinsatser.xls", sheet = 2)
insatserFotnot <- read_excel("data/DIDinsatser.xls", sheet = 3)

# Read item parameters ----------------------------------------------------

allItemInfo <- read_excel("data/SthlmsEnk_allItemInfo_2022-12-06.xls")

# subset psychosomatic item thresholds for persona-plot (calculation of person score/theta)
psf.itemps <- allItemInfo %>% 
  filter(Index == "PsykSomBesv") %>% 
  select(starts_with("Thresh")) %>% 
  as.matrix

# Import data -------------------------------------------------------------

## For sankey diagrams
lst.data <- read_excel("data/RISE LST RS-faktorer tabeller OSF.xlsx")
rskontext <- c("Individ","Familj","Kamrater och fritid","Skola","Samhälle")


## Stockholmsenkäten --------------------------------------

# protective factors are reversed scored to make high score = high protection

df <- arrow::read_parquet("data/2023-01-23_ScoredRev.parquet")
df <- df %>%
  rename(Kommun = DIDkommun)
# temporary renaming for old recode code to work
# df <- df %>% 
#   rename(Community = Närsamhälle,
#          Parenting = Föräldraskap,
#          PsykSomBesv = 'Psykiska/ psykosomatiska besvär',
#          SkolaNegativ = 'Vantrivsel i skolan',
#          Wellbeing = Välbefinnande,
#          SkolaPositiv = 'Positiv skolanknytning'
#   )

# define demographic variables of interest (could maybe be removed)
# removeDemographics <- c("Skolenhetskod","Skolnamn","F3_Omkodad","SkolID_gammal")
# demogr.vars <- read.csv("data/SthlmsEnk_demographicVariables.csv") %>% 
#   filter(!demogr.vars %in% removeDemographics)
# demogr.vars <- demogr.vars$demogr.vars
# 
# # rename/recode minor remaining issues
# df <- df %>% 
#   rename(Kommun = DIDkommun) %>% 
#   mutate(F5 = dplyr::recode(F5, "Mindre än 5  år" = "Mindre än 5 år"), # fix odd answers
#          F7 = dplyr::recode(F7, "6" = "<NA>", # fix odd answers
#                             "Boende växlar" = "<NA>"), # remove extremly rare response (n=6 for all years)
#          f6a = dplyr::recode(f6a, "Folkskola eller grundskola (max 9 år i skolan)" = "Folkskola/grundskola"),
#          f6b = dplyr::recode(f6b, "Folkskola eller grundskola (max 9 år i skolan)" = "Folkskola/grundskola"),
#                   )
# 
# # recode NA/missing to be consistent with R
# for (i in names(df)){
#   df[[i]] <- recode(df[[i]],"'<NA>'=NA")
# }
# 
# # rename demographic variables for use in selectInput() later
# df <- df %>% 
#   rename(`Hur länge har du bott i Sverige?` = F5,
#          `Vilken högsta utbildning har din mamma?` = f6a,
#          `Vilken högsta utbildning har din pappa?` = f6b,
#          `Vad bor du i för typ av bostad?` = F7)

demogr.choices <- c("Hur länge har du bott i Sverige?",
                    "Vilken högsta utbildning har din mamma?",
                    "Vilken högsta utbildning har din pappa?",
                    "Vad bor du i för typ av bostad?")

# final set of items based on psychometric analyses
itemlabels.final <- read_excel("data/SthlmsEnk_allItemInfo_2022-12-06.xls") %>% 
  select(itemnr,item,Index)

# list of all items included in analyses (even those discarded)
allitems <- read.csv("data/SthlmsEnk_allitems.csv")

# list of item responses for Psykiska/ psykosomatiska besvär, for use in the "persona" visualization
itemresponses <- read.xlsx("data/SthlmsEnk_04psfRespCats.xls", sheetName = "04psf")

# create vector with all index names
sthlm.index <- itemlabels.final %>% 
  distinct(Index) %>% 
  pull()

## Skolinspektionen --------------------------------------------------------

# read data from processed file with Rasch based scores
df.si <- arrow::read_parquet("data/SkolinspAk5Scored_2022-12-20.parquet")
# this data is also based on higher score = higher risk

# some functions are based on the SthlmsEnkät labeling of year as "ar"
# we add a duplicate year variable, class double (which ggplot sometimes prefers)
df.si <- df.si %>% 
  mutate(ar = as.numeric(as.character(År)),
         ar = vec_cast(ar, double()))

# read item info
si.items <- read_csv("data/SkolinspFinalItems_2022-12-20.csv")
# note that all SI items have merged the top 3 response categories (top = highest risk)

# Cutoff values SthlmsEnk -------------------------------------------------------------

# percentiles based on 2006-2020 data for all of Stockholm Stad (~ 110k responses)
# each year's 75th and 90th percentile value was used to calculate an average (not weighted)
# see script "file 04 Distributions and risk limits.R" in https://github.com/pgmj/sthlmsenk/tree/main/OtherScripts
#rslimits <- read_csv("data/SthlmsEnk_rslimitsNoRev2022-12-06.csv")
rslimits <- read_csv("data/2023-01-17_rslimitsRisk.csv")

# read cutoffs for protective factors
#rslimits.prot <- read_csv("data/2022-12-16_protective.csv")
rslimits.prot <- read_csv("data/2023-01-17_rslimitsProt.csv")

rslimits <- cbind(rslimits,rslimits.prot)
rslimits <- rslimits %>% 
  relocate(SkolaPositiv, .after = SkolaNegativ)

# for Skolinspektionen ÅK5
rslimits.si <- read_csv("data/2022-12-20_SkolinspLimits.csv")

# legacy Skolinspektionen ÅK5 (remove later)
#rslimits.si.old <- read_csv("data/SkolinspRSlimitsNoRev.csv")
rslimits$`Positiv skolanknytning åk 5` <- rslimits.si$`Positiv skolanknytning åk 5`



# define a vector with all municipalities included in the dataset, to be used in input selection lists
kommuner <- df %>%
  distinct(Kommun) %>% 
  pull()

# vector of years to be included in year selection inputs
årtal <- c(2006,2008,2010,2012,2014,2016,2018,2020,2022)

rsfaktorer <- c('Utagerande','Närsamhälle','Föräldraskap','Psykiska/ psykosomatiska besvär','Vantrivsel i skolan','Positiv skolanknytning','Välbefinnande')

rfactors <- c('Utagerande','Närsamhälle','Föräldraskap','Psykiska/ psykosomatiska besvär','Vantrivsel i skolan')
pfactors <- c('Välbefinnande','Positiv skolanknytning')


#---- sums.index -> key metrics for each year and municipality----

RSsmf <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>% 
    group_by(ar,Kommun) %>% 
    summarise(Medel = mean({{i}}, na.rm = T),
              StDev = sd({{i}},  na.rm = T),
              n = n(),
              StErr = StDev/sqrt(n),
              sd.lo = Medel-StDev,
              sd.hi = Medel+StDev,
              n.90 = length(which({{i}} > rslimits[2,j]))/n*100,
              n.70 = length(which({{i}} > rslimits[1,j]))/n*100) %>% 
    rename(År = ar) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    as.data.frame()
}  

sums.Utagerande <- RSsmf(df, Utagerande, 1) %>% 
  add_column(Faktor = 'Utagerande')
sums.SkolaNegativ <- RSsmf(df, SkolaNegativ, 2) %>% 
  add_column(Faktor = 'SkolaNegativ')
sums.SkolaPositiv <- RSsmf(df, SkolaPositiv, 3) %>% 
  add_column(Faktor = 'SkolaPositiv')
sums.PsykSomBesv <- RSsmf(df, PsykSomBesv, 4) %>% 
  add_column(Faktor = 'PsykSomBesv')
sums.Parenting <- RSsmf(df, Parenting, 5) %>% 
  add_column(Faktor = 'Parenting')
sums.Community <- RSsmf(df, Community, 6) %>% 
  add_column(Faktor = 'Community')
sums.Wellbeing <- RSsmf(df, Wellbeing, 7) %>%
 add_column(Faktor = 'Wellbeing')

sums.index <- rbind(sums.Utagerande,
                    sums.SkolaPositiv,
                    sums.SkolaNegativ,
                    sums.PsykSomBesv,
                    sums.Parenting,
                    sums.Community,
                    sums.Wellbeing)

# same but for Skolinspektionens data
sums.Skolinsp <- RSsmf(df.si,Indexvärde, 8) %>%
  add_column(Faktor = 'SkolaPositivSI')


## get key values divided by gender----

RSsmfGender <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>% 
    group_by(ar,Kommun,Kön) %>% 
    summarise(Medel = mean({{i}}, na.rm = T),
              StDev = sd({{i}},  na.rm = T),
              n = n(),
              StErr = StDev/sqrt(n),
              sd.lo = Medel-StDev,
              sd.hi = Medel+StDev,
              n.90 = length(which({{i}} > rslimits[2,j]))/n*100,
              n.70 = length(which({{i}} > rslimits[1,j]))/n*100) %>% 
    rename(År = ar) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    as.data.frame()
}  

sums.Utagerande <- RSsmfGender(df, Utagerande, 1) %>% 
  add_column(Faktor = 'Utagerande') %>% 
  filter(!Kön == '<NA>')
sums.SkolaPositiv <- RSsmfGender(df, SkolaPositiv, 2) %>% 
  add_column(Faktor = 'SkolaPositiv') %>% 
  filter(!Kön == '<NA>')
sums.SkolaNegativ <- RSsmfGender(df, SkolaNegativ, 3) %>% 
  add_column(Faktor = 'SkolaNegativ') %>% 
  filter(!Kön == '<NA>')
sums.PsykSomBesv <- RSsmfGender(df, PsykSomBesv, 4) %>% 
  add_column(Faktor = 'PsykSomBesv') %>% 
  filter(!Kön == '<NA>')
sums.Parenting <- RSsmfGender(df, Parenting, 5) %>% 
  add_column(Faktor = 'Parenting') %>% 
  filter(!Kön == '<NA>')
sums.Community <- RSsmfGender(df, Community, 6) %>% 
  add_column(Faktor = 'Community') %>% 
  filter(!Kön == '<NA>')
sums.Wellbeing <- RSsmfGender(df, Wellbeing, 7) %>%
  add_column(Faktor = 'Wellbeing') %>%
  filter(!Kön == '<NA>')

sums.indexG <- rbind(sums.Utagerande,
                     sums.SkolaPositiv,
                     sums.SkolaNegativ,
                     sums.PsykSomBesv,
                     sums.Parenting,
                     sums.Community,
                     sums.Wellbeing)

# same but for Skolinspektionens data
sums.SkolinspG <- RSsmfGender(df.si,Indexvärde, 8)%>%
  add_column(Faktor = 'SkolaPositivSI')


## merge sums.index files----

sums.index <- sums.index %>%
  add_column(Kön = "Flickor och pojkar")

sums.indexG <- sums.indexG %>%
  relocate(Kön, .after = "Faktor")

sums.index <- rbind(sums.index, sums.indexG)

# same but for Skolinspektionens data
sums.si <- sums.Skolinsp %>%
  add_column(Kön = "Flickor och pojkar")
sums.SkolinspG <- sums.SkolinspG %>%
  relocate(Kön, .after = "Faktor")
sums.si <- rbind(sums.si, sums.SkolinspG)
sums.si <- sums.si %>% 
  mutate(ar = as.numeric(as.character(År)),
         ar = vec_cast(ar, double()))

# Create risk-level variable -------------------------------------------------

RSrisklevel <- function(df, i) { # input df, index, and index number
  df |>
    mutate(
      !!paste0("Risk",i) := case_when(
        .data[[i]] < rslimits |> select(i) |> slice(1) |> pull() ~ "Låg risk",
        .data[[i]] >= rslimits |> select(i) |> slice(1) |> pull()
        & .data[[i]] < rslimits |> select(i) |> slice(2) |> pull() ~ "Medelhög risk",
        .data[[i]] >= rslimits |> select(i) |> slice(2) |> pull() ~ "Hög risk")
      ) |>
    select(!!paste0("Risk",i))
}

# Rename RS-factors -------------------------------------------------------

sums.index$Faktor <- car::recode(sums.index$Faktor,"'Community'='Närsamhälle';'Parenting'='Föräldraskap';'PsykSomBesv'='Psykiska/ psykosomatiska besvär';'SkolaNegativ'='Vantrivsel i skolan';'Wellbeing'='Välbefinnande';'SkolaPositiv'='Positiv skolanknytning'")

df <- df %>% 
  rename(Närsamhälle = Community,
         Föräldraskap = Parenting,
         'Psykiska/ psykosomatiska besvär' = PsykSomBesv,
         'Vantrivsel i skolan' = SkolaNegativ,
         Välbefinnande = Wellbeing,
         'Positiv skolanknytning' = SkolaPositiv
  )

rslimits <- rslimits %>% 
  rename(Närsamhälle = Community,
         Föräldraskap = Parenting,
         'Psykiska/ psykosomatiska besvär' = PsykSomBesv,
         'Vantrivsel i skolan' = SkolaNegativ,
         Välbefinnande = Wellbeing,
         'Positiv skolanknytning' = SkolaPositiv
  )

rslimits.prot <- rslimits.prot %>% 
  rename(Välbefinnande = Wellbeing,
         'Positiv skolanknytning' = SkolaPositiv
  )

# Create long format risklevel df ------------------------
#for coord_polar plot. Might be able to replace others
riskCalc <- function(df,index){
  df %>% 
    mutate(riskLevel = case_when(
      .data[[index]] < rslimits |> select(index) |> slice(1) |> pull() ~ "Låg risk",
      .data[[index]] >= rslimits |> select(index) |> slice(1) |> pull() & 
        .data[[index]] < rslimits |> select(index) |> slice(2) |> pull() ~ "Medelhög risk",
      .data[[index]] >= rslimits |> select(index) |> slice(2) |> pull() ~ "Hög risk")
    ) %>% 
    group_by(Kommun,ar)  %>%  
    count(riskLevel) %>% 
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%  
    mutate(riskLevel = factor(riskLevel, levels = c('Hög risk','Medelhög risk','Låg risk','NA'))) %>% 
    ungroup() %>% 
    add_column(Index = as.character(index)) %>% 
    mutate(År = as.factor(ar)) %>%
    select(!all_of(c("n","ar")))
}

df.risk <- data.frame(matrix(ncol = 4, nrow = 0))
for (i in rsfaktorer){
  df.r1<-as.data.frame(riskCalc(df,i))
  df.risk <- rbind(df.risk,df.r1)
}

riskCalcGender <- function(df,index){
  df %>% 
    mutate(riskLevel = case_when(
      .data[[index]] < rslimits |> select(index) |> slice(1) |> pull() ~ "Låg risk",
      .data[[index]] >= rslimits |> select(index) |> slice(1) |> pull() & 
        .data[[index]] < rslimits |> select(index) |> slice(2) |> pull() ~ "Medelhög risk",
      .data[[index]] >= rslimits |> select(index) |> slice(2) |> pull() ~ "Hög risk")
    ) %>% 
    group_by(Kommun,ar,Kön)  %>%  
    count(riskLevel) %>% 
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%  
    mutate(riskLevel = factor(riskLevel, levels = c('Hög risk','Medelhög risk','Låg risk','NA'))) %>% 
    ungroup() %>% 
    add_column(Index = as.character(index)) %>% 
    mutate(År = as.factor(ar)) %>%
    filter(!Kön == "<NA>") %>% 
    select(!all_of(c("n","ar")))
}

df.risk.gender <- data.frame(matrix(ncol = 4, nrow = 0))
for (i in rsfaktorer){
  df.r1<-as.data.frame(riskCalcGender(df,i))
  df.risk.gender <- rbind(df.risk.gender,df.r1)
}

# Mode values -------------------------------------------------------------

df <- df %>% 
  mutate(riskPSF = case_when(
    `Psykiska/ psykosomatiska besvär` < rslimits |> select(`Psykiska/ psykosomatiska besvär`) |> slice(1) |> pull() ~ "Låg risk",
    `Psykiska/ psykosomatiska besvär` >= rslimits |> select(`Psykiska/ psykosomatiska besvär`) |> slice(1) |> pull() & 
      `Psykiska/ psykosomatiska besvär` < rslimits |> select(`Psykiska/ psykosomatiska besvär`) |> slice(2) |> pull() ~ "Medelhög risk",
    `Psykiska/ psykosomatiska besvär` >= rslimits |> select(`Psykiska/ psykosomatiska besvär`) |> slice(2) |> pull() ~ "Hög risk")
  )

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

psf.items <- allItemInfo %>% 
  filter(Index == "PsykSomBesv") %>% 
  pull(itemnr)

#create data frame with 0 rows and 8 columns
psf.modes <- data.frame(matrix(ncol = 8, nrow = 3))
names(psf.modes) <- psf.items
lr <- c()
mr <- c()
hr <- c()
# get the mode value from each risk group for each item
for (i in psf.items){
  lr <- Mode(df %>% filter(riskPSF == "Låg risk") %>% select(i) %>% pull())
  mr <- Mode(df %>% filter(riskPSF == "Medelhög risk") %>% select(i) %>% pull())
  hr <- Mode(df %>% filter(riskPSF == "Hög risk") %>% select(i) %>% pull())
  psf.modes[[i]] <- rbind(lr,mr,hr)
}
# reverse code F99
psf.modes$F99 <- recode(psf.modes$F99,"3=0;2=1;1=2;0=3")
# transform to dataframe with numeric variables for later extraction as vectors
psf.modes<- psf.modes %>% 
  t() %>% 
  as.data.frame()


# KOLADA data -------------------------------------------------------------

KOLADA <- read_parquet("data/koladaData.parquet")

# which KPI's to remove?
removed.kpi <- c("N17473","N15613","N15643","N17620")
# set which are available in app
kpiChoices <- KOLADA %>% 
  filter(!kpi %in% removed.kpi) %>% 
  arrange(kpi) %>% 
  distinct(KPI) %>% 
  pull()

kolaMissing <- KOLADA %>% 
  group_by(Kommun,År,KPI) %>% 
  summarise(missing = is.na(Andel)) %>% 
  filter(missing == TRUE) %>% 
  distinct(KPI) %>% 
  ungroup()
# formattable(kolaMissing)
# 
# 
# KOLADA %>% 
#   distinct(KPI,kpi) %>% 
#   formattable()



  













