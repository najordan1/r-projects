library(tidyverse)
library(readr)
library(naniar)
library(nFactors)
library(psych)
library(stargazer)
library(aod)
library(likert)
library(ggpubr)


AB_WaveV_EN <- read_csv("AB-WaveV-EN.csv")

df <- AB_WaveV_EN %>%
  select(Q606_1, Q606_2, Q606_3, Q606_4, Q505A, Q303A, Q609A, Q516A,
         Q513, Q607_2, Q607_6, Q1003, Q1002, Q516_4, Q610_6A, Q101,
         Q512, country, Q516_1, Q516_2, Q516_3, Q609, Q604A_1)

na_numbers <- c(96,97,98,99)

#takes a min, hence the copy
df <- df %>% 
 replace_with_na_all(~.x %in% na_numbers)
# # 
# gdp <- read_csv("gdp.csv")
# 
# gdp <- gdp %>%
#   select(country, GDP)
# 
# df <- left_join(df, gdp, by="country")

df <- df %>% filter(country != 9 & country != 1)

df_copy <- df

df2 <- read.csv("ABII_English.csv")

#------- factor analysis ------

religion <- df %>%
  select(Q607_2,Q607_6,Q606_2,Q606_3,Q606_4,Q609A,Q505A)


ev <- eigen(cor(na.omit(religion)))
#ev <- eigen(cor(democracy, use="pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject=nrow(religion),var=ncol(religion),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation
fit <- factanal(na.omit(religion), 2, rotation="varimax")
print(fit, digits=2, cutoff=.35, sort=TRUE)

#Liberalism: Q505A,Q601_1,Q606_1,Q606_2,Q606_3,Q606_4
#Electoral Democracy: Q303A


#Interpretation: Q607_2, Q607_6

#Religiosity: Q609, Q609A

# 
# test <- fa.parallel(democracy)
# 
# fa <- fa(democracy, nfactors=2, rotate="varimax")
# colnames(fa$loadings) <- c("Liberal", "Democracy")
# fa.diagram(fa)

democracy <- df %>% select(Q512, Q516A, Q516_1, Q516_2, Q516_3, Q516_4)

ev <- eigen(cor(na.omit(democracy)))
#ev <- eigen(cor(democracy, use="pairwise.complete.obs")) # get eigenvalues
ap <- parallel(subject=nrow(democracy),var=ncol(democracy),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation
fit2 <- factanal(na.omit(democracy), 2, rotation="varimax")
print(fit2, digits=2, cutoff=.35, sort=TRUE)


#----data analysis-----


df <- df %>%
  mutate(Religiosity = Q609A,
         MinRights = 5 - Q607_2,
         RelGov = 5 - ((Q606_2 + Q606_3 + Q505A) / 3),
         Interpretation =  (Q606_4 + Q607_6) / 2,
         DemSupport = Q512,
         DemApproval = ((Q516A/3) + ((5 - Q516_4)/4)) / 2,
         DemStrength = 5-((Q516_1 + Q516_2 + Q516_3)/3),
         Elections = 4 - Q303A,
         Education = Q1003,
         Gender = Q1002,
         Economy = Q101,
         Governance = Q513,
         country = country,
         MinRights = case_when(MinRights >= 2.5 ~ 1,
                               MinRights < 2.5 ~ 0),
         DemSupport = case_when(DemSupport >= 5 ~ 1,
                                DemSupport < 5 ~ 0),
         DemApproval = case_when(DemApproval >= 0.5 ~ 1,
                                 DemApproval < 0.5 ~ 0),
         DemStrength = case_when(DemStrength >= 2.5 ~ 1,
                                 DemStrength < 2.5 ~ 0))

model <- function(data, dv){
  glm(paste(dv, " ~ Religiosity + RelGov + Interpretation + Economy +
        Gender + Elections + Governance + Education"), 
      family = "binomial", data)
}

r1 <- model(df, "DemSupport")
r2 <- model(df, "DemApproval")
r3 <- model(df, "DemStrength")
r4 <- model(df, "MinRights")

setwd("C:/Users/natha/OneDrive - Davidson College/2020-2021/Political Islam (POL 443)")

stargazer(r1, r2, r3, model.numbers = F, 
          title = "Democratic Views", 
          out = "Democracy.html",
          header = T)

stargazer(r4, model.numbers = F, 
          title = "Liberal Views", 
          out = "Liberal.html",
          header = T)

egypt <- df %>% filter(country==5)
lebanon <- df %>% filter(country==10)
libya <- df %>% filter(country==11)
jordan <- df %>% filter(country==8)
iraq <- df %>% filter(country==7)
palestine <- df %>% filter(country==15)
morocco <- df %>% filter(country==13)
sudan <- df %>% filter(country==19)
tunisia <- df %>% filter(country==21)
yemen <- df %>% filter(country==22)

r10 <- model(egypt, "DemApproval")
r11 <- model(lebanon, "DemApproval")
r12 <- model(libya, "DemApproval")
r13 <- model(jordan, "DemApproval")
r14 <- model(iraq, "DemApproval")
r15 <- model(palestine, "DemApproval")
r16 <- model(morocco, "DemApproval")
r17 <- model(tunisia, "DemApproval")
r18 <- model(sudan, "DemApproval")
r19 <- model(yemen, "DemApproval")

stargazer(r10, r11, r12, r13, r14, r15, r16, r17, r18, r19,
          title = "Democracy Approval By Country",
          column.labels = c("Egypt", "Lebanon", "Libya", "Jordan", "Iraq",
                     "Palestine", "Morocco", "Tunisia", "Sudan", "Yemen"),
          model.numbers = F,
          header=T, out = "demcountries.html")

r20 <- model(egypt, "MinRights")
r21 <- model(lebanon, "MinRights")
r22 <- model(libya, "MinRights")
r23 <- model(jordan, "MinRights")
r24 <- model(iraq, "MinRights")
r25 <- model(palestine, "MinRights")
r26 <- model(morocco, "MinRights")
r27 <- model(tunisia, "MinRights")
r28 <- model(sudan, "MinRights")
r29 <- model(yemen, "MinRights")

stargazer(r20, r21, r22, r23, r24, r25, r26, r27, r28, r29,
          title = "Minority Rights Approval By Country",
          column.labels = c("Egypt", "Lebanon", "Libya", "Jordan", "Iraq",
                            "Palestine", "Morocco", "Tunisia", "Sudan", "Yemen"),
          model.numbers = F,
          header=T, out = "libcountries.html")

stargazer(r2, r4, title="Regional Models", 
          header=T, model.numbers=F, out="testregion.html")


#data viz ----

mylevels <- c('Strongly Agree', 'Agree', 'NA', 'Disagree', 'Strongly Disagree')

items <- df %>%
  select(Q607_2, Q607_6, Q606_2, Q606_3, Q606_4, Q505A, Q516_4) %>%
  mutate(Q607_2 = case_when(Q607_2==1 ~ mylevels[1],
                            Q607_2==2 ~ mylevels[2],
                            Q607_2==3 ~ mylevels[4],
                            Q607_2==4 ~ mylevels[5],
                            T ~ mylevels[3]),
         Q607_6 = case_when(Q607_6==1 ~ mylevels[1],
                            Q607_6==2 ~ mylevels[2],
                            Q607_6==3 ~ mylevels[4],
                            Q607_6==4 ~ mylevels[5],
                            T ~ mylevels[3]),
         Q606_2 = case_when(Q606_2==1 ~ mylevels[1],
                            Q606_2==2 ~ mylevels[2],
                            Q606_2==3 ~ mylevels[4],
                            Q606_2==4 ~ mylevels[5],
                            T ~ mylevels[3]),
         Q606_3 = case_when(Q606_3==1 ~ mylevels[1],
                            Q606_3==2 ~ mylevels[2],
                            Q606_3==3 ~ mylevels[4],
                            Q606_3==4 ~ mylevels[5],
                            T ~ mylevels[3]),
         Q606_4 = case_when(Q606_4==1 ~ mylevels[1],
                            Q606_4==2 ~ mylevels[2],
                            Q606_4==3 ~ mylevels[4],
                            Q606_4==4 ~ mylevels[5],
                            T ~ mylevels[3]),
         Q505A = case_when(Q505A==1 ~ mylevels[1],
                            Q505A==2 ~ mylevels[2],
                            Q505A==3 ~ mylevels[4],
                            Q505A==4 ~ mylevels[5],
                           T ~ mylevels[3]),
         Q516_4 = case_when(Q516_4==1 ~ mylevels[1],
                           Q516_4==2 ~ mylevels[2],
                           Q516_4==3 ~ mylevels[4],
                           Q516_4==4 ~ mylevels[5],
                           T ~ mylevels[3]))

items <- data.frame('Q607_2' = factor(items$Q607_2, levels = mylevels),
                   'Q607_6' = factor(items$Q607_6, levels = mylevels),
                   'Q505A' = factor(items$Q505A, levels = mylevels),
                   'Q606_2' = factor(items$Q606_2, levels = mylevels),
                   'Q606_3' = factor(items$Q606_3, levels = mylevels),
                   'Q606_4' = factor(items$Q606_4, levels = mylevels),
                   'Q516_4' = factor(items$Q516_4, levels = mylevels))



# Build plot
p <- likert(items) 
likert.bar.plot(p) + theme_minimal() +
  labs(title="Survey Responses Distribution") +
  theme(legend.position = "bottom")

all_questions <- c("Q607_2", "Q607_6", "Q606_2", "Q606_3", "Q606_4", "Q505A", "Q609A", 
                   "Q516A", "Q516_1", "Q516_2", "Q516_3", "Q516_4", "Q512",
                   "Q101", "Q1002", "Q1003", "Q303A", "Q513")

stargazer(as.data.frame(df[all_questions]), type = "html", title = "Survey Questions", 
          digits = 3, median = TRUE, omit.summary.stat = c("p25", "p75"),
          out = "All.html")

model_questions <- c("Illiberal", "DemApproval", 
                     "Religiosity", "RelGov", "Interpretation",
                     "Elections", "Education", "Gender",
                     "Economy", "Governance")

df_out <- df %>% mutate(Illiberal = MinRights)

stargazer(as.data.frame(df_out[model_questions]), type = "html", title = "Model Questions", 
          digits = 3, median = TRUE, omit.summary.stat = c("p25", "p75"),
          out = "Model.html")


#likert pt 2 ----

one <- df_out %>%
  select(country, Illiberal, Q505A, Q606_2, Q606_3, Q606_4, Q607_6) %>%
  filter(!is.na(Illiberal)) %>%
  mutate(Illiberal = case_when(Illiberal == 0 ~ "Liberal",
                               Illiberal == 1 ~ "Illiberal"),
         Country = case_when(country==5 ~ "Egypt",
                             country==10 ~ "Lebanon",
                             country==11 ~ "Libya",
                             country==8 ~ "Jordan",
                             country==7 ~ "Iraq",
                             country==15 ~ "Palestine",
                             country==13 ~ "Morocco",
                             country==19 ~ "Sudan",
                             country==21 ~ "Tunisia",
                             country==22 ~ "Yemen"))

make_likert <- function(data, q, lib, levels, plotText){
  onea1 <- data %>%
    filter(.[[2]]==lib)%>%
    select(Country, sym(q)) %>%
    mutate(id=row_number(),
           Q = case_when(.[[2]]==1 ~ levels[1],
                         .[[2]]==2 ~ levels[2],
                         .[[2]]==3 ~ levels[4],
                         .[[2]]==4 ~ levels[5],
                         is.na(.[[2]]) ~ levels[3])) %>%
    pivot_wider(names_from = Country, values_from = Q)
  
  onea1 <- data.frame('Egypt' = factor(onea1$Egypt, levels = levels),
                     'Lebanon' = factor(onea1$Lebanon, levels = levels),
                     'Libya' = factor(onea1$Libya, levels = levels),
                     'Jordan' = factor(onea1$Jordan, levels = levels),
                     'Iraq' = factor(onea1$Iraq, levels = levels),
                     'Palestine' = factor(onea1$Palestine, levels = levels),
                     'Morocco' = factor(onea1$Morocco, levels = levels),
                     'Sudan' = factor(onea1$Sudan, levels = levels),
                     'Tunisia' = factor(onea1$Tunisia, levels = levels),
                     'Yemen' = factor(onea1$Yemen, levels = levels))

  p <- likert(onea1)
  # Build plot
  likert.bar.plot(p) + theme_minimal() +
   labs(title=plotText) + theme(legend.position="bottom")
  
}

onealevels <- c("Strongly Prefer Religious", "Prefer Religious", "NA",
                "Prefer NonReligious", "Strongly Prefer Nonreligious")

onea1p <- make_likert(one, "Q505A", "Illiberal", onealevels, "Q505A: Illiberal Respondents")
onea2p <- make_likert(one, "Q505A", "Liberal", onealevels, "Q505A: Liberal Respondents")
oneb1p <- make_likert(one, "Q606_2", "Illiberal", mylevels, "Q606_2: Illiberal Respondents") 
oneb2p <- make_likert(one, "Q606_2", "Liberal", mylevels, "Q606_2: Liberal Respondents")
onec1p <- make_likert(one, "Q606_3", "Illiberal", mylevels, "Q606_3: Illiberal Respondents")
onec2p <- make_likert(one, "Q606_3", "Liberal", mylevels, "Q606_3: Liberal Respondents")
oned1p <- make_likert(one, "Q606_4", "Illiberal", mylevels, "Q606_4: Illiberal Respondents")
oned2p <- make_likert(one, "Q606_4", "Liberal", mylevels, "Q606_4: Liberal Respondents")
onee1p <- make_likert(one, "Q607_6", "Illiberal", mylevels, "Q607_6: Illiberal Respondents")
onee2p <- make_likert(one, "Q607_6", "Liberal", mylevels, "Q607_6: Liberal Respondents")

onea_out <- ggarrange(oneb1p, oneb2p, onec1p, onec2p, nrow = 2, ncol=2, common.legend=T)
oneb_out <- ggarrange(onea1p, onea2p, nrow=1, common.legend=T)
onec_out <- ggarrange(oned1p, oned2p, onee1p, onee2p, nrow = 2, ncol=2, common.legend=T)


two <- df_out %>%
  select(country, DemApproval, Q607_6, Q606_4) %>%
  filter(!is.na(DemApproval)) %>%
  mutate(DemApproval = case_when(DemApproval == 0 ~ "Disapproval",
                               DemApproval == 1 ~ "Approval"),
         Country = case_when(country==5 ~ "Egypt",
                             country==10 ~ "Lebanon",
                             country==11 ~ "Libya",
                             country==8 ~ "Jordan",
                             country==7 ~ "Iraq",
                             country==15 ~ "Palestine",
                             country==13 ~ "Morocco",
                             country==19 ~ "Sudan",
                             country==21 ~ "Tunisia",
                             country==22 ~ "Yemen"))

twoa1p <- make_likert(two, "Q607_6", "Disapproval", mylevels, 
                      "Q607_6: Nondemocratic Respondents")
twoa2p <- make_likert(two, "Q607_6", "Approval", mylevels, 
                      "Q607_6: Democratic Respondents")
twob1p <- make_likert(two, "Q606_4", "Disapproval", mylevels, 
                      "Q606_4: Nondemocratic Respondents")
twob2p <- make_likert(two, "Q606_4", "Approval", mylevels, 
                      "Q606_4: Democratic Respondents")

two_out <- ggarrange(twoa1p, twoa2p, twob1p, twob2p, nrow = 2, ncol=2, common.legend=T)
