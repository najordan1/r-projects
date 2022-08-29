#### Overview ----

# This file contains code used for the 2022 APSA Conference presentation,
# authored by Nathan Jordan for a research project in collaboration with
# Dr. Katherine Bersch.

#### IMPORTS, PACKAGES, VARIABLES ----

# Set working directory to location of data!
# Note: data not included in this repository due to restrictions

# Load packages:
library(readxl)
library(dplyr)
library(tidyr)
library(devtools)
library(naniar)

# Define variables of interest, from Felix:
highEducation <- c(
  "Mestre+Rsc-III Lei 12772/12",
  "Pos-Grad O+Rsc-II L12772/12",
  "Pos-Graduacao(T)",
  "Aperfeicoamento Niv Sup(T)",
  "Doutorado(T)",
  "Doutorado",
  "Especializacao Niv Sup(T)",
  "Mestrado",
  "Mestrado(T)",
  "Pos-Doutorado(T)"
)

# Survey agency is the key on left side, agency data on right side
# this line of code represents maybe 20 hours of collaboration over 
# the years :)
agencyMapping <- c(
  `AGU` = "PR",
  `MDIC` = "MDIC",
  `MEC` = "MEC",
  `MJ` = "MJ",
  `ME` = "ME",
  `MT` = "MTB",
  `MS` = "MS",
  `MRE` = "MRE",
  `MTUR` = "MTUR",
  `MD` = "MD",
  `PR` = "PR",
  `MME` = "MME",
  `MPDG` = "MP",
  `MEC` = "MINC",
  `MTPA` = "MINFRA",
  `CIDADES` = "MDR",
  `MI` = "MDR",
  `MCTIC` = "MCTIC",
  `MAPA` = "MAPA",
  `MTF` = "CGU",
  `MMA` = "MMA",
  `MDS` = "MCID",
  `MF` = "ME",
  `MTF` = "CGU",
  `MPDG` = "MP",
  `MDH` = "MJ",
  `SG` = "PR",
  `CASA CIVIL` = "PR",
  `GSI` = "PR"
)

# It's nice to display the full question on visualizations as opposed to the
# question number
questionLabels <- c(
  QA1 = "What is your gender?",
  QA2 = "What is your ethnicity?",
  QA3 = "Year of birth?",
  QA4 = "What is your highest level of education?",
  QB0A1to3 = "In relation to your organization, assess your degree of agreement
  with the following statements:",
  QB1 = "My work unit is capable of recruiting people with the right skills",
  QB2 = "Promotions for political appointees in my work unit are based on merit",
  QB3 = "Nominations for politically appointed positions should be based on merit",
  QB4 = "How satisfied are you with the recognition you receive for doing a good job?",
  QB0B5to8 = "Evaluate the importance of the following items for the nomination
  to politically appointed positions",
  QB5 = "Technical competence",
  QB6 = "Affiliation to political parties",
  QB7 = "Career expertise",
  QB8 = "Network of personal relationships",
  QC0A1to4 = "Evaluate your degree of agreement with the following items:",
  QC1 = "I feel like the government takes maximum advantage of my talents",
  QC2 = "In my current position, I feel encouraged to come up with new and better ways of doing things",
  QC3 = "I can make my own decisions when doing my job",
  QC4 = "Approval of a project depends on my technical considerations",
  QC5 = "How satisfied are you with your involvement in decisions that affect your job?",
  QC6 = "How often do rules and regulations impede your ability to do your job?",
  QC7 = "Graduates from the country's best universities desire a top-level career in the public sector",
  QC8 = "Civil servants never benefit from their jobs except for their salary",
  QC9 = "Civil servants are politically neutral in performing their jobs",
  QC10 = "Civil servants should strictly obey their superiors",
  QC11 = "Civil servants are free to replace formal rules in favor of their judgement",
  QC0B12to13 = "Having political connections can be important in the public
  sector on some occasions. Evaluate the degree of importance of political
  connections to the following items:",
  QC12 = "Obtaining a salary increase",
  QC13 = "Reaching the public policy's goal",
  QD0 = "How much do you agree with the following statements:",
  QD1 = "The organizational culture of my agency makes it difficult for corruption practices to take place",
  QD2 = "Civil society organizations are able to participate in the policy decision-making processes of the agency where I work",
  QD3 = "My organization is held accountable for achieving results",
  QD4 = "My organization's performance is well-evaluated by society",
  QD5 = "The management of policies in my work unit includes coordination between different government levels",
  QD6 = "Society respects unelected government authorities",
  QE0 = "Are the following items obstacles to your work unit's performance:",
  QE1 = "Human Resources",
  QE2 = "Budget",
  QE3 = "Technological resources",
  QE4 = "Turnover of senior management",
  QE5 = "Planning, monitoring and evaluation processes",
  QE6 = "Legislation",
  QE7 = "Coordination with the Judiciary Branch and the Federal Prosecution Service",
  QE8 = "Coordination with the LEgislative Branch",
  QE9 = "Interfederal coordination with states and municipalities",
  QE10 = "Audits and control processes",
  QE11 = "Social participation",
  QE12 = "Access to key political decision makers",
  QE13 = "Political Party Interference",
  QF0 = "In the last 12 months, how often did you interact with:",
  QF1 = "Other organizations or agencies linked to the agency you currently work for",
  QF2 = "Other ministries or agencies",
  QF3 = "Congress / Congressional Representatives",
  QF4 = "Judiciary / Judiciary members",
  QF5 = "Institutions of accountability (Federal Court of Accounts, Office of the Comptroller General - CGU, Federal Prosecution Service)",
  QF6 = "Local Government",
  QF7 = "State Government",
  QF8 = "Private companies",
  QF9 = "International Organizations",
  QF10 = "Civil society organizations",
  QF11 = "Universities and research institutes",
  QG0 = "Do your organization's civil servants have the skills described below? Please rate your level of agreement with the following:",
  QG1 = "Knowledge of public policies in their policy field",
  QG2 = "Research skills",
  QG3 = "Policy analysis skills",
  QG4 = "Leadership",
  QG5 = "Conflict management skills",
  QG6 = "Interpersonal relationship building skills",
  QG7 = "Communication skills",
  QH1 = "Has your organization become more or less effective over the past five years?",
  QH2 = "The policies produced by my organization achieve the expected results",
  QH3 = "The resources available were sufficient to fulfill the tasks in my organization",
  QH4 = "My work unit is able to recruit people with the right skills",
  QH5 = "My organization is creative and innovative",
  QI1 = "How many years have you been in the public service?",
  QI2 = "What is your relationship with the public administration?",
  QI3 = "What is your job type (career)?",
  QI4 = "Which ministry does your organization belong to?",
  QI5 = "Do you work in the Federal District or another Federal Unit (UF)?",
  QI6 = "In what type of organization do you currently work?",
  QI7 = "In which ministry/agency do you currently work?",
  QI8 = "Do you have a politically appointed position?",
  QI9 = "(If yes in the previous answer): What is your politically appointed position?",
  QI10 = "Are you affiliated with any political party?",
  QJ1 = "Vignette: Promote civil servant with shorter career but more tech skills before longer tenure civil servant",
  QJ2 = "Civil servant with family connections promoted over civil servants with same technical skills",
  QJ3 = "Favoring ethnic-racial minorities for promotion",
  QJ4 = "Favoring women for promotion"
)

# Define latent concepts as well as groups of questions we find interesting:
concepts <- list(
  Autonomy = c("QC1", "QC2", "QC3", "QC4", "QC5"),
  `Propensity For Corruption` = c("QC9", "QC10", "QC11", "QD1"),
  # Capacity = c("QB1", "QB4", "QD3", "QH2", "QH3", "QH4", "QH5"), #revisit definition of capacity, ind opinions
  # Capacity = c("QH2", "QH3", "QH4", "QC12"),
  `Importance of Political Connections` = c("QC12", "QC13"),
  # `Political Autonomy` = c("QB2", "QB6", "QC9", "QC12", "QC13", "QE13"),
  # I know I know, better way to do this:
  A = c("QA1", "QA2", "QA4"), # A3 doesn't exist
  B = c("QB1", "QB2", "QB3", "QB4", "QB5", "QB6", "QB7", "QB8"),
  C = c("QC1", "QC2", "QC3", "QC4", "QC5", "QC6", "QC7", "QC8", "QC9", "QC10", "QC11", "QC12", "QC13"),
  D = c("QD1", "QD2", "QD3", "QD4", "QD5", "QD6"),
  E = c("QE1", "QE2", "QE3", "QE4", "QE5", "QE6", "QE7", "QE8", "QE9", "QE10", "QE11", "QE12", "QE13"),
  `F` = c("QF1", "QF2", "QF3", "QF4", "QF5", "QF6", "QF7", "QF8", "QF9", "QF10", "QF11"), 
  G = c("QG1", "QG2", "QG3", "QG4", "QG5", "QG6", "QG7"),
  H = c("QH1", "QH2", "QH3", "QH4", "QH5"),
  I = c("QI1", "QI2", "QI3", "QI4", "QI5", "QI6", "QI7", "QI8", "QI9", "QI10"),
  J = c("QJ1", "QJ2", "QJ3", "QJ4"),
  Quality = c("QH2", "QH1", "QH5", "QD4"),
  Capacity = c("QH3", "QH4", "QE4"),#, "QE2", "QE3", "QE5"),
  `Political Autonomy` = c("QC9", "QE13", "QB2", "QD1", "QC4")
)

# Import and clean data:
adminData <- read_excel("2018 Administrative Data.xlsx", sheet = "microdados")
surveyData <- read.csv("Brazil Data.csv") |> 
  replace_with_na_all(~.x==999)

# Source measurement invariance functions from Nathan's github
# Brings in the following functions:
# - create_likert: returns likert plot of a question's responses, by group
# - create_graphs_by_concept: creates a patchwork of likert graphs relating to one concept
# - create_lavaan_model: creates a multi-group cfa model for a concept
# - invariance_tester: runs the invariances tests on a cfa model
# - latent_variable_explorer: tests for invariance, and if passes, creates visualization of weighted
#   sum score values for that concept across the various groups
#
# To view the code in browser, change URL to github.com/najordan1....
source_url("https://raw.githubusercontent.com/najordan1/r-projects/main/Measurement%20Invariance/main.R")

#### FORMATTING ADMINISTRATIVE DATA ----

# Create variables of interest in admin data
adminData <- adminData |>
  # filter(!(orgao_superior == "MEC" & grepl('PROFESSOR', cargo))) |> 
  filter(
    # Rules sent by Felix to remove university professors:
    !grepl("^UF", orgao),
    !grepl("^IF", orgao),
    !grepl("^UNI", orgao),
    !grepl("^C.PEDRO", orgao),
    !grepl("^CEFET", orgao),
    !grepl("^EPL", orgao),
    !grepl("^EAF", orgao),
    !grepl("^EX-TER", orgao),
    !grepl("^CODEBAR", orgao),
    !grepl("^ETF/MT", orgao),
    !grepl("^UNB", orgao),
    !grepl("^FUFPEL", orgao),
    !grepl("^FUFOP", orgao),
    !grepl("^FURG", orgao),
    !grepl("^FUFS", orgao),
    !grepl("^FUFSCAR", orgao),
    !grepl("^UTFPR", orgao),
    !grepl("^C.EX", orgao),
    !grepl("F OSORIO", orgao),
    !grepl("^C.AER", orgao),
    !grepl("^MARE", orgao),
    !grepl("^CFETPE", orgao),
    !grepl("^FUFT", orgao)
  ) |> 
  transmute(
    ano = ano,
    uid_servidor = uid_servidor,
    `Civil Servant` = case_when(
      situacao_funcional != "EST-04" ~ 1,
      T ~ 0
    ),
    `Highly Educated` = case_when(
      escolaridade %in% highEducation ~ 1,
      T ~ 0
    ),
    Doctorate = case_when(
      escolaridade %in% c(
        "Doutorado(T)",
        "Doutorado",
        "Pos-Doutorado(T)"
      ) ~ 1,
      T ~ 0
    ),
    `Years in Public Service` = tempo_serv_pub,
    `Political Affiliation` = case_when(
      !is.na(sigla_do_partido) ~ sigla_do_partido,
      T ~ 'No Pol Affiliation'
    ),
    orgao_superior = case_when(
      # Move the M.ESPORTE individuals to ME
      orgao_superior == "M.ESPORTE" ~ "ME",
      T ~ orgao_superior
    ),
    `Political Technocrat` = case_when(
      `Civil Servant` == 0 &
        `Years in Public Service` <= 5 &
        `Political Affiliation` != 'No Pol Affiliation' ~ 1,
      T ~ 0
    ),
    `Bureaucratic Technocrat` = case_when(
      `Civil Servant` == 1 &
        `Highly Educated` == 1 &
        `Years in Public Service` > 5 &
        `Political Affiliation` == 'No Pol Affiliation' ~ 1,
      T ~ 0
    )
  )

# Concerning ids that have multiple entries per year and agency
dataDuplicates <- adminData |> group_by(uid_servidor, ano) |> filter(n() > 1)

# Looking at exit rates:
adminExitData <- adminData |>
  select(ano, uid_servidor, `Political Technocrat`, `Bureaucratic Technocrat`) |> 
  group_by(uid_servidor) |> 
  summarise(
    `Political Technocrat` = any(`Political Technocrat` == 1),
    `Bureaucratic Technocrat` = any(`Bureaucratic Technocrat` == 1),
    `2018` = any(ano == 2018),
    `2019` = any(ano == 2019),
    `2020` = any(ano == 2020)
  )

# Example query to generate numbers:
# adminExitData |> filter(`Political Technocrat` & `2019` & !`2020`)

# Exit rates in particular groups of agencies:
adminExitData2 <- adminData |> 
  select(ano, uid_servidor, orgao_superior) |> 
  pivot_wider(
    id_cols=c("uid_servidor"),
    names_from = ano,
    values_from = orgao_superior,
    values_fn = list
  )

# Example query:
# adminExitData2 |> 
#   rowwise() |> #forces the list/vector methods to do one row, not entire dataset
#   filter(
#     !is.null(`2019`) &
#     !is.null(`2020`) &
#     length(intersect(`2019`, c("MEC", "MMA", "MCTIC"))) > 0 &
#     length(intersect(`2020`, c("MEC", "MMA", "MCTIC"))) == 0 &
#     length(intersect(`2019`, `2020`)) == 0
#   )

# Make aggregated data for 2018:
adminAggregate <- adminData |> 
  filter(ano == 2018) |> 
  select(-ano, -uid_servidor) |> 
  mutate(id = row_number(), dummy_value = 1) |> 
  pivot_wider(
    names_from = `Political Affiliation`,
    values_from = dummy_value,
    values_fill = list(dummy_value = 0)
  ) |>
  select(-`Years in Public Service`, -id) |> 
  mutate(total = 1) |> 
  group_by(orgao_superior) |>
  summarise_all(sum) |> 
  mutate(
    bt_pct = 100 * (`Bureaucratic Technocrat` / total),
    pt_pct = 100 * (`Political Technocrat` / total),
    pa_pct = 100 * (1 - (`No Pol Affiliation` / total))
  ) |> 
  select(orgao_superior, total, bt_pct, pt_pct, pa_pct, everything())

#### SURVEY DATA ----

# Map agencies in survey data to align with admin data
surveyData <- surveyData |> 
  separate(
    # Make orgao_superior
    # min_orgao_limpo is formatted XXX-YYY and we want just XXX
    # the warnings about NAs here are ok
    min_orgao_limpo,
    c("orgao_superior", "trash"),
    sep="-", remove=F
  ) |> 
  select(-trash) |> 
  filter(
    # Cut survey respondents without an agency mapping
    orgao_superior != 'Eu não sei/não quero responder'
    #, orgao_superior != 'MEC'
  ) |> 
  mutate(
    # map the keys here to the agency keys, trimming whitespace when needed
    orgao_superior = agencyMapping[trimws(orgao_superior)],
    group = factor(
      case_when(
        orgao_superior %in% c("MTB", "MCID") ~ "PT",
        orgao_superior %in% c("MS", "MMA") ~ "BT",
        T ~ "Rest"
      ), levels = c("PT", "BT", "Rest")
    )
  )

# Can then view the Likert graphs like this:
create_graphs_by_concept(surveyData, "Political Autonomy", concepts[["Political Autonomy"]], "group", questionLabels)

png("Political Autonomy.png", units="in", width=14, height=5, res=300)
# code
dev.off()

polAutonomyModel <- create_lavaan_model(surveyData, 'Political Autonomy', concepts[["Political Autonomy"]], 'individual')
capModel <- create_lavaan_model(surveyData, 'Capacity', concepts[["Capacity"]], 'individual')
qualModel <- create_lavaan_model(surveyData, 'Quality', concepts[["Quality"]], 'individual')

fitMeasures(capModel, fit_indices)

qualLoadings <- parameterestimates(qualModel)[['est']][1:4]
capLoadings <- parameterestimates(capModel)[['est']][1:3]
polAutonomyLoadings <- parameterestimates(polAutonomyModel)[['est']][1:5]


surveyData <- surveyData |> 
  mutate(
    Quality = ((QH2*qualLoadings[[1]]) + (QH1*qualLoadings[[2]]) + (QH5*qualLoadings[[3]]) + (QD4*qualLoadings[[4]])) / 4,
    Capacity = ((QH3*capLoadings[[1]]) + (QH4*capLoadings[[2]]) + (QE4*capLoadings[[3]])) / 3,
    `Political Autonomy` = ((QC9*polAutonomyLoadings[[1]]) + (QE13*polAutonomyLoadings[[2]]) + (QB2*polAutonomyLoadings[[3]]) + (QD1*polAutonomyLoadings[[4]]) + (QC4*polAutonomyLoadings[[5]])) / 5,
    # Create binary variables on high vs low:
    Quality = case_when(
      Quality > (3 * sum(qualLoadings)) / 4 ~ 1,
      T ~ 0
    ),
    Capacity = case_when(
      Capacity > sum(capLoadings) ~ 1,
      T ~ 0
    ),
    `Political Autonomy` = case_when(
      `Political Autonomy` > (3 * sum(polAutonomyLoadings)) / 5 ~ 1,
      T ~ 0
    )
  )

adminJoin <- adminAggregate |> select(orgao_superior, pt_pct, bt_pct, pa_pct, total, `Civil Servant`, `Highly Educated`)

surveyData <- surveyData |> left_join(adminJoin)

surveyData <- surveyData |> 
  mutate(
    appointee = case_when(QI8 == 1 ~ 1, T ~ 0),
    highlyEducated = case_when(QA4 >= 3 ~ 1, T ~ 0)
  )

fit_indices <- c("chisq", "df", "pvalue", "cfi", "rmsea")

# r1 <- lm(Quality ~ bt_pct + pa_pct + `Highly Educated` + `Civil Servant`, surveyData)
# summary(r1)

r1 <- glm(`Political Autonomy` ~ bt_pct + pt_pct + pa_pct + appointee + highlyEducated, family = "binomial", surveyData)
r2 <- glm(Quality ~ bt_pct + pt_pct + pa_pct + appointee + highlyEducated, family = "binomial", surveyData)
r3 <- glm(Capacity ~ bt_pct + pt_pct + pa_pct + appointee + highlyEducated, family = "binomial", surveyData)

summary(r1)
summary(r2)
summary(r3)

