library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(stringr)

men <- read_excel("men.xlsx")
women <- read_excel("women.xlsx")

formatData <- function(df, name){
  df %>%
    filter(Year > 2018) %>%
    pivot_longer(c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                 names_to="key", values_to=name) %>%
    unite("Month", c("key", "Year"), sep = "-")
}

men <- formatData(men, "male") %>%
  filter(!is.na(male))

women <- formatData(women, "female") %>%
  filter(!is.na(female))

sex <- left_join(men, women) %>%
  mutate(date = "01") %>%
  unite("Month", c("date", "Month"), sep="-") %>%
  mutate(Month=dmy(Month)) %>%
  pivot_longer(c("male", "female")
               , names_to="Sex"
               , values_to="Unemployment Rate") %>%
  filter(Month >= "2019-07-01")

one <- ggplot(sex) + geom_line(aes(x=Month, y=`Unemployment Rate`,
                                  color=Sex)) + theme_minimal()
  

both_20_24 <- read_excel("both_20_24.xlsx")
total <- read_excel("total.xlsx")

both_20_24 <- formatData(both_20_24, "20-24 Years Old") %>%
  filter(!is.na(`20-24 Years Old`))

total <- formatData(total, "Overall") %>%
  filter(!is.na(Overall))

collegeGrads <- left_join(both_20_24, total)%>%
  mutate(date = "01") %>%
  unite("Month", c("date", "Month"), sep="-") %>%
  mutate(Month=dmy(Month)) %>%
  pivot_longer(c("20-24 Years Old", "Overall")
               , names_to="Category"
               , values_to="Unemployment Rate") %>%
  filter(Month >= "2019-07-01") %>%
  mutate(label=case_when())

two <- ggplot(collegeGrads) + 
  geom_line(aes(x=Month,y=`Unemployment Rate`,color=Category), size=1.5) +
  theme_minimal() + scale_color_manual(values=c("#1F78B4", "#A6CEE3")) +
  labs(title="U.S. Young Adult Unemployment") +
  scale_x_date(breaks=pretty_breaks(6))

ccd1 <- read_excel("ccd1.xlsx")

ccd1 <- ccd1 %>%
  pivot_longer(cols=c("AY20", "AY21"), names_to="Year") %>%
  mutate(Question=str_wrap(Question, width=10))

#df1 <- ccd1 %>% filter()

three <- ggplot(ccd1) +
  geom_bar(aes(x=Question, y=value, fill=Year),position="dodge",stat="identity",width=0.5) +
  theme_minimal() + scale_fill_manual(values=c("#1F78B4", "#A6CEE3")) +
  labs(x="", y="", title="Handshake Postings") + scale_y_continuous(n.breaks=10)
three


ccd2 <- read_excel("ccd2.xlsx")
ccd2 <- ccd2 %>%
  pivot_longer(cols=c("Fall", "Spring"), names_to="time")%>%
  mutate(value=100*value,
         time = case_when(time=="Fall" ~ "10-01",
                          T ~ "04-01")) %>%
  unite("Month", c("time", "Recruiting Year"), sep="-") %>%
  filter(!is.na(value))%>%
  mutate(Month=mdy(Month))

four <- ggplot(ccd2) + geom_line(aes(x=Month,y=value), color="#1F78B4") +
  theme_minimal() + labs(x="", y="% change in hiring", title="Job Outlook Hiring Projections")
four

ccd3 <- read_excel("ccd3.xlsx")

ccd3 <- ccd3 %>%
  pivot_longer(cols=c("AY20", "AY21"), names_to="Year") %>%
  mutate(Question=str_wrap(Question, width=10))

five <- ggplot(ccd3, aes(x=Question,y=value)) +
  geom_bar(aes(fill=Year),position="dodge",stat="identity",width=0.5) +
  theme_minimal() + scale_fill_manual(values=c("#1F78B4", "#A6CEE3")) +
  labs(x="", y="", title="CCD Senior Appointments")
five
