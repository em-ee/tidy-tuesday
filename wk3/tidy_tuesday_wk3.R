# load packages

library(tidyverse)
library(reshape2)
library(ggrepel)

# load data

gm<-read.csv("./tidy-tuesday/wk3/global_mortality.csv")
#names(gm)<-gsub("\\.", "", x = names(gm))
colnames(gm)<-(c("country", "country_code", "year", "CVD", "Cancer", "Respiratory", 
                 "Diabetes", "Dementia", "Resp infections", "Neonatal", 
                 "Diarrheal", "Road accidents", "Liver", "TB", "Kidney", "Digestive",
                 "HIVAIDS", "Suicide", "Malaria", "Homicide", "Malnutrition", "Meningitis",
                 "Protein deficiency", "Drowning", "Maternal", "Parkinsons", "Alcohol", 
                 "Intestinal infections", "Drugs", "Hepatitis", "Fire", "HeatCold",
                 "Nature", "Conflict", "Terrorism"
                 ))

# explore the data

glimpse(gm)

# tidy the data
gmtidy<-gm%>%
          gather(disease, rate, CVD:Terrorism)

# plot to show terrorism deaths (where it's more than 1% plus USA for reference)

gmtidy%>%
  filter(disease == "Terrorism")%>%
  filter(rate>=1 | country_code=="USA")%>%
  ggplot(aes(x = year, y = rate, fill = country))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~country)+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Mortality rate")+
  ggtitle("Countries with deaths due to Terrorism > 1%",
          subtitle = "(and USA for reference)")
ggsave("terrorism.png", width = 20, height = 10, path = "~/tidy-tuesday/wk3")


# plot to show suicide deaths top 25 countries

gmtidy%>%
  filter(disease== "Suicide")%>%
  filter(rate>=1.79)%>%
  filter(country_code!="")%>%
  ggplot(aes(y = rate, x = country_code, col = country))+
  geom_point()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45))+
  labs(x = "Country", y = "Mortality rate")+
  ggtitle("Top 25% suicide rates by country")

ggsave("suicide_rates.png", width = 20, height = 10, path = "~/tidy-tuesday/wk3")

# plot to show deaths in the UK over time

gmtidy%>%
  filter(country_code=="GBR")%>%
  mutate(label = if_else((year == max(year) & rate>=4),
                        as.character(disease), 
                        NA_character_))%>%
  ggplot(aes(x = year, y = rate, col = disease,
             label = label))+
  geom_line()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Mortality rate")+
  ggtitle("Mortality rates change over time, UK")+
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE)

ggsave("uk_deaths.png", width = 20, height = 10, path = "~/tidy-tuesday/wk3")


