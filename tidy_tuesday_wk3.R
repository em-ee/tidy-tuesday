library(tidyverse)

gm<-read.csv("./tidy-tuesday/global_mortality.csv")
names(gm)<-gsub("\\.", "", x = names(gm))
# tidy the data
gmtidy<-gm%>%
          gather(disease, rate, Cardiovasculardiseases:Terrorism)
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
ggsave("terrorism.png", 
       path = "~/tidy-tuesday/")


# plot to show suicide deaths in each country

gmtidy%>%
  filter(disease== "Suicide")%>%
  filter(rate>=1.79)%>%
  ggplot(aes(y = rate, x = country_code, col = country))+
  geom_point()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45))

