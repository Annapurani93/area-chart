library(tidytuesdayR)
library(tidyverse)
library(reshape2)
tuesdata <- tidytuesdayR::tt_load('2021-10-12')
tuesdata
tuesdata$`capture-fisheries-vs-aquaculture`->production
production
glimpse(production)
production%>%
  filter(Code=="OWID_WRL")%>%
  select(-c(Entity,Code))%>%
  distinct(Year,.keep_all = TRUE)->global
data.frame(global)->global
colnames(global)
global%>%rowwise()%>%
  mutate(Total=sum(Aquaculture.production..metric.tons.,Capture.fisheries.production..metric.tons.))->g1

data.frame(g1)->g1
colnames(g1)<-c("Year","Aqua","Capture","Total")
g1%>%
  mutate(Aquap=round((Aqua/Total)*100))%>%
  mutate(Cap=round((Capture/Total)*100))->g12
g12
melt(g12,id.vars = "Year",measure.vars = c("Aquap","Cap"),
     value.name = "Value")->m1

ggplot(m1,aes(x=Year,y=Value,fill=variable))+
  geom_area(size=0.5,colour="white",linetype = 1)+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(breaks=c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015))+
  scale_fill_manual(values = c("#0392cf", "#7bc043"),labels = c("Aquaculture (in %)", "Capture fisheries (in %)"))+
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_text(colour = "white"),
        legend.text = element_text(colour = "white", face="bold"),
        legend.background = element_rect("black"),
        legend.title = element_blank(),
        legend.position = "top")+
  theme(axis.text.y = element_text(margin = margin(r =-20)))+
  labs(title="SEAFOOD PRODUCTION: AQUACULTURE VERSUS WILD FISH CATCH",
       subtitle = "Aquaculture is the farming of aquatic organisms including fish, molluscs, crustaceans and aquatic plants. On the other hand, capture
fishery production is the volume of wild fish catches landed for all commercial, industrial, recreational and subsistence purposes",
       caption="Data from Our World in Data|Design and analysis: @annapurani93" )+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))->p

ggsave("stackarea.png",p,width =12,height=8)

