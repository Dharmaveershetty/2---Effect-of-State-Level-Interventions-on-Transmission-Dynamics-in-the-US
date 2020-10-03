library(ggplot2)
library(Cairo)



data<-read.csv("~/COVID Projects/interv_mobility_cases_04102020_v2.csv")
data$date<-as.Date(data$date)
data$state_code<-as.character(data$state_code)
data$DateEnactedStayAtHome<-as.numeric(data$DateEnactedStayAtHome)


png("facet_state.png", width=1300,height=900, res=100,type="cairo-png")
ggplot(data, aes(date, log(positive), group=state_code))+
  geom_line()+
  facet_wrap(~state_code)+theme_minimal()+
  scale_x_date(name ="Date", date_breaks = "1 week", date_labels="%b %y")+
  scale_y_continuous(name="Log of Positive Cases")+
  geom_vline(aes(xintercept = as.numeric(DateEnactedStayAtHome),color="Stay At Home Order"),alpha=.8)+
  geom_vline(aes(xintercept = as.numeric(as.Date(DateEnactedEmergDec)),color="Emergency Declaration"),alpha=.8)+
  geom_vline(aes(xintercept = as.numeric(as.Date(DateEnactedNEBusinessClose)),color="Non-Essential Business Closure"),alpha=.8)+
  geom_vline(aes(xintercept = as.numeric(as.Date(DateEnactedQuarantine)),color="Quarantine"),alpha=.8)+
  geom_vline(aes(xintercept = as.numeric(as.Date(DateEnactedRestaurantRestrict)),color="Restaurant Restrictions"),alpha=.8)+
  geom_vline(aes(xintercept = as.numeric(as.Date(DateEnactedSchoolClose)),color="School Closure"),alpha=.8)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=9,face="bold"),
        axis.text.y=element_text(size=10),
        strip.text = element_text(size=13),
        plot.title=element_text(size=18,face="bold"),
        axis.title.x = element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.title=element_text(size=15, face="bold"),
        legend.text=element_text(size=10),
        legend.key = element_rect(color="black"))+
  scale_color_manual(name="Date Mandate Enacted", values=c("Stay At Home Order"="#0076CA","Emergency Declaration"="#017949","Non-Essential Business Closure"="#6E37FF","Quarantine"="#D469FF","Restaurant Restrictions"="#CA2500","School Closure"="#00A6D8"))+
  labs(title="Positive Cases and Date of Interventions By State")
  
dev.off()