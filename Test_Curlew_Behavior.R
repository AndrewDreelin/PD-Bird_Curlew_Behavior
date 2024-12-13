#Test Curlew Behavior Analysis

#load general library
library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme)
library(visreg)
library(mgcv)
library(freqtables)

#load specific packages

#load data
trials<-read.csv("Predator_Trial_Data_All.csv") #read final dataset

#view data
str(trials)

trials$Year<-as.factor(trials$Year) #change variable classification
trials$Trial_Order<-as.factor(trials$Trial_Order) #change variable classification
trials$Mob_Distance<-as.numeric(trials$Mob_Distance) #change variable classification

#remove the one where I accidentally put the speaker too close
trials<-filter(trials, Nest_ID != "MT23-XRV-02")

#remove the NAs/nests where I only have one trial
trials<-filter(trials, Nest_ID != "MT23-XRV-04") 

#remove Kima in 2024 because she was previously trialed and remembered us
trials<-filter(trials, Nest_ID !="MT24-RAD-01")

#remove outlier
trials<-filter(trials, Nest_ID !="MT23-XRV-03") ## do I have justification to remove this? Did Pronghorn change things?

#data exploration
summary(trials$Cryptic_Distance)

hist(trials$Cryptic_Distance)

ggplot(data=trials, aes(sample = Cryptic_Distance))+
  stat_qq() + stat_qq_line()

#mostly normal, one outlier

#filter for playback/no playback
treatment<-filter(trials, PD_Playback == "Yes") 
control<-filter(trials, PD_Playback == "No") 

#testing if control protocol change affected cryptic distance
control23<-filter(control, Year == "2023")
control24<-filter(control, Year == "2024")

summary(control23$Cryptic_Distance)
summary(control24$Cryptic_Distance)

summary(aov(Cryptic_Distance~Year, data = control))
#Cryptic distance on control trials doesn't vary between years

#testing if trial order affected cryptic distance

summary(trials$Trial_Order)
#not balanced

control_first<-filter(trials, Trial_Order == "Control First")
treatment_first<-filter(trials, Trial_Order == "Playback First")

summary(control_first$Cryptic_Distance)
summary(treatment_first$Cryptic_Distance)

summary(aov(Cryptic_Distance~Trial_Order, data = trials))
#Trial order does not affect cryptic distance


#effect of male presence on cryptic distance
trials_male<-filter(trials, Male_Visible == "Yes")
trials_nomale<-filter(trials, Male_Visible == "No")
treatment_male<-filter(treatment, Male_Visible == "Yes")
treatment_nomale<-filter(treatment, Male_Visible == "No")
control_male<-filter(control, Male_Visible == "Yes")
control_nomale<-filter(control, Male_Visible == "No")

summary(trials_male$Cryptic_Distance)
summary(trials_nomale$Cryptic_Distance)

summary(treatment_male$Cryptic_Distance)
summary(treatment_nomale$Cryptic_Distance)

summary(control_male$Cryptic_Distance)
summary(control_nomale$Cryptic_Distance)

#visually, doesn't seem like much of difference on treatment trials but DOES seem like a difference on controls
#only three instances where the male was visible during control trials though, so tiny sample size-- difference just driven by Marsha


#basic summary stats
summary(treatment$Cryptic_Distance)
summary(control$Cryptic_Distance)

#basic anova
summary(aov(Cryptic_Distance~PD_Playback, data = trials))
#biologically significant difference in cryptic distance when playback (moderate p-value)

cryptic_model<-lmer(Cryptic_Distance~PD_Playback + (1|Trial_Order), data = trials)
anova(cryptic_model)
#singular boundary fit-- probably because of uneven trial order

cryptic_model2<-lmer(Cryptic_Distance~Trial_Order + (1|PD_Playback), data = trials)
anova(cryptic_model2)
#trial order does not affect cryptic distance

cryptic_model3<-lmer(Cryptic_Distance~Male_Visible + (1|PD_Playback), data = trials)
anova(cryptic_model3)
summary(aov(Cryptic_Distance~Male_Visible, data = trials))
#male presence does not affect cryptic distance

cryptic_dist_plot<-ggplot(data = trials, mapping = aes(x = reorder(PD_Playback,-Cryptic_Distance), y = Cryptic_Distance, fill = PD_Playback)) +
  geom_boxplot() +
  geom_point () +
  labs(x = "Prairie Dog Alarm Calls", y = "Cryptic Posture Initiation Distance (m)") + 
  coord_flip() +
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title=element_text(size=18),
    axis.text = element_text(size=18),
    panel.grid.minor = element_blank()
  )
  
plot(cryptic_dist_plot)

#########################

#using a GLM to test for differences in cryptic posture between control and treatment

cryptic_lm<-lm(Cryptic_Distance ~ PD_Playback, data = trials)
summary(cryptic_lm)
#significant difference/same result as with ANOVA

#adding in trial order as a random effect
cryptic_lmer<-lmer(Cryptic_Distance ~ PD_Playback + (1|Trial_Order), data = trials)
summary(cryptic_lmer)
#singular boundary fit because there's not an even number of playback first and control first

#########################

#are alarm calls more likely with PD playback?

str(trials)

trials$PD_Playback<-as.factor(trials$PD_Playback)
trials$Alarm_Call<-as.factor(trials$Alarm_Call)
trials$Mobbing<-as.factor(trials$Mobbing)

alarm_data = data.frame(trials$Alarm_Call,trials$PD_Playback)

alarm_data = table(trials$Alarm_Call,trials$PD_Playback)

print(alarm_data)

print(chisq.test(alarm_data))
#no difference/not enough info

alarm_df = data.frame(trials$Alarm_Call,trials$PD_Playback)

#visualizing

cbPalette <- c("#F0E442","#0072B2")
               
#new version
alarm_plot<-ggplot(alarm_df) + 
  geom_bar(aes(x = trials.Alarm_Call, fill = trials.Alarm_Call)) +
  geom_text(stat='count',aes(x=trials.Alarm_Call, label=after_stat(count)),vjust=-1, size = 8) + 
  facet_wrap(~factor(alarm_df$trials.PD_Playback, levels=c("Yes","No")), strip.position = "bottom") + 
  labs(x = "Prairie Dog Playback", y = "# of Trials") +
  scale_fill_manual("Curlew Alarm Calls", values = cbPalette) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        #legend.position = "none",
        strip.text = element_text(size=14),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=14),
  )
alarm_plot + guides(fill=guide_legend(title="Curlew Alarm Calls"))

#is mobbing more likely with PD playback?

mobbing_data = data.frame(trials$Mobbing,trials$PD_Playback)

mobbing_data = table(trials$Mobbing,trials$PD_Playback)

print(mobbing_data)

print(chisq.test(mobbing_data))
#no difference/not enough info

mob_df = data.frame(trials$Mobbing,trials$PD_Playback)

#visualizing

cbPalette <- c("#F0E442","#0072B2")

#new version
mob_plot<-ggplot(mob_df) + 
  geom_bar(aes(x = trials.Mobbing, fill = trials.Mobbing)) +
  geom_text(stat='count',aes(x=trials.Mobbing, label=after_stat(count)),vjust=-0.5, size = 8) + 
  facet_wrap(~factor(alarm_df$trials.PD_Playback, levels=c("Yes","No")), strip.position = "bottom") + 
  labs(x = "Prairie Dog Playback", y = "# of Trials") +
  scale_fill_manual("Curlew Mobbing", values = cbPalette) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        #legend.position = "none",
        strip.text.x = element_text(size=14),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=14),
  )
mob_plot + guides(fill=guide_legend(title=" Curlew Mobbing"))


mob_df_male = data.frame(trials_male$Mobbing,trials_male$PD_Playback)

mob_male_plot<-ggplot(mob_df_male) + 
  geom_bar(aes(x = trials_male.Mobbing, fill = trials_male.Mobbing)) +
  geom_text(stat='count',aes(x=trials_male.Mobbing, label=after_stat(count)),vjust=-0.5, size = 8) + 
  facet_wrap(~factor(mob_df_male$trials_male.PD_Playback, levels=c("Yes","No")), strip.position = "bottom") + 
  labs(x = "Prairie Dog Playback", y = "# of Trials") +
  scale_fill_manual("Curlew Mobbing", values = cbPalette) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        #legend.position = "none",
        strip.text.x = element_text(size=14),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=14),
  )
mob_male_plot + guides(fill=guide_legend(title=" Curlew Mobbing"))


#########################


#doing the same for alarm calls and mobbing

#summary stats
summary(treatment$Alarm_Distance)
summary(control$Alarm_Distance)

#basic anova
summary(aov(Alarm_Distance~PD_Playback, data = trials))
#not a significant difference in alarm distance

alarm_model<-lmer(Alarm_Distance~PD_Playback + (1|Trial_Order), data = trials)
anova(alarm_model)

alarm_dist_plot<-ggplot(data = trials, mapping = aes(x = PD_Playback, y = Alarm_Distance, fill = PD_Playback)) +
  geom_boxplot() +
  geom_point () +
  labs(x = "Prairie Dog Alarm Calls", y = "Alarm Call Initation Distant (m)") + 
  coord_flip() +
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title=element_text(size=18),
    axis.text = element_text(size=14),
    panel.grid.minor = element_blank()
  )

plot(alarm_dist_plot)

#summary stats
summary(treatment$Mob_Distance)
summary(control$Mob_Distance)

#basic anova
summary(aov(Mob_Distance~PD_Playback, data = trials))
#not a significant difference in mobbing distance with playback

mob_dist_plot<-ggplot(data = trials, mapping = aes(x = PD_Playback, y = Mob_Distance, fill = PD_Playback)) +
  geom_boxplot() +
  geom_point () +
  labs(x = "Prairie Dog Alarm Calls", y = "Alarm Call Initiation Distance (m)") + 
  coord_flip() +
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title = element_text(size=18),
    axis.text = element_text(size=18),
    panel.grid.minor = element_blank()
  )

plot(mob_dist_plot)









