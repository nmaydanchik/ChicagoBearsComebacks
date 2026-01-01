library(tidyverse)
library(patchwork)
library(stringr)
library(scales)
library(flextable)

# Load results
load("BearsResults_Wk17.RData")

# Convert results to data frames with numeric entries, rename subsets for visualizations 
offense_compare <- as.data.frame(cbind(sample=rownames(offense_compare), offense_compare), row.names = FALSE); offense_compare[,2:20]=sapply(offense_compare[,2:20], as.numeric)
offense_compare$sample <- c("Fourth quarter of\ncomeback games", "First three quarters\nof comeback games", "Fourth quarter of\nall other games", "First three quarters of all other games", "Fourth quarter of\nfailed comebacks", "First three quarters of comebacks\nand failed comebacks", "Fourth quarter of all games", "First three quarters of all games", "All offensive plays", "All plays excluding the fourth\nquarters of comebacks")

defense_compare <- as.data.frame(cbind(sample=rownames(defense_compare), defense_compare), row.names = FALSE); defense_compare[,2:20]=sapply(defense_compare[,2:20], as.numeric)
defense_compare$sample <- c("Fourth quarter of\ncomeback games", "First three quarters\nof comeback games", "Fourth quarter of\nall other games", "First three quarters of all other games", "Fourth quarter of\nfailed comebacks", "First three quarters of comebacks\nand failed comebacks", "Fourth quarter of all games", "First three quarters of all games", "All defensive plays", "All plays excluding the fourth\nquarters of comebacks")

special_teams_compare <- as.data.frame(cbind(sample=rownames(special_teams_compare), special_teams_compare), row.names = FALSE); special_teams_compare[,2:6]=sapply(special_teams_compare[,2:6], as.numeric)
special_teams_compare$sample <- c("Fourth quarter of\ncomeback games", "First three quarters\nof comeback games", "Fourth quarter of\nall other games", "First three quarters of all other games", "Fourth quarter of\nfailed comebacks", "First three quarters of comebacks\nand failed comebacks", "Fourth quarter of all games", "First three quarters of all games", "All special teams plays", "All plays excluding the fourth\nquarters of comebacks")

setwd("Figures/")

# Stage 1: Comebacks vs Season wide metrics vs Season excluding comebacks ----

# EPA/play based on play_type and subset

offense <- offense_compare %>% select(sample, epa, epa_pass, epa_run, epa_dropback); colnames(offense)[2:5]=c("epa_All plays", "epa_Pass", "epa_Run", "epa_Dropbacks")
offense <- offense[c(1,9,10),]; offense$sample <- factor(offense$sample, c("All offensive plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

olong <- pivot_longer(offense, names_to = "play_type", names_prefix="epa_",cols=starts_with("epa_"), values_to="epa")

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8:7, 2)]
olong %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,2)), vjust=-0.3, size=2.8, color="black") +
  facet_wrap(~play_type, nrow=1, strip.position="bottom") +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual(values=mycols) +
  geom_hline(yintercept=0) +
  labs(title="Bears offensive efficiency skyrockets during\nthe fourth quarters of comebacks", x="", y="EPA per play") +
  guides(fill=guide_legend(title="Plays"))
ggsave("Stage1/Stage1_OffenseEPA.png", width=7, height=4, dpi=600)  

# Big/Bad Play rate

offense <- offense_compare %>% select(sample, big_play_rate, bad_play_rate)
offense <- offense[c(1,9,10),]; offense$sample <- factor(offense$sample, c("All offensive plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

olong <- pivot_longer(offense, names_to = "play_type", names_pattern="(.*)_play_rate",cols=contains("_rate"), values_to="rate")
olong$play_type <- factor(olong$play_type, c("big", "bad"))

olong %>% ggplot(mapping=aes(x=sample, y=rate, shape=play_type, color=play_type)) +
  geom_point(size=5) +
  #  geom_text(aes(label=percent(round(rate,3)), vjust=(if_else(play_type=="big", -1.2, 2.0))), size=2.8, color="black") +
  coord_cartesian(ylim=c(0,0.25)) +
  scale_y_continuous(labels=scales::percent) +
  guides(fill=guide_legend(title="Big vs. Bad play")) +
  labs(x="", y="Rate", title = "Bears offense has marginal improvement\nin Big/Bad Play rate during comebacks",
       subtitle="Big Play rate = percentage of plays with EPA > 0.90\nBad Play rate = percentage of plays with EPA < -0.75") +
  ggthemes::theme_solarized_2() +
  guides(color=guide_legend(title="Classification"), shape=guide_legend(title="Classification"))
ggsave("Stage1/Stage1_BigPlays_NoText.png", width=7, height=4, dpi=600)  

# Success Rate

offense <- offense_compare %>% select(sample, success_rate)
offense <- offense[c(1,9,10),]; offense$sample <- factor(offense$sample, c("All offensive plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8,7, 2)]
offense %>% ggplot(mapping=aes(x=sample, y=success_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(success_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,.50)) +
  ggthemes::theme_solarized_2() +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "none") +
  labs(x="", y="Success Rate", title = "Bears offense has marginal improvement\nin Success Rate during comebacks",
       subtitle="Success Rate = percentage of plays with EPA > 0")
ggsave("Stage1/Stage1_SuccessRate.png", width=7, height=4, dpi=600)  

# Explosive play rate

offense <- offense_compare %>% select(sample, explosive_play_rate)
offense <- offense[c(1,9,10),]; offense$sample <- factor(offense$sample, c("All offensive plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8,7, 2)]
offense %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(explosive_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.2)) +
  ggthemes::theme_solarized_2() +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "none") +
  labs(x="", y="Explosive play rate", title = "Bears offense explosive play rate nearly doubles\nduring fourth quarters of comebacks",
       subtitle="Explosive play defined as 20+ yard pass or 10+ yard run")
ggsave("Stage1/Stage1_ExplosivePlays.png", width=7, height=4, dpi=600)  

# Completion percentage and completion percentage over expected

offense <- offense_compare %>% select(sample, completion_percent, completion_percent_oe)
offense <- offense[c(1,9,10),]; offense$sample <- factor(offense$sample, c("All offensive plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8,7, 2)]
p1 <- offense %>% ggplot(mapping=aes(x=sample, y=completion_percent, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(completion_percent,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.6)) +
  scale_y_continuous(labels=scales::percent, breaks=c(0,0.15,0.30,0.45,0.60)) +
  geom_hline(yintercept=0) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="", subtitle="Completion percentage", title="Caleb Williams is not completing more passes in comebacks") +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x = element_blank())

p2 <- offense %>% ggplot(mapping=aes(x=sample, y=completion_percent_oe, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(completion_percent_oe,3))), vjust=1.25, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  scale_y_continuous(labels=scales::percent) +
  ggthemes::theme_solarized_2() +
  guides(fill=guide_legend(title="Plays")) +
  coord_cartesian(ylim=c(-0.04,0.04)) +
  geom_hline(yintercept=0) +
  labs(x="", y="", subtitle="Completion percentage\nover expected") +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x = element_blank())
p1+p2
ggsave("Stage1/Stage1_CompPct.png", width=7, height=4, dpi=600)  

# Air yards per completion and YAC

offense <- offense_compare %>% select(sample, air_yards_completion, yac) %>% mutate(total_yards=air_yards_completion+yac)
offense <- offense[c(1,9,10),]; offense$sample <- factor(offense$sample, c("All offensive plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

colnames(offense)[c(2:3)]=c("Air yards per completion", "Yards after the catch")
mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(3,2)]
olong <- offense %>% pivot_longer(names_to = "location", cols=contains("c"), values_to="yards")
olong$location <- factor(olong$location, c("Yards after the catch","Air yards per completion"))
olong %>% ggplot(mapping=aes(x=sample, y=yards)) +
  geom_col(position="stack", aes(fill=location)) +
  scale_fill_manual(values=mycols) +
  geom_text(data=offense, aes(x=sample, y=total_yards, label=format(round(total_yards,1)), nsmall=2), vjust=-0.3, size=2.8, color="black") +
  ggthemes::theme_solarized_2() +
  theme(legend.title = element_blank()) +
  coord_cartesian(ylim=c(0,16)) +
  scale_y_continuous(breaks=c(0,4,8,12,16)) +
  geom_hline(yintercept=0) +
  labs(x="", y="Yards", title="Bears get more yards out of each reception\nduring the fourth quarter of comebacks")
ggsave("Stage1/Stage1_PassYds.png", width=8, height=4, dpi=600)  

# Defense EPA/play, turnovers, and explosive plays

defense <- defense_compare %>% select(sample, epa, to_rate, explosive_play_rate)
defense <- defense[c(1,9,10),]; defense$sample <- factor(defense$sample, c("All defensive plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8,7, 2)]
p1 <- defense %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,3)), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  labs(y="", x="", title="Bears defense is playing worse during comebacks", subtitle="Bears defense EPA\nallowed per play") + 
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(0,0.015)) +
  #  scale_y_continuous(breaks=c(0,0.003,0.006,0.009,0.012)) +
  geom_hline(yintercept=0) +
  theme(legend.position = "none")

p2 <- defense %>% ggplot(mapping=aes(x=sample, y=to_rate, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  coord_cartesian(ylim=c(0.00, 0.04)) +
  labs(x="", y="", subtitle="Bears defense\ntakeaway rate") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  scale_y_continuous(labels=scales::percent) +
  geom_hline(yintercept=0) +
  theme(legend.position = "none")

p3 <- defense %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  scale_y_continuous(labels=scales::percent, breaks=c(0,0.04,0.08,0.12,0.16)) +
  geom_text(aes(label=percent(round(explosive_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  coord_cartesian(ylim=c(0.00, 0.16)) +
  labs(x="", y="", subtitle="Opposing offense\nexplosive play rate") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  guides(fill=guide_legend(title="Plays")) +
  geom_hline(yintercept=0)
p1+p3
ggsave("Stage1/Stage1_DefenseSummary.png", width=8, height=4, dpi=600)

# Special teams Big Play rate and EPA/play

special_teams <- special_teams_compare[c(1,9,10),]; special_teams$sample <- factor(special_teams$sample, c("All special teams plays", "All plays excluding the fourth\nquarters of comebacks", "Fourth quarter of\ncomeback games"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8, 7, 2)]
special_teams %>% ggplot(mapping=aes(x=sample, y=big_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(big_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.15)) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels=scales::percent) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Big Play rate", title="Bears special teams Big Play rate nearly doubles\nduring fourth quarters of comebacks",
       subtitle="Big Play rate = percentage of plays with EPA > 0.90")
ggsave("Stage1/Stage1_SpecialTeamsBigPlays.png", width=7, height=4, dpi=600)

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(8, 7, 2)]
special_teams %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,3)), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  #  coord_cartesian(ylim=c(0,0.15)) +
  geom_hline(yintercept=0) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="EPA per play", title="Bears special teams EPA per play explodes\nduring fourth quarters of comebacks")
ggsave("Stage1/Stage1_SpecialTeamsEPA.png", width=7, height=4, dpi=600)

# Stage 2: Comparison vs first three quarters of those games and other fourth quarters --------

offense_2 <- offense_compare[c(1,2,3,4),]; offense_2$sample <- factor(offense_2$sample, c("First three quarters\nof all other games", "Fourth quarter of\nall other games", "First three quarters\nof comeback games", "Fourth quarter of\ncomeback games"))

# EPA per play

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(7,8,3,2)]
offense_2 %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,2), vjust=if_else(epa>0, -0.3, 1.25)), size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(-.04,0.3)) +
  geom_hline(yintercept=0) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="EPA per play", title="Bears offensive efficiency during\ncomebacks is an anomaly")
ggsave("Stage2_New/Stage2_OffenseEPA.png", width=7, height=4, dpi=600)

# Big/Bad play rate

offense <- offense_2 %>% select(sample, big_play_rate, bad_play_rate)
olong <- pivot_longer(offense, names_to = "play_type", names_pattern="(.*)_play_rate",cols=contains("_rate"), values_to="rate")
olong$play_type <- factor(olong$play_type, c("big", "bad"))

olong %>% ggplot(mapping=aes(x=sample, y=rate, group=play_type, shape=play_type, color=play_type, linetype = play_type)) +
  geom_point(size=5) +
  #  geom_line() +
  coord_cartesian(ylim=c(.15,0.25)) +
  scale_y_continuous(labels=scales::percent) +
  guides(fill=guide_legend(title="Big vs. Bad play")) +
  labs(x="", y="Rate", title = "Bears offense Big/Bad Play rate improves in fourth\nquarter but improves much more in comebacks",
       subtitle="Y-axis truncated for visual clarity") +
  ggthemes::theme_solarized_2() +
  guides(color=guide_legend(title="Classification"), shape=guide_legend(title="Classification"), linetype=guide_legend(title="Classification"))
ggsave("Stage2_New/Stage2_BigBad.png", width=7, height=4, dpi=600)

# Success Rate

offense_2 %>% ggplot(mapping=aes(x=sample, y=success_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(success_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,.50)) +
  ggthemes::theme_solarized_2() +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "none") +
  labs(x="", y="Success Rate", title = "Bears offense Success Rate improvement is not fourth\nquarter specific but possibly game specific",
       subtitle="Success Rate = percentage of plays with EPA > 0")
ggsave("Stage2_New/Stage2_SuccessRate.png", width=8, height=4, dpi=600)  

# Explosive play rate

offense_2 %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(explosive_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.2)) +
  ggthemes::theme_solarized_2() +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "none") +
  labs(x="", y="Explosive play rate", title = "Bears offense explosive play rate only\nincreases dramatically in comebacks")
ggsave("Stage2_New/Stage2_ExplosivePlays.png", width=7, height=4, dpi=600)  

# Pass yards per catch

offense_3 <- offense_2 %>% select(sample, air_yards_completion, yac) %>% mutate(total_yards=air_yards_completion+yac)

colnames(offense_3)[c(2:3)]=c("Air yards per completion", "Yards after the catch")
mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(3,2)]
olong <- offense_3 %>% pivot_longer(names_to = "location", cols=contains("c"), values_to="yards")
olong$location <- factor(olong$location, c("Yards after the catch","Air yards per completion"))
olong %>% ggplot(mapping=aes(x=sample, y=yards)) +
  geom_col(position="stack", aes(fill=location)) +
  geom_text(data=offense_3, aes(x=sample, y=total_yards, label=format(round(total_yards,1)), nsmall=2), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  coord_cartesian(ylim=c(0,16)) +
  scale_y_continuous(breaks=c(0,4,8,12,16)) +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept=0) +
  labs(x="", y="Yards", title="Bears offense yards per reception increases slightly in the\nfourth quarter but increases much more in comebacks")
ggsave("Stage2_New/Stage2_PassYds.png", width=8, height=4, dpi=600)  

# Special teams

special_teams_2 <- special_teams_compare[c(1,2,3,4),]; special_teams_2$sample <- factor(special_teams_2$sample, c("First three quarters\nof all other games", "Fourth quarter of\nall other games", "First three quarters\nof comeback games", "Fourth quarter of\ncomeback games"))

mycols <- RColorBrewer::brewer.pal(8, "RdYlBu")[c(7,8,3,2)]
special_teams_2 %>% ggplot(mapping=aes(x=sample, y=big_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(big_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.15)) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels=scales::percent) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Big Play rate", title="Bears special teams Big Play rate improvement\nis not fourth quarter or game specific")
ggsave("Stage2_New/Stage2_SpecialTeamsBigPlays.png", width=7, height=4, dpi=600)

special_teams_2 %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,3), vjust=if_else(epa>0, -0.3, 1.25)), size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(-.04,0.06)) +
  geom_hline(yintercept=0) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="EPA per play", title="Bears special teams EPA per play improves in fourth\nquarter but improves much more in comebacks")
ggsave("Stage2_New/Stage2_SpecialTeamsEPA.png", width=7, height=4, dpi=600)


# Stage 3: Successful vs Failed fourth quarter comebacks and first 3 quarters of one possession games baseline ----

# Offense

offense_4 <- offense_compare[c(1,5,6),]; offense_4$sample <- factor(offense_4$sample, c("First three quarters of comebacks\nand failed comebacks", "Fourth quarter of\nfailed comebacks", "Fourth quarter of\ncomeback games"))

mycols <- c(RColorBrewer::brewer.pal(8, "Set2")[1], RColorBrewer::brewer.pal(8, "RdYlBu")[c(1, 2)])
p1 <- offense_4 %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,2)), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(0,0.3)) +
  labs(subtitle="EPA per play", y="", x="", title="Bears offensive efficiency does not improve enough in fourth quarters\nof one possesion losses") +
  theme(legend.position = "none")

p2 <- offense_4 %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(explosive_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(0,0.2)) +
  scale_y_continuous(labels=scales::percent) +
  labs(x="", y="", subtitle="Explosive play rate") +
  guides(fill=guide_legend(title="Plays"))

p3 <- offense_4 %>% ggplot(mapping=aes(x=sample, y=completion_percent, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(completion_percent,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(0,0.65)) +
  scale_y_continuous(labels=scales::percent, breaks=c(0,0.15,0.30,0.45,0.60)) +
  labs(title="Caleb Williams makes fewer passes, passes do not get enough yards\nper reception in fourth quarters of one possesion losses", x="", subtitle ="Completion percentage", y="") +
  theme(legend.position = "none")

p4 <- offense_4 %>% ggplot(mapping=aes(x=sample, y=yac, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(yac,1)), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(0,9)) +
  scale_y_continuous(breaks=c(0,2,4,6,8)) +
  labs(x="", y="", subtitle ="Yards after the catch") +
  theme(legend.position = "none")

p5 <- offense_4 %>% ggplot(mapping=aes(x=sample, y=air_yards_completion, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(air_yards_completion,1)), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(0,9)) +
  scale_y_continuous(breaks=c(0,2,4,6,8)) +
  labs(x="", y="", subtitle ="Air yards per completion") +
  theme(legend.position = "none")

(p1+p2)/(p5+p4)+plot_layout(guides="collect")

ggsave("Stage3/Stage3_Offense.png", width=8, height=5, dpi=600)

# Special Teams

special_teams_4 <- special_teams_compare[c(1,5,6),]; special_teams_4$sample <- factor(special_teams_4$sample, c("First three quarters of comebacks\nand failed comebacks", "Fourth quarter of\nfailed comebacks", "Fourth quarter of\ncomeback games"))

mycols <- c(RColorBrewer::brewer.pal(8, "Set2")[1], RColorBrewer::brewer.pal(8, "RdYlBu")[c(1, 2)])
special_teams_4 %>% ggplot(mapping=aes(x=sample, y=big_play_rate, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(big_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(0,0.15)) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels=scales::percent) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="Big Play rate", title="Bears special teams made zero big plays in failed comebacks")
ggsave("Stage3/Stage3_SpecialTeamsBigPlays.png", width=7, height=4, dpi=600)

special_teams_4 %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position=position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,2), vjust=if_else(epa>0, -0.3, 1.25)), size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  coord_cartesian(ylim=c(-.2,0.1)) +
  geom_hline(yintercept=0) +
  ggthemes::theme_solarized_2() +
  theme(legend.position = "none") +
  labs(x="", y="EPA per play", title="Bears special teams was really bad in failed comebacks")
ggsave("Stage3/Stage3_SpecialTeamsEPA.png", width=7, height=4, dpi=600)

# Defense

defense_4 <- defense_compare[c(1,5,6),]; defense_4$sample <- factor(defense_4$sample, c("First three quarters of comebacks\nand failed comebacks", "Fourth quarter of\nfailed comebacks", "Fourth quarter of\ncomeback games"))

mycols <- c(RColorBrewer::brewer.pal(8, "Set2")[1], RColorBrewer::brewer.pal(8, "RdYlBu")[c(1, 2)])
p1 <- defense_4 %>% ggplot(mapping=aes(x=sample, y=epa, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  geom_text(aes(label=round(epa,2), vjust=if_else(epa>0, -0.3, 1.25)), size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  labs(y="", x="", title="Bears defense is horrific during failed comebacks", subtitle="Bears defense EPA\nallowed per play") + 
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(-0.03,0.6)) +
  geom_hline(yintercept=0) +
  theme(legend.position = "none")

p2 <- defense_4 %>% ggplot(mapping=aes(x=sample, y=to_rate, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(to_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  coord_cartesian(ylim=c(0.00, 0.04)) +
  labs(x="", y="", subtitle="Bears defense\ntakeaway rate") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  scale_y_continuous(labels=scales::percent) +
  geom_hline(yintercept=0) +
  theme(legend.position = "none")

p3 <- defense_4 %>% ggplot(mapping=aes(x=sample, y=explosive_play_rate, fill=sample)) +
  geom_col(position = position_dodge(width=0.8)) +
  geom_text(aes(label=percent(round(explosive_play_rate,3))), vjust=-0.3, size=2.8, color="black") +
  scale_fill_manual(values=mycols) +
  ggthemes::theme_solarized_2() +
  scale_y_continuous(labels=scales::percent) +
  coord_cartesian(ylim=c(0.00, 0.20)) +
  labs(x="", y="", subtitle="Opposing offense\nexplosive play rate") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  guides(fill=guide_legend(title="Plays")) +
  geom_hline(yintercept=0)
p1+p2+p3
ggsave("Stage3/Stage3_DefenseSummary.png", width=8, height=4, dpi=600)

# Making tables ----

# Table settings
set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 6,
  background.color = "#EFEFEF")

# Prepare offense results for table
offense_table <- cbind(offense_compare$sample, as.data.frame(lapply(offense_compare[,-1], round, 3)))
colnames(offense_table)[1]="subset"

offense_table %>% select(subset, epa, epa_pass, epa_run, epa_dropback, yac, air_yards_completion) %>% flextable() # Continuous metrics
offense_table %>% select(subset, success_rate, big_play_rate, bad_play_rate, explosive_play_rate, to_rate, completion_percent, completion_percent_oe) %>% flextable() # Rates

# Prepare defense results for table
defense_table <- cbind(defense_compare$sample, as.data.frame(lapply(defense_compare[,-1], round, 3)))
colnames(defense_table)[1]="subset"

defense_table %>% select(subset, epa, epa_pass, epa_run, epa_dropback, yac, air_yards_completion) %>% flextable() # Continuous metrics
defense_table %>% select(subset, success_rate, big_play_rate, bad_play_rate, explosive_play_rate, to_rate, completion_percent, completion_percent_oe) %>% flextable() # Rates

# Prepare special teams results for table
special_table <- cbind(special_teams_compare$sample, as.data.frame(lapply(special_teams_compare[,-1], round, 3)))
colnames(special_table)[1]="subset"

special_table %>% flextable()
