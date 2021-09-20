suppressMessages(library("tidyverse"))
suppressMessages(library("viridis"))
suppressMessages(library("gridExtra"))
library("modelr")
suppressMessages(library("broom"))
library("ggrepel")

set.seed(42)

#Part 1
#importing data
allmatches <- suppressMessages(read_csv('D:/TCET SE/DS internship/results.csv'))%>% mutate(Year=as.numeric(format(date,"%Y")))
head(allmatches)

#importing fifa data
fifa2018worldcup <- suppressMessages(read_csv('D:/TCET SE/DS internship/fifa2018.csv'))

#prediction dataset (testing data)
fifa2018worldcup_pred <- fifa2018worldcup %>% 
  mutate(Date=as.Date(Date,"%d/%m/%Y %H:%M"),
  ) %>% 
  select(Date,Round='Round Number',Group,home_team='Home Team',away_team='Away Team') %>%
  mutate(home_team=ifelse(str_detect(home_team,"Group"),ifelse(str_detect(home_team,"Winner"),
                                                               paste0(str_replace(home_team,"Winner Group",""),"1"),
                                                               paste0(str_replace(home_team,"Runner-up Group",""),"2")),home_team),
         away_team=ifelse(str_detect(away_team,"Group"),ifelse(str_detect(away_team,"Winner"),
                                                               paste0(str_replace(away_team,"Winner Group",""),"1"),
                                                               paste0(str_replace(away_team,"Runner-up Group",""),"2")),away_team),
         Round=ifelse(Round %in% c("1","2","3"),paste0("G"),Round),
         Round=ifelse(Round == "Round of 16","LS",Round),
         Round=ifelse(Round == "Quarter Finals","QF",Round),
         Round=ifelse(Round == "Semi Finals","SF",Round),
         Round=ifelse(Round == "Finals","F",Round)) %>% group_by(Round) %>% 
  mutate(GameID=paste0(Round,1:n()),
         Winner=paste0("Winner_",GameID),
         Looser=paste0("Looser_",GameID),
         Group=ifelse(is.na(Group),"All",Group),
         home_team=ifelse(Group=="All",str_replace_all(home_team,"[\t\b ]",""),home_team),
         away_team=ifelse(Group=="All",str_replace_all(away_team,"[\t\b ]",""),away_team)) %>% ungroup() 

fifa2018worldcup_pred <- rbind(fifa2018worldcup_pred %>% filter((Round %in% c("G","LS"))),fifa2018worldcup_pred %>% filter(!(Round %in% c("G","LS"))) %>%
                                 mutate(home_team=c(paste0("LS",c(1,5,7,3)),paste0("QF",c(1,3)),"SF1","SF1"),
                                        away_team=c(paste0("LS",c(2,6,8,4)),paste0("QF",c(2,4)),"SF2","SF2")))



# Create List of Teams in Tournament
fifa2018teams <- fifa2018worldcup_pred %>% filter(Round=="G") %>% count(home_team) %>% select(home_team)
fifa2018teams <- fifa2018teams$home_team


head(allmatches)
tail(allmatches)
cbind(c("Games","Variables"),dim(allmatches))


options(repr.plot.width=7, repr.plot.height=4)
top_7_tournaments <- allmatches %>% count(tournament) %>% top_n(7,n) %>% select(-n) 
top_7_tournaments <- allmatches %>% filter(tournament!="Friendly") %>% ungroup() %>% 
  mutate(Year=floor(Year/4)*4,
         tournament=ifelse(tournament %in% top_7_tournaments$tournament,tournament,"Other")) %>%
  group_by(tournament)
#ggolpt1
ggplot(top_7_tournaments %>% count(Year) %>% filter(!is.na(Year) & !is.na(n) & Year<2016) ,
       aes(x=Year,y=n,fill=reorder(tournament,n,sum))) + 
  geom_area(show.legend=T, color="White",size=0.5) + scale_fill_viridis(discrete=T) + 
  scale_x_continuous(limits=c(min(top_7_tournaments$Year),max(top_7_tournaments$Year)-1))+
  labs(y="") + ggtitle("Annual matches") + theme_minimal()


#Also, most of the matches aren't played in the major competitions but as friendlies or during smaller tournaments.
ggplot(top_7_tournaments%>% filter(!is.na(tournament))  %>% count(tournament)  , 
       aes(x=reorder(tournament,n,sum), y=n, fill=n)) + labs(y="", x="", fill="") +
  geom_bar(stat="identity", pos="stack",show.legend=F) + coord_flip() + 
  scale_fill_viridis() + ggtitle("Occasions")+ theme_minimal()




#Match importance (obviously FIFA >>> anyother tournament)
# Recode Matches
matches <- allmatches %>% mutate(Importance = ifelse(str_detect(tournament,"FIFA"),1,NA),
                                 Importance = ifelse(str_detect(tournament,"UEFA"),.9,Importance),
                                 Importance = ifelse(str_detect(tournament,"Copa América"),.5,Importance),
                                 Importance = ifelse(str_detect(tournament,"African Cup of Nations"),.5,Importance),
                                 Importance = ifelse(!str_detect(tournament,"Friendly") & is.na(Importance),.1,Importance),
                                 Importance = ifelse(str_detect(tournament,"Friendly"),.01,Importance),
                                 Importance = ifelse(str_detect(tournament,"qualification"),Importance*.75,Importance))




#Top 5 competitions
top5competitions <- suppressMessages(matches %>% group_by(tournament) %>% summarise(n=n(),Importance=mean(Importance)) %>% arrange(-Importance) %>% top_n(5))

#plot of importance
options(repr.plot.width=8, repr.plot.height=4)
ggplot(top5competitions,aes(x=n,y=Importance,colour=tournament,size=n))+
  geom_point()+  ggtitle("Importance by Tournament")+ theme_minimal() + scale_colour_viridis(discrete=T) +
  guides(size=FALSE) + theme(legend.position="bottom")+labs(y="",colour="",x="\nNumber of Games 1872-2018")

#past data 

options(repr.plot.width=4, repr.plot.height=3)
fifa_finals <- matches %>% filter(str_detect(tournament,"FIFA") &  !str_detect(tournament,"qualification")) %>%  
  mutate(doy=as.numeric(format(date,"%j"))) %>% group_by(Year) %>% arrange(-Year,-doy) %>% filter(doy==max(doy)) %>%
  mutate(Winner=ifelse(home_score>away_score,home_team,away_team),
         Looser=ifelse(home_score<away_score,home_team,away_team)) %>% ungroup() %>% select(Year,date,Winner,Looser,city)
options(repr.plot.width=6, repr.plot.height=3)


#updating teams dataset
teams <- rbind(matches %>% select(Year,date,Team=home_team, Opponent = away_team,
                                  scored=home_score,received=away_score,Importance) 
               %>% mutate(Location="Home"),
               matches %>% select(Year,date,Team=away_team, Opponent = home_team,
                                  scored=away_score,received=home_score,Importance) %>% 
                 mutate(Location="Away")) %>%
  arrange(date) %>% 
  group_by(Year,Team) 
head(teams)
rbind(c("Matches","Features"),dim(teams))

#we first defined a  strength indicator based on the proportion of games won multiplied by sum of the importance of the games.
team_strength_Year10 <- teams %>% 
  summarise(Won=mean(as.numeric(scored>received)), Matches=n(),
            Importance=sum(Importance)) %>% ungroup() %>% mutate(Year10=floor(Year/10)*10) %>% 
  group_by(Year10,Team) %>% 
  summarise(Won=mean(Won), Matches=sum(Matches),Importance=sum(Importance)) %>% 
  mutate(Strength=Won*Importance) %>%  arrange(-Year10,-Strength) %>% group_by(Year10) %>% 
  mutate(Strength=Strength/max(Strength))


#We're going to estimate 3 parameters and their development over time for each time having ever participated in a World Cup
#Overvall Strength: 
#Offensive Strength
#Defensive Strength





# Join Matches and Crude Opponent Strength Estimates 
basedata <- left_join(teams %>% mutate(Year10=floor(Year/10)*10),
                      team_strength_Year10 %>% ungroup() %>% select(Opponent=Team, Year10,Crude_Opp_Strength=Strength),
                      by=c("Opponent","Year10"), all.x=T) %>% filter(Year>=1955)

#Add features to control for
basedata <- basedata %>% mutate(Month=format(date, "%b"),Month_num=format(date, "%m"))


# Include Time to next final in yrs and indicator for Game Won
regdata <- left_join(basedata,fifa_finals %>% select(Year,Next_final=date),by="Year")  %>% 
  ungroup() %>% arrange(date)  %>% fill(Next_final, .direction="up") %>% filter(Year>=1955) %>% 
  mutate(Years_to_next_final=as.numeric(Next_final-date)/365) %>% select(-Year10,-Month,-Next_final) %>% 
  mutate(Game_Won=as.numeric(scored>received ))

# Filter: Predict only for Teams that managed to ever participate in FIFA Finals

legitteams <- regdata %>% filter(Importance==1 | Team %in% fifa2018teams) %>% count(Team) %>% filter(n>0)

# Function to recode features in Training and Prediction Datasets
mytransformations <- function(data) {
  data %>%
    mutate(Year2=Year^2,
           Year3=Year^3,
           Home=as.numeric(Location=="Home"))
}



# Create Training and Prediction Datasets
prediction.df <- regdata %>% filter(Year>1955 & Team %in% legitteams$Team) %>% 
  group_by(Team) %>%
  mytransformations(.)
margins.df <- data_grid(regdata %>% filter(Year>1955 & Team %in% legitteams$Team)  , 
                        Team= Team, Year = seq_range(Year,100),
                        Crude_Opp_Strength=1,Importance=1,Location="Home") %>% 
  mytransformations(.)
prediction.df

#multiple linear regression
# Fit Models
# Fit Models
str(prediction.df)
options(warn=-1)
prediction.model <- NULL
prediction.model <- prediction.df %>% 
  do(overall = glm(Game_Won ~ Crude_Opp_Strength+Importance+Year+Year^2+Year^3+Home+1,
                   family=binomial(link='logit'),data = ., weight=1/(1+2018-Year)),
     offensive = glm(scored ~ Crude_Opp_Strength+Importance+Year+Year^2+Year^3+Home+1,
                     family="poisson",data = ., weight=1/(1+2018-Year)),
     defensive = glm(received ~ Crude_Opp_Strength+Importance+Year+Year^2+Year^3+Home+1,
                     family="poisson",data = ., weight=1/(1+2018-Year)))

options(warn=0)



#plotting chances of all teams winning or losing
#multiple linear regression('applied using variables off strength,overall strength)


ggplot(fifa_finals %>% count(Winner), aes(x=reorder(Winner,n,sum),y=n,fill=reorder(Winner,n,sum))) + 
  geom_bar(stat="identity", show.legend=F) + scale_fill_viridis(discrete=T) + 
  labs(x="", y="") + ggtitle("FIFA World Cup Winners") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust=1))

ggplot(fifa_finals %>% count(Looser), aes(x=reorder(Looser,n,sum),y=n,fill=reorder(Looser,n,sum))) + 
  geom_bar(stat="identity", show.legend=F) + scale_fill_viridis(discrete=T) + 
  labs(x="", y="") + ggtitle("FIFA World Cup Loosers") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust=1))


