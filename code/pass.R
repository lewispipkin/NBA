rm(list=ls())
##necessary packages
library(tidyverse)
library(readxl)
library(wrapr)
library(e1071)


dataPassesPlayerBase <- read.csv("passesMaster.csv")
names(dataPassesPlayerBase)
passing_perGame <- read_excel("~/Downloads/passing.xlsx", sheet = "perGame")
passing_Totals <- read_excel("~/Downloads/passing.xlsx", sheet = "Totals")


players_passes <- dataPassesPlayerBase %>% 
  filter(typePass=='made') %>% 
  group_by(namePlayer) %>% 
  summarize(passes=sum(passes), fgm=sum(fgm), fga=sum(fga),
            fg2m=sum(fg2m),fg2a=sum(fg2a),fg3m=sum(fg3m),
            fg3a=sum(fg3a),ast=sum(ast))

left_join(passing_perGame, passing_Totals,by="PLAYER_ID") -> t
right_join(t,players_passes,by=c("PLAYER_NAME.x"="namePlayer")) -> tx

names(tx) <- wrapr::qc(Player_ID,Player_Name,Team_ID,Team_Abbrev,GP,W,L,MPG,PassesMade_PG,PassesReceived_PG,APG,
                       FTAst_PG,SecAst_PG,PotentAst_PG,AstPointsCreated_PG,AdjAst_PG,AstToPassPct_PG,
                       AstToPassPctAdj_PG,name,teamid,abbr,gp,w,l,MP,TotalPassesMade,TotalPassesReceived,TotalAst,
                       TotalFTAst,TotalSecAst,TotalPotentAst,TotalAstPointsCreated,TotalAdjAst,TotalAstToPassPct,
                       TotalAdjAstToPassPct,passes,EventualFGM,EventualFGA,EventualFG2M,EventualFG2A,
                       EventualFG3M,EventualFG3A,ast)

tx %>% select(Player_Name,Team_Abbrev,
              GP,MPG,MP,PassesMade_PG,TotalPassesMade,
              APG,TotalAst,FTAst_PG,TotalFTAst,
              SecAst_PG,TotalSecAst,PotentAst_PG,TotalPotentAst,
              AstPointsCreated_PG,TotalAstPointsCreated,
              EventualFGM,EventualFGA,EventualFG2M,EventualFG2A,
              EventualFG3M,EventualFG3A) -> base
base %>% mutate(PassesMade_PG=TotalPassesMade/GP,
                APG=TotalAst/GP,
                TotalFTAst=as.numeric(TotalFTAst),
                FTAst_PG=TotalFTAst/GP,
                MPG=MP/GP,
                SecAst_PG=TotalSecAst/GP,
                PotentAst_PG=TotalPotentAst/GP,
                AstPointsCreated_PG=TotalAstPointsCreated/GP) -> base
#side-by-side tables
breaks <- data.frame(I=rep("|",6))
t1 <- base %>% select("Player Name"=Player_Name, PassesMade_PG) %>% arrange(desc(PassesMade_PG)) %>% head()
t2 <- base %>% select("Player Name"=Player_Name,FTAst_PG) %>% arrange(desc(FTAst_PG)) %>% head() 
t3 <- base %>% select("Player Name"=Player_Name,SecAst_PG) %>% arrange(desc(SecAst_PG)) %>% head() 
cbind(t1,breaks,t2,breaks,t3)
t1=base %>% select("Player Name"=Player_Name,PotentAst_PG) %>% arrange(desc(PotentAst_PG)) %>% head() 
t2=base %>% select("Player Name"=Player_Name,AstPointsCreated_PG) %>% arrange(desc(AstPointsCreated_PG)) %>% head() 
cbind(t1,breaks,t2)

#adding turnovers
basestats <- read_excel("Documents/basestats.xlsx",col_names = F)
names(basestats) = c("x1","x2")
vec <- 1:nrow(basestats)
odds <- vec[vec %% 2==1]
evens <- vec[vec %% 2==0]
basestats %>% filter(x1 %in% c("PLAYER_NAME","TOV")) -> tov
turnovers <- data.frame(tov[odds,2],tov[evens,2])
names(turnovers) <- c("Player_Name","TOV")
turnovers$TOV <- as.numeric(turnovers$TOV)

base %>% mutate(ProductivePasses=TotalAst+.44*TotalFTAst+TotalSecAst+TotalPotentAst,
                ProductivePasses_per_game=ProductivePasses/GP,
                ProductivePassRate=ProductivePasses/TotalPassesMade,
                AssistPtsCreatedPerPass=TotalAstPointsCreated/TotalPassesMade) -> new_vars
merge(new_vars,turnovers) -> new_vars

#tables
t1=new_vars %>% select("Player Name"=Player_Name,ProductivePasses_per_game) %>% arrange(desc(ProductivePasses_per_game)) %>% head()
t2=new_vars %>% select("Player Name"=Player_Name,ProductivePassRate) %>% arrange(desc(ProductivePassRate)) %>% head()
t3=new_vars %>% select("Player Name"=Player_Name,AssistPtsCreatedPerPass) %>% arrange(desc(AssistPtsCreatedPerPass)) %>% head()
cbind(t1,breaks,t2,breaks,t3)

#calculating passer rating
new_vars %>% filter(MP >= quantile(new_vars$MP,probs=0.2)) %>%
  mutate(TORate=TOV/TotalPassesMade,
         PP_to_TO_ratio=ProductivePassRate/TORate,
         logPPTO=log(PP_to_TO_ratio),
         tAPCPP=(1+AssistPtsCreatedPerPass),
         Passer_Rating=log((log(1+PP_to_TO_ratio)+(1+ProductivePassRate)^2)^tAPCPP)) -> Filtered_PassRtg


goal_mean=10
goal_sd=2
#transforming the distribution
Filtered_PassRtg$Passer_Rating=round(goal_mean + (Filtered_PassRtg$Passer_Rating-mean(Filtered_PassRtg$Passer_Rating)) * goal_sd/sd(Filtered_PassRtg$Passer_Rating),1)

mean(Filtered_PassRtg$Passer_Rating) #9.99
sd(Filtered_PassRtg$Passer_Rating) #1.99
skewness(Filtered_PassRtg$Passer_Rating) #0.12

ggplot(Filtered_PassRtg,aes(x=Passer_Rating)) + geom_histogram(binwidth = 0.75)

Filtered_PassRtg %>% select(Player_Name,Passer_Rating) %>% filter(Passer_Rating>=quantile(Passer_Rating,.975)) %>% arrange(desc(Passer_Rating))
Filtered_PassRtg %>% select(Player_Name,Passer_Rating) %>% filter(Passer_Rating<=quantile(Passer_Rating,.025)) %>% arrange(desc(Passer_Rating)) 

positions <- read.csv("/Users/lewispipkin/Documents/positions.csv")
positions$Player <- str_extract(positions$Player,"[:alpha:]+[\\.\\-\\']*[:alpha:]+[\\.\\-\\']*[:alpha:]+ [:alpha:]+[\\.\\-\\']*[:alpha:]+[\\.\\-\\']*[:alpha:]+(?=\\\\)")
positions <- distinct(positions)
Filtered_PassRtg$Player_Name <- str_remove(Filtered_PassRtg$Player_Name,"Jr[:punct:]")
Filtered_PassRtg$Player_Name <- str_remove(Filtered_PassRtg$Player_Name,"III")
Filtered_PassRtg$Player_Name <- str_trim(Filtered_PassRtg$Player_Name)
positions$Player <- str_trim(positions$Player)

left_join(Filtered_PassRtg,positions,by=c("Player_Name"="Player")) -> X
X[which(is.na(X$Pos)),]
X$Pos[which(X$Player_Name=="Al Horford")] <- "C"
X$Pos[which(X$Player_Name=="CJ McCollum")] <- "SG"
X$Pos[which(X$Player_Name=="CJ Miles")] <- "SF"
X$Pos[which(X$Player_Name=="D.J. Augustin")] <- "PG"
X$Pos[which(X$Player_Name=="D.J. Wilson")] <- "PF"
X$Pos[which(X$Player_Name=="DeAndre' Bembry")] <- "SG"
X$Pos[which(X$Player_Name=="Devonte' Graham")] <- "PG"
X$Pos[which(X$Player_Name=="Ed Davis")] <- "C"
X$Pos[which(X$Player_Name=="J.J. Barea")] <- "PG"
X$Pos[which(X$Player_Name=="JJ Redick")] <- "SG"
X$Pos[which(X$Player_Name=="Juancho Hernangomez")] <- "PF"
X$Pos[which(X$Player_Name=="Mo Bamba")] <- "C"
X$Pos[which(X$Player_Name=="Nene")] <- "C"
X$Pos[which(X$Player_Name=="OG Anunoby")] <- "SF"
X$Pos[which(X$Player_Name=="Svi Mykhailiuk")] <- "SF"
X$Pos[which(X$Player_Name=="T.J. McConnell")] <- "PG"
X$Pos[which(X$Player_Name=="T.J. Warren")] <- "SF"
X$Pos[which(X$Player_Name=="Taurean Prince")] <- "SF"
X$Pos[which(X$Player_Name=="TJ Leaf")] <- "PF"
X$Pos[which(X$Player_Name=="Wes Iwundu")] <- "SF"

write.csv(X,"fullTable.csv")





