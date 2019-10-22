rm(list=ls())
#####
setwd("/Users/lewispipkin")
##necessary packages
library(tidyverse)
library(readxl)
library(wrapr)
library(e1071)
library(nbastatR)

dataPassesPlayerBase <- read.csv("passesMaster.csv")
names(dataPassesPlayerBase)
passing_perGame <- read_excel("~/Downloads/passing.xlsx", sheet = "perGame")
passing_Totals <- read_excel("~/Downloads/passing.xlsx", sheet = "Totals")


players_passes <- dataPassesPlayerBase %>% 
  filter(typePass=='made') %>% 
  group_by(namePlayer) %>% 
  summarize(passes=sum(passes),
            fgm=sum(fgm), 
            fga=sum(fga),
            fg2m=sum(fg2m),
            fg2a=sum(fg2a),
            fg3m=sum(fg3m),
            fg3a=sum(fg3a),
            ast=sum(ast))

left_join(passing_perGame, passing_Totals,by="PLAYER_ID") -> t
right_join(t,players_passes,by=c("PLAYER_NAME.x"="namePlayer")) -> tx

names(tx) <- wrapr::qc(Player_ID,Player_Name,Team_ID,Team_Abbrev,GP,
                       W,L,MPG,PassesMade_PG,PassesReceived_PG,APG,
                       FTAst_PG,SecAst_PG,PotentAst_PG,AstPointsCreated_PG,
                       AdjAst_PG,AstToPassPct_PG,AstToPassPctAdj_PG,
                       name,teamid,abbr,gp,w,l,MP,TotalPassesMade,
                       TotalPassesReceived,TotalAst,
                       TotalFTAst,TotalSecAst,TotalPotentAst,
                       TotalAstPointsCreated,TotalAdjAst,TotalAstToPassPct,
                       TotalAdjAstToPassPct,passes,EventualFGM,EventualFGA,
                       EventualFG2M,EventualFG2A,
                       EventualFG3M,EventualFG3A,ast)

tx %>% select(Player_Name,Team_Abbrev,
              GP,MPG,MP,PassesMade_PG,
              TotalPassesMade,APG,
              TotalAst,FTAst_PG,
              TotalFTAst,SecAst_PG,
              TotalSecAst,PotentAst_PG,
              TotalPotentAst,AstPointsCreated_PG,
              TotalAstPointsCreated,
              EventualFGM,EventualFGA,
              EventualFG2M,EventualFG2A,
              EventualFG3M,EventualFG3A) -> base
base %>% mutate(PassesMade_PG=TotalPassesMade/GP,
                APG=TotalAst/GP,
                TotalFTAst=as.numeric(TotalFTAst),
                FTAst_PG=TotalFTAst/GP,
                MPG=MP/GP,
                SecAst_PG=TotalSecAst/GP,
                PotentAst_PG=TotalPotentAst/GP,
                AstPointsCreated_PG=TotalAstPointsCreated/GP) -> base

#adding turnovers
tov <- read.csv("Downloads/Bad Passes w names.csv",stringsAsFactors=F)
tov %>% select(1,3)-> tov
names(tov) <- c("Player_Name","TOV")
turnovers <- tov
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
new_vars %>% filter(MP >= quantile(new_vars$MP,probs=0.2),
                    TOV > 0) %>%
  mutate(TORate=TOV/TotalPassesMade,
         PP_to_TO_ratio=ProductivePassRate/TORate,
         logPPTO=log(PP_to_TO_ratio),
         tAPCPP=(1+AssistPtsCreatedPerPass),
         Value_per_Pass=(log(1+PP_to_TO_ratio)+(1+ProductivePassRate)^2)^tAPCPP,
         Passer_Rating_uT=log(Value_per_Pass*PassesMade_PG)) -> Filtered_PassRtg

#transforming the distribution
adjust_normal = function(col,goal_mean,goal_sd){
  round(goal_mean + (col-mean(col)) * goal_sd/sd(col),1)
}
Filtered_PassRtg$Passer_Rating=adjust_normal(Filtered_PassRtg$Passer_Rating_uT,10,2)

mean(Filtered_PassRtg$Passer_Rating) # 10.00207
sd(Filtered_PassRtg$Passer_Rating) #1.999246
skewness(Filtered_PassRtg$Passer_Rating) #-0.04574408

ggplot(Filtered_PassRtg,aes(x=Passer_Rating)) + geom_histogram(binwidth = 0.75)
hist(Filtered_PassRtg$Passer_Rating)

#bottom 2.5%
Filtered_PassRtg %>% select(Player_Name,Passer_Rating) %>% 
  filter(Passer_Rating>=quantile(Passer_Rating,.975)) %>% arrange(desc(Passer_Rating))
#bottom 2.5%
Filtered_PassRtg %>% select(Player_Name,Passer_Rating) %>% 
  filter(Passer_Rating<=quantile(Passer_Rating,.025)) %>% arrange(desc(Passer_Rating)) 
