NBA playoff matchup/lottery predictions
================
Lewis Pipkin
December 21, 2018

I've done this a few times on my Medium page and I took some time off from it for finals, graduation, moving, a wedding, etc. Since then, however, I figured that it's easier to just scrape basketball-reference (which is discouraged, of course, but thanks to *nbastatR* it is very simple and not at all taxing on their servers) and automate most of these analyses to cut out writing and interpretation. I'm also continuously adapting this model to include different things.

``` r
library(tidyverse)
library(randomForest)
devtools::install_github("abresler/nbastatR")
devtools::install_github("haozhu233/kableExtra")
library(kableExtra)
library(nbastatR)
bref_teams_stats(2013:2019)
```

The hidden ("proprietary"... but admittedly pretty simple, just covering my bases for when I make millions off of this) process that happens in this script is an ensemble of models based on things like net rating and the Four Factors. These models, when combined with preseason expectations, give an imperfect glance at the actual numbers but I feel more confident about what the playoff seedings might be. I'm still working on a more complete (and ideally more accurate) approximation, but this will do for now. More recent information is, of course, more heavily weighted.

So, I've got the numbers, but as of right now the numbers don't always come out to 1230 wins. (They've tended to hover around 1229-1232.) Using a *highly sophisticated process*, I either give wins to a *very* deserving team or take one away from a team who obviously won't win that road back-to-back against `sample(c("New Orleans","Miami","the Globetrotters","the Tri-Cities Blackhawks"),1)` in March, or something like that.

``` r
final_df <- data.frame(Team=all_preds$Team,Conference=all_preds$Conference.x,Wins=all_preds$Wpred)
final_df$Wins <- round(final_df$Wins)
sum(final_df$Wins)
```

    ## [1] 1231

``` r
make_1230()
West <- final_df %>% filter(Conference=="Western") %>% arrange(desc(Wins))
East <- final_df %>% filter(Conference=="Eastern") %>% arrange(desc(Wins))
```

Here are the playoff matchups:

``` r
print_matchups(East)
```

    ## East Playoffs Round 1:  
    ## 
    ## The 1st-seeded 62-win Milwaukee Bucks take on the 8th-seeded 38-win Detroit Pistons.
    ## The 2nd-seeded 60-win Toronto Raptors take on the 7th-seeded 38-win Brooklyn Nets.
    ## The 3rd-seeded 59-win Boston Celtics take on the 6th-seeded 45-win Charlotte Hornets.
    ## The 4th-seeded 53-win Indiana Pacers take on the 5th-seeded 46-win Philadelphia 76ers.

``` r
print_matchups(West)
```

    ## West Playoffs Round 1:  
    ## 
    ## The 1st-seeded 57-win Denver Nuggets take on the 8th-seeded 43-win San Antonio Spurs.
    ## The 2nd-seeded 57-win Oklahoma City Thunder take on the 7th-seeded 43-win Portland Trail Blazers.
    ## The 3rd-seeded 54-win Golden State Warriors take on the 6th-seeded 43-win New Orleans Pelicans.
    ## The 4th-seeded 44-win Los Angeles Lakers take on the 5th-seeded 43-win Houston Rockets.

Net rating is a big factor in this, and as of the time of writing, 7 Western Conference teams have a Net rating between 0.4 and 1.6. As a result, it's very bunched up, so there are a lot of teams hovering slightly above .500 even with preseason expectations taken into account.

Aaaand last but not least, let's simulate the lottery. This is using the new draft odds. If you want them, they're [right here](https://github.com/lewispipkin/NBA/blob/master/data/draftprob.csv)!

``` r
WestLotto <- West[9:15,]
EastLotto <- East[9:15,]
Lotto <- rbind(WestLotto,EastLotto) %>% arrange(Wins)

####

#draft pick probs
draftprob <- read.csv("~/Documents/draftprob.csv")
teams <- sapply(Lotto$Team,as.character)
pick <- c()
pickteam <- c()

for(i in 1:4){
  team <- sample(teams,1,F,prob = draftprob[1,])
  pick[i] <- i
  pickteam[i] <- team
  draftprob <- draftprob[,-1]
  draftprob <- draftprob[-which(team==teams),]
  teams <- teams[-which(team==teams)]
}
lotteryorder <- data.frame(pick=c(pick,5:14),team=c(pickteam,teams))
```

Now let's see where ~~Zion~~ the top pick will call home for the next 4 to 7 years:

``` r
lotto_winner()
```

    ## And the first pick goes to... The Chicago Bulls!

Here is the entire lottery order.

``` r
lotteryorder %>% arrange(desc(pick)) %>% kable
```

<table>
<thead>
<tr>
<th style="text-align:right;">
pick
</th>
<th style="text-align:left;">
team
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Dallas Mavericks
</td>
</tr>
<tr>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Utah Jazz
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Minnesota Timberwolves
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Los Angeles Clippers
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Memphis Grizzlies
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Miami Heat
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Sacramento Kings
</td>
</tr>
<tr>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Washington Wizards
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Cleveland Cavaliers
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Atlanta Hawks
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
New York Knicks
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Phoenix Suns
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Orlando Magic
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Chicago Bulls
</td>
</tr>
</tbody>
</table>
