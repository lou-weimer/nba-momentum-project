library(Zelig)
library(MatchIt)
library(cobalt)
library(dplyr)

home_data<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_homeruns.csv")

home_data$any_subsequent_to<-home_data$subsequent_nontvto+home_data$subsequent_tv_to
home_data<-subset(home_data, home_data$home_run>5)
home_data<-subset(home_data, home_data$any_subsequent_to==1)
home_data$momentum_current_run<-home_data$home_run
home_data$opposing_momentum_current_run<-home_data$away_run
home_data$momentum_substitutions<-home_data$home_subs
home_data$opposing_momentum_substitutions<-home_data$away_subs


home_data$momentum_lookahead_run1m<-home_data$home_points_lookahead_run_1min
home_data$momentum_lookahead_run110m<-home_data$home_points_lookahead_run_110min
home_data$momentum_lookahead_run120m<-home_data$home_points_lookahead_run_120min
home_data$momentum_lookahead_run130m<-home_data$home_points_lookahead_run_130min
home_data$momentum_lookahead_run140m<-home_data$home_points_lookahead_run_140min
home_data$momentum_lookahead_run150m<-home_data$home_points_lookahead_run_150min

home_data$momentum_lookahead_run2m<-home_data$home_points_lookahead_run_2min
home_data$momentum_lookahead_run210m<-home_data$home_points_lookahead_run_210min
home_data$momentum_lookahead_run220m<-home_data$home_points_lookahead_run_220min
home_data$momentum_lookahead_run230m<-home_data$home_points_lookahead_run_230min
home_data$momentum_lookahead_run240m<-home_data$home_points_lookahead_run_240min
home_data$momentum_lookahead_run250m<-home_data$home_points_lookahead_run_250min

home_data$momentum_lookahead_run3m<-home_data$home_points_lookahead_run_3min

home_data$momentum_lookahead_run310m<-home_data$home_points_lookahead_run_310min
home_data$momentum_lookahead_run320m<-home_data$home_points_lookahead_run_320min
home_data$momentum_lookahead_run330m<-home_data$home_points_lookahead_run_330min
home_data$momentum_lookahead_run340m<-home_data$home_points_lookahead_run_340min
home_data$momentum_lookahead_run350m<-home_data$home_points_lookahead_run_350min

home_data$momentum_lookahead_run4m<-home_data$home_points_lookahead_run_4min
home_data$momentum_lookahead_run410m<-home_data$home_points_lookahead_run_410min
home_data$momentum_lookahead_run420m<-home_data$home_points_lookahead_run_420min
home_data$momentum_lookahead_run430m<-home_data$home_points_lookahead_run_430min
home_data$momentum_lookahead_run440m<-home_data$home_points_lookahead_run_440min
home_data$momentum_lookahead_run450m<-home_data$home_points_lookahead_run_450min
home_data$momentum_lookahead_run5m<-home_data$home_points_lookahead_run_5min



home_data$opposing_momentum_lookahead_run3m<-home_data$away_points_lookahead_run_3min
home_data$opposing_momentum_lookahead_run2m<-home_data$away_points_lookahead_run_2min
home_data$opposing_momentum_lookahead_run1m<-home_data$away_points_lookahead_run_1min


home_data$lookahead_3min_diff<-home_data$momentum_lookahead_run3m-home_data$opposing_momentum_lookahead_run3m
home_data$lookahead_2min_diff<-home_data$momentum_lookahead_run2m-home_data$opposing_momentum_lookahead_run2m
home_data$lookahead_1min_diff<-home_data$momentum_lookahead_run1m-home_data$opposing_momentum_lookahead_run1m

#colnames(home_data) <- gsub('home_points','momentum_',colnames(home_data))

away_data<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_awayruns.csv")


away_data$any_subsequent_to<-away_data$subsequent_nontvto+away_data$subsequent_tv_to
away_data<-subset(away_data, away_data$home_run>5)
away_data<-subset(away_data, away_data$any_subsequent_to==1)
away_data$momentum_current_run<-away_data$away_run
away_data$opposing_momentum_current_run<-away_data$home_run
away_data$momentum_substitutions<-away_data$away_subs
away_data$opposing_momentum_substitutions<-away_data$home_subs


#colnames(away_data) <- gsub('home_points','momentum_',colnames(away_data))


away_data$momentum_lookahead_run1m<-away_data$away_points_lookahead_run_1min
away_data$momentum_lookahead_run110m<-away_data$away_points_lookahead_run_110min
away_data$momentum_lookahead_run120m<-away_data$away_points_lookahead_run_120min
away_data$momentum_lookahead_run130m<-away_data$haway_points_lookahead_run_130min
away_data$momentum_lookahead_run140m<-away_data$away_points_lookahead_run_140min
away_data$momentum_lookahead_run150m<-away_data$away_points_lookahead_run_150min

away_data$momentum_lookahead_run2m<-away_data$away_points_lookahead_run_2min
away_data$momentum_lookahead_run210m<-away_data$away_points_lookahead_run_210min
away_data$momentum_lookahead_run220m<-away_data$away_points_lookahead_run_220min
away_data$momentum_lookahead_run230m<-away_data$away_points_lookahead_run_230min
away_data$momentum_lookahead_run240m<-away_data$away_points_lookahead_run_240min
away_data$momentum_lookahead_run250m<-away_data$away_points_lookahead_run_250min

away_data$momentum_lookahead_run3m<-away_data$away_points_lookahead_run_3min

away_data$momentum_lookahead_run310m<-away_data$away_points_lookahead_run_310min
away_data$momentum_lookahead_run320m<-away_data$away_points_lookahead_run_320min
away_data$momentum_lookahead_run330m<-away_data$away_points_lookahead_run_330min
away_data$momentum_lookahead_run340m<-away_data$away_points_lookahead_run_340min
away_data$momentum_lookahead_run350m<-away_data$away_points_lookahead_run_350min

away_data$momentum_lookahead_run4m<-away_data$away_points_lookahead_run_4min
away_data$momentum_lookahead_run410m<-away_data$away_points_lookahead_run_410min
away_data$momentum_lookahead_run420m<-away_data$away_points_lookahead_run_420min
away_data$momentum_lookahead_run430m<-away_data$away_points_lookahead_run_430min
away_data$momentum_lookahead_run440m<-away_data$away_points_lookahead_run_440min
away_data$momentum_lookahead_run450m<-away_data$away_points_lookahead_run_450min
away_data$momentum_lookahead_run5m<-away_data$away_points_lookahead_run5_min
#


away_data$opposing_momentum_lookahead_run3m<-away_data$home_points_lookahead_run_3min
away_data$opposing_momentum_lookahead_run2m<-away_data$home_points_lookahead_run_2min
away_data$opposing_momentum_lookahead_run1m<-away_data$home_points_lookahead_run_1min


away_data$lookahead_3min_diff<-away_data$momentum_lookahead_run3m-away_data$opposing_momentum_lookahead_run3m
away_data$lookahead_2min_diff<-away_data$momentum_lookahead_run2m-away_data$opposing_momentum_lookahead_run2m
away_data$lookahead_1min_diff<-away_data$momentum_lookahead_run1m-away_data$opposing_momentum_lookahead_run1m







all_data<-bind_rows(home_data, away_data)



all_data2<-all_data[, c('subsequent_tv_to', 'subsequent_nontvto' ,'momentum_current_run', 'opposing_momentum_current_run',
                        'momentum_substitutions', 'opposing_momentum_substitutions',
                        "momentum_lookahead_run110m"  ,     "momentum_lookahead_run120m"  ,     "momentum_lookahead_run130m",      
                        "momentum_lookahead_run140m"   ,    "momentum_lookahead_run150m"  ,     "momentum_lookahead_run1m"   ,     
                        "momentum_lookahead_run210m"    ,   "momentum_lookahead_run220m"  ,     "momentum_lookahead_run230m"  ,    
                        "momentum_lookahead_run240m"     ,  "momentum_lookahead_run250m"  ,     "momentum_lookahead_run2m"    ,    
                        "momentum_lookahead_run310m"      , "momentum_lookahead_run320m"  ,     "momentum_lookahead_run330m"  ,    
                        "momentum_lookahead_run340m"     ,  "momentum_lookahead_run350m" ,      "momentum_lookahead_run3m"   ,     
                        "momentum_lookahead_run410m"     ,  "momentum_lookahead_run420m" ,      "momentum_lookahead_run430m" ,     
                        "momentum_lookahead_run440m"     ,  "momentum_lookahead_run450m",       "momentum_lookahead_run4m"   ,     
                        "momentum_lookahead_run5m"  , "opposing_momentum_lookahead_run3m"   ,   "lookahead_3min_diff",
                        "lookahead_2min_diff", "lookahead_1min_diff"
)]



all_data3<-all_data2
all_data3[is.na(all_data3)] <- 0


m.out<-matchit(subsequent_tv_to~momentum_current_run+opposing_momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=all_data3, method="nearest",
               distance="logit")

bal.tab(m.out)
summary(m.out)

matched_control<-match.data(m.out, 'control')
matched_treated<-match.data(m.out, 'treat')

matched_data<-rbind(matched_control, matched_treated)


matched_data1<-subset(matched_data, matched_data$momentum_lookahead_run3m>(-1))






glm_run6<-glm(momentum_lookahead_run3m~subsequent_tv_to+momentum_current_run+
                momentum_substitutions+opposing_momentum_substitutions, data=matched_data1, family="poisson")

summary(glm_run6)
