rm(list=ls())

library(Zelig)
library(MatchIt)

season_0405<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2004-05concat.csv")
season_0506<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2005-06concat.csv")
season_0607<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2006-07concat.csv")
season_0708<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2007-08concat.csv")
season_0809<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2008-09concat.csv")
season_0910<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2009-10concat.csv")
season_1011<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2010-11concat.csv")
season_1112<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2011-12concat.csv")
season_1213<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2012-13concat.csv")
season_1314<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2013-14concat.csv")
season_1415<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2014-15concat.csv")
season_1516<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2015-16concat.csv")
season_1617<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/2016-17concat.csv")

all_season<-rbind(season_0405, season_0506, season_0607, season_0708, season_0809, season_0910,
                  season_1011, season_1112, season_1213, season_1314, season_1415, season_1516,
                  season_1617)
rm(season_0405, season_0506, season_0607, season_0708, season_0809, season_0910, season_1011,
   season_1112, season_1314, season_1415, season_1516, season_1617)

away_runs<-subset(all_season, all_season$away_run>(0))
#
#home_runs<-subset(all_season, all_season$home_run>(0))
# 
# all_seasonsub<-subset(all_season, (is.na(all_season$home_points_lookahead_run==FALSE)) & (is.na(all_season$home_run==FALSE)))

season_1617<-away_runs
#season_1617<-home_runs

#season_1617_sub<-subset(season_1617, (is.na(season_1617$home_points_lookahead_run3min)==FALSE) & (is.na(season_1617$home_run)==FALSE)
#                        & (is.na(season_1617$away_run)==FALSE))


season_1617_sub<-subset(season_1617, !is.na(season_1617$away_points_lookahead_run_3min))
season_1617_sub<-subset(season_1617_sub, !is.na(season_1617$home_run))
season_1617_sub<-subset(season_1617_sub, !is.na(season_1617$away_run))

#season_1617_sub<-season_1617[!is.na(season_1617$home_points_lookahead_run3min), ]
#season_1617_sub<-season_1617_sub[!is.na(season_1617_sub$home_run), ]
#season_1617_sub<-season_1617_sub[!is.na(season_1617_sub$away_run), ]



season_1617_sub1<-season_1617_sub
# [, c("subsequent_tv_to", "home_run", 
#                                       "away_run", "subsequent_nontvto", "a1", "a2", "a3", "a4", "a5", "h1", "h2", "h3",
#                                       "h4", "h5",
#                                       "away_points_lookahead_run_110min" ,"away_points_lookahead_run_120min",
#                                       "away_points_lookahead_run_130min", "away_points_lookahead_run_140min", "away_points_lookahead_run_150min",
#                                        "away_points_lookahead_run_1min",   "away_points_lookahead_run_210min", "away_points_lookahead_run_220min",
#                                        "away_points_lookahead_run_230min", "away_points_lookahead_run_240min", "away_points_lookahead_run_250min",
#                                        "away_points_lookahead_run_2min",   "away_points_lookahead_run_310min", "away_points_lookahead_run_320min",
#                                        "away_points_lookahead_run_330min", "away_points_lookahead_run_340min", "away_points_lookahead_run_350min",
#                                        "away_points_lookahead_run_3min",   "away_points_lookahead_run_410min", "away_points_lookahead_run_420min",
#                                        "away_points_lookahead_run_430min", "away_points_lookahead_run_440min", "away_points_lookahead_run_450min",
#                                        "away_points_lookahead_run_4min",   "away_points_lookahead_run_5min",
#                                        "home_points_lookahead_run_110min", "home_points_lookahead_run_120min", "home_points_lookahead_run_130min",
#                                         "home_points_lookahead_run_140min", "home_points_lookahead_run_150min", "home_points_lookahead_run_1min"  ,
#                                         "home_points_lookahead_run_210min", "home_points_lookahead_run_220min", "home_points_lookahead_run_230min",
#                                         "home_points_lookahead_run_240min", "home_points_lookahead_run_250min", "home_points_lookahead_run_2min"  ,
#                                         "home_points_lookahead_run_310min", "home_points_lookahead_run_320min", "home_points_lookahead_run_330min",
#                                         "home_points_lookahead_run_340min", "home_points_lookahead_run_350min", "home_points_lookahead_run_3min"  ,
#                                         "home_points_lookahead_run_410min", "home_points_lookahead_run_420min", "home_points_lookahead_run_430min",
#                                       "home_points_lookahead_run_440min", "home_points_lookahead_run_450min", "home_points_lookahead_run_4min"  ,
#                                         "home_points_lookahead_run_5min", 
#                                         "point_gap")]

#Away team substiutions
season_1617_sub1$a1<-as.character(season_1617_sub1$a1)
season_1617_sub1$a2<-as.character(season_1617_sub1$a2)
season_1617_sub1$a3<-as.character(season_1617_sub1$a3)
season_1617_sub1$a4<-as.character(season_1617_sub1$a4)
season_1617_sub1$a5<-as.character(season_1617_sub1$a5)


season_1617_sub1$away_subs<-NA
library(dplyr)

n<-nrow(season_1617_sub1)-1

for (i in 1:n){
  season_1617_sub1$away_subs[i]<-5-(sum(c(season_1617_sub1$a1[i], season_1617_sub1$a2[i], season_1617_sub1$a3[i], season_1617_sub1$a4[i], season_1617_sub1$a5[i]) %in% c(season_1617_sub1$a1[i+1], season_1617_sub1$a2[i+1], 
                                                                                                                                                                         season_1617_sub1$a3[i+1], season_1617_sub1$a4[i+1], season_1617_sub1$a5[i+1])))
}


season_1617_sub1$away_subs[season_1617_sub1$away_subs==5]<-0

#Home team substitutions
season_1617_sub1$h1<-as.character(season_1617_sub1$h1)
season_1617_sub1$h2<-as.character(season_1617_sub1$h2)
season_1617_sub1$h3<-as.character(season_1617_sub1$h3)
season_1617_sub1$h4<-as.character(season_1617_sub1$h4)
season_1617_sub1$h5<-as.character(season_1617_sub1$h5)


season_1617_sub1$home_subs<-NA
library(dplyr)

n<-nrow(season_1617_sub1)-1

for (i in 1:n){
  season_1617_sub1$home_subs[i]<-5-(sum(c(season_1617_sub1$h1[i], season_1617_sub1$h2[i], season_1617_sub1$h3[i], season_1617_sub1$h4[i], season_1617_sub1$h5[i]) %in% c(season_1617_sub1$h1[i+1], season_1617_sub1$h2[i+1], 
                                                                                                                                                                         season_1617_sub1$h3[i+1], season_1617_sub1$h4[i+1], season_1617_sub1$h5[i+1])))
}


season_1617_sub1$home_subs[season_1617_sub1$home_subs==5]<-0


#season_1617_sub2<-na.omit(season_1617_sub1)
#write.csv(season_1617_sub1, "C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_homeruns.csv")
write.csv(season_1617_sub1, "C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_awayruns.csv")

season_1617_sub2<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_homeruns.csv")
season_1617_sub2<-season_1617_sub2[, c("subsequent_tv_to", "home_run", "home_points_lookahead_run", "away_points_lookahead_run",
                                       "away_run", "subsequent_nontvto", "away_subs", "home_subs")]


season_1617_sub2<-na.omit(season_1617_sub2)

m.out<-matchit(subsequent_tv_to~home_run+home_subs+away_subs, data=season_1617_sub2, method="nearest",
               distance="logit")
               #, discard = "hull.control")

matched_home_control<-match.data(m.out, 'control')
matched_home_treated<-match.data(m.out, 'treat')

matched_data<-rbind(matched_home_control, matched_home_treated)

# z.out <- zelig(home_points_lookahead_run~home_run+subsequent_tv_to+subsequent_nontvto+away_run+away_subs+home_subs,
#                data = match.data(m.out, group='Matched'), 
#                model = "poisson")

matched_data1<-subset(matched_data, matched_data$home_points_lookahead_run>(-1))

z.out <- zelig(home_points_lookahead_run~home_run+subsequent_tv_to+subsequent_nontvto+away_run+away_subs+home_subs,
               data = matched_data1, 
                 model = "poisson")
z.out
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.5438  -0.9213  -0.0594   0.7128   4.0396  
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)         1.810584   0.005590 323.925   <2e-16
# home_run            0.001779   0.001394   1.277    0.202
# subsequent_tv_to   -0.150316   0.005133 -29.286   <2e-16
# subsequent_nontvto -0.194028   0.010630 -18.252   <2e-16
# away_run                  NA         NA      NA       NA
# away_subs          -0.074919   0.008154  -9.188   <2e-16
# home_subs          -0.122241   0.010132 -12.065   <2e-16
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 43983  on 29093  degrees of freedom
# Residual deviance: 42657  on 29088  degrees of freedom
# AIC: 141423
# 
# Number of Fisher Scoring iterations: 4
# 
# Next step: Use 'setx' method

big_runs<-subset(season_1617_sub2, season_1617_sub2$home_run>10)

m.out<-matchit(subsequent_tv_to~home_run+home_subs+away_subs, data=big_runs, method="nearest",
               distance="mahalanobis")
#, discard = "hull.control")

z.out <- zelig(home_points_lookahead_run~home_run+subsequent_tv_to+subsequent_nontvto+away_run+away_subs+home_subs,
               data = match.data(m.out), 
               model = "poisson")
z.out

# runs >6:

# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)         1.888977   0.047467  39.795  < 2e-16
# home_run           -0.006167   0.005178  -1.191 0.233652
# subsequent_tv_to   -0.183957   0.023876  -7.705 1.31e-14
# subsequent_nontvto -0.130725   0.038418  -3.403 0.000667
# away_run                  NA         NA      NA       NA
# away_subs          -0.104276   0.030710  -3.396 0.000685
# home_subs          -0.184291   0.050655  -3.638 0.000275

#runs>10

# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)         1.886210   0.183935  10.255  < 2e-16
# home_run           -0.002311   0.013443  -0.172   0.8635
# subsequent_tv_to   -0.262913   0.064961  -4.047 5.18e-05
# subsequent_nontvto -0.247125   0.115534  -2.139   0.0324
# away_run                  NA         NA      NA       NA
# away_subs          -0.028176   0.079285  -0.355   0.7223
# home_subs          -0.126198   0.146465  -0.862   0.3889
