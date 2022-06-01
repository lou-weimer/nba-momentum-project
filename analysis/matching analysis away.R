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
rm(all_season)
# 
# all_seasonsub<-subset(all_season, (is.na(all_season$home_points_lookahead_run==FALSE)) & (is.na(all_season$home_run==FALSE)))
season_1617<-away_runs

season_1617_sub<-subset(season_1617, (is.na(season_1617$away_points_lookahead_run)==FALSE) & (is.na(season_1617$away_run)==FALSE)
                        & (is.na(season_1617$home_run)==FALSE))
season_1617_sub1<-season_1617_sub[, c("subsequent_tv_to", "home_run", "home_points_lookahead_run", "away_points_lookahead_run",
                                      "away_run", "subsequent_nontvto", "a1", "a2", "a3", "a4", "a5", "h1", "h2", "h3",
                                      "h4", "h5")]

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

season_1617_sub2<-season_1617_sub1[, c("subsequent_tv_to", "home_run", "home_points_lookahead_run", "away_points_lookahead_run",
                                       "away_run", "subsequent_nontvto", "away_subs", "home_subs")]

season_1617_sub2<-na.omit(season_1617_sub2)
#write.csv(season_1617_sub1, "C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_homeruns.csv")
#write.csv(season_1617_sub1, "C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_awayruns.csv")


#season_1617_sub2<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_homeruns.csv")
season_1617_sub2<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_awayruns.csv")
season_1617_sub2<-season_1617_sub2[, c("subsequent_tv_to", "home_run", "home_points_lookahead_run", "away_points_lookahead_run",
                                       "away_run", "subsequent_nontvto", "away_subs", "home_subs")]


season_1617_sub2<-na.omit(season_1617_sub2)

m.out<-matchit(subsequent_tv_to~home_run+home_subs+away_subs, data=season_1617_sub2, method="nearest", distance="logit"
               )
               #, discard = "hull.control")

#m.out<-matchit(subsequent_tv_to~home_run+home_subs+away_subs, data=season_1617_sub2, method="exact",
#)

matched_away_control<-match.data(m.out, 'control')
matched_away_treated<-match.data(m.out, 'treat')


matched_data<-rbind(matched_away_control, matched_away_treated)
#We are interested in regressing away runs, so by design the home run variable will be entirely 0
z.out <- zelig(away_points_lookahead_run ~ 
                 #home_run+
                 subsequent_tv_to+
                 subsequent_nontvto+away_run+away_subs+home_subs,
               #data = match.data(m.out, "all"), 
               data=matched_data,
                 model = "poisson")
z.out

x.low<-setx(z.out, subsequent_tv_to = 0)
x.high<-setx1(z.out, subsequent_tv_to = 1)

s.out1 <- sim(z.out, x = x.low, x1 = x.high)
plot(s.out1)

s.out.data<-zelig_qi_to_df(s.out1)
s.out.data$subsequent_tv_to<-factor(s.out.data$subsequent_tv_to)


library(ggplot2)

s.out.data$Timeout_Status<-ifelse(s.out.data$subsequent_tv_to==1, "Subsequent TV Timeout", "No subsequent Timeout")

p<-ggplot(s.out.data, aes(expected_value, colour=Timeout_Status, fill=Timeout_Status))+
  geom_density( alpha=.1)+
  theme_minimal()+
  ggtitle("Expected scoring over next three minutes of gameplay \n after TV timeout and after no timeout \n for teams on identical scoring runs prior to timeout")+
  xlab("Subsequent Score Value")

p


plot(s.out1)

df<-match.data(m.out, "all")

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


















#Away Runs:

m.out<-matchit(subsequent_tv_to~away_run+home_subs+away_subs, data=season_1617_sub2, method="nearest",
               distance="mahalanobis")
#, discard = "hull.control")

z.out <- zelig(away_points_lookahead_run~away_run+subsequent_tv_to+subsequent_nontvto+home_run+away_subs+home_subs,
               data = match.data(m.out), 
               model = "poisson")
z.out
# 
# (Intercept)         1.785406   0.005820 306.751   <2e-16
# away_run            0.001212   0.001482   0.817    0.414
# subsequent_tv_to   -0.146407   0.005268 -27.791   <2e-16
# subsequent_nontvto -0.152957   0.011213 -13.641   <2e-16
# home_run                  NA         NA      NA       NA
# away_subs          -0.139367   0.010162 -13.714   <2e-16
# home_subs          -0.078668   0.008190  -9.606   <2e-16
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 43224  on 28157  degrees of freedom
# Residual deviance: 41993  on 28152  degrees of freedom
# AIC: 136716

season_1617_sub3<-subset(season_1617_sub2, season_1617_sub2$away_run>6)
m.out<-matchit(subsequent_tv_to~away_run+home_subs+away_subs, data=season_1617_sub3, method="nearest",
               distance="mahalanobis")
#, discard = "hull.control")

z.out <- zelig(away_points_lookahead_run~away_run+subsequent_tv_to+subsequent_nontvto+home_run+away_subs+home_subs,
               data = match.data(m.out), 
               model = "poisson")
z.out
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)         1.829899   0.057408  31.875  < 2e-16
# away_run           -0.004785   0.006598  -0.725  0.46827
# subsequent_tv_to   -0.138528   0.024013  -5.769 7.98e-09
# subsequent_nontvto -0.112259   0.037790  -2.971  0.00297
# home_run                  NA         NA      NA       NA
# away_subs          -0.056674   0.054598  -1.038  0.29926
# home_subs          -0.086663   0.029593  -2.929  0.00341
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 2177.2  on 1441  degrees of freedom
# Residual deviance: 2130.8  on 1436  degrees of freedom
# AIC: 7007.7
# 
# Number of Fisher Scoring iterations: 4
# 
# Next step: Use 'setx' method

season_1617_sub4<-subset(season_1617_sub2, season_1617_sub2$away_run>9)
m.out<-matchit(subsequent_tv_to~away_run+home_subs+away_subs, data=season_1617_sub4, method="nearest",
               distance="mahalanobis")
#, discard = "hull.control")

z.out <- zelig(away_points_lookahead_run~away_run+subsequent_tv_to+subsequent_nontvto+home_run+away_subs+home_subs,
               data = match.data(m.out), 
               model = "poisson")
z.out
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)         1.99865    0.28687   6.967 3.24e-12
# away_run           -0.01809    0.02298  -0.787    0.431
# subsequent_tv_to   -0.13433    0.07361  -1.825    0.068
# subsequent_nontvto -0.17049    0.10922  -1.561    0.119
# home_run                 NA         NA      NA       NA
# away_subs          -0.26544    0.14806  -1.793    0.073
# home_subs           0.01769    0.08309   0.213    0.831
# 
# (Dispersion parameter for poisson family taken to be 1)
