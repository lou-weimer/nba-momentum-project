library(Zelig)
library(MatchIt)
library(cobalt)

home_data<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat_homeruns.csv")
# home_data<-home_data[, c("subsequent_tv_to", "home_run",
#                          "away_points_lookahead_run_110min" ,"away_points_lookahead_run_120min",
#                          "away_points_lookahead_run_130min", "away_points_lookahead_run_140min", "away_points_lookahead_run_150min",
#                          "away_points_lookahead_run_1min",   "away_points_lookahead_run_210min", "away_points_lookahead_run_220min",
#                          "away_points_lookahead_run_230min", "away_points_lookahead_run_240min", "away_points_lookahead_run_250min",
#                          "away_points_lookahead_run_2min",   "away_points_lookahead_run_310min", "away_points_lookahead_run_320min",
#                          "away_points_lookahead_run_330min", "away_points_lookahead_run_340min", "away_points_lookahead_run_350min",
#                          "away_points_lookahead_run_3min",   "away_points_lookahead_run_410min", "away_points_lookahead_run_420min",
#                          "away_points_lookahead_run_430min", "away_points_lookahead_run_440min", "away_points_lookahead_run_450min",
#                          "away_points_lookahead_run_4min",   "away_points_lookahead_run_5min",
#                          "home_points_lookahead_run_110min", "home_points_lookahead_run_120min", "home_points_lookahead_run_130min",
#                          "home_points_lookahead_run_140min", "home_points_lookahead_run_150min", "home_points_lookahead_run_1min"  ,
#                          "home_points_lookahead_run_210min", "home_points_lookahead_run_220min", "home_points_lookahead_run_230min",
#                          "home_points_lookahead_run_240min", "home_points_lookahead_run_250min", "home_points_lookahead_run_2min"  ,
#                          "home_points_lookahead_run_310min", "home_points_lookahead_run_320min", "home_points_lookahead_run_330min",
#                          "home_points_lookahead_run_340min", "home_points_lookahead_run_350min", "home_points_lookahead_run_3min"  ,
#                          "home_points_lookahead_run_410min", "home_points_lookahead_run_420min", "home_points_lookahead_run_430min",
#                          "home_points_lookahead_run_440min", "home_points_lookahead_run_450min", "home_points_lookahead_run_4min"  ,
#                          "home_points_lookahead_run_5min",   "point_gap")]

home_data<-subset(home_data, home_data$home_run>5)
home_data<-subset(home_data, home_data$subsequent_nontvto==0)
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
# away_data<-away_data[, c("subsequent_tv_to", "home_run",
#                                        "away_run", "subsequent_nontvto", "away_subs", "home_subs",
#                          "home_points_lookahead_run_230min", "away_points_lookahead_run_230min", "home_points_lookahead_run_220min",
#                          "away_points_lookahead_run_220min", "home_points_lookahead_run_210min", "away_points_lookahead_run_210min",
#                          "home_points_lookahead_run_2min",   "away_points_lookahead_run_2min",   "home_points_lookahead_run_150min",
#                          "away_points_lookahead_run_150min", "home_points_lookahead_run_140min", "away_points_lookahead_run_140min",
#                          "home_points_lookahead_run_130min", "away_points_lookahead_run_130min", "home_points_lookahead_run_120min",
#                          "away_points_lookahead_run_120min", "home_points_lookahead_run_110min", "away_points_lookahead_run_110min",
#                          "home_points_lookahead_run_1min",   "away_points_lookahead_run_1min",   "home_points_lookahead_run_310min",
#                          "away_points_lookahead_run_310min", "home_points_lookahead_run_320min", "away_points_lookahead_run_320min",
#                          "home_points_lookahead_run_330min", "away_points_lookahead_run_330min", "home_points_lookahead_run_340min",
#                          "away_points_lookahead_run_340min", "home_points_lookahead_run_350min", "away_points_lookahead_run_350min",
#                          "home_points_lookahead_run_4min",   "away_points_lookahead_run_4min",   "home_points_lookahead_run_410min",
#                          "away_points_lookahead_run_410min", "home_points_lookahead_run_420min", "away_points_lookahead_run_420min",
#                          "home_points_lookahead_run_430min", "away_points_lookahead_run_430min", "home_points_lookahead_run_440min",
#                          "away_points_lookahead_run_440min", "home_points_lookahead_run_450min", "away_points_lookahead_run_450min",
#                          "home_points_lookahead_run_5min",   "away_points_lookahead_run_5min",   "point_gap")]

away_data<-subset(away_data, away_data$away_run>5)
away_data<-subset(away_data, away_data$subsequent_nontvto==0)
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



#all_data<-rbind(home_data, away_data)
#lapply(all_data,function(x) { length(which(is.na(x)))})
library(dplyr)
all_data<-bind_rows(home_data, away_data)


#write.csv(all_data, "C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat.csv")

#Kevin and Zach--Start Here

#all_data<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat.csv")


#all_data1<-all_data[ , colSums(is.na(away_data)) == 0]
#all_data1<-all_data1[all_data1$subsequent_nontvto==0, ]

#all_data1<-subset(all_data1, select=-c(outof, reason, steal, converted_y, away_points_lookahead_run_1min))
#sapply(all_data1, function(y) sum(length(which(is.na(y)))))


all_data2<-all_data[, c('subsequent_tv_to', 'momentum_current_run', 'opposing_momentum_current_run',
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
matched_data1$momemtum_run_3_to_5<-matched_data1$momentum_lookahead_run4m-matched_data1$momentum_lookahead_run3m







glm_run6<-glm(momentum_lookahead_run310m~subsequent_tv_to+momentum_current_run+
                momentum_substitutions+opposing_momentum_substitutions, data=matched_data1, family="poisson")


summary(glm_run6)
library(MASS)

glm_run6_nb<-glm.nb(momentum_lookahead_run~subsequent_tv_to+momentum_current_run+
                momentum_substitutions+opposing_momentum_substitutions, data=matched_data1)

summary(glm_run6)

z.out <- zelig(momentum_lookahead_run~subsequent_tv_to+momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=matched_data1, model="poisson")
z.out

s0 <- setx(z.out, subsequent_tv_to = 0)
s1 <- setx(z.out, subsequent_tv_to = 1)
s2 <- sim(z.out, x = s0, x1 = s1)

sim_df<-zelig_qi_to_df(s2)
sim_df$Subsequent_TV_Timeout<-factor(sim_df$subsequent_tv_to)
sim_df$Timeout_Status<-ifelse(sim_df$subsequent_tv_to==1, "Subsequent TV Timeout", "No Subsequent Timeout")
levels(sim_df$Timeout_Status)[levels(sim_df$Timeout_Status)=="Subsequent TV Timeout"] <- "Subsequent TV Timeout"
levels(sim_df$Timeout_Status)[levels(sim_df$Timeout_Status)=="No Subsequent Timeout"] <- "No Subsequent Timeout"
names(sim_df)[names(sim_df)=="Timeout_Status"]  <- "Timeout Status"







library(ggplot2)

p<-ggplot(sim_df, aes(x=expected_value, color=`Timeout Status`, fill=`Timeout Status`))+
  geom_density()+
  theme_minimal()+
  xlab('Expected Points Scored in Subsequent 3 Minutes for Team with Momentum')+
  ylab('Count')+
  ggtitle("Expected Distributions of Points Scored For Team With Momentum \nOver Subsequent 3 Minutes with and without TV Timeout")+
  theme(axis.text=element_text(size=24),
         axis.title=element_text(size=24), plot.title = element_text(size =28, face = "bold"), legend.text=element_text(size=20),
        legend.title=element_text(size=20))

p





runs_10<-subset(all_data, all_data$momentum_current_run>9)

m.out.run10<-matchit(subsequent_tv_to~momentum_current_run+opposing_momentum_current_run+
                       momentum_substitutions+opposing_momentum_substitutions, data=runs_10, method="nearest",
                    distance="logit")

matched_control_run10<-match.data(m.out.run10, 'control')
matched_treated_run10<-match.data(m.out.run10, 'treat')

matched_data_run10<-rbind(matched_control_run10, matched_treated_run10)

glm_run10<-glm(momentum_lookahead_run~subsequent_tv_to+momentum_current_run+
                momentum_substitutions+opposing_momentum_substitutions, data=matched_data_run10, family="poisson")
summary(glm_run10)


glm_run10nb<-glm.nb(momentum_lookahead_run~subsequent_tv_to+momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=matched_data_run10)







runs_15<-subset(all_data, all_data$momentum_current_run>14)

m.out.run15<-matchit(subsequent_tv_to~momentum_current_run+opposing_momentum_current_run+
                        momentum_substitutions+opposing_momentum_substitutions, data=runs_15, method="nearest",
                     distance="logit")

matched_control_run15<-match.data(m.out.run15, 'control')
matched_treated_run15<-match.data(m.out.run15, 'treat')

matched_data_run15<-rbind(matched_control_run15, matched_treated_run15)

matched_data_run15.1<-subset(matched_data_run15, matched_data_run15$momentum_lookahead_run>(-1))

glm_run15<-glm(momentum_lookahead_run~subsequent_tv_to+momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=matched_data_run15, family="poisson")

stargazer(glm_run6, glm_run10, glm_run15, type="html")

glm_run15nb<-glm.nb(momentum_lookahead_run~subsequent_tv_to+momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=matched_data_run15)

stargazer(glm_run6_nb, glm_run10nb, glm_run15nb, type="html")






