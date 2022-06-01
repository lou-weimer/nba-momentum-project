library(Zelig)
library(MatchIt)
library(cobalt)

all_data<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat.csv")
all_data1<-all_data[all_data$subsequent_tv_to==0, ]


all_data2<-all_data1[, c('subsequent_tv_to', 'subsequent_nontvto' ,'momentum_current_run', 'opposing_momentum_current_run',
                        'momentum_substitutions', 'opposing_momentum_substitutions',
                        "momentum_lookahead_run250m" ,      "momentum_lookahead_run310m"   ,    "momentum_lookahead_run320m"   ,
                        "momentum_lookahead_run330m" ,      "momentum_lookahead_run340m"   ,    "momentum_lookahead_run350m"   ,
                        "momentum_lookahead_run3m"    ,     "momentum_lookahead_run410m"    ,   "momentum_lookahead_run420m"   ,
                        "momentum_lookahead_run430m" ,      "momentum_lookahead_run440m"  ,     "momentum_lookahead_run450m" ,
                        "momentum_lookahead_run4m"
)]

all_data2[is.na(all_data2)] <- 0
all_data3<-all_data2

m.out<-matchit(subsequent_nontvto~momentum_current_run+
                 #opposing_momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=all_data3, method="nearest",
               distance="logit")
bal.tab(m.out)
summary(m.out)

matched_control<-match.data(m.out, 'control')
matched_treated<-match.data(m.out, 'treat')

matched_data<-rbind(matched_control, matched_treated)


matched_data1<-subset(matched_data, matched_data$momentum_lookahead_run3m>(-1))  # ZST: What does this line do?  dim(matched_data) is the same as dim(matched_data1)


glm_run6_3m <- glm(momentum_lookahead_run3m~subsequent_nontvto+momentum_current_run+
                     momentum_substitutions+opposing_momentum_substitutions, data=matched_data1, family="poisson")
summary(glm_run6_3m)

#10 points and more
all_data4<-subset(all_data3, all_data3$momentum_current_run>9)


m.out<-matchit(subsequent_nontvto~momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=all_data4, method="nearest",
               distance="logit")

matched_control<-match.data(m.out, 'control')
matched_treated<-match.data(m.out, 'treat')

matched_data<-rbind(matched_control, matched_treated)
glm_run10_3m <- glm(momentum_lookahead_run3m~subsequent_nontvto+momentum_current_run+
                      momentum_substitutions+opposing_momentum_substitutions, data=matched_data, family="poisson")

#15 points and more

all_data4<-subset(all_data3, all_data3$momentum_current_run>14)


m.out<-matchit(subsequent_nontvto~momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=all_data4, method="nearest",
               distance="logit")

matched_control<-match.data(m.out, 'control')
matched_treated<-match.data(m.out, 'treat')

matched_data<-rbind(matched_control, matched_treated)
glm_run15_3m <- glm(momentum_lookahead_run3m~subsequent_nontvto+momentum_current_run+
                      momentum_substitutions+opposing_momentum_substitutions, data=matched_data, family="poisson")



library(stargazer)
stargazer(glm_run6_3m, 
          glm_run10_3m, 
          glm_run15_3m, 
          type='html', 
          covariate.labels = c("Subsequent Coaches' Timeout", "Current Run Size", "Substitutions by Team on Run",
                               "Substitutions by Team Opposing Run"))
