library(ggplot2)
library(readxl)
library(MatchIt)



#all_data1<-subset(all_data1, select=-c(outof, reason, steal, converted_y, away_points_lookahead_run_1min))
#sapply(all_data1, function(y) sum(length(which(is.na(y)))))
# all_data2<-all_data[, c('subsequent_tv_to', 'momentum_current_run', 'opposing_momentum_current_run',
#                         'momentum_substitutions', 'opposing_momentum_substitutions',
#                         "momentum_lookahead_run1m", "momentum_lookahead_run110m",
#                         "momentum_lookahead_run120m", "momentum_lookahead_run130m",
#                         "momentum_lookahead_run140m", "momentum_lookahead_run150m",
#                         "momentum_lookahead_run2m", "momentum_lookahead_run210m",
#                         "momentum_lookahead_run220m", "momentum_lookahead_run230m",
#                         "momentum_lookahead_run240m",
#                         "momentum_lookahead_run250m" ,      "momentum_lookahead_run310m"   ,    "momentum_lookahead_run320m"   ,
#                         "momentum_lookahead_run330m" ,      "momentum_lookahead_run340m"   ,    "momentum_lookahead_run350m"   ,
#                         "momentum_lookahead_run3m"    ,     "momentum_lookahead_run410m"    ,   "momentum_lookahead_run420m"   ,
#                         "momentum_lookahead_run430m" ,      "momentum_lookahead_run440m"  ,     "momentum_lookahead_run450m" ,
#                         "momentum_lookahead_run4m"
# )]

all_data<-read.csv("C:/Users/Louis/Desktop/NBA project/allseasons_concat/allseasonconcat.csv")


#all_data1<-all_data[ , colSums(is.na(away_data)) == 0]
all_data1<-all_data[all_data$subsequent_nontvto==0, ]

#all_data1<-subset(all_data1, select=-c(outof, reason, steal, converted_y, away_points_lookahead_run_1min))
#sapply(all_data1, function(y) sum(length(which(is.na(y)))))
all_data2<-all_data[, c('subsequent_tv_to', 'momentum_current_run', 'opposing_momentum_current_run',
                        'momentum_substitutions', 'opposing_momentum_substitutions',
                        "momentum_lookahead_run1m", "momentum_lookahead_run110m",
                                                "momentum_lookahead_run120m", "momentum_lookahead_run130m",
                                                "momentum_lookahead_run140m", "momentum_lookahead_run150m",
                                                "momentum_lookahead_run2m", "momentum_lookahead_run210m",
                                                "momentum_lookahead_run220m", "momentum_lookahead_run230m",
                                                "momentum_lookahead_run240m",
                                                "momentum_lookahead_run250m"  ,   "momentum_lookahead_run310m"   ,    "momentum_lookahead_run320m"   ,
                        "momentum_lookahead_run330m" ,      "momentum_lookahead_run340m"   ,    "momentum_lookahead_run350m"   ,
                        "momentum_lookahead_run3m"    ,     "momentum_lookahead_run410m"    ,   "momentum_lookahead_run420m"   ,
                        "momentum_lookahead_run430m" ,      "momentum_lookahead_run440m"  ,     "momentum_lookahead_run450m" ,
                        "momentum_lookahead_run4m"
)]

all_data2[is.na(all_data2)] <- 0

all_data3<-all_data2


m.out<-matchit(subsequent_tv_to~momentum_current_run+
                 #opposing_momentum_current_run+
                 momentum_substitutions+opposing_momentum_substitutions, data=all_data3, method="nearest",
               distance="logit")

bal.tab(m.out)
summary(m.out)

matched_control<-match.data(m.out, 'control')
matched_treated<-match.data(m.out, 'treat')

matched_data<-rbind(matched_control, matched_treated)


matched_data1<-subset(matched_data, matched_data$momentum_lookahead_run3m>(-1))  # ZST: What does this line do?  dim(matched_data) is the same as dim(matched_data1)


glm_run6_3m <- glm(momentum_lookahead_run5m~subsequent_tv_to+momentum_current_run+
                     momentum_substitutions+opposing_momentum_substitutions, data=matched_data1, family="poisson")
summary(glm_run6_3m)










time_operationalization_table <- read_excel("C:/Users/Louis/Desktop/NBA project/JSA review/time operationalization table.xlsx")

time_operationalization_table<-time_operationalization_table[c(1:21), ]



ggplot(time_operationalization_table, aes(x = Minutes, y = coefficient_of_subsequent_tv_to)) + 
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(legend.title=element_blank(),legend.key=element_blank(), legend.position='top', text=element_text(size=18), 
        plot.margin=unit(c(1,1,.5,.5), 'cm'), axis.title.x=element_text(vjust=-.25), legend.text=element_text(size=18),
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  xlab("Time frame of Subsequent Scoring used for Dependent Variable")+
  ylab("Coefficient of Subsequent TV timeout Variable in Regression")
  #ggtitle("Length of Time Used for Calculating Subsequent Scoring \n and Regression Coefficients of Variable of Interest")


