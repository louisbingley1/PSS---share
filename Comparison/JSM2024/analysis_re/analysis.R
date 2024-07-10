library(dplyr)
library(ggplot2)
source("Comparison/JSM2024/analysis_s1.R")
source("Comparison/JSM2024/analysis_s2.R")
source("Comparison/JSM2024/analysis_s3.R")
source("Comparison/JSM2024/analysis_s4.R")
source("Comparison/JSM2024/analysis_s5.R")
source("Comparison/JSM2024/analysis_s6.R")
source("Comparison/JSM2024/analysis_s7.R")
source("Comparison/JSM2024/analysis_s8.R")
rownames = c(rep('s1',6),rep('s2',6),rep('s3',6),rep('s4',6),rep('s5',6),rep('s6',6),rep('s7',6),rep('s8',6))

#bias
s1_bias_matrix
s2_bias_matrix
s3_bias_matrix
s4_bias_matrix
s5_bias_matrix
s6_bias_matrix
s7_bias_matrix
s8_bias_matrix
bias_matrix_ <- cbind.data.frame(scenario = rownames,rbind(s1_bias_matrix,s2_bias_matrix,s3_bias_matrix,s4_bias_matrix,s5_bias_matrix,s6_bias_matrix,s7_bias_matrix,s8_bias_matrix))   ;bias_matrix
bias_matrix  <- bias_matrix_ %>% 
                filter(row.names(bias_matrix_) %in% c("Bayesian Delta","AdACE methodA","PS Non-Cov-Adj","PS Cov-Adj",
                                                     "Bayesian Delta1","AdACE methodA1","PS Non-Cov-Adj1","PS Cov-Adj1",
                                                     "Bayesian Delta2","AdACE methodA2","PS Non-Cov-Adj2","PS Cov-Adj2",
                                                     "Bayesian Delta3","AdACE methodA3","PS Non-Cov-Adj3","PS Cov-Adj3",
                                                     "Bayesian Delta4","AdACE methodA4","PS Non-Cov-Adj4","PS Cov-Adj4",
                                                     "Bayesian Delta5","AdACE methodA5","PS Non-Cov-Adj5","PS Cov-Adj5")) %>%
                filter(scenario %in% c("s1","s2","s3","s4","s5","s6"))%>%
                select(scenario, stratum_00,stratum_s0)
bias_matrix 
write.csv(bias_matrix,"Comparison/JSM2024/results/bias_matrix.csv")

# coverage prob
s1_cover_prob_matrix
s2_cover_prob_matrix
s3_cover_prob_matrix
s4_cover_prob_matrix
s5_cover_prob_matrix
s6_cover_prob_matrix
s7_cover_prob_matrix
s8_cover_prob_matrix
cover_prob_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_cover_prob_matrix,s2_cover_prob_matrix,s3_cover_prob_matrix,s4_cover_prob_matrix,s5_cover_prob_matrix,s6_cover_prob_matrix,s7_cover_prob_matrix,s8_cover_prob_matrix))   ;cover_prob_matrix
write.csv(cover_prob_matrix,"Comparison/JSM2024/results/cover_prob_matrix.csv")

# power
s1_power_matrix
s2_power_matrix
s3_power_matrix
s4_power_matrix
s5_power_matrix
s6_power_matrix
s7_power_matrix
s8_power_matrix
power_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_power_matrix,s2_power_matrix,s3_power_matrix,s4_power_matrix,s5_power_matrix,s6_power_matrix,s7_power_matrix,s8_power_matrix))   ;power_matrix
write.csv(power_matrix,"Comparison/JSM2024/results/power_matrix.csv")


###################
#  bias
###################
source("Comparison/JSM2024/bias_s1.R")
source("Comparison/JSM2024/bias_s2.R")
source("Comparison/JSM2024/bias_s3.R")
source("Comparison/JSM2024/bias_s4.R")
source("Comparison/JSM2024/bias_s5.R")
source("Comparison/JSM2024/bias_s6.R")
source("Comparison/JSM2024/bias_s7.R")
source("Comparison/JSM2024/bias_s8.R")

df_bias = rbind.data.frame(df_1,df_2,df_3,df_4,df_5,df_6) %>% filter(strata %in% c("Strata *0","Strata 00") )

# df_bias = rbind.data.frame(df_bias_1,df_bias_2,df_bias_3,df_bias_4,df_bias_5,df_bias_6,df_bias_7,df_bias_8) %>% filter(strata != "Strata 11" )
# ggplot(df_bias, aes(x=strata, y=bias,color=method)) +  geom_point()+facet_wrap(~scenario) # + geom_text(label=df_bias$method)
ggplot(df_bias, aes(x=scenario, y=bias,color=method)) +  
  geom_boxplot()+
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  facet_wrap(~strata) # + geom_text(label=df_bias$method)

#------------------
# bias boxplot
#------------------


ggplot(df_1 %>% filter(strata %in% c("Strata 00","Strata *0") ),aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 1)")+theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)
ggplot(df_2,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 2)")
ggplot(df_3,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 3)")
ggplot(df_4,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 4)")
ggplot(df_5,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 5)")
ggplot(df_6,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 6)")
ggplot(df_7,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 7)")
ggplot(df_8,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 8)")


##############################
# coverage prob scatter plot
##############################

df_cover_prob = rbind.data.frame(df_cover_prob_1,df_cover_prob_2,df_cover_prob_3,df_cover_prob_4,df_cover_prob_5,df_cover_prob_6,df_cover_prob_7,df_cover_prob_8) %>%
  filter(strata  %in% c("Strata 00","Strata *0")  & scenario != "s7" & scenario != "s8" )
# ggplot(df_cover_prob, aes(x=strata, y=cover_prob,color=method)) +  geom_point()+facet_wrap(~scenario) + theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)# + geom_text(label=df_cover_prob$method)
ggplot(df_cover_prob, aes(x=scenario, y=cover_prob,color=method)) + 
  geom_point(size=4)+
  facet_wrap(~strata)  +
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  ylab("Coverage Probability")+
  xlab("Scenarios")
ggplot(df_cover_prob %>% filter(method != "Bayesian"), aes(x=scenario, y=cover_prob,color=method)) + 
  geom_point(size=4)+
  facet_wrap(~strata)  +
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  ylab("Coverage Probability")+
  xlab("Scenarios")

#-----------------------------
# table
#-----------------------------
df_cover_prob

df_cover_prob_trt_mod_B    = rbind.data.frame(df_cover_prob_1) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(cover_prob_LowEff_B = cover_prob)
df_cover_prob_trt_mod_C    = rbind.data.frame(df_cover_prob_2) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(cover_prob_LowEff_C = cover_prob)
df_cover_prob_trt_large_B  = rbind.data.frame(df_cover_prob_3) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(cover_prob_ModEff_B = cover_prob)
df_cover_prob_trt_large_C  = rbind.data.frame(df_cover_prob_4) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(cover_prob_ModEff_C = cover_prob)
df_cover_prob_trt_vlarge_B = rbind.data.frame(df_cover_prob_5) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(cover_prob_LargeEff_B = cover_prob)
df_cover_prob_trt_vlarge_C = rbind.data.frame(df_cover_prob_6) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(cover_prob_LargeEff_C = cover_prob)
 
m = df_cover_prob_trt_mod_B;m
m = merge(m,df_cover_prob_trt_large_B);m
m = merge(m,df_cover_prob_trt_vlarge_B);m
m = merge(m,df_cover_prob_trt_mod_C);m
m = merge(m,df_cover_prob_trt_large_C);m
m = merge(m,df_cover_prob_trt_vlarge_C);m

write.csv(m,"Comparison/JSM2024/results/cover_prob_tb.csv")


##############################
# power & type I error
##############################

#-----------------------------
# power scatter plot
#-----------------------------
df_power = rbind.data.frame(df_power_1,df_power_2,df_power_3,df_power_4,df_power_5,df_power_6,df_power_7,df_power_8) %>% 
  filter(strata != "Strata 11" & scenario != "s7" & scenario != "s8")
ggplot(df_power, aes(x=scenario, y=power,color=method)) +  
  geom_point(size=4)+facet_wrap(~strata) +
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  ylab("Power")+
  xlab("Scenarios")

#-----------------------------
# alpha scatter plot
#-----------------------------
df_alpha = rbind.data.frame(df_power_1,df_power_2,df_power_3,df_power_4,df_power_5,df_power_6,df_power_7,df_power_8) %>% 
  filter(strata != "Strata 11" & (scenario == "s7" | scenario == "s8"))
ggplot(df_alpha, aes(x=scenario, y=power,color=method)) +  
  geom_point(size=4)+facet_wrap(~strata) +
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  ylab("Type I error")+
  xlab("Scenarios")

#-----------------------------
# table
#-----------------------------
df_power

df_power_trt_mod_B    = rbind.data.frame(df_power_1) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(power_LowEff_B = power)
df_power_trt_mod_C    = rbind.data.frame(df_power_2) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(power_LowEff_C = power)
df_power_trt_large_B  = rbind.data.frame(df_power_3) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(power_ModEff_B = power)
df_power_trt_large_C  = rbind.data.frame(df_power_4) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(power_ModEff_C = power)
df_power_trt_vlarge_B = rbind.data.frame(df_power_5) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(power_LargeEff_B = power)
df_power_trt_vlarge_C = rbind.data.frame(df_power_6) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(power_LargeEff_C = power)
df_alpha_B            = rbind.data.frame(df_power_7) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(alpha_B = power)
df_alpha_C            = rbind.data.frame(df_power_8) %>%  filter(strata != "Strata 11") %>% select(-scenario) %>% rename(alpha_C = power)

m = df_power_trt_mod_B;m
m = merge(m,df_power_trt_large_B);m
m = merge(m,df_power_trt_vlarge_B);m
m = merge(m,df_alpha_B);m
m = merge(m,df_power_trt_mod_C);m
m = merge(m,df_power_trt_large_C);m
m = merge(m,df_power_trt_vlarge_C);m
m = merge(m,df_alpha_C);m

write.csv(m,"Comparison/JSM2024/results/power_alpha_tb.csv")
