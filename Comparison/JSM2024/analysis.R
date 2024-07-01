source("Comparison/JSM2024/analysis_s1.R")
source("Comparison/JSM2024/analysis_s2.R")
source("Comparison/JSM2024/analysis_s3.R")
source("Comparison/JSM2024/analysis_s4.R")
source("Comparison/JSM2024/analysis_s5.R")
source("Comparison/JSM2024/analysis_s6.R")
source("Comparison/JSM2024/analysis_s7.R")
source("Comparison/JSM2024/analysis_s8.R")
rownames = c(rep('s1',6),rep('s2',6),rep('s3',6),rep('s4',6))

#bias
s1_bias_matrix
s2_bias_matrix
s3_bias_matrix
s4_bias_matrix
s5_bias_matrix
s6_bias_matrix
s7_bias_matrix
s8_bias_matrix
bias_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_bias_matrix,s2_bias_matrix,s3_bias_matrix,s4_bias_matrix))   ;bias_matrix

# coverage prob
s1_cover_prob_matrix
s2_cover_prob_matrix
s3_cover_prob_matrix
s4_cover_prob_matrix
s5_cover_prob_matrix
s6_cover_prob_matrix
s7_cover_prob_matrix
s8_cover_prob_matrix
cover_prob_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_cover_prob_matrix,s2_cover_prob_matrix,s3_cover_prob_matrix,s4_cover_prob_matrix))   ;cover_prob_matrix

# power
s1_power_matrix
s2_power_matrix
s3_power_matrix
s4_power_matrix
s5_power_matrix
s6_power_matrix
s7_power_matrix
s8_power_matrix
# power_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_power_matrix,s2_power_matrix,s3_power_matrix,s4_power_matrix))   ;power_matrix


#------------------
# bias boxplot
#------------------
source("Comparison/JSM2024/bias_s1.R")
source("Comparison/JSM2024/bias_s2.R")
source("Comparison/JSM2024/bias_s3.R")
source("Comparison/JSM2024/bias_s4.R")
source("Comparison/JSM2024/bias_s5.R")
#source("Comparison/JSM2024/bias_s6.R")
source("Comparison/JSM2024/bias_s7.R")
source("Comparison/JSM2024/bias_s8.R")

ggplot(df_1,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 1)")
ggplot(df_2,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 2)")
ggplot(df_3,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 3)")
ggplot(df_4,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 4)")
ggplot(df_5,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 5)")
ggplot(df_6,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 6)")
ggplot(df_7,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 7)")
ggplot(df_8,aes(strata,bias)) + geom_boxplot(aes(fill=method))+ ylab("bias (Scenario 8)")

#-----------------------------
# coverage prob scatter plot
#-----------------------------

# df_bias = rbind.data.frame(df_bias_1,df_bias_2,df_bias_3,df_bias_4,df_bias_7,df_bias_8)
# ggplot(df_bias, aes(x=strata, y=bias,color=method)) +  geom_point()+facet_wrap(~scenario) # + geom_text(label=df_bias$method)
# ggplot(df_bias, aes(x=scenario, y=bias,color=method)) +  geom_point()+facet_wrap(~strata) # + geom_text(label=df_bias$method)


df_cover_prob = rbind.data.frame(df_cover_prob_1,df_cover_prob_2,df_cover_prob_3,df_cover_prob_4,df_cover_prob_7,df_cover_prob_8) %>%
  filter(strata != "Strata 11"  & scenario != "s7" & scenario != "s8" )
# ggplot(df_cover_prob, aes(x=strata, y=cover_prob,color=method)) +  geom_point()+facet_wrap(~scenario) + theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)# + geom_text(label=df_cover_prob$method)
ggplot(df_cover_prob, aes(x=scenario, y=cover_prob,color=method)) + 
  geom_point(size=4)+facet_wrap(~strata)  +
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  ylab("Coverage Probability")+
  xlab("Scenarios")
ggplot(df_cover_prob %>% filter(method != "Bayesian"), aes(x=scenario, y=cover_prob,color=method)) + 
  geom_point(size=4)+facet_wrap(~strata)  +
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  ylab("Coverage Probability")+
  xlab("Scenarios")


#-----------------------------
# power scatter plot
#-----------------------------
df_power = rbind.data.frame(df_power_1,df_power_2,df_power_3,df_power_4,df_power_7,df_power_8) %>% 
  filter(strata != "Strata 11" & scenario != "s7" & scenario != "s8")
ggplot(df_power, aes(x=scenario, y=power,color=method)) +  
  geom_point(size=4)+facet_wrap(~strata) +
  theme_bw(base_size = 15, base_line_size = .3,base_rect_size  =.3)+
  ylab("Power")+
  xlab("Scenarios")

df_power = rbind.data.frame(df_power_1,df_power_2,df_power_3,df_power_4,df_power_7,df_power_8) %>% 
  filter(strata != "Strata 11")
df_power_alpha_scenB= df_power %>% filter(scenario %in% c("s1","s3","s5","s7"))
df_power_alpha_scenC= df_power %>% filter(scenario %in% c("s2","s4","s6","s8"))
