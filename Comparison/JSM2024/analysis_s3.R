s3_PS = read.csv("Comparison/JSM2024/outputs/s3_PS.csv",header=T)
s3_BS = read.csv("Comparison/JSM2024/outputs/s3_BS.csv",header=T)
s3_AD = read.csv("Comparison/JSM2024/outputs/s3_AD.csv",header=T)
s3_tb = read.csv("Comparison/JSM2024/outputs/s3_tb.csv",header=T)
head(s3_PS)[,1:10]
View(s3_PS)


#########################
#  BIAS
#########################
{
f_bias= function(true_v, est_v){ bias_v = est_v-true_v; bias=mean(bias_v);return(bias) }

#--------------
# BS
#--------------

# 01/H
N1 = f_bias(true_v = s3_BS$trued_H_01, est_v = s3_BS$ITT_H  )
N2 = f_bias(true_v = s3_BS$trued_H_01, est_v = s3_BS$delta_H )

# 10/B
Q1 = f_bias(true_v = s3_BS$trued_B_10, est_v = s3_BS$ITT_B)
Q2 = f_bias(true_v = s3_BS$trued_B_10, est_v = s3_BS$delta_B )

# 00/I
T1 = f_bias(true_v = s3_BS$trued_I_00, est_v = s3_BS$ITT_I)
T2 = f_bias(true_v = s3_BS$trued_I_00, est_v = s3_BS$delta_I )

# 11/D
W1 = f_bias(true_v = s3_BS$trued_D_11, est_v = s3_BS$ITT_D)
W2 = f_bias(true_v = s3_BS$trued_D_11, est_v = s3_BS$delta_D )

#  *0/IB
Z1 = f_bias(true_v = s3_BS$trued_IB_sp, est_v =s3_BS$ITT_IB)
Z2 = f_bias(true_v = s3_BS$trued_IB_sp, est_v =s3_BS$delta_IB)

#--------------
# AD
#--------------

N3 = N4 = Q3 = Q4 =W3 = W4 = NA

# 00/++
T3 = f_bias(true_v =  s3_AD$trued_00, est_v = s3_AD$adace_mean_plusplusA)
T4 = f_bias(true_v =  s3_AD$trued_00, est_v = s3_AD$adace_mean_plusplusB)

# *0/*+
Z3 = f_bias(true_v =  s3_AD$trued_sp, est_v = s3_AD$adace_mean_starplusA)
Z4 = f_bias(true_v =  s3_AD$trued_sp, est_v = s3_AD$adace_mean_starplusB)

#--------------
# PS
#--------------
W5 = W6 = NA

# 01/N
N5 = f_bias(true_v = s3_PS$trued_N_01, est_v = s3_PS$NACE )
N6 = f_bias(true_v = s3_PS$trued_N_01, est_v = s3_PS$NACE.adj )

# 10/A
Q5 = f_bias(true_v = s3_PS$trued_A_10, est_v = s3_PS$AACE)
Q6 = f_bias(true_v = s3_PS$trued_A_10, est_v = s3_PS$AACE.adj )

# 00/C
T5 = f_bias(true_v = s3_PS$trued_C_00, est_v = s3_PS$CACE )
T6 = f_bias(true_v = s3_PS$trued_C_00, est_v = s3_PS$CACE.adj )

# *0/AC
Z5 = f_bias(true_v = s3_PS$trued_AC_sp, est_v = s3_PS$CAACE)
Z6 = f_bias(true_v = s3_PS$trued_AC_sp, est_v = s3_PS$CAACE.adj)

#--------------
# All bias
#--------------
stratum_01 = c(N1,N2,N3,N4,N5,N6)
stratum_10 = c(Q1,Q2,Q3,Q4,Q5,Q6)
stratum_00 = c(T1,T2,T3,T4,T5,T6)
stratum_11 = c(W1,W2,W3,W4,W5,W6)
stratum_s0 = c(Z1,Z2,Z3,Z4,Z5,Z6)
s3_bias_matrix = cbind.data.frame(stratum_01,stratum_10,stratum_00,stratum_11,stratum_s0)
rownames(s3_bias_matrix)<-c("Bayesian ITT","Bayesian Delta","AdACE methodA","AdACE methodB", "PS Non-Cov-Adj","PS Cov-Adj")
s3_bias_matrix = round(s3_bias_matrix,3)
s3_bias_matrix
#write.csv(s3_bias_matrix,"Comparison/JSM2024/outputs/s3_bias_matrix.csv")
}

#########################
#  COVERAGE PROB
#########################
{
f_cover_prob= function(true_v, l_v,u_v){ ind_v = (true_v>=l_v & true_v<=u_v); cover_prob=mean(ind_v);return(cover_prob) }

#--------------
# BS
#--------------

# 01/H
O1 = f_cover_prob(true_v = s3_BS$trued_H_01, l_v =s3_BS$ITT_lb_H , u_v = s3_BS$ITT_ub_H )
O2 = f_cover_prob(true_v = s3_BS$trued_H_01, l_v =s3_BS$delta_lb_H , u_v = s3_BS$delta_ub_H )

# 10/B
R1 = f_cover_prob(true_v = s3_BS$trued_B_10, l_v = s3_BS$ITT_lb_B , u_v = s3_BS$ITT_ub_B )
R2 = f_cover_prob(true_v = s3_BS$trued_B_10, l_v = s3_BS$delta_lb_B, u_v = s3_BS$delta_ub_B )

# 00/I
U1 = f_cover_prob(true_v = s3_BS$trued_I_00, l_v = s3_BS$ITT_lb_I , u_v = s3_BS$ITT_ub_I )
U2 = f_cover_prob(true_v = s3_BS$trued_I_00, l_v = s3_BS$delta_lb_I, u_v = s3_BS$delta_ub_I  )

# 11/D
X1 = f_cover_prob(true_v = s3_BS$trued_D_11, l_v = s3_BS$ITT_lb_D , u_v = s3_BS$ITT_ub_D  )
X2 = f_cover_prob(true_v = s3_BS$trued_D_11, l_v = s3_BS$delta_lb_D, u_v = s3_BS$delta_ub_D )

#  *0/IB
AA1 = f_cover_prob(true_v = s3_BS$trued_IB_sp, l_v = s3_BS$ITT_lb_IB, u_v =s3_BS$ITT_ub_IB  )
AA2 = f_cover_prob(true_v = s3_BS$trued_IB_sp, l_v = s3_BS$delta_lb_IB, u_v = s3_BS$delta_ub_IB  )

#--------------
# AD
#--------------

O3 = O4 = R3 = R4 =X3 = X4 = NA

s3_AD$lb_ppA = s3_AD$adace_mean_plusplusA - 1.96 * s3_AD$adace_se_plusplusA
s3_AD$ub_ppA = s3_AD$adace_mean_plusplusA + 1.96 * s3_AD$adace_se_plusplusA
s3_AD$lb_ppB = s3_AD$adace_mean_plusplusB - 1.96 * s3_AD$adace_se_plusplusB
s3_AD$ub_ppB = s3_AD$adace_mean_plusplusB + 1.96 * s3_AD$adace_se_plusplusB

s3_AD$lb_spA = s3_AD$adace_mean_starplusA - 1.96 * s3_AD$adace_se_starplusA
s3_AD$ub_spA = s3_AD$adace_mean_starplusA + 1.96 * s3_AD$adace_se_starplusA
s3_AD$lb_spB = s3_AD$adace_mean_starplusB - 1.96 * s3_AD$adace_se_starplusB
s3_AD$ub_spB = s3_AD$adace_mean_starplusB + 1.96 * s3_AD$adace_se_starplusB

# 00/++
U3 = f_cover_prob(true_v =  s3_AD$trued_00, l_v = s3_AD$lb_ppA, u_v =s3_AD$ub_ppA  )
U4 = f_cover_prob(true_v =  s3_AD$trued_00, l_v = s3_AD$lb_ppB, u_v =s3_AD$ub_ppB )

# *0/*+
AA3 = f_cover_prob(true_v =  s3_AD$trued_sp, l_v = s3_AD$lb_spA, u_v = s3_AD$ub_spA )
AA4 = f_cover_prob(true_v =  s3_AD$trued_sp, l_v = s3_AD$lb_spB, u_v = s3_AD$ub_spB   )

#--------------
# PS
#--------------
X5 = X6 = NA

# 01/N
O5 = f_cover_prob(true_v = s3_PS$trued_N_01, l_v = s3_PS$NACE_bs_CI_l, u_v = s3_PS$NACE_bs_CI_u  )
O6 = f_cover_prob(true_v = s3_PS$trued_N_01, l_v = s3_PS$NACE.adj_CI_l, u_v =s3_PS$NACE.adj_CI_u  )

# 10/A
R5 = f_cover_prob(true_v = s3_PS$trued_A_10, l_v =  s3_PS$AACE_CI_l, u_v = s3_PS$AACE_CI_u   )
R6 = f_cover_prob(true_v = s3_PS$trued_A_10, l_v =  s3_PS$AACE.adj_CI_l, u_v =s3_PS$AACE.adj_CI_u  )

# 00/C
U5 = f_cover_prob(true_v = s3_PS$trued_C_00, l_v = s3_PS$CACE_CI_l, u_v = s3_PS$CACE_CI_u  )
U6 = f_cover_prob(true_v = s3_PS$trued_C_00, l_v = s3_PS$CACE.adj_CI_l, u_v =s3_PS$CACE.adj_CI_u  )

# *0/AC
AA5 = f_cover_prob(true_v = s3_PS$trued_AC_sp, l_v = s3_PS$CAACE_CI_l, u_v =s3_PS$CAACE_CI_u  )
AA6 = f_cover_prob(true_v = s3_PS$trued_AC_sp, l_v = s3_PS$CAACE.adj_CI_l, u_v =s3_PS$CAACE.adj_CI_u  )

#--------------
# All bias
#--------------
stratum_01 = c(O1,O2,O3,O4,O5,O6)
stratum_10 = c(R1,R2,R3,R4,R5,R6)
stratum_00 = c(U1,U2,U3,U4,U5,U6)
stratum_11 = c(X1,X2,X3,X4,X5,X6)
stratum_s0 = c(AA1,AA2,AA3,AA4,AA5,AA6)
s3_cover_prob_matrix          = cbind.data.frame(stratum_01,stratum_10,stratum_00,stratum_11,stratum_s0)
rownames(s3_cover_prob_matrix)= c("Bayesian ITT","Bayesian Delta","AdACE methodA","AdACE methodB", "PS Non-Cov-Adj","PS Cov-Adj")
s3_cover_prob_matrix          = round(s3_cover_prob_matrix,3)
s3_cover_prob_matrix
# write.csv(s3_cover_prob_matrix,"Comparison/JSM2024/outputs/s3_cover_prob_matrix.csv")
}

#########################
#  PCP
#########################
{
  #--------------
  # BS
  #--------------
  
  # 01/H
  P1 = mean(s3_BS$PCP_H)
  
  # 10/B
  S1 = mean(s3_BS$PCP_B)
  
  # 00/I
  V1 = mean(s3_BS$PCP_I)
  
  # 11/D
  Y1 = mean(s3_BS$PCP_D)
  
   
  #--------------
  # AD
  #--------------
  P2=S2=V2=Y2=NA
   
  #--------------
  # PS
  #--------------
  Y3=NA
  
  # 01/N
  P3 = mean(s3_PS$NPCP)
  
  # 10/A
  S3 = mean(s3_PS$APCP)
  
  # 00/C
  V3 = mean(s3_PS$CPCP)
  
  #--------------
  # All bias
  #--------------
  stratum_01 = c(P1,P2,P3)
  stratum_10 = c(S1,S2,S3)
  stratum_00 = c(V1,V2,V3)
  stratum_11 = c(Y1,Y2,Y3)
  pcp_matrix = cbind.data.frame(stratum_01,stratum_10,stratum_00,stratum_11)
  rownames(pcp_matrix)<-c("Bayesian","AdACE","PS")
  pcp_matrix = round(pcp_matrix,3)
  pcp_matrix
}

#########################
#  POWER
#########################
{
  f_power= function(true_v, l_v,u_v){ 
    ind_tp2 = (0>=l_v & 0<=u_v); 
    pwr     = 1-mean(ind_tp2);
    return(pwr) }
  
  #--------------
  # BS
  #--------------
  
  # 01/H
  O1 = f_power(true_v = s3_BS$trued_H_01, l_v =s3_BS$ITT_lb_H , u_v = s3_BS$ITT_ub_H )
  O2 = f_power(true_v = s3_BS$trued_H_01, l_v =s3_BS$delta_lb_H , u_v = s3_BS$delta_ub_H )
  
  # 10/B
  R1 = f_power(true_v = s3_BS$trued_B_10, l_v = s3_BS$ITT_lb_B , u_v = s3_BS$ITT_ub_B )
  R2 = f_power(true_v = s3_BS$trued_B_10, l_v = s3_BS$delta_lb_B, u_v = s3_BS$delta_ub_B )
  
  # 00/I
  U1 = f_power(true_v = s3_BS$trued_I_00, l_v = s3_BS$ITT_lb_I , u_v = s3_BS$ITT_ub_I )
  U2 = f_power(true_v = s3_BS$trued_I_00, l_v = s3_BS$delta_lb_I, u_v = s3_BS$delta_ub_I  )
  
  # 11/D
  X1 = f_power(true_v = s3_BS$trued_D_11, l_v = s3_BS$ITT_lb_D , u_v = s3_BS$ITT_ub_D  )
  X2 = f_power(true_v = s3_BS$trued_D_11, l_v = s3_BS$delta_lb_D, u_v = s3_BS$delta_ub_D )
  
  #  *0/IB
  AA1 = f_power(true_v = s3_BS$trued_IB_sp, l_v = s3_BS$ITT_lb_IB, u_v =s3_BS$ITT_ub_IB  )
  AA2 = f_power(true_v = s3_BS$trued_IB_sp, l_v = s3_BS$delta_lb_IB, u_v = s3_BS$delta_ub_IB  )
  
  #--------------
  # AD
  #--------------
  
  O3 = O4 = R3 = R4 =X3 = X4 = NA
  
  s3_AD$lb_ppA = s3_AD$adace_mean_plusplusA - 1.96 * s3_AD$adace_se_plusplusA
  s3_AD$ub_ppA = s3_AD$adace_mean_plusplusA + 1.96 * s3_AD$adace_se_plusplusA
  s3_AD$lb_ppB = s3_AD$adace_mean_plusplusB - 1.96 * s3_AD$adace_se_plusplusB
  s3_AD$ub_ppB = s3_AD$adace_mean_plusplusB + 1.96 * s3_AD$adace_se_plusplusB
  
  s3_AD$lb_spA = s3_AD$adace_mean_starplusA - 1.96 * s3_AD$adace_se_starplusA
  s3_AD$ub_spA = s3_AD$adace_mean_starplusA + 1.96 * s3_AD$adace_se_starplusA
  s3_AD$lb_spB = s3_AD$adace_mean_starplusB - 1.96 * s3_AD$adace_se_starplusB
  s3_AD$ub_spB = s3_AD$adace_mean_starplusB + 1.96 * s3_AD$adace_se_starplusB
  
  # 00/++
  U3 = f_power(true_v =  s3_AD$trued_00, l_v = s3_AD$lb_ppA, u_v =s3_AD$ub_ppA  )
  U4 = f_power(true_v =  s3_AD$trued_00, l_v = s3_AD$lb_ppB, u_v =s3_AD$ub_ppB )
  
  # *0/*+
  AA3 = f_power(true_v =  s3_AD$trued_sp, l_v = s3_AD$lb_spA, u_v = s3_AD$ub_spA )
  AA4 = f_power(true_v =  s3_AD$trued_sp, l_v = s3_AD$lb_spB, u_v = s3_AD$ub_spB   )
  
  #--------------
  # PS
  #--------------
  X5 = X6 = NA
  
  # 01/N
  O5 = f_power(true_v = s3_PS$trued_N_01, l_v = s3_PS$NACE_bs_CI_l, u_v = s3_PS$NACE_bs_CI_u  )
  O6 = f_power(true_v = s3_PS$trued_N_01, l_v = s3_PS$NACE.adj_CI_l, u_v =s3_PS$NACE.adj_CI_u  )
  
  # 10/A
  R5 = f_power(true_v = s3_PS$trued_A_10, l_v =  s3_PS$AACE_CI_l, u_v = s3_PS$AACE_CI_u   )
  R6 = f_power(true_v = s3_PS$trued_A_10, l_v =  s3_PS$AACE.adj_CI_l, u_v =s3_PS$AACE.adj_CI_u  )
  
  # 00/C
  U5 = f_power(true_v = s3_PS$trued_C_00, l_v = s3_PS$CACE_CI_l, u_v = s3_PS$CACE_CI_u  )
  U6 = f_power(true_v = s3_PS$trued_C_00, l_v = s3_PS$CACE.adj_CI_l, u_v =s3_PS$CACE.adj_CI_u  )
  
  # *0/AC
  AA5 = f_power(true_v = s3_PS$trued_AC_sp, l_v = s3_PS$CAACE_CI_l, u_v =s3_PS$CAACE_CI_u  )
  AA6 = f_power(true_v = s3_PS$trued_AC_sp, l_v = s3_PS$CAACE.adj_CI_l, u_v =s3_PS$CAACE.adj_CI_u  )
  
  #--------------
  # All power
  #--------------
  stratum_01 = c(O1,O2,O3,O4,O5,O6)
  stratum_10 = c(R1,R2,R3,R4,R5,R6)
  stratum_00 = c(U1,U2,U3,U4,U5,U6)
  stratum_11 = c(X1,X2,X3,X4,X5,X6)
  stratum_s0 = c(AA1,AA2,AA3,AA4,AA5,AA6)
  s3_power_matrix          = cbind.data.frame(stratum_01,stratum_10,stratum_00,stratum_11,stratum_s0)
  rownames(s3_power_matrix)= c("Bayesian ITT","Bayesian Delta","AdACE methodA","AdACE methodB", "PS Non-Cov-Adj","PS Cov-Adj")
  s3_power_matrix          = round(s3_power_matrix,3)
  s3_power_matrix
#  write.csv(s3_power_matrix,"Comparison/JSM2024/outputs/s3_power_matrix.csv")
}