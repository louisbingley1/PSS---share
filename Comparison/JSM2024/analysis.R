source("Comparison/JSM2024/analysis_s1.R")
source("Comparison/JSM2024/analysis_s2.R")
source("Comparison/JSM2024/analysis_s3.R")
source("Comparison/JSM2024/analysis_s4.R")
rownames = c(rep('s1',6),rep('s2',6),rep('s3',6),rep('s4',6))

#bias
s1_bias_matrix
s2_bias_matrix
s3_bias_matrix
s4_bias_matrix
bias_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_bias_matrix,s2_bias_matrix,s3_bias_matrix,s4_bias_matrix))   ;bias_matrix

# coverage prob
s1_cover_prob_matrix
s2_cover_prob_matrix
s3_cover_prob_matrix
s4_cover_prob_matrix
cover_prob_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_cover_prob_matrix,s2_cover_prob_matrix,s3_cover_prob_matrix,s4_cover_prob_matrix))   ;cover_prob_matrix

# power
s1_power_matrix
s2_power_matrix
s3_power_matrix
s4_power_matrix
# power_matrix <- cbind.data.frame(scenario = rownames,rbind(s1_power_matrix,s2_power_matrix,s3_power_matrix,s4_power_matrix))   ;power_matrix
