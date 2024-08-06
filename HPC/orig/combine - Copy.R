result_df_PS <- c()
result_df_BS <- c()
result_df_AD <- c()

for (b in 1:nSim) {
  
  load(file = paste0("./ex1_res_", b, ".rdata")) # loads result
  result_df_PS <- rbind.data.frame( result_df_PS, data_out$result_df_PS )
  result_df_BS <- rbind.data.frame( result_df_BS, data_out$result_df_BS )
  result_df_AD <- rbind.data.frame( result_df_AD, data_out$result_df_AD )

}
 
result_df_PS; result_df_BS; result_df_AD
