
# Scenarxlsx# Scenarios:
# 1B. large  trt effect,  scenario B   
# 1C. large  trt effect,  scenario C
# 1D. large  trt effect,  scenario D  
# 2B. modest  trt effect,  scenario B   
# 2C. modest  trt effect,  scenario C
# 2D. modest  trt effect,  scenario D  
# 3B. null  trt effect,  scenario B   
# 3C. null  trt effect,  scenario C
# 3D. null  trt effect,  scenario D
# 11. diff_1 , scenario 1
# 12. diff_1 , scenario 2
# 13. diff_1 , scenario 3
# 14. diff_1 , scenario 4
# 15. diff_1 , scenario 5
# 16. diff_1 , scenario 6


#######
# 1B
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_large_scen_B.r")               
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_1B = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_large_scen_B_0618.csv")

write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_large_scen_B.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_large_scen_B.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_large_scen_B.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_large_scen_B.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_large_scen_B.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_large_scen_B.csv")
write.csv(tb_1B,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_1B.csv")

#######
# 1C
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_large_scen_C.r")             
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_1C = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_large_scen_C.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_large_scen_C.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_large_scen_C.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_large_scen_C.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_large_scen_C.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_large_scen_C.csv")
write.csv(tb_1C,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_1C.csv")

#######
# 1D
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_large_scen_D.r")             
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_1D = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_large_scen_D.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_large_scen_D.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_large_scen_D.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_large_scen_D.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_large_scen_D.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_large_scen_D.csv")
write.csv(tb_1D,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_1D.csv")

#######
# 2B
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_modest_scen_B.r")         
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_2B = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_modest_scen_B.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_modest_scen_B.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_modest_scen_B.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_modest_scen_B.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_modest_scen_B.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_modest_scen_B.csv")
write.csv(tb_2B,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_2B.csv")

#######
# 2C
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_modest_scen_C.r")            
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_2C = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_modest_scen_C.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_modest_scen_C.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_modest_scen_C.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_modest_scen_C.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_modest_scen_C.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_modest_scen_C.csv")
write.csv(tb_2C,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_2C.csv")

#######
# 2D
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_modest_scen_D.r")          
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_2D = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_modest_scen_D.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_modest_scen_D.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_modest_scen_D.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_modest_scen_D.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_modest_scen_D.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_modest_scen_D.csv")
write.csv(tb_2D,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_2D.csv")


#######
# 3B
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_null_scen_B.r")             
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_3B = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_null_scen_B.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_null_scen_B.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_null_scen_B.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_null_scen_B.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_null_scen_B.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_null_scen_B.csv")
write.csv(tb_3B,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_3B.csv")

#######
# 3C
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_null_scen_C.r")            
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_3C = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_null_scen_C.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_null_scen_C.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_null_scen_C.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_null_scen_C.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_null_scen_C.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_null_scen_C.csv")
write.csv(tb_3C,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_3C.csv")

#######
# 3D
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/trt_null_scen_D.r")          
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_3D = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_trt_null_scen_D.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_trt_null_scen_D.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_trt_null_scen_D.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_null_scen_D.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_null_scen_D.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_null_scen_D.csv")
write.csv(tb_3D,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_3D.csv")


#######
# 11
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/diff_1_scen_1.R")              
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_11 = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_diff_1_scen_1.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_diff_1_scen_1.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_diff_1_scen_1.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_1.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_1.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_1.csv")
write.csv(tb_11,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_11.csv")

#######
# 12
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/diff_1_scen_2.R")          
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_12 = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_diff_1_scen_2.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_diff_1_scen_2.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_diff_1_scen_2.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_2.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_2.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_2.csv")
write.csv(tb_12,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_12.csv")

#######
# 13
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/diff_1_scen_3.R")              
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_13 = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_diff_1_scen_3.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_diff_1_scen_3.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_diff_1_scen_3.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_3.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_3.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_3.csv")
write.csv(tb_13,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_13.csv")

#######
# 14
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/diff_1_scen_4.R")               
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_14 = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_diff_1_scen_4.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_diff_1_scen_4.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_diff_1_scen_4.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_4.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_4.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_4.csv")
write.csv(tb_14,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_14.csv")

#######
# 15
#######

# Error in lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,  :   NA/NaN/Inf in 'x' In addition: There were 12 warnings (use warnings() to see them)
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/diff_1_scen_5.R")                  
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_15 = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_diff_1_scen_5.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_diff_1_scen_5.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_diff_1_scen_5.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_5.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_5.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_5.csv")
write.csv(tb_15,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_15.csv")

#######
# 16
#######
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 1.r")
source("Data Simulator/cities simulator/scenarios/diff_1_scen_6.R")                 
source("Comparison/main - multi scenarios/main - cities simulator - fixed part 2.r")
source("Comparison/utility functions/f_comparison_table.r")
tb_16 = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
write.csv(result_df_PS,"Comparison/main - multi scenarios/resultprint/PS_citiesimulator_diff_1_scen_6.csv")
write.csv(result_df_BS,"Comparison/main - multi scenarios/resultprint/BS_citiesimulator_diff_1_scen_6.csv")
write.csv(result_df_AD,"Comparison/main - multi scenarios/resultprint/AD_citiesimulator_diff_1_scen_6.csv")
write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_6.csv")
write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_6.csv")
write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_6.csv")
write.csv(tb_16,"Simulation Study/WIP - Bing/resultprint/cities simulator/tb_16.csv")

