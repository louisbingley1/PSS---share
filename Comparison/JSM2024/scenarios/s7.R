#S7

total_data = nSim
starting_seed_val = 1

reference_id = 1
threshold = NA
timepoints = c(0,12,24,48,55)
IR_display = TRUE
delta_adjustment_in = NA

n_patient_ctrl      = n_patient_ctrl
n_patient_expt      = n_patient_expt
n_patient_vector = c(n_patient_ctrl, n_patient_expt)
n_total = sum(n_patient_vector)

mean_control = c(0,0,0,0,0)
mean_treatment = mean_control
mean_list = list(mean_control, mean_treatment)

sigma_ar_vec = c(3, 3)
pacf_list = list(c(0.5), 
                 c(0.5))

beta_list = list(c(1.25, 1.25),
                 c(1.25, 1.25))
covariate_df = NA

# LoE & EE
up_good = "Up" 
p_loe_max = 0.5
z_l_loe = -7
z_u_loe = -2.5
p_ee_max = 0
z_l_ee = 0
z_u_ee = 00

# Admin & AE

p_admin_ctrl = 0.012
p_admin_expt = 0.012
p_admin = c(p_admin_ctrl, p_admin_expt)

prob_ae_ctrl = 0.2
prob_ae_expt = 0.2
prob_ae = c(prob_ae_ctrl, prob_ae_expt)

rate_dc_ae_ctrl = 0.2
rate_dc_ae_expt = 0.2
rate_dc_ae = c(rate_dc_ae_ctrl, rate_dc_ae_expt)

static_output = TRUE

# Add (B.L.)
nSim                = total_data
maxtime             = length(timepoints)-1
seed_vec            = seq(starting_seed_val, starting_seed_val+nSim-1,1)
plot_po             = FALSE