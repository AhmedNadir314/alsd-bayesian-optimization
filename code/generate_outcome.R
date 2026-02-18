library(cmdstanr)
library(here)
library(tidyverse)
library(fastDummies)

popframe <- read_csv('data/popframe_dhs_census22_adm1.csv')
df_region_index <- read_csv('data/region_index.csv')
df_params <- read.csv('data/posterior_params.csv')
popframe <- popframe |> 
  filter(adm2_name%in%df_region_index$adm2_name) |>
  left_join(df_region_index|> select(-adm1_name),
            by ="adm2_name")

param_means <- colnames(df_params)
b0  <- mean(df_params$b0)
betas <- df_params |> select(starts_with("betas")) |> colMeans() |> unname()
phi   <- df_params |> select(starts_with("phi")) |> colMeans()
theta <- df_params |> select(starts_with("theta")) |> colMeans()
psi   <- df_params |> select(starts_with("psi")) |> colMeans()
nu    <- df_params |> select(starts_with("nu")) |> colMeans()
xi    <- df_params |> select(starts_with("xi")) |> colMeans()
psi <- matrix(psi, nrow = length(xi), 
              ncol = 8)

df_main <- popframe # this should be household level

df_main <- df_main |> 
  mutate(    
    # For binary
    hhh_sex = factor(hhh_sex),
    modality = factor(rep("f2f",nrow(df_main)), levels = c("rtm","f2f")),
    h_water_source_2way = factor(h_water_source_2way, levels=c("other","improved")),
    h_toilet_type_2way = factor(h_toilet_type_2way,levels=c("unimproved","improved")),
    # For multiclass categorical
    hh_size_bucket = factor(hh_size_bucket,levels=c("1-2", "3-4", "5-6", "7+")),
    hhh_edu = factor(hhh_edu,levels=c("preprimary/other", "primary", "secondary","higher"))
  )
df_main <- fastDummies::dummy_cols(df_main, 
                        select_columns = c("h_cell","modality", 
                                           "hhh_sex","hh_size_bucket","hhh_edu",
                                           "h_water_source_2way","h_toilet_type_2way"),
                        remove_first_dummy = F)

variables.interaction <- c("hhh_sex_F",
                           "hh_size_bucket_3-4", "hh_size_bucket_5-6","hh_size_bucket_7+",
                           "hhh_edu_primary","hhh_edu_secondary","hhh_edu_higher",
                           "h_water_source_2way_improved", 
                           "h_toilet_type_2way_improved")
variables.toinclude <- c('h_cell_yes','modality_f2f',variables.interaction)
for (var in variables.interaction){
  int_var_name = paste0('modality_f2f:',var)
  variables.toinclude = c(variables.toinclude,int_var_name)
  df_main[int_var_name] = df_main['modality_f2f'] * df_main[var]
}

N <- nrow(df_main)
adm1_index <- df_main$adm1_index
adm2_index <- df_main$adm2_index
time_index <- rep(40, N) ## corresponds to 2023 December 
psi_vec = vector(mode = "numeric",length=N)
for (i in 1:N){
  psi_vec[i] = psi[time_index[i],adm1_index[i]]
}
X <- as.matrix(df_main[, variables.toinclude])
storage.mode(X) <- "numeric"
eta = b0 + X%*%betas + phi[adm2_index] + theta[adm2_index] + psi_vec + nu[time_index] + xi[time_index]
df_main$prob <- c(1/(1+exp(-eta))) 
rbinom()

df_main$poor_fcs <- rbinom(N,df_main$N_cell,df_main$prob) ## for household level this should be the 
