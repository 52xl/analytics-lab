##### Modello 6 #####

hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

hyperparameters <- list(
  
  Google.Search_alphas = c(0.5, 3)
  ,Google.Search_gammas = c(0.3, 1)
  ,Google.Search_thetas = c(0, 0.4)
  
  ,programmatic_sociodemo_investments_alphas = c(0.5, 3)
  ,programmatic_sociodemo_investments_gammas = c(0.3, 1)
  ,programmatic_sociodemo_investments_thetas = c(0, 0.4)
  
  ,programmatic_behavioural_investments_alphas = c(0.5, 3)
  ,programmatic_behavioural_investments_gammas = c(0.3, 1)
  ,programmatic_behavioural_investments_thetas = c(0, 0.4)
  
  ,reservation_social_video_investments_alphas = c(0.5, 3)
  ,reservation_social_video_investments_gammas = c(0.3, 1)
  ,reservation_social_video_investments_thetas = c(0, 0.4)
  
  ,reservation_social_display_investments_alphas = c(0.5, 3)
  ,reservation_social_display_investments_gammas = c(0.3, 1)
  ,reservation_social_display_investments_thetas = c(0, 0.4)
  
  ,TV.settimanale_alphas = c(0.5, 3)
  ,TV.settimanale_gammas = c(0.3, 1)
  ,TV.settimanale_thetas = c(0.3, 0.8)  #ho cambiato theta
  
)

InputCollect <- robyn_inputs(
  
  dt_input = ds_strategia        
  ,dt_holidays = dt_prophet_holidays
  ,date_var = "date" # date format must be "2020-01-01"
  ,dep_var = "FB_Value_Sales" # there should be only one dependent variable
  ,dep_var_type = "revenue" # "revenue" or "conversion" (son due keyword: possiamo solo usare queste come target)
  
  ,prophet_vars = c("trend", "season", "holiday") 
  ,prophet_country = "IT" 
  ,prophet_signs = c("positive", "default", "default") 
  
  ,context_vars = c("C1_Value_Sales", "C1_Assortment_number_SKUs", "C1_Weighted_anypromo","FB_Price_per_SU_nonpromo", "FB_Assortment_number_SKUs", "FB_Weighted_anypromo", "lockdown")
  ,context_signs = c("default", "negative", "negative", "negative", "positive", "positive", "negative") 
  
  ,paid_media_spends = c("Google.Search", "programmatic_sociodemo_investments", "programmatic_behavioural_investments", "reservation_social_video_investments",
                         "reservation_social_display_investments","TV.settimanale") 
  
  ,paid_media_vars = c("Google.Search...6", "programmatic_sociodemo_impressions","programmatic_behavioural_impressions",
                       "reservation_social_video_impressions","reservation_social_display_impressions", "TV.Tot.Grp.TV.TOT") # mandatory. Se non ho le impression per alcune variabili (tipo TV), devo comunque metterci qualcosa, e ci metto la spesa.
  
  ,paid_media_signs = c("positive","positive", "positive", "positive","positive", "positive")
  
  # ,organic_vars = c("") # marketing activity without media spend
  ,factor_vars = c("lockdown") # specify which variables in context_vars or organic_vars are factorial 
  
  ,cores = 2 # per settare i core che devono lavorare sul dataset
  
  ,window_start = "2019-06-30" # 2 anni: 2019-06-30, 
  ,window_end = "2021-07-04"
  ,adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
  
  ,iterations = 5000 # 2000 recommended for geom, 4000 for weibull_cdf, 6000 for weibull_pdf
  
  ,intercept_sign = "non_negative" # if I spend 0 on a channel, how much am I gonna generate? Se smetto di fare marketing le mie vendite non diventano negative!
  ,hyperparameters = hyperparameters
  
)


print(InputCollect)

# mettere vostro percorso
robyn_object <- "C:/Users/nicco/Politecnico di Milano/Claudia Decicco - AFB LAB/ROBYN/Modelli Migliori/MyRobin.RDS"

OutputModels <- robyn_run(
  InputCollect = InputCollect # feed in all model specification
  , cores = 2
  #, add_penalty_factor = FALSE # Untested feature. Use with caution.
  , iterations = 2000 # recommended for the dummy dataset
  , trials = 5 # recommended for the dummy dataset
  , outputs = FALSE # outputs = FALSE disables direct model output
)

print(OutputModels)

OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

OutputCollect <- robyn_outputs(
  InputCollect, OutputModels
  , pareto_fronts = 3 # QUANTI REPORT STAMPA CON I GRAFICI VISTI NEL VIDEO? (quindi, quanti modelli?)
  # , calibration_constraint = 0.1 # range c(0.01, 0.1) & default at 0.1
  , csv_out = "pareto" # "pareto" or "all"
  , clusters = TRUE # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  , plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
  , plot_folder = robyn_object # path for plots export
)

print(OutputCollect)

select_model <- "4_725_1"

robyn_save(robyn_object = robyn_object
           , select_model = select_model
           , InputCollect = InputCollect
           , OutputCollect = OutputCollect)

AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect
  , OutputCollect = OutputCollect
  , select_model = select_model
  , scenario = "max_response_expected_spend"
  , channel_constr_low = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
  , channel_constr_up = c(1.3, 1.3, 1.3, 1.3, 1.3, 1.3)
  , expected_spend = 51403*4 # Total spend to be simulated
  , expected_spend_days = 28 # Duration of expected_spend in days
)
print(AllocatorCollect)
AllocatorCollect$dt_optimOut






















##### Modello 7 #####

hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

hyperparameters <- list(
  
  Google.Search_alphas = c(0.5, 3)
  ,Google.Search_gammas = c(0.3, 1)
  ,Google.Search_thetas = c(0, 0.4)
  
  ,social_investment_alphas = c(0.5, 3)
  ,social_investment_gammas = c(0.3, 1)
  ,social_investment_thetas = c(0, 0.4)
  
  ,reservation_investment_alphas = c(0.5, 3)
  ,reservation_investment_gammas = c(0.3, 1)
  ,reservation_investment_thetas = c(0, 0.4)
  
  ,programmatic_investment_alphas = c(0.5, 3)
  ,programmatic_investment_gammas = c(0.3, 1)
  ,programmatic_investment_thetas = c(0, 0.4)
  
  ,TV.settimanale_alphas = c(0.5, 3)
  ,TV.settimanale_gammas = c(0.3, 1)
  ,TV.settimanale_thetas = c(0.3, 0.8)
  
)

InputCollect <- robyn_inputs(
  
  dt_input = ds_tipo_media        
  ,dt_holidays = dt_prophet_holidays
  ,date_var = "date" # date format must be "2020-01-01"
  ,dep_var = "FB_Value_Sales" # there should be only one dependent variable
  ,dep_var_type = "revenue" # "revenue" or "conversion" (son due keyword: possiamo solo usare queste come target)
  
  ,prophet_vars = c("trend", "season", "holiday") 
  ,prophet_country = "IT" 
  ,prophet_signs = c("positive", "default", "default") 
  
  ,context_vars = c("C1_Value_Sales", "C1_Assortment_number_SKUs", "C1_Weighted_anypromo","FB_Price_per_SU_nonpromo", "FB_Assortment_number_SKUs", "FB_Weighted_anypromo", "lockdown")
  ,context_signs = c("default", "negative", "negative", "negative", "positive", "positive", "negative") 
  
  ,paid_media_spends = c("Google.Search", "social_investment", "reservation_investment", "programmatic_investment", "TV.settimanale") 
  
  ,paid_media_vars = c("Google.Search...6", "social_imps","reservation_imps",
                       "programmatic_imps", "TV.Tot.Grp.TV.TOT") # mandatory. Se non ho le impression per alcune variabili (tipo TV), devo comunque metterci qualcosa, e ci metto la spesa.
  
  ,paid_media_signs = c("positive","positive", "positive", "positive","positive")
  
  # ,organic_vars = c("") # marketing activity without media spend
  ,factor_vars = c("lockdown") # specify which variables in context_vars or organic_vars are factorial 
  
  ,cores = 2 # per settare i core che devono lavorare sul dataset
  
  ,window_start = "2019-06-30" # 2 anni: 2019-06-30, 
  ,window_end = "2021-07-04"
  ,adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
  
  ,iterations = 5000 # 2000 recommended for geom, 4000 for weibull_cdf, 6000 for weibull_pdf
  
  ,intercept_sign = "non_negative" # if I spend 0 on a channel, how much am I gonna generate? Se smetto di fare marketing le mie vendite non diventano negative!
  ,hyperparameters = hyperparameters
  
)


print(InputCollect)

# mettere vostro percorso
robyn_object <- "C:/Users/nicco/Politecnico di Milano/Claudia Decicco - AFB LAB/ROBYN/Modelli Migliori/MyRobin.RDS"

OutputModels <- robyn_run(
  InputCollect = InputCollect # feed in all model specification
  , cores = 2
  #, add_penalty_factor = FALSE # Untested feature. Use with caution.
  , iterations = 5000 # recommended for the dummy dataset
  , trials = 10 # recommended for the dummy dataset
  , outputs = FALSE # outputs = FALSE disables direct model output
)

print(OutputModels)

OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

OutputCollect <- robyn_outputs(
  InputCollect, OutputModels
  , pareto_fronts = 3 # QUANTI REPORT STAMPA CON I GRAFICI VISTI NEL VIDEO? (quindi, quanti modelli?)
  # , calibration_constraint = 0.1 # range c(0.01, 0.1) & default at 0.1
  , csv_out = "pareto" # "pareto" or "all"
  , clusters = TRUE # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  , plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
  , plot_folder = robyn_object # path for plots export
)

print(OutputCollect)