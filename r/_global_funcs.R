
print_mod_results <- function(mod) {
  
  print(report_model(mod)); cat(".\n\n")
  print(report_performance(mod)); cat("\n")
  print(VarCorr(mod)); cat("\n\n")
  print(report_intercept(mod)); cat("\n\n")
  
  mod_rep <- report(mod) 
  
  # create pretty table of fixed effect coefs
  mod_rep_df <- summary(as.data.frame(mod_rep)) 
  # remove random effect rows as not labelled properly
  mod_rep_df <- mod_rep_df %>% dplyr::filter(!is.na(mod_rep_df$Parameter))
  # remove superfluous columns
  mod_rep_df$df_error <- mod_rep_df$p <- mod_rep_df$df_error <- 
    mod_rep_df$Effects <- mod_rep_df$Group <- mod_rep_df$Component <- 
    mod_rep_df$Std_Coefficient <- NULL
  
  print(mod_rep_df); cat("\n\n")
  
  print(car::Anova(mod, type = "III"))
  
}

