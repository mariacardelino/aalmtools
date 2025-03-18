#' Bayesian inference for soil and dust ingestion rates
#' @param blood_pb Numeric vector of blood lead concentrations (μg/dL)
#' @param soil_conc Soil lead concentration (μg/g)
#' @param dust_conc Dust lead concentration (μg/g)
#' @param pop_params List of population parameters (optional)
#' @param n_chains Number of MCMC chains
#' @param n_iter Number of iterations per chain
#' @return List containing posterior distributions and diagnostics
#' @import rjags
#' @import coda
#' @export
estimate_ingestion_rates <- function(blood_pb, soil_conc, dust_conc, 
                                   pop_params = NULL, n_chains = 4, n_iter = 10000) {
    
    # JAGS model specification
    model_string <- "
    model {
        # Likelihood
        for(i in 1:N) {
            blood[i] ~ dnorm(mu[i], tau)
            mu[i] <- beta0 + beta_soil * soil_ir * soil_conc + beta_dust * dust_ir * dust_conc
        }
        
        # Priors for ingestion rates
        soil_ir ~ dlnorm(log(0.05), 1)T(0, 0.2)  # Truncated at typical range
        dust_ir ~ dlnorm(log(0.05), 1)T(0, 0.2)
        
        # Priors for other parameters
        beta0 ~ dnorm(0, 0.001)
        beta_soil ~ dnorm(0, 0.001)
        beta_dust ~ dnorm(0, 0.001)
        tau ~ dgamma(0.001, 0.001)
        
        # Derived quantities
        sigma <- 1/sqrt(tau)
    }
    "
    
    # Prepare data
    jags_data <- list(
        N = length(blood_pb),
        blood = blood_pb,
        soil_conc = soil_conc,
        dust_conc = dust_conc
    )
    
    # Add population parameters if provided
    if(!is.null(pop_params)) {
        jags_data$pop_mean <- pop_params$mean
        jags_data$pop_sd <- pop_params$sd
    }
    
    # Initialize MCMC
    model <- jags.model(textConnection(model_string), 
                       data = jags_data, 
                       n.chains = n_chains)
    
    # Burn-in
    update(model, 1000)
    
    # Sample from posterior
    mcmc_samples <- coda.samples(model,
                                variable.names = c("soil_ir", "dust_ir", "sigma"),
                                n.iter = n_iter)
    
    # Return results
    return(list(
        samples = mcmc_samples,
        diagnostics = gelman.diag(mcmc_samples),
        summary = summary(mcmc_samples)
    ))
}