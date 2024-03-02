rm(list = ls())
graphics.off()


packages = cbind("vars", "psych", "tidyverse", "stargazer",
                 "ecm", "ConnectednessApproach", "readxl", 'tikzDevice', 'BVAR', 'devtools')

'for (package in packages) {
  install.packages(package)
}'

for (package in packages) {
  library(package, character.only = TRUE)
}

install_github("JuliusGr01/ConnectednessBVAR")
install_github("GabauerDavid/ConnectednessApproach")
# Data cleaning and transformation ----------------------------------------

setwd("/Users/juliusgross/Desktop/Bachelorarbeit Julius Gross/HA_Metrics")

# Importing data set (holding information on daily returns):

price.data.daily <- read_excel("dataset.xlsx")
price.daily <- subset(price.data.daily, select = -c(date1))

# Using the return values from above into daily volatility measures:

#Calculating daily volatilities (according to Diebold and Yilmaz (2012) (see references of paper):

countries <- c("Nigeria", "Namibia", "Kenya", "SA", "EURO")

for (country in countries) {
  high <- paste0(country, "_H")
  low <- paste0(country, "_L")
  vol_name <- paste0("v.", country)
  price.daily[[vol_name]] <- 100 * asinh(sqrt(252 * 0.361 * log(price.daily[[high]] / price.daily[[low]])^2))
}

# Not annulized and not in %:

for (country in countries) {
  high <- paste0(country, "_H")
  low <- paste0(country, "_L")
  vol_name <- paste0("v_alternative.", country)
  price.daily[[vol_name]] <- 100 * asinh(sqrt(0.361*log(price.daily[[high]]/price.daily[[low]])^2))
}

# Renaming data set for structure
daily.vola <- as.data.frame(price.daily)

# Keeping the environment nice and tidy
remove(price.daily, price.data.daily, packages, high, low, package, vol_name, country)

#In the case of Kenya, it becomes visible, that the data stops being valuable for us as of March 2014 because only one price is indicated from that time backwards.
#Thus, I delete the time span for all observations.

daily.vola <- daily.vola[-c(1:529),]
daily.vola <-  subset(daily.vola, select = -c(Nigeria_H, Nigeria_L,Namibia_H, Namibia_L,Kenya_H,Kenya_L,SA_H, SA_L, EURO_H, EURO_L))
daily.vola.NA <- na.omit(daily.vola)

daily.vola.NA.2 <- daily.vola.NA  #Just a copy as backup
row.names(daily.vola.NA) <- as.Date(daily.vola.NA$Date)
daily.vola.NA$Date <- NULL

# Summary Statistics and Visualisation --------------------------

#TABLE 1: Log annualized volatility summary statistics

summary(daily.vola.NA)


# Plotting each volatility measure over time:

daily.vola.NA.2$Date <- as.Date(daily.vola.NA.2$Date, "%Y-%m-%d")

# Looping through the data to create visualisation of all the volatility measures of the different countries in the sample:

for (country in countries) {
  
  aes_string <- aes_string(x = "Date", y = paste0("v.", country))
  plot.vola.country <- ggplot(daily.vola.NA.2, aes_string) +
    geom_line() +
    scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 20)) +
    labs(title = country,
         x = "",
         y = "")+
    theme(plot.margin=unit(c(0,0,0, 0), "cm")) +
    theme_bw()
  print(plot.vola.country)
}

aic_values <- numeric(14)
bic_values <- numeric(14)

# Assuming daily.vola.NA is your dataset and already loaded
library(vars)

# Initialize vectors to store AIC and BIC values
aic_values <- numeric(14)
bic_values <- numeric(14)

# Loop to fit VAR models for p = 1 to 14 and compute AIC and BIC
for (p in 1:14) {
  model <- vars::VAR(daily.vola.NA, p = p, type = "none")
  
  # Store AIC and BIC values
  aic_values[p] <- AIC(model)
  bic_values[p] <- BIC(model)
}

# Find the minimum AIC and BIC values and their corresponding p values
min_aic_value <- min(aic_values)
min_bic_value <- min(bic_values)
p_min_aic <- which.min(aic_values)
p_min_bic <- which.min(bic_values)

# Print the results
cat("Minimum AIC:", min_aic_value, "at p =", p_min_aic, "\n")
cat("Minimum BIC:", min_bic_value, "at p =", p_min_bic, "\n")

# Based on the BIC, in the following I am using the model specification: p=2, H12


# FUNCTION ----------------------------------------------------------------


TimeConnectedness = function(Phi=NULL, Sigma=NULL, nfore=10, generalized=TRUE, corrected=FALSE, FEVD=NULL) {
  if ((is.null(Phi) || is.null(Phi)) && is.null(fevd)) {
    stop("Either Sigma and Phi need to be given or FEVD")
  }
  if (is.null(fevd)) {
    if (nfore<=0) {
      stop("nfore needs to be a positive integer")
    }
    if (length(dim(Sigma))<=1) {
      stop("Sigma needs to be at least a 2-dimensional matrix")
    }
    if (length(dim(Phi))<=1) {
      stop("Phi needs to be at least a 2-dimensional matrix")
    }
    NAMES = colnames(Sigma)
    k = ncol(Sigma)
    if (length(dim(Phi))==2) {
      Phi = array(Phi, c(nrow(Phi),ncol(Phi),1))
    }
    if (length(dim(Sigma))==2) {
      Sigma = array(Sigma, c(nrow(Sigma),ncol(Sigma),1))
    }
    t = dim(Sigma)[3]
    
    if (is.null(NAMES)) {
      NAMES = 1:k
    }
    date = as.character(as.Date(dimnames(Sigma)[[3]]))
    CT = array(NA, c(k, k, t), dimnames=list(NAMES, NAMES, as.character(date)))
    for (i in 1:t) {
      CT[,,i] = fevd(Phi=Phi[,,i], Sigma=Sigma[,,i], nfore=nfore, generalized=generalized, type="time")$FEVD
    }
  } else {
    CT = fevd
    t = dim(CT)[3]
    NAMES = dimnames(CT)[[1]]
    k = length(NAMES)
    date = as.character(as.Date(dimnames(CT)[[3]]))
  }
  TCI = array(NA, c(t,1), dimnames=list(date, "TCI"))
  NPT = NET = FROM = TO = array(NA, c(t, k), dimnames=list(date, NAMES))
  PCI = NPDC = INFLUENCE = array(NA, c(k, k, t), dimnames=list(NAMES, NAMES, date))
  pb = progress::progress_bar$new(total=t)
  for (i in 1:t) {
    ct = ConnectednessTable(CT[,,i])
    TO[i,] = ct$TO
    FROM[i,] = ct$FROM
    NET[i,] = ct$NET
    NPT[i,] = ct$NPT
    NPDC[,,i] = ct$NPDC
    INFLUENCE[,,i] = ct$INFLUENCE
    PCI[,,i] = ct$PCI
    if (corrected) {
      TCI[i,] = ct$cTCI
    } else {
      TCI[i,] = ct$TCI
    }
    pb$tick()
  }
  TABLE = ConnectednessTable(CT)$TABLE
  config = list(nfore=nfore, approach="Time", generalized=generalized, corrected=corrected)
  return = list(TABLE=TABLE, CT=CT, TCI=TCI, TO=TO, FROM=FROM,
                NET=NET, NPT=NPT, NPDC=NPDC, PCI=PCI, INFLUENCE=INFLUENCE, config=config)
}



ConnectednessApproch = function(x,
                                 nlag=1, 
                                 nfore=10, 
                                 window.size=NULL, 
                                 corrected=FALSE,
                                 model=c("VAR", "QVAR", "LAD", "LASSO", "Ridge", "Elastic", "TVP-VAR", "DCC-GARCH", "BVAR"),
                                 connectedness=c("Time","Frequency", "Joint", "Extended Joint", "R2"),
                                 VAR_config=list(
                                   QVAR=list(tau=0.5, method="fn"),
                                   ElasticNet=list(nfolds=10, alpha=NULL, loss="mae", n_alpha=10),
                                   BVAR = list(lags=nlag, n_draw=10000L, n_burn=5000L, n_thin=1L, priors = "MinnesotaPrior", verbose = TRUE, gamma=0.01),
                                   TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior", gamma=0.01)),
                                 DCC_config=list(standardize=FALSE),
                                 Connectedness_config = list(
                                   TimeConnectedness=list(generalized=TRUE),
                                   FrequencyConnectedness=list(partition=c(pi,pi/2,0), generalized=TRUE, scenario="ABS"),
                                   R2Connectedness=list(method="pearson", decomposition=TRUE, relative=FALSE)
                                 )) {
  if (!is(x, "zoo")) {
    stop("Data needs to be of type 'zoo'")
  }
  model = match.arg(model)
  if (length(connectedness)>1) {
    connectedness = "Time"
  } else {
    connectedness = match.arg(connectedness)
  }
  
  if (nlag<=0 & connectedness!="R2") {
    stop("nlag needs to be a positive integer")
  }
  
  NAMES = colnames(x)
  k = ncol(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  if (connectedness=="R2") {
    if (!Connectedness_config$R2Connectedness$decomposition) {
      nlag = 0
    }
  }
  t = nrow(x)
  if (is.null(window.size)) {
    window.size = nrow(x)
    t0 = 1
  } else {
    window.size = window.size - nlag
    t0 = t - window.size + 1
  }
  
  if (model=="VAR") {
    var_model = VAR
    configuration = list(nlag=nlag)
  } else if (model=="QVAR") {
    var_model = QVAR
    if (is.null(VAR_config$QVAR$method)) {
      VAR_config$QVAR$method = "fn"
    }
    configuration = list(nlag=nlag, tau=VAR_config$QVAR$tau, method=VAR_config$QVAR$method)
  } else if (model=="LAD") {
    var_model = LADVAR
    configuration = list(nlag=nlag)
  } else if (model=="LASSO") {
    var_model = ElasticNetVAR
    configuration = list(nlag=nlag, alpha=1, nfolds=VAR_config$ElasticNet$nfolds, loss=VAR_config$ElasticNet$loss)
  } else if (model=="Ridge") {
    var_model = ElasticNetVAR
    configuration = list(nlag=nlag, alpha=0, nfolds=VAR_config$ElasticNet$nfolds, loss=VAR_config$ElasticNet$loss)
  } else if (model=="Elastic") {
    var_model = ElasticNetVAR
    configuration = list(nlag=nlag, alpha=VAR_config$ElasticNet$alpha, nfolds=VAR_config$ElasticNet$nfolds,
                         loss=VAR_config$ElasticNet$loss, n_alpha=VAR_config$ElasticNet$n_alpha)
  } else if (model=="TVP-VAR") {
    prior_ = VAR_config$TVPVAR$prior
    if (prior_=="MinnesotaPrior") {
      prior = MinnesotaPrior(gamma=VAR_config$TVPVAR$gamma, k=k, nlag=nlag)
    } else if (prior_=="UninformativePrior") {
      prior = UninformativePrior(k=k, nlag=nlag)
    } else if (prior_=="BayesPrior") {
      prior = BayesPrior(x=x, size=window.size, nlag=nlag)
    } else {
      stop("Error Prior choice")
    }
    configuration = list(l=c(VAR_config$TVPVAR$kappa1, VAR_config$TVPVAR$kappa2), nlag=nlag, prior=prior)
    var_model = TVPVAR
    
  } else if (model=="BVAR") {
    prior_ = VAR_config$BVAR$priors
    if (prior_=="MinnesotaPrior") {
      priors = bv_priors("full")
    } else if (prior_=="SingleUnitRoot") {
      add_sur <- function(Y, lags, par) {
        sur <- if(lags == 1) {Y[1, ] / par} else {
          colMeans(Y[1:lags, ]) / par
        }
        Y_sur <- sur
        X_sur <- c(1 / par, rep(sur, lags))
        
        return(list("Y" = Y_sur, "X" = X_sur))
      }
      sur <- bv_dummy(mode = 1, sd = 1, min = 0.0001, max = 50, fun = add_sur)
      
      # Add the new prior
      priors = bv_priors(hyper = "auto", sur = sur)
    } else {
      stop("Error Prior choice")
    }
    lags =  VAR_config$BVAR$lags
    n_draw = VAR_config$BVAR$n_draw
    n_burn = VAR_config$BVAR$n_burn
    n_thin = VAR_config$BVAR$n_thin
    verbose = VAR_config$BVAR$verbose
    priors = priors
    
  } else if (model=="DCC-GARCH") {
    ugarch.spec = rugarch::ugarchspec(mean.model=list(armaOrder=c(0,0)),
                                      variance.model=list(garchOrder=c(1,1), model="sGARCH"),
                                      distribution.model="norm")
    mgarch.spec = rmgarch::dccspec(uspec=rugarch::multispec(replicate(k, ugarch.spec)),
                                   dccOrder=c(1,1), distribution="mvnorm")
  } else {
    stop("Model does not exist")
  }
  
  message("Estimating model")
  if (model=="TVP-VAR") {
    fit = var_model(x, configuration=configuration)
    B_t = fit$beta
    Q_t = fit$sigma
  } else if (model=="BVAR") {
    # Configuration for BVAR
    configuration = VAR_config$BVAR
    # Call the bvar function directly with the specified parameters
    fit = bvar(x, lags = configuration$lags, n_draw = configuration$n_draw, n_burn = configuration$n_burn, n_thin = configuration$n_thin, priors = priors, verbose = configuration$verbose)
    # Since the structure of `fit` might differ, ensure to correctly extract B_t and Q_t based on the actual output of your `bvar` function
    B_t = fit$beta 
    Q_t = fit$sigma
  } else if (model=="DCC-GARCH") {
    fit = rmgarch::dccfit(mgarch.spec, data=x)
    fevd = VFEVD(fit, nfore=nfore, standardize=DCC_config$standardize)$FEVD
    Q_t = fevd
  } else {
    Q_t = array(NA, c(k, k, t0))
    B_t = array(NA, c(k, k*nlag, t0))
    if (connectedness!="R2") {
      for (i in 1:t0) {
        fit = var_model(x[i:(i+window.size-1),], configuration=configuration)
        B_t[,,i] = fit$B
        Q_t[,,i] = fit$Q
        pb$tick()
      }
    }
  }
  DATE = as.character(zoo::index(x))
  date = DATE[(length(DATE)-dim(Q_t)[3]+1):length(DATE)]
  
  if (model=="BVAR") {
    fit$variables = NAMES
    row.names(x) = as.character(date)
  } else {
    dimnames(Q_t)[[1]] = dimnames(Q_t)[[2]] = NAMES
    dimnames(Q_t)[[3]] = as.character(date)
  }

  message("Computing connectedness measures")
  if (connectedness=="Time") {
    generalized = Connectedness_config$TimeConnectedness$generalized
    if (model=="DCC-GARCH") {
      dca = TimeConnectedness(FEVD=fevd, corrected=corrected)
      message("The DCC-GARCH connectedness approach is implemented according to:\n Gabauer, D. (2020). Volatility impulse response analysis for DCC-GARCH models: The role of volatility transmission mechanisms. Journal of Forecasting, 39(5), 788-796.")
    } else {
      dca = TimeConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore,
                              generalized=generalized,
                              corrected=corrected)
      if (model=="VAR" && !generalized) {
        message("The (orthogonalized) VAR connectedness approach is implemented according to:\n Diebold, F. X., & Yilmaz, K. (2009). Measuring financial asset return and volatility spillovers, with application to global equity markets. The Economic Journal, 119(534), 158-171.")
      } else if (model=="VAR" && generalized) {
        message("The (generalized) VAR connectedness approach is implemented according to:\n Diebold, F. X., & Yilmaz, K. (2012). Better to give than to receive: Predictive directional measurement of volatility spillovers. International Journal of Forecasting, 28(1), 57-66.")
      } else if (model=="TVP-VAR") {
        message("The TVP-VAR connectedness approach is implemented according to:\n Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. Journal of Risk and Financial Management, 13(4), 84.")
      } else if (model=="QVAR") {
        message("The QVAR connectedness approach is implemented according to:\n Chatziantoniou, I., Gabauer, D., & Stenfors, A. (2021). Interest rate swaps and the transmission mechanism of monetary policy: A quantile connectedness approach. Economics Letters, 204, 109891.")
      } else if (model=="LASSO" || model=="Ridge" || model=="Elastic") {
        message("The Elastic Net and its restricted models, namely, the LASSO and Ridge VAR connectedness approach are implemented according to:\n Gabauer, D., Gupta, R., Marfatia, H., & Miller, S. (2020). Estimating US Housing Price Network Connectedness: Evidence from Dynamic Elastic Net, Lasso, and Ridge Vector Autoregressive Models (No. 202065). University of Pretoria, Department of Economics.")
      }
    }
  } else if (connectedness=="Frequency") {
    dca = FrequencyConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore,
                                 partition=Connectedness_config$FrequencyConnectedness$partition,
                                 generalized=Connectedness_config$FrequencyConnectedness$generalized,
                                 scenario=Connectedness_config$FrequencyConnectedness$scenario, 
                                 corrected=corrected)
    if (model=="VAR") {
      message("The VAR frequency connectedness approach is implemented according to:\n Barunik, J., & Krehlik, T. (2018). Measuring the frequency dynamics of financial connectedness and systemic risk. Journal of Financial Econometrics, 16(2), 271-296.")
    } else if (model=="TVP-VAR") {
      message("The TVP-VAR frequency connectedness approach is implemented according to:\n Chatziantoniou, I., Gabauer, D., & Gupta, R. (2021). Integration and Risk Transmission in the Market for Crude Oil: A Time-Varying Parameter Frequency Connectedness Approach (No. 202147).")
    } else if (model=="QVAR") {
      message("The QVAR frequency connectedness approach is implemented according to:\n Chatziantoniou, I., Aikins Abakah, E. J., Gabauer, D., & Tiwari, A. K. (2021). Quantile time-frequency price connectedness between green bond, green equity, sustainable investments and clean energy markets: Implications for eco-friendly investors. Available at SSRN 3970746.")
    } else if (model=="BVAR") {
      message("My implementation")
    }
    
  } else if (connectedness=="Joint") {
    dca = JointConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore)
    if (model=="VAR") {
      message("The VAR joint connectedness approach is implemented according to:\n Lastrapes, W. D., & Wiesen, T. F. (2021). The joint spillover index. Economic Modelling, 94, 681-691.")
    }
  } else if (connectedness=="Extended Joint") {
    dca = ExtendedJointConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore)
    if (model=="VAR" || model=="TVP-VAR" || model == "BVAR") {
      message("My implementation")
    } else if (model=="QVAR") {
      message("The QVAR extended joint connectedness approach is implemented according to:\n Cunado, J, Chatziantoniou, I., Gabauer, D., Hardik, M., & de Garcia, F.P. (2022). Dynamic spillovers across precious metals and energy realized volatilities: Evidence from quantile extended joint connectedness measures.")
    }
  } else if (connectedness=="R2") {
    if (Connectedness_config$R2Connectedness$decomposition) {
      if (nlag>0) {
        message("The contemporaneous R2 connectedness approach is implemented according to:\n Naeem, M. A., Chatziantoniou, I., Gabauer, D., & Karim, S. (2023). Measuring the G20 Stock Market Return Transmission Mechanism: Evidence From the R2 Connectedness Approach. International Review of Financial Analysis.\n")
        message("The generalized R2 connectedness approach is implemented according to:\n Balli, F., Balli, H. O., Dang, T. H. N., & Gabauer, D. (2023). Contemporaneous and lagged R2 decomposed connectedness approach: New evidence from the energy futures market. Finance Research Letters, 57, 104168.")
      } else {
        message("The contemporaneous R2 connectedness approach is implemented according to:\n Naeem, M. A., Chatziantoniou, I., Gabauer, D., & Karim, S. (2023). Measuring the G20 Stock Market Return Transmission Mechanism: Evidence From the R2 Connectedness Approach. International Review of Financial Analysis.")
      }
      dca = R2Connectedness(x, nlag=nlag, window.size=window.size, method=Connectedness_config$R2Connectedness$method,
                            relative=Connectedness_config$R2Connectedness$relative, corrected=corrected)
    } else {
      fevd = Q_t
      for (i in 1:t0) {
        ct = cor(x[i:(i+window.size-1),], method=Connectedness_config$R2Connectedness$method)^2
        if (Connectedness_config$R2Connectedness$method=="kendall") {
          ct = sin(0.5*pi*ct)
        }
        fevd[,,i] = ct/rowSums(ct)
      }
      dca = TimeConnectedness(FEVD=fevd, corrected=corrected)
      message("The unconditional connectedness approach is implemented according to:\n Gabauer, D, Chatziantoniou, I., & Stenfors, A. (2023). Model-free connectedness measures.")
    }
  } else {
    stop("Connectedness approach does not exist")
  }
  dca
}




# MAIN-PART ---------------------------------------------------------------

# Here, I want to try to integrate the BVAR approach (using bvar_tools) 

# Dynamic directional spillover measures ----------------------------------------------

daily.vola.NA.zoo <- as.zoo(daily.vola.NA)

connectedness.2.12 = ConnectednessApproch(daily.vola.NA.zoo,
                                           nlag = 2,
                                           nfore = 12,
                                           window.size = 200,
                                           corrected = FALSE,
                                           model = c("BVAR"),
                                           connectedness = c("Time"),
                                           Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                       FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                     scenario = "ABS"))
)

# Spillover Table ------------------------------------------------

spillover.table <- connectedness.2.12$TABLE
stargazer(spillover.table, summary=FALSE, digits=2)
