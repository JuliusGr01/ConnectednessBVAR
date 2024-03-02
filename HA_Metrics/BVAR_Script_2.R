rm(list = ls())
graphics.off()


packages = cbind("vars", "psych", "tidyverse", "stargazer",
                 "ecm", "ConnectednessApproach", "readxl", 'tikzDevice', 'BVAR', 'devtools', 'grid')

'for (package in packages) {
  install.packages(package)
}'

for (package in packages) {
  library(package, character.only = TRUE)
}

#install_github("JuliusGr01/ConnectednessBVAR")



# FUNCTION ----------------------------------------------------------------

ConnectednessApprach = function(x,
                                nlag=1, 
                                nfore=10, 
                                window.size=NULL, 
                                corrected=FALSE,
                                model=c("VAR", "QVAR", "LAD", "LASSO", "Ridge", "Elastic", "TVP-VAR", "DCC-GARCH"),
                                connectedness=c("Time","Frequency", "Joint", "Extended Joint", "R2"),
                                VAR_config=list(
                                  QVAR=list(tau=0.5, method="fn"),
                                  ElasticNet=list(nfolds=10, alpha=NULL, loss="mae", n_alpha=10),
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
    B_t = fit$B_t
    Q_t = fit$Q_t
  } else if (model=="DCC-GARCH") {
    fit = rmgarch::dccfit(mgarch.spec, data=x)
    fevd = VFEVD(fit, nfore=nfore, standardize=DCC_config$standardize)$FEVD
    Q_t = fevd
  } else {
    Q_t = array(NA, c(k, k, t0))
    B_t = array(NA, c(k, k*nlag, t0))
    if (connectedness!="R2") {
      pb = progress_bar$new(total=t0)
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
  dimnames(Q_t)[[1]] = dimnames(Q_t)[[2]] = NAMES
  dimnames(Q_t)[[3]] = as.character(date)
  
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
    }
  } else if (connectedness=="Joint") {
    dca = JointConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore)
    if (model=="VAR") {
      message("The VAR joint connectedness approach is implemented according to:\n Lastrapes, W. D., & Wiesen, T. F. (2021). The joint spillover index. Economic Modelling, 94, 681-691.")
    }
  } else if (connectedness=="Extended Joint") {
    dca = ExtendedJointConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore)
    if (model=="VAR" || model=="TVP-VAR") {
      message("The VAR and TVP-VAR extended joint connectedness approach is implemented according to:\n Balcilar, M., Gabauer, D., & Umar, Z. (2021). Crude Oil futures contracts and commodity markets: New evidence from a TVP-VAR extended joint connectedness approach. Resources Policy, 73, 102219.")
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

# Based on the BIC, in the following I am using the model specification: p=2, H12

# MAIN-PART ---------------------------------------------------------------

# Here, I want to try to integrate the BVAR approach (using bvar_tools) 

# Dynamic directional spillover measures ----------------------------------------------

daily.vola.NA.zoo <- as.zoo(daily.vola.NA)

daily.vola.NA.zoo.2 <- as.zoo(daily.vola.NA.2)

x.date <- as.Date(daily.vola.NA.2$Date)

daily.vola.NA.zoo <- zoo(daily.vola.NA, x.date)

daily.vola.NA.2$Date <- as.Date(daily.vola.NA.2$Date, "%Y-%m-%d")

#I now graph the data for each country:

# Define a list of countries and their respective details
countries <- list(
  Nigeria = list(name = "Nigeria", column = "v.Nigeria", title = "(a) Nigeria - NSE All Share"),
  Namibia = list(name = "Namibia", column = "v.Namibia", title = "(b) Namibia - FTSE NSX Overall", filter = TRUE),
  Kenya = list(name = "Kenya", column = "v.Kenya", title = "(c) Kenya - FTSE NSE Kenya"),
  SA = list(name = "South Africa", column = "v.SA", title = "(d) South Africa - South Africa Top 40"),
  EURO = list(name = "Europe", column = "v.EURO", title = "(e) Europe - EUROSTOXX 50")
)

# Loop through each country to create and save plots
for (country in names(countries)) {
  # For countries with specific filtering conditions (e.g., Namibia)
  if (!is.null(countries[[country]]$filter) && countries[[country]]$filter == TRUE) {
    data <- subset(daily.vola.NA.2, get(countries[[country]]$column) <= 300)
  } else {
    data <- daily.vola.NA.2
  }
  
  plot <- ggplot(data, aes_string(x = "Date", y = countries[[country]]$column)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) +
    labs(title = countries[[country]]$title, x = "", y = "") +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme_bw()
  
  print(plot)
}


# Lag length: BIC and AIC -------------------------------------------------

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


# Spillover Table ------------------------------------------------


connectedness.2.12.BVAR.BayesPrior = ConnectednessApproach(daily.vola.NA.zoo,
                                                           nlag = 2,
                                                           nfore = 12,
                                                           window.size = 200,
                                                           corrected = FALSE,
                                                           model = c("TVP-VAR"),
                                                           connectedness = c("Time"),
                                                           VAR_config = list(QVAR = list(tau = 0.5, method = "fn"), ElasticNet = list(nfolds = 10,
                                                                                                                                      alpha = NULL, loss = "mae", n_alpha = 10), TVPVAR = list(kappa1 = 0.99, kappa2 =
                                                                                                                                                                                                 0.99, prior = "BayesPrior", gamma = 0.01)),
                                                           Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                                       FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                                     scenario = "ABS"))
)

connectedness.2.12.BVAR.MinnesotaPrior = ConnectednessApproach(daily.vola.NA.zoo,
                                                               nlag = 2,
                                                               nfore = 12,
                                                               window.size = 200,
                                                               corrected = FALSE,
                                                               model = c("TVP-VAR"),
                                                               connectedness = c("Time"),
                                                               VAR_config = list(QVAR = list(tau = 0.5, method = "fn"), ElasticNet = list(nfolds = 10,
                                                                                                                                          alpha = NULL, loss = "mae", n_alpha = 10), TVPVAR = list(kappa1 = 0.99, kappa2 =
                                                                                                                                                                                                     0.99, prior = "MinnesotaPrior", gamma = 0.01)),
                                                               Connectedness_config = list(TimeConnectedness = list(generalized = TRUE),
                                                                                           FrequencyConnectedness = list(partition = c(pi, pi/2, 0), generalized = TRUE,
                                                                                                                         scenario = "ABS"))
)


spillover.table <- connectedness.2.12.BVAR.BayesPrior$TABLE
stargazer(spillover.table, summary=FALSE, digits=2)


#Computing spillover index by hand:
mean(connectedness.2.12.BVAR.BayesPrior$TCI)


# Figure 2 ----------------------------------------------------------------

#Preparation and 2 different versions

data.tci <- as.data.frame(connectedness.2.12.BVAR.BayesPrior$TCI)

'tikz(file = "YYY/TotalvolaspillDY.tex", standAlone=T)
PlotTCI(connectedness.2.12.BVAR.BayesPrior, ylim=c(0,70))
dev.off()'

data.tci$date <- row.names(data.tci)
data.tci <- as.data.frame(data.tci)
data.tci$date <- as.Date(data.tci$date)

tci.3.12 = ggplot(data.tci, aes(x = date, y = TCI)) +
  geom_line() +
  labs(title = "",
       x = "Date",
       y = "") 

#Final: Used Figure 2

'tikz(file = "YYY/Totalvolaspillfig2.tex", standAlone=T)
tci.3.12
dev.off()'


#Figure 3
#Preparation:
data.to  <- as.data.frame(connectedness.2.12.BVAR.BayesPrior$TO)
data.to$date <- row.names(data.to)
data.to$date <- as.Date(data.to$date)

# Define a list of countries and their corresponding plot margins
countries <- list(
  Nigeria = c("v.Nigeria", unit(c(-0.25,0,-0.25,1), "cm")),
  Namibia = c("v.Namibia", unit(c(-0.25,1,-0.25,0), "cm")),
  Kenya = c("v.Kenya", unit(c(-0.25,0,-0.25,1), "cm")),
  SA = c("v.SA", unit(c(-0.25,1,-0.25,0), "cm")),
  EURO = c("v.EURO", unit(c(-0.25,0,-0.25,1), "cm"))
)

# Loop through the list to create and save each plot
for(country in names(countries)) {
  variable_name <- countries[[country]][1]
  margin_setting <- countries[[country]][2]
  
  plot_name <- paste("to.3.12", country, sep = ".")
  file_name <- paste("YYY/Vola", tolower(country), ".tex", sep = "")
  
  plot <- ggplot(data.to, aes(x = date, y = get(variable_name))) +
    geom_line() +
    scale_y_continuous(limits = c(0, 130), breaks = seq(0, 120, 20)) +
    labs(title = paste("(a)", country),
         x = "",
         y = "") +
    theme(plot.margin = margin_setting) +
    theme_bw()
}


# Figure 3 ----------------------------------------------------------------

'tikz(file = "YYY/directional.to.newest.tex", standAlone=T)
to <- to.3.12.Nigeria+ to.3.12.Namibia+to.3.12.Kenya+to.3.12.SA+to.3.12.EU+to.empty + plot_layout(ncol = 2) + theme(panel.spacing = unit(c(-0.5,0-0.5,0), "lines")) + theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  theme(strip.background = element_blank()) +
  theme(plot.background = element_blank()) +
  theme(strip.text = element_blank()) 
dev.off()
'

'tikz(file = "YYY/directional.to.newest.tex", standAlone=T)
grid.draw(to)
dev.off()'


'tikz(file = "YYY/Totalvolaspillfig2.tex", standAlone=T)
tci.3.12
dev.off()'

#Alternatively:
'tikz(file = "YYY/directional.to.tex", standAlone=T)
to.3.12.Nigeria+ to.3.12.Namibia + to.3.12.Kenya + to.3.12.SA + to.3.12.EU + plot_layout(ncol = 2)
dev.off()'


#Figure 4
#Preparation

data.from  <- as.data.frame(connectedness.2.12.BVAR.BayesPrior$FROM)
data.from$date <- row.names(data.from)
data.from$date <- as.Date(data.from$date)

countries <- c("Nigeria", "Namibia", "Kenya", "SA", "EURO")
plot_list <- list()

for (country in countries) {
  data_subset <- if (country == "Namibia") {
    subset(data.from, v.Namibia <= 300)
  } else {
    data.from
  }
  
  plot <- ggplot(data_subset, aes(x = date, y = get(paste("v", country, sep = ".")))) +
    geom_line() +
    scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
    labs(title = paste(country), x = "", y = "") +
    theme(plot.margin = unit(c(-0.25, ifelse(country == "Namibia" || country == "SA", 1, 0), -0.25, ifelse(country == "Namibia" || country == "SA", 0, 1)), "cm")) +
    theme_bw()
  
  plot_list[[country]] <- plot
}



#Figure 5: Net-spillovers
#Preparation

data.net  <- as.data.frame(connectedness.2.12.BVAR.BayesPrior$NET)
data.net$date <- row.names(data.net)
data.net$date <- as.Date(data.net$date)


net_plot_list <- list()

for (country in countries) {
  plot <- ggplot(data.net, aes(x = date, y = get(paste("v", country, sep = ".")))) +
    geom_area() +
    scale_y_continuous(limits = c(-40, 86), breaks = seq(-40, 80, 20)) +
    labs(title = paste(country), x = "", y = "") +
    theme(plot.margin = unit(c(-0.25, ifelse(country == "Namibia" || country == "SA", 1, 0), -0.25, ifelse(country == "Namibia" || country == "SA", 0, 1)), "cm")) +
    theme_bw()
  
  net_plot_list[[country]] <- plot
}


summary(data.net$v.Nigeria)
summary(data.net$v.Namibia)
summary(data.net$v.Kenya)
summary(data.net$v.SA)
summary(data.net$v.EURO)


#Figure 6: Pairwise Net-spillovers
#Preparation

'tikz(file = "YYY/net.pairwise.tex", standAlone=T)
PlotNPDC(connectedness.2.12.BVAR.BayesPrior)
dev.off()
'

data.net.pairwise  <- as.data.frame(connectedness.2.12.BVAR.BayesPrior$NPDC)

date = as.Date(dimnames(connectedness.2.12.BVAR.BayesPrior$NPDC)[[3]])

data.net.pairwise <- transpose(data.net.pairwise)
data.net$date <- row.names(data.net)
data.net$date <- as.Date(data.net$date)

net.3.12.Nigeria = ggplot(data.net, aes(x = date, y = v.Nigeria)) +
  geom_area() +
  scale_y_continuous(limits = c(-62,63), breaks = seq(-60, 60, 10)) +
  labs(title = "(a) Nigeria",
       x = "Date",
       y = "")

net.3.12.Namibia = ggplot(data.net, aes(x = date, y = v.Namibia)) +
  geom_area() +
  scale_y_continuous(limits = c(-62,63), breaks = seq(-60, 60, 10)) +
  labs(title = "(b) Namibia",
       x = "Date",
       y = "")

net.3.12.Kenya = ggplot(data.net, aes(x = date, y = v.Kenya)) +
  geom_area() +
  scale_y_continuous(limits = c(-62,63), breaks = seq(-40, 60, 10)) +
  labs(title = "(c) Kenya",
       x = "Date",
       y = "")

net.3.12.SA = ggplot(data.net, aes(x = date, y = v.SA)) +
  geom_area() +
  scale_y_continuous(limits = c(-62,63), breaks = seq(-40, 60, 10)) +
  labs(title = "(d) South Africa",
       x = "Date",
       y = "")

net.3.12.EU = ggplot(data.net, aes(x = date, y = v.EURO)) +
  geom_area() +
  scale_y_continuous(limits = c(-25,86), breaks = seq(-20, 80, 20)) +
  labs(title = "(e) Europe",
       x = "Date",
       y = "")

'tikz(file = "YYY/net.tex", standAlone=T)
net.3.12.Nigeria+ net.3.12.Namibia + net.3.12.Kenya + net.3.12.SA + net.3.12.EU + plot_layout(ncol = 2)
dev.off()
'

# 2.3 Quarterly values for BVAR(2) with N=12 ----------------------------------

#Even though is is something that we would be looking for, the format is not practical for further calculations. The format is, at least with my knowledge not changeable.
#Unfortunately, the package I am using does not include a possibility to get a bidirectional spillover table. Thus, I am dependent on another package:



#The panel data is computed with quarterly data. Hence I need quartelized bidrectional spillovers between Europe and each African country.


Pairwise.Connectedness <- connectedness.2.12.BVAR.BayesPrior$PCI
Pairwise.Connectedness.1 <- as.data.frame.table(Pairwise.Connectedness) 

Pairwise.Connectedness.EU.Nigeria <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.Nigeria")
Pairwise.Connectedness.EU.Namibia <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.Namibia")
Pairwise.Connectedness.EU.Kenya <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.Kenya")
Pairwise.Connectedness.EU.SA <- subset(Pairwise.Connectedness.1, Pairwise.Connectedness.1$Var1 =="v.EURO" & Pairwise.Connectedness.1$Var2 =="v.SA")


#Now, we need quarterly data instead of daily data. Thus, I compute the quarterly averages over the daily values:

library(tidyverse)
library(lubridate)

Pairwise.Connectedness.EU.Nigeria$Var3 <- as.Date(Pairwise.Connectedness.EU.Nigeria$Var3)

Pairwise.Quarterly.Connectedness.EU.Nigeria <- Pairwise.Connectedness.EU.Nigeria %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.Nigeria$`@REF_AREA` <- "NG"

Pairwise.Quarterly.Connectedness.EU.Nigeria$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.Nigeria$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.Nigeria)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.Nigeria)[4] <- "@TIME_PERIOD"

Pairwise.Quarterly.Connectedness.EU.Nigeria$year_quarter <- NULL
#Quarterly spillovers for Nigeria


Pairwise.Connectedness.EU.Namibia$Var3 <- as.Date(Pairwise.Connectedness.EU.Namibia$Var3)

Pairwise.Quarterly.Connectedness.EU.Namibia <- Pairwise.Connectedness.EU.Namibia %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.Namibia$`@REF_AREA` <- "NA"

Pairwise.Quarterly.Connectedness.EU.Namibia$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.Namibia$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.Namibia)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.Namibia)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Connectedness.EU.Namibia$year_quarter <- NULL
#Quarterly spillovers for Namibia

Pairwise.Connectedness.EU.Kenya$Var3 <- as.Date(Pairwise.Connectedness.EU.Kenya$Var3)

Pairwise.Quarterly.Connectedness.EU.Kenya <- Pairwise.Connectedness.EU.Kenya %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.Kenya$`@REF_AREA` <- "KE"

Pairwise.Quarterly.Connectedness.EU.Kenya$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.Kenya$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.Kenya)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.Kenya)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Connectedness.EU.Kenya$year_quarter <- NULL
#Quarterly spillovers for Kenya 

Pairwise.Connectedness.EU.SA$Var3 <- as.Date(Pairwise.Connectedness.EU.SA$Var3)

Pairwise.Quarterly.Connectedness.EU.SA <- Pairwise.Connectedness.EU.SA %>% mutate(year_quarter = as.yearqtr(Var3, with_year = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(Freq = mean(Freq))

Pairwise.Quarterly.Connectedness.EU.SA$`@REF_AREA` <- "ZA"

Pairwise.Quarterly.Connectedness.EU.SA$Quarter <- format(Pairwise.Quarterly.Connectedness.EU.SA$year_quarter, format = "%Y-Q%q")

names(Pairwise.Quarterly.Connectedness.EU.SA)[2] <- "Spillover"
names(Pairwise.Quarterly.Connectedness.EU.SA)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Connectedness.EU.SA$year_quarter <- NULL
#Quarterly spillovers for South Africa


BIDIRECTIONAL.EU.TO.AFRICA.QUARTERLY.3.12 <- rbind(Pairwise.Quarterly.Connectedness.EU.Kenya, Pairwise.Quarterly.Connectedness.EU.Namibia, Pairwise.Quarterly.Connectedness.EU.Nigeria, Pairwise.Quarterly.Connectedness.EU.SA)

#REDO: I ACKNOWLEDGE THAT NET-PAIRWISE SPILLOVERS ARE NOT EXACTLY WHAT I WANT. I RATHER WANT DYNAMIC SPILLOVERS OF EACH DAY INDICATING 
#WHICH AMOUNT OF SPILLOVERS OCCURED FROM EUROPE TO THE SPECIFIC COUNTRY. HOW MUCH SPILLOVERS OCCURED FROM THE AFRICAN COUNTRY TO THE EUROPEAN FINANCIAL MARKET IS NOT OF INTEREST (PORTRAYED IN NET-SPILLOVERS):


melted.CT.EU <- reshape2::melt(connectedness.2.12.BVAR.BayesPrior$CT[1:4,5,,drop=FALSE])
names(melted.CT.EU)[4] <- "Spillover"
names(melted.CT.EU)[3] <- "@TIME_PERIOD"
melted.CT.EU$Var2 <- NULL
names(melted.CT.EU)[1] <- "@REF_AREA"


melted.CT.EU$VERSUCH = ifelse(melted.CT.EU$`@REF_AREA` %in% "v.Kenya" ,"KE",
                              ifelse(melted.CT.EU$`@REF_AREA` %in% "v.Namibia", "NA",
                                     ifelse(melted.CT.EU$`@REF_AREA` %in% "v.SA", "ZA",
                                            ifelse(melted.CT.EU$`@REF_AREA` %in% "v.Nigeria", "NG",0))))


melted.CT.EU$`@REF_AREA` <- NULL
names(melted.CT.EU)[3] <- "@REF_AREA"
melted.CT.EU$`@TIME_PERIOD` <- as.Date(melted.CT.EU$`@TIME_PERIOD`)

Pairwise.Quarterly.Spillovers <- melted.CT.EU %>% mutate(year_quarter = as.yearqtr(`@TIME_PERIOD`, with_year = TRUE)) %>%
  group_by(year_quarter, `@REF_AREA`) %>%
  summarize(Spillover = mean(Spillover))

Pairwise.Quarterly.Spillovers$Quarter <- format(Pairwise.Quarterly.Spillovers$year_quarter, format = "%Y-Q%q")
names(Pairwise.Quarterly.Spillovers)[4] <- "@TIME_PERIOD"
Pairwise.Quarterly.Spillovers$year_quarter <- NULL
BIDIRECTIONAL.EU.TO.AFRICA.QUARTERLY.3.12 <- Pairwise.Quarterly.Spillovers



# 2.4 Preliminary Test: CHECK FOR STATIONARITY --------------------------------------------------


acf.nigeria <- acf(daily.vola.NA$v.Nigeria,lag.max = length(daily.vola.NA$v.Nigeria),
                   xlab = "lag #", ylab = 'ACF',main='Nigeria ', plot = FALSE)

acf.namibia <- acf(daily.vola.NA$v.Namibia,lag.max = length(daily.vola.NA$v.Namibia),
                   xlab = "lag #", ylab = 'ACF',main='Namibia ', plot = FALSE)
acf.namibia <- plot(acf.namibia)

acf.kenya <- acf(daily.vola.NA$v.Kenya,lag.max = length(daily.vola.NA$v.Kenya),
                 xlab = "lag #", ylab = 'ACF',main='Kenya', plot = FALSE)
acf.kenya <- plot(acf.kenya)

acf.sa <- acf(daily.vola.NA$v.SA,lag.max = length(daily.vola.NA$v.SA),
              xlab = "lag #", ylab = 'ACF',main='South Africa', plot = FALSE)
acf.sa <- plot(acf.sa)

acf.eu <- acf(daily.vola.NA$v.EURO,lag.max = length(daily.vola.NA$v.EURO),
              xlab = "lag #", ylab = 'ACF',main='Europe', plot = FALSE)
acf.eu <- plot(acf.eu)

#, width=4.2,height=12

'tikz(file = "YYY/autocorrelation.tex", standAlone=T)
acf.nigeria + acf.namibia + acf.kenya + acf.sa + acf.eu + plot_layout(ncol = 2)
dev.off()'



# 2.5 Pairwise measures -------------------------------------------------------

#Pairwise combinations with Nigeria:

melted_NPDC.Nigeria <- reshape2::melt(connectedness.2.12.BVAR.BayesPrior$NPDC[1,,,drop=FALSE])

NPDC.nigeria.namibia <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.Namibia", ]
NPDC.nigeria.namibia$Var3 <- as.Date(NPDC.nigeria.namibia$Var3)

NPDC.nigeria.kenya <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.Kenya", ]
NPDC.nigeria.kenya$Var3 <- as.Date(NPDC.nigeria.kenya$Var3)

NPDC.nigeria.sa <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.SA", ]
NPDC.nigeria.sa$Var3 <- as.Date(NPDC.nigeria.sa$Var3)

NPDC.nigeria.eu <- melted_NPDC.Nigeria[melted_NPDC.Nigeria$Var2 == "v.EURO", ]
NPDC.nigeria.eu$Var3 <- as.Date(NPDC.nigeria.eu$Var3)



net.pairwise.nigeria.namibia = ggplot(NPDC.nigeria.namibia, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(a) Nigeria - Namibia",
       x = "",
       y = "") +
  theme(plot.margin=unit(c(0,0,-0.25,1), "cm"))

net.pairwise.nigeria.kenya = ggplot(NPDC.nigeria.kenya, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(b) Nigeria - Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(0,1,-0.25,0), "cm"))

net.pairwise.nigeria.sa = ggplot(NPDC.nigeria.sa, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(c) Nigeria - South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

net.pairwise.nigeria.eu = ggplot(NPDC.nigeria.eu, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-35,33.2), breaks = seq(-30, 30, 10)) +
  labs(title = "(d) Nigeria - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))


#Pairwise connections with Namibia

melted_NPDC.Namibia <- reshape2::melt(connectedness.2.12.BVAR.BayesPrior$NPDC[2,,,drop=FALSE])

NPDC.namibia.kenya <- melted_NPDC.Namibia[melted_NPDC.Namibia$Var2 == "v.Kenya", ]
NPDC.namibia.kenya$Var3 <- as.Date(NPDC.namibia.kenya$Var3)

NPDC.namibia.sa <- melted_NPDC.Namibia[melted_NPDC.Namibia$Var2 == "v.SA", ]
NPDC.namibia.sa$Var3 <- as.Date(NPDC.namibia.sa$Var3)

NPDC.namibia.eu <- melted_NPDC.Namibia[melted_NPDC.Namibia$Var2 == "v.EURO", ]
NPDC.namibia.eu$Var3 <- as.Date(NPDC.namibia.eu$Var3)


net.pairwise.namibia.kenya = ggplot(NPDC.namibia.kenya, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(e) Namibia - Kenya",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

net.pairwise.namibia.sa = ggplot(NPDC.namibia.sa, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(f) Namibia - South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))

net.pairwise.namibia.eu = ggplot(NPDC.namibia.eu, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-31,32), breaks = seq(-30, 30, 10)) +
  labs(title = "(g) Namibia - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

#Pairwise connections with Kenya

melted_NPDC.Kenya <- reshape2::melt(connectedness.2.12.BVAR.BayesPrior$NPDC[3,,,drop=FALSE])

NPDC.kenya.sa <- melted_NPDC.Kenya[melted_NPDC.Kenya$Var2 == "v.SA", ]
NPDC.kenya.sa$Var3 <- as.Date(NPDC.kenya.sa$Var3)

NPDC.kenya.eu <- melted_NPDC.Kenya[melted_NPDC.Kenya$Var2 == "v.EURO", ]
NPDC.kenya.eu$Var3 <- as.Date(NPDC.namibia.sa$Var3)


net.pairwise.kenya.sa = ggplot(NPDC.kenya.sa, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(h) Kenya - South Africa",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25,0), "cm"))

net.pairwise.kenya.eu= ggplot(NPDC.kenya.eu, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(i) Kenya - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,0,-0.25,1), "cm"))

net.pairwise.kenya.eu
# Pairwise connection with South Africa 

melted_NPDC.SA <- reshape2::melt(connectedness.2.12.BVAR.BayesPrior$NPDC[4,,,drop=FALSE])

NPDC.sa.eu <- melted_NPDC.SA[melted_NPDC.SA$Var2 == "v.EURO", ]
NPDC.sa.eu$Var3 <- as.Date(NPDC.sa.eu$Var3)


net.pairwise.sa.eu = ggplot(NPDC.sa.eu, aes(x = Var3, y = value)) +
  geom_area() +
  scale_y_continuous(limits = c(-30,30), breaks = seq(-30, 30, 10)) +
  labs(title = "(j) South Africa - Europe",
       x = "",
       y = "")+
  theme(plot.margin=unit(c(-0.25,1,-0.25, 0), "cm"))



library(grid)
library(ggpubr)
install.packages("gridExtra")
library(gridExtra)

'tikz(file = "YYY/net.pairwise.grid.tex", standAlone=T)
grid.arrange(net.pairwise.nigeria.namibia,net.pairwise.nigeria.kenya,net.pairwise.nigeria.sa,net.pairwise.nigeria.eu, net.pairwise.namibia.kenya, net.pairwise.namibia.sa, net.pairwise.namibia.eu, net.pairwise.kenya.sa, net.pairwise.kenya.eu,
             net.pairwise.sa.eu, ncol = 2, nrow = 5)
dev.off()
'



'tikz(file = "YYY/networkplot.npdc.tex", standAlone=T)
PlotNetwork(connectedness.2.12.BVAR.BayesPrior, method = "NPDC")
dev.off()
'





# Figure 6 ----------------------------------------------------------------

'tikz(file = "YYY/net.pairwise.tex", standAlone=T)
g <- net.pairwise.nigeria.namibia+net.pairwise.nigeria.kenya+net.pairwise.nigeria.sa+net.pairwise.nigeria.eu+ net.pairwise.namibia.kenya+net.pairwise.namibia.sa+net.pairwise.namibia.eu+net.pairwise.kenya.sa+net.pairwise.kenya.eu+
  net.pairwise.sa.eu+ plot_layout(ncol = 2) + theme(panel.spacing = unit(c(-0.5,0-0.5,0), "lines")) + theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  theme(strip.background = element_blank()) +
  theme(plot.background = element_blank()) +
  theme(strip.text = element_blank()) '




Sensitivity.Prior <- as.data.frame(connectedness.2.12.BVAR.MinnesotaPrior$TCI)
Sensitivity.Prior$Bayes <- as.data.frame(connectedness.2.12.BVAR.BayesPrior$TCI)

max.min.mean.function.Prior <- transform(Sensitivity.Prior, Min = pmin(TCI,Bayes$TCI), Max = pmax(TCI,Bayes$TCI), Median = pmedian(TCI,Bayes$TCI), indx = seq_len(dim(Sensitivity.Prior)[1]))
max.min.mean.function.Prior$Date <- as.Date(row.names(connectedness.2.12.BVAR.MinnesotaPrior$TCI))

sensitivity.Prior <- ggplot(max.min.mean.function.Prior) +
  geom_line(aes(Date, Median), group=1) +
  geom_ribbon(aes(x = Date, ymax = Max, ymin = Min), alpha = 0.6, fill = "darkgray")



