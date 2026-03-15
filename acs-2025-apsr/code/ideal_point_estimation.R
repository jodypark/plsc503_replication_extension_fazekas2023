## CODE TO ESTIMATE AGENCY IDEAL POINTS



#### STATIC MODEL


library(rstan)
library(readr)
library(tidyr)
library(lme4)

options(mc.cores = parallel::detectCores())

v <- read.csv(file = 'data/estimation_data.csv')


v$agency.id <- as.factor(v$agency.id)
v <- v[order(v$agency.id),]

A <- length(unique(v$agency.id))
P <- length(unique(v$admin))
R <- length(unique(v$sig))

y <- v$review
a <- c(1:A)[v$agency.id]
p <- ifelse(v$admin == 'clinton', 1,
     ifelse(v$admin ==  'bush43', 2,
            ifelse(v$admin ==  'obama', 3, 4)))
r <- ifelse(v$sig == 0, 1, 2)

N <- length(y)


##  start values ------------------------
##  1. use multi-level model (MLM)
##  2. use MLM values in variational inference (vi)
##  3. use VI estimates in HMC model

## MLM
v$clinton <- ifelse(v$admin == 'clinton', 1, 0)
m1 <- glmer(review ~ (1 + clinton | agency.id) +
            clinton +
            sig,
          data = v, subset = admin == 'clinton' | admin == 'bush43',
            family = binomial(),
            control=glmerControl(optimizer="bobyqa"))
agencystart <- ranef(m1)$agency.id[,2]
kappastart <- ranef(m1)$agency.id[,1]

## use the start values from a multi-level model for xagency
initF <- function() {
    list(xagency=agencystart,
         kappa=kappastart,
         xpresident=c(-.4, .7, -.35, .4),
         scale = c(.1, .05),
         lambda = c(-2, -.5))
}

stan.data <- list(A=A, P=P, R=R, N=N, a=a, p=p, r=r, y=y,
                  agencystart=agencystart,
                  kappastart=kappastart)


source('code/stan_models.R') # stan.code object found in stan_models.R

## compile stan code
stan.model.static <- stan_model(model_code = stan.code.static)

## get starting values using variational inference
fit.vi <- vb(stan.model.static,
             data = stan.data,
             algorithm = "fullrank",
             adapt_iter = 100,
             iter = 5000,
             tol_rel_obj = 1e-4,
             init = initF,
             eta = .1,
             seed = 123)

vi.init.values <- get_posterior_mean(fit.vi)

## update start values
kappastart <- vi.init.values[grep('kappa', row.names(vi.init.values))]
agencystart <- vi.init.values[grep('xagency', row.names(vi.init.values))]


## 2. RUN 'Hamiltonian Monte Carlo with Adaptations (NUTS)' USING VI ESTIMATES AS PRIORS


hmc.fit.static <- sampling(stan.model.static, data = stan.data, iter=5000, chains=4, init=initF,
                           warmup=500,
                           seed = 123)

save(hmc.fit.static, file = 'data/static_stan_model_estimates.RData')



#### DYNAMIC MODEL

v <- read.csv(file = 'data/estimation_data.csv')


v$agency.id <- as.factor(v$agency.id)
v <- v[order(v$agency.id),]

A <- length(unique(v$agency.id))
P <- length(unique(v$admin))
R <- length(unique(v$sig))

y <- v$review
a <- c(1:A)[v$agency.id]
p <- ifelse(v$admin == 'clinton', 1,
     ifelse(v$admin ==  'bush43', 2,
            ifelse(v$admin ==  'obama', 3, 4)))
r <- ifelse(v$sig == 0, 1, 2)

N <- length(y)

t <- p
T <- P

##  start values ------------------------
load('data/static_stan_model_estimates.RData')
x <- data.frame(summary(hmc.fit.static, pars=c('xagency'), probs = c(.05,.95), digits=3)$summary)
agencystart <- rbind(x$mean, rbind(x$mean, rbind(x$mean, x$mean)))
x2 <- data.frame(summary(hmc.fit.static, pars=c('kappa'), probs = c(.05,.95), digits=3)$summary)
kappastart <- x2$mean

scale.means <- summary(hmc.fit.static, pars=c('scale'))$summary[,'mean']
lambda.means <- summary(hmc.fit.static, pars=c('lambda'))$summary[,'mean']

initF <- function() {
    list(xagency=agencystart,
         kappa=kappastart,
         xpresident=c(-.438, .693, -.358, .403), # dw nominate scores
         scale = scale.means,
         lambda = lambda.means)
}

stan.data <- list(A=A, P=P, R=R, N=N, T=T, a=a, p=p, r=r, y=y, t=t)

## import stan code objects
source('code/stan_models.R')

## compile model
stan.model <- stan_model(model_code = stan.code.v2)

## 2. RUN VARIATIONAL INFERENCE USING STATIC MODEL PRIORS

## get starting values using variational inference
fit_vi <- vb(stan.model,
             data = stan.data,
             algorithm = "fullrank",
             adapt_iter = 500,
             iter = 10000,
             tol_rel_obj = 1e-4,
             init = initF,
             eta = .01,
             seed = 123)

vi.init.values <- get_posterior_mean(fit_vi)

## update start values for kappa and agency
kappastart <- vi.init.values[grep('kappa', row.names(vi.init.values))]
agencystart <- rbind(vi.init.values[grep('agency\\.1', row.names(vi.init.values))],
                     rbind(vi.init.values[grep('agency\\.2', row.names(vi.init.values))],
                           rbind(vi.init.values[grep('agency\\.3', row.names(vi.init.values))],
                                 vi.init.values[grep('agency\\.4', row.names(vi.init.values))])))


stan.data <- list(A=A, P=P, R=R, N=N, T=T, a=a, p=p, r=r, y=y, t=t)

init.values.non.centered.kappa <- function() {
  sigma_guess <- 1

  # xagency_raw from agencystart + sigma_guess
  xagency_raw <- matrix(0, T, A)
  xagency_raw[1, ] <- agencystart[1, ]
  if (T > 1) {
    for (i in 2:T) {
      xagency_raw[i, ] <- (agencystart[i, ] - agencystart[i - 1, ]) / sigma_guess
    }
  }

  # hierarchical kappa inits from kappastart
  mu0  <- mean(kappastart, na.rm = TRUE)
  tau0 <- stats::sd(kappastart, na.rm = TRUE)
  if (!is.finite(tau0) || tau0 <= 1e-6) tau0 <- 0.1  # small floor
  kappa_raw0 <- (kappastart - mu0) / tau0
  kappa_raw0[is.na(kappa_raw0)] <- 0
  kappa_raw0 <- pmax(pmin(kappa_raw0, 4), -4)

  xpres0 <- c(-.438, .693, -.358, .403)

  scale0  <- pmax(as.numeric(scale.means), 1e-6)  # ensure > 0
  lambda0 <- as.numeric(lambda.means)

  list(
    xagency_raw = xagency_raw,
    sigma_rw    = sigma_guess,
    # hierarchical pieces:
    mu_kappa    = mu0,
    tau_kappa   = tau0,
    kappa_raw   = kappa_raw0,
    # other params:
    xpresident  = xpres0,
    scale       = scale0,
    lambda      = lambda0
  )
}


## hierarhical kappa
stan.model.non.centered <- stan_model(model_code = stan.code.non.centered.kappa)

# KAPPA + jittered inits (make sure to set seed above)
hmc.fit <- sampling(stan.model.non.centered, data = stan.data, iter=7000, # iter=25000, 5000 10000
                    chains=4, seed = 123,
                    init=init.values.non.centered.kappa,
                    control = list(adapt_delta=0.96),
                    warmup=1000)

save(hmc.fit, file = 'data/dynamic_stan_model_estimates.RData')



## CSV CONVERSION

x <- data.frame(summary(hmc.fit, pars=c('xagency'), probs = c(.05,.95), digits=3)$summary)
x <- x[,c(1,3,4,5)]
colnames(x) <- c('est', 'se', 'lower', 'upper')
x$time.period <- gsub('xagency\\[(.*),.*\\]', '\\1', rownames(x))
x$agency.digit <- gsub('xagency\\[.*,(.*)\\]', '\\1', rownames(x))
x$agency.digit <- x$agency.digit

##  bring in agency names
w <- read.csv(file = 'data/estimation_data.csv')
w <- w[order(w$agency.id),]

n.agencies <- length(unique(w$agency.id))
m <- data.frame(agency.digit = as.character(1:n.agencies), agency.id = unique(w$agency.id))
x <- merge(x, m, by = 'agency.digit', all.x = T)

x$admin <- 'Clinton'
x$admin[x$time.period == 2] <- 'Bush'
x$admin[x$time.period == 3] <- 'Obama'
x$admin[x$time.period == 4] <- 'Trump'
x$admin <- factor(x$admin, levels = c('Clinton', 'Bush', 'Obama', 'Trump'))

x$dept <- gsub('^(.*)-.*$', '\\1', x$agency.id)
x$agency <- gsub('^.*-(.*)$', '\\1', x$agency.id)
x$agency.id[x$dept == x$agency] <- x$dept[x$dept == x$agency]
write.csv(x, file = 'data/dynamic_estimates.csv', row.names = F)


