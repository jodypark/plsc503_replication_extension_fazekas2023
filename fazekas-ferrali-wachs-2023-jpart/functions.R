#####################################################################
## Functions used in replication file for                          ##
## Agency independence, campaign contributions,                    ##
##   and favouritism in US federal government contracting.         ##
## Authors: Mihaly Fazekas, Romain Ferrali, Johannes Wachs         ##
## April 13, 2022                                                  ##
## Maintainer: Romain Ferrali (romain.ferrali@univ-amu.fr)         ##
#####################################################################


covariate_labels <- c(
  "isDonation" = "Donation dummy",
  "isDonation:agency_catcabinet/exec. dep." = "Donation dummy $\\times$ Cabinet/Exec. dep.",
  "isDonation:agency_cat2indep. agency" = "Donation dummy $\\times$ Indep. agency",
  "isDonation:agency_cat2cabinet/exec. dep. and bureau" = "Donation dummy $\\times$ Cabinet/Exec. dep. and bureau",
  "isDonation:agency_cat2cabinet/exec. dep. (not bureau)" = "Donation dummy $\\times$ Cabinet/Exec. dep. (not bureau)",
  "logDonation" = "Log donation",
  "agency_catcabinet/exec. dep.:logDonation" = "Log donation $\\times$ Cabinet/Exec. dep.",
  "agency_cat2indep. agency:logDonation" = "Log donation $\\times$ Indep. agency",
  "agency_cat2cabinet/exec. dep. and bureau:logDonation" = "Log donation $\\times$ Cabinet/Exec. dep. and bureau",
  "agency_cat2cabinet/exec. dep. (not bureau):logDonation" = "Log donation $\\times$ Cabinet/Exec. dep. (not bureau)",
  "giveAbs" = "Log donation to majority ($\\beta_1$)",
  "giveAbs:agency_catcabinet/exec. dep." = "Log donation to majority $\\times$ Cabinet/Exec. dep.",
  "giveOther" = "Log donation to opp. ($\\beta_2$)",
  "fDonation3mid" = "Med. donation",
  "fDonation3high" = "Lrg. donation",
  "fDonation3mid:agency_catcabinet/exec. dep." = "Med. donation $\\times$ Cabinet/Exec. dep.",
  "fDonation3high:agency_catcabinet/exec. dep." = "Lrg. donation $\\times$ Cabinet/Exec. dep.",
  "fDonation3mid:agency_cat2indep. agency" = "Med. donation $\\times$ Indep. agency",
  "fDonation3high:agency_cat2indep. agency" = "Lrg. donation $\\times$ Indep. agency",
  "fDonation3mid:agency_cat2cabinet/exec. dep. and bureau" = "Med. donation $\\times$ Cabinet/Exec. dep. and bureau",
  "fDonation3high:agency_cat2cabinet/exec. dep. and bureau" = "Lrg. donation $\\times$ Cabinet/Exec. dep. and bureau",
  "fDonation3mid:agency_cat2cabinet/exec. dep. (not bureau)" = "Med. donation $\\times$ Cabinet/Exec. dep. (not bureau)",
  "fDonation3high:agency_cat2cabinet/exec. dep. (not bureau)" = "Lrg. donation $\\times$ Cabinet/Exec. dep. (not bureau)",
  "giveAbsBinmid" = "Intermediate donation to majority ($\\beta_1$)",
  "giveAbsBinhigh" = "Large donation to majority ($\\beta_3$)",
  "giveAbsBinmid:agency_catcabinet/exec. dep." = "Med. donation to maj. $\\times$ Cabinet/Exec. dep.",
  "giveAbsBinhigh:agency_catcabinet/exec. dep." = "Lrg. donation to maj. $\\times$ Cabinet/Exec. dep.",
  "giveAbsBinmid:agency_cat2indep. agency" = "Med. donation to maj. $\\times$ Indep. agency",
  "giveAbsBinhigh:agency_cat2indep. agency" = "Lrg. donation to maj. $\\times$ Indep. agency",
  "giveAbsBinmid:agency_cat2cabinet/exec. dep. and bureau" = "Med. donation to maj. $\\times$ Cabinet/Exec. dep. and bureau",
  "giveAbsBinhigh:agency_cat2cabinet/exec. dep. and bureau" = "Lrg. donation to maj. $\\times$ Cabinet/Exec. dep. and bureau",
  "giveAbsBinmid:agency_cat2cabinet/exec. dep. (not bureau)" = "Med. donation to maj. $\\times$ Cabinet/Exec. dep. (not bureau)",
  "giveAbsBinhigh:agency_cat2cabinet/exec. dep. (not bureau)" = "Lrg. donation to maj. $\\times$ Cabinet/Exec. dep. (not bureau)",
  "giveOtherBinmid" = "Intermediate donation to opposition ($\\beta_2$)",
  "giveOtherBinhigh" = "Large donation to opposition ($\\beta_4$)",
  "agency_catcabinet/exec. dep." = "Cabinet/Exec. dep.",
  "agency_cat2indep. agency" = "Indep. agency",
  "agency_cat2cabinet/exec. dep. and bureau" = "Cabinet/Exec. dep. and bureau",
  "agency_cat2cabinet/exec. dep. (not bureau)" = "Cabinet/Exec. dep. (not bureau)",
  "logValue" = "Log contract value",
  "contract_action_type_dummy" = "GSA-run procurement",
  "commercial_procedure_dummy" = "Commercial item",
  "ctypefixed" = "Fixed-price contract",
  "ctypecost" = "Cost-plus contract",
  "rev" = "Log($\\text{revenue}_{t-1}$)"
)

covariate_labels_revenue <- c(
  "isDonation" = "Donation dummy",
  "logDonation" = "Log donation",
  "fDonation3mid" = "Med. donation",
  "fDonation3high" = "Lrg. donation",
  "giveAbs" = "Log donation to majority ($\\beta_1$)",
  "giveAbsBinmid" = "Med. donation to majority ($\\beta_1$)",
  "giveAbsBinmhigh" = "Lrg. donation to majority ($\\beta_3$)",
  "giveOther" = "Log donation to opposition ($\\beta_2$)",
  "giveOtherBinmid" = "Med. donation to opposition ($\\beta_2$)",
  "giveOtherBinhigh" = "Lrg. donation to opposition ($\\beta_4$)",
  "lagLogValue" = "$\\log(\\text{revenue})_{t-1}$",
  "lagLogValue2" = "$\\log(\\text{revenue})^2_{t-1}$"
)

tableH1 <- function(models, title = "Favouritism Risk Index (FRI)") {
  clabs <- covariate_labels
  clabs <- gsub(" ($\\beta_1$)", "", clabs, fixed = T)
  clabs <- gsub(" ($\\beta_2$)", "", clabs, fixed = T)
  clabs <- gsub(" ($\\beta_3$)", "", clabs, fixed = T)
  clabs <- gsub(" ($\\beta_4$)", "", clabs, fixed = T)
  tit <- c(1, 3)
  names(tit) <- c(" ", title)
  mms(
    models,
    coef_map = clabs
  ) %>%
    add_header_above(tit)
}

h2Test <- function(mod3, mod4, high = T) {
  tests <- list(
    myFtest(mod3, "giveAbs", "giveOther"),
    myFtest(mod4, "giveAbsBinmid", "giveOtherBinmid")
  )
  if (high) {
    tests[[3]] <-
      myFtest(mod4, "giveAbsBinhigh", "giveOtherBinhigh")
  } else {
    tests[[3]] <- c(0, 0)
  }
  tests <- map(tests, function(v) {
    v <- sprintf("\\num{%.3f}", v)
    v[2] <- sprintf("(%s)", v[2])
    v
  })
  out <- tibble(
    term = c(
      "$H_0: \\beta_1 - \\beta_2 = 0$", "",
      "$H_0: \\beta_3 - \\beta_4 = 0$", ""
    ),
    col1 = rep("", 4),
    col2 = rep("", 4),
    col3 = c(tests[[1]], "", ""),
    col4 = c(tests[[2]], tests[[3]])
  )
  if (!high) out <- out[1:2, ]
  out
}

tableH2 <- function(models, title = "Favouritism Risk Index (FRI)", high = T) {
  tit <- c(1, 4)
  names(tit) <- c(" ", title)
  mms(models,
    coef_map = covariate_labels,
    add_rows = h2Test(models[[3]], models[[4]], high = high)
  ) %>%
    add_header_above(tit)
}

tableH3 <- function(models, title = "Favouritism Risk Index (FRI)") {
  clabs <- covariate_labels
  clabs <- gsub(" ($\\beta_1$)", "", clabs, fixed = T)
  clabs <- gsub(" ($\\beta_2$)", "", clabs, fixed = T)
  clabs <- gsub(" ($\\beta_3$)", "", clabs, fixed = T)
  clabs <- gsub(" ($\\beta_4$)", "", clabs, fixed = T)
  tit <- c(1, 5)
  names(tit) <- c(" ", title)
  mms(
    models,
    coef_map = clabs
  ) %>%
    add_header_above(tit)
}

mms <- function(models, ...) {
  names(models) <- sprintf("(%s)", 1:length(models))
  modelsummary(
    models, ...,
    gof_omit = "Std|R2 Adj.|AIC|BIC|Log.Lik.|F|RMSE",
    statistic = "p.value",
    output = "latex_tabular",
    escape = F
  )
}

myFtest <- function(mod, beta1, beta2, diff = T) {
  hypMat <- matrix(0, nrow = 1, ncol = length(coef(mod)))
  colnames(hypMat) <- names(coef(mod))
  hypMat[, beta1] <- 1
  hypMat[, beta2] <- ifelse(diff, -1, 1)
  test <- car::linearHypothesis(mod, hypMat, test = "F")
  Fstat <- test$F[2]
  pval <- test$`Pr(>F)`[2]
  c(Fstat, pval)
}

myCEM <- function(df, treat, vars) {
  dfMatch <- df %>%
    group_by_(.dots = as.list(c(treat, vars))) %>%
    summarize(n = n()) %>%
    ungroup()
  treatValues <- sort(unique(dfMatch %>% pull(treat)))
  treatVars <- paste(treat, treatValues, sep = "_")
  naList <- as.list(rep(0, length(treatValues)))
  names(naList) <- treatVars
  dfMatch <- dfMatch %>%
    spread(treat, n, sep = "_") %>%
    replace_na(naList)
  condition <- map(treatVars, ~ pull(dfMatch, .))
  condition <- do.call(cbind, condition)
  condition <- apply(condition, 1, prod)
  condition <- condition > 0
  dfMatch <- dfMatch %>%
    filter(condition) %>%
    select(-treatVars)
  inner_join(df, dfMatch)
}

cor_fun <- function(dat, rowlabs) {
  dat <- as.matrix(dat)
  cobj <- rcorr(dat)
  cmat <- cobj$r
  pvals <- cobj$P
  out <- matrix("", nrow = nrow(cmat), ncol = ncol(cmat))
  for (i in 1:nrow(cmat)) {
    for (j in 1:ncol(cmat)) {
      if (j < i) {
        out[i, j] <-
          sprintf(
            "\\num{%.3f} (\\num{%.3f})",
            cmat[i, j], pvals[i, j]
          )
      }
      if (i == j) {
        out[i, j] <- "."
      }
    }
  }
  colnames(out) <- colnames(dat)
  rownames(out) <- rowlabs
  out
}

mystar <- function(..., font.size = "small", float = F, no.space = T, keep.stat = c("n", "rsq")) {
  stargazer(..., 
            font.size = font.size, 
            float = float, 
            no.space = no.space, 
            keep.stat = keep.stat)
}