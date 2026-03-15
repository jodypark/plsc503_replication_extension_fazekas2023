#####################################################################
## Replication file for                                            ##
## Agency independence, campaign contributions,                    ##
##   and favouritism in US federal government contracting.         ##
## Authors: Mihaly Fazekas, Romain Ferrali, Johannes Wachs         ##
## April 13, 2022                                                  ##
## Maintainer: Romain Ferrali (romain.ferrali@univ-amu.fr)         ##
#####################################################################

# environment: R 4.1.3
# platform: aarch64-apple-darwin20 (64-bit)
# run on Apple Macbook Pro M1-Pro, 32 Gb shared memory
library(Hmisc) # 4.6.0
library(MASS) # 7.3.55
library(tidyverse) # 1.3.1
library(lfe) # 2.8.7.1
library(modelsummary) # 0.9.6
library(stargazer) # 5.2.3
library(lubridate) # 1.8.0
library(scales) # 1.1.1
library(rdrobust) # 0.99.4
library(kableExtra) # 1.3.4
library(gridExtra) # 2.3
library(psych) # 2.2.3
library(nFactors) # 2.4.1
# extra: car 3.0.12, knitr 1.38

library(here)

knitr::opts_knit$set(root.dir = here::here())

getwd()
list.files()



#### constants ####
source("./functions.R")
theme_set(theme_minimal(base_size = 8))
nRep <- 1e3
set.seed(6174) # the most mysterious number
pdf <- TRUE # set to TRUE to output manuscript figures to pdf
eps <- TRUE # set to TRUE to output manuscript figures to eps

#### read in data ####
df <- read_csv("./contracts.csv", na = "NA")
df <- df %>% mutate(
    id = 1:nrow(.),
    nmissing = is.na(singleb) + is.na(nocft) + is.na(corr_proc) + is.na(corr_solic) + is.na(tax_haven) + is.na(disbarred) + is.na(modAB_binary),
    CRI7_final = ifelse(is.na(singleb), 0, singleb) +
        ifelse(is.na(nocft), 0, nocft) +
        ifelse(is.na(corr_proc), 0, corr_proc) +
        ifelse(is.na(corr_solic), 0, corr_solic) +
        ifelse(is.na(tax_haven), 0, tax_haven) +
        ifelse(is.na(disbarred), 0, disbarred) +
        ifelse(is.na(modAB_binary), 0, modAB_binary),
    CRI7_final = CRI7_final / (7 - nmissing),
    CRI7_final = ifelse(nmissing > 3, NA, CRI7_final),
    simple = singleb * corr_proc
)

#### factor analysis ####
mf <- df %>%
    select(
        id, singleb, nocft, corr_proc, corr_solic, tax_haven, disbarred, modAB_binary
    ) %>%
    na.omit()

cmat <- tetrachoric(mf %>% select(-id))$rho
test <- nScree(x = cmat)

pdf("./figures/scree.pdf", width = 4, height = 4)
plot(test)
dev.off()

fas <- list(
    f71 = fa(mf %>% select(-id), 1, cor = "tet"),
    f73 = fa(mf %>% select(-id), 3, cor = "tet")
)

dvs <- list(
    f71 = factor.scores(mf %>% select(-id), fas$f71)$scores,
    f73 = factor.scores(mf %>% select(-id), fas$f73)$scores
)
dvs <- do.call(cbind, dvs)
colnames(dvs) <- c(
    "f71", "f731", "f732", "f733"
)
dvs <- as_tibble(dvs) %>% mutate(id = mf$id)
dvs <- dvs %>% select(id, f71, f731)
df <- df %>%
    as_tibble() %>%
    left_join(dvs)

df <- df %>%
    mutate(
        donationR = ifelse(current_R_donor < 0, 0, current_R_donor),
        donationD = ifelse(current_D_donor < 0, 0, current_D_donor),
        logDonationD = log(donationD + 1),
        logDonationR = log(donationR + 1),
        logDonation = log(donationR + donationD + 1),
        isDonation = as.integer(logDonation > 0),
        giveAbs = ifelse(D_pres, logDonationD, logDonationR),
        giveOther = ifelse(D_pres, logDonationR, logDonationD),
        logValue = log(dollarsobligated_sum),
        congTerm = floor((year - 2001) / 2), 
        signeddate = as_date(signeddate),
        product1 = substr(productorservicecode, 1, 1),
        product2 = substr(productorservicecode, 1, 2),
        CRI7 = CRI7_final,
        ctype = recode(
            contract_type,
            A = "fixed",
            B = "fixed",
            J = "fixed",
            K = "fixed",
            L = "fixed",
            M = "fixed",
            R = "cost",
            S = "cost",
            T = "cost",
            U = "cost",
            V = "cost",
            Y = "cost",
            Z = "cost",
            .default = "other"
        ),
        ctype = fct_relevel(ctype, "other", "fixed", "cost"),
        agency_cat = recode(
            agency_cat2,
            `cabinet/exec. dep. (not bureau)` = "cabinet/exec. dep.",
            `cabinet/exec. dep. and bureau` = "cabinet/exec. dep.",
            `indep. agency` = "indep. agency",
            `indep. commission & IRC` = "indep. agency"
        ),
        agency_cat2 = as_factor(agency_cat2),
        agency_cat2 = fct_relevel(agency_cat2, "indep. commission & IRC"),
        agency_cat = as_factor(agency_cat),
        agency_cat = fct_relevel(agency_cat, "indep. agency")
    )

revenues <- df %>%
    group_by(congTerm, pduns) %>%
    summarize(rev = sum(dollarsobligated_sum)) %>%
    ungroup()

mods <- list()
df$CRI7_final <- df$f71

#### descriptives ####

#### Table A1 ####

desc <- df %>%
    select(
        `Single bidding` = singleb,
        `FRI` = CRI7_final,
        `Contract value (log)` = logValue,
        `Donation dummy` = isDonation,
        `Donation (log)` = logDonation
    ) %>%
    as.data.frame()
mystar(
    desc,
    summary = T, out = "tables/desc.tex", keep.stat = NULL
)

#### Figure 1 ####

desc <- df %>% select(
    singleb, nocft, corr_proc, corr_solic, modAB_binary, tax_haven, disbarred, CRI7_final
)

pl1 <- desc %>%
    select(-CRI7_final) %>%
    gather(var, value) %>%
    group_by(var) %>%
    summarize(
        nFlag = sum(value, na.rm = T),
        nMissing = sum(is.na(value)),
        n = n()
    ) %>%
    mutate(
        nFlag = nFlag / n,
        nMissing = nMissing / n
    ) %>%
    select(-n) %>%
    gather(key, value, -var) %>%
    arrange(key, value) %>%
    mutate(
        var = recode(var,
            corr_proc = "Non-competitive procedure",
            corr_solic = "Non-open solicitation",
            disbarred = "Debarred supplier",
            nocft = "No publication",
            modAB_binary = "Modifications",
            singleb = "Single bidding",
            tax_haven = "Supplier registered in tax haven"
        ),
        var = fct_inorder(var),
        key = recode(
            key,
            nMissing = "Missing data",
            nFlag = "Has red flag"
        ),
        key = fct_rev(key)
    ) %>%
    ggplot(aes(x = var, y = value, fill = key)) +
    geom_col() +
    coord_flip() +
    scale_fill_grey() +
    scale_y_continuous(
        labels = scales::percent,
        limits = c(0, 1)
    ) +
    labs(
        x = "Red flag",
        y = "Percent contracts",
        fill = "",
        title = "(b) FRI components"
    ) +
    theme(legend.position = "bottom")

pl2 <- ggplot(desc, aes(CRI7_final)) +
    stat_ecdf() +
    xlim(-1.75, 2) +
    labs(
        x = "FRI",
        title = "(a) Cumulative distribution of FRI",
        y = "Pr(FRI < x)"
    )

pl <- grid.arrange(pl2, pl1, widths = c(2, 3))

if (eps) ggsave("figures/descriptives.eps", pl, width = 5, height = 3)
if (pdf) ggsave("figures/descriptives.pdf", pl, width = 5, height = 3)

#### Table A3 ####

desc <- df %>% select(
    singleb, nocft, corr_proc, corr_solic, modAB_binary, tax_haven, disbarred,
    simple, CRI7, f731, CRI7_final
)
colnames(desc) <- c(
    "FRI 1", "FRI 2", "FRI 3",
    "FRI 4", "FRI 5", "FRI 6", "FRI 7", "FRI 1+3", "Avg.", "Fct. 1/3", "Fct. 1/1"
)
rowlabs <- c(
    "FRI 1 - Single bidding", "FRI 2 - No publication of call for tenders",
    "FRI 3 - Non-competitive procedure", "FRI 4 - Non-open solicitation",
    "FRI 5 - Contract modifications",
    "FRI 6 - Tax haven registration", "FRI 7 - Debared supplier",
    "FRI 1+3", "Average", "Factor 1/3", "FRI = Factor 1/1"
)

datasummary_correlation(
    desc,
    method = function(x) cor_fun(x, rowlabs),
    escape = F, output = "latex_tabular",
    align = "lccccccccccc"
) %>% save_kable("./tables/val3.tex")

#### Table A5 ####

desc2 <- cbind(
    unclass(fas$f71$loadings),
    unclass(fas$f73$loadings)
)
desc2 <- desc2[c("singleb", "nocft", "corr_proc", "corr_solic", "modAB_binary", "tax_haven", "disbarred"), ]
rownames(desc2) <- rowlabs[1:7]
colnames(desc2) <- c("Fct. 1/1", "Fct. 1/3", "Fct. 2/3", "Fct. 3/3")
kbl(
    desc2,
    format = "latex",
    escape = F, booktabs = T, align = "cccc", digits = 3, linesep = ""
) %>%
    save_kable("./tables/loadings.tex")

rm(pl1, pl2, pl, desc, desc2)
#### H1 ####

## first 2 models

baselineH1 <- felm(CRI7_final ~ isDonation + logValue +
    contract_action_type_dummy + commercial_procedure_dummy + ctype |
    product2 + congTerm + state_code + contractingofficeid |
    0 | pduns + congTerm, data = df, exactDOF = T)

mods$h1$bFirm$base <- baselineH1
mods$h1$bFirm$cont <- update(baselineH1, . ~ . + logDonation)

## thresholds
t1 <- coef(mods$h1$bFirm$cont)
t1 <- -t1["isDonation"] / t1["logDonation"]
t3 <- max(df$logDonation)

cutLabels <- c("low", "mid", "high")
mods$h1$bFirm$thresh <- map(
    seq(.5, .9, .1),
    function(thresh) {
        qt <- quantile(df$logDonation[df$isDonation == 1], thresh)
        cutBins <- c(-1, t1, qt, t3)
        df <- df %>%
            mutate(
                fDonation3 = cut(logDonation, cutBins, cutLabels, include.lowest = T)
            )
        update(baselineH1, . ~ . - isDonation + fDonation3, data = df)
    }
)

## inline: t1 in distribution of donations

df %>%
    select(pduns, congTerm, logDonation) %>%
    distinct() %>%
    filter(logDonation > 0) %>%
    mutate(logDonation = as.integer(logDonation < t1)) %>%
    pull(logDonation) %>%
    mean()

## inline: effect of increasing donations from 1e3 to 1e6

(log(1e6) - log(1e3)) * coef(mods$h1$bFirm$cont)["logDonation"]

#### Figure A2 ####

## figure on thresholds

pl <- map_dfr(mods$h1$bFirm$thresh, function(m) {
    tibble(
        pe = coef(m)[c("fDonation3mid", "fDonation3high")],
        LB = confint(m)[c("fDonation3mid", "fDonation3high"), 1],
        UB = confint(m)[c("fDonation3mid", "fDonation3high"), 2],
        type = c("Intermediate", "Large")
    )
}) %>%
    mutate(
        tnum = seq(.5, .9, .1) %>% rep(2) %>% sort(),
        thresh = sprintf(
            "%s - K$%s",
            tnum,
            comma(exp(quantile(df$logDonation[df$isDonation == 1], tnum)) * 1e-3)
        )
    )
ggplot(pl, aes(x = thresh, y = pe, ymin = LB, ymax = UB, color = type)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_linerange(position = position_dodge(width = .5)) +
    geom_hline(yintercept = 0, lty = "dotted") +
    scale_color_manual(values = c("Intermediate" = "grey66", "Large" = "black")) +
    labs(
        x = "Threshold for intermediate vs. large donation",
        y = "Effect of donation on FRI",
        color = "Donation size"
    ) +
    theme(legend.position = "bottom")
ggsave("figures/thresholds.pdf", height = 3, width = 4)

## extraction of upper threshold

t2 <- pl %>%
    filter(type == "Intermediate" & LB > 0) %>%
    pull(tnum)
t2 <- quantile(df$logDonation[df$isDonation == 1], t2[1])
cutBins <- c(-1, t1, t2, t3)

df <- df %>% mutate(
    fDonation3 = cut(logDonation, cutBins, cutLabels, include.lowest = T)
)

## binned model

mods$h1$bFirm$bins <- update(baselineH1, . ~ . - isDonation + fDonation3)

#### Table 2 ####

list(
    mods$h1$bFirm$base,
    mods$h1$bFirm$cont,
    mods$h1$bFirm$bins
) %>%
    tableH1() %>%
    save_kable("./tables/h1.tex")


#### Figure 2 ####

pl <- data.frame(
    logDonation = seq(from = log(1), to = max(df$logDonation), length.out = 1000)
)
pl <- cbind(
    pl,
    pe = cbind(1, 0, 0, 0, 0, 0, pl$logDonation) %*% coef(mods$h1$bFirm$cont),
    t(apply(
        cbind(1, 0, 0, 0, 0, 0, pl$logDonation) %*% t(mvrnorm(nRep, coef(mods$h1$bFirm$cont), vcov(mods$h1$bFirm$cont))), 1,
        quantile,
        probs = c(.025, .975)
    ))
)
colnames(pl) <- c("logDonation", "pe", "LB", "UB")
pl <- as.tibble(pl)
pl$donation <- exp(pl$logDonation)
ggplot(pl, aes(x = donation, y = pe, ymin = LB, ymax = UB)) +
    geom_line() +
    geom_ribbon(lty = "dashed", fill = NA, color = "black") +
    geom_hline(yintercept = 0, lty = "dotted") +
    scale_x_log10(labels = dollar) +
    geom_vline(xintercept = exp(t1), lty = "dotted") +
    annotate("text",
        x = 7000, y = 0.2, label = dollar(round(exp(t1))),
        angle = 90, size = 3
    ) +
    labs(x = "Amount donated", y = "Marginal effect on FRI")

if (eps) ggsave("figures/H1marginal.eps", width = 5, height = 3)
if (pdf) ggsave("figures/H1marginal.pdf", width = 5, height = 3)

#### Table A10 ####

dfMatch <- df %>%
    select(
        CRI7_final,
        isDonation, fDonation3,
        logValue, contract_action_type_dummy, commercial_procedure_dummy, ctype,
        product2, year, state_code, pduns, contractingofficeid, congTerm
    ) %>%
    na.omit() %>%
    mutate(binLogValue = cut(logValue,
        c(
            min(logValue) - 1,
            quantile(
                logValue,
                seq(.2, 1, .2)
            )
        ),
        include.lowest = T
    ))

matchBin <- myCEM(
    dfMatch, "isDonation",
    c("congTerm", "state_code", "contractingofficeid", "binLogValue")
)

mods$h1$bFirm$baseMatch <- update(mods$h1$bFirm$base, data = matchBin)


matchTri <- myCEM(
    dfMatch, "fDonation3",
    c("congTerm", "state_code", "contractingofficeid", "binLogValue")
)

mods$h1$bFirm$triMatch <- update(mods$h1$bFirm$bins, data = matchTri)

mms(
    list(
        mods$h1$bFirm$base,
        mods$h1$bFirm$baseMatch,
        mods$h1$bFirm$bins,
        mods$h1$bFirm$triMatch
    ),
    coef_map = covariate_labels,
    add_rows = tribble(
        ~"term", ~"1", ~"2", ~"3", ~"4",
        "Matched sample", "\\_", "\\checkmark", "\\_", "\\checkmark"
    )
) %>%
    add_header_above(c(" " = 1, "Favouritism Risk Index (FRI)" = 4)) %>%
    save_kable("./tables/h1Match.tex")

mods$h1$bFirm$thresh <- mods$h1$bFirm$baseMatch <- mods$h1$bFirm$triMatch <- NULL

#### Figure 3 ####

df %>%
    filter(isDonation == 1) %>%
    select(giveAbs, giveOther, congTerm, pduns) %>%
    distinct() %>%
    mutate(
        giveAbs = 1 + exp(giveAbs),
        giveOther = 1 + exp(giveOther),
        giveWho = ifelse(giveAbs / giveOther > 10, "majority", "both"),
        giveWho = ifelse(giveAbs / giveOther < .1, "opposition", giveWho)
    ) %>%
    ggplot(aes(x = giveAbs, y = giveOther)) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    geom_abline(slope = 1, intercept = 0) +
    geom_segment(aes(x = 1, xend = 10e5, y = 10, yend = 10e6), lty = "dotted") +
    geom_segment(aes(x = 10, xend = 10e6, y = 1, yend = 10e5), lty = "dotted") +
    scale_fill_gradient(low = "grey90", high = "black") +
    scale_x_log10(labels = scales::dollar, limits = c(1, 10e6)) +
    scale_y_log10(labels = scales::dollar, limits = c(1, 10e6)) +
    labs(
        x = "Donation to maj.",
        y = "Donation to opp."
    )
if (eps) ggsave("figures/h2Density.eps", height = 4, width = 5, units = "in")
if (pdf) ggsave("figures/h2Density.pdf", height = 4, width = 5, units = "in")


#### Table 3 ####

mods$h2$bFirm$base <- update(baselineH1, . ~ . + giveAbs)

cutBins2 <- cutBins
t12 <- coef(mods$h2$bFirm$base)
t12 <- -t12["isDonation"] / t12["giveAbs"]
cutBins2[2] <- t12

thresh <- map(
    seq(.5, .9, .1),
    function(thresh) {
        qt <- quantile(df$giveAbs[df$isDonation == 1], thresh)
        cutBins <- cutBins2
        cutBins[3] <- qt
        df <- df %>%
            mutate(
                giveAbsBin = cut(giveAbs, cutBins, cutLabels, include.lowest = T)
            )
        update(baselineH1, . ~ . + giveAbsBin - isDonation, data = df)
    }
)
thresh %>% map_lgl(function(m) all(m$cpval[c("giveAbsBinmid", "giveAbsBinhigh")] < 0.05))
cutBins2[3] <- quantile(df$giveAbs[df$isDonation == 1], .9)
df <- df %>% mutate(
    giveAbsBin = cut(giveAbs, cutBins2, include.lowest = T, labels = cutLabels),
    giveOtherBin = cut(giveOther, cutBins2, include.lowest = T, labels = cutLabels)
)
rm(thresh)

mods$h2$bFirm$bin <- update(baselineH1, . ~ . - isDonation + giveAbsBin)
mods$h2$bFirm$base2 <- update(baselineH1, . ~ . + giveAbs + giveOther)
mods$h2$bFirm$bin2 <- update(
    baselineH1, . ~ . - isDonation + giveAbsBin + giveOtherBin,
    psdef = T
)

tableH2(mods$h2$bFirm) %>% save_kable("./tables/h2.tex")

#### Table A6 ####

tibble(
    ` ` = c("Small-Medium", "Medium-Large"),
    All = dollar(exp(cutBins[2:3]) - 1),
    `To majority` = dollar(exp(cutBins2[2:3]) - 1)
) %>%
    kbl(format = "latex") %>%
    save_kable("./tables/cutpoints.tex")

#### Table 4 ####

m <- felm(CRI7_final ~ isDonation + agency_cat + isDonation:agency_cat + logValue +
    contract_action_type_dummy + commercial_procedure_dummy + ctype |
    product2 + state_code + contractingofficeid |
    0 | pduns + congTerm, data = df, exactDOF = T)

mods$h3$nofe$base <- update(baselineH1, . ~ . + agency_cat + isDonation:agency_cat)
mods$h3$nofe$cont <- update(baselineH1, . ~ . + agency_cat + logDonation + logDonation:agency_cat)
mods$h3$nofe$bins <- update(baselineH1, . ~ . - isDonation + fDonation3 + agency_cat + fDonation3:agency_cat)
mods$h3$nofe$contpres <- update(baselineH1, . ~ . + giveAbs + agency_cat + giveAbs:agency_cat)
mods$h3$nofe$binpres <- update(baselineH1, . ~ . - isDonation + giveAbsBin + agency_cat + giveAbsBin:agency_cat)

tableH3(mods$h3$nofe) %>% save_kable("./tables/h3.tex")

#### Table A23 ###

mods$h3$cat4$base <- update(baselineH1, . ~ . + agency_cat2 + isDonation:agency_cat2)
mods$h3$cat4$cont <- update(baselineH1, . ~ . + agency_cat2 + logDonation + logDonation:agency_cat2)
mods$h3$cat4$bins <- update(baselineH1, . ~ . - isDonation + fDonation3 + agency_cat2 + fDonation3:agency_cat2)
mods$h3$cat4$contpres <- update(baselineH1, . ~ . + giveAbs + agency_cat2 + giveAbs:agency_cat2)
mods$h3$cat4$binpres <- update(baselineH1, . ~ . - isDonation + giveAbsBin + agency_cat2 + giveAbsBin:agency_cat2)

tableH3(mods$h3$cat4) %>% save_kable("./tables/h3cat4.tex")

#### Figure 4 ####

x <- model.matrix(mods$h3$cat4$cont)
x <- x[1:4, ]
x[, ] <- 0
x[, "logDonation"] <- 1
x[2, "agency_cat2indep. agency:logDonation"] <-
    x[3, "agency_cat2cabinet/exec. dep. and bureau:logDonation"] <-
    x[4, "agency_cat2cabinet/exec. dep. (not bureau):logDonation"] <- 1


CI <- t(mvrnorm(nRep, mu = coef(mods$h3$cat4$cont), Sigma = vcov(mods$h3$cat4$cont))) # sample coefs from the variance covariance matrix
CI <- x %*% CI
CI <- t(apply(CI, 1, quantile, probs = c(.025, .975)))
colnames(CI) <- c("LB", "UB")

pe <- as.numeric(x %*% coef(mods$h3$cat4$cont))
pl <- tibble(
    donation = c("Indep. commission & IRC", "Indep. agency", "Cabinet/exec. dep. and bureau", "Cabinet/exec. dep. (not bureau)"),
    pe = pe
) %>%
    bind_cols(as_tibble(CI)) %>%
    mutate(
        donation = fct_inorder(donation)
    )

ggplot(pl, aes(x = donation, y = pe, ymin = LB, ymax = UB)) +
    geom_point() +
    geom_linerange() +
    geom_hline(yintercept = 0, lty = "dotted") +
    coord_flip() +
    labs(
        x = "Agency type",
        y = "Marginal effect of Log donation on FRI"
    )
if (eps) ggsave("figures/H3marginal.eps", width = 5, height = 2)
if (pdf) ggsave("figures/H3marginal.pdf", width = 5, height = 2)

#### Table A27 ####

df_wdonor <- df %>% mutate(CRI7_final = CRI7)

mods$h1$robDVCRI <- map(mods$h1$bFirm, ~ update(., data = df_wdonor))
tableH1(mods$h1$robDVCRI, "Simple average") %>%
    save_kable("./tables/h1RobDVCRI.tex")

#### Table A28 ####

mods$h2$robDVCRI <- map(
    mods$h2$bFirm,
    ~ update(., data = df_wdonor)
)
tableH2(mods$h2$robDVCRI, "Simple average") %>%
    save_kable("./tables/h2RobDVCRI.tex")

#### Table A29 ####

mods$h3$robDVCRI <- map(mods$h3$nofe, ~ update(., data = df_wdonor))
tableH3(mods$h3$robDVCRI, "Simple average") %>%
    save_kable("./tables/h3RobDVCRI.tex")

#### Table A11 ####

df <- df %>% left_join(
    revenues %>% mutate(
        congTerm = congTerm + 1,
        rev = log(rev)
    )
)
mods$h1$robLagRev <- map(mods$h1$bFirm, ~ update(., . ~ . + rev))
tableH1(mods$h1$robLagRev) %>% save_kable("./tables/h1robLagRev.tex")

#### Table A12 ####

mods$h2$robLagRev <- map(mods$h2$bFirm, ~ update(., . ~ . + rev))
tableH2(mods$h2$robLagRev) %>% save_kable("./tables/h2robLagRev.tex")

#### Table A13 ####

mods$h3$robLagRev <- map(mods$h3$nofe, ~ update(., . ~ . + rev))
tableH3(mods$h3$robLagRev) %>% save_kable("./tables/h3robLagRev.tex")

#### Table A30 ####

mods$h1$robDVF71 <- map(mods$h1$bFirm, ~ update(., data = df_wdonor))
tableH1(mods$h1$robDVF71, "Factor 1/3") %>%
    save_kable("./tables/h1RobDVF71.tex")

#### Table A31 ####

mods$h2$robDVF71 <- map(
    mods$h2$bFirm,
    ~ update(., data = df_wdonor)
)
tableH2(mods$h2$robDVF71, "Factor 1/3") %>%
    save_kable("./tables/h2RobDVF71.tex")

#### Table A32 ####

mods$h3$robDVF71 <- map(mods$h3$nofe, ~ update(., data = df_wdonor))
tableH3(mods$h3$robDVF71, "Factor 1/3") %>%
    save_kable("./tables/h3RobDVF71.tex")

#### Table A20 ####

df_wdonor <- df %>%
    mutate(
        giveAbsT = 1 + exp(giveAbs),
        giveOtherT = 1 + exp(giveOther),
        keep = (giveAbsT / giveOtherT > 10) | (giveAbsT / giveOtherT < .1) | isDonation == 0
    ) %>%
    filter(keep)

mods$h1$rob6 <- map(mods$h1$bFirm, ~ update(., data = df_wdonor))
tableH1(mods$h1$rob6) %>%
    save_kable("./tables/h1Rob6.tex")

#### Table A21 ####

mods$h2$rob6 <- map(
    mods$h2$bFirm,
    ~ update(., data = df_wdonor)
)
tableH2(mods$h2$rob6, high = F) %>%
    save_kable("./tables/h2Rob6.tex")

#### Table A22 ####

mods$h3$rob6 <- map(mods$h3$nofe, ~ update(., data = df_wdonor))
tableH3(mods$h3$rob6) %>%
    save_kable("./tables/h3Rob6.tex")

#### Table A14 ####

table(df$agency_name)
df_ndefense <- filter(
    df, agency_name != "Department of the Air Force", agency_name != "Department of the Navy", agency_name != "Department of the Army",
    !str_detect(agency_name, "Defense")
)
table(df_ndefense$agency_name)

mods$h1$rob2 <- map(mods$h1$bFirm, ~ update(., data = df_ndefense))
tableH1(mods$h1$rob2) %>%
    save_kable("./tables/h1Rob2.tex")


#### Table A15 ####

mods$h2$rob2 <- map(
    mods$h2$bFirm,
    ~ update(., data = df_ndefense)
)
tableH2(mods$h2$rob2) %>%
    save_kable("./tables/h2Rob2.tex")

#### Table A16 ####

mods$h3$rob2 <- map(mods$h3$nofe, ~ update(., data = df_ndefense))
tableH3(mods$h3$rob2) %>%
    save_kable("./tables/h3Rob2.tex")

#### Table A24 ####

mods$h1$rob3 <- map(mods$h1$bFirm, ~ update(., simple ~ .))
tableH1(mods$h1$rob3, "Single bidding & non-competitive procedure") %>%
    save_kable("./tables/h1Rob3.tex")

#### Table A25 ####

mods$h2$rob3 <- map(
    mods$h2$bFirm,
    ~ update(., simple ~ .)
)
tableH2(mods$h2$rob3, "Single bidding & non-competitive procedure") %>%
    save_kable("./tables/h2Rob3.tex")

#### Table A26 ####

mods$h3$rob3 <- map(mods$h3$nofe, ~ update(., simple ~ .))
tableH3(mods$h3$rob3, "Single bidding & non-competitive procedure") %>%
    save_kable("./tables/h3Rob3.tex")

#### Table A2 ####

mods$val1$nocft <-
    felm(singleb ~ nocft + logValue |
        product2 + state_code + contractingofficeid + year |
        0, data = df, exactDOF = T)
mods$val1$proc <-
    felm(singleb ~ corr_proc + logValue |
        product2 + state_code + contractingofficeid + year |
        0, data = df, exactDOF = T)
mods$val1$solic <-
    felm(singleb ~ corr_solic + logValue |
        product2 + state_code + contractingofficeid + year |
        0, data = df, exactDOF = T)
mods$val1$overrun2 <-
    felm(singleb ~ modAB_binary + logValue |
        product2 + state_code + contractingofficeid + year |
        0, data = df, exactDOF = T)
mods$val1$all <-
    felm(singleb ~ nocft + corr_proc + corr_solic + modAB_binary + logValue |
        product2 + state_code + contractingofficeid + year |
        0, data = df, exactDOF = T)

mms(
    mods$val1,
    coef_map = c(
        "nocft" = "No publication of call for tenders",
        "corr_proc" = "Non-competitive procedure type",
        "corr_solic" = "Non-open solicitation type",
        "modAB_binary" = "Modifications",
        "logValue" = "Log contract value"
    )
) %>%
    add_header_above(c(" " = 1, "Single bidding" = 5)) %>%
    save_kable("./tables/val1.tex")


#### Table A3 ####

validation <- read_csv("./validation.csv")

vvvv <- df %>%
    filter(year == 2014) %>%
    group_by(state_code) %>%
    summarize(CRI_2014 = mean(CRI7_final, na.rm = T)) %>%
    inner_join(
        df %>% filter(year >= 2006 & year <= 2014) %>%
            group_by(state_code) %>%
            summarize(CRI = mean(CRI7_final, na.rm = T))
    ) %>%
    inner_join(validation) %>%
    filter(
        state_code %in% c(state.abb, "DC")
    ) %>%
    select(
        CRI, CRI_2014, gallup, cia_ill_ex, cia_le_ex, bl_q6_score, bl_sc_score
    )
colnames(vvvv) <- c(
    "FRI", "FRI (2014)", "GALLUP", "CASSR 1",
    "CASSR 2", "BL 1", "BL 2"
)

rowlabs <- c(
    "FRI (2006-2014)", "FRI (2014)",
    "GALLUP  - perception of corruption", "CASSR 1 - illegal corruption",
    "CASSR 2 - legal corrutpion", "BL 1 - perceived state ranking", "BL 2 - index"
)

datasummary_correlation(
    vvvv,
    method = function(x) cor_fun(x, rowlabs), escape = F, output = "latex_tabular",
    align = "lccccccc"
) %>% save_kable("./tables/val2.tex")


#### Table 1 ####

dfFirms <- df %>%
    group_by(pduns, congTerm) %>%
    summarize(
        value = sum(dollarsobligated_sum),
        n = n(),
        logDonation = max(logDonation, na.rm = T),
        giveAbs = max(giveAbs, na.rm = T),
        giveOther = max(giveOther, na.rm = T)
    ) %>%
    group_by(pduns) %>%
    mutate(
        minTerm = min(congTerm),
        maxTerm = max(congTerm)
    ) %>%
    ungroup() %>%
    complete(congTerm = 1:6, pduns, fill = list(logDonation = 0, value = 0, n = 0)) %>%
    group_by(pduns) %>%
    mutate(minTerm = max(minTerm, na.rm = T), maxTerm = max(maxTerm, na.rm = T)) %>%
    filter(congTerm >= minTerm & congTerm <= maxTerm) %>%
    filter(maxTerm > minTerm) %>%
    ungroup() %>%
    mutate(
        logValue = log(value + 1),
        logN = log(n + 1),
        isDonation = as.integer(logDonation > 0),
        fDonation3 = cut(logDonation, cutBins, cutLabels),
        giveAbsBin = cut(giveAbs, cutBins, cutLabels),
        giveOtherBin = cut(giveOther, cutBins, cutLabels)
    ) %>%
    group_by(pduns) %>%
    arrange(pduns, congTerm) %>%
    mutate(
        lagLogValue = lag(logValue),
        lagLogValue2 = lagLogValue^2
    )

mods$h1awards$main$base <- felm(
    logValue ~ isDonation + lagLogValue + lagLogValue2 | pduns | 0 | pduns + congTerm,
    data = dfFirms
)
mods$h1awards$main$intercept <- update(
    mods$h1awards$main$base, . ~ . + logDonation
)
mods$h1awards$main$bin <- update(
    mods$h1awards$main$base, . ~ . - isDonation + fDonation3
)

mms(mods$h1awards$main, coef_map = covariate_labels_revenue) %>%
    add_header_above(c(" " = 1, "$\\\\log(\\\\text{revenue})_{t}$" = 3), escape = F) %>%
    save_kable("./tables/h1awards.tex")

#### Table A8 ####

mods$h1awards$twoway$base <- felm(
    logValue ~ isDonation + lagLogValue + lagLogValue2 | pduns + congTerm | 0 | pduns + congTerm,
    data = dfFirms
)
mods$h1awards$twoway$intercept <- update(
    mods$h1awards$twoway$base, . ~ . + logDonation
)
mods$h1awards$twoway$bin <- update(
    mods$h1awards$twoway$base, . ~ . - isDonation + fDonation3
)

mms(mods$h1awards$twoway, coef_map = covariate_labels_revenue) %>%
    add_header_above(c(" " = 1, "$\\\\log(\\\\text{revenue})_{t}$" = 3), escape = F) %>%
    save_kable("./tables/h1awardsRob.tex")

#### Table A7 ####

mods$h2awards$main$base <- update(mods$h1awards$main$base, . ~ . + giveAbs)
mods$h2awards$main$bin <- update(mods$h1awards$main$base, . ~ . - isDonation + giveAbsBin)
mods$h2awards$main$base2 <- update(mods$h2awards$main$base, . ~ . + giveOther)
mods$h2awards$main$bin2 <- update(mods$h2awards$main$bin, . ~ . + giveOtherBin)

mms(
    mods$h2awards$main,
    coef_map = covariate_labels_revenue,
    add_rows = h2Test(mods$h2awards$main$base2, mods$h2awards$main$bin2)
) %>%
    add_header_above(c(" " = 1, "$\\\\log(\\\\text{revenue})_{t}$" = 4), escape = F) %>%
    save_kable("./tables/h2awards.tex")

#### Table A9 ####

mods$h2awards$twoway$base <- update(mods$h1awards$twoway$base, . ~ . + giveAbs)
mods$h2awards$twoway$bin <- update(mods$h1awards$twoway$base, . ~ . - isDonation + giveAbsBin)
mods$h2awards$twoway$base2 <- update(mods$h2awards$twoway$base, . ~ . + giveOther)
mods$h2awards$twoway$bin2 <- update(mods$h2awards$twoway$bin, . ~ . + giveOtherBin)

mms(
    mods$h2awards$twoway,
    coef_map = covariate_labels_revenue,
    add_rows = h2Test(mods$h2awards$twoway$base2, mods$h2awards$twoway$bin2)
) %>%
    add_header_above(c(" " = 1, "$\\\\log(\\\\text{revenue})_{t}$" = 4), escape = F) %>%
    save_kable("./tables/h2awardsRob.tex")

#### Table A17 ####

df <- df %>% mutate(
    good = as.integer(substr(product2, 1, 1) %in% 0:9)
)
mods$h1$rob4 <- map(mods$h1$bFirm, ~ update(., subset = good == 1))
tableH1(mods$h1$rob4) %>% save_kable("./tables/h1Rob4.tex")

#### Table A18 ####

mods$h2$rob4 <- map(mods$h2$bFirm, ~ update(., subset = good == 1))
tableH2(mods$h2$rob4) %>% save_kable("./tables/h2Rob4.tex")

#### Table A19 ####

mods$h3$rob4 <- map(mods$h3$nofe, ~ update(., subset = good == 1))
tableH3(mods$h3$rob4) %>% save_kable("./tables/h3Rob4.tex")

#### Table A33 ####

df <- df %>% mutate(
    donor1 = as.integer(giveAbs > 0),
    donor2 = as.integer(giveAbs > t1)
)

dfRDL <- df %>% filter(dollarsobligated_sum < 12.5e6)
dfRDH <- df %>% filter(dollarsobligated_sum > 650e3)

extractRD <- function(mod) {
    est <- sprintf("\\num{%.3f}", mod$Estimate[2])
    pval <- sprintf("(\\num{%.3f})", mod$pv[3, 1])
    bw <- mod$bws[1, ]
    bw <- round(bw * 1e-3) %>% scales::comma()
    bw <- sprintf("[-%s; %s]", bw[1], bw[2])
    n <- sum(mod$N) %>% scales::comma()
    out <- c(est, pval, bw, n)
    out
}

mods$RD$lTresh$pool <- rdrobust(
    dfRDL$CRI7_final, dfRDL$dollarsobligated_sum,
    c = 650e3, bwselect = "msetwo", vce = "hc0"
)
mods$RD$lTresh$donor <- update(mods$RD$lTresh$pool, subset = dfRDL$donor1 == 1)
mods$RD$lTresh$nDonor <- update(mods$RD$lTresh$pool, subset = dfRDL$donor1 == 0)

mods$RD$hTresh$pool <- rdrobust(
    dfRDH$CRI7_final, dfRDH$dollarsobligated_sum,
    c = 12.5e6, bwselect = "msetwo", vce = "hc0"
)
mods$RD$hTresh$donor <- update(mods$RD$hTresh$pool, subset = dfRDH$donor1 == 1)
mods$RD$hTresh$nDonor <- update(mods$RD$hTresh$pool, subset = dfRDH$donor1 == 0)


out <- data.frame(
    `Pooled` = extractRD(mods$RD$lTresh$pool),
    `Donor` = extractRD(mods$RD$lTresh$donor),
    `Non-donor` = extractRD(mods$RD$lTresh$nDonor),
    `Pooled` = extractRD(mods$RD$hTresh$pool),
    `Donor` = extractRD(mods$RD$hTresh$donor),
    `Non-donor` = extractRD(mods$RD$hTresh$nDonor)
)
rownames(out) <- c("Estimate", "", "Bandwidth (k\\$)", "$N$")
colnames(out) <- rep(c("Pooled", "Donor", "Non-Donor"), 2)
knitr::kable(out, format = "latex", escape = F, booktabs = T, align = c(rep("c", 4))) %>%
    add_header_above(c("", "Low threshold ($650k)" = 3, "High threshold ($12.5m)" = 3)) %>%
    write_file("tables/rd.tex")

#### Table 6 ####

mods$RD$lTresh$pool <- rdrobust(
    dfRDL$CRI7_final, dfRDL$dollarsobligated_sum,
    c = 650e3, bwselect = "msetwo", vce = "hc0"
)
mods$RD$lTresh$donor <- update(mods$RD$lTresh$pool, subset = dfRDL$donor2 == 1)
mods$RD$lTresh$nDonor <- update(mods$RD$lTresh$pool, subset = dfRDL$donor2 == 0)

mods$RD$hTresh$pool <- rdrobust(
    dfRDH$CRI7_final, dfRDH$dollarsobligated_sum,
    c = 12.5e6, bwselect = "msetwo", vce = "hc0"
)
mods$RD$hTresh$donor <- update(mods$RD$hTresh$pool, subset = dfRDH$donor2 == 1)
mods$RD$hTresh$nDonor <- update(mods$RD$hTresh$pool, subset = dfRDH$donor2 == 0)


out <- data.frame(
    `Pooled` = extractRD(mods$RD$lTresh$pool),
    `Donor` = extractRD(mods$RD$lTresh$donor),
    `Non-donor` = extractRD(mods$RD$lTresh$nDonor),
    `Pooled` = extractRD(mods$RD$hTresh$pool),
    `Donor` = extractRD(mods$RD$hTresh$donor),
    `Non-donor` = extractRD(mods$RD$hTresh$nDonor)
)
rownames(out) <- c("Estimate", "", "Bandwidth (k\\$)", "$N$")
colnames(out) <- rep(c("Pooled", "Donor", "Non-Donor"), 2)
knitr::kable(out, format = "latex", escape = F, booktabs = T, align = c(rep("c", 4))) %>%
    add_header_above(c("", "Low threshold ($650k)" = 3, "High threshold ($12.5m)" = 3)) %>%
    write_file("tables/rd1.tex")

#### Figure A4 ####

estimate <- function(cutoff, type) {
    if (type == "low") {
        mods <- list(
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0"),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor1 == 0),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor1 == 1),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor2 == 0),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor2 == 1)
        )
    } else {
        mods <- list(
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0"),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor1 == 0),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor1 == 1),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor2 == 0),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = cutoff, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor2 == 1)
        )
    }

    out <- mods %>% map_dfr(function(m) {
        tibble(
            pe = m$Estimate[2],
            lwr = m$ci[3, 1],
            upr = m$ci[3, 2]
        )
    })
    out$type <- type
    out$sample <- c("pooled", "non-donors", "donors", "non-donors (strong)", "donors (strong)")
    out$cutoff <- cutoff
    out
}

cutoffsLow <- seq(500e3, 800e3, 25e3)
cutoffsHigh <- seq(11e6, 14e6, 250e3)
pl <- bind_rows(
    map_dfr(cutoffsLow, ~ estimate(., "low")),
    map_dfr(cutoffsHigh, ~ estimate(., "high"))
)

pl %>%
    mutate(
        cutoff = ifelse(
            type == "high",
            sprintf("$%2.2fm", round(cutoff / 1e6, 2)),
            sprintf("$%sk", round(cutoff / 1e3))
        ),
        type = recode(type, high = "(b) High threshold ($12.5m)", low = "(a) Low threshold ($650k)")
    ) %>%
    ggplot(aes(x = cutoff, y = pe, ymin = lwr, ymax = upr, color = sample)) +
    geom_point(position = position_dodge(.5)) +
    geom_linerange(position = position_dodge(.5)) +
    geom_hline(yintercept = 0, lty = "dotted") +
    facet_wrap(vars(type), ncol = 1, scales = "free_x") +
    scale_color_grey() +
    theme(legend.position = "bottom") +
    labs(
        color = "Sample",
        x = "Cutoff",
        y = "Point estimate"
    )

ggsave("figures/robustnessRDthreshold.pdf", width = 6, height = 4)

#### Figure A3 ####

estimate <- function(factors = seq(.8, 1.2, .05)) {
    mods <- list(
        rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0"),
        rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor1 == 0),
        rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor1 == 1),
        rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor2 == 0),
        rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor2 == 1),
        rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0"),
        rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor1 == 0),
        rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor1 == 1),
        rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor2 == 0),
        rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor2 == 1)
    )

    bws <- map(mods, ~ .$bws)
    out <- map_dfr(factors, function(f) {
        bws <- map(bws, ~ . * f)
        mods <- list(
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", h = bws[[1]][1, ], b = bws[[1]][2, ]),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor1 == 0, h = bws[[2]][1, ], b = bws[[2]][2, ]),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor1 == 1, h = bws[[3]][1, ], b = bws[[3]][2, ]),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor2 == 0, h = bws[[4]][1, ], b = bws[[4]][2, ]),
            rdrobust(dfRDL$CRI7_final, dfRDL$dollarsobligated_sum, c = 650e3, bwselect = "msetwo", vce = "hc0", subset = dfRDL$donor2 == 1, h = bws[[5]][1, ], b = bws[[5]][2, ]),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", h = bws[[6]][1, ], b = bws[[6]][2, ]),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor1 == 0, h = bws[[7]][1, ], b = bws[[7]][2, ]),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor1 == 1, h = bws[[8]][1, ], b = bws[[8]][2, ]),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor2 == 0, h = bws[[9]][1, ], b = bws[[9]][2, ]),
            rdrobust(dfRDH$CRI7_final, dfRDH$dollarsobligated_sum, c = 12.5e6, bwselect = "msetwo", vce = "hc0", subset = dfRDH$donor2 == 1, h = bws[[10]][1, ], b = bws[[10]][2, ])
        )
        mods %>%
            map_dfr(function(m) {
                tibble(
                    pe = m$Estimate[2],
                    lwr = m$ci[3, 1],
                    upr = m$ci[3, 2]
                )
            }) %>%
            mutate(
                type = c(rep("low", 5), rep("high", 5)),
                sample = rep(c("pooled", "non-donors", "donors", "non-donors (strong)", "donors (strong)"), 2),
                bw = f
            )
    })
    out
}

pl <- estimate()
pl %>%
    mutate(
        bw = scales::percent(bw),
        bw = fct_inorder(bw),
        type = recode(type, high = "(b) High threshold ($12.5m)", low = "(a) Low threshold ($650k)")
    ) %>%
    ggplot(aes(x = bw, y = pe, ymin = lwr, ymax = upr, color = sample)) +
    geom_point(position = position_dodge(.5)) +
    geom_linerange(position = position_dodge(.5)) +
    geom_hline(yintercept = 0, lty = "dotted") +
    facet_wrap(vars(type), ncol = 1, scales = "free_x") +
    scale_color_grey() +
    theme(legend.position = "bottom") +
    labs(
        color = "Sample",
        x = "Bandwidth",
        y = "Point estimate"
    )
ggsave("figures/robustnessRDbandwidth.pdf", width = 6, height = 4)

#### Table 5 ####

analyze <- function(df, bounds, cutoff, band, mesh, donation = 0, polyorder = 7) {
    cuts <- seq(bounds[1], bounds[2], mesh)
    dfHist <- df %>%
        filter(dollarsobligated_sum >= bounds[1] & dollarsobligated_sum <= bounds[2]) %>%
        mutate(
            amount = cut(dollarsobligated_sum, cuts, include.lowest = T),
            isDonation = as.integer(giveAbs > donation)
        ) %>%
        group_by(isDonation, amount) %>%
        summarize(n = n()) %>%
        ungroup() %>%
        complete(isDonation, amount, fill = list(n = 0)) %>%
        arrange(isDonation, amount) %>%
        mutate(lb = rep(cuts[-length(cuts)], 2)) %>%
        group_by(isDonation) %>%
        mutate(pct = n / sum(n)) %>%
        ungroup() %>%
        mutate(inBand = as.integer(lb >= band[1] & lb <= band[2]))
    mod <- lm(pct ~ poly(lb, 7) * isDonation + isDonation * inBand, data = dfHist)
    dfHist$pr <- predict(mod)

    pl <- dfHist %>%
        mutate(isDonation = ifelse(isDonation == 1, "Donors", "Non-donors")) %>%
        ggplot(aes(x = lb, y = pct)) +
        geom_line(lwd = 0.25, color = "grey50") +
        geom_line(lwd = 1, aes(y = pr)) +
        geom_vline(xintercept = cutoff, lty = "dotted") +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "bottom") +
        labs(
            x = "Contract value",
            y = "Density"
        ) +
        facet_grid(~isDonation)
    out <- list(pl = pl, mod = mod)
    return(out)
}

mods$bunching$lowCut0 <- analyze(df, c(100e3, 1.5e6), 650e3, c(645e3, 650e3), 2.5e3)
mods$bunching$lowCut1 <- analyze(df, c(100e3, 1.5e6), 650e3, c(645e3, 650e3), 2.5e3, t1)
mods$bunching$highCut0 <- analyze(df, c(5e6, 20e6), 12.5e6, c(12.5e6, 13e6), 25e3)
mods$bunching$highCut1 <- analyze(df, c(5e6, 20e6), 12.5e6, c(12.5e6, 13e6), 25e3, t1)

tests <- map(mods$bunching, ~ myFtest(.$mod, "inBand", "isDonation:inBand", diff = F))
tests <- map(tests, function(v) {
    v <- sprintf("\\num{%.3f}", v)
    v[2] <- sprintf("(%s)", v[2])
    v
}) %>% bind_cols()
tests$term <- c("$H_0: \\beta_1 + \\beta_2 = 0$", "")
tests <- tests %>% select(term, everything())
colnames(tests) <- c("term", "1", "2", "3", "4")

mms(
    list(
        mods$bunching$lowCut0$mod, mods$bunching$lowCut1$mod,
        mods$bunching$highCut0$mod, mods$bunching$highCut1$mod
    ),
    fmt = 5,
    coef_map = c(
        "isDonation" = "Donor ($\\beta_1$)",
        "inBand" = "In bunching interval",
        "isDonation:inBand" = "Donor $\\times$ in bunching interval ($\\beta_2$)"
    ),
    add_rows = tribble(
        ~"term", ~"1", ~"2", ~"3", ~"4",
        "Threshold", "\\$650k", "\\$650k", "\\$12.5m", "\\$12.5m",
        "Donor", "any", "mid-high", "any", "mid-high",
    ) %>% bind_rows(tests)
) %>% save_kable("./tables/bunching.tex")

#### Figure 5 ####

mods$bunching$highCut0$pl +
    scale_x_continuous(labels = sprintf("$%sm", seq(5, 20, 5)))
if (eps) ggsave("figures/bunching.eps", height = 3, width = 5)
if (pdf) ggsave("figures/bunching.pdf", height = 3, width = 5)