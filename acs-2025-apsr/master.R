## -------------------------------------------------------
##  Mapping the Political Contours of the Regulatory State
##                 Replication Code
## -------------------------------------------------------


rm(list = ls())
options(scipen=100)
options(digits=2)


#### ------------ PREAMBLE SCRIPTS (OPTIONAL) -------------------

## match package versions using the 'renv' package in R
## install.packages("renv") # if renv is not already installed
source('renv/activate.R')
renv::restore(prompt = FALSE) # read renv.lock

## rebuild CSV files used in main results
source('code/preamble_oira.R', local = TRUE)
source('code/preamble_ua.R', local = TRUE)
source('code/preamble_merge.R', local = TRUE)
# source('code/preamble_fed_reg_api.R', local = TRUE) # duration to run is several hours; requires 10,000+ API calls)


## re-estimate ideal point models (static and dynamic)
source('code/ideal_point_estimation.R') # approximately 45 minutes, depending on compute power


#### ----------------- TABLES & FIGURES ---------------------


####  FIGURE 1 (plot of estimates)


library(ggplot2)

x <- read.csv(file = 'data/dynamic_estimates.csv')

x$agency.id[x$agency.id == 'DOD-DODOASHA'] <- 'DOD-OASHA'
x$agency.id[x$agency.id == 'EPA-AR'] <- 'EPA-AIR'

x$admin <- factor(x$admin, levels = c("Clinton", "Bush", "Obama", "Trump"))

benchmark.y <- 33

pdf('results/figure1.pdf')
ggplot(x, aes(x = est, y = reorder(agency.id, est))) +
  facet_grid(. ~ admin) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", colour = 'salmon') +
    geom_hline(yintercept = benchmark.y, colour = "salmon", linetype = "dashed") +
    geom_segment(aes(x = est, xend = upper, yend = reorder(agency.id, est))) +
  geom_segment(aes(x = est, xend = lower, yend = reorder(agency.id, est))) +
  theme(axis.text.y = element_text(size = 6)) +
  ylab('') + xlab('')
dev.off()



####  FIGURE 2 (boxplot)

load('data/static_stan_model_estimates.RData')

library(rstan)
library(ggplot2)

y <- data.frame(summary(hmc.fit.static, pars=c('xagency'), probs = c(.05,.95), digits=3)$summary)
y <- y[,c(1,4,5)]
colnames(y) <- c('est', 'lower', 'upper')
w <- read.csv(file = 'data/estimation_data.csv') # retrieve agency names
w <- w[order(w$agency.id),]
y$agency.id <- unique(w$agency.id)
y <- y[order(y$est),]
dim(y)

# number of rules per agency
b <- read.csv(file = 'data/estimation_data.csv')

b$n.rules <- rep(1, nrow(b))
b <- aggregate(n.rules ~ agency.id, data = b, sum)
y <- merge(y, b, by = 'agency.id')

# get dept names
a <- read.csv(file = 'data/ua_data/agencycodes.csv')
a$agency.id <- paste(a$agency.abb, a$bureau.abb, sep = '-')
a <- subset(a, select = c(agency.id, agency, bureau, agency.abb) )
a <- a[!duplicated(a),]
y <- merge(y, a, by = c('agency.id'), all.x = T)

# recode names to keep
y$bureau[y$agency == 'Department of State'] <- ''
y$bureau[y$agency == 'Department of Veterans Affairs'] <- ''

z <- subset(y, as.character(y$agency) != as.character(y$bureau))
z <- subset(z, select = c(agency.id, est, agency, agency.abb, n.rules) )
colnames(z) <- c('agency', 'ideal.point', 'Department', 'Department.abb', 'n.rules')

z$n.dept.rules <- tapply(z$n.rules, z$Department, sum )[z$Department]
z$weight <- with(z, n.rules / n.dept.rules)

z$ideal.point.w <- with(z, weight * ideal.point)

# mean ideology per agency
z$mean.bias.w <- tapply(z$ideal.point.w, z$Department, sum)[z$Department]
z$mean.bias <- tapply(z$ideal.point, z$Department, mean)[z$Department]
z$median.bias <- tapply(z$ideal.point, z$Department, median)[z$Department]


z$label <- gsub('Department of |al Protection Agency| and Human Services|the | and Urban Development', '', z$Department)

z$label <- paste(z$label, ' (', z$Department.abb, ')', sep = '')

m <- max(nchar(z$label))
test <- sapply(z$label, function(x) {
  w <- m - nchar(x)
  paste(paste(rep(' ', w), collapse = ''), x, sep = '')
})
z$label <- as.character(test)


pdf('results/figure2.pdf')
ggplot(z, aes(x = reorder(factor(label), mean.bias.w, decreasing = TRUE), y = ideal.point)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "steelblue", linewidth = .6) +
    geom_boxplot(fill = 'salmon', coef = 1.3) +
    coord_flip() +
  xlab('') +
  ylab('') +
  ggtitle('') +
  theme(axis.text.x = element_text(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey95")) +
annotate("text", vjust = 0, hjust = 0,
         x = c(13.2),
         y = c(-2.6),
         label = c('OSHA'),
         size = 3) +
annotate("text", vjust = 0, hjust = 0,
         x = c(1.2),
         y = c(-2.2),
         label = c('Bureau of Indian Affairs'),
         size = 3) +
    scale_y_continuous(breaks = c(-1.9, -.87, 0, 1, 1.96), # see below for ideal point calculations
                     labels = c("Most Liberal \n Senate \n Democrat",
                                "Median \n Senate \n Democrat",
                                0,
                                "Median \n Senate \n Republican",
                                "Most Conserv. \n Senate \n Republican"),
                     limits = c(-3, 3))
dev.off()


## Senate ideal points:

library(pscl) # for IRT model of votes

x <- read.csv('data/voteview_s112_votes.csv')
y <- read.csv('data/voteview_S112.csv')

x$cast_code[x$cast_code == 7] <- NA
x$cast_code[x$cast_code == 9] <- NA
x$vote <- x$cast_code

## build vote matrix
leg.ids <- sort(unique(x$icpsr))
roll.ids <- sort(unique(x$rollnumber))

m <- matrix(NA_integer_, nrow = length(leg.ids), ncol = length(roll.ids),
                   dimnames = list(as.character(leg.ids), as.character(roll.ids)))
r <- match(x$icpsr, leg.ids)
c <- match(x$rollnumber, roll.ids)

m[cbind(r, c)] <- x$vote

sen_rc <- rollcall(data = m,
                   yea = 1,
                   nay = 6,
                   missing = NA,
                   legis.names = rownames(m),
                   legis.data = data.frame(icpsr = as.integer(rownames(m))),
                   vote.names = colnames(m))

f <- ideal(object = sen_rc,
           d = 1,
           meanzero = TRUE, # identification: mean zero
           normalize = TRUE, # identification: variance 1
           maxiter = 5000,
           burnin = 1000,
           thin = 5,
           store.item = TRUE,
           verbose = TRUE)

## change polarity if mike lee is estimated as negative
y[y$nominate_dim1 == max(y$nominate_dim1),] # Mike Lee, ICPSR 41104
d <- f$xbar[grep('41104', rownames(f$xbar))]
if(d < 0) f$xbar <- f$xbar * -1 # change polarity

## note: numbers can vary slightly across different model runs

max(f$xbar) # 1.96
min(f$xbar) # -1.9
mean(f$xbar) # 0

## get party means
g <- data.frame(icpsr = rownames(f$xbar), f$xbar)
y <- merge(y, g, by = 'icpsr')
mean(y$D1[y$party_code == 100]) # DEM mean: -.87
mean(y$D1[y$party_code == 200]) # REP mean: 1.00

## max, min and party means used in figure2.pdf (see above)


####  FIGURE 3 (heatmap)


library(dplyr)
library(ggplot2)
library(tidyr)


x <- read.csv(file = 'data/merged_ideal_points.csv')

y <- data.frame(department = x$Department, sapply(x[,c(2:6)], rank))
y$department <- gsub('Department of |al Protection Agency| and Human Services|the | and Urban Development', '', y$department)
colnames(y) <- c('Agency', 'Ideal_Points', 'Richardson_et_al_2018', 'Clinton_Lewis_2008', 'Chen_Johnson_2015',
                 'Clinton_et_al_2012')
y$Agency[y$Agency == 'Environment'] <- 'EPA'


# Determine top or bottom half and convert to long format
long_data <- y %>%
  pivot_longer(cols = -Agency, names_to = "Study", values_to = "Ranking") %>%
  mutate(Half = ifelse(Ranking <= 8, "Top Half", "Bottom Half"))

long_data <- long_data %>%
  mutate(Study = gsub("_", " ", Study),
         Study = gsub("(\\d{4})", "(\\1)", Study))

# Set factor levels for Half to control legend order
long_data$Half <- factor(long_data$Half, levels = c("Top Half", "Bottom Half"))

study_order <- c("Ideal Points", "Richardson et al (2018)", "Clinton Lewis (2008)",
                 "Chen Johnson (2015)", "Clinton et al (2012)")
long_data$Study <- factor(long_data$Study, levels = study_order)

# Count the number of "Top Half" occurrences for each agency
top_half_counts <- long_data %>%
  group_by(Agency) %>%
  summarize(Top_Half_Count = sum(Half == "Top Half"), .groups = 'drop') %>%
  arrange(desc(Top_Half_Count), Agency)

# Merge the counts back to the long data and sort
long_data <- long_data %>%
  left_join(top_half_counts, by = "Agency") %>%
  arrange(desc(Top_Half_Count), Agency)

# manipulations to change order of ties
long_data <- data.frame(long_data)
long_data$Top_Half_Count[long_data$Agency == 'Housing'] <- 3.1
long_data$Top_Half_Count[long_data$Agency == 'Energy'] <- 1.2

pdf('results/figure3.pdf')
ggplot(long_data, aes(x = Study, y = reorder(Agency, Top_Half_Count), fill = Half)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Top Half" = "steelblue", "Bottom Half" = "salmon"),
                    name = "Department \n Ranked In",
                    breaks = c("Top Half", "Bottom Half")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(size = 8)) +
    labs(title = "",
       x = "",
       y = "",
       fill = "Ranking Half") +
  scale_x_discrete(labels = function(x) ifelse(x == "Ideal Points", "Current Study", x))
dev.off()


####  FIGURE 4 (conservative trends)


x <- read.csv(file = 'data/dynamic_estimates.csv')
x <- subset(x, x$agency != 'BENCHMARK')

## average ideal points by admin
##  - statistics used in section 'Temporal Dynamics of Agency Ideal Points'
tapply(x$est, x$admin, mean)
## weighted.avg <- function(var, se) sum(var * (1/se^2)) / sum(1/se^2)
## tapply(x$est, x$admin, weighted.avg, x$se)
## (.04 - (-.11)) / abs(-.11)

z <- split(x, x$agency.id)
m <- lapply(z, function(k) {
    k$est.max  <- max(k$est)
    k$est.min  <- min(k$est)
    k$admin.max <- k$admin[k$est == k$est.max]
    k$admin.min <- k$admin[k$est == k$est.min]
    k$dif <- sqrt(abs(k$est.max - k$est.min))
    return(k)
})
r <- do.call('rbind', m)
rownames(r) <- 1:nrow(r)
r <- r[!duplicated(r$agency.id), c('agency.id', 'est.max', 'est.min', 'admin.max', 'admin.min', 'dif')]
r <- r[order(r$dif, decreasing = TRUE),]
rownames(r) <- 1:nrow(r)

## collect biggest shifters (top 10)
a <- r$agency.id[1:10]
y <- x[x$agency.id %in% a,]
## identify 7 conservative shifts from top 10
a <- c('AID', 'TREAS-FINCEN', 'DOD-DARC', 'OPM', 'DOD-DODOASHA', 'VA', 'EPA-AR')

y$administration <- factor(y$time.period,
                              levels = c(1, 2, 3, 4),
                              labels = c("Clinton", "Bush", "Obama", "Trump"))
y <- y[y$agency.id %in% a,]

## recode names
y$group[y$agency.id == 'OPM' | y$agency.id == 'EPA-AR' |
        y$agency.id == 'AID'] <- 'Shift During \n Trump Administration'
y$group[y$agency.id == 'VA' | y$agency.id == 'DOD-DODOASHA'] <- 'Gradual Trend \n Liberal Agencies'
y$group[y$agency.id == 'TREAS-FINCEN'
        | y$agency.id == 'DOD-DARC'] <- 'Gradual Trend \n Conserv. Agencies'
y$group <- factor(y$group,
                  levels = c('Gradual Trend \n Liberal Agencies',
                             'Shift During \n Trump Administration',
                             'Gradual Trend \n Conserv. Agencies'))

y$agency.name <- y$agency.id
y$agency.name[y$agency.id == 'EPA-AR'] <- 'EPA (Office of Air)'
y$agency.name[y$agency.id == 'DOD-DODOASHA'] <- 'Health Affairs (DOD-OASHA)'
y$agency.name[y$agency.id == 'OPM'] <- 'Personnel Management (OPM)'
y$agency.name[y$agency.id == 'VA'] <- 'Veterans Affairs (VA)'
y$agency.name[y$agency.id == 'AID'] <- 'International Development (AID)'
y$agency.name[y$agency.id == 'DOD-DARC'] <- 'Defense Acquisition (DOD-DARC)'
y$agency.name[y$agency.id == 'TREAS-FINCEN'] <- 'Financial Crimes (TREAS-FINCEN)'


pdf('results/figure4.pdf')
ggplot(y, aes(x = administration, y = est, linetype = agency.name, shape = agency.name, color = agency.name, group = agency.name)) +
    geom_line() +
   theme(plot.margin = unit(c(0.1, 0.5, 0.5, 0.5), "cm")) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    facet_wrap(~ group) +
  scale_linetype_manual(values = 1:7) +
  scale_shape_manual(values = 1:7) +
    scale_color_manual(values = c("salmon", "salmon", "darkgreen", "salmon",
                                  "steelblue", "darkgreen", "steelblue")) +
  labs(title = "",
       x = "",
       y = "",
       linetype = "Agency",
       shape = "Agency",
       color = "Agency") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_line(linewidth = 0.5, colour = "grey"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(linetype = guide_legend(nrow = 4, title = NULL),
           shape = guide_legend(nrow = 4, title = NULL),
           color = guide_legend(nrow = 4, title = NULL))
dev.off()


####  TABLE 1 (ideal point changes - follows from Figure 4)

library(xtable)

## use 'y' object from Figure 4

# Compute min and max by agency
min.vals <- tapply(y$est, y$agency.id, min)
max.vals <- tapply(y$est, y$agency.id, max)

# Combine into a data frame
tab <- data.frame(
  Agency = names(min.vals),
  Min = as.numeric(min.vals),
  Max = as.numeric(max.vals),
  row.names = NULL
)

# Create LaTeX table (NOTE: table is formatted differently in the paper)
print(
  xtable(tab,
         align = c("l", "l", "r", "r")),
  include.rownames = FALSE,
  file = 'results/table1.tex')


####  TABLE 2 (productivity in number of rules)


library(stargazer)
library(lubridate)
library(MASS)

v <- read.csv(file = 'data/estimation_data.csv')

v$agency.id[v$agency.abb == v$bureau.abb] <- v$agency.abb[v$agency.abb == v$bureau.abb]
v$agency.code <- substr(v$rin, 1, 4)
v <- v[!duplicated(v$agency.code), c('agency.id', 'agency.code')]
rownames(v) <- 1:nrow(v)
v$agency.code <- sprintf("%04s", v$agency.code)

y <- read.csv(file = 'data/dynamic_estimates.csv')

v$agency.id[!v$agency.id %in% y$agency.id] <- gsub('(.*)-.*', '\\1', v$agency.id[!v$agency.id %in% y$agency.id])

y <- merge(y, v, by = 'agency.id')
nrow(y) # 276

## import OIRA data to get RINs of reviewed rules
load('data/oira_data/oira_data.RData')
g <- oira.data
colnames(g) <- c('agency.code', 'rin', 'rule', 'stage', 'econ.sig', 'date.received',
                 'legal.dline', 'date.completed', 'decision', 'date.published')
## recode variables
g$returned <- ifelse(g$decision == 'Returned for Reconsideration' |
                     g$decision == 'Withdrawn', 1, 0)
g$accepted <- ifelse(g$decision == 'Consistent without Change', 1, 0)
g$review.duration <- as.numeric(as.Date(g$date.completed) - as.Date(g$date.received))

r <- subset(g, date.received > '1995-01-01' & date.received < '2021-01-19')
r <- subset(r, stage == 'Proposed Rule')
## drop odd cases
d <- c('Exempt from Executive Order', 'Improperly Submitted', 'Emergency',
       'Statutory or Judicial Deadline')
r <- subset(r, !decision %in% d)
r$accepted <- ifelse(r$decision == 'Consistent without Change', 1, 0)
r$total <- rep(1, nrow(r))
r$admin <- rep('Clinton', nrow(r))
r$admin[as.Date(r$date.received) > '2001-01-19'] <- 'Bush'
r$admin[as.Date(r$date.received) > '2009-01-19'] <- 'Obama'
r$admin[as.Date(r$date.received) > '2016-01-19'] <- 'Trump'

r1 <- aggregate(cbind(returned, accepted, total) ~ agency.code + admin, data = r, sum, na.rm = TRUE)
r2 <- aggregate(review.duration ~ agency.code + admin, data = r, mean, na.rm = TRUE)
r1 <- merge(r1, r2, by = c('agency.code', 'admin'), all.x = TRUE)

v.expand <- expand.grid(agency.id = v$agency.id,
                    admin = c('Clinton', 'Bush', 'Obama', 'Trump'))
v.expand <- merge(v, v.expand, by =  'agency.id')
r1 <- merge(v.expand, r1, by = c('agency.code', 'admin'), all.x = T)
r1[is.na(r1)] <- 0 ## agencies with no reviews get count of 0

r3 <- aggregate(review.duration ~ agency.code + admin,
                data = subset(r, econ.sig == 'Yes'), mean, na.rm = TRUE)
colnames(r3)[3] <- 'review.dur.econ'
r <- merge(r1, r3, by = c('agency.code', 'admin'), all.x = TRUE)

## federal register data on final rules
x <- read.csv(file = 'data/final_rules_from_api.csv')

## fix date and admin
x$date <- mdy(x$publication_date)
x$date <- format(x$date, "%Y-%m-%d")
x$admin <- rep('Clinton', nrow(x))
x$admin[as.Date(x$date) > '2001-01-19'] <- 'Bush'
x$admin[as.Date(x$date) > '2009-01-19'] <- 'Obama'
x$admin[as.Date(x$date) > '2017-01-19'] <- 'Trump'
x$year <- substr(x$date, 1, 4)
x <- subset(x, date < '2021-01-20')

table(x$significant)
x$sig <- x$significant
x$sig[x$significant == 'true'] <- 1
x$sig[x$significant == 'false'] <- 0
table(x$sig)

x$rin <- gsub('^(.*?);.*', '\\1', x$regulation_id_numbers)

x$agency.code <- substr(x$rin, 1, 4)
x <- subset(x, !is.na(x$agency.code))
## counter for the number of rules
x$counter <- rep(1, nrow(x))

## fix action
length(unique(x$action))
x$action.new <- x$action
x$action.new <- gsub('ACTION:', '', x$action.new)
x$action.new <- trimws(x$action.new)
x$action.new <- gsub('  +', ' ', x$action.new)
x$action.new <- gsub('\\.$', '', x$action.new)
x$action.new <- tolower(x$action.new)
length(unique(x$action.new))

## categories to isolate final rules
x$action.new[grep('rescission|removal|commentary|temporary|technical amend|notice of enforcement|bridge|catch|final special conditions|withdraw|quota|immediate final rule|notice of availability|closure|clarification|sunset review|retention limits|delay|information collection|deferral|petition|correct|clarification|conditional|confirmation|compliance date|compliance deadline|extension|extend|expiration|change of|approval of|request for|court order|adjustments|modification|stay|interim|direct|information collection|advisory opinion|^notice$|interpret|notification of|notice of.*date|small entity compliance guide|meeting|guid', x$action.new)] <- 'direct.final.etc'
length(unique(x$action.new))

## ASSUMPTION: what counts as a final rule
## drop.direct.rules = 0
drop.direct.rules = 1
if(drop.direct.rules == 0) {
    x$final.rule <- rep(1, nrow(x))
} else {
    x$final.rule <- rep(0, nrow(x))
    x$final.rule[grep('final rule|final reg|final order|final policy', x$action.new)] <- 1
}
table(x$final.rule)

## search for deregulatory action
## keywords associated with deregulation
keywords <- c("remove", "eliminate", "repeal", "reduce", "lower", "decrease",
              "simplify", "streamline", "ease", "exempt", "exclusion", "waive",
              "deregulate", "flexibility", "burden reduction", ## "flexibility act",
              "relief", "less burdensome", "sunset", "expiration", "reform", # "modernize"
              "cost savings", "efficiency", "remove barriers",
              "rescind", "decreased oversight", "less supervision", "voluntary compliance")
# Combine the keywords into a single regex pattern
pattern <- paste(keywords, collapse = "|")
x$dereg <- rep(0, nrow(x))
x$dereg[grep(pattern, x$abstract, ignore.case = TRUE)] <- 1
x$counter.regulatory <- x$counter
x$counter.regulatory[x$dereg == 1] <- 0
x$final.regulatory <- x$final.rule
x$final.regulatory[x$dereg == 1] <- 0

x$n.pages <- x$page_length
x$n.pages.regulatory <- x$page_length
x$n.pages.regulatory[x$dereg == 1] <- 0
x$n.pages.final.rule <- x$n.pages
x$n.pages.final.rule[x$final.rule == 0] <- 0

x$n.pages.final.rule.regulatory <- x$n.pages
x$n.pages.final.rule.regulatory[x$final.rule == 0] <- 0
x$n.pages.final.rule.regulatory[x$dereg == 1] <- 0

## significant
x$n.pages.final.rule.sig <- x$n.pages
x$n.pages.final.rule.sig[x$final.rule == 0] <- 0
x$n.pages.final.rule.sig[x$sig != 1] <- 0

## significant & regulatory
x$n.pages.final.rule.sig.reg <- x$n.pages
x$n.pages.final.rule.sig.reg[x$final.rule == 0] <- 0
x$n.pages.final.rule.sig.reg[x$sig != 1] <- 0
x$n.pages.final.rule.sig.reg[x$dereg == 1] <- 0
## x$n.pages.final.rule.sig[x$dereg == 1] <- 0

g <- g[g$stage == 'Proposed Rule',] # focus on reivew of proposal
x$n.pages.no.review <- x$n.pages  ## this version does not drop when x$final.rule == 0
x$n.pages.no.review[x$rin %in% unique(g$rin)] <- 0 # drop pages from reviewed rules
x$n.pages.no.review.final <- x$n.pages.no.review
x$n.pages.no.review.final[x$final.rule == 0] <- 0 # drop non-final rules
x$n.pages.no.review.final.dereg <- x$n.pages.no.review.final
x$n.pages.no.review.final.dereg[x$dereg == 1] <- 0 # drop de-regulatory rules

robust = 0
if(robust == 1) {
    x <- x[x$year != 2001 &
           x$year != 2009 &
           x$year != 2017,]
}

z <- aggregate(cbind(counter, counter.regulatory, final.rule, n.pages,
                     n.pages.final.rule, n.pages.final.rule.regulatory,
                     n.pages.regulatory, final.regulatory,
                     n.pages.final.rule.sig, n.pages.final.rule.sig.reg,
                     n.pages.no.review, n.pages.no.review.final,
                     n.pages.no.review.final.dereg) ~
               agency.code + admin, data = x, sum)
w <- aggregate(dereg ~ agency.code + admin, data = x, mean)
z <- merge(z, w, by = c('agency.code', 'admin'))

## y$agency.code %in% z$agency.code # all present

y <- merge(y, z, by = c('agency.code', 'admin'), all.x = T)
y[is.na(y)] <- 0

y$pres.ideal <- rep(.693, nrow(y))
y$pres.ideal[y$admin == 'Clinton'] <- -.438
y$pres.ideal[y$admin == 'Obama'] <- -.358
y$pres.ideal[y$admin == 'Trump'] <- .403
y$distance <- abs(y$pres.ideal - y$est)

year.adjust <- function(var, admin) {
    var[admin == 'Clinton'] <- var[admin == 'Clinton'] / 6 # (1995 - 2000)
    var[admin == 'Obama'] <- var[admin == 'Obama'] / 8
    var[admin == 'Bush'] <- var[admin == 'Bush'] / 8
    var[admin == 'Trump'] <- var[admin == 'Trump'] / 4
    return(var)
}

if(robust == 1) {
    year.adjust <- function(var, admin) {
        var[admin == 'Clinton'] <- var[admin == 'Clinton'] / 6 # (1995 - 2000)
        var[admin == 'Obama'] <- var[admin == 'Obama'] / 7
        var[admin == 'Bush'] <- var[admin == 'Bush'] / 7
        var[admin == 'Trump'] <- var[admin == 'Trump'] / 3
        return(var)
    }
}

y$counter.adj <- year.adjust(y$counter, y$admin)
y$counter.regulatory.adj <- year.adjust(y$counter.regulatory, y$admin)
y$final.rule.adj <- year.adjust(y$final.rule, y$admin)
y$final.regulatory.adj <- year.adjust(y$final.regulatory, y$admin)
y$n.pages.adj <- year.adjust(y$n.pages, y$admin)
y$n.pages.regulatory.adj <- year.adjust(y$n.pages.regulatory, y$admin)

y$n.pages.final.rule.adj <- year.adjust(y$n.pages.final.rule, y$admin)
y$n.pages.final.rule.regulatory.adj <- year.adjust(y$n.pages.final.rule.regulatory, y$admin)

y$n.pages.final.rule.sig.adj <- year.adjust(y$n.pages.final.rule.sig, y$admin)
y$n.pages.final.rule.sig.reg.adj <- year.adjust(y$n.pages.final.rule.sig.reg, y$admin)


y$n.pages.no.review.adj <- year.adjust(y$n.pages.no.review, y$admin)
y$n.pages.no.review.final.adj <- year.adjust(y$n.pages.no.review.final, y$admin)
y$n.pages.no.review.final.dereg.adj <- year.adjust(y$n.pages.no.review.final.dereg, y$admin)


nrow(y)
a <- aggregate(est ~ agency.id, data = y, mean)
colnames(a)[2] <- 'est.avg'
y <- merge(y, a, by = 'agency.id')
y$liberal.agency <- rep(0, nrow(y))
y$liberal.agency[y$est.avg < 0] <- 1


agency.fe <- c('Agency Fixed Effects', rep('\\checkmark', 5))

n <- c('President-Agency Distance')


m1 <- glm.nb(as.integer(n.pages.final.rule.adj) ~ distance + agency.id, data = y, weights = 1/se^2)

m2 <- glm.nb(as.integer(n.pages.final.rule.sig.adj) ~ distance + agency.id, data = y, weights = 1/se^2)
m3 <- glm.nb(as.integer(n.pages.final.rule.sig.reg.adj) ~ distance + agency.id, data = y, weights = 1/se^2)

m4 <- glm.nb(as.integer(n.pages.no.review.final.adj) ~ distance + agency.id, data = y, weights = 1/se^2)

m5 <- glm.nb(as.integer(n.pages.no.review.final.dereg.adj) ~ distance + agency.id, data = y,
             weights = 1/se^2)

## exp(coef(m1)['distance']) - 1
## exp(coef(m2)['distance']) - 1
## exp(coef(m3)['distance']) - 1

options(digits=3)

stargazer(m1, m2, m3, m4, m5,
          ## type = 'text',
          out = 'results/table2.tex',
          omit = 'Constant|agency',
          label = 'tbl:reg',
          digits = 3,
          title = 'Ideological Distance and Regulatory Productivity',
          dep.var.labels = c('Formal', 'formal and sig', 'form, sig and Reg.', 'formal', 'formal and reg'),
          star.cutoffs = c(.1, .05, .01),
          star.char = c('^\\cdot', '*', '**'),
          covariate.labels = n,
          model.names = F,
          add.lines = list(agency.fe),
          no.space = TRUE,
          font.size = 'small',
          omit.stat=c("f", 'ser', 'adj.rsq', 'aic', 'bic', 'theta'))



####  TABLE 3 (appendix table of agency names)

y <- data.frame(summary(hmc.fit.static, pars=c('xagency'), probs = c(.05,.95), digits=3)$summary)
y <- y[,c(1,4,5)]
colnames(y) <- c('est', 'lower', 'upper')
y <- y[-10,] # drop benchmark

x <- read.csv(file = 'data/estimation_data.csv')
x <- subset(x, x$agency.id != 'BENCHMARK')

# get dept names
a <- read.csv(file = 'data/ua_data/agencycodes.csv')
a$agency.id <- paste(a$agency.abb, a$bureau.abb, sep = '-')
a <- subset(a, select = c(agency.id, agency, bureau, agency.abb, bureau.abb) )
a <- a[!duplicated(a),]
x <- merge(x, a, by = c('agency.id'), all.x = T)

## recode agency names
x$agency.id.new <- x$agency.id
x$agency.id.new[x$agency.abb == x$bureau.abb] <- x$agency.abb[x$agency.abb == x$bureau.abb]
x$agency.parent.new <- x$agency
x$agency.parent.new[x$agency.abb == x$bureau.abb] <- ''
z <- x[!duplicated(x$agency.id.new), c('agency.id.new', 'agency.parent.new', 'bureau')]
colnames(z) <- c('AgencyIdentifier', 'Department', 'Agency')
z$Department <- gsub('Department of|the ', '', z$Department)

z <- z[order(z$AgencyIdentifier),]
z$ideal <- y$est
z <- z[order(z$Department),]
z <- z[,c(1,4,2,3)]

# edit text to fit in table
z$Department <- trimws(z$Department)
z$Agency[z$AgencyIdentifier == 'FAR'] <- 'Federal Acquisition Regulatory Council'
z$Department[z$AgencyIdentifier == 'STATE'] <- 'State'
z$Department[z$Department == 'Environmental Protection Agency'] <- 'EPA'
z$Department[z$Department == 'Agriculture'] <- 'Agric.'
z$Department[z$Department == 'Housing and Urban Development'] <- 'Housing'
z$Department[z$Department == 'Health and Human Services'] <- 'Health'
z$Department[z$Department == 'Homeland Security'] <- 'Home. Sec.'
z$Department[z$Department == 'Transportation'] <- 'Trans.'
z$Department[z$Department == 'Transportation'] <- 'Trans.'
z$Agency <- gsub('Administration', 'Admin\\.', z$Agency)
z$Agency <- gsub('Office of', '', z$Agency)
z$Agency <- gsub('United States', 'U\\.S\\.', z$Agency)
z$Agency <- gsub('and', '&', z$Agency)

print(xtable(z),
      include.rownames = FALSE,
      file = 'results/appendix_table.tex')



#### SUPPLEMENTARY APPENDIX



####   - TABLE A1

x <- read.csv(file = 'data/estimation_data.csv')
x <- subset(x, x$agency.id != 'BENCHMARK')

# get dept names
a <- read.csv(file = 'data/ua_data/agencycodes.csv')
a$agency.id <- paste(a$agency.abb, a$bureau.abb, sep = '-')
a <- subset(a, select = c(agency.id, agency, bureau, agency.abb, bureau.abb) )
a <- a[!duplicated(a),]
x <- merge(x, a, by = c('agency.id'), all.x = T)

x$agency.id.new <- x$agency.id
x$agency.id.new[x$agency.abb == x$bureau.abb] <- x$agency.abb[x$agency.abb == x$bureau.abb]

## create agency name table (APPENDIX TABLE 1)
x$agency.parent.new <- x$agency
x$agency.parent.new[x$agency.abb == x$bureau.abb] <- ''


y <- subset(x, select = c(agency.id.new, admin, sig, review))

library(dplyr)
library(tidyr)
library(xtable)

collapsed_data <- y %>%
  group_by(admin, agency.id.new) %>%
  summarise(
    sig_mean = mean(sig, na.rm = TRUE),
    review_mean = mean(review, na.rm = TRUE),
    counter = n()
  ) %>%
  ungroup()

# convert to wide format
collapsed_pivot <- collapsed_data %>%
  pivot_wider(
    names_from = admin,
    values_from = c(sig_mean, review_mean, counter),
    names_glue = "{admin}_{.value}"
  )

z <- as.data.frame(collapsed_pivot)

# Order the columns
ordered_columns <- c(
  "agency.id.new",
  "clinton_sig_mean", "clinton_review_mean", "clinton_counter",
  "bush43_sig_mean", "bush43_review_mean", "bush43_counter",
  "obama_sig_mean", "obama_review_mean", "obama_counter",
  "trump_sig_mean", "trump_review_mean", "trump_counter"
)

# Reorder the dataframe columns
z <- z[, ordered_columns]

rownames(z) <- NULL
print(xtable(z),
      include.rownames = F,
      file = 'results/tableA1.tex')


####   - TABLE A2


x <- read.csv('data/merged_ideal_points.csv')

y <- data.frame(department = x$Department, sapply(x[,c(2:6)], rank))
y$department <- gsub('Department of |al Protection Agency| and Human Services|the | and Urban Development', '', y$department)

current.paper <- y[order(y$current.paper),'department']
rcl <- y[order(y$rcl),'department']
clinton.lewis <- y[order(y$clinton.lewis),'department']
chen.johnson <- y[order(y$chen.johnson),'department']
clinton.et.al <- y[order(y$chen.johnson),'department']

m <- data.frame(current.paper, rcl, chen.johnson, clinton.et.al, clinton.lewis)

print(xtable(m),
       file = 'results/tableA2.tex')



####   - TABLE A3 (validation with public comments)


## hand collected and coded public comments from regulations.gov
x <- read.csv('data/comments_handcoded.csv')

v <- with(x, table(admin, agency, position))
v <- as.data.frame(ftable(v))
v <- v[order(v$admin, v$agency),]

print(xtable(v),
       file = 'results/tableA3.tex', row.names = FALSE)


####   - FIGURE A1 (correlation between sample size and credible intervals)


library(ggplot2)

y <- read.csv(file = 'data/estimation_data.csv')
y <- subset(y, select = c(agency.id, admin, sig))
y <- aggregate(sig ~ agency.id + admin, data = y, length)
colnames(y)[3] <- 'n.rules'
y$admin[y$admin == 'bush43'] <- 'bush'

x <- read.csv(file = 'data/dynamic_estimates.csv')
x$admin <- tolower(x$admin)
x$agency.id[x$agency.id != 'BENCHMARK'] <- paste(x$dept[x$agency.id != 'BENCHMARK'],
                                                 x$agency[x$agency.id != 'BENCHMARK'], sep = '-')

m <- merge(x, y, by = c('agency.id', 'admin'))
m$ci.width <- abs(m$lower - m$upper)

pdf('results/figureA1.pdf')
ggplot(m, aes(x = n.rules, y = ci.width)) +
  geom_point(aes(color = ifelse(agency.id == "BENCHMARK", "Benchmark", "Actual")), alpha = 0.7) +
  scale_color_manual(
    name = "Agency Type",
    values = c("Benchmark" = "salmon", "Actual" = "black")
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10() +
  labs(
    x = "Number of Proposed Rules (Log Scale)",
    y = "Width of Bayesian Credible Interval",
    title = ""
  ) +
  theme_minimal()
dev.off()


####   - FIGURE A2 (scatter plot of richardson scores)


x <- read.csv(file = 'data/rcl_ideal_points_merged.csv')

library(ggplot2)
library(ggpubr)
library(wCorr)

x <- subset(x, !is.na(x$ideo_rating))

x <- x[-grep('VA|DODOASHA|DEA|FWS', x$agency.id),]


correlation <- cor(x$est, x$ideo_rating, use = 'complete.obs')
# Calculate the weighted correlation

x$weights <- 1 / (x$se + x$ideo_sd)
weighted_corr <- weightedCorr(x$est, x$ideo_rating, weights = x$weights, method = "pearson")
## print(weighted_corr)

x$agency.id[x$agency.id == 'EPA-AR'] <- 'EPA-AIR'

# Create the scatter plot
plot <- ggplot(x, aes(x = est, y = ideo_rating)) +
  geom_text(aes(label = agency.id), check_overlap = TRUE, vjust = -0.5, size = 3) +  # Add labels
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "salmon") +  # Add 45-degree line
  ## ggtitle("Scatter Plot with Correlation and Labels") +
  labs(x = "Agency Ideal Points (Current Paper)", y = "Agency Ideal Points (Richardson, Clinton and Lewis 2018)") +  # Add axis labels
theme_minimal() +
annotate("text", vjust = 0, hjust = 0,
         x = c(-2.5, -1.69, -1.52, 1.72),
         y = c(.66, .73, 1.78, -1.58),
         label = c('VA', 'DOD-OASHA', 'DOJ-DEA', 'DOI-FWS'),
         size = 3, color = 'steelblue')


# Add the correlation to the plot
plot_with_corr <- plot +
geom_label(aes(x = Inf, y = Inf, label = paste("Weighted Correlation:", round(weighted_corr, 2))),
           hjust = 1.1, vjust = 1.1, size = 5, color = "steelblue", fontface = "bold", fill = "white",
           alpha = 0.7, label.size = 0.5)

## Print the plot
pdf('results/figureA2.pdf')
print(plot_with_corr)
dev.off()


####   - FIGURE A3 (correlation plot with other measures)


library(GGally)

x <- read.csv(file = 'data/merged_ideal_points.csv')


pdf('results/figureA3.pdf')
ggpairs(x[, c("current.paper", "rcl", "clinton.lewis", "chen.johnson", "clinton.et.al")],
        columnLabels = c('Current Paper', 'RCL (2018)', 'Clinton Lewis (2008)',
                         'Chen Johnson (2015)', 'Clinton et al (2012)'),
        lower = list(continuous="smooth")) +
        ggplot2::theme(axis.text = element_blank())
dev.off()









