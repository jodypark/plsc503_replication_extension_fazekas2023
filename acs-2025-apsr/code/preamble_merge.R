## ---------------------------------
##
##  PREAMBLE: MERGE OIRA AND UA DATA
##
## ---------------------------------




#####     - unified agenda data


## 1.  combine proposed and final rules


load('data/ua_data/ua_data_final_rules.RData')
start.year <- 1983
x <- subset(x, select = -c(cfr, rule))
x <- as.data.frame(x, stringsAsFactors = F)
y <- x
load('data/ua_data/ua_data_all_rules.RData')
x <- subset(x, select = -c(cfr, rule))
x <- as.data.frame(x, stringsAsFactors = F)
##
x[,'date.final'][as.Date(x[,'date.final']) > as.Date('2023-1-1')] <- '2023-1-1'
x[,'date.initiated'][as.Date(x[,'date.initiated']) > as.Date('2023-1-1')] <- '2023-1-1'
##
y <- subset(y, select = c(rin, date.complete, year, admin))
colnames(y)[3:4] <- c('year.complete', 'admin.complete')
colnames(x)[colnames(x) == 'year']  <- 'year.init'
colnames(x)[colnames(x) == 'admin'] <- 'admin.init'
x$year.withdrawn <- as.numeric(substr(x$date.withdrawn, 1, 4))
x$year.nprm <- as.numeric(substr(x$date.nprm, 1, 4))
z <- merge(x, y, by = 'rin', all.x = T)

z <- subset(z, select = c(rin, agency.code, bureau.abb, agency.abb, priority,
                 nprm, date.nprm, year.nprm, year.init,
                 admin.init, date.initiated, date.final, comment, date.comment,
                 admin.complete, year.complete, date.complete, merged,
                 withdrawn, date.withdrawn, year.withdrawn, interim.final,
                 direct.final, ua.date.first, ua.date.last,
                 nprm.planned, date.nprm.planned))
z$completed <- ifelse(is.na(z$year.complete), 0, 1)

## add agency and bureau names
b <- read.csv('data/ua_data/agencycodes.csv')
b <- subset(b, select = c(agency.code, bureau.abb, agency, bureau, agency.abb))
b$agency.id <- paste(b$agency.abb, b$bureau.abb, sep = '-')
b <- subset(b, select = c(agency.id, agency, bureau))
colnames(b) <- c('agency.id', 'agency.parent', 'agency.name')
b <- b[!duplicated(b$agency.id),]

z$agency.id <- paste(z$agency.abb, z$bureau.abb, sep = '-')
x <- merge(b, z, by = 'agency.id')

nrow(x)
x$withdrawn <- as.numeric(x$withdrawn)
x$completed <- as.numeric(x$completed)
x$agency.id <- paste(x$agency.abb, x$bureau.abb, sep = '-')
## focus on only planned nprms
x <- subset(x, x$nprm.planned == 1)

year.fix <- function(d) {
  d <- as.Date(d)
  year <- substr(d, 1, 4)
  threshold <- as.Date(paste(year, '-01-20', sep = ''))
  year <- ifelse(as.Date(d) < as.Date(threshold), as.numeric(year) - 1, as.numeric(year))
  return(year)
}

x$year <- year.fix(x$date.nprm.planned)
nrow(x)

x$commission <- ifelse(x$agency.abb == 'CFTC' | x$agency.abb ==  'SEC' |
                       x$agency.abb == 'FCC' | x$agency.abb == 'CPSC' |
                       x$agency.abb ==  'FTC' | x$agency.abb == 'ICC' |
                       x$agency.abb == 'FMC' | x$agency.abb == 'NRC' |
                       x$agency.id == 'DOE-FERC' | x$agency.id == 'NIGC-NIGC' |
                       x$agency.id == 'FDIC-FDIC' | x$agency.id == 'DOT-STB' |
                       x$agency.id == 'FCA-FCA', 1, 0)

x <- subset(x, x$commission == 0)
nrow(x)

x$econ.sig <- ifelse(x$priority == 'Economically Significant', 1, 0)
x$sig <- ifelse(x$priority == 'Other Significant', 1, 0)
x$substantive <- ifelse(x$priority == 'Substantive, Nonsignificant', 1, 0)

admin <- rep('reagan', nrow(x))
admin[as.Date(x[,'date.nprm.planned']) > '1989-01-20'] <- 'bush41'
admin[as.Date(x[,'date.nprm.planned']) > '1993-01-20'] <- 'clinton'
admin[as.Date(x[,'date.nprm.planned']) > '2001-01-20'] <- 'bush43'
admin[as.Date(x[,'date.nprm.planned']) > '2009-01-20'] <- 'obama'
admin[as.Date(x[,'date.nprm.planned']) > '2017-01-20'] <- 'trump'
admin[as.Date(x[,'date.nprm.planned']) > '2021-01-20'] <- 'biden'
x <- cbind(x, admin)

## determine how many proposals were reviewed PRIOR to NPRM by SAME ADMIN

x1 <- subset(x, x$admin == 'clinton')
x2 <- subset(x, x$admin == 'bush43')
x3 <- subset(x, x$admin == 'obama')
x4 <- subset(x, x$admin == 'trump')
x5 <- subset(x, x$admin == 'biden')

## import oira data
y <- read.csv(file = 'data/oira_data/oira_data.csv')

## the NPRM is reviewed if the RIN had been reviewed previously by the administration
x1 <- merge(x1, y, by = c('rin', 'admin'), all.x = T)
x1$review <- ifelse(is.na(x1$review), 0, 1)

x2 <- merge(x2, y, by = c('rin', 'admin'), all.x = T)
x2$review <- ifelse(is.na(x2$review), 0, 1)

x3 <- merge(x3, y, by = c('rin', 'admin'), all.x = T)
x3$review <- ifelse(is.na(x3$review), 0, 1)

x4 <- merge(x4, y, by = c('rin', 'admin'), all.x = T)
x4$review <- ifelse(is.na(x4$review), 0, 1)

x5 <- merge(x5, y, by = c('rin', 'admin'), all.x = T)
x5$review <- ifelse(is.na(x5$review), 0, 1)


x <- rbind(x3, rbind(x1, x2))
x <- rbind(x, rbind(x4, x5))

x <- subset(x, !is.na(x$priority))

x$nprm <- as.numeric(x$nprm)

x$abandon <- rep(NA, nrow(x))
x$abandon <- ifelse(x$completed == 0 & x$withdrawn == 0, 1, 0)

x$date.abandon <- rep(NA, nrow(x))
x$date.abandon[x$abandon == 1] <- x$ua.date.last[x$abandon == 1]


admin.nprm <- rep('reagan', nrow(x))
admin.nprm[as.Date(x[,'date.nprm']) > '1989-01-20'] <- 'bush41'
admin.nprm[as.Date(x[,'date.nprm']) > '1993-01-20'] <- 'clinton'
admin.nprm[as.Date(x[,'date.nprm']) > '2001-01-20'] <- 'bush43'
admin.nprm[as.Date(x[,'date.nprm']) > '2009-01-20'] <- 'obama'
admin.nprm[as.Date(x[,'date.nprm']) > '2017-01-20'] <- 'trump'
admin.nprm[as.Date(x[,'date.nprm']) > '2021-01-20'] <- 'biden'
x <- cbind(x, admin.nprm)

x$nprm.same.admin <- ifelse(as.character(x$admin.nprm) == as.character(x$admin), 1, 0)


## identify the withdrawn and abandoned rules
drop <- ifelse(x$abandon == 1 | x$withdrawn == 1, 1, 0)
## keep those that were reviewed or had nprms in the same admin
drop[x$review == 1] <- 0
drop[x$nprm.same.admin == 1] <- 0

x <- subset(x, drop == 0)

y <- x

## IRS rules are not reviewed
y <- subset(y, y$agency.id != 'TREAS-IRS')
## drop secretary levels
y <- subset(y, y$bureau.abb != 'OS' & y$bureau.abb != 'HUDSEC' &
               y$bureau.abb != 'AgSEC' & y$bureau.abb != 'ASPMB')


y$econ.significant <- ifelse(y$priority == 'Economically Significant', 1, 0)
y$significant <- ifelse(y$priority == 'Other Significant', 1, 0)
y$routine <- ifelse(y$priority == 'Routine and Frequent', 1, 0)
y$substantive.nonsig <- ifelse(y$priority == 'Substantive, Nonsignificant', 1, 0)
y$info.admin.other <- ifelse(y$priority == 'Info./Admin./Other', 1, 0)
y <- subset(y, y$routine != 1 & y$info.admin.other != 1)
nrow(y)

n.regs = 25 # avg one reg per year

n <- names(tapply(y$review, y$agency.id, length)[tapply(y$review, y$agency.id, length) > n.regs])
y <- y[y$agency.id %in% n,]

# remove bureaus not in all administrations
r <- unique(y$agency.id[y$admin == 'clinton'])
d <- unique(y$agency.id[y$admin == 'bush43'])
k <- unique(y$agency.id[y$admin == 'obama'])

p <- unique(y$agency.id[y$admin == 'trump'])
q <- unique(y$agency.id[y$admin == 'biden'])

length(d); length(r); length(k); length(p); length(q)

y$admin <- as.character(y$admin)

y$sig <- y$econ.significant + y$significant


#### update on preamble

y <- subset(y, admin != 'biden')

## drop agencies not in all administrations
r <- unique(y$agency.id[y$admin == 'clinton'])
d <- unique(y$agency.id[y$admin == 'bush43'])
k <- unique(y$agency.id[y$admin == 'obama'])
k2 <- unique(y$agency.id[y$admin == 'trump'])

length(d); length(r); length(k); length(k2)
r <- r[r %in% d]
d <- d[d %in% r]
d <- d[d %in% k]
d <- d[d %in% k2]
length(d);
y <- y[y$agency.id %in% d,]

n <- table(y$bureau.abb) >= 40
n <- names(n[n != FALSE])
y <- subset(y, bureau.abb %in% n)

#### CREATE BENCHMARK AGENCY

n.obs <- 100  ## pick number of rules for benchmark

d <- matrix(rep(y[1,], n.obs), n.obs, ncol(y), byrow = TRUE)
d <- as.data.frame(d)
d[] <- lapply(d, function(col) as.character(unlist(col)))
colnames(d) <- colnames(y)
d$agency.id <- rep('BENCHMARK', nrow(d))
k <- n.obs / 4
d$admin <- c(rep('clinton', k), rep('bush43', k),
             rep('obama', k), rep('trump', k))

## generate significance
set.seed(1234)
d$sig <- rbinom(100, 1, prob = .5)
d$review <- ifelse(d$sig == 1, 1, 0) # no over or under reviewing

y <- rbind(y, d)


y <- subset(y, select = c(rin, agency.id, admin, sig, review,
                          date.received, date.nprm))


write.csv(y,
          file = 'data/estimation_data.csv',
          row.names = F)




