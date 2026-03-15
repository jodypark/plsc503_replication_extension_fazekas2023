## ------------------------------------------------
##                   OIRA DATA
## ------------------------------------------------



library(XML)

## 1. import and combine XML files

f <- list.files(pattern = 'EO_',
                path = './data/oira_data/raw_data',
                full.names = TRUE)
y <- c()
for(i in 1:length(f)) {
  x <- xmlRoot(xmlTreeParse(f[i], useInternalNodes=T))
  x <- xmlApply(x, function(z) xmlSApply(z, xmlValue))
  x <- lapply(x, function(z) {
    z <- t(as.matrix(z))
    if(!'DECISION' %in% colnames(z)) z <- cbind(z, DECISION = 'Consistent without Change')
    if(!'DATE_PUBLISHED' %in% colnames(z)) z <- cbind(z, DATE_PUBLISHED = NA)
    z <- subset(z, select = c(AGENCY_CODE, RIN, TITLE, STAGE, ECONOMICALLY_SIGNIFICANT,
                     DATE_RECEIVED, LEGAL_DEADLINE, DATE_COMPLETED, DECISION, DATE_PUBLISHED))
    return(as.data.frame(z, stringsAsFactors = F))
  })
  y <- rbind(y, do.call('rbind', x) )
}

## create RData file to use in later regulatory productivity application (Table 2)
oira.data <-  y
save(oira.data, file = 'data/oira_data/oira_data.RData')

## 2. recode data

colnames(y) <- c('agency.code', 'rin', 'rule', 'stage', 'econ.sig', 'date.received',
                 'legal.dline', 'date.completed', 'decision', 'date.published')
y <- subset(y, select = -c(rule))
# fix dates
y$date.received <- as.Date(y$date.received, "%Y-%m-%d")
y$date.completed <- as.Date(y$date.completed, "%Y-%m-%d")
# adminstration completed
admin.complete <- rep('reagan', nrow(y))
admin.complete[as.Date(y[,'date.completed']) >= '1989-01-20'] <- 'bush41'
admin.complete[as.Date(y[,'date.completed']) >= '1993-01-20'] <- 'clinton'
admin.complete[as.Date(y[,'date.completed']) >= '2001-01-20'] <- 'bush43'
admin.complete[as.Date(y[,'date.completed']) >= '2009-01-20'] <- 'obama'
admin.complete[as.Date(y[,'date.completed']) >= '2017-01-20'] <- 'trump'
admin.complete[as.Date(y[,'date.completed']) >= '2021-01-20'] <- 'biden'
y <- cbind(y, admin.complete)
# administration review is initiated
admin <- rep('reagan', nrow(y))
admin[as.Date(y[,'date.received']) >= '1989-01-20'] <- 'bush41'
admin[as.Date(y[,'date.received']) >= '1993-01-20'] <- 'clinton'
admin[as.Date(y[,'date.received']) >= '2001-01-20'] <- 'bush43'
admin[as.Date(y[,'date.received']) >= '2009-01-20'] <- 'obama'
admin[as.Date(y[,'date.received']) >= '2017-01-20'] <- 'trump'
admin[as.Date(y[,'date.received']) >= '2021-01-20'] <- 'biden'
y <- cbind(y, admin)

## recode decision variable:
y$decision[y$decision == 'Consistent with Change'] <- 'change'
# accept
y$decision[y$decision == 'Consistent without Change'] <- 'accept'
y$decision[y$decision == 'Statutory or Judicial Deadline'] <- 'accept'
y$decision[y$decision == 'Emergency'] <- 'accept'
y$decision[y$decision == 'Exempt from Executive Order'] <- 'accept'
# return
y$decision[y$decision == 'Withdrawn'] <- 'return'
y$decision[y$decision == 'Returned for Reconsideration'] <- 'return'
y$decision[y$decision == 'Improperly Submitted'] <- 'return'
y$decision[y$decision == 'Suspended Review'] <- 'return'


## prepare merge
y$review <- rep(1, nrow(y) )
## get first review for each admin
y$date.received <- as.Date(y$date.received)

#
reagan <- subset(y, y$admin == 'reagan')
reagan <- reagan[order(reagan$rin, reagan$date.received),] # earliest date is first
reagan <- subset(reagan, !duplicated(reagan$rin) )
#
bush41 <- subset(y, y$admin == 'bush41')
bush41 <- bush41[order(bush41$rin, bush41$date.received),]
bush41 <- subset(bush41, !duplicated(bush41$rin) )
#
clinton <- subset(y, y$admin == 'clinton')
clinton <- clinton[order(clinton$rin, clinton$date.received),]
clinton <- subset(clinton, !duplicated(clinton$rin) )
#
bush43 <- subset(y, y$admin == 'bush43')
bush43 <- bush43[order(bush43$rin, bush43$date.received),]
bush43 <- subset(bush43, !duplicated(bush43$rin) )
#
obama <- subset(y, y$admin == 'obama')
obama <- obama[order(obama$rin, obama$date.received),]
obama <- subset(obama, !duplicated(obama$rin) )
##
trump <- subset(y, y$admin == 'trump')
trump <- trump[order(trump$rin, trump$date.received),]
trump <- subset(trump, !duplicated(trump$rin) )
##
biden <- subset(y, y$admin == 'biden')
biden <- biden[order(biden$rin, biden$date.received),]
biden <- subset(biden, !duplicated(biden$rin) )
##
y <- rbind(reagan, bush41); y <- rbind(y, clinton); y <- rbind(y, bush43)
y <- rbind(y, obama); y <- rbind(y, trump); y <- rbind(y, biden)
nrow(y)
#
colnames(y)[colnames(y) == 'stage'] <- 'oira.stage'
y <- subset(y, select = c(admin, rin, review, decision, oira.stage, date.received) )


## save file
write.csv(y, file = 'data/oira_data/oira_data.csv', row.names = F)

