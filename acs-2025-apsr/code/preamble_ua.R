## -------------------------------
## RAW UNIFIED AGENDA DATA
## CONVERT XML FILES TO CSV FORMAT
## -------------------------------



library(XML)

#### xml files (1983 - 2003)


f <- list.files(pattern = 'Unified',
                path = './data/ua_data/raw_data',
                full.names = TRUE)
f <- f[-grep('cut', f)]

for(i in 1:length(f)) {

    ## deal with malformed XML
    if(length(grep('1988_April', f[i])) > 0)  {
    ## if(f[i] == "1988_April_Unified_Agenda.xml") {
        x <- xmlRoot(xmlTreeParse('data/ua_data/raw_data/1988_April_Unified_Agenda_cut.xml', useInternalNodes=T)) }
  # regular HTML
    if(length(grep('1988_April', f[i])) == 0)  {
  ## if(f[i] != "1988_April_Unified_Agenda.xml") {
    x <- xmlRoot(xmlTreeParse(f[i], useInternalNodes=T)) }

  n <- length(names(x))
# subset some regs with less than full information
  omit <- c()
  for(k in 1:n) omit[k] <- length(names(x[[k]][[1]][['RIN_INFO']]))
  p <- as.logical(ifelse(omit == 35, 1, 0))
  x <- x[p]

# storage matrix
  dta <- c()
  for(j in 1:length(x)) {
    rin <- xmlValue(x[[j]][[1]][['RIN']])
    rule <- xmlValue(x[[j]][[1]][['RIN_INFO']][['RULE_TITLE']])
    ua.date <- f[i]
    agency <- xmlValue(x[[j]][[1]][['RIN_INFO']][['AGENCY']])
    stage <- xmlValue(x[[j]][[1]][['RIN_INFO']][['RULE_MAKING_STAGE']])
    major <- xmlValue(x[[j]][[1]][['RIN_INFO']][['MAJOR']])
    priority <- xmlValue(x[[j]][[1]][['RIN_INFO']][['PRIORITY_CATEGORY']])
    abstract <- xmlValue(x[[j]][[1]][['RIN_INFO']][['ABSTRACT']])
    procurement <- ifelse(length(grep(' procurement', abstract)) > 0, 1, 0)

# time table information
# calculate total number of actions
    k <- length(names(x[[j]][[1]][['RIN_INFO']][['TIMETABLE_LIST']]))
# collect most recent action -- using max(k)
    if(k > 0) {
      action.id <- c(); action <- c(); dates <- c(); fr.citation <- c()
      for(m in 1:k){
        action.id[m]  <- xmlValue(x[[j]][[1]][['RIN_INFO']][['TIMETABLE_LIST']][[m]][['TTBL_ACTION']])
        action[m]  <- xmlValue(x[[j]][[1]][['RIN_INFO']][['TIMETABLE_LIST']][[m]][['TTBL_ACTION_QUALIFIER']])
        dates[m] <- xmlValue(x[[j]][[1]][['RIN_INFO']][['TIMETABLE_LIST']][[m]][['TTBL_DATE']])
        fr.citation[m] <- xmlValue(x[[j]][[1]][['RIN_INFO']][['TIMETABLE_LIST']][[m]][['FR_CITATION']])
      }
      action.id <- paste(action.id, collapse='> ')
      action <- paste(action, collapse='> ')
      dates <- paste(dates, collapse='> ')
      fr.citation <- paste(fr.citation, collapse='> ')
    }
    if(k == 0) { action.id <- NA; action <- NA; dates <- NA; fr.citation <- NA}

    k <- length(names(x[[j]][[1]][['RIN_INFO']][['SMALL_ENTITIES_LIST']]))
    if(k > 0) {
      small.ent <- c()
      for(g in 1:k) {
        small.ent[g] <- xmlValue(x[[1]][[1]][['RIN_INFO']][['SMALL_ENTITIES_LIST']][[g]])
      }
      small.ent <- paste(small.ent, collapse=',')
    }
    if(k == 0) small.ent <- NA

    k <- length(names(x[[j]][[1]][['RIN_INFO']][['UNFUNDED_MANDATE_LIST']]))
    if(k > 0) {
      unfun.mandate <- c()
      for(g in 1:k) {
        unfun.mandate[g] <- xmlValue(x[[j]][[1]][['RIN_INFO']][['UNFUNDED_MANDATE_LIST']][[g]])
      }
      unfun.mandate <- paste(unfun.mandate, collapse=',')
    }
    if(k == 0) unfun.mandate <- NA

    k <- length(names(x[[j]][[1]][['RIN_INFO']][['RFA_ANALYSIS_LIST']]))
    if(k > 0) {
      rfa <- c()
      for(g in 1:k) rfa[g] <- xmlValue(x[[j]][[1]][['RIN_INFO']][['RFA_ANALYSIS_LIST']][[g]])
      rfa <- paste(rfa, collapse=',')
    }
    if(k == 0) rfa <- NA

    k <- length(names(x[[j]][[1]][['RIN_INFO']][['LEGAL_AUTHORITY_LIST']]))
    if(k > 0) {
      legal.auth <- c()
      for(g in 1:k) {
        legal.auth[g] <- xmlValue(x[[j]][[1]][['RIN_INFO']][['LEGAL_AUTHORITY_LIST']][[g]])
      }
      legal.auth <- paste(legal.auth, collapse=',')
    }
    if(k == 0) legal.auth <- NA

    k <- length(names(x[[j]][[1]][['RIN_INFO']][['GOVT_LEVEL_LIST']]))
    if(k > 0) {
      gov.levels <- c()
      for(g in 1:k) {
        gov.levels[g] <- xmlValue(x[[j]][[1]][['RIN_INFO']][['GOVT_LEVEL_LIST']][[g]])}
      gov.levels <- paste(gov.levels, collapse=',')
    }
    if(k == 0) gov.levels <- NA

    k <- length(names(x[[j]][[1]][['RIN_INFO']][['CFR_LIST']]))
    if(k > 0) {
      cfr <- c()
      for(g in 1:k) cfr[g] <- xmlValue(x[[j]][[1]][['RIN_INFO']][['CFR_LIST']][[g]])
      cfr <- paste(cfr, collapse=',')
    }
    if(k == 0) cfr <- NA

    k <- length(names(x[[j]][[1]][['RIN_INFO']][['LEGAL_DLINE_INFO_LIST']]))
    if(k > 0) {
      legal.dline.type <- c(); legal.dline.date <- c(); legal.dline.desc <- c(); legal.dline.stage <- c()
      for(g in 1:k) {
        legal.dline.type[g] <-
          xmlValue(x[[j]][[1]][['RIN_INFO']][['LEGAL_DLINE_INFO_LIST']][[g]][['DLINE_TYPE']])
        legal.dline.stage[g] <-
          xmlValue(x[[j]][[1]][['RIN_INFO']][['LEGAL_DLINE_INFO_LIST']][[g]][['DLINE_ACTION_STAGE']])
        legal.dline.desc[g] <-
          xmlValue(x[[j]][[1]][['RIN_INFO']][['LEGAL_DLINE_INFO_LIST']][[g]][['DLINE_DESC']])
        legal.dline.date[g] <-
          xmlValue(x[[j]][[1]][['RIN_INFO']][['LEGAL_DLINE_INFO_LIST']][[g]][['DLINE_DATE']])
      }
      legal.dline.type <- paste(legal.dline.type, collapse = ',')
      legal.dline.date <- paste(legal.dline.date, collapse = ',')
      legal.dline.desc <- paste(legal.dline.desc, collapse = ',')
      legal.dline.stage <- paste(legal.dline.stage, collapse = ',')
    }
    if(k == 0) {legal.dline.type <- NA; legal.dline.date <- NA; legal.dline.desc <- NA; legal.dline.stage <- NA}

   dta[j] <- list(c(rin, agency, rule, ua.date, stage, major, priority, action.id, action, dates,
                    fr.citation, small.ent, unfun.mandate, rfa, legal.auth, gov.levels, cfr, procurement,
                    legal.dline.type, legal.dline.date, legal.dline.desc, legal.dline.stage))
  }

  m <- do.call('rbind', dta)
  colnames(m) <- c('rin','agency', 'rule', 'ua.date','stage','major','priority',
                   'action.id','action','dates','fr.citation', 'small.ent','unfun.mandate','rfa',
                   'legal.auth','gov.levels','cfr', 'procurement',
                   'legal.dline.type', 'legal.dline.date', 'legal.dline.desc', 'legal.dline.stage')
    code <- gsub('([0-9]{4}.*)_Unified_Agenda.*', '\\1', f[i])
    code <- gsub('raw_data/', '', code)
  code <- paste(code, 'csv', sep='.')
  write.csv(m, code)
  # clean memory
  rm(x,m,dta,rin, rule, ua.date, agency, stage, major, priority, abstract, procurement,code,
     action.id,action,dates,fr.citation,small.ent,unfun.mandate,rfa,legal.auth,gov.levels,
     legal.dline.type, legal.dline.date, legal.dline.desc, legal.dline.stage)

}


#### xml files (2004)

## several broken lines of XML in 2004 files need to be fixed

f <- list.files(pattern = 'REGINFO_RIN_DATA.*2004',
                path = './data/ua_data/raw_data',
                full.names = TRUE)

for(i in 1:length(f)) {
    ## drop broken lines in each file
    if(length(grep('fall_2004', f[i])) > 0)  {
        m <- readLines(f[i])
        skip <- grep('The FY 2005 budget request', m)
        m <- m[-skip]
        x <- xmlRoot(xmlTreeParse(m, useInternalNodes=T))
    }
    if(length(grep('spring_2004', f[i])) > 0)  {
        m <- readLines(f[i])
        skip <- grep('The Department of the Interior proposes to include', m)
        m <- m[-skip]
        x <- xmlRoot(xmlTreeParse(m, useInternalNodes=T))
    }
       n <- length(names(x))
  # storage matrix
  dta <- c()
  for(j in 1:n) {
    rin <- xmlValue(x[[j]][['RIN']])
    rule <- xmlValue(x[[j]][['RULE_TITLE']])
    ua.date <- f[i]
    agency <- xmlValue(x[[j]][['AGENCY']])
    stage <- xmlValue(x[[j]][['RULE_STAGE']])
    major <- xmlValue(x[[j]][['MAJOR']])
    priority <- xmlValue(x[[j]][['PRIORITY_CATEGORY']])
    rfa <- xmlValue(x[[j]][['RFA_REQUIRED']])
    abstract <- xmlValue(x[[j]][[1]][['RIN_INFO']][['ABSTRACT']])
    procurement <- ifelse(length(grep(' procurement', abstract)) > 0, 1, 0)

    k <- length(names(x[[j]][['TIMETABLE_LIST']]))

    if(k > 0) {
      action <- c(); dates <- c(); action.id <- c(); fr.citation <- c()
      for(m in 1:k){
        action[m]  <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['TTBL_ACTION']])
        dates[m] <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['TTBL_DATE']])
        action.id[m] <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['TTBL_ACTION_QUALIFIER']])
        fr.citation[m] <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['FR_CITATION']])
      }
      action <- paste(action, collapse='> ')
      dates <- paste(dates, collapse='> ')
      action.id <- paste(action.id, collapse='> ')
      fr.citation <- paste(fr.citation, collapse='> ')
    }
    if(k == 0) { action <- NA; dates <- NA; action.id <- NA; fr.citation <- NA}

    k <- length(names(x[[j]][['UNFUNDED_MANDATE_LIST']]))
        if(k > 0) {
            unfun.mandate <- c()
            for(g in 1:k) {
                    unfun.mandate[g] <- xmlValue(x[[j]][['UNFUNDED_MANDATE_LIST']][[g]])
                    }
                    unfun.mandate <- paste(unfun.mandate, collapse=',')
                    }
        if(k == 0) unfun.mandate <- NA

    k <- length(names(x[[j]][['GOVT_LEVEL_LIST']]))
        if(k > 0) {
            gov.levels <- c()
            for(g in 1:k) {
                    gov.levels[g] <- xmlValue(x[[j]][['GOVT_LEVEL_LIST']][[g]])
                    }
                    gov.levels <- paste(gov.levels, collapse=',')
                    }
        if(k == 0) gov.levels <- NA

    k <- length(names(x[[j]][['LEGAL_AUTHORITY_LIST']]))
    if(k > 0) {
      legal.auth <- c()
      for(g in 1:k) legal.auth[g] <- xmlValue(x[[j]][['LEGAL_AUTHORITY_LIST']][[g]])
      legal.auth <- paste(legal.auth, collapse=',')
    }
    if(k == 0) legal.auth <- NA

    k <- length(names(x[[j]][['CFR_LIST']]))
    if(k > 0) {
      cfr <- c()
      for(g in 1:k) cfr[g] <- xmlValue(x[[j]][['CFR_LIST']][[g]])
      cfr <- paste(cfr, collapse=',')
    }
    if(k == 0) cfr <- NA


    k <- length(names(x[[j]][['LEGAL_DLINE_LIST']][['LEGAL_DLINE_INFO']]))
    if(k > 0) {
      legal.dline.type <- c(); legal.dline.date <- c(); legal.dline.desc <- c(); legal.dline.stage <- c()
      legal.dline.type <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_TYPE']])[1]
      legal.dline.stage <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_ACTION_STAGE']])
      legal.dline.desc <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_DESC']])
      legal.dline.date <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_DATE']])
      }
    if(k == 0) {legal.dline.type <- NA; legal.dline.date <- NA; legal.dline.desc <- NA; legal.dline.stage <- NA}


    dta[j] <- list(c(rin, agency, rule, ua.date, stage, major, priority, action.id, action, dates,
                    fr.citation, unfun.mandate, rfa, legal.auth, gov.levels, cfr, procurement,
                     legal.dline.type, legal.dline.date, legal.dline.desc, legal.dline.stage))
  }

  m <- do.call('rbind', dta)

    colnames(m) <- c('rin','agency', 'rule', 'ua.date','stage','major','priority',
                   'action.id','action','dates','fr.citation','unfun.mandate','rfa',
                   'legal.auth','gov.levels','cfr', 'procurement',
                   'legal.dline.type', 'legal.dline.date', 'legal.dline.desc', 'legal.dline.stage')

    if(length(grep('spring_2004', f[i])) > 0) code <- 'data/ua_data/spring_2004.csv'
    if(length(grep('fall_2004', f[i])) > 0) code <- 'data/ua_data/fall_2004.csv'

    write.csv(m, code, row.names=F)
# clean memory
  rm(x,m,dta,rin, rule, ua.date, agency, stage, major, priority, abstract, procurement,code,
     action.id,action,dates,fr.citation,unfun.mandate,rfa,legal.auth,gov.levels,
     legal.dline.type, legal.dline.date, legal.dline.desc, legal.dline.stage)

}

#### xml files (2005 - 2024)


f <- list.files(pattern = 'REGINFO_RIN_DATA_[a-z]',
                path = './data/ua_data/raw_data',
                full.names = TRUE)
f <- f[-grep('2004', f)]

for(i in 1:length(f)) {
    x <- xmlRoot(xmlTreeParse(f[i], useInternalNodes=TRUE))
  n <- length(names(x))
  # storage matrix
  dta <- c()
  for(j in 1:n) {
    rin <- xmlValue(x[[j]][['RIN']])
    rule <- xmlValue(x[[j]][['RULE_TITLE']])
    ua.date <- f[i]
    agency <- xmlValue(x[[j]][['AGENCY']])
    stage <- xmlValue(x[[j]][['RULE_STAGE']])
    major <- xmlValue(x[[j]][['MAJOR']])
    priority <- xmlValue(x[[j]][['PRIORITY_CATEGORY']])
    rfa <- xmlValue(x[[j]][['RFA_REQUIRED']])
    abstract <- xmlValue(x[[j]][[1]][['RIN_INFO']][['ABSTRACT']])
    procurement <- ifelse(length(grep(' procurement', abstract)) > 0, 1, 0)

    k <- length(names(x[[j]][['TIMETABLE_LIST']]))

    if(k > 0) {
      action <- c(); dates <- c(); action.id <- c(); fr.citation <- c()
      for(m in 1:k){
        action[m]  <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['TTBL_ACTION']])
        dates[m] <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['TTBL_DATE']])
        action.id[m] <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['TTBL_ACTION_QUALIFIER']])
        fr.citation[m] <- xmlValue(x[[j]][['TIMETABLE_LIST']][[m]][['FR_CITATION']])
      }
      action <- paste(action, collapse='> ')
      dates <- paste(dates, collapse='> ')
      action.id <- paste(action.id, collapse='> ')
      fr.citation <- paste(fr.citation, collapse='> ')
    }
    if(k == 0) { action <- NA; dates <- NA; action.id <- NA; fr.citation <- NA}

    k <- length(names(x[[j]][['UNFUNDED_MANDATE_LIST']]))
        if(k > 0) {
            unfun.mandate <- c()
            for(g in 1:k) {
                    unfun.mandate[g] <- xmlValue(x[[j]][['UNFUNDED_MANDATE_LIST']][[g]])
                    }
                    unfun.mandate <- paste(unfun.mandate, collapse=',')
                    }
        if(k == 0) unfun.mandate <- NA

    k <- length(names(x[[j]][['GOVT_LEVEL_LIST']]))
        if(k > 0) {
            gov.levels <- c()
            for(g in 1:k) {
                    gov.levels[g] <- xmlValue(x[[j]][['GOVT_LEVEL_LIST']][[g]])
                    }
                    gov.levels <- paste(gov.levels, collapse=',')
                    }
        if(k == 0) gov.levels <- NA

    k <- length(names(x[[j]][['LEGAL_AUTHORITY_LIST']]))
    if(k > 0) {
      legal.auth <- c()
      for(g in 1:k) legal.auth[g] <- xmlValue(x[[j]][['LEGAL_AUTHORITY_LIST']][[g]])
      legal.auth <- paste(legal.auth, collapse=',')
    }
    if(k == 0) legal.auth <- NA

    k <- length(names(x[[j]][['CFR_LIST']]))
    if(k > 0) {
      cfr <- c()
      for(g in 1:k) cfr[g] <- xmlValue(x[[j]][['CFR_LIST']][[g]])
      cfr <- paste(cfr, collapse=',')
    }
    if(k == 0) cfr <- NA

    k <- length(names(x[[j]][['LEGAL_DLINE_LIST']][['LEGAL_DLINE_INFO']]))
    if(k > 0) {
      legal.dline.type <- c(); legal.dline.date <- c(); legal.dline.desc <- c(); legal.dline.stage <- c()
      legal.dline.type <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_TYPE']])[1]
      legal.dline.stage <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_ACTION_STAGE']])
      legal.dline.desc <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_DESC']])
      legal.dline.date <-
        xmlValue(x[[j]][['LEGAL_DLINE_LIST']][[1]][['DLINE_DATE']])
      }
    if(k == 0) {legal.dline.type <- NA; legal.dline.date <- NA; legal.dline.desc <- NA; legal.dline.stage <- NA}


    dta[j] <- list(c(rin, agency, rule, ua.date, stage, major, priority, action.id, action, dates,
                    fr.citation, unfun.mandate, rfa, legal.auth, gov.levels, cfr, procurement,
                     legal.dline.type, legal.dline.date, legal.dline.desc, legal.dline.stage))
  }

  m <- do.call('rbind', dta)
  colnames(m) <- c('rin','agency', 'rule', 'ua.date','stage','major','priority',
                   'action.id','action','dates','fr.citation','unfun.mandate','rfa',
                   'legal.auth','gov.levels','cfr', 'procurement',
                   'legal.dline.type', 'legal.dline.date', 'legal.dline.desc', 'legal.dline.stage')

    code <- gsub('REGINFO_RIN_DATA_(.*[0-9]{4}).*', '\\1', f[i])
      code <- gsub('raw_data/', '', code)
  code <- paste(code, 'csv', sep='.')

  write.csv(m, code, row.names=F)
# clean memory
  rm(x,m,dta,rin, rule, ua.date, agency, stage, major, priority, abstract, procurement,code,
     action.id,action,dates,fr.citation,unfun.mandate,rfa,legal.auth,gov.levels,
     legal.dline.type, legal.dline.date, legal.dline.desc, legal.dline.stage)

}



#### bring data together


f <- tolower(list.files(pattern = 'csv',
                        path = './data/ua_data',
                        full.names = TRUE))
f1 <- f[grep('[0-9]{4}_(october|april)', f)]
f2 <- f[grep('(fall|spring)_[0-9]{4}', f)]
# load first set
x <- read.csv(file=f1[1])
for(i in 2:length(f1)) {
  y <- read.csv(file=f1[i])
  x <- rbind(x, y)
  }
# delete two variables
x <- subset(x, select = -c(X, small.ent))
# load second set
z <- read.csv(file=f2[1])
for(i in 2:length(f2)) {
  y <- read.csv(file=f2[i])
  z <- rbind(z, y)
  }
x <- rbind(x, z)
x <- as.matrix(x)

x[,'ua.date'] <- gsub('\\./data/ua_data/raw_data/', '', x[,'ua.date'])


## error correction; turn '09/00/0000' format dates into all zeros
##  - i.e., replace matches with '00/00/0000'
pattern <- "(0[1-9]|1[0-2])/00/0000"
x[,'dates'] <- gsub(pattern, "00/00/0000", x[,'dates'])
x[,'dates'] <- gsub('To Be Determined', "00/00/0000", x[,'dates'])

## replace years that start with 0
pattern2 <- "(0[1-9]|1[0-2])/\\d{2}/0\\d{3}"
x[,'dates'] <- gsub(pattern2, "00/00/0000", x[,'dates'])

## RECODING

# legal deadlines
legal.dline <- rep(0, nrow(x))
legal.dline[grep('S|J', x[,'legal.dline.type'])] <- 1
x <- subset(x, select = -c(legal.dline.stage, legal.dline.type, legal.dline.date,
                           legal.dline.desc))
x <- cbind(x, legal.dline)

# stage of regulation
x[,'stage'][x[,'stage'] == 0] <- 'No Stage'
x[,'stage'][x[,'stage'] == 1] <- 'PreRule'
x[,'stage'][x[,'stage'] == 2] <- 'Proposed Rule'
x[,'stage'][x[,'stage'] == 3] <- 'Final Rule'
x[,'stage'][x[,'stage'] == 4] <- 'Long-term Action'
x[,'stage'][x[,'stage'] == 5] <- 'Completed Action'

x[,'stage'] <- gsub('Actions', 'Action', x[,'stage'])
x[,'stage'] <- gsub(' Stage', '', x[,'stage'])
x[,'stage'] <- gsub('Long-Term', 'Long-term', x[,'stage'])
x[,'stage'] <- gsub('PreRule', 'Prerule', x[,'stage'])
x[,'stage'] <- gsub('No', 'No Stage', x[,'stage'])

# standardize UA date
year <- gsub('([0-9]{4}).*', '\\1', x[,'ua.date'])
year[grep('^REG', year)] <- gsub('.*([0-9]{4}).*', '\\1', x[,'ua.date'][grep('^REG', year)])
year[grep('^reginfo', year)] <- gsub('.*([0-9]{4}).*', '\\1', x[,'ua.date'][grep('^reginfo', year)])
day <- rep('1', nrow(x))
month <- rep(NA, nrow(x))
month[grep('October|october', x[,'ua.date'])] <- '11'
month[grep('April|april', x[,'ua.date'])] <- '05'
month[grep('Spring|spring', x[,'ua.date'])] <- '05'
month[grep('Fall|fall', x[,'ua.date'])] <- '11'
ua.date.new <- paste(year, month, day, sep = '-')
x <- cbind(x, ua.date.new)
length(ua.date.new[is.na(ua.date.new)]) # 0

## enter UA dates by hand
## when available, dates taken from: https://www.americanactionforum.org/insight/publication-dates-of-the-unified-agenda-of-federal-regulation/

x[,'ua.date.new'][x[,'ua.date.new'] == "1996-05-1"] <- '1996-04-25' # April 25, 1996
x[,'ua.date.new'][x[,'ua.date.new'] == "1996-11-1"] <- '1996-11-18' # November 18, 1996
x[,'ua.date.new'][x[,'ua.date.new'] == "1997-05-1"] <- '1997-04-04' # April 4, 1997
x[,'ua.date.new'][x[,'ua.date.new'] == "1997-11-1"] <- '1997-10-01' # October 1, 1997
x[,'ua.date.new'][x[,'ua.date.new'] == "1998-05-1"] <- '1998-04-15' # April 15, 1998
x[,'ua.date.new'][x[,'ua.date.new'] == "1998-11-1"] <- '1998-10-27' # October 27, 1998
x[,'ua.date.new'][x[,'ua.date.new'] == "1999-05-1"] <- '1999-04-12' # April 12, 1999
x[,'ua.date.new'][x[,'ua.date.new'] == "1999-11-1"] <- '1999-11-09' # November 9, 1999
x[,'ua.date.new'][x[,'ua.date.new'] == "2000-05-1"] <- '2000-04-13' # April 13, 2000
x[,'ua.date.new'][x[,'ua.date.new'] == "2000-11-1"] <- '2000-10-30' # October 30, 2000
x[,'ua.date.new'][x[,'ua.date.new'] == "2001-05-1"] <- '2001-04-25' # April 25, 2001
x[,'ua.date.new'][x[,'ua.date.new'] == "2001-11-1"] <- '2001-11-5' # November 5, 2001
x[,'ua.date.new'][x[,'ua.date.new'] == "2002-05-1"] <- '2002-04-30' # April 30, 2002
x[,'ua.date.new'][x[,'ua.date.new'] == "2002-11-1"] <- '2002-10-30' # October 30, 2002
x[,'ua.date.new'][x[,'ua.date.new'] == "2003-05-1"] <- '2003-05-07' # May 7, 2003
x[,'ua.date.new'][x[,'ua.date.new'] == "2003-11-1"] <- '2003-12-05' # December 5, 2003
x[,'ua.date.new'][x[,'ua.date.new'] == "2004-05-1"] <- '2004-06-17' # June 17, 2004
x[,'ua.date.new'][x[,'ua.date.new'] == "2004-11-1"] <- '2004-11-22' # November 22, 2004
x[,'ua.date.new'][x[,'ua.date.new'] == "2005-05-1"] <- '2005-04-27' # April 27, 2005
x[,'ua.date.new'][x[,'ua.date.new'] == "2005-11-1"] <- '2005-10-07' # October 7, 2005
x[,'ua.date.new'][x[,'ua.date.new'] == "2006-05-1"] <- '2006-04-24' # April 24, 2006
x[,'ua.date.new'][x[,'ua.date.new'] == "2006-11-1"] <- '2006-12-01' # December 1, 2006
x[,'ua.date.new'][x[,'ua.date.new'] == "2007-05-1"] <- '2007-03-29' # March 29, 2007
x[,'ua.date.new'][x[,'ua.date.new'] == "2007-11-1"] <- '2007-11-21' # November 21, 2007
x[,'ua.date.new'][x[,'ua.date.new'] == "2008-05-1"] <- '2008-04-24' # April 24, 2008
x[,'ua.date.new'][x[,'ua.date.new'] == "2008-11-1"] <- '2008-11-12' # November 12, 2008
x[,'ua.date.new'][x[,'ua.date.new'] == "2009-05-1"] <- '2009-04-22' # April 22, 2009
x[,'ua.date.new'][x[,'ua.date.new'] == "2009-11-1"] <- '2009-11-18' # November 18, 2009
x[,'ua.date.new'][x[,'ua.date.new'] == "2010-05-1"] <- '2010-04-07' # April 7, 2010
x[,'ua.date.new'][x[,'ua.date.new'] == "2010-11-1"] <- '2010-11-29' # November 29, 2010
x[,'ua.date.new'][x[,'ua.date.new'] == "2011-05-1"] <- '2011-06-01' # June 1, 2011
x[,'ua.date.new'][x[,'ua.date.new'] == "2011-11-1"] <- '2011-12-19' # December 19, 2011
x[,'ua.date.new'][x[,'ua.date.new'] == "2012-11-1"] <- '2012-12-21' # December 21, 2012
x[,'ua.date.new'][x[,'ua.date.new'] == "2013-05-1"] <- '2013-07-03' # July 3, 2013
x[,'ua.date.new'][x[,'ua.date.new'] == "2013-11-1"] <- '2013-11-26' # November 26, 2013
x[,'ua.date.new'][x[,'ua.date.new'] == "2014-05-1"] <- '2014-05-23' # May  23, 2014
x[,'ua.date.new'][x[,'ua.date.new'] == "2014-11-1"] <- '2014-11-21' # November 21, 2014
x[,'ua.date.new'][x[,'ua.date.new'] == "2015-05-1"] <- '2015-05-21' # May 21, 2015
x[,'ua.date.new'][x[,'ua.date.new'] == "2015-11-1"] <- '2015-11-19' # November 19, 2015
x[,'ua.date.new'][x[,'ua.date.new'] == "2016-05-1"] <- '2016-05-18' # May 18, 2016
x[,'ua.date.new'][x[,'ua.date.new'] == "2016-11-1"] <- '2016-11-17' # November 17, 2016


# code
x <- cbind(x, agency.code = substr(x[,'rin'], 1,4))
a <- read.csv(file = 'data/ua_data/agencycodes.csv',
              stringsAsFactors=F)
a$bureau <- toupper(a$bureau)
a$agency.code[nchar(a$agency.code) == 3] <- paste('0',
                     a$agency.code[nchar(a$agency.code) == 3], sep='')
a <- as.matrix(a[,c('bureau.abb', 'agency.abb', 'agency.code')])
x <- merge(x, a, by='agency.code', all.x=T)

x <- as.matrix(x)

x <- subset(x, select = -agency)

# set max number of actions
count <- nchar(sapply(x[,'action'], function(x) gsub('[^>]', '', x)))
count <- count + 1
count <- as.character(count)
# number of actions
x <- cbind(x, count)

# actions (1 to 9)
action9 <- rep(NA, nrow(x)); action8 <- rep(NA, nrow(x)); action7 <- rep(NA, nrow(x))
action6 <- rep(NA, nrow(x)); action5 <- rep(NA, nrow(x)); action4 <- rep(NA, nrow(x))
action3 <- rep(NA, nrow(x)); action2 <- rep(NA, nrow(x)); action1 <- rep(NA, nrow(x))
# dates (1 to 9)
date9 <- rep(NA, nrow(x)); date8 <- rep(NA, nrow(x)); date7 <- rep(NA, nrow(x))
date6 <- rep(NA, nrow(x)); date5 <- rep(NA, nrow(x)); date4 <- rep(NA, nrow(x))
date3 <- rep(NA, nrow(x)); date2 <- rep(NA, nrow(x)); date1 <- rep(NA, nrow(x))
# federal register citation (1 to 9)
fr.citation9 <- rep(NA, nrow(x)); fr.citation8 <- rep(NA, nrow(x)); fr.citation7 <- rep(NA, nrow(x))
fr.citation6 <- rep(NA, nrow(x)); fr.citation5 <- rep(NA, nrow(x)); fr.citation4 <- rep(NA, nrow(x))
fr.citation3 <- rep(NA, nrow(x)); fr.citation2 <- rep(NA, nrow(x)); fr.citation1 <- rep(NA, nrow(x))
# combine with x
x <- cbind(x, action1, action2, action3, action4, action5, action6, action7, action8, action9,
           date1, date2, date3, date4, date5, date6, date7, date8, date9, fr.citation1,
           fr.citation2, fr.citation3, fr.citation4, fr.citation5, fr.citation6,
           fr.citation7, fr.citation8, fr.citation9)

# delete if (1) no date or (2) no action
x <- subset(x, !is.na(x[,'dates']))
x <- subset(x, !is.na(x[,'action']))

# unpack action/date/fr.citation variables
# (date1, action1 etc are the most recent)
x <- t(apply(x, 1, function(z) {
  # for dates
  d <- unlist(strsplit(z['dates'], split = '> '))
  d <- rev(d)[1:9]
  for(i in 1:length(d)) z[paste('date', i, sep = '')] <- d[i]
  # for action
  d <- unlist(strsplit(z['action'], split = '> '))
  d <- rev(d)[1:9]
  for(i in 1:length(d)) z[paste('action', i, sep = '')] <- d[i]
  # for fr.citation
  d <- unlist(strsplit(z['fr.citation'], split = '> '))
  d <- rev(d)[1:9]
  for(i in 1:length(d)) z[paste('fr.citation', i, sep = '')] <- d[i]
  return(z)
}))


# drop two  variables
x <- subset(x, select = -c(action, action.id, dates))
# put action to lower
x[,colnames(x)[grep('action', colnames(x))]] <- tolower(x[,colnames(x)[grep('action', colnames(x))]])

# create action.type variables
for(i in 1:9) assign(paste('action', i, '.type', sep = ''), rep(NA, nrow(x)))
x <- cbind(x, action1.type); x <- cbind(x, action2.type); x <- cbind(x, action3.type)
x <- cbind(x, action4.type); x <- cbind(x, action5.type); x <- cbind(x, action6.type)
x <- cbind(x, action7.type); x <- cbind(x, action8.type); x <- cbind(x, action9.type)

## decision rules to define an "action.type" for each action
n <- grep('action1$', colnames(x))
for(j in grep('action1.type', colnames(x)):grep('action9.type', colnames(x))) {
  x[,j][(grep('public hearing|public meeting|^hearing$', x[,n]))] <- 'hearing'
  x[,j][(grep('comment.*end', x[,n]))] <- 'comment'
  x[,j][(grep('temporary regulation|temporary standard|temporary rule', x[,n]))] <- 'temp.reg'
  x[,j][(grep('final rule|promulgated|r&o', x[,n]))] <- 'final.rule'
  x[,j][(grep('final action|^final$', x[,n]))] <- 'last.action'
  x[,j][(grep('^anprm$', x[,n]))] <- 'anprm'
  x[,j][(grep('^proposed notice$|^notice$|^noi$|npp|guidelines|guidance|request for info|rfi|notice of inquiry|notice soliciting|proposed determination|notice of preliminary determination|public mailing of proposed rule',
              x[,n]))] <- 'notice'
  #
  x[,j][(grep('nprm|notice of prop|proposed rule', x[,n]))] <- 'nprm'
  x[,j][(grep('anprm', x[,n]))] <- 'anprm'
  x[,j][(grep('interim final|interim rule', x[,n]))] <- 'interim.final'
  x[,j][(grep('direct final|direct rule|final rule with comment', x[,n]))] <- 'direct.final'
  #
  x[,j][(grep('withdraw|^cancelled$|rescission|case closed|delete| terminat|^terminat|discontinue|repeal|no further action|withdrew|closed.*regulation|rescind',
              x[,n]))] <- 'withdrawn'
  x[,j][(grep('proposed rule to rescind', x[,n]))] <- 'nprm' # added 9/24
  x[,j][(grep('transfer|combined|subsumed|superseded|merged|^duplicate of|^incorporated in|^integrated ',
              x[,n]))] <- 'merged'
  x[,j][(grep('undetermined|to be determined|review|case opened|^further board action by$',
              x[,n]))] <- 'tba'
  x[,j][(grep('comment.*[^end]', x[,n]))] <- 'comment'
  x[,j][(grep('comment$', x[,n]))] <- 'comment'
  x[,j][(grep('comment.*end', x[,n]))] <- 'comment.end'
  # catch-all category
  x[,j][is.na(x[,j])] <- NA
  n <- n+1
}


## prospective action - created indicator that flags '00' date entries
for(i in 1:9) assign(paste('action', i, '.prospective', sep = ''), rep(0, nrow(x)))
# add to x
x <- cbind(x, action1.prospective); x <- cbind(x, action2.prospective);
x <- cbind(x, action3.prospective); x <- cbind(x, action4.prospective);
x <- cbind(x, action5.prospective); x <- cbind(x, action6.prospective);
x <- cbind(x, action7.prospective); x <- cbind(x, action8.prospective);
x <- cbind(x, action9.prospective)
for(i in 1:9) {
  x[,paste('action', i, '.prospective', sep = '')][grep('/00/', x[,paste('date', i, sep = '')])] <- 1
}


# change '00' days to '01'
for(i in 1:9) x[,paste('date', i, sep = '')] <- gsub('/00/', '/01/',
                                                     x[,paste('date', i, sep = '')])
for(i in 1:9) x[,paste('date', i, sep = '')] <- as.character(
                  as.Date(x[,paste('date', i, sep = '')], '%m/%d/%Y'))
# indicator for completed action

for(i in 1:9) assign(paste('action', i, '.complete', sep = ''), rep(NA, nrow(x)))
# add to x
x <- cbind(x, action1.complete); x <- cbind(x, action2.complete); x <- cbind(x, action3.complete)
x <- cbind(x, action4.complete); x <- cbind(x, action5.complete); x <- cbind(x, action6.complete)
x <- cbind(x, action7.complete); x <- cbind(x, action8.complete); x <- cbind(x, action9.complete)

# set 'comment.end' entries back 30 days (length of comment period)
n <- grep('action1.type', colnames(x))
for(j in grep('date1', colnames(x)):grep('date9', colnames(x))) {
  x[,j][x[,n] == 'comment.end' & !is.na(x[,j]) & !is.na(x[,n])] <-
    as.character(as.Date(x[,j][x[,n] == 'comment.end' & !is.na(x[,j]) & !is.na(x[,n])]) - 30)
  n <- n + 1
}

## indicate which actions have been completed (based on the date given: retrospective or prospective)
## '00' entries are deemed prospective, even if listed late UNLESS there is an FR citation

n <- grep('date1$', colnames(x))
m <- grep('fr.citation1$', colnames(x))
k <- grep('action1.prospective', colnames(x))
z <- grep('action1.type', colnames(x))
for(j in grep('action1.complete', colnames(x)):grep('action9.complete', colnames(x))) {
  x[,j][as.Date(x[,n]) < as.Date(x[,'ua.date.new'])] <- 1
  x[,j][as.Date(x[,n]) >= as.Date(x[,'ua.date.new'])] <- 0
  # code 'yes' if there is a citation to FR
  x[,j][as.Date(x[,n]) < (as.Date(x[,'ua.date.new']) + 30) & (x[,m] != '' | !is.na(x[,m]) | x[,m] != 'NA') &
        x[,k] == 0] <- 1
  ## code 'no' if had '00' day entry WITHOUT an FR citation
  ## although this "no completion" coding is conservative, it can be overriden by other UA entries
  ##  - e.g., if it triggers NO NPRM for one entry, an NPRM entry later would override this if in error
  x[,j][x[,k] == 1 & (x[,m] == '' | is.na(x[,m]) | x[,m] == 'NA') ] <- 0
  ## make exception for withdrawals, which tend not to have FR citation
  x[,j][as.Date(x[,n]) < (as.Date(x[,'ua.date.new']) + 30) & x[,z] == 'withdrawn' & x[,k] == 0] <- 1
  n <- n+1
  m <- m+1
  k <- k+1
  z <- z+1
}

# reshape - more efficient to break up by agencies
x <- cbind(country = 1:nrow(x), x)
x <- as.data.frame(x, stringsAsFactors = F)
v <- split.data.frame(x, x$agency.code)

test <- lapply(v, function(x) {
    reshape(x, idvar = 'country',
            varying=list(grep('action1$', colnames(x)):grep('action9$', colnames(x)),
                grep('date1', colnames(x)):grep('date9', colnames(x)),
                grep('fr.citation1', colnames(x)):grep('fr.citation9', colnames(x)),
                grep('action1.type', colnames(x)):grep('action9.type', colnames(x)),
                grep('action1.complete', colnames(x)):grep('action9.complete', colnames(x))),
            v.names = c("action", "date", "fr.citation", "action.type",
                "action.complete"), direction="long") })
test <- lapply(test, function(x) as.matrix(x) )
test <- do.call('rbind', test)
x <- test


# variables not needed
x <- subset(x, select = -c(country, ua.date, count, time))


x <- subset(x, !duplicated(x))
x <- subset(x, !is.na(x[,'action']))
## without date variable, completed/prospective actions cannot be assessed
x <- subset(x, !is.na(x[,'date']))

# fix comment entries
x[,'action.type'][x[,'action.type'] == 'comment.end'] <- 'comment'

year <- substr(x[,'date'], 1, 4)
x <- cbind(x, year)


# fix priority
x[,'priority'][x[,'priority'] == 10] <- 'Economically Significant'
x[,'priority'][x[,'priority'] == 20] <- 'Other Significant'
x[,'priority'][x[,'priority'] == 30] <- 'Substantive, Nonsignificant'
x[,'priority'][x[,'priority'] == 40] <- 'Routine and Frequent'
x[,'priority'][x[,'priority'] == 50] <- 'Info./Admin./Other'
# major rules
x[,'major'] <- ifelse(x[,'priority'] == 'Economically Significant' |
                      x[,'priority'] == 'Other Significant', 1, 0)

# recode rfa
# codebook: B - business; G - government; O - other
x[,'rfa'][grep('B|G|O|Y', x[,'rfa'])] <- 'yes'
x[,'rfa'][grep('N|U', x[,'rfa'])] <- 'no'

# recode 'unfunded mandates'
# codebook: P - private sector; G - government; U - undetermined
x[,'unfun.mandate'][grep('P|G|govern', x[,'unfun.mandate'])] <- 'yes'
x[,'unfun.mandate'][grep('N|U', x[,'unfun.mandate'])] <- 'no'

## STAGE 2 MANIPULATIONS ---------------------------
# (all manipulations done after splitting by agency)


## drop the action prospective variables
k1 <- grep('action1.prospective', colnames(x))
k2 <- grep('action9.prospective', colnames(x))
x <- x[,-c(k1:k2)]

x <- x[order(x[,'rin'], x[,'date'], decreasing = T),]
m <- split.data.frame(x, x[,'rin'])
length(m[!sapply(m, is.matrix)]) # 0
# mark a final action that is not withdrawn and has an fr.citation as "complete"
m <- lapply(m, function(x) {
  # get max date
  d <- as.character(max(as.Date(x[,'date'])))
  x[,'action.complete'][x[,'action.type'] == 'withdrawn' & x[,'date'] == d] <- 1
  return(x)
})

# decision rules for inconsistent entries
m <- lapply(m, function(x) {
  h <- c('priority', 'rfa', 'unfun.mandate', 'procurement', 'gov.levels', 'legal.dline')
  for(j in 1:length(h)) {
    g <- names(table(x[,h[j]]))
    if(!is.null(g)) {
      if(length(g) == 1) x[,h[j]] <- rep(g, nrow(x))
      if(length(g) > 1) {
        z <- c()
        for(i in 1:length(g)) z[i] <- length(grep(g[i], x))
        # if only one max, choose max
        if(length(z[z == max(z)]) == 1) x[,h[j]] <- rep(g[z == max(z)], nrow(x))
        # if a tie, choose most recent
        if(length(z[z == max(z)]) > 1) x[,h[j]] <- rep(x[1,h[j]], nrow(x)) }}}
  return(x)
})


# NOTE - action.type categories are mutually exclusive
# this function identifies those actions that have been completed
m <- lapply(m, function(x) {
  # earlist date on record
  date.initiated <- as.character(min(as.Date(x[,'date'])))
  # date finished (across all action types (just gives a sense of overall completion times)
  date.final <- as.character(max(as.Date(x[,'date'])))
  actions <- c('nprm', 'comment', 'withdrawn', 'final.rule', 'last.action', 'interim.final',
               'direct.final', 'merged', 'hearing', 'anprm', 'notice')
  for(i in 1:length(actions)) {
    assign(actions[i], ifelse(nrow(subset(x, x[,'action.complete'] == 1 &
                                       x[,'action.type'] == actions[i])) > 0, 1, 0))
    if(get(actions[i]) == 1) {
      assign(paste('date.', actions[i], sep = ''),
             as.character(min(as.Date(subset(x[,'date'], x[,'action.type'] == actions[i] &
                                             x[,'action.complete'] == 1) ))) )
      assign(paste('date.', actions[i], sep = ''), rep(get(paste('date.', actions[i], sep = '')), nrow(x)) )
      assign(actions[i], rep(1, nrow(x)) )}
    if(unique(get(actions[i])) == 0) {
      assign(actions[i], rep(0, nrow(x)) )
      assign(paste('date.', actions[i], sep = ''), rep(NA, nrow(x)) )}
    }
  x <- cbind(x, date.initiated, date.final, anprm, date.anprm, nprm, date.nprm, comment,
             date.comment, withdrawn, date.withdrawn, final.rule, date.final.rule,
             last.action, date.last.action, interim.final, date.interim.final, direct.final,
             date.direct.final, merged, date.merged, hearing, date.hearing, notice, date.notice)
})



m <- lapply(m, function(x) {
  x <- x[order(x[,'date']), , drop = F]
  nprm.planned <- ifelse(length(x[grep('^nprm', x[,'action.type']), 'action.type']) > 0, 1, 0)
  anprm.planned <- ifelse(length(x[grep('anprm', x[,'action.type']), 'action.type']) > 0, 1, 0)
  date.nprm.planned <- ifelse(nprm.planned == 1, x[grep('^nprm', x[,'action.type']), 'date'][1], 0)
  date.anprm.planned <- ifelse(anprm.planned == 1, x[grep('anprm', x[,'action.type']), 'date'][1], 0)
  x <- cbind(x, nprm.planned, date.nprm.planned, anprm.planned, date.anprm.planned)
})


## collect detailed FR citations
m <- lapply(m, function(x) {
    x <- x[order(x[,'date']), , drop = F]
    z <- x[grep('FR|fr', x[,'fr.citation']), , drop = F]
    nprm.fr.cite <- ifelse(z[,'nprm'] == 1, z[grep('^nprm', z[,'action.type']), 'fr.citation'][1], 0)
    anprm.fr.cite <- ifelse(z[,'anprm'], z[grep('anprm', z[,'action.type']), 'fr.citation'][1], 0)
    final.fr.cite <- ifelse(z[,'final.rule'] == 1 | z[,'last.action'] == 1,
                            z[grep('final\\.rule|last\\.action', z[,'action.type']), 'fr.citation'][1], 0)
    if(nrow(z) > 0) {
        if(is.na(nprm.fr.cite[1]) & z[1,'nprm'] == 1) nprm.fr.cite <- z[1 ,'fr.citation']
        if(is.na(final.fr.cite[1]) & z[1,'final.rule'] == 1) final.fr.cite <- z[nrow(z) ,'fr.citation']
        if(is.na(final.fr.cite[1]) & z[1,'last.action'] == 1) final.fr.cite <- z[nrow(z) ,'fr.citation']
    }
    nprm.fr.cite <- rep(nprm.fr.cite[1], nrow(x))
    anprm.fr.cite <- rep(anprm.fr.cite[1], nrow(x))
    final.fr.cite <- rep(final.fr.cite[1], nrow(x))
    z <- cbind(x, nprm.fr.cite, anprm.fr.cite, final.fr.cite)
})


m <- lapply(m, function(x) {
    x <- x[order(x[,'date']), , drop = F]
    z <- x[x[,'action.type'] == 'nprm' & x[,'action.complete'] == '1',
           c('fr.citation', 'date', 'action.complete') , drop = F]
    if(nrow(z) == 0) {
        nprm.count <- '0'
        nprm.list <- ''
    }
    if(nrow(z) > 0) {
        z <- z[!duplicated(z), , drop = F]
        z <- z[!is.na(z[,'fr.citation']), c('fr.citation', 'date'), drop = F]
        nprm.count <- nrow(z)
        nprm.list <- paste(z[,'fr.citation'], z[,'date'], sep = '; ')
        nprm.list <- paste(nprm.list, collapse = ' > ')
    }
    nprm.count <- rep(nprm.count[1], nrow(x))
    nprm.list <- rep(nprm.list[1], nrow(x))
    z <- cbind(x, nprm.count, nprm.list)
})


## collapse by RIN
## collect first and last UA entry dates, plus first stage and fr citation
m <- lapply(m, function(x) {
  x[,'fr.citation'] <- tolower(x[,'fr.citation'])
  g <- grep('fr', x[,'fr.citation'], value = T)
  if(length(g) > 0) x[,'fr.citation'] <- rep(g[1], nrow(x))
  x <- subset(x, select = -c(action.type, action.complete, action, year))
  if(nrow(x) > 1) x <- x[order(as.Date(x[,'ua.date.new'])),]
  stages <- paste(x[,'stage'], sep = ',')
  stage <- c()
  stage[1] <- stages[1]
  if(nrow(x) > 1) {
      j <- 1
      for(i in 1:(length(stages) - 1)) {
          if(stages[i] != stages[i + 1]) {
              j <- j + 1
              stage[j] <- stages[i + 1]
          }
      }
  }
  if(nrow(x) > 1) stage.path <- paste(stage, collapse = ', ')
  if(nrow(x) == 1) stage.path <- stage
  ua.date.first <- as.character(min(as.Date(x[,'ua.date.new'])))
  ua.date.last <- as.character(max(as.Date(x[,'ua.date.new'])))
  x <- x[1, , drop = F]
  x <- cbind(x, stage.path, ua.date.first, ua.date.last)
  x <- subset(x, select = -c(ua.date.new))
})

x <- do.call('rbind', m)

#------ convert to data frame

x <- as.data.frame(x)


## date typos:
## fix unrecognizable dates
pattern <- "^\\d{4}-\\d{2}-\\d{2}$" # YYYY-MM-DD pattern
x$date.initiated[!grepl(pattern, x$date.initiated)] <- x$ua.date.first[!grepl(pattern, x$date.initiated)]
## correct obvious date errors
x$date.initiated[as.Date(x$date.initiated) < '1977-01-01'] <-
    x$ua.date.first[as.Date(x$date.initiated) < '1977-01-01']
x$date.initiated[as.Date(x$date.initiated) > '2024-01-01'] <-
    x$ua.date.last[as.Date(x$date.initiated) > '2024-01-01']

x <- subset(x, x[,'date.initiated'] > as.Date('1977-01-01') &
            x[,'date.initiated'] < as.Date('2023-01-20') )

# start years at (jan 20)
years <- 1978:2023
year <- rep('1977', nrow(x))
for(i in 1:length(years)) {
  year[as.Date(x[,'date.initiated']) >
       as.Date(paste(years[i], '-01-20', sep = ''))] <- as.character(years[i])
}
x <- cbind(x, year)

# indicator for administration reg was initiated in
admin <- rep('carter', nrow(x))
admin[as.Date(x[,'date.initiated']) > '1981-01-20'] <- 'reagan'
admin[as.Date(x[,'date.initiated']) > '1989-01-20'] <- 'bush41'
admin[as.Date(x[,'date.initiated']) > '1993-01-20'] <- 'clinton'
admin[as.Date(x[,'date.initiated']) > '2001-01-20'] <- 'bush43'
admin[as.Date(x[,'date.initiated']) > '2009-01-20'] <- 'obama'
admin[as.Date(x[,'date.initiated']) > '2017-01-20'] <- 'trump'
admin[as.Date(x[,'date.initiated']) > '2021-01-20'] <- 'biden'
x <- cbind(x, admin)

## hand-coding some errors

x$final.rule[x$rin == '2060-AS91'] <- 0
x$date.final.rule[x$rin == '2060-AS91'] <- NA
x$date.nprm[x$rin == '2060-AS91'] <- '2017-09-08'
x$date.last.action[x$rin == '3235-AJ80'] <- '2008-03-27'  # fix date.last.action

x[,'priority'][x[,'rin'] == '2030-AA43'] <- 'Info./Admin,/Other.'
x[,'major'][x[,'rin'] == '2030-AA43'] <- '0'
x[,'nprm'][x[,'rin'] == '2040-AC69'] <- '0'
x[,'date.nprm'][x[,'rin'] == '2040-AC69'] <- NA
x[,'comment'][x[,'rin'] == '2040-AC69'] <- '0'
x[,'date.comment'][x[,'rin'] == '2040-AC69'] <- NA
x[,'nprm'][x[,'rin'] == '2060-AJ20'] <- '0'
x[,'date.nprm'][x[,'rin'] == '2060-AJ20'] <- NA
x[,'nprm'][x[,'rin'] == '2060-AE73'] <- '0'
x[,'date.nprm'][x[,'rin'] == '2060-AE73'] <- NA


save(x, file = 'data/ua_data/ua_data_all_rules.RData')


#### isolate final rules

load('data/ua_data/ua_data_all_rules.RData')

withered <- ifelse(x[,'anprm'] == 0 &
                   x[,'nprm'] == 0 &
                   x[,'withdrawn'] == 0 &
                   x[,'comment'] == 0 &
                   x[,'final.rule'] == 0 &
                   x[,'last.action'] == 0 &
                   x[,'interim.final'] == 0 &
                   x[,'direct.final'] == 0 & x[,'merged'] == 0, 1, 0)
x <- cbind(x, withered)
x <- subset(x, x[,'withered'] != 1 & x[,'merged'] != 1 & x[,'withdrawn'] != 1)

## drop those with no completion date
actions <- c('date.final.rule', 'date.interim.final',
             'date.direct.final', 'date.last.action')
date.complete <- apply(x[,actions], 1, function(y) {
  y <- y[!is.na(y)]
  if(length(y) > 0) return(as.character(min(as.Date(y)))) else return(NA)
})
x <- cbind(x, date.complete)
x <- x[!is.na(x[,'date.complete']),]

## drop those where stage.path never reached final rule or completed action
final.rule.stage <- rep(0, nrow(x))
final.rule.stage[grep('Final Rule|Completed', x[,'stage.path'])] <- 1
x <- cbind(x, final.rule.stage)
x <- x[x[,'final.rule.stage'] == 1,]

years <- 1984:2023
year <- rep('1983', nrow(x))
for(i in 1:length(years)) {
  year[as.Date(x[,'date.complete']) >
       as.Date(paste(years[i], '-01-20', sep = ''))] <- as.character(years[i])
}
## recode initiation year/admin
colnames(x)[colnames(x) == 'year'] <- 'year.init'
colnames(x)[colnames(x) == 'admin'] <- 'admin.init'
x <- cbind(x, year)


test <- substr(x[,'date.complete'], 1, 4)
test <- ifelse(as.numeric(test) > 2022, 1, 0)
x[,'year'][test == 1] <- substr(x[,'date.complete'], 1, 4)[test == 1]
# indicator for administration reg was initiated in
admin <- rep('carter', nrow(x))
admin[as.Date(x[,'date.complete']) > '1981-01-20'] <- 'reagan'
admin[as.Date(x[,'date.complete']) > '1989-01-20'] <- 'bush41'
admin[as.Date(x[,'date.complete']) > '1993-01-20'] <- 'clinton'
admin[as.Date(x[,'date.complete']) > '2001-01-20'] <- 'bush43'
admin[as.Date(x[,'date.complete']) > '2009-01-20'] <- 'obama'
admin[as.Date(x[,'date.complete']) > '2017-01-20'] <- 'trump'
admin[as.Date(x[,'date.complete']) > '2021-01-20'] <- 'biden'
x <- cbind(x, admin)

save(x, file = 'data/ua_data/ua_data_final_rules.RData')
