## ------------------------------------------------
##              FEDERAL REGISTER DATA
## ------------------------------------------------



library('curl')
library('httr')
library('jsonlite')
library('utils')


## select variables ("fields") to download
n <- c('action', 'agencies', 'agency_names', 'body_html_url', 'citation', 'comment_url',
           'docket_id', 'docket_ids', 'document_number', 'executive_order_number', 'full_text_xml_url',
           'page_length', 'page_views', 'president', 'publication_date',
           'regulation_id_numbers', 'regulations_dot_gov_url', 'significant',
       'toc_subject', 'topics', 'type', 'abstract', 'presidential_document_number',
       'proclamation_number', 'title')


## loop by dates, starting in 1995
dates <- seq(as.Date('1995-01-03'), as.Date('2024-07-15'), by='day')

## NOTE on 'type'
## ‘type’: a character string containing “RULE” for a Final Rule,
##      “PRORULE” for a Proposed Rule, “NOTICE” for a Notice, or
##      “PRESDOCU” for a Presidential Document; multiple ‘type’ arguments
##      can be specified and only one type should be named with each.

## search function
##  - based on fr_search() from 'federalregister' R library
fr_search_update <- function (..., fields = NULL, per_page = NULL, page = NULL, order = "relevance",
    version = "v1", getopts = NULL)
{
    baseurl <- paste("https://www.federalregister.gov/api/",
                     version, "/documents.csv?", sep = "")
    ## print(baseurl)
    query <- list(...)
    if ("publication_date" %in% names(query)) {
        w <- which(names(query) == "publication_date")
        p <- query$publication_date
        names(p) <- paste("publication_date][", names(p), sep = "")
        query <- query[-w]
        query <- c(query, p)
    }
    if ("effective_date" %in% names(query)) {
        w <- which(names(query) == "effective_date")
        p <- query$effective_date
        names(p) <- paste("effective_date][", names(p), sep = "")
        query <- query[-w]
        query <- c(query, p)
    }
    if ("cfr" %in% names(query)) {
        w <- which(names(query) == "cfr")
        p <- query$cfr
        names(p) <- paste("cfr][", names(p), sep = "")
        query <- query[-w]
        query <- c(query, p)
    }
    if ("near" %in% names(query)) {
        w <- which(names(query) == "near")
        p <- query$near
        names(p) <- paste("near][", names(p), sep = "")
        query <- query[-w]
        query <- c(query, p)
    }
    query <- paste(curl_escape(paste("conditions[", names(query),
        "]=", query, sep = "")), collapse = "&")
    if (!is.null(per_page) && as.numeric(per_page) > 1000) {
        stop("'per_page' cannot be greater than 1000")
    }
    else if (!is.null(per_page) & !is.null(page)) {
        p <- paste("per_page=", per_page, "&page=", page, sep = "")
    }
    else if (!is.null(per_page) & is.null(page)) {
        p <- paste("per_page=", per_page, sep = "")
    }
    else if (!is.null(page)) {
        p <- paste("page=", page, sep = "")
    }
    else {
        p <- NULL
    }
    if (!is.null(fields)) {
        fields <- paste(paste(curl_escape("fields[]"), fields,
            sep = "="), collapse = "&")
        args <- paste(fields, query, p, sep = "&")
    }
    else {
        args <- paste(query, p, sep = "&")
    }
    ## print(args)
    args <- URLdecode(args)
    r <- do.call("GET", c(list(url = paste(baseurl, args, sep = "")),
                          getopts))
    stop_for_status(r)
    response <- content(r, "text")
    out <- read.csv(textConnection(response), header = TRUE)
    return(out)
}


## final rules from API
pause_duration <- 30
batch.breaks <- seq(100, 10700, by = 100)
d <- data.frame()
for(i in 1:length(dates)) {
    m <- fr_search_update(fields = n, publication_date = list(is = as.character(dates[i])),
                     type = 'RULE', per_page=1000)
    print(nrow(m))
    if(nrow(m) > 0) d <- rbind(d, m)
    if(i %in% batch.breaks) Sys.sleep(pause_duration)
}

write.csv(d, file = 'data/final_rules_from_api.csv', row.names = F)

