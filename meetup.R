library(shiny)
library(dplyr)
library(qdap)
library(data.table)
library(tm)
library(ggthemes)
library(scales)
library(RColorBrewer)

# Set working directory #
setwd("~/GitHub/FI_Meetup")

# SEC URL #
sec_url <- function(Year, Quarter){
    s = tolower(Quarter)
    if(!Year %in% seq(2009, 2015, 1)){
        warning("SEC data is only available between 2009 and 2015")
        return(NA)
    } else if (!s %in% c("q1", "q2", "q3", "q4")){
        warning("Please enter q1, q2, q3, or q4 for Quarter")
        return(NA)
    } else{
    paste0("http://www.sec.gov/data/financial-statements/", Year, as.character(s), ".zip")
    }
}

# Download Consumer Financial Protection Bureau Data #
if(!file.exists("cfpb.csv")){
download.file("http://data.consumerfinance.gov/api/views/s6ew-h6mp/rows.csv", "cfpb.csv")
} 
cfpb <- fread("cfpb.csv")

# Specify states and other endings to remove from the end of company names #
state <- tolower(unique(cfpb$State))
legal_entity <- c("inc", "corp", "co", "plc", "ltd", "llc", 
                  "company", "companymn", "holdings", "financial services", "services", 
                  "financial", "corporation", "group", "bank", "banks")
endings <- c(state, legal_entity)
rm(list=c("state", "legal_entity"))

# Remove punctuation, whitespace, and lower case company names #
cfpb$Company <- cfpb$Company %>% tolower() %>% removePunctuation() %>% stripWhitespace()
# Replace the CFPB name for Citi with the SEC name #
cfpb$Company <- gsub("citibank", "citigroup", cfpb$Company)

## Remove endings from CFPB companies ##
for (i in 1:length(endings)){
    cfpb$Company <- gsub(paste(" ", endings[i], "$", sep = ""), "", cfpb$Company)
}

# Store CFPB Companies and clean dataset#
companies_of_interest <- unique(cfpb$Company)
write.csv(cfpb, "app/data/cfpb_clean.csv")

# Download SEC Data #
# Specify the year and quarter of data to download
Y <- seq(2009, 2015, 1)
Q <- c("q1", "q2", "q3", "q4")

# Create QC Metric Buckets 
merge_percent_companies <- matrix(nrow = length(Y), ncol = length(Q))
merge_percent_of_complaints <- matrix(nrow = length(Y), ncol = length(Q))

# Create an SEC database by programatically extracting SEC data relevant to CFPB #
## If the SEC database already exists, this will not overwrite it ##
if(!dir.exists("SEC_database")){
    dir.create("SEC_database")
}
SEC_database_path <- paste(getwd(), "/SEC_database", sep="")
if(getwd() != SEC_database_path){
    # Set working directory to SEC_database_path
    setwd(paste(getwd(), "/SEC_database", sep=""))}
    if(!file.exists("2015q2sub_num.csv")){
        for (i in 1:length(Y)){for (j in 1:length(Q)){
        # Create a temporary directory and download the data #
        temp <- tempfile()
        Ye <- Y[i]
        Qu <- Q[j]
        set_url <- sec_url(Ye, Qu)
        download.file(set_url, temp, mode = "wb")
        unzip(temp) 
        num <- fread("num.txt", stringsAsFactors = F)
        subm <- fread("sub.txt", stringsAsFactors = F)
        # datatag <- fread("tag.txt", stringsAsFactors = F)
        # pre <- fread("pre.txt", stringsAsFactors = F)
        unlink(temp)
            
        ## Note The SEC website folder http://www.sec.gov/Archives/edgar/data/{cik}/{accession}/ 
        ## will always contain all the files for a given submission, where {accession} is the adsh 
        ## with the "-" characters removed.
            
        # Remove punctuation, whitespace, and lower case company names #
        subm$name <- subm$name %>% tolower() %>% removePunctuation() %>% stripWhitespace()
            
        ## Remove endings from SEC companies ##
        for (k in 1:length(endings)){
            subm$name <- gsub(paste(" ", endings[k], "$", sep = ""), "", subm$name)
        }
        # Merge the SEC and CFPB data on cleaned company name #
        both <- merge(x = subm, y = cfpb, by.x = "name", by.y = "Company")
            
        # QC metrics for the merge #
        merge_percent_companies[i,j] <- length(unique(both$name)) / length(unique(cfpb$Company))
        merge_percent_of_complaints[i,j] <- sum(cfpb$Company %in% unique(both$name)) / nrow(cfpb)
            
        # Merge and store relevant information in new directory #
        subm_num <- merge(subm, num, by = "adsh")
        subm_num <- subm_num %>% filter(name %in% companies_of_interest)
            
        write.csv(subm_num, file = paste(Y[i], Q[j], "sub_num.csv", sep = ""))
        }
    }
}

# Read and bind all SEC files #
multibind = function(mypath = SEC_database_path, 
                     year_range = seq(2011, 2015, 1), 
                     quarter_range = c("q1", "q2", "q3", "q4")){
    filenames = list.files(path = mypath, full.names=TRUE, pattern = "csv$")
    datalist = lapply(filenames, function(x){read.csv(file = x, header=T)})
    Reduce(function(x, y) {rbind(x, y)}, 
    datalist)
}
all_sec <- multibind()


# Create Assets Table and write to app data directory #
assets <- all_sec[all_sec$tag=="Assets" & all_sec$fy %in% c(2015), c("name", "tag", "value", "fy", "fp", "period")]
write.csv(assets, paste(sub("SEC_database", "app/data/", SEC_database_path), "assets.csv", sep = ""))

assets <- merge(cfpb, assets,
                by.x = "Company",
                by.y = "name", allow.cartesian = TRUE)

# Average assets by company #
average_assets <- assets %>%
    group_by(Company) %>%
    summarize(assets = mean(value))

sum_cfpb <- cfpb %>%
    group_by(Company) %>%
    summarize(total = n(),
              timely = sum(`Timely response?`=="Yes"),
              disputed = sum(`Consumer disputed?`=="Yes")) %>%
    mutate(percent_timely = timely/total,
           percent_disputed = disputed/total)

# Complaints standardized by company size in assets #
both2 <- merge(sum_cfpb, average_assets, by = "Company")
both2 <- within(both2, 
                standardized <- ((total-mean(total))/sd(total))-((assets-mean(assets))/sd(assets)))
both2 <- within(both2, 
                z_total <- ((total-mean(total))/sd(total)))
both2 <- within(both2, 
                z_assets <- ((assets-mean(assets))/sd(assets)))
both2 <- within(both2, 
                rank_total <- frank(-total))
both2 <- within(both2, 
                rank_standardized <- frank(-standardized))

# Assets #
assets <- merge(x = subm[, .(adsh, name)], y = num[num$tag=="Assets", .(adsh, tag, value)], by="adsh")

# Average assets by company #
std_assets <- assets %>%
    group_by(name) %>%
    summarize(tot_assets = mean(value)) %>%
    filter(tot_assets>0)%>%
    mutate(z_assets = ((tot_assets - mean(tot_assets, na.rm=TRUE)) / sd(tot_assets)))

# Complaints standardized by company size in assets #
both4 <- merge(cfpb, average_assets, by.x = "Company", by.y = "name")

std_complaints <- both4 %>%
    group_by(Company, Product) %>%
    summarize(total = n(),
              assets = unique(assets)) %>%
    group_by(Product) %>%
    mutate(z_total = ((total - mean(total, na.rm=TRUE)) / sd(total, na.rm=TRUE)))

both3 <- merge(std_assets, std_complaints, by.x = "name", by.y = "Company")
both3 <- within(both3, z_score <- z_total - z_assets)

coef(lm(total~assets, data=both2))
mypallete <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
ggplot(data = both2, aes(x=assets, y = total, color=standardized, size = z_total)) +
        geom_point()+
        geom_abline(intercept=7.321669e+02, slope=2.110519e-08, size = 1.5, alpha=.4)+
        scale_colour_gradientn(colours=mypallete(30))+
        scale_size_continuous(range=c(4,10))+
        ggtitle("Basic Asset Standardization")+
        scale_x_continuous("Total Assets", labels=dollar)+
        scale_y_continuous("Total Complaints", labels = comma)+
        #labs(x = "Total Assets", y = "Total Complaints")+
        theme_pander()