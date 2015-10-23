library(shiny)
library(dplyr)
library(qdap)
library(data.table)
library(tm)
library(ggthemes)
library(scales)
library(RColorBrewer)

# Load the data #
cfpb <- fread("data/cfpb_clean.csv")
assets <- fread("data/assets.csv")

# Assets #
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

both3 <- both2[, .(Company, total, assets, standardized)]

mypallete <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

p1 <- ggplot(data = both2, aes(x=disputed, y = total, color=percent_disputed, size = total)) +
        geom_point()+stat_smooth(method = "lm", level = .99, size=1.5, alpha=.4, color="darkgray")+
        geom_text(data=both2[rank_total<11,], 
                  aes(label=Company),hjust=1, vjust=.5, color="black", alpha=.7)+
        scale_colour_gradientn(colours=mypallete(30), limits=c(.10, .33))+
        scale_size_continuous(range=c(4,10))+
        #ggtitle("One Dimensional Cut-off Using Total Complaints")+
        scale_x_continuous("Company Response Disputed by Consumer", labels=comma, limits = c(0,12000))+
        scale_y_continuous("Total Complaints", labels = comma)+
        #labs(x = "Total Assets", y = "Total Complaints")+
        theme_pander()

p2 <- ggplot(data = both2, aes(x=assets, y = total, color=standardized, size = z_total, label=rank_standardized)) +
    geom_point()+stat_smooth(method = "lm", level = .99, size=1.5, alpha=.4, color="darkgray")+
    #geom_text(data=both2[rank_standardized<11,], 
    #aes(label=rank_standardized),hjust=.5, vjust=.5, color="black", alpha=.7)+
    geom_text(data=both2[rank_standardized<11,], 
              aes(label=Company),hjust=-.1, vjust=.5, color="black", alpha=.7)+
    scale_colour_gradientn(colours=mypallete(30))+
    scale_size_continuous(range=c(4,10))+
    guides(col=guide_colourbar("Standardization"))+
    #ggtitle("Asset Standardization")+
    scale_x_continuous("Total Assets", labels=dollar)+
    scale_y_continuous("Total Complaints", labels = comma)+
    #labs(x = "Total Assets", y = "Total Complaints")+
    theme_pander()
