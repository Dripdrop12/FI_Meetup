library(shiny)
library(dplyr)
library(qdap)
library(data.table)
library(tm)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(DT)

# Load the data #
both2 <- read.csv("data/both2.csv")
sum_cfpb <- read.csv("data/sum_cfpb.csv")
both3 <- both2[, c("Company", "total", "assets", "standardized")]

mypallete <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

p1 <- ggplot(data = both2, aes(x=disputed, y = total, color=percent_disputed, size = total)) +
        geom_point()+stat_smooth(method = "lm", level = .99, size=1.5, alpha=.4, color="darkgray")+
        geom_text(data=both2[both2$rank_total<11,], 
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
    geom_text(data=both2[both2$rank_standardized<11,], 
              aes(label=Company),hjust=-.1, vjust=.5, color="black", alpha=.7)+
    scale_colour_gradientn(colours=mypallete(30))+
    scale_size_continuous(range=c(4,10))+
    guides(col=guide_colourbar("Standardization"))+
    #ggtitle("Asset Standardization")+
    scale_x_continuous("Total Assets", labels=dollar)+
    scale_y_continuous("Total Complaints", labels = comma)+
    #labs(x = "Total Assets", y = "Total Complaints")+
    theme_pander()
