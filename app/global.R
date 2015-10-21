setwd("~/GitHub/FI_Meetup/app")

mypallete <- colorRampPalette(rev(brewer.pal(11, "Spectal")))
p2 <- ggplot(data = both2, aes(x=assets, y = total, color=standardized, size = z_total, label=rank_standardized)) +
        geom_point()+stat_smooth(method = "lm", level = .99, size=1.5, alpha=.4, color="darkgray")+
        geom_abline(intercept=mean(both2$total), slope=0, size = 1.5, alpha=.4)+
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

p1 <- ggplot(data = both2, aes(x=assets, y = total, color=total, size = total, label=rank_total)) +
        geom_point()+#geom_text(data=both2[rank_total<11,],
                               #aes(label=rank_total),hjust=.5, vjust=.5, color="black", alpha=.7)+
        geom_text(data=both2[rank_total<11,], 
                  aes(label=Company),hjust=-.1, vjust=.5, color="black", alpha=.7)+
        geom_abline(intercept=mean(both2$total), slope=0, size = 1.5, alpha=.4)+
        scale_colour_gradientn(colours=mypallete(30))+
        scale_size_continuous(range=c(4,10))+
        #ggtitle("One Dimensional Cut-off Using Total Complaints")+
        scale_x_continuous("Total Assets", labels=dollar)+
        scale_y_continuous("Total Complaints", labels = comma)+
        #labs(x = "Total Assets", y = "Total Complaints")+
        theme_pander()

