setwd("~/Desktop/U Georgia/Rebecca_Project")
#outputs <- read.csv("results.treatment.mod.oct6.csv", header = T, stringsAsFactors = F)
outputs <- read.csv("results.treatment.mod.may10.csv", header = T, stringsAsFactors = F)
library(ggplot2)
library(egg)

phis <- outputs[c(68:71, 73:84),c(1,2,4,5)]
phis$Placement <- c(rep(1,4), rep(2:4, 4))
phis$Label <- c(rep("Average",4), rep(c("Direct release", "Head-start", "Wild recruit"), 4))
phis$group <- c("Hatchlings\n", "Juveniles\n", "Subadults\n", "Adults\n",
                rep(c("Hatchlings\n", "Juveniles\n", "Subadults\n", "Adults\n"), each = 3))
phis <- phis[phis$group != "Adults\n",]
phis$p <- 6
graphissue <- phis[c(6,9,12),] 
#names(phis) <- c("X", "Lower95", "Upper95", "Estimated Survival Probability", #
#                 )

g <- ggplot(phis, aes(y = Mean, x = Placement, shape = Label))+
  facet_grid(~group)+
  geom_errorbar(aes(ymin = Lower95, ymax = Upper95), width = 0.2, size = 1.2)+
  geom_point(size = 7, fill = "black")+
  scale_shape_manual(values=c(21:24))+
  theme_light()+
  labs(y = "\nAnnual apparent survival\n")+
  ylim(0,1)+
  theme(text = element_text(size = 30, color = "black"),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size = 28),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(size=.7, color = "black"),
        strip.placement = "inside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", size = 32),
        legend.position="bottom", legend.direction="horizontal",
        legend.text = element_text(size = 32),
        panel.border = element_rect(colour = "white", fill=NA, size=.5))
g
g <- tag_facet(g, tag_pool = c("A Hatchlings", "B Juveniles", "C Subadults"), close = "", open = "", hjust = -0.07, cex = 8)
#g <- tag_facet(g, tag_pool = c("A", "B", "C"), close = "", open = "", hjust = -0.50, cex = 8)
g

ggsave("Fig3_July13.jpeg", g, dpi = 600, width = 16, height = 8, device = "jpeg" )
#ggsave("Fig3_July13_alt.jpeg", g, dpi = 600, width = 16, height = 8, device = "jpeg" )

g2 <- ggplot(phis, aes(y = Mean, x = Placement, shape = Label, col = Label))+
  facet_grid(~group)+
  geom_errorbar(aes(ymin = Lower95, ymax = Upper95), width = 0.2, size = 1.2)+
  geom_point(size = 4)+
  scale_shape_manual(values=c(15:18))+
  scale_color_grey()+
theme_light() +
  labs(y = "\nEstimated Yearly Survival Probability\n")+
  ylim(0,1)+
  theme(text = element_text(size = 28),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(size=.7, color = "black"),
        strip.placement = "inside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", size = 24),
        legend.position="bottom", legend.direction="horizontal",
        legend.text = element_text(size = 22),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("Alt_Fig3.jpeg", g2, dpi = 300, width = 12, height = 10, device = "jpeg" )




# ################### 
# phi.age2 <- c(.052, .191, .357, .1996)
# phi.age3 <- c(.65618, .76516, .86563, .76515)
# phi.age.4 <- c(.8541, .94999, .99897, .94035)
# 
# phi.12 <- c(.186, .268, .362, .27)
# phi.22 <- c(.2696, .48214, .7254, .49053)
# phi.32 <- c(0, .03764, .25372, .0708)
# phi.22 
# 
# 
# plot(1:4, c(phi.age2[4], phi.12[4], phi.22[4], phi.32[4]), 
#      pch = 19, xlim  = c(.5, 4.5), ylim = c(0,1), 
#      xaxt = "n", xlab = "", ylab = "Survival Estimate",
#      main  = "Hatchling Survival Estimates")
# lines(c(1,1), phi.age2[c(1,3)])
# text(1, .9, "Overall \n n= 201")
# lines(c(2,2), phi.12[c(1,3)]) 
# text(2, .9, "DR \n n= 169")
# lines(c(3,3), phi.22[c(1,3)]) 
# text(3, .9, "Headstart \n n= 21")
# lines(c(4,4), phi.32[c(1,3)])     
# text(4, .9, "Natural, \n n= 11")
# 
# library(ggplot2)

