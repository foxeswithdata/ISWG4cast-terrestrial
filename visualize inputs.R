


df<-as.data.frame(read.csv("C:/Users/Dropcopter2/Documents/GitHub/ISWG4cast-terrestrial/input3.csv", header=T))
table(df$siteID)


# Now say which column you want
names(df)


library(tidyr)
library(ggplot2)

gat<-gather(df, "variable","value",4:10)
table(gat$siteID, gat$variable)

gat$date<-as.Date(gat$time)

ggplot(gat, aes(x=date, y=value, col=siteID))+geom_point(shape=".")+facet_wrap(~variable, scales="free")


ggplot(gat, aes(x=date, y=value, col=siteID))+geom_point(shape=".")+facet_grid(variable~siteID, scales="free")+theme_bw()

names(df)

####### Correlation panel
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y,use="complete.obs")) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  test <- cor.test(x,y,use="complete.obs") 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}



osbs<-df[df$siteID=="OSBS",]
bart<-df[df$siteID=="BART",]
konz<-df[df$siteID=="KONZ",]

pairs(konz[ c(4:10)], lower.panel=panel.smooth, upper.panel=panel.cor)




write.csv(df[,-1], file="seven_efi_inputs_12_21.csv")


head(df)
ggplot(df, aes(x=preci, y=vswc, col=siteID))+geom_point(shape=".")

df$AM5 <- rollapply(df$preci, 5, sum, fill = NA, align = "right")


met_data$AM5 <- shift(met_data$AM5, n = 1, fill = NA)







