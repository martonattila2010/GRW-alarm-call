
library(rptR)

df <- read.csv("DFx2b.csv", header = T, sep = ";")

df$nestid <- as.factor(df$nestid)

df$boots.grwf[df$boots.grwf == "0"] <- 0
df$boots.grwf[df$boots.grwf == "1"] <- NA
df$boots.grwf[df$boots.grwf == "2"] <- 1

df$boots.grwm[df$boots.grwm == "0"] <- 0
df$boots.grwm[df$boots.grwm == "1"] <- NA
df$boots.grwm[df$boots.grwm == "2"] <- 1

df$pair[df$boots.grwm == "0" & df$boots.grwf == "0"] <- 0
df$pair[df$boots.grwm == "1" & df$boots.grwf == "0"] <- 1
df$pair[df$boots.grwm == "0" & df$boots.grwf == "1"] <- 2
df$pair[df$boots.grwm == "1" & df$boots.grwf == "1"] <- 3
df$pair <- as.factor(df$pair)

df$bpair[df$pair == "0"] <- 0
df$bpair[df$pair != "0"] <- 1

df$multiple <- 1
n <- summaryBy(multiple ~ nestid, data = df, FUN = c(sum))
m <- n$nestid[n$multiple.sum > 1]

df.rep <- df[df$nestid %in% m,]
df.rep <- df[!is.na(df$boots.grwf),]

boots.grwf.rpt <- rptBinary(boots.grwf ~ (1|nestid), grname=c("nestid"), data=df.rep)

boots.grwm.rpt <- rptBinary(boots.grwm ~ (1|nestid), grname=c("nestid"), data=df.rep)

pair.rpt <- rptBinary(bpair ~ (1|nestid), grname=c("nestid"), data=df.rep)

save.image()