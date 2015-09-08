
suppressMessages(library(relsurv))
ratedata     <-      read.delim2("popmort/popmort19582015.txt",header=T)

men    <- matrix(type.convert(as.character(ratedata[ratedata$sex==1,"prob"])),
                 nrow=max(ratedata$X_age)-min(ratedata$X_age)+1,
                 dimnames=list(min(ratedata$X_age):max(ratedata$X_age),
                               min(ratedata$X_year):max(ratedata$X_year))
) + 10^-10
women  <- matrix(type.convert(as.character(ratedata[ratedata$sex==2,"prob"])),
                 nrow=max(ratedata$X_age)-min(ratedata$X_age)+1,
                 dimnames=list(min(ratedata$X_age):max(ratedata$X_age),
                               min(ratedata$X_year):max(ratedata$X_year))
) + 10^-10

popmort  <- transrate(men=men, 
                      women=women,
                      yearlim=c(min(ratedata$X_year),max(ratedata$X_year)),
                      int.length=1)  


