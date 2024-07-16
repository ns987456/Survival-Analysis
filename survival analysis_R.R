Time <- cancer.dat [,"Time"] 
Status <- 1- cancer.dat [,"Status"] 
cancer.cox <- coxreg (Time, Status, cancer.dat [, c("Age","Smear",
                                                    "Infill","Index","Blasts","Temp")]) ↲
cancer.cox 

coxreg (Time, Status, cancer.dat[,"Age"]) 

cancer.cox1<- coxreg (Time, Status, cancer.dat [,"Age"], resid = "mart") 
plot (cancer.dat [,"Age"], cancer.cox1 $ resid,xlab ="Age", 
      ylab ="martingal residual") 
lines (lowees (cancer.dat [,"Age"], cancer.cox1 $ resid))
abline (h=0)

cancer.cox1<- coxreg (Time, Status,cancer.dat [,"Age"], resid = "dev") 
plot (cancer.dat [,"Age"], cancer.cox1 $ resid,xlab ="Age", 
      ylab ="martingal residual") 
lines (lowees (cancer.dat [,"Age"], cancer.cox1 $ resid)) 
abline (h=0) 


agroup <- cancer.dat [,"Age"] – 45
agroup [agroup >=0] <- 1
agroup [agroup <0] <- 0
plot (surv.fit (Time, Status, agroup),xlab = "survival time in months", 
      ylab = "proportion surviving", lty = 1:2)
legend (28,0.8, c("under 45","45 and over"),
        lty = 1;2)

Surv.diff(Time, Status, agroup)

coxreg(Time, Status, agroup)

        