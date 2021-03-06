# CMEE 2020 HPC excercises R code challenge G proforma

rm(list=ls()) # nothing written elsewhere should be needed to make this work

# please edit these data to show your information.
name <- "Zebulon Bond"
preferred_name <- "Zeb"
email <- "zb520@imperial.ac.uk"
username <- "zb520"

# don't worry about comments for this challenge - the number of characters used will be counted starting from here

# maybe if u save points and plot as type = "l"
X=function(s,S,d,l,D){
    if(l>0.01){ 
        segments(s,S,e<-s+l*cos(d),E<-S+l*sin(d))
        X(e,E,d+D*4/pi,l*0.38,D)
        X(e,E,d,l*0.87,-1*D)
    }
}
plot(NA,,,c<-c(0,8),c)
X(4,0,pi/2,1,1)