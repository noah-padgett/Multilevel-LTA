# miscellaneous tests
p1 <- 0.46
p2 <- 0.52

g11 <- 3.93
g2 <- 0.41

a11 <- log(p1/(1-p1))
a21 <- -2.52

t11 <- exp(a21+g11)/(1+ exp(a21+g11))
t11
t12 <- 1 - exp(a21+g11)/(1+ exp(a21+g11))
t12


t21 <- exp(a21)/(1+exp(a21))
t21
t22 <- 1 - exp(a21)/(1+exp(a21))
t22

f <- function(a){
  exp(a + 3.93)/(1+exp(a+3.93))
}

x <- seq(-10,2, 0.01)
y <- f(x)
plot_dat <- data.frame(X=x, Y=y)

library(ggplot2)

p <- ggplot(plot_dat, aes(X, Y))+
  geom_line()+
  geom_hline(yintercept=0.07) +
  lims(y=c(0,1))+
  theme_classic()
p



N <- 1000
J <- 1
## ================================================= ##
##    2-class Model

# Model:
# alpha1 - time 1 intercepts
mu1 <- a1 <-  -0.405
sig1 <- .8 # 2.5
# alpha2 | alpha1 - time 2 intercepts
mu2 <- -2.5
sig2 <- 0.25

# factors affecting transition
# level-1
g <- 4 # level-1 effect on transition prob
# high value means class 1 -> class 1
# negative value means class 1 -> class 2
# level-2
B <- 0.3 # slope of dummy variable on multinomial regression
# high value indicates strong cluster effect on intercept

## Model parameter tests
a1 <- numeric(J)
a2 <- numeric(J)
j <- 1
for(j in 1:J){
  a1[j] <- mu1 #+ rnorm(1, 0, sig1)

  a2[j] <- mu2 + B*a1[j] #+ rnorm(1, sig2)
}
# structural model (to, from)
p11 <- p21 <- p12 <- p22 <- numeric(J)

p11<-exp(a2+g)/(1+exp(a2+g))
p21<-1-exp(a2+g)/(1+exp(a2+g))
p12<-exp(a2)/(1+exp(a2))
p22<-1-exp(a2)/(1+exp(a2))

a1;a2; a2+g
p11;p21;p12;p22

## decomposition of variance

# computing R2 statistics
# 1. ICC C1 by (alpha1)
icc <- sig1/(sig1+(pi**2)/3)
icc
# the larger sig1 is the greater the cluster effect

# 2. variance c1
p1 <- exp(a1)/(1+exp(a1))
vc1 <- p1*(1-p1)
vc1

# 3. contribution of C1 on C2
con_c1_to_c2 <- g**2*vc1
con_c1_to_c2

# 4. Split con by icc
split1 <- con_c1_to_c2*icc
split2 <- con_c1_to_c2*(1-icc)
split1;split2
# 5. contribution of a1 to C2
con_a1_by_a2 <- B**2*sig1
tot <-  split1 + con_a1_by_a2+ 2*sqrt(split1*con_a1_by_a2)
tot
# 6. total variance of C2
total_c2 <- (pi**2)/3 + split1 + tot + sig2
total_c2
# 7. compute proportion of variance in C2 explained by a1
ex_a1 <- con_c1_to_c2/total_c2
ex_a1
# 8. proportion of variance in C2 explained by a2
ex_a2 <- sig2/total_c2
ex_a2
# 9. compute proportion of variance in C2 explained by residual of C1
ex_c1r <- split2/total_c2
ex_c1r
# 10. find total variance in C2 explained by the model
ex_mod <- (ex_a1 + ex_a2 + ex_c1r)
ex_mod

# 11. find total proportion of variance in C2 explained by C1
ex_c1_tot <- con_c1_to_c2/total_c2
ex_c1_tot

# 12. find the additional proportion of variance in C2 explain by adding the school level-2 effect at time-2
ex_mod - ex_c1_tot


## =========================================== ##
##  3-class model

## Model
J <- 1
# alpha1 - time 1 intercepts
a011 <- 0.40
a012 <- 0.90
sig1 <- .8
# alpha2 | alpha1 - time 2 intercepts
a021 <- -15
a022 <- -15
sig2 <- 0.25

# factors affecting transition
# level-1
g <- numeric(4)
g <- c(18, 17.5, 0, 16) # level-1 effect on transition prob
# high value means class 1 -> class 1
# negative value means class 1 -> class 2
# level-2
B <- numeric(4)
B <- c(0.3, 0.3, 0, 0.3) # slope of dummy variable on multinomial regression
# high value indicates strong cluster effect on intercept

a011 <- -0.5
a012 <- 0.5
a021 <- -15
a022 <- -15
g <- c(9.9, 11.6, 15.2, 13.7)
B <- c(1.3, -0.03, 0.2, -0.9)


a11 <- a021 + B[1]*a011
a21 <- a021 + B[2]*a011
a31 <- 0

a12 <- a022 + B[3]*a012
a22 <- a022 + B[4]*a012
a32 <- 0

a13 <- a021
a23 <- a022
a33 <- 0

# structural model (to, from)
p11 <- p21 <- p31 <- p12 <- p22 <- p32 <- p13 <- p23 <- p33 <- numeric(1)

p11<-exp(a11+g[1])/(1+exp(a11+g[1])+exp(a21+g[2]))
p21<-exp(a21+g[2])/(1+exp(a11+g[1])+exp(a21+g[2]))
p31<-1 - p11 - p21

p12<-exp(a12+g[3])/(1+exp(a12+g[3])+exp(a22+g[4]))
p22<-exp(a22+g[4])/(1+exp(a12+g[3])+exp(a22+g[4]))
p32<-1 - p12 - p22

p13<- exp(a12)/(1 + a31 + a32)
p23<- exp(a22)/(1 + a31 + a32)
p33<- 1 - p13 - p23

a011;a021;a012;a022
A <- matrix(c(a11+g[1],a21+g[2],a31,
              a12+g[3],a22+g[4],a32,
              a13, a23, a33), byrow = T, ncol=3)
round(A,3)
p11;p21;p31;p12;p22;p32;p31;p32;p33

P <- matrix(c(p11, p21, p31,
              p12, p22, p32,
              p13, p23, p33), ncol=3, byrow = T)
round(P,3)

## decomposition of variance

# computing R2 statistics
# 1. ICC C1 by (alpha1)
icc <- sig1/(sig1+(pi**2)/3)
icc
# the larger sig1 is the greater the cluster effect

# 2. variance c1
p1 <- exp(a011)/(1+exp(a011)+exp(a012))
p2 <- exp(a012)/(1+exp(a012)+exp(a011))
p3 <- 1 - p1 -p2
e1 <- p1*1 + p2*4 + p3*9
e2 <- (p1*1 + p2*2 + p3*3)**2
vc1 <- e1 - e2
vc1

# 3. contribution of C1 on C2
con_c1_to_c2 <- g**2*vc1
con_c1_to_c2

# 4. Split con by icc
split1 <- con_c1_to_c2*icc
split2 <- con_c1_to_c2*(1-icc)
split1;split2
# 5. contribution of a1 to C2
con_a1_by_a2 <- B**2*sig1
tot <-  split1 + con_a1_by_a2+ 2*sqrt(split1*con_a1_by_a2)
tot
# 6. total variance of C2
total_c2 <- (pi**2)/3 + split1 + tot + sig2
total_c2
# 7. compute proportion of variance in C2 explained by a1
ex_a1 <- con_c1_to_c2/total_c2
ex_a1
# 8. proportion of variance in C2 explained by a2
ex_a2 <- sig2/total_c2
ex_a2
# 9. compute proportion of variance in C2 explained by residual of C1
ex_c1r <- split2/total_c2
ex_c1r
# 10. find total variance in C2 explained by the model
ex_mod <- (ex_a1 + ex_a2 + ex_c1r)
ex_mod

# 11. find total proportion of variance in C2 explained by C1
ex_c1_tot <- con_c1_to_c2/total_c2
ex_c1_tot

# 12. find the additional proportion of variance in C2 explain by adding the school level-2 effect at time-2
ex_mod - ex_c1_tot




# Measurement models
# Model 1
# time 1
m11 <- matrix(
  c(0.8, 0.4, 0.2, 0.0,
    0.95, 0.9, 0.5, 0.3), byrow=T, ncol=4
)
dist(m11)
# time 2
m12 <- matrix(
  c(0.85, 0.44, 0.23, 0.2,
    0.97, 0.92, 0.48, 0.35), byrow=T, ncol=4
)
dist(m12)

# convert to logits
tr_logit <- function(x){
  log(x/(1-x))
}

apply(m11, c(1,2), tr_logit)
apply(m12, c(1,2), tr_logit)

# transition probabilities changed slightly. Mimics what is likely to occur where response probabilities can't exactly equal over time, but you can make the argument that they are close to invariant.

# Model 2
# time 1
m11 <- matrix(
  c(0.41, 0.32, 0.07, 0.00,
    0.94, 0.92, 0.34, 0.02,
    0.96, 0.94, 0.76, 0.37), byrow=T, ncol=4
)
dist(m11)
# time 2
m12 <- matrix(
  c(0.57, 0.43, 0.16, 0.03,
    0.97, 0.95, 0.43, 0.20,
    1.00, 0.97, 0.83, 0.45), byrow=T, ncol=4
)
dist(m12)





# kaplan & Walpole (2005)
X <- matrix(
  c(0, 0, 0, 0, 0,
    0.6, 0.03, 0.02, .00, 0.0,
    0.96, 0.59, 0.18, 0.0, .0,
    .99, .99, .90, .01, .00,
    1, .96, .91, .97, .38), byrow=T, ncol=5
)
dist(X)

        



## Converting response probabilities to logits
