# Variance Decomposition: 3-class

# latent class time 1
p1 <- c(0.4, 0.4, 0.2)  # class probabilities
c1 <- c(1, 2, 0)        # latent class values


# multinomial intercepts
a1 <- c(0.40, 0.90, 0)  # time 1
  # class sizes
  p1 <- exp(a1[1])/(sum(exp(a1)))
  p2 <- exp(a1[2])/(sum(exp(a1)))
  p3 <- exp(a1[3])/(sum(exp(a1)))
  p1; p2; p3
a2 <- c(-15, -15, 0) # time 2

# Level-1 Transition Weights
gamma <- c(18, 17.5, -0.12, 16)

# level-2 Transition Weights
beta <- c(0.3, 0.3, 0.3, 0.3)


## class 1
p1 <- exp(a1)/(sum(exp(a1)))
# 1. ICC
sig_alpha1 <- c(0.8, 0.8)

icc <- sum(sig_alpha1)/(sum(sig_alpha1) + pi**2/3)
icc

# 2. variance C_1 (time 1 latent class)
var_c1 <- sum(p1*c1**2) - (sum(p1*c1)**2)
var_c1

# 3. contribution of C1 on C2
con_c1_on_c2 <- sum(gamma**2*var_c1)
con_c1_on_c2

# 4. Split con by icc
split1 <- con_c1_on_c2*icc
split2 <- con_c1_on_c2*(1-icc)
split1;split2
# 5. contribution of a1 to C2
con_a1_by_a2 <- sum(beta**2*sig_alpha1)
tot <-  split1 + con_a1_by_a2+ 2*sqrt(split1*con_a1_by_a2)
tot
# 6. total variance of C2
total_c2 <- sum((pi**2)/3 + split1 + tot + sig_alpha1)
total_c2
# 7. compute proportion of variance in C2 explained by a1
ex_a1 <- con_c1_on_c2/total_c2
ex_a1
# 8. proportion of variance in C2 explained by a2
ex_a2 <- sum(sig_alpha1/total_c2)
ex_a2
# 9. compute proportion of variance in C2 explained by residual of C1
ex_c1r <- sum(split2/total_c2)
ex_c1r
# 10. find total variance in C2 explained by the model
ex_mod <- (ex_a1 + ex_a2 + ex_c1r)
ex_mod

# 11. find total proportion of variance in C2 explained by C1
ex_c1_tot <- con_c1_on_c2/total_c2
ex_c1_tot

# 12. find the additional proportion of variance in C2 explain by adding the school level-2 effect at time-2
ex_mod - ex_c1_tot





# compute variance components
compute_r2_stats <- function(K, alpha1, sig_alpha1, gamma, beta){
  
  c1 <- 0:(K-1)
  ## class  probabilities (for variance)
  p1 <- exp(a1) / (sum(exp(a1)))
  # 1. ICC
  icc <- sum(sig_alpha1) / (sum(sig_alpha1) + pi ** 2 / 3)
  
  # 2. variance C_1 (time 1 latent class)
  var_c1 <- sum(p1 * c1 ** 2) - (sum(p1 * c1)) ** 2
  
  # 3. contribution of C1 on C2
  con_c1_on_c2 <- sum(gamma ** 2 * var_c1)
  
  # 4. Split con by icc
  split1 <- con_c1_on_c2 * icc
  split2 <- con_c1_on_c2 * (1 - icc)
  
  # 5. contribution of a1 to C2
  con_a1_by_a2 <- sum(beta ** 2 * sig_alpha1)
  tot <-  split1 + con_a1_by_a2 + 2 * sqrt(split1 * con_a1_by_a2)
  
  # 6. total variance of C2
  total_c2 <- sum((pi ** 2) / 3 + split1 + tot + sig_alpha1)
  
  # 7. compute proportion of variance in C2 explained by a1
  R2_1 <- ex_a1 <- con_c1_on_c2 / total_c2
  
  # 8. proportion of variance in C2 explained by a2
  R2_2 <- ex_a2 <- sum(sig_alpha1 / total_c2)
  # 9. compute proportion of variance in C2 explained by residual of C1
  R2_3 <- ex_c1r <- sum(split2 / total_c2)
  # 10. find total variance in C2 explained by the model
  R2_model <- ex_mod <- (ex_a1 + ex_a2 + ex_c1r)
  
  # 11. find total proportion of variance in C2 explained by C1
  R2_c1 <- ex_c1_tot <- con_c1_on_c2 / total_c2
  
  # 12. find the additional proportion of variance in C2 explain by adding the school level-2 effect at time-2
  R2_add <- ex_mod - ex_c1_tot
  
  out <-
    matrix(c(R2_1, R2_2, R2_3, R2_c1, R2_model, R2_add), ncol = 1)
  rownames(out) <-
    c("R2-alpha1",
      "R2-alpha2",
      "R2-C1-residual",
      "R2-C1",
      "R2-model",
      "R2-addition-level-2")
  return(out)
}





# 3-class model
compute_r2_stats(
  alpha1 = c(0.40, 0.90, 0),
  sig_alpha1 = c(0.8, 0.8),
  gamma = c(18, 17.5, -0.12, 16),
  beta = c(0.3, 0.3, 0.3, 0.3)
)

# 2-classmodel
compute_r2_stats(
  alpha1 = c(-0.405, 0),
  sig_alpha1 = c(0.8),
  gamma = c(4),
  beta = c(0.3)
)
