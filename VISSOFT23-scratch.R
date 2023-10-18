# VISSOFT IST paper scratch tests
# Roberto E. Lopez-Herrejon
# ETS-LOGTI 2023

# https://www.r-bloggers.com/2022/01/manova-in-r-how-to-implement-and-interpret-one-way-manova/#google_vignette

library(ggplot2)
library(gridExtra)

box_sl <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")
box_sw <- ggplot(iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")
box_pl <- ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")
box_pw <- ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")

grid.arrange(box_sl, box_sw, box_pl, box_pw, ncol = 2, nrow = 2)

# Manova test
dependent_vars <- cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)
independent_var <- iris$Species

manova_model <- manova(dependent_vars ~ independent_var, data = iris)
summary(manova_model)
# Df Pillai approx F num Df den Df    Pr(>F)    
# independent_var   2 1.1919   53.466      8    290 < 2.2e-16 ***
#   Residuals       147                                            
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


summary.aov(manova_model)

# Response 1 :
#   Df Sum Sq Mean Sq F value    Pr(>F)    
# independent_var   2 63.212  31.606  119.26 < 2.2e-16 ***
#   Residuals       147 38.956   0.265                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Response 2 :
#   Df Sum Sq Mean Sq F value    Pr(>F)    
# independent_var   2 11.345  5.6725   49.16 < 2.2e-16 ***
#   Residuals       147 16.962  0.1154                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Response 3 :
#   Df Sum Sq Mean Sq F value    Pr(>F)    
# independent_var   2 437.10 218.551  1180.2 < 2.2e-16 ***
#   Residuals       147  27.22   0.185                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Response 4 :
#   Df Sum Sq Mean Sq F value    Pr(>F)    
# independent_var   2 80.413  40.207  960.01 < 2.2e-16 ***
#   Residuals       147  6.157   0.042                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Effect size on the dependent variables, partial eta squared
library(effectsize)

eta_squared(manova_model)


# Interpretation of MANOVA results with Post-HOc Test. Which group mean vector differs?
# Linear Discriminant Analysis LDA
library(MASS)

iris_lda <- lda(independent_var ~ dependent_vars, CV = F)
iris_lda


# 
# lda(independent_var ~ dependent_vars, CV = F)
# 
# Prior probabilities of groups:
#   setosa versicolor  virginica 
# 0.3333333  0.3333333  0.3333333 
# 
# Group means:
#   dependent_vars1 dependent_vars2 dependent_vars3 dependent_vars4
# setosa               5.006           3.428           1.462           0.246
# versicolor           5.936           2.770           4.260           1.326
# virginica            6.588           2.974           5.552           2.026
# 
# Coefficients of linear discriminants:
#   LD1         LD2
# dependent_vars1  0.8293776  0.02410215
# dependent_vars2  1.5344731  2.16452123
# dependent_vars3 -2.2012117 -0.93192121
# dependent_vars4 -2.8104603  2.83918785
# 
# Proportion of trace:
#   LD1    LD2 
# 0.9912 0.0088 

# LD1 = 0.83 * Sepal.Length + 1.53 * Sepal.Width - 2.20 * Petal Length - 2.81 * Petal.Width
# This is for the individual values
# ???? LD1 = 0.83 * 5.006 + 1.53 * 3.428 - 2.20 * 1.462 - 2.81 * 0.246


lda_df <- data.frame(
  species = iris[, "Species"],
  lda = predict(iris_lda)$x
)
lda_df



ggplot(lda_df) +
  geom_point(aes(x = lda.LD1, y = lda.LD2, color = species), size = 4) +
  theme_classic()

# Conclusion: he setosa species is significantly different when 
# compared to virginica and versicolor. These two are more similar, 
# suggesting that it was the setosa group that had the most impact for us to reject
# the null hypothesis.
