# for the more in depth code, check out the multiverse.R code :)

# Code here: 
# https://github.com/sdparsons/multiverse_intro

# Data here (16PF):
# https://openpsychometrics.org/_rawdata/

library("tidyverse")
library("lm.beta")
library("patchwork")

dat <- read.delim("16PF/data.csv", 
                  stringsAsFactors = FALSE)

model1_R2 <- summary(lm(data = dat,
   formula = A1 ~ A2))$r.squared

model1 <- lm(data = dat,
             formula = A1 ~ A2)

model1_std <- lm.beta(model1)
View(model1_std)


#################

predictors <- c("A1", "A2", "A3", "A4", "A5",
                "A6", "A7", "A8", "A9", "A10") 

predictors_formula <- paste("B1 ~",
                            predictors)


out <- list()
R2s <- NA
std_betas <- NA

for(i in 1:length(predictors)) {

out[[i]] <- lm(data = dat,
   formula = predictors_formula[i])

R2s[i] <- summary(out[[i]])$r.squared
std_betas[i] <- lm.beta(out[[i]])$standardized.coefficients[2]

}

out_table <- data.frame(predictor1 = predictors,
                        R2 = R2s,
                        std_beta = std_betas)

out_table2 <- out_table[order(out_table$std_beta), ]
out_table2$n <- 1:nrow(out_table2)


mvp <- ggplot(data = out_table2,
       aes(x = n, y = std_beta)) +
  geom_point()  +
  labs(x = " ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=10)) 

out_table3 <- out_table2 %>%
  gather(Bigdecision, Decision, -R2, -std_beta, -n) %>%
  filter(Decision != "NA")

dashboard <- ggplot(data = out_table3,
       aes(x = n, y = Decision)) +
  geom_point(shape = 108, size = 7)  +
  labs(x = "specification number") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=10))

mvp / dashboard


###################

predictor_list <- list(A1 = c(NA, "A1"),
                       A2 = c(NA, "A2"),
                       A3 = c(NA, "A3"),
                       A4 = c(NA, "A4"),
                       A5 = c(NA, "A5"),
                       A6 = c(NA, "A6"),
                       A7 = c(NA, "A7"),
                       A8 = c(NA, "A8"),
                       A9 = c(NA, "A9"),
                       A10 = c(NA, "A10")) 

predictor_grid <- expand.grid(predictor_list)

predictor_grid <- predictor_grid[-1,]

predictor_grid2 <- predictor_grid %>%
  tidyr::unite(formula, A1:A10, sep = "+", na.rm = TRUE)


output <- list()

for(i in 1:nrow(predictor_grid2)) {
  
  output[[i]] <- lm(data= dat,
                     formula = paste("B1 ~",
                                     predictor_grid2[i,"formula"]))
  
}


R2 <- NA

for(i in 1:nrow(predictor_grid2)) {

 R2[i] <- summary(output[[i]])$r.squared
   
}

final <- cbind(predictor_grid, R2)

final2 <- final[order(final$R2), ]
final2$n <- 1:nrow(final2)

mvp2 <- ggplot(data = final2,
              aes(x = n, y = R2)) +
  geom_point() +
  labs(x = " ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=10)) 

final3 <- final2 %>%
  gather(Bigdecision, Decision, -R2, -n) %>%
  filter(Decision != "NA")

final3$Bigdecision <- factor(final3$Bigdecision, levels = names(predictor_grid))

dashboard2 <- ggplot(data = final3,
                     aes(x = n, y = Decision, colour = Bigdecision)) +
  facet_grid(Bigdecision ~ ., scales = "free", space = "free", drop = ) +
  geom_point(aes(colour = Bigdecision), shape = 108, size = 7) +
  labs(x = "specification number") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=10))

mvp2 / dashboard2






