# First, the packages
library("tidyverse") #mainly for dplyr and ggplot, but I always load this
library("lm.beta") # to get standardised betas
library("patchwork") # for plotting

# Second, the data
dat <- read.delim("16PF/data.csv", stringsAsFactors = FALSE) #read.csv doesnt work

# running a simple regression:
lm(data = dat,
   formula = A1 ~ A2)

# saving the results and extracting information from it:
model1 <- lm(data = dat,
             formula = A1 ~ A2)

summary(model1) # lots of information, but we want specific things, so lets save the summary and extract some stuff

model1_summary <- summary(model1)

model1_summary$r.squared # the r squared

model1_summary$coefficients["A2", 1] # the unstandardised beta for the "A2" predictor. Lets get the standardised

model1_summary_beta <- lm.beta(model1)
model1_summary_beta$standardized.coefficients # to extract them

# the summary adds the standardised beta to the summary data, it's awesome.
summary(model1_summary_beta)


# running a multiverse
# let's start easy. let's predict B1 from any of the As

predictors <- c("A1", "A2", "A3", "A4", "A5",
                "A6", "A7", "A8", "A9", "A10") 

predictors_formula <- paste(predictors, "~ B1")

out <- list()
R2s <- NA
std_betas <- NA

for(i in 1:length(predictors_formula)) {

out[[i]] <- lm(data = dat,
        formula = predictors_formula[i])
R2s[i] <- summary(out[[i]])$r.squared
std_betas[i] <- lm.beta(out[[i]])$standardized.coefficients[2]
  
}

out_table <- data.frame(predictor1 = predictors,
           R2 = R2s,
           std_beta = std_betas)


out_table
out_table2 <- out_table[order(out_table$std_beta),]
out_table2$n <- 1:nrow(out_table)

mvp <-  ggplot(data = out_table2,
               aes(x = n, y = std_beta)) +
  geom_point() +
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

#out_table3$Bigdecision <- factor(out_table3$Bigdecision, levels = "")

dashboard <- ggplot(data = out_table3,
                    aes(x = n, y = Decision, colour = Bigdecision)) +
  facet_grid(Bigdecision ~ ., scales = "free", space = "free", drop = ) +
  geom_point(aes(colour = Bigdecision), shape = 108, size = 10) +
  labs(x = "specification number") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=10))




mvp / dashboard + plot_layout(heights = c(2,2))


###################################



# lets try all combinations of predictors

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
  output[[i]] <- lm(data = dat,
                    formula = paste("B1 ~ ",
                                    predictor_grid2[i,"formula"]))

}

R2 <- NA

for(i in 1:nrow(predictor_grid2)) {
  R2[i] <- summary(output[[i]])$r.squared
}

final <- cbind(predictor_grid, R2)

final2 <- final[order(final$R2),]
final2$n <- 1:nrow(final2)

final2 <- final2 %>%
  rowwise() %>%
  mutate(n_pred = sum(!is.na(c_across(A1:A10))))

mvp <- ggplot(data = final2,
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


dashboard <- ggplot(data = final3,
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


mvp / dashboard + plot_layout(heights = c(1,3))
