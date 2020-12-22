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

model1_summary <- summary(model1) # we can save the summaries like this

model1_summary$r.squared # the r squared

model1_summary$coefficients["A2", 1] # the unstandardised beta for the "A2" predictor. Lets get the standardised

model1_summary_beta <- lm.beta(model1)
model1_summary_beta$standardized.coefficients # to extract the standardised estimates

# the summary adds the standardised beta to the summary data, it's awesome.
summary(model1_summary_beta)


# running a multiverse
# let's start easy. let's predict B1 from any of the A variables. 

predictors <- c("A1", "A2", "A3", "A4", "A5",
                "A6", "A7", "A8", "A9", "A10") # our list of predictors

# create formula for each model in the form "B1 ~ A1"
predictors_formula <- paste("B1 ~", predictors)

# we'll save our model outputs into these empty objects next
out <- list()
R2s <- NA
std_betas <- NA

# in this for loop we'll run all of our models, save the models into the list 'out', and save the Rsquared and standardised beta from each model into R2s and std_betas, respectively .
for(i in 1:length(predictors_formula)) {

out[[i]] <- lm(data = dat,
        formula = predictors_formula[i])
R2s[i] <- summary(out[[i]])$r.squared
std_betas[i] <- lm.beta(out[[i]])$standardized.coefficients[2]
  
}

# combine the outputs into a data-frame (you could also set up this data frame before the for loop and save the outputs directly into it)
out_table <- data.frame(predictor1 = predictors,
           R2 = R2s,
           std_beta = std_betas)


out_table # check what it looks like

# order the data by the output of interest, here the standardised beta. Then add a variable 'n' that is just the model order - we'll use this in plotting
out_table2 <- out_table[order(out_table$std_beta),]
out_table2$n <- 1:nrow(out_table)

# the video has a more in depth description of this. In short, we're making a scatterplot with the models ordered by the std_betas
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

# we need to restructure the data a bit to get things working for the dashboard. gather() from the dplyr package transforms data from wide to long.
out_table3 <- out_table2 %>%
  gather(Bigdecision, Decision, -R2, -std_beta, -n) %>%
  filter(Decision != "NA")

# not needed now, but we'll use in the later example. 
# out_table3$Bigdecision <- factor(out_table3$Bigdecision, levels = "")

# the dashboard is a bit like a scatterplot too. the points correspond to the decsions made in creating the model - in this case, which variable we chose as the first predictor. 
dashboard <- ggplot(data = out_table3,
                    aes(x = n, y = Decision)) +
  geom_point(, shape = 108, size = 10) +
  labs(x = "specification number") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=10))



# using the patchwork package we can plot our saved ggplot objects and combine them in great ways.
mvp / dashboard + plot_layout(heights = c(2,2))


###################################



# lets try all combinations of predictors. In this more complicated example we will predict B1 with any combination of the 10 A variables. 

# In this list we set up our multiverse decisions. Later, I describe the 'BigDecision' as each object in the list and the 'Decision' as the choices within that decision - e.g. here NA or A1. This is mainly important when it comes to plotting everything
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

# expand grid generates all possible combinations from our list above
predictor_grid <- expand.grid(predictor_list)

# the first row will be all NAs, so we'll remove that. 
predictor_grid <- predictor_grid[-1,]

# to reduce the number of models, we could also run
predictor_grid <- predictor_grid %>%
  sample_n(200)

# using the unite() function from the tidyr package (note that :: lets us run the function without loading the package) allows us to combine the variables into a string that we'll use shortly to make our lm formula. It will ignore the NAs and create all combinations - e.g. "A1+A4+A5+A10"
predictor_grid2 <- predictor_grid %>%
  tidyr::unite(formula, A1:A10, sep = "+", na.rm = TRUE)

# this time we'll run all of the models in one for loop, and extract the information we want in another for loop. I prefer this as it allows you to adapt the code needed to extract the information and run it without needing to re-run all models. 
output <- list()

# note that the formula pastes together the DV we're interested in with the formula for that run in the loop. 
for(i in 1:nrow(predictor_grid2)) {
  output[[i]] <- lm(data = dat,
                    formula = paste("B1 ~ ",
                                    predictor_grid2[i,"formula"]))

}

# let's extract the variance explained, Rsquared, this time
R2 <- NA

for(i in 1:nrow(predictor_grid2)) {
  R2[i] <- summary(output[[i]])$r.squared
}

# lets put together our output with our model specification grid
final <- cbind(predictor_grid, R2)

# as above, putting things in order
final2 <- final[order(final$R2),]
final2$n <- 1:nrow(final2)

# this was not in the video. But, it's useful to have a picture of the number of predictors in a lm model - we should expect that including more predictors will lead to more more explained variance. 
final2 <- final2 %>%
  rowwise() %>%
  mutate(n_pred = sum(!is.na(c_across(A1:A10))))

# plotting the model R2 estimates
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

# restructuring the data, as above
final3 <- final2 %>%
  gather(Bigdecision, Decision, -R2, -n) %>%
  filter(Decision != "NA")

# we want the order of variables to be as they are set up above (should fix e.g. alphabetical order issues)
final3$Bigdecision <- factor(final3$Bigdecision, levels = names(predictor_grid))

# in addition to the code above, we're adding facet_grid(). This will plot each of the BigDecisions separately (note that we removed the NAs earlier, so we will either have a point plotted to represent that variable being included in the model, or not plotted.)
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

# there are a lot of model specs to plot, so we'll change the layout slightly
mvp / dashboard + plot_layout(heights = c(1,3))

