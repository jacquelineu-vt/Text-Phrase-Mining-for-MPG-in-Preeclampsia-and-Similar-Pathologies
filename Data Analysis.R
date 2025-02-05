library(readxl)
library(ggplot2)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(paletteer)
library(ggfortify)
library(viridis)
library(ggplot2)
library(RColorBrewer)
library(stringr)

PE<-read_excel("preeclampsia (1) (1).xlsx", sheet=1)
names(PE)[names(PE) == "score"] <- "PE"
PE <- PE[,c("phrase", "PE")]
substr(PE$phrase, 1, 1) <- toupper(substr(PE$phrase, 1, 1))
HELLP<-read_excel("preeclampsia (1) (1).xlsx", sheet=3)
names(HELLP)[names(HELLP) == "score"] <- "HELLP"
HELLP <- HELLP[,c("phrase", "HELLP")]
substr(HELLP$phrase, 1, 1) <- toupper(substr(HELLP$phrase, 1, 1))
AFL<-read_excel("preeclampsia (1) (1).xlsx", sheet=4)
names(AFL)[names(AFL) == "score"] <- "AFL"
AFL <- AFL[,c("phrase", "AFL")]
substr(AFL$phrase, 1, 1) <- toupper(substr(AFL$phrase, 1, 1))
TA<-read_excel("preeclampsia (1) (1).xlsx", sheet=5)
names(TA)[names(TA) == "score"] <- "TA"
TA <- TA[,c("phrase", "TA")]
substr(TA$phrase, 1, 1) <- toupper(substr(TA$phrase, 1, 1))
APL<-read_excel("preeclampsia (1) (1).xlsx", sheet=6)
names(APL)[names(APL) == "score"] <- "APL"
APL <- APL[,c("phrase", "APL")]
substr(APL$phrase, 1, 1) <- toupper(substr(APL$phrase, 1, 1))
SLE<-read_excel("preeclampsia (1) (1).xlsx", sheet=7)
names(SLE)[names(SLE) == "score"] <- "SLE"
SLE <- SLE[,c("phrase", "SLE")]
substr(SLE$phrase, 1, 1) <- toupper(substr(SLE$phrase, 1, 1))

dat1<-merge(HELLP,AFL,by="phrase",all=TRUE)
dat2<-merge(dat1,TA,by="phrase",all=TRUE)
dat3<-merge(dat2,APL,by="phrase",all=TRUE)
dat4<-merge(dat3,SLE,by="phrase",all=TRUE)
dat5<-merge(dat4,PE,by="phrase",all=TRUE)

dat5$HELLP<-ifelse(is.na(dat5$HELLP),0,dat5$HELLP)
dat5$AFL<-ifelse(is.na(dat5$AFL),0,dat5$AFL)
dat5$TA<-ifelse(is.na(dat5$TA),0,dat5$TA)
dat5$APL<-ifelse(is.na(dat5$APL),0,dat5$APL)
dat5$SLE<-ifelse(is.na(dat5$SLE),0,dat5$SLE)
dat5$PE<-ifelse(is.na(dat5$PE),0,dat5$PE)


remove_these <- c("_diet", "activity_level", "ADHD", "alcohol", "alcohol_exposure", "alcohol_use_disorder", "anxiety", 
                  "bipolar", "BMI", "cocaine", "cognitive_behavioral_therapy", "counseling", "depression", "dietary_modification" ,
                  "drug_exposure", "drug_use", "economic_status", "excessive_weight_gain", "exercise", "gestational_weight_gain",
                  "health_literacy", "housing", "insurance_status", "low_carbohydrate", "marijuana", "mental_health", "methamphetamine",
                  "number_of_prenatal_visits", "nutrition", "nutrition_counseling", "OCD", "panic_disorder", "prenatal_visits",
                  "psychiatric_disease", "Race", "schizophrenia", "sedentary", "sexual_partners",
                  "socio-economic_status" , "stress", "substance_use", "walking", "zip_code", "_diet", "uninsured", "mental_healthcare")
substr(remove_these, 1, 1) <- toupper(substr(remove_these, 1, 1))

dat_final <- subset(dat5, !phrase %in% remove_these)
dat_final <- subset(dat_final, !(HELLP == 0 & phrase == "PIGF"))

dat_final_long <- pivot_longer(dat_final, 
                               cols = c("PE", "HELLP", "AFL", "APL", "TA", "SLE"),
                               values_to = "score",
                               names_to = c("disease"))

color_values <- paletteer_c("ggthemes::Blue-Green Sequential", 30)
color_values <- color_values[-(1:5)]


ggplot(data = dat_final_long, aes(x = disease, y = phrase, fill = score)) + 
  geom_tile() + theme_bw() + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_gradientn(colors = color_values,
                       na.value = "transparent") + 
  theme(panel.grid.major = element_blank())

ggplot(data = dat_final_long, aes(x = disease, y = phrase, fill = score)) + 
  geom_tile() + 
  theme_bw() + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  theme(panel.grid.major = element_blank())

ggplot(data = dat_final_long, aes(x = disease, y = phrase, fill = score)) + 
  geom_tile() + 
  theme_bw() + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_gradientn(colors = brewer.pal(5, "Blues"), na.value = "transparent") + 
  theme(panel.grid.major = element_blank())

ggplot(data = dat_final_long, aes(x = disease, y = phrase, fill = score)) + 
  geom_tile() + 
  theme_bw() + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_gradient(low = "white", high = "black") + 
  theme(panel.grid.major = element_blank())

pca1 <- princomp(dat_final[,-1], scores = TRUE)

summary(pca1) ## tells us the proportion of variance explained from eeach component

var_info <- pca1$sdev^2/sum(pca1$sdev^2) ## TThis is the calculation to get the info from the summary above
pca_data <- pca1$scores

textdat <- data.frame(x = 1:6, y = rep(1.2, 6),
                      lab = paste0("Explained Variance\n", round(100*var_info, 1), "%"))

loadings_dat <- data.frame(pca1$loadings[1:6, 1:6])
loadings_dat$disease <- rownames(loadings_dat)

loadings_long <- pivot_longer(loadings_dat, cols = starts_with("Comp"))

ggplot() + 
  geom_col(position = "dodge",
           data = loadings_long, aes(x = name, y = value, fill = disease)) + theme_bw() + 
  geom_text(data = textdat,
            aes(x = x, y = y, 
                label = lab)) + ## this line and two above it add the text with "explained variance"
  coord_cartesian(ylim = c(-0.75, 1), # This focuses the x-axis on the range of interest
                  clip = 'off') + 
  scale_x_discrete(labels = paste("Component", 1:6)) + ## change the x axis to have nicer names
  labs(x = "", y = "Featured Weight", fill = "Disease") + 
  theme(legend.position = "bottom",
        plot.margin = margin(b = 5.5, l = 5.5, r = 5.5, t = 50))

ggplot() + 
  geom_col(position = "dodge",
           data = loadings_long, aes(x = name, y = value, fill = disease)) + 
  theme_bw() + 
  geom_text(data = textdat,
            aes(x = x, y = y, 
                label = lab)) + 
  coord_cartesian(ylim = c(-0.75, 1), clip = 'off') + 
  scale_x_discrete(labels = paste("Component", 1:6)) + 
  labs(x = "", y = "Featured Weight", fill = "Disease") + 
  theme(legend.position = "bottom",
        plot.margin = margin(b = 5.5, l = 5.5, r = 5.5, t = 55))  # Adjusted top margin to reduce space

ggplot() + 
  geom_col(position = "dodge",
           data = loadings_long, aes(x = name, y = value, fill = disease)) + 
  theme_bw() + 
  geom_text(data = textdat,
            aes(x = x, y = y, 
                label = lab)) + 
  coord_cartesian(ylim = c(-0.75, 1), clip = 'off') + 
  scale_x_discrete(labels = NULL) + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  theme(legend.position = "bottom",
        plot.margin = margin(b = 5.5, l = 5.5, r = 5.5, t = 5.5))  # Adjusted top margin to reduce space

ggplot() + 
  geom_col(position = "dodge",
           data = loadings_long, aes(x = name, y = value, fill = disease)) + 
  theme_bw() + 
  geom_text(data = textdat,
            aes(x = x, y = y, label = lab), color = "black") +  # Set text color to black
  coord_cartesian(ylim = c(-0.75, 1), clip = 'off') + 
  scale_x_discrete(labels = paste("Component", 1:6)) + 
  labs(x = "", y = "Featured Weight", fill = "Disease") + 
  scale_fill_manual(values = gray.colors(n = nlevels(factor(loadings_long$disease)), start = 0, end = 1)) +
  theme(legend.position = "bottom",
        plot.margin = margin(b = 5.5, l = 5.5, r = 5.5, t = 50))


fviz_pca_var(pca1, col.var = "black")

# loadings_dat$label <- ""
# ggplot(data = loadings_dat, aes(x = 0, y = 0, color = disease)) + 
#   geom_textsegment(aes(xend = Comp.1, yend = Comp.2, label = label), arrow = arrow())
# geom_point()


autoplot(pca1, data = dat_final[,-1], loadings = TRUE, 
         loadings.label = TRUE, #loadings.label.vjust = 1,
         loadings.label.size = 5,
         # loadings.label.hjust = 1, 
         loadings.color = "black") + 
  theme_bw() + 
  labs(x = "PC1 (40.3%)", y = "PC2 (29.6%)")

autoplot(pca1, data = dat_final[,-1], loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size = 5,
         loadings.color = "black") + 
  theme_bw() + 
  labs(x = "PC1 (40.3%)", y = "PC2 (29.6%)") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black")

autoplot(pca1, data = dat_final[,-1], loadings = TRUE, 
         loadings.label = FALSE, 
         loadings.label.size = 5,
         loadings.color = "black") + 
  theme_bw() + 
  labs(x = NULL, y = NULL) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black")

###########
test <- expand.grid(x = c("PE", "HELLP", "AFL", "APL", "TA", "SLE"),
                    y = c("PE", "HELLP", "AFL", "APL", "TA", "SLE"))
test$protein_total <- NA

for (i in 1:nrow(test)){
  disease1 <- as.character(test$x[i])
  disease2 <- as.character(test$y[i])
  
  if (as.numeric(test$x[i]) > as.numeric(test$y[i])){next}
  
  test$protein_total[i] <- sum(dat_final[,disease1] != 0 & dat_final[,disease2] != 0)
}

test$y <- factor(test$y, levels = rev(levels(test$y)))

ggplot(data = test, aes(x = x, y = y, fill = protein_total)) +
  geom_tile() + 
  geom_text(aes(label = protein_total)) + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_gradientn(colors = c(brewer.pal(3, "Blues")[1:2], brewer.pal(5, "Blues")[3:5]),
                       values = scales::rescale(c(50, 75, 100)),  # Adjust the range (0 to 100 in this example)
                       na.value = "transparent") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank())

ggplot(data = test, aes(x = x, y = y, fill = protein_total)) +
  geom_tile(alpha = 0.6) + # Set alpha to make the black color semi-transparent
  geom_text(aes(label = protein_total), color = "white") +  # Set text color to white
  labs(x = "", y = "", fill = "") + 
  scale_fill_gradientn(colors = c("#CCCCCC", "black"),  # Darker shade of white
                       values = scales::rescale(c(0, 100)),  # Adjust the range based on your data
                       na.value = "transparent")
theme_bw() + 
  theme(panel.grid.major = element_blank())
