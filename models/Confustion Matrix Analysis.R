

# Insert libraries
library(ggplot2)
library(ggthemes)
library(RColorBrewer)



#--------------------------------------Confustion matrix analysis

cm.consolidate <- function (path, Observations, model) {
        dat = readRDS(path)
        cm = as.data.frame(dat$byClass[1:4, 1:2])
        cm$Observations <- Observations
        cm$Model <- model
        cm$class <- rownames(cm)
        rownames(cm) <- NULL
        cm$Sensitivity <- round(cm$Sensitivity, 2)
        cm$Specificity <- round(cm$Specificity, 2)
        
        return(cm)
}

cm.accurary <- function (path, name) {
        dat = readRDS(path)$overall[1]
        
        return(dat)
}

# create a confustion matrix for all models results
cm.1000.mlr <-
        cm.consolidate(path = 'models/Price Range/cm_Multinomial Multinomial Regression1000.rda',
                       Observations = 1000,
                       model = 'Multinomial Regression')


cm.10000.mlr <-
        cm.consolidate(path = 'models/Price Range/cm_Multinomial Multinomial Regression1000.rda',
                       Observations = 10000,
                       model = 'Multinomial Regression')

cm.5000.mlr <-
        cm.consolidate(path = 'models/Price Range/cm_Multinomial Multinomial Regression5000.rda',
                       Observations = 5000,
                       model = 'Multinomial Regression')

cm.5000.rf <-
        cm.consolidate(path = 'models/Price Range/cm_Random Forest5000.rda',
                       Observations = 5000,
                       model = 'Random Forest')

cm.1000.rf <-
        cm.consolidate(path = 'models/Price Range/cm_Random Forest1000.rda',
                       Observations = 1000,
                       model = 'Random Forest')

cm.10000.rf <-
        cm.consolidate(path = 'models/Price Range/cm_Random Forest10000.rda',
                       Observations = 10000,
                       model = 'Random Forest')

# Combine models
cm.all <-
        rbind(cm.1000.mlr,
              cm.5000.mlr,
              cm.10000.mlr,
              cm.1000.rf,
              cm.5000.rf,
              cm.10000.rf)


# Sensitivity plotting
ggplot(data = cm.all, aes(x = class, y = Sensitivity)) +
        geom_bar(stat = 'identity',
                 alpha = 0.9,
                 position = 'dodge',
                 aes(fill = Model)) +
        facet_grid(Observations ~ .) +
        ggtitle('Model Confusion Matrix Results: Class Sensitivity Faceted by Sample Size ') +
        xlab('Class') + ylab('Sensitivity') +
        theme_classic() +
        scale_fill_manual(values = c('#EF6A45', '#00B087')) +
        theme(
                legend.position = "bottom",
                text = element_text(size = 20),
                axis.text.x = element_text(angle = 0),
                plot.title = element_text(hjust = 0.5)
        )

# Sensitivity plotting
ggplot(data = cm.all, aes(x = class, y = Specificity)) +
        geom_bar(stat = 'identity',
                 alpha = 0.9,
                 position = 'dodge',
                 aes(fill = Model)) +
        facet_grid(Observations ~ .) +
        ggtitle('Model Confusion Matrix Results: Class Specificity Faceted by Sample Size ') +
        xlab('Class') + ylab('Sensitivity') +
        theme_classic() +
        scale_fill_manual(values = c('#EF6A45', '#00B087')) +
        theme(
                legend.position = "bottom",
                text = element_text(size = 20),
                axis.text.x = element_text(angle = 0),
                plot.title = element_text(hjust = 0.5)
        )

#--------------------------------------Accuracy

accuracy.mlr.1000 <-
        cm.accurary(path = 'models/Price Range/cm_Multinomial Multinomial Regression1000.rda',
                    name = 'LR 1000 Accuracy')
accuracy.mlr.5000 <-
        cm.accurary(path = 'models/Price Range/cm_Multinomial Multinomial Regression5000.rda',
                    name = 'LR 5000 Accuracy')
accuracy.mlr.10000 <-
        cm.accurary(path = 'models/Price Range/cm_Multinomial Multinomial Regression10000.rda',
                    name = 'LR 10000 Accuracy')

accuracy.rf.1000 <-
        cm.accurary(path = 'models/Price Range/cm_Random Forest1000.rda',
                    name = 'RF 1000 Accuracy')
accuracy.rf.5000 <-
        cm.accurary(path = 'models/Price Range/cm_Random Forest5000.rda',
                    name = 'RF 5000 Accuracy')
accuracy.rf.10000 <-
        cm.accurary(path = 'models/Price Range/cm_Random Forest10000.rda',
                    name = 'RF 10000 Accuracy')

accuracy.all <-
        rbind(
                accuracy.mlr.1000,
                accuracy.mlr.5000,
                accuracy.mlr.10000,
                accuracy.rf.1000,
                accuracy.rf.5000,
                accuracy.rf.10000
        )

accuracy.all <- as.data.frame(accuracy.all)
