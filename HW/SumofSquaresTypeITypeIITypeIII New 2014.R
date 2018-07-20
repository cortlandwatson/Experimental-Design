# This gives us Type II Sum of Squares in SPSS
AnovaModel.2 <- (lm(days ~ gender*type, data=Dataset))
Anova(AnovaModel.2)
# This gives us Type I Sum of Squares in SPSS
AnovaModel.1 <- anova(lm(days ~ gender*type, data=Dataset))
AnovaModel.1

# This gives us Type III Sum of Squares in SPSS
cancer.lm2 <- lm(days~type*gender, data=Dataset, contrasts=list(type=contr.sum, gender=contr.sum))
Anova(cancer.lm2, type="III")


