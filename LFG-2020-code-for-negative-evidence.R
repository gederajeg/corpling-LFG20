# codes to run chi-square test to check if the zero (0)
# occurrence of 'impose' in AV mengenai is a significant absense
# this method adopts Stefanowitsch's (2006; 2008) proposal of Negative Evidence using
# statistical corpus data:
# 
# Stefanowitsch, Anatol. 2006. Negative evidence and the raw frequency fallacy. Corpus Linguistics and Linguistic Theory 2(1). 61–77.
# Stefanowitsch, Anatol. 2008. Negative entrenchment: A usage-based approach to negative evidence. Cognitive Linguistics 19(3). 513–531. https://doi.org/10.1515/COGL.2008.020.


mengenai_sense <- c(0, (255+29))
dikenai_sense <- c(124, (7+8))
mtx <- cbind(mengenai_sense, dikenai_sense)
dimnames(mtx) <- list(senses = c("impose/subject to", "other senses"),
                      verbs = c("AV: mengenai", "PASS: dikenai"))
mtx
mtx_chisq <- chisq.test(mtx, correct = FALSE) # chi-square
mtx_chisq
phi_coefficient <- sqrt(mtx_chisq$statistic/sum(mtx)*(min(dim(mtx))-1))
round(unname(phi_coefficient), 3) # Phi Coefficient for effect size

# zero occurrence of 'impose' in AV mengenai is a highly significant absence 
# and can point to the negative evidence for this sense to occur in AV.
# see the Google Spreadsheet version of the analysis at http://bit.ly/negative-evidence
# the Google Spreadsheet includes some descriptive prose.

fisher.test(mtx, alternative = "less")$estimate
# odds ratio (i.e. 0) from the Fisher exact test indicates that it is highly unlikely that 'impose' sense occurs in AV mengenai than in PASS dikenai
