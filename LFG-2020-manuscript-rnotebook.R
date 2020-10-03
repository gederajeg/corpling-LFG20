## ----setup, message = FALSE, echo = FALSE-----------------------------------------------------
# global option chunck
knitr::opts_chunk$set(fig.width = 7, 
                      fig.asp = 0.618,
                      fig.retina = 2,
                      dpi = 300,
                      dev = "pdf",
                      echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      fig.path = "figures/")


## ----load-package, message = FALSE, warning = FALSE-------------------------------------------
library(tidyverse)
library(vcd)


## ----dikenai-conc-usage-----------------------------------------------------------------------
dikenai_conc <- readr::read_tsv('dataLFG20/dikenai_concordance.txt')
dikenai_conc_tokens <- dim(dikenai_conc)[1]
dikenai_sense <- dikenai_conc %>% 
  mutate(usage = replace(usage, usage == "physical_touch",
                         "come into touch/contact; hit"),
         usage = replace(usage, usage == "physical_disease_affected",
                         "affect (medical; mental)")) %>% 
  count(usage, sort = TRUE) %>% 
  mutate(prop = (n/sum(n))*100,
         verb = "dikenai")


## ----mengenai-conc-usage----------------------------------------------------------------------
mengenai_conc <- readr::read_tsv('dataLFG20/mengenai_concordance.txt')
mengenai_category_count <- mengenai_conc %>% 
  mutate(category = dplyr::if_else(category %in% c("typo", 
                                                   "unclear", 
                                                   "duplicate"),
                                   "excluded",
                                   category)) %>% 
  count(category, sort = TRUE) %>% 
  mutate(prop = round(n/sum(n) * 100, digits = 2))
mengenai_conc_lexicalverb <- mengenai_conc %>% 
  filter(category == "lexical_verb")
mengenai_usage <- mengenai_conc_lexicalverb %>% 
  mutate(usage_gen = senses,
         usage_gen = replace(usage_gen, 
                             usage_gen == "on target",
                             "come into touch/contact; hit"),
         usage_gen = replace(usage_gen,
                             usage_gen == "affect",
                             "affect (medical; mental)")) %>% 
  count(usage_gen, sort = TRUE) %>% 
  mutate(perc = round(n/sum(n) * 100, 2),
         verb = "mengenai")
names(mengenai_usage$n) <- mengenai_usage$usage_gen
mengenai_chisq <- chisq.test(mengenai_usage$n)


## ----mengenakan-conc-usage, message=FALSE-----------------------------------------------------
mengenakan_conc <- readr::read_tsv("dataLFG20/mengenakan_concordance.txt")
mengenakan_conc <- mengenakan_conc %>% 
  filter(!usage %in% c("unclear", "duplicate", "typo_mengenakkan")) 
mengenakan_conc <- mengenakan_conc %>% 
  mutate(usage = if_else(str_detect(node_sentences, "duri tajam di kepala-Nya"), "wear", usage)) %>% 
  mutate(usage2 = usage,
         usage2 = if_else(str_detect(usage, "^(wear-pp|apply OBJ)"), "wear", usage2))
mengenakan_conc <- mengenakan_conc %>% 
  mutate(usage2 = if_else(str_detect(usage, "^(deceive|hit|wear-me)"), "others", usage2)) # 'others' is for proportion for each is below 1%
mengenakan_usage <- mengenakan_conc %>% 
  count(usage2, sort = TRUE) %>% 
    rename(usage = usage2) %>% 
  mutate(perc = round(n/sum(n) * 100, 2),
         verb = "mengenakan")


## ----dikenakan-conc-usage, message = FALSE----------------------------------------------------
dikenakan_conc <- readr::read_tsv("dataLFG20/dikenakan_concordance.txt")
dikenakan_conc_tokens <- dim(dikenakan_conc)[1]
dikenakan_conc <- tibble::as_tibble(dikenakan_conc)
dikenakan_conc <- dplyr::filter(dikenakan_conc, !usage %in% c("duplicate", "unclear"))
dikenakan_conc <- dikenakan_conc %>% 
  mutate(usage2 = usage) %>% 
  mutate(usage2 = if_else(str_detect(usage, "^(wear-|be applied to)"), "others", usage2))
dikenakan_usage <- dikenakan_conc %>% 
  count(usage2, sort = TRUE) %>% 
  rename(usage = usage2) %>%
  mutate(prop = (n/sum(n))*100,
         verb = "dikenakan")
names(dikenakan_usage$n) <- dikenakan_usage$usage
dikenakan_chisq <- dikenakan_conc %>% 
  infer::chisq_test(response = usage2)
dikenakan_stdres <- chisq.test(dikenakan_usage$n)$stdres
dikenakan_chisq2 <- chisq.test(dikenakan_usage$n)


## ----kena-initial-distribution, fig.cap="Form-meaning pairings for *kena*-derived verbs based on (@kenai_initial) and (@dikenai_initial)."----
knitr::include_graphics("figsLFG20/rajeg-rajeg-arka_2020_fig1-kena_initial_form_meaning.png")


## ----leipzig-size, message = FALSE, warning = FALSE-------------------------------------------
corpsize <- readr::read_tsv("dataLFG20/leipzig_corpus_size.txt")
corpsize <- corpsize$Size[corpsize$Corpus == "ind_mixed_2012_1M-sentences.txt"]
corpsize <- prettyNum(corpsize, big.mark = ",")
mengenai_concs <- readr::read_tsv('dataLFG20/mengenai_concordance.txt')
mengenai_tokens <- mengenai_concs %>% 
    filter(category == "lexical_verb") %>% nrow()
mengenakan_tokens <- nrow(readr::read_tsv("dataLFG20/mengenakan_concordance.txt"))



## ----mengenai-dikenai-combined, message = FALSE, warning = FALSE------------------------------
mengenai_dikenai <- mengenai_usage %>% 
  rename(usage = usage_gen) %>% 
  bind_rows(dikenai_sense %>% 
              rename(perc = prop))
kenai_senses <- mengenai_dikenai %>% 
  group_by(usage) %>% 
  summarise(n = sum(n)) %>% 
  mutate(perc = round(n/sum(n) * 100, 2)) %>% 
  arrange(desc(n))


## ----mengenai-dikenai-chisq-independence------------------------------------------------------
kenai_xtab <- pivot_wider(select(mengenai_dikenai, usage, n, verb), names_from = verb, values_from = n, values_fill = 0L)
kenai_xtab <- as.matrix(data.frame(kenai_xtab, row.names = 1, stringsAsFactors = FALSE))
kenai_chisq <- chisq.test(kenai_xtab)
kenai_cramer <- round(sqrt(kenai_chisq$statistic / sum(kenai_xtab) * (min(dim(kenai_xtab)) - 1)), 3)
kenai_pval <- format(kenai_chisq$p.value, scientific = TRUE, digits = 3)
rownames(kenai_xtab) <- c("contact/touch/hit", "affect", "subject to")


## ----mengenai-dikenai-plot, fig.cap="Distribution of senses for *kenai* in PASS and AV"-------
mengenai_dikenai_plot <- mengenai_dikenai %>% 
  mutate(usage = fct_reorder(usage, -n)) %>% 
  ggplot(aes(x = verb, y = perc, fill = usage)) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = paste(round(perc, 2), "%", sep = "")), 
            position = position_dodge(0.9), 
            vjust = -0.5) + 
  geom_text(aes(label = paste("n=", n, sep = "")), 
            position = position_dodge(0.9), 
            vjust = 1.25) +
  scale_fill_manual(values=c("#A6CEE3", "#FDBF6F", "#009E73")) +
  labs(fill = "senses") +
  ylab("percentage") +
  xlab("verbs") +
  scale_x_discrete(breaks = c("dikenai", "mengenai"),
                   labels = c("dikenai (PASS)", "mengenai (AV)")) +
  theme_light()
p <- mengenai_dikenai_plot +
  labs(title = "Distribution of senses for *kenai* in PASS and AV")
ggsave("figsLFG20/rajeg-rajeg-arka_2020_fig2-senses-of-kenai-in-PASS-AV.png", plot = p, units = "in", height = 6, width = 8)
mengenai_dikenai_plot


## ----kenai-assocplot-saving, fig.asp=0.8, include = FALSE-------------------------------------
rownames(kenai_xtab)[1] <- "contact; hit"
colnames(kenai_xtab) <- c("AV-mengenai", "PASS-dikenai")
png("figsLFG20/rajeg-rajeg-arka_2020_fig0-1-association-plot-kenai.png", units = "in", width = 9, height = 8, res = 300)
vcd::assoc(kenai_xtab, main = expression(paste("Association plot between senses of ", italic(kenai), " and voice", sep = "")), shade = TRUE, sub = "Bluish shading indicates positive residuals while redish shows negative residuals.\nSignificant positive association (bluish): strong preference of 'hit' for AV and 'subject to' for PASS.", sub_gp = gpar(fontsize = 10), main_gp = gpar(fontsize = 15), labeling_args = list(set_varnames = c(A = "Senses", B = "Voice")))
dev.off()


## ----kenai-assoc-plot, fig.asp=0.8------------------------------------------------------------
vcd::assoc(kenai_xtab, main = expression(paste("Association plot between senses of ", italic(kenai), " and voice", sep = "")), shade = TRUE, sub = "Bluish shading indicates positive residuals while redish shows negative residuals.\nSignificant positive association (bluish): strong preference of 'hit' for AV and 'subject to' for PASS.", sub_gp = gpar(fontsize = 10), main_gp = gpar(fontsize = 15), labeling_args = list(set_varnames = c(A = "Senses", B = "Voice")))


## ----mengenakan-dikenakan-combined, message=FALSE, warning=FALSE------------------------------
mengenakan_dikenakan <- mengenakan_usage %>% 
  mutate(usage = replace(usage, usage == "impose", "subject to/imposed")) %>% 
  bind_rows(dikenakan_usage %>% 
              rename(perc = prop))
kenakan_senses <- mengenakan_dikenakan %>% 
  group_by(usage) %>% 
  summarise(n = sum(n)) %>% 
  mutate(perc = round(n/sum(n) * 100, 2)) %>% 
  arrange(desc(n))


## ----mengenakan-dikenakan-plot, fig.cap="Distribution of senses for *kenakan* in PASS and AV"----
mengenakan_dikenakan_plot <- mengenakan_dikenakan %>% 
  mutate(usage = fct_reorder(usage, -n)) %>% 
  ggplot(aes(x = verb, y = perc, fill = usage)) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = paste(round(perc, 2), "%", sep = "")), 
            position = position_dodge(0.9), 
            vjust = -0.5) + 
  geom_text(aes(label = paste("n=", n, sep = "")), 
            position = position_dodge(0.9), 
            vjust = 1.25) +
  scale_fill_manual(values=c("#1F78B4", "#FDBF6F",
                             "#999999")) +
  labs(fill = "senses") +
  ylab("percentage") +
  xlab("verbs") +
  scale_x_discrete(breaks = c("dikenakan", "mengenakan"),
                   labels = c("dikenakan (PASS)", "mengenakan (AV)")) +
  theme_light()
p <- mengenakan_dikenakan_plot +
  labs(title = "Distribution of senses for *kenakan* in PASS and AV")
ggsave("figsLFG20/rajeg-rajeg-arka_2020_fig3-senses-of-kenakan-in-PASS-AV.png", plot = p, units = "in", height = 6, width = 8)
mengenakan_dikenakan_plot


## ----mengenakan-dikenakan-chisq-independence, message = FALSE, warning = FALSE----------------
kenakan_xtab <- pivot_wider(select(mengenakan_dikenakan, usage, n, verb), names_from = verb, values_from = n, values_fill = 0L)
kenakan_xtab <- as.matrix(data.frame(kenakan_xtab, row.names = 1, stringsAsFactors = FALSE))
kenakan_chisq <- chisq.test(kenakan_xtab)
kenakan_cramer <- round(sqrt(kenakan_chisq$statistic / sum(kenakan_xtab) * (min(dim(kenakan_xtab)) - 1)), 3)
kenakan_pval <- format(kenakan_chisq$p.value, scientific = TRUE, digits = 3)


## ----kenakan-assocplot-saving, fig.asp=0.8, include=FALSE-------------------------------------
colnames(kenakan_xtab) <- c("AV-mengenakan", "PASS-dikenakan")
png("figsLFG20/rajeg-rajeg-arka_2020_fig0-2-association-plot-kenakan.png", units = "in", width = 9, height = 8, res = 300)
vcd::assoc(kenakan_xtab, main = expression(paste("Association plot between senses of ", italic(kenakan), " and voice", sep = "")), shade = TRUE, sub = "Bluish shading indicates positive residuals while redish shows negative residuals.\nSignificant positive association (bluish): strong preference of 'wearing' for AV and 'subject to' for PASS.", sub_gp = gpar(fontsize = 10), main_gp = gpar(fontsize = 15), legend_args = list(width = unit(0.4, "lines")), labeling_args = list(set_varnames = c(A = "Senses", B = "Voice")))
dev.off()


## ----kenakan-assocplot, fig.asp=0.8-----------------------------------------------------------
vcd::assoc(kenakan_xtab, main = expression(paste("Association plot between senses of ", italic(kenakan), " and voice", sep = "")), shade = TRUE, sub = "Bluish shading indicates positive residuals while redish shows negative residuals.\nSignificant positive association (bluish): strong preference of 'wearing' for AV and 'subject to' for PASS.", sub_gp = gpar(fontsize = 10), main_gp = gpar(fontsize = 15), legend_args = list(width = unit(0.4, "lines")), labeling_args = list(set_varnames = c(A = "Senses", B = "Voice")))

