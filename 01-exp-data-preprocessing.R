library(tidyverse)
library(googlesheets4)
library(vcd)

# ===== DO NOT RUN! ======
response_sheet <- read_lines("dataLFG20/sentence-elicitation-response-link.txt")

participantdata <- read_sheet(ss = response_sheet,
                              sheet = 1)

# a method to check duplicated responses that were removed eventually
participantdata |>
  select(1:6, matches("dikenai")) |>
  rename(sentence = `Buatlah satu kalimat menggunakan kata: "dikenai" (tanpa perlu tanda kutip).`) -> x
x |>
  filter(`Nama Lengkap (akan dirahasiakan; hanya untuk keperluan administrasi kelas)` %in%
           filter(count(x, `Nama Lengkap (akan dirahasiakan; hanya untuk keperluan administrasi kelas)`,
                        sort = T),
                  n > 1)[[1]]) |>
  select(`Nama Lengkap (akan dirahasiakan; hanya untuk keperluan administrasi kelas)`, sentence) |>
  arrange(`Nama Lengkap (akan dirahasiakan; hanya untuk keperluan administrasi kelas)`) |>
  as.data.frame()
# ===== DO NOT RUN! ======

# read in "dikenai" experimental sentence ======
# read_sheet(ss = response_sheet,
#            sheet = "dikenai",
#            range = "A2:B104", 
#            col_names = c("sentence", "usage")) |> 
#   write_tsv("dataLFG20/dikenai_exp.tsv")
dikenai <- read_tsv('dataLFG20/dikenai_exp.tsv')

# read in "dikenakan" experimental sentence ======
# read_sheet(ss = response_sheet,
#            sheet = "dikenakan",
#            range = "A2:B104",
#            col_names = c("sentence", "usage")) |>
#   write_tsv("dataLFG20/dikenakan_exp.tsv")
dikenakan <- read_tsv('dataLFG20/dikenakan_exp.tsv')

# read in "mengenai" experimental sentence ======
# read_sheet(ss = response_sheet,
#            sheet = "mengenai",
#            range = "A2:C104",
#            col_names = c("sentence", "pos", "usage")) |>
#   write_tsv("dataLFG20/mengenai_exp.tsv")
mengenai <- read_tsv('dataLFG20/mengenai_exp.tsv')

# read in "mengenakan" experimental sentence ======
# read_sheet(ss = response_sheet,
#            sheet = "mengenakan") |>
#   filter(is.na(Duplicated)) |> 
#   select(9, 10) |> 
#   rename(sentence = `Buatlah satu kalimat menggunakan kata: "mengenakan" (tanpa perlu tanda kutip).`,
#          usage = Usage) |> 
#   write_tsv("dataLFG20/mengenakan_exp.tsv")
mengenakan <- read_tsv('dataLFG20/mengenakan_exp.tsv')

# KENAI analyses ======
mengenai_verb <- mengenai |> 
  filter(str_detect(pos, "^(prep|irre)", negate = TRUE)) |> 
  select(-pos) |> 
  mutate(verb = "mengenai")
dikenai_verb <- dikenai |> 
  filter(str_detect(usage, "(irrel|typo|unclear)", negate = TRUE)) |> 
  mutate(verb = "dikenai")
kenai_all <- mengenai_verb |> 
  select(-sentence) |> 
  bind_rows(dikenai_verb |> 
              select(-sentence))
kenai_all_count <- table(kenai_all$verb, kenai_all$usage)
fisher.test(kenai_all_count)
fisher.test(kenai_all_count)$p.value < 0.001
fisher.test(kenai_all_count)$p.value < 0.0001
chisq.test(kenai_all_count, correct = TRUE)$stdres

kenai_all_count_df <- kenai_all_count |> 
  data.frame() |> 
  rename(verbs = Var1, senses = Var2, n = Freq) |> 
  group_by(verbs) |> 
  mutate(perc = round(n/sum(n) * 100, 1)) |> 
  ungroup()

kenai_all_count_df |> 
  ggplot(aes(x = verbs, y = n, fill = senses)) + 
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  geom_text(aes(label = paste(round(perc, 2), "%", sep = "")), 
            position = position_dodge(0.9), 
            vjust = -0.5) + 
  geom_text(aes(label = paste("n=", n, sep = "")), 
            position = position_dodge(0.9), 
            vjust = 1.25) +
  scale_fill_manual(values=c("#A6CEE3", "#FDBF6F", "#999999")) +
  labs(fill = "Senses") +
  ylab("Absolute frequency (n)") +
  xlab("Verbs") +
  scale_x_discrete(breaks = c("dikenai", "mengenai"),
                   labels = c("dikenai (PASS)", "mengenai (AV)")) +
  theme_light()


