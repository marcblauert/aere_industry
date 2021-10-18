# HEADER ----

# Term Paper in Advanced Environmental and Resource Economics (WiSe 2020/21)
# Topic: Industry Presence and Urban Climate Action: Evidence from a Set of Dutch Cities
# Student: Marc Blauert (ID: 605555)

# Purpose of the script: Conduct the csQCA analysis.

# Setup ----

rm(list = ls())
pacman::p_load(tidyverse, stringr, RColorBrewer, hrbrthemes, corrplot, QCA, SetMethods, xtable)

# Load set of cities ----
QCA_matrix <- "output/tables/QCA_matrix.csv"
QCA_matrix <- read.csv2(QCA_matrix, header = TRUE, stringsAsFactors = FALSE, row.names = 1)


# Explaining presence of outcome (OUTCOME) ----

# Individual necessity
nec_Y <- QCAfit(QCA_matrix[, -4], QCA_matrix[, 4]); nec_Y

# Individual sufficiency
suff_Y <- QCAfit(QCA_matrix[, -4], QCA_matrix[, 4], necessity = FALSE); suff_Y

# Truth table
tt_Y <- truthTable(QCA_matrix,
                   outcome = colnames(QCA_matrix)[4],
                   conditions = colnames(QCA_matrix)[-4],
                   complete = TRUE,
                   show.cases = TRUE,
                   incl.cut = 0.75,
                   sort = "incl"); tt_Y

# Logical minimization (solution formula)
c_sol_Y <- minimize(tt_Y, details = TRUE); c_sol_Y


# Explaining absence of outcome (~OUTCOME) ----

# Individual necessity
nec_no_Y <- QCAfit(QCA_matrix[, -4], QCA_matrix[, 4], neg.out = TRUE); nec_no_Y

# Individual sufficiency
suff_no_Y <- QCAfit(QCA_matrix[, -4], QCA_matrix[, 4], necessity = FALSE, neg.out = TRUE); suff_no_Y

# Truth table
tt_no_y <- truthTable(QCA_matrix,
                      outcome = colnames(QCA_matrix)[4],
                      conditions = colnames(QCA_matrix)[-4],
                      complete = TRUE,
                      show.cases = TRUE,
                      incl.cut = 0.75,
                      sort = "incl",
                      neg.out = TRUE); tt_no_y

# Logical minimization (solution formula)
c_sol_no_y <- minimize(tt_no_y, details = TRUE);c_sol_no_y


# Export results ----

export_qca <- c("nec_Y", "suff_Y", "nec_no_Y", "suff_no_Y")

for(i in 1:length(export_qca)) {
  write.csv2(get(export_qca[i]),
             paste0("output/tables/",
                    export_qca[i],
                    ".csv"),
             row.names = TRUE)
}