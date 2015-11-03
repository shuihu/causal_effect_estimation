source("simulations/full.simulation.R")

all.full.simulations <- function(param) {
  if (param == 1) {
    full.simulation(designs = array(c(8,9)), seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = TRUE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/all_honest_1000_repl.csv")
  } else if (param == 2) {
    full.simulation(designs = array(c(8,9)), seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/all_dishonest_1000_repl.csv")
  } else if (param == 3) {
    full.simulation(designs = array(c(8,9)), seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = TRUE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/four_honest_1000_repl.csv")
  } else if (param == 4) {
    full.simulation(designs = array(c(8,9)), seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/four_dishonest_1000_repl.csv")
  } else if (param == 5) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = TRUE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/all_honest0.5_1000_repl.csv")
  } else if (param == 6) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/all_dishonest0.5_1000_repl.csv")
  } else if (param == 7) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = TRUE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/four_honest0.5_1000_repl.csv")
  } else if (param == 8) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/four_dishonest0.5_1000_repl.csv")
  } else if (param == 9) {
    full.simulation(designs = array(c(8,9)), seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = TRUE, data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/all_dishonest2_1000_repl.csv")
  } else if (param == 10) {
    full.simulation(designs = array(c(8,9)), seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = TRUE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000_1000", output.filename = "extra/results/obs_10000/four_dishonest2_1000_repl.csv")
  } else if (param == 11) {
    full.simulation(designs = array(c(8,9)), seed = 1, is.honest = TRUE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, data.path = "../data_500_1000", output.filename = "extra/results/obs_500/all_honest_repl.csv")
  } else if (param == 12) {
    full.simulation(designs = array(c(8,9)), seed = 1, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, data.path = "../data_500_1000", output.filename = "extra/results/obs_500/all_dishonest_repl.csv")
  } else if (param == 13) {
    full.simulation(designs = array(c(8,9)), seed = 1, is.honest = TRUE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_500_1000", output.filename = "extra/results/obs_500/four_honest_repl.csv")
  } else if (param == 14) {
    full.simulation(designs = array(c(8,9)), seed = 1, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_500_1000", output.filename = "extra/results/obs_500/four_dishonest_repl.csv")
  } else if (param == 15) {
    full.simulation(seed = 1, is.honest = TRUE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, data.path = "../data_500_1000", output.filename = "extra/results/obs_500/all_honest0.5_repl.csv")
  } else if (param == 16) {
    full.simulation(seed = 1, is.honest = FALSE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, data.path = "../data_500_1000", output.filename = "extra/results/obs_500/all_dishonest0.5_repl.csv")
  } else if (param == 17) {
    full.simulation(seed = 1, is.honest = TRUE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_500_1000", output.filename = "extra/results/obs_500/four_honest0.5_repl.csv")
  } else if (param == 18) {
    full.simulation(seed = 1, is.honest = FALSE, is.honest0.5 = TRUE, is.dishonest2 = FALSE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_500_1000", output.filename = "extra/results/obs_500/four_dishonest0.5_repl.csv")
  } else if (param == 19) {
    full.simulation(designs = array(c(8,9)), seed = 1, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = TRUE, data.path = "../data_500_1000", output.filename = "extra/results/obs_500/all_dishonest2_repl.csv")
  } else if (param == 20) {
    full.simulation(designs = array(c(8,9)), seed = 1, is.honest = FALSE, is.honest0.5 = FALSE, is.dishonest2 = TRUE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"),  data.path = "../data_500_1000", output.filename = "extra/results/obs_500/four_dishonest2_repl.csv")
  }
}