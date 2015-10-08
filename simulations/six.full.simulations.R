source("simulations/full.simulation.R")

six.full.simulations <- function(param) {
  if (param == 1) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, data.path = "../data_10000", output.filename = "extra/results/obs_10000/all_honest_1000_repl.csv")
  } else if (param == 2) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, data.path = "../data_10000", output.filename = "extra/results/obs_10000/all_dishonest_1000_repl.csv")
  } else if (param == 3) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000", output.filename = "extra/results/obs_10000/four_honest_1000_repl.csv")
  } else if (param == 4) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), is.honest = FALSE, data.path = "../data_10000", output.filename = "extra/results/obs_10000/four_dishonest_1000_repl.csv")
  } else if (param == 5) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = TRUE, is.honest0.5 = TRUE, data.path = "../data_10000", output.filename = "extra/results/obs_10000/all_honest2_1000_repl.csv")
  } else if (param == 6) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = TRUE, data.path = "../data_10000", output.filename = "extra/results/obs_10000/all_dishonest2_1000_repl.csv")
  } else if (param == 7) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = TRUE, is.honest0.5 = TRUE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000", output.filename = "extra/results/obs_10000/four_honest2_1000_repl.csv")
  } else if (param == 8) {
    full.simulation(seed = 1, num.replications = 1000, num.obs.per.set = 10000, is.honest = FALSE, is.honest0.5 = TRUE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), data.path = "../data_10000", output.filename = "extra/results/obs_10000/four_dishonest2_1000_repl.csv")
  } else if (param == 9) {
    full.simulation(seed = 1, output.filename = "extra/results/obs_500/all_honest_repl.csv")
  } else if (param == 10) {
    full.simulation(seed = 1, is.honest = FALSE, output.filename = "extra/results/obs_500/all_dishonest_repl.csv")
  } else if (param == 11) {
    full.simulation(seed = 1, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), output.filename = "extra/results/obs_500/four_honest_repl.csv")
  } else if (param == 12) {
    full.simulation(seed = 1, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), is.honest = FALSE, output.filename = "extra/results/obs_500/four_dishonest_repl.csv")
  } else if (param == 13) {
    full.simulation(seed = 1, is.honest = TRUE, is.honest0.5 = TRUE, output.filename = "extra/results/obs_500/all_honest2_repl.csv")
  } else if (param == 14) {
    full.simulation(seed = 1, is.honest = FALSE, is.honest0.5 = TRUE, output.filename = "extra/results/obs_500/all_dishonest2_repl.csv")
  } else if (param == 15) {
    full.simulation(seed = 1, is.honest = TRUE, is.honest0.5 = TRUE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), output.filename = "extra/results/obs_500/four_honest2_repl.csv")
  } else if (param == 16) {
    full.simulation(seed = 1, is.honest = FALSE, is.honest0.5 = TRUE, model.names = c("ST", "TT", "TOT_split_xval_rpart", "CT"), output.filename = "extra/results/obs_500/four_dishonest2_repl.csv")
  }
}