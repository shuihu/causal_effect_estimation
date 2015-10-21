get.design.label <- function(design) {
  if (design == 1) {
    "Design I"
  } else if (design == 2) {
    "Design II"
  } else if (design == 3) {
    "Design III"
  } else if (design == 4) {
    "Design IV" 
  } else if (design == 5) {
    "Design V" 
  } else if (design == 6) {
    "Design VI" 
  } else {
    stop("Invalid design")
  }
}

get.criterion.label <- function(criterion) {
  if (criterion == "conf.intervals.true") {
    "Confidence Interval Check"
  } else if (criterion == "conf.intervals.between.trees") {
    "Honest-Dishonest Confidence Intervals"
  } else if (criterion == "conf.intervals.for.test.data") {
    "Test Data in Confidence Intervals"
  } else if (criterion == "weighted.conf.intervals.for.test.data") {
    "Weighted Test Data in Confidence Intervals"
  } else if (criterion == "full.trees") {
    "Full Trees"
  } else if (criterion == "leaves") {
    "Leaves"
  } else if (criterion == "os.to") {
    "TO criterion"
  } else if (criterion == "os.m") {
    "Matching criterion"
  } else if (criterion == "os.infeasible") {
    "Infeasible (oracle) criterion"
  } else {
    stop("Invalid criterion")
  }
}

get.stats.label <- function(criterion, stats) {
  if (criterion == "leaves") {
    stats
  } else if (criterion == "conf.intervals.true") {
    if (stats == "dishonest.in.dishonest.conf.intv.95") {
      "Dishonest in Dishonest 95%"
    } else if (stats == "dishonest.in.dishonest.conf.intv.90") {
      "Dishonest in Dishonest 90%"
    } else if (stats == "honest.in.honest.conf.intv.95") {
      "Honest in Honest 95%"
    } else if (stats == "honest.in.honest.conf.intv.90") {
      "Honest in Honest 90%"
    } else {
      stop("Invalid stats")
    }
  } else if (criterion == "conf.intervals.between.trees") {
    if (stats == "honest.in.dishonest.conf.intv.95") {
      "Honest in Dishonest 95%"
    } else if (stats == "honest.in.dishonest.conf.intv.90") {
      "Honest in Dishonest 90%"
    } else if (stats == "dishonest.in.honest.conf.intv.95") {
      "Dishonest in Honest 95%"
    } else if (stats == "dishonest.in.honest.conf.intv.90") {
      "Dishonest in Honest 90%"
    } else {
      stop("Invalid stats")
    }
  } else if (criterion == "conf.intervals.for.test.data") {
    if (stats == "test.in.dishonest.conf.intv.95") {
      "Test in Dishonest 95%"
    } else if (stats == "test.in.dishonest.conf.intv.90") {
      "Test in Dishonest 90%"
    } else if (stats == "test.in.honest.conf.intv.95") {
      "Test in Honest 95%"
    } else if (stats == "test.in.honest.conf.intv.90") {
      "Test in Honest 90%"
    } else {
      stop("Invalid stats")
    }
  } else if (criterion == "weighted.conf.intervals.for.test.data") {
    if (stats == "weighted.test.in.dishonest.conf.intv.95") {
      "Test in Dishonest 95%"
    } else if (stats == "weighted.test.in.dishonest.conf.intv.90") {
      "Test in Dishonest 90%"
    } else if (stats == "weighted.test.in.honest.conf.intv.95") {
      "Test in Honest 95%"
    } else if (stats == "weighted.test.in.honest.conf.intv.90") {
      "Test in Honest 90%"
    } else {
      stop("Invalid stats")
    }
  } else if (criterion == "full.trees") {
    if (stats == "Proportion") {
      "Proportion"
    } else {
      stop("Invalid stats")
    }
  } else if (criterion == "os.to" || criterion == "os.m" || criterion == "os.infeasible") {
    if (stats == "Q.Mean") {
      "-Q Mean"
    } else if (stats == "Q.Median") {
      "-Q Median"
    } else if (stats == "Q") {
      "-Q Median"            #original paper only had Q Median
    } else if (stats == "Q.Std") {
      "-Q Std"
    } else if (stats == "Q.Spread") {
      "-Q Spread"
    } else if (stats == "Share") {
      "Share"
    } else {
      stop("Invalid stats")
    }
  } else {
    stop("Invalid criterion")
  }
}

get.model.label <- function(model.name) {
  if (model.name == "ST") {
    "ST"
  } else if (model.name == "TT") {
    "TT"
  } else if (model.name == "TOT_split_xval_rpart") {
    "TOT (using rpart)"
  } else if (model.name == "TOT_xval") {
    'CT with "TOT" xval'
  } else if (model.name == "CT") {
    'CT with "matching" xval'
  } else {
    stop("Invalid model name")
  }
}