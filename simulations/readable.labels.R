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
  if (criterion == "conf.intervals") {
    "Confidence Intervals"
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
  if (criterion == "leaves" || criterion == "conf.intervals") {
    stats
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