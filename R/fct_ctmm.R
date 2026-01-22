
# This file includes hidden functions from the "ctmm" package
# that have been imported into this package in case they change
# in future versions.

DOP.LIST <- list(
  unknown = list(
    axes = NA,
    geo = NA,
    DOP = NA,
    VAR = NA,
    COV = NA,
    COV.geo = NA,
    units = NA
  ),
  horizontal = list(
    axes = c("x", "y"),
    geo = c("longitude", "latitude"),
    DOP = "HDOP",
    VAR = "VAR.xy",
    COV = c("COV.x.x", "COV.x.y", "COV.y.y"),
    COV.geo = c("COV.major", "COV.minor", "COV.angle"),
    units = "distance"
  ),
  vertical = list(
    axes = "z",
    geo = "z",
    DOP = "VDOP",
    VAR = "VAR.z",
    COV = NA,
    COV.geo = NA,
    units = "distance"
  ),
  speed = list(
    axes = c("vx", "vy"),
    geo = c("speed", "heading"),
    DOP = "SDOP",
    VAR = "VAR.v",
    COV = c("COV.vx.vx", "COV.vx.vy", "COV.vy.vy"),
    COV.geo = NA,
    units = "speed"
  ),
  frequency = list(
    axes = 'f',
    geo = NA,
    DOP = NA,
    VAR = NA,
    COV = NA,
    COV.geo = NA,
    units = 'frequency'
  ),
  mass = list(
    axes = 'm',
    geo = NA,
    DOP = NA,
    VAR = NA,
    COV = NA,
    COV.geo = NA,
    units = 'mass'
  )
)


DOP.match <- function (axes) {
  DOP.LIST <- DOP.LIST[-1]
  NAMES <- names(DOP.LIST)
  for (i in seq_along(DOP.LIST)) {
    if (all(axes == DOP.LIST[[i]]$axes)) return(NAMES[i])
  }
  return("unknown")
}


outer <- function (X, Y = X, FUN = "*", ...) {
  base::outer(X, Y, FUN = FUN, ...)
}


get.telemetry <- function (data, axes = c("x", "y")) {
  if (all(axes %in% names(data))) {
    data <- as.matrix(data.frame(data)[, axes], dimnames = axes)
  } else data <- numeric(0)
  return(data)
}


get.class.mat <- function (data, LEVELS = levels(data$class)) {
  if ("class" %in% names(data)) {
    C <- sapply(LEVELS, function(lc) {
      data$class == lc
    })
    dim(C) <- c(nrow(data), length(LEVELS))
    colnames(C) <- LEVELS
  }
  else {
    C <- cbind(rep(1, length(data$t)))
  }
  return(C)
}


continuity <- function (CTMM) {
  K <- sum(CTMM$tau > 0)
  return(K)
}


nant <- function(x,to) {
  NAN <- is.na(x) # TRUE for NaN and NA
  if (any(NAN)) {
    to <- array(to,length(x))
    x[NAN] <- to[NAN]
  }
  return(x)
}


get.error <- function(data, 
                      CTMM,
                      circle = FALSE,
                      DIM = FALSE, 
                      calibrate = TRUE) {
  n <- nrow(data)
  axes <- CTMM$axes
  COLS <- names(data)
  ELLIPSE <- FALSE
  error <- rep(0, n)
  if (any(CTMM$error > 0)) {
    TYPE <- DOP.match(axes)
    UERE.DOF <- attr(data, "UERE")$DOF[, TYPE]
    names(UERE.DOF) <- rownames(attr(data, "UERE")$DOF)
    ERROR <- rep(FALSE, length(UERE.DOF))
    names(ERROR) <- names(UERE.DOF)
    ERROR[names(CTMM$error)] <- CTMM$error
    UERE.FIT <- ERROR & !is.na(UERE.DOF) & UERE.DOF < Inf
    UERE.FIX <- ERROR & (is.na(UERE.DOF) | UERE.DOF == Inf)
    UERE.PAR <- names(UERE.FIT)[UERE.FIT > 0]
    if (any(!UERE.FIT)) {
      ERROR[!UERE.FIT] <- as.logical(ERROR[!UERE.FIT])
    }
    if ("class" %in% names(data)) {
      LEVELS <- levels(data$class)
      ERROR <- ERROR[LEVELS]
      UERE.DOF <- UERE.DOF[LEVELS]
      UERE.FIT <- UERE.FIT[LEVELS]
      UERE.FIX <- UERE.FIX[LEVELS]
      UERE.PAR <- UERE.PAR[UERE.PAR %in% LEVELS]
    }
    CLASS <- get.class.mat(data)
    FIT <- as.logical(c(CLASS %*% UERE.FIT))
    TYPE <- DOP.LIST[[TYPE]]
    AXES <- TYPE$axes
    COV <- TYPE$COV
    VAR <- TYPE$VAR
    DOP <- TYPE$DOP
    ELLIPSE <- all(COV %in% COLS)
    CIRCLE <- VAR %in% COLS
    if (ELLIPSE) {
      ellipse <- get.telemetry(data, COV[c(1, 2, 2, 3)])
      dim(ellipse) <- c(nrow(ellipse), 2, 2)
      if (circle) {
        error <- (ellipse[, 1, 1] + ellipse[, 2, 2])/2
        ELLIPSE <- FALSE
      }
    }
    else if (CIRCLE) {
      error <- data[[VAR]]
    }
    if (any(UERE.FIT)) {
      if (calibrate) {
        CLASS <- c(CLASS %*% ERROR)
      }
      else {
        CLASS <- c(CLASS %*% as.logical(ERROR))
      }
      if (DOP %in% COLS) {
        CLASS <- CLASS * data[[DOP]]
      }
      CLASS <- CLASS^2/length(axes)
      error[FIT] <- CLASS[FIT]
    }
    if (ELLIPSE) {
      if (any(UERE.FIT)) {
        ellipse[FIT, 1, 1] <- ellipse[FIT, 2, 2] <- error[FIT]
        ellipse[FIT, 1, 2] <- ellipse[FIT, 2, 1] <- 0
      }
      error <- ellipse
    }
  }
  if (!ELLIPSE && !circle && DIM) {
    error <- outer(error, diag(DIM))
  }
  attr(error, "ellipse") <- ELLIPSE
  return(error)
}


norm.ci <- function(MLE, VAR, level = 0.95, alpha = 1 - level) {
  NAMES.CI <- c("low","est","high")
  
  z <- stats::qnorm(1 - alpha/2) * c(-1, 0, 1)
  CI <- MLE + z * sqrt(VAR)
  names(CI) <- NAMES.CI
  return(CI)
}


axes2var <- function (CTMM, MEAN = TRUE) {
  PAR <- c("major", "minor", "angle")
  COV <- CTMM$COV
  if (any(c("minor", "angle") %!in% rownames(COV))) {
    NAMES <- rownames(COV)
    NAMES[NAMES == "major"] <- "variance"
    dimnames(COV) <- list(NAMES, NAMES)
    return(COV)
  }
  OLD <- rownames(COV)
  OTHER <- OLD[OLD %!in% PAR]
  if (CTMM$isotropic[1]) {
    NEW <- c("variance", OTHER)
    if (!MEAN) {
      COV["major", ] <- 2 * COV["major", ]
      COV[, "major"] <- 2 * COV[, "major"]
    }
  }
  else {
    NEW <- c("variance", OTHER)
    grad <- matrix(0, length(NEW), length(OLD))
    rownames(grad) <- NEW
    colnames(grad) <- OLD
    if (length(OTHER) == 1) {
      grad[OTHER, OTHER] <- 1
    }
    else if (length(OTHER) > 1) {
      diag(grad[OTHER, OTHER]) <- 1
    }
    grad["variance", "major"] <- grad["variance", "minor"] <- 1
    if (MEAN) {
      grad <- grad/2
    }
    COV <- grad %*% COV %*% t(grad)
    for (i in 1:nrow(COV)) {
      if (any(is.nan(COV[i, ]) | is.nan(COV[, i]))) {
        COV[i, ] <- COV[, i] <- 0
        COV[i, i] <- Inf
      }
    }
  }
  dimnames(COV) <- list(NEW, NEW)
  return(COV)
}


get.states <- function (CTMM) {
  dynamics <- CTMM$dynamics
  if (is.null(dynamics) || dynamics == "stationary") {
    states <- NULL
  }
  else if (dynamics == "change.point") {
    states <- levels(CTMM[[dynamics]]$state)
  }
  return(states)
}


get.taus <- function(CTMM, zeroes = FALSE, simplify = FALSE) {
  
  STATES <- get.states(CTMM)
  for (S in STATES) {
    CTMM[[S]] <- get.taus(CTMM, zeroes = zeroes, simplify = simplify)
  }
  CTMM$tau <- sort(CTMM$tau, decreasing = TRUE)
  if (simplify) {
    CTMM$tau <- CTMM$tau[CTMM$tau > 0]
  }
  K <- if (zeroes) {
    length(CTMM$tau)
  }
  else {
    continuity(CTMM)
  }
  CTMM$K <- K
  if (!simplify) {
    VARS <- dimnames(CTMM$COV)[[1]]
  }
  else {
    VARS <- NULL
  }
  if (K == 1 && CTMM$tau[1] < Inf) {
    CTMM$tau.names <- "tau position"
    CTMM$TAU <- CTMM$tau[1]
  }
  else if (K > 1 && CTMM$tau[1] == Inf) {
    CTMM$tau.names <- "tau velocity"
    CTMM$TAU <- CTMM$tau[2]
    CTMM$Omega2 <- 1/CTMM$tau[2]
    CTMM$J.Omega2 <- -1/CTMM$tau[2]^2
    CTMM$f <- 1/CTMM$tau
    CTMM$f.nu <- c(mean(CTMM$f), +diff(CTMM$f)/2)
    CTMM$TfOmega2 <- 2 * CTMM$f.nu[1]/CTMM$Omega2
  }
  else if (K > 1 && all(c("tau position", "tau velocity", 
                          "omega") %in% VARS)) {
    CTMM$tau.names <- c("tau position", "tau velocity", 
                        "omega")
    CTMM$f <- 1/CTMM$tau
    CTMM$f.nu <- c(CTMM$f, CTMM$omega)
    CTMM$Omega2 <- prod(CTMM$f) + CTMM$omega^2
    CTMM$TAU <- c(1, 1, 2 * pi)/CTMM$f.nu
    CTMM$J.nu.tau <- diag(c(-CTMM$f^2, 1))
    CTMM$J.TAU.tau <- diag(c(1, 1, -2 * pi/CTMM$omega^2))
    CTMM$J.Omega2 <- c(CTMM$f[2:1], 2 * CTMM$omega) %*% 
      CTMM$J.nu.tau
  }
  else if (K > 1 && (CTMM$tau[1] > CTMM$tau[2] || all(
    c("tau position", "tau velocity") %in% VARS))) {
    CTMM$tau.names <- c("tau position", "tau velocity")
    CTMM$TAU <- CTMM$tau
    CTMM$f <- 1/CTMM$tau
    CTMM$f.nu <- c(mean(CTMM$f), +diff(CTMM$f)/2)
    CTMM$Omega2 <- prod(CTMM$f)
    CTMM$TfOmega2 <- 2 * CTMM$f.nu[1]/CTMM$Omega2
    CTMM$J.f.tau <- -diag(CTMM$f^2)
    CTMM$J.tau.f <- -diag(CTMM$tau^2)
    CTMM$J.nu.tau <- rbind(-c(1, 1) * CTMM$f^2/2, -c(-1, +1) * CTMM$f^2/2)
    CTMM$J.Omega2 <- -CTMM$Omega2/CTMM$tau
  }
  else if (K > 1 && CTMM$tau[1] == CTMM$tau[2] && !CTMM$omega && 
           "omega" %!in% VARS) {
    CTMM$tau.names <- c("tau")
    CTMM$TAU <- CTMM$tau[1]
    CTMM$f <- 1/CTMM$tau
    CTMM$f.nu <- c(CTMM$f[1], 0)
    CTMM$Omega2 <- prod(CTMM$f)
    CTMM$TfOmega2 <- 2 * CTMM$f.nu[1]/CTMM$Omega2
    CTMM$J.tau.f <- -CTMM$tau[1]^2
    CTMM$J.f.tau <- -CTMM$f^2
    CTMM$J.Omega2 <- -2/CTMM$tau[1]^3
  }
  else if (K > 1 && (CTMM$omega || "omega" %in% VARS)) {
    CTMM$tau.names <- c("tau", "omega")
    CTMM$f <- 1/CTMM$tau
    CTMM$f.nu <- c(mean(CTMM$f), CTMM$omega)
    CTMM$Omega2 <- sum(CTMM$f.nu^2)
    CTMM$TfOmega2 <- 2 * CTMM$f.nu[1]/CTMM$Omega2
    CTMM$TAU <- c(1, 2 * pi)/CTMM$f.nu
    CTMM$J.nu.tau <- diag(c(-CTMM$f[1]^2, 1))
    CTMM$J.TAU.tau <- diag(c(1, -2 * pi/CTMM$omega^2))
    CTMM$J.Omega2 <- 2 * CTMM$f.nu %*% CTMM$J.nu.tau
  }
  else {
    CTMM$tau.names <- NULL
  }
  return(CTMM)
}


svf.func <- function (CTMM, moment = FALSE) {
  CTMM <- get.taus(CTMM)
  tau <- CTMM$tau
  sigma <- var.covm(CTMM$sigma, average = TRUE)
  circle <- CTMM$circle
  COV <- CTMM[["COV"]]
  if (!is.null(COV)) {
    COV <- axes2var(CTMM, MEAN = TRUE)
  }
  range <- CTMM$range
  tau <- tau[tau < Inf]
  if (any(tau == 0)) {
    DEL <- paste("tau", names(tau[tau == 0]))
    if (!is.null(COV)) {
      COV <- rm.name(COV, DEL)
    }
    tau <- tau[tau > 0]
  }
  K <- length(tau)
  NAMES <- CTMM$tau.names
  if (K == 0 && range) {
    acf <- function(t) {
      if (t == 0) {
        1
      }
      else {
        0
      }
    }
    acf.grad <- function(t) {
      NULL
    }
  }
  else if (K == 0) {
    acf <- function(t) {
      1 - t
    }
    acf.grad <- function(t) {
      NULL
    }
  }
  else if (K == 1 && range) {
    acf <- function(t) {
      exp(-t/tau)
    }
    acf.grad <- function(t) {
      t/tau^2 * acf(t)
    }
  }
  else if (K == 1) {
    acf <- function(t) {
      1 - (t - tau * (1 - exp(-t/tau)))
    }
    acf.grad <- function(t) {
      1 - (1 + t/tau) * exp(-t/tau)
    }
  }
  else if (K == 2) {
    if (tau[1] > tau[2]) {
      acf <- function(t) {
        diff(tau * exp(-t/tau))/diff(tau)
      }
      acf.grad <- function(t) {
        c(1, -1) * ((1 + t/tau) * exp(-t/tau) - acf(t))/diff(tau)
      }
    }
    else if (!CTMM$omega) {
      tau <- tau[1]
      acf <- function(t) {
        (1 + t/tau) * exp(-t/tau)
      }
      acf.grad <- function(t) {
        (t^2/tau^3) * exp(-t/tau)
      }
    }
    else if (CTMM$omega) {
      f <- CTMM$f.nu[1]
      nu <- CTMM$f.nu[2]
      acf <- function(t) {
        (cos(nu * t) + (f/nu) * sin(nu * t)) * exp(-f * 
                                                     t)
      }
      acf.grad <- function(t) {
        c(exp(-f * t) * c(-t * cos(nu * t) + (1 - f * 
                                                t)/nu * sin(nu * t),
                          -(t + f/nu^2) * sin(nu * t) + 
                            (f/nu) * t * cos(nu * t)) %*% CTMM$J.nu.tau)
      }
    }
  }
  if (!circle) {
    ACF <- function(t) {
      acf(t)
    }
    svf <- function(t) {
      sigma * (1 - acf(t))
    }
    grad <- function(t, ...) {
      c(svf(t)/sigma, -sigma * acf.grad(t))
    }
  }
  else {
    NAMES <- c(NAMES, "circle")
    ACF <- function(t) {
      cos(circle * t) * acf(t)
    }
    svf <- function(t) {
      sigma * (1 - cos(circle * t) * acf(t))
    }
    grad <- function(t, ...) {
      c(svf(t)/sigma, -sigma * cos(circle * t) * acf.grad(t), 
        +sigma * t * sin(circle * t) * acf(t))
    }
  }
  NAMES <- c("variance", NAMES)
  
  drift.fn <- function(fn,CTMM,...) {
    F1 <- paste0(CTMM$mean,".",fn) # preferred function
    F2 <- paste0("stationary.",fn) # default function
    
    if(exists(F1,mode="function")) { 
      fn <- get(F1,mode="function") 
    } else { fn <- get(F2,mode="function") }
    
    fn(CTMM,...)
  }
  
  drift.svf <- function(CTMM,...) { drift.fn("svf",CTMM,...) }
  stationary.svf <- function(CTMM,...) {
    EST <- function(t) { 0 }
    VAR <- function(t) { 0 }
    return(list(EST=EST,VAR=VAR))
  }
  
  if (moment) { MEAN <- drift.svf(CTMM) 
  } else { MEAN <- stationary.svf(CTMM) }
  
  SVF <- function(t) { svf(t) + MEAN$EST(t) }
  
  if (is.null(COV)) {
    COV <- diag(0, nrow = length(grad(0)))
  }
  BLANK <- array(0, length(NAMES) * c(1, 1))
  dimnames(BLANK) <- list(NAMES, NAMES)
  NAMES <- NAMES[NAMES %in% dimnames(COV)[[1]]]
  if (length(NAMES)) {
    BLANK[NAMES, NAMES] <- COV[NAMES, NAMES]
  }
  COV <- BLANK
  VAR <- function(t) {
    g <- grad(t)
    return(c(g %*% COV %*% g) + MEAN$VAR(t))
  }
  DOF <- function(t) {
    return(2 * SVF(t)^2/VAR(t))
  }
  return(list(svf = SVF, VAR = VAR, DOF = DOF, ACF = ACF))
}


time_res <- function (DT) {
  
  gcd <- function (x, y) {
    r <- x%%y
    return(ifelse(r, gcd(y, r), y))
  }
  
  gcd.vec <- function (vec) {
    vec <- sort(vec, method = "quick")
    GCD <- gcd(vec[1], vec[2])
    for (i in vec[-(1:2)]) {
      GCD <- gcd(i, GCD)
    }
    return(GCD)
  }
  
  dt <- DT[DT > 0]
  if (length(dt) == 0) {
    return(c(1, 1))
  }
  if (length(dt) == 1) {
    return(c(dt, min(1, dt/2)))
  }
  M <- -log10(min(dt))
  M <- ceiling(M)
  M <- max(0, M)
  M <- 10^M
  dt <- round(M * dt)
  dt <- gcd.vec(dt)/M
  DT <- DT == 0
  if (any(DT)) {
    for (i in 2:length(DT)) {
      if (DT[i]) {
        DT[i] <- DT[i] + DT[i - 1]
      }
    }
    DT <- max(DT)
    DT <- dt/(1 + DT)
  }
  else {
    DT <- 0
  }
  return(c(dt, DT))
}


assign_speeds <- function(data, 
                          DT = NULL,
                          UERE = 0,
                          method = "max", 
                          axes = c("x", "y")) {
  method <- match.arg(method, c("max", "min"))
  if (is.null(DT)) {
    DT <- diff(data$t)
    dt <- time_res(DT)
  }
  v.dt <- speedMLE(data, UERE = UERE, DT = DT, axes = axes)
  VAR.dt <- v.dt$VAR
  v.dt <- v.dt$X
  if (length(v.dt) == 1) {
    v <- c(v.dt, v.dt)
    VAR <- c(VAR.dt, VAR.dt)
    return(list(v.t = v, VAR.t = VAR, v.dt = v.dt, VAR.dt = VAR.dt))
  }
  N <- length(data$t)
  if (length(UERE) == 1) {
    v1 <- speedMLE(data[c(1, 3), ], DT = sum(DT[1:2]), UERE = UERE)
    v2 <- speedMLE(data[c(N - 2, N), ], DT = sum(
      DT[(N - 1):N]), UERE = UERE)
  }
  else if (length(dim(UERE)) == 3) {
    v1 <- speedMLE(data[c(1, 3), ], DT = sum(DT[1:2]), 
                   UERE = UERE[c(1, 3), , ])
    v2 <- speedMLE(data[c(N - 2, N), ], DT = sum(
      DT[(N - 2):(N - 1)]), UERE = UERE[c(N - 2, N), , ])
  }
  else {
    v1 <- speedMLE(data[c(1, 3), ], DT = sum(DT[1:2]),
                   UERE = UERE[c(1, 3)])
    v2 <- speedMLE(data[c(N - 2, N), ], DT = sum(
      DT[(N - 2):(N - 1)]), UERE = UERE[c(N - 2, N)])
  }
  VAR1 <- v1$VAR
  v1 <- v1$X
  VAR2 <- v2$VAR
  v2 <- v2$X
  v1 <- c(v1, v.dt)
  v2 <- c(v.dt, v2)
  VAR1 <- c(VAR1, VAR.dt)
  VAR2 <- c(VAR.dt, VAR2)
  if (method == "max") {
    v1 <- v1[-N]
    v2 <- v2[-1]
    LESS <- v1 < v2
    vs <- sort(v.dt, index.return = TRUE, decreasing = TRUE)
    is <- vs$ix
    vs <- vs$x
    v <- numeric(length(LESS))
    VAR <- numeric(length(LESS))
    LESS <- LESS[is]
    v[is + !LESS] <- vs
    VAR[is + !LESS] <- VAR.dt[is]
    LESS <- rev(LESS)
    is <- rev(is)
    vs <- rev(vs)
    v[is + LESS] <- vs
    VAR[is + LESS] <- VAR.dt[is]
    rm(is, vs, LESS)
  }
  else if (method == "min") {
    v <- cbind(v1, v2)
    is <- apply(v, 1, which.min)
    v <- vapply(1:nrow(v), function(i) {
      v[i, is[i]]
    }, v[, 1])
    VAR <- vapply(1:nrow(VAR), function(i) {
      VAR[i, is[i]]
    }, v)
  }
  return(list(v.t = v, VAR.t = VAR, v.dt = v.dt, VAR.dt = VAR.dt))
}


speedMLE <- function(data,
                     DT = NULL,
                     UERE = 0,
                     axes = c("x", "y"),
                     CTMM = ctmm::ctmm(error = UERE, axes = axes)) {
  AXES <- length(axes)
  if (is.null(DT)) {
    DT <- diff(data$t)
    dt <- time_res(DT)
  }
  f <- 1/DT
  if (length(UERE) == 1) {
    error <- get.error(data, ctmm::ctmm(error = UERE, axes = axes))
  }
  else {
    error <- UERE
  }
  if (AXES == 2) {
    dx <- diff(data$x)
    dy <- diff(data$y)
    if (length(dim(error)) == 3) {
      dr <- cbind(dx, dy)
    }
    else {
      dr <- sqrt(dx^2 + dy^2)
    }
    rm(dx, dy)
  }
  else if (AXES == 1) {
    dr <- diff(data$z)
    dr <- abs(dr)
  }
  if (length(UERE) > 1 || UERE) {
    if (length(dim(error)) == 3) {
      error <- error[-1, , , drop = FALSE] + error[-nrow(error), 
                                                   , , drop = FALSE]
    }
    else {
      error <- error[-1] + error[-length(error)]
    }
    DR <- distanceMLE(dr, error, axes = axes, return.VAR = TRUE)
    VAR <- DR[, 2]
    DR <- DR[, 1]
  }
  else {
    DR <- dr
    VAR <- numeric(length(dr))
  }
  RETURN <- data.frame(X = DR * f, VAR = VAR * f^2)
  return(RETURN)
}


abs_bivar <- function (mu, Sigma, return.VAR = FALSE) {
  BESSEL_LIMIT <- 2^16
  
  sigma0 <- mean(diag(Sigma))
  stdev0 <- sqrt(sigma0)
  mu2 <- sum(mu^2)
  mu <- sqrt(mu2)
  sigma <- eigen(Sigma)$values
  Barg <- mu2/(4 * sigma0)
  if (sigma0 == 0 || Barg >= BESSEL_LIMIT) {
    M1 <- mu
  }
  else {
    B0 <- besselI(Barg, 0, expon.scaled = TRUE)
    B1 <- besselI(Barg, 1, expon.scaled = TRUE)
    sqrtpi2 <- sqrt(pi/2)
    Bv <- sqrtpi2 * sqrt(Barg) * (B0 + B1) * mu
    Bs <- B0/sqrtpi2 * sqrt(sigma[1]) * ellipke(
      1 - sigma[2]/sigma[1])$e
    M1 <- Bv + Bs
  }
  if (return.VAR) {
    M2 <- mu2 + 2 * sigma0
    VAR <- max(0, M2 - M1^2)
    M1 <- c(M1, VAR)
  }
  return(M1)
}


rm.name <- function (object, name) {
  if (length(dim(object)) == 2) {
    object <- object[!rownames(object) %in% name, !colnames(object) %in% 
                       name, drop = FALSE]
  }
  else {
    object <- object[!names(object) %in% name]
  }
  return(object)
}


distanceMLE <- function(dr,
                        error, 
                        axes = c("x", "y"), 
                        return.VAR = FALSE) {
  BESSEL_LIMIT <- 2^16
  
  BesselSolver <- function (y) {
    x <- numeric(length(y))
    SUB2 <- (2 < y) & (y < 3.6)
    if (any(SUB2)) {
      x.SUB <- sqrt(2 * y[SUB2])
      x[SUB2] <- 4 * sqrt((x.SUB - 2)/(4 - x.SUB))
    }
    SUB3 <- (3.6 <= y) & (y <= BESSEL_LIMIT)
    if (any(SUB3)) {
      y.SUB <- y[SUB3]
      BI0 <- besselI(y.SUB, 0, expon.scaled = TRUE)
      BI1 <- besselI(y.SUB, 1, expon.scaled = TRUE)
      x[SUB3] <- 2 * y.SUB * (y.SUB * BI0 - (y.SUB + 1) * 
                                BI1)/((2 * y.SUB - 1) * 
                                        BI0 - (2 * y.SUB + 1) * BI1)
    }
    SUB4 <- BESSEL_LIMIT < y
    if (any(SUB4)) {
      y.SUB <- y[SUB4]
      x[SUB4] <- 2 * y.SUB * (y.SUB - 1)/(2 * y.SUB - 1)
    }
    SUB <- SUB2 & SUB3
    if (any(SUB)) {
      x.SUB <- x[SUB]
      y.SUB <- y[SUB]
      BI0 <- besselI(x.SUB, 0, expon.scaled = TRUE)
      BI1 <- besselI(x.SUB, 1, expon.scaled = TRUE)
      x[SUB] <- x.SUB * (x.SUB * (x.SUB + y.SUB) * 
                           BI0 - (x.SUB^2 + (x.SUB + 2) * y.SUB) *
                           BI1)/(x.SUB * (x.SUB + y.SUB - 1) *
                                   BI0 - (x.SUB^2 + (x.SUB + 1) *
                                            y.SUB) * BI1)
    }
    return(x)
  }
  
  TanhSolver <- function (y) {
    x <- y
    SUB <- y < 1
    if (any(SUB)) {
      x[SUB] <- 0
    }
    SUB <- 1 < y & y <= 1.11066772414266
    if (any(SUB)) {
      x[SUB] <- sqrt((5 * y[SUB] - sqrt(120 - 95 * y[SUB]^2))/y[SUB]^3)/2
    }
    SUB <- 1.11066772414266 <= y & y < Inf
    if (any(SUB)) {
      TEMP <- 2 * y[SUB]^2
      x[SUB] <- nant((sinh(TEMP) - TEMP)/(cosh(TEMP) - TEMP + 
                                            1), 1) * y[SUB]
    }
    SUB <- 1 < y & y < Inf
    if (any(SUB)) {
      TANH <- tanh(x[SUB] * y[SUB])
      GRAD <- (1 - TANH^2) * y[SUB]
      dx <- (y[SUB] * TANH - x[SUB])/(1 - y[SUB] * GRAD)
      x[SUB] <- x[SUB] + dx
    }
    return(x)
  }
  
  if (length(dim(error)) == 3) {
    dr <- sapply(1:nrow(dr), function(i) {
      abs_bivar(dr[i, ], error[i, , ], return.VAR = TRUE)
    })
    dr <- t(dr)
  }
  else {
    AXES <- length(axes)
    DR <- dr
    SUB <- dr > 0 & error > 0
    if (any(SUB)) {
      if (AXES == 2) {
        y <- DR[SUB]^2/error[SUB]
        x <- BesselSolver(y)
        DR[SUB] <- ifelse(error[SUB] < Inf, error[SUB]/DR[SUB] * 
                            x, 0)
      }
      else if (AXES == 1) {
        error[SUB] <- sqrt(error[SUB])
        y <- DR[SUB]/error[SUB]
        x <- TanhSolver(y)
        DR[SUB] <- ifelse(error[SUB] < Inf, error[SUB] * 
                            x, 0)
      }
    }
    VAR <- error/(AXES - (dr^2 - DR^2)/error)
    dr <- cbind(DR, VAR)
  }
  if (!return.VAR) {
    dr <- dr[, 1]
  }
  return(dr)
}
