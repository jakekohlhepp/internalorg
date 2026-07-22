#' =============================================================================
#' MODEL CONFIGURATION
#' =============================================================================
#' This configuration object controls the estimation specification.
#' Modify these values to adapt the model to different settings.
#' =============================================================================

CONFIG <- list(

  # ---------------------------------------------------------------------------
  # Logging and execution control
  # ---------------------------------------------------------------------------
  # Directory for log files (absolute path so subscripts that setwd() still
  # write into the same logs/ folder)
  log_dir = normalizePath("logs", mustWork = FALSE),

  # Set TRUE to force re-run all scripts regardless of timestamps
  force_rerun = FALSE,

  # Print log messages to console
  verbose_logging = TRUE,

  # ---------------------------------------------------------------------------
  # Data paths
  # ---------------------------------------------------------------------------
  # Path to raw transaction data.
  # Override with env var raw_data_base for machine-specific locations.
  raw_data_path = Sys.getenv("JMP_RAW_DATA_PATH", unset = "mkdata/raw"),
  raw_data_base = Sys.getenv("raw_data_base"),
  prep_output_dir = "mkdata/data",
  counterfactual_data_dir = file.path("results", "data", "counterfactuals"),
  counterfactual_table_dir = file.path("results", "out", "tables"),
  counterfactual_figure_dir = file.path("results", "out", "figures"),
  legacy_counterfactual_data_dir = file.path("analysis_final", "data"),
  legacy_counterfactual_table_dir = file.path("analysis_final", "out", "tables"),
  legacy_counterfactual_figure_dir = file.path("analysis_final", "out", "figures"),
  qcew_cache_path = "mkdata/raw/20220427_qcew_code/qcew_county_cache_2014_2021_812112.rds",
  qcew_force_refresh = FALSE,
  qcew_years = 2014:2021,
  qcew_quarters = 1:4,
  qcew_industry_code = 812112,
  # ---------------------------------------------------------------------------
  # Geographic scope
  # ---------------------------------------------------------------------------
  # County FIPS codes to include in estimation
  # Default: Cook County (Chicago), Manhattan (NYC), Los Angeles County
  counties = c("17031", "36061", "6037"),

  # Padded county FIPS codes (with leading zeros) used in data construction
  # and clustering where the raw data uses zero-padded codes
  counties_padded = c("17031", "36061", "06037"),

  # Estimation sample quarters (exclude 2020 Q2/Q3 COVID disruption quarters)
  estimation_quarters = c(
    2018.1, 2018.2, 2018.3, 2018.4,
    2019.1, 2019.2, 2019.3, 2019.4,
    2020.1, 2020.4,
    2021.1, 2021.2
  ),

  # Partial quarters to exclude from the stylized-facts / full-sample analyses.
  # 2021.3 is only partially observed in the raw data pull.
  excluded_quarters_analysis = c(2021.3),
  counterfactual_focus_quarters = c(2021.2),

  # ---------------------------------------------------------------------------
  # Model dimensions
  # ---------------------------------------------------------------------------
  # Number of worker types (rows of assignment matrix B)
  n_worker_types = 5,


  # Number of task types (columns of assignment matrix B)
  n_task_types = 5,

  # Quantile of within-firm min_cutlevel used to set the county-wide cut height
  # in the worker-type clustering. The original specification used the max
  # (set 1.0 to recover it), which makes a single outlier firm-quarter dictate
  # the cut height for every salon in the county and over-collapses typical
  # salons to <5 types. The cut applied to each firm-quarter is
  # max(county_quantile, firm_min_cutlevel), so the >5-cluster firms still
  # fall back to their own min_cutlevel; under complete linkage this is
  # equivalent to "cut at county_quantile, then complete-linkage-merge the
  # excess clusters."
  cutlevel_quantile = 0.9,

  # ---------------------------------------------------------------------------
  # Numerical parameters
  # ---------------------------------------------------------------------------
  # Initial guess for worker type distribution (uniform by default)
  # Length must equal n_worker_types; will be normalized to sum to 1
  initial_E = NULL,  # NULL means use uniform: rep(1/n_worker_types, n_worker_types)

  # Numerical ceiling to replace Inf (prevents overflow in exp calculations)
  numeric_ceiling = 1e16,

  # Numerical floor to replace 0 (prevents underflow/division by zero)
  numeric_floor = 1e-16,

  # Threshold below which B matrix entries are set to 0
  B_zero_threshold = 1e-16,

  # ---------------------------------------------------------------------------
  # Bisection parameters (for finding gamma given s_index)
  # ---------------------------------------------------------------------------
  # Initial lower bound for gamma search
  bisection_lower = 1,

  # Initial upper bound for gamma search
  bisection_upper = 10000,

  # Maximum iterations for bisection
  bisection_max_iter = 10000,

  # Bisection bracket adjustment parameters
  bisection_low_test_point = 0.1,      # Test point to check if f(low) > 0
  bisection_high_start_if_positive = 40, # Starting lower bound when f(0.1) > 0
  bisection_nan_step = 10,             # Step size when searching for non-NaN region
  bisection_lower_step = 0.005,        # Step size when lowering lower bound
  bisection_upper_step = 10,           # Step size when raising upper bound

  # ---------------------------------------------------------------------------
  # Fixed-point iteration parameters (SQUAREM)
  # ---------------------------------------------------------------------------
  # Maximum iterations for inner fixed-point. Env-overridable so counterfactual
  # runs can raise the cap for near-corner firms (small gamma + large wage-level
  # cost spreads) where SQUAREM needs ~1.5e5 fpevals to converge; estimation
  # leaves the env unset and keeps the 1e5 default.
  fixedpoint_max_iter = as.integer(Sys.getenv("JMP_FIXEDPOINT_MAX_ITER", unset = "100000")),

  # ---------------------------------------------------------------------------
  # Tolerance and control parameters
  # ---------------------------------------------------------------------------
  # Inner fixed-point tolerance
  innertol = 1e-10,

  # Bisection tolerance for gamma
  outertol = 1e-08,

  # Absolute objective threshold used by wage-solver convergence gates.
  obj_tol = 0.01,

  # Wage-stage convergence flag: "obj_tol" requires the absolute GMM objective;
  # "reltol" uses the polish optimizer's convergence status. This changes
  # classification only, not the optimization path.
  wage_convergence_gate = tolower(Sys.getenv("JMP_WAGE_CONVERGENCE_GATE", unset = "obj_tol")),

  # The structural wage-parameter solve is expensive. 06_estimation.R runs the
  # full wage solver (per `wage_optimizer_mode`, default pso) by default;
  # set JMP_SKIP_STRUCTURAL_OPTIMIZER=true only for refactor smoke tests that
  # need to validate downstream assembly quickly.
  skip_structural_optimizer = tolower(Sys.getenv("JMP_SKIP_STRUCTURAL_OPTIMIZER", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  structural_optimizer_maxit = as.integer(Sys.getenv("JMP_STRUCTURAL_OPTIMIZER_MAXIT", unset = "1000000")),
  check_structural_start = tolower(Sys.getenv("JMP_CHECK_STRUCTURAL_START", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),

  # ---------------------------------------------------------------------------
  # Shared structural solver speed controls
  # ---------------------------------------------------------------------------
  # The wage estimator and counterfactual solvers both solve the same inner
  # entropy-regularized assignment problem. These switches keep speed-oriented
  # behavior explicit and reusable.
  use_solver_cache = tolower(Sys.getenv("JMP_USE_SOLVER_CACHE", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  solver_cache_round_digits = as.integer(Sys.getenv("JMP_SOLVER_CACHE_ROUND_DIGITS", unset = "12")),
  use_solver_warm_starts = tolower(Sys.getenv("JMP_USE_SOLVER_WARM_STARTS", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  use_fixedpoint_warm_starts = tolower(Sys.getenv("JMP_USE_FIXEDPOINT_WARM_STARTS", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  use_staged_solver_tolerances = tolower(Sys.getenv("JMP_USE_STAGED_SOLVER_TOLERANCES", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  coarse_innertol = as.numeric(Sys.getenv("JMP_COARSE_INNERTOL", unset = "1e-5")),
  coarse_outertol = as.numeric(Sys.getenv("JMP_COARSE_OUTERTOL", unset = "1e-3")),
  staged_tolerance_switch_norm = as.numeric(Sys.getenv("JMP_STAGED_TOLERANCE_SWITCH_NORM", unset = "1e-3")),
  use_rcpp_equilibrium = tolower(Sys.getenv("JMP_USE_RCPP_EQUILIBRIUM", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  ## The interior-share penalty applies to minimizer modes; nleqslv is an
  ## unpenalized root-finding alternative.
  wage_optimizer_mode = Sys.getenv("JMP_WAGE_OPTIMIZER_MODE", unset = "pso"),
  ## Skill-matrix monotonicity restriction. "none" leaves the demand-IV step
  ## unconstrained (matches 06_estimation.R). "workers_rows" imposes that each
  ## county's productivity matrix is consistent with some total ordering of
  ## worker types: B[t, pi(i+1)] >= B[t, pi(i)] for every task t. The
  ## permutation pi is searched over all n_w! orderings per county.
  skill_monotone_orientation = Sys.getenv("JMP_SKILL_MONOTONE", unset = "none"),
  nleqslv_method = Sys.getenv("JMP_NLEQSLV_METHOD", unset = "Broyden"),
  nleqslv_global = Sys.getenv("JMP_NLEQSLV_GLOBAL", unset = "dbldog"),
  nleqslv_maxit = as.integer(Sys.getenv("JMP_NLEQSLV_MAXIT", unset = "200")),
  nleqslv_trace = as.integer(Sys.getenv("JMP_NLEQSLV_TRACE", unset = "1")),
  ## Settings for wage_optimizer_mode = "min_optim" (per-county optim
  ## Nelder-Mead minimizer on ||g_wage||^2). parscale_wage assumes the wage
  ## parameters are O(50); set per-context if needed.
  min_optim_maxit = as.integer(Sys.getenv("JMP_MIN_OPTIM_MAXIT", unset = "5000")),
  min_optim_reltol = as.numeric(Sys.getenv("JMP_MIN_OPTIM_RELTOL", unset = "1e-8")),
  min_optim_parscale_wage = as.numeric(Sys.getenv("JMP_MIN_OPTIM_PARSCALE_WAGE", unset = "20")),
  ## Optional per-county Nelder-Mead scale overrides, keyed by county code.
  min_optim_parscale_wage_by_county = list(
    "36061" = as.numeric(Sys.getenv("JMP_MIN_OPTIM_PARSCALE_WAGE_36061", unset = "200"))
  ),
  ## When optim returns convergence code 10 (degenerate simplex), perturb the
  ## current best vertex by min_optim_restart_jitter (relative) and restart
  ## up to min_optim_max_restarts times before raising the strict-exit error.
  min_optim_max_restarts = as.integer(Sys.getenv("JMP_MIN_OPTIM_MAX_RESTARTS", unset = "3")),
  min_optim_restart_jitter = as.numeric(Sys.getenv("JMP_MIN_OPTIM_RESTART_JITTER", unset = "0.05")),
  ## Per-county multistart counts. The supplied seed is always included, and
  ## the candidate with the lowest final objective is retained.
  min_optim_n_multistarts_by_county = list(
    "36061" = as.integer(Sys.getenv("JMP_MIN_OPTIM_N_MULTISTARTS_36061", unset = "5"))
  ),
  min_optim_multistart_scale_by_county = list(),
  ## Settings for per-county particle swarm search followed by NM polish.
  pso_n_particles = as.integer(Sys.getenv("JMP_PSO_N_PARTICLES", unset = "40")),
  pso_n_iter = as.integer(Sys.getenv("JMP_PSO_N_ITER", unset = "100")),
  pso_search_halfwidth = as.numeric(Sys.getenv("JMP_PSO_SEARCH_HALFWIDTH", unset = "2000")),
  pso_search_halfwidth_by_county = list(),
  pso_w  = as.numeric(Sys.getenv("JMP_PSO_W",  unset = "0.7")),
  pso_c1 = as.numeric(Sys.getenv("JMP_PSO_C1", unset = "1.5")),
  pso_c2 = as.numeric(Sys.getenv("JMP_PSO_C2", unset = "1.5")),
  pso_polish_method = Sys.getenv("JMP_PSO_POLISH_METHOD", unset = "Nelder-Mead"),
  pso_seed_offset = as.integer(Sys.getenv("JMP_PSO_SEED_OFFSET", unset = "0")),
  min_optim_trace = as.integer(Sys.getenv("JMP_MIN_OPTIM_TRACE", unset = "0")),
  ## Run the per-county PSO solves in parallel forks (Linux only). Counties are
  ## exactly separable (build_cost_matrix() selects parameters by county
  ## pattern; each county's RNG is reseeded from its own pso_seed), so results
  ## are bit-identical to the sequential loop. The one sequential coupling --
  ## the staged-tolerance coarse/fine decision at each county's first
  ## objective call, which depends on the previous county's final moment norm
  ## -- is checked after the forks return, and any county whose decision would
  ## have differed is transparently re-run in-process with the exact serial
  ## incoming state (see estimate_wage_parameters_pso).
  wage_pso_county_parallel = tolower(Sys.getenv("JMP_WAGE_PSO_COUNTY_PARALLEL", unset = "true")) %in%
    c("true", "1", "yes"),

  ## ---------------------------------------------------------------------------
  ## Optional layered fallbacks for improving solutions left in weak basins.
  ## Each layer is independently configurable; layer 5 is disabled by default.
  ## ---------------------------------------------------------------------------

  ## Layer 0: skip fallback work for county slices already below this objective.
  wage_fallback_converged_ssq_tol = as.numeric(Sys.getenv("JMP_WAGE_FB_CONVERGED_TOL", unset = "1e-8")),

  ## Layer 1: per-county post-PSO polish with a tighter relative tolerance.
  wage_fallback_post_polish_enable = tolower(Sys.getenv("JMP_WAGE_FB_L1_ENABLE", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  wage_fallback_post_polish_reltol = as.numeric(Sys.getenv("JMP_WAGE_FB_L1_RELTOL", unset = "1e-4")),
  wage_fallback_post_polish_maxit = as.integer(Sys.getenv("JMP_WAGE_FB_L1_MAXIT", unset = "2000")),
  wage_fallback_post_polish_threshold = as.numeric(Sys.getenv("JMP_WAGE_FB_L1_THRESHOLD", unset = "0.01")),

  ## Layer 2: per-county slice-Hessian probe (diagnostic only -- emits a log
  ## verdict, does not change the parameter vector). Mirrors 06b's eigenvalue
  ## test exactly. Marks counties as local_min / ridge / saddle so layer 3
  ## can target the bad ones. Cost: ~3 min/county.
  wage_fallback_hessian_enable = tolower(Sys.getenv("JMP_WAGE_FB_L2_ENABLE", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  wage_fallback_hessian_neg_eig_tol = as.numeric(Sys.getenv("JMP_WAGE_FB_L2_NEG_EIG_TOL", unset = "1e-9")),

  ## Layer 3: per-county multistart for counties flagged saddle/ridge by
  ## layer 2 (or any county if layer 2 is off). K random starts uniform in
  ## [x* - scale*parscale, x* + scale*parscale]^d, polish each, keep best.
  wage_fallback_multistart_enable = tolower(Sys.getenv("JMP_WAGE_FB_L3_ENABLE", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  wage_fallback_multistart_k = as.integer(Sys.getenv("JMP_WAGE_FB_L3_K", unset = "4")),
  wage_fallback_multistart_scale_mult = as.numeric(Sys.getenv("JMP_WAGE_FB_L3_SCALE_MULT", unset = "5")),
  wage_fallback_multistart_only_flagged = tolower(Sys.getenv("JMP_WAGE_FB_L3_ONLY_FLAGGED", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),

  ## Layer 4: re-PSO from the joint vector improved by layers 1-3. Iterate
  ## up to repso_max_iter times; stop when no county's slice ssq improves by
  ## more than repso_min_improvement (relative).
  wage_fallback_repso_max_iter = as.integer(Sys.getenv("JMP_WAGE_FB_L4_MAX_ITER", unset = "2")),
  wage_fallback_repso_min_improvement = as.numeric(Sys.getenv("JMP_WAGE_FB_L4_MIN_IMPROVEMENT", unset = "0.01")),
  ## Optionally skip a repeated swarm and polish only from the improved seed.
  ## This is faster but need not be bit-identical because solver cache state can
  ## differ from a full re-dispatch.
  wage_fallback_repso_polish_only = tolower(Sys.getenv("JMP_WAGE_FB_L4_POLISH_ONLY", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),

  ## Layer 5: joint multistart. Run the entire wage solver (with layers 1-4)
  ## K times from K perturbed joint starts, keep the run with the lowest
  ## joint score. Off by default: each extra start is a full wage solve.
  ## Set JMP_WAGE_FB_L5_K=4 (or similar) to enable for ship-time runs.
  wage_fallback_joint_multistart_k = as.integer(Sys.getenv("JMP_WAGE_FB_L5_K", unset = "0")),
  wage_fallback_joint_multistart_jitter = as.numeric(Sys.getenv("JMP_WAGE_FB_L5_JITTER", unset = "0.1")),
  ## Run the K joint-multistart solves in parallel forks (Linux only). Each
  ## start is a pure function of its start vector: the dispatcher resets the
  ## global solver cache on entry and all RNG consumers reseed
  ## deterministically, so results are bit-identical to the sequential loop.
  ## The parent re-installs start K's final solver cache and RNG state so the
  ## downstream price stage sees exactly what a serial run would have left.
  ## Only takes effect when solver_state is NULL (the 06 path); a caller-
  ## supplied solver_state couples the starts and forces the serial loop.
  wage_fallback_joint_multistart_parallel = tolower(Sys.getenv("JMP_WAGE_FB_L5_PARALLEL", unset = "true")) %in%
    c("true", "1", "yes"),

  ## Checkpoint path for the L1/L3/L4 joint wage vector. Defaults (when unset)
  ## to <prep_output_dir>/06_wage_fb_checkpoint.rds inside
  ## wage_fallback_checkpoint_write(). 06 and 06b both run the wage solver, so
  ## they both write this file: give them distinct paths (or "none") whenever
  ## they run CONCURRENTLY, or they race on it. Unset => NA => solver_value()
  ## falls through to that default, so normal runs are unchanged.
  wage_fallback_checkpoint_path = Sys.getenv("JMP_WAGE_FB_CHECKPOINT_PATH", unset = NA_character_),

  ## Global fallback gates. Bootstrap runs use the same ladder unless explicitly
  ## disabled for speed.
  wage_fallback_skip_in_bootstrap = tolower(Sys.getenv("JMP_WAGE_FB_SKIP_BOOTSTRAP", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  wage_fallback_verbose = tolower(Sys.getenv("JMP_WAGE_FB_VERBOSE", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),

  ## Interior-share penalty for the wage stage:
  ##   weight * sum max(0, log(min_share) - log(model_share))^2.
  ## It prevents numerical-floor shares from leaving wage coefficients locally
  ## unidentified. Minimizer modes and Murphy-Topel use it; nleqslv ignores it.
  wage_interior_penalty_enabled = tolower(Sys.getenv("JMP_WAGE_INTERIOR_PENALTY", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  wage_interior_penalty_weight = as.numeric(Sys.getenv("JMP_WAGE_INTERIOR_PENALTY_WEIGHT", unset = "1")),
  wage_interior_penalty_min_share = as.numeric(Sys.getenv("JMP_WAGE_INTERIOR_PENALTY_MIN_SHARE", unset = "1e-3")),

  structural_bound_guard_enabled = tolower(Sys.getenv("JMP_STRUCTURAL_BOUND_GUARD", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  structural_bound_guard_weight = as.numeric(Sys.getenv("JMP_STRUCTURAL_BOUND_GUARD_WEIGHT", unset = "10")),
  structural_bound_guard_tol = as.numeric(Sys.getenv("JMP_STRUCTURAL_BOUND_GUARD_TOL", unset = "1e-10")),
  optimizer_failure_penalty = as.numeric(Sys.getenv("JMP_OPTIMIZER_FAILURE_PENALTY", unset = "1e6")),
  county_optimizer_rounds = as.integer(Sys.getenv("JMP_COUNTY_OPTIMIZER_ROUNDS", unset = "1")),
  price_optimizer_maxit = as.integer(Sys.getenv("JMP_PRICE_OPTIMIZER_MAXIT", unset = "1000000")),
  price_optimizer_trace = as.integer(Sys.getenv("JMP_PRICE_OPTIMIZER_TRACE", unset = "3")),
  counterfactual_wage_tol = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_WAGE_TOL", unset = "0.01")),
  ## Tier-2 fallback for a rank-deficient labor-clearing system. It fixes one
  ## wage, drops one residual, and solves the reduced system. Explicit county
  ## choices override the coordinates selected from the Jacobian null spaces.
  tier2_drop_residual_by_county = list("6037" = 1L),
  tier2_fix_wage_by_county = list(),
  # PSO search halfwidth in log-wage space for counterfactual_solve_wage_market_pso.
  # Default 6 -> wages explored in roughly [exp(-6), exp(6)] = [0.0025, 403].
  # Distinct from pso_search_halfwidth (=2000) because the estimation PSO
  # operates on wage *coefficients* with no sign constraint, while the
  # counterfactual PSO searches positive wage *levels* in log space.
  counterfactual_pso_log_halfwidth = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_PSO_LOG_HALFWIDTH", unset = "6")),
  # NM polish parscale on log-wages for the counterfactual PSO. Wage levels
  # differ by O(1) in log space so unit parscale is appropriate; distinct from
  # min_optim_parscale_wage (=20) which targets wage-coefficient scales.
  counterfactual_pso_log_parscale = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_PSO_LOG_PARSCALE", unset = "1")),
  # Inner SQUAREM tolerance used by counterfactual organization solves.
  counterfactual_innertol = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_INNERTOL", unset = "1e-10")),
  # Counterfactual-assignment SQUAREM cap, separate from the setup-loop limit
  # because near-corner assignments may require many iterations.
  counterfactual_fixedpoint_max_iter = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_FIXEDPOINT_MAX_ITER", unset = "5000000")),
  # Adaptive-cap cheap budget for counterfactual_assignment's SQUAREM. Each
  # firm first runs SQUAREM with this cap; if it converges (most firms do in
  # <1000 fpevals), accept. Else warm-restart at the full cap. Avoids paying
  # the full cap cost on the well-conditioned firms. Set to 0 (or >= full
  # cap) to disable the adaptive path.
  counterfactual_fixedpoint_max_iter_cheap = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_FIXEDPOINT_MAX_ITER_CHEAP", unset = "1000")),
  # Outer best-response price contraction tolerance passed to
  # counterfactual_best_response_prices and the inline figure helpers.
  # Hardened default 1e-8 (production value). Loosen via env for smoke runs.
  counterfactual_outertol = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_OUTERTOL", unset = "1e-8")),
  counterfactual_nleqslv_maxit = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_NLEQSLV_MAXIT", unset = "400")),
  counterfactual_bbsolve_maxit = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_BBSOLVE_MAXIT", unset = "10000")),
  counterfactual_broyden_maxit = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_BROYDEN_MAXITER", unset = "1000")),
  counterfactual_price_max_iter = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_PRICE_MAX_ITER", unset = "10000")),
  counterfactual_zero_gamma_floor = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_ZERO_GAMMA_FLOOR", unset = "0")),
  counterfactual_smoke_mode = tolower(Sys.getenv("JMP_COUNTERFACTUAL_SMOKE_MODE", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),

  # full_5d_retry knobs (read via solver_value(); defaults preserve prior
  # in-code values so unset env vars keep existing behaviour). Override per
  # run when widening the fallback ladder for hard markets.
  counterfactual_fallback_multistarts = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_FALLBACK_MULTISTARTS", unset = "5")),
  counterfactual_fallback_perturb_log_sd = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_FALLBACK_PERTURB_LOG_SD", unset = "0.5")),
  counterfactual_lhs_dfsane_n_starts = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_LHS_DFSANE_N_STARTS", unset = "8")),
  counterfactual_lhs_dfsane_halfwidth = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_LHS_DFSANE_HALFWIDTH", unset = "2.5")),
  counterfactual_lhs_dfsane_maxit = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_LHS_DFSANE_MAXIT", unset = "600")),
  counterfactual_fallback_pso_halfwidth = as.numeric(Sys.getenv("JMP_COUNTERFACTUAL_FALLBACK_PSO_HALFWIDTH", unset = "3")),
  counterfactual_fallback_pso_particles = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_FALLBACK_PSO_PARTICLES", unset = "40")),
  counterfactual_fallback_pso_iter = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_FALLBACK_PSO_ITER", unset = "100")),
  counterfactual_coord_descent_sweeps = as.integer(Sys.getenv("JMP_COUNTERFACTUAL_COORD_DESCENT_SWEEPS", unset = "14")),

  # When disabled, try root-finders before applying the acceptance gate and
  # escalating to coordinate descent.
  counterfactual_coord_descent_first = tolower(Sys.getenv("JMP_COUNTERFACTUAL_COORD_DESCENT_FIRST", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),

  # "max" requires every worker type to meet the tolerance; "labor_weighted"
  # uses the target-labor-weighted mean residual. Ranking still uses max|r|.
  counterfactual_convergence_metric = tolower(Sys.getenv("JMP_COUNTERFACTUAL_CONVERGENCE_METRIC", unset = "labor_weighted")),

  # ---------------------------------------------------------------------------
  # BBsolve warm-restart checkpoint (debugging only)
  # ---------------------------------------------------------------------------
  # When TRUE and wage_optimizer_mode is "joint" or "county", every Nth call to
  # objective_gmm writes the current theta to mkdata/data/06_bb_warmstart.rds via
  # an atomic temp-rename, so an interrupted BBsolve run can be resumed from the
  # last checkpoint. Default is FALSE because the per-evaluation disk I/O is
  # only useful when actively debugging long BB runs.
  bb_checkpoint_enabled = tolower(Sys.getenv("JMP_BB_CHECKPOINT_ENABLED", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  bb_checkpoint_every = as.integer(Sys.getenv("JMP_BB_CHECKPOINT_EVERY", unset = "10")),

  # ---------------------------------------------------------------------------
  # Bootstrap execution
  # ---------------------------------------------------------------------------
  bootstrap_reps = as.integer(Sys.getenv("JMP_BOOTSTRAP_REPS", unset = "1100")),
  bootstrap_seed = as.integer(Sys.getenv("JMP_BOOTSTRAP_SEED", unset = "833927")),
  bootstrap_backend = Sys.getenv("JMP_BOOTSTRAP_BACKEND", unset = "auto"),
  bootstrap_workers = as.integer(Sys.getenv("JMP_BOOTSTRAP_WORKERS", unset = NA_character_)),
  bootstrap_results_dir = Sys.getenv(
    "JMP_BOOTSTRAP_RESULTS_DIR",
    unset = file.path("results", "data", "bootstrap_reps")
  ),
  bootstrap_iteration = as.integer(Sys.getenv("JMP_BOOTSTRAP_ITERATION", unset = NA_character_)),
  bootstrap_iteration_start = as.integer(Sys.getenv("JMP_BOOTSTRAP_ITERATION_START", unset = NA_character_)),
  bootstrap_iteration_end = as.integer(Sys.getenv("JMP_BOOTSTRAP_ITERATION_END", unset = NA_character_)),
  bootstrap_combine_only = tolower(Sys.getenv("JMP_BOOTSTRAP_COMBINE_ONLY", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  slurm_array_task_id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA_character_)),

  # Whether bootstrap standard errors use only status=="ok" replications.
  # By default all non-error replications are retained.
  bootstrap_se_filter_to_ok = tolower(Sys.getenv("JMP_BOOTSTRAP_SE_FILTER_TO_OK", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),

  # ---------------------------------------------------------------------------
  # Murphy-Topel structural standard errors (07_vcov.R)
  # ---------------------------------------------------------------------------
  # Structural SE source: Murphy-Topel or SD across first-stage draws. Demand
  # rows always use analytical clustered 2SLS standard errors.
  structural_se_source = tolower(Sys.getenv("JMP_STRUCTURAL_SE_SOURCE", unset = "murphy_topel")),
  # Workers for the numerical Jacobian columns (mclapply forks; serial default).
  mt_workers = as.integer(Sys.getenv("JMP_MT_WORKERS", unset = "1")),
  # Multiplier on the finite-difference step sizes (beta: 0.01*SE_j;
  # wage: 1e-3*max(|w_j|, county parscale)). Use with JMP_MT_STEP_CHECK to
  # probe derivative stability.
  mt_step_scale = as.numeric(Sys.getenv("JMP_MT_STEP_SCALE", unset = "1")),
  # Richardson-style robustness probe: recompute the first few Jacobian
  # columns at 2h and report the relative disagreement.
  mt_step_check = tolower(Sys.getenv("JMP_MT_STEP_CHECK", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  # Smoke mode: run the base evaluation, alignment/FOC/bound diagnostics and
  # one Jacobian column per block, then exit WITHOUT writing the vcov file.
  mt_smoke = tolower(Sys.getenv("JMP_MT_SMOKE", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),

  # Whether to use parallel processing
  pl_on = TRUE,

  # Number of cores for parallel processing (NULL = auto-detect)
  core_count = NULL,

  # ---------------------------------------------------------------------------
  # Column naming patterns (for extracting data from data.table)
  # ---------------------------------------------------------------------------
  # Pattern for task mix columns (e.g., "task_mix_1", "task_mix_2", ...)
  task_mix_pattern = "task_mix_",

  # Pattern for E_raw columns (observed worker type shares)
  E_raw_pattern = "E_raw_",

  # Pattern for B_raw columns (skill parameters in beta)
  B_raw_pattern = "B_raw_",

  # ---------------------------------------------------------------------------
  # Instrument construction
  # ---------------------------------------------------------------------------
  # Index of the task type used for the dye instrument
  dye_task_index = 2
)

#' Helper function to get initial E vector based on config
#' @param config Configuration list
#' @return Numeric vector of initial worker type distribution
get_initial_E <- function(config = CONFIG) {
  if (is.null(config$initial_E)) {
    return(rep(1 / config$n_worker_types, config$n_worker_types))
  }
  # Normalize to sum to 1
  return(config$initial_E / sum(config$initial_E))
}

#' Helper function to generate column names for task mix
#' @param config Configuration list
#' @return Character vector of task mix column names
get_task_mix_cols <- function(config = CONFIG) {
  paste0(config$task_mix_pattern, 1:config$n_task_types)
}

#' Helper function to generate column names for E_raw
#' @param config Configuration list
#' @return Character vector of E_raw column names
get_E_raw_cols <- function(config = CONFIG) {
  paste0(config$E_raw_pattern, 1:config$n_worker_types)
}

#' Detect operating system
#'
#' @return Character string: "windows", "osx", or "linux"
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  unname(tolower(os))
}

#' Get core count for parallel processing
#'
#' Returns cores in the following priority:
#' 1. CONFIG$core_count (if explicitly set)
#' 2. SLURM_CPUS_PER_TASK environment variable (if on a cluster)
#' 3. MC_CORES environment variable
#' 4. OS-specific auto-detection (Windows: detectCores()-1, Linux/Mac: 1)
#'
#' @param config Configuration list
#' @return Integer number of cores to use
get_core_count <- function(config = CONFIG) {
  if (!is.null(config$core_count)) {
    return(config$core_count)
  }

  # Check environment variables
  slurm_cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
  if (slurm_cores != "") {
    return(as.integer(slurm_cores))
  }

  mc_cores <- Sys.getenv("MC_CORES")
  if (mc_cores != "") {
    return(as.integer(mc_cores))
  }

  # Fallback to auto-detection
  if (get_os() == "windows") {
    return(parallel::detectCores() - 1)
  } else {
    # On Linux/Unix, default to 1 core unless explicitly told otherwise.
    # This prevents 'core-bombing' shared login nodes or cluster nodes.
    return(1)
  }
}

#' Build model.matrix formula for E_raw columns
#'
#' Generates a formula like ~county:E_2 + county:E_3 + ... + county:E_N + county:s_index - 1
#' for use with model.matrix in GMM estimation.
#'
#' @param e_indices Integer vector of E indices to include (e.g., 2:5 for E_2..E_5)
#' @param include_s_index Logical, whether to include county:s_index term
#' @param config Configuration list
#' @return A formula object
build_E_formula <- function(e_indices, include_s_index, config = CONFIG) {
  terms <- paste0("county:E_", e_indices)
  if (include_s_index) {
    terms <- c(terms, "county:s_index")
  }
  as.formula(paste0("~", paste0(terms, collapse = " + "), " - 1"))
}

#' Build task_mix sum string for formulas
#'
#' Generates a string like "task_mix_2+task_mix_3+task_mix_4+task_mix_5"
#' for use in regression formulas.
#'
#' @param config Configuration list
#' @return Character string of task_mix terms joined by "+"
build_task_mix_sum <- function(config = CONFIG) {
  paste0("task_mix_", 2:config$n_task_types, collapse = "+")
}
get_project_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

  repeat {
    has_config <- file.exists(file.path(current_dir, "config.R"))
    has_project <- file.exists(file.path(current_dir, "refactor_estimation.Rproj")) |
      dir.exists(file.path(current_dir, ".git"))

    if (has_config & has_project) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Could not locate project root from: ", start_dir)
    }

    current_dir <- parent_dir
  }
}

project_path <- function(..., root_dir = get_project_root()) {
  file.path(root_dir, ...)
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  return(invisible(path))
}

assert_required_files <- function(paths) {
  missing_paths <- paths[!file.exists(paths)]

  if (length(missing_paths) > 0) {
    stop("Missing required file(s):\n  ", paste(missing_paths, collapse = "\n  "))
  }

  return(invisible(paths))
}

assert_required_columns <- function(dt, required_cols, object_name) {
  missing_cols <- setdiff(required_cols, names(dt))

  if (length(missing_cols) > 0) {
    stop(object_name, " is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  return(invisible(TRUE))
}

activate_project_renv <- function(root_dir = get_project_root()) {
  activate_path <- file.path(root_dir, "renv", "activate.R")

  if (!file.exists(activate_path)) {
    return(invisible(FALSE))
  }

  source(activate_path, local = TRUE)
  return(invisible(TRUE))
}


