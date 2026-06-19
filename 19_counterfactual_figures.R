## Plot the Los Angeles immigration counterfactual margins.
## The earlier counterfactual scripts solve wages and productivity panels; this
## script reconstructs firm-level organization snapshots only for the figure
## decomposition.
source("config.R")
source("utils/counterfactuals_core.R")

innertol <- CONFIG$counterfactual_innertol
outertol <- CONFIG$counterfactual_outertol

counterfactual_context <- load_counterfactual_context(
  extra_packages = c("ggplot2", "ggforce")
)
working_data <- counterfactual_context$working_data
initial_wages <- counterfactual_context$initial_wages
market_parms <- counterfactual_context$market_parms
rho <- counterfactual_context$rho

cf_log <- function(msg, level = "INFO") {
  if (exists("log_message", mode = "function")) {
    log_message(msg, level)
  } else {
    message("[", level, "] ", msg)
  }
}

spec_log <- counterfactual_spec_log

task_mix_cols <- get_task_mix_cols(CONFIG)
e_field_names <- counterfactual_e_field_names(CONFIG)
b_field_names <- counterfactual_b_field_names(CONFIG)
structure_output_cols <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)
market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
)

build_market_matrices <- function(wage_guess, cnty) {
  new_theta <- matrix(
    market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
    ncol = CONFIG$n_task_types,
    nrow = CONFIG$n_worker_types,
    byrow = FALSE
  )
  w_mat <- matrix(wage_guess, ncol = CONFIG$n_task_types, nrow = CONFIG$n_worker_types, byrow = FALSE)
  new_tild_theta <- w_mat + (rho[cnty])^(-1) * new_theta
  new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min), FUN = "-")
  list(new_theta = new_theta, new_tild_theta = new_tild_theta)
}

apply_pricing <- function(counter_res, cnty, qy) {
  counter_res[, Q := q_endog * avg_labor + qual_exo]
  counter_res[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  counter_res[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy)
  )]
  counter_res[, new_share := exp(Q + rho[cnty] * newprice)]
  counter_res[, new_share := new_share / (sum(weight * new_share) + 1)]
  counter_res
}

get_everything_reorg <- function(wage_guess, cnty, qy) {
  counter_res <- copy(working_data[
    county == cnty & quarter_year == qy,
    ..market_input_cols
  ])
  mats <- build_market_matrices(wage_guess, cnty)

  counter_res[, (structure_output_cols) := counterfactual_org_outputs(
      cost_matrix  = mats$new_tild_theta,
      alpha        = as.numeric(.SD),
      gamma        = gamma_invert,
      wage_guess   = wage_guess,
      new_theta    = mats$new_theta,
      innertol     = innertol,
      with_s_index = TRUE,
      with_b       = TRUE,
      config       = CONFIG
    ),
    by = c("location_id"),
    .SDcols = task_mix_cols
  ]

  apply_pricing(counter_res, cnty, qy)
}

cf_log("Reconstructing baseline firm organization")
orig_struct <- build_counterfactual_structure_snapshot(get_everything_reorg, initial_wages)

get_everything_realloc <- function(wage_guess, cnty, qy) {
  counter_res <- copy(working_data[
    county == cnty & quarter_year == qy,
    ..market_input_cols
  ])
  mats <- build_market_matrices(wage_guess, cnty)
  saved_b_cols <- grep("^B_", colnames(orig_struct[[cnty]]), value = TRUE)

  solve_one <- function(loc, alpha, gamma) {
    saved_B <- matrix(
      as.numeric(orig_struct[[cnty]][location_id == loc, .SD, .SDcols = saved_b_cols]),
      byrow = FALSE,
      nrow = CONFIG$n_worker_types,
      ncol = CONFIG$n_task_types
    )
    counterfactual_org_outputs_from_b(
      B            = saved_B,
      alpha        = alpha,
      gamma        = gamma,
      wage_guess   = wage_guess,
      new_theta    = mats$new_theta,
      with_s_index = TRUE,
      with_b       = TRUE,
      config       = CONFIG
    )
  }

  counter_res[, (structure_output_cols) := solve_one(
      location_id, as.numeric(.SD), gamma_invert
    ),
    by = c("location_id"),
    .SDcols = task_mix_cols
  ]

  apply_pricing(counter_res, cnty, qy)
}


## Reconstruct structures with the wage vector from each equilibrium.
cf_log("Loading immigration wage solution")
wage_vect_immigration<-read_counterfactual_rds(
  "16_wages_immigration.rds",
  legacy_filenames = "16_wages_immigration.rds",
  description = "immigration counterfactual wages"
)



cf_log("Reconstructing immigration reallocation organization")
realloc_struct <- build_counterfactual_structure_snapshot(
  get_everything_realloc,
  wage_vect_immigration,
  solution_type = "realloc"
)


cf_log("Reconstructing immigration reorganization organization")
reorg_struct <- build_counterfactual_structure_snapshot(
  get_everything_reorg,
  wage_vect_immigration,
  solution_type = "reorg"
)



## Focus on Los Angeles County. In this county the lowest-wage worker is type 1,
## and the immigration counterfactual increases that supply by 10%.
cf_log("Preparing Los Angeles immigration figure panel")

reorg_struct[['6037']][, sol_type:="Reorganization"]
realloc_struct[['6037']][, sol_type:="Reallocation"]
orig_struct[['6037']][, initial_share:=B_1_1+B_2_1+B_3_1+B_4_1+B_5_1][, initial_price:=newprice][, initial_s_index:=s_index][, initial_newshare:=new_share]

imm_la<-rbind(reorg_struct[['6037']],realloc_struct[['6037']] )
imm_la[, share_impact:=B_1_1+B_2_1+B_3_1+B_4_1+B_5_1]
imm_la<-merge(imm_la,orig_struct[['6037']][, c("location_id", "initial_price", "initial_s_index","initial_newshare","initial_share")], by="location_id" )


imm_la[, delta_share:=(new_share-initial_newshare)/initial_newshare]
imm_la[, delta_price:=(newprice-initial_price)/initial_price]
imm_la[, delta_sindex:=(s_index-initial_s_index)/initial_s_index]
imm_la[, delta_immshare:=(share_impact-initial_share)/initial_share]
imm_la[, non_imm:=1-share_impact]
imm_la[, log_gamma:=log(gamma_invert)]

imm_la[round(initial_share,digits=5)==0,initial_share:=0]
imm_la[round(share_impact,digits=5)==0,share_impact:=0]

imm_la[, ldemand:=share_impact*new_share]
imm_la[, initial_ldemand:=initial_share*initial_newshare]
imm_la[, delta_ldemand:=(ldemand-initial_ldemand)/initial_ldemand]

## exclude one firm which has 0 market share because they have all task_4
imm_la<-imm_la[round(new_share, digits=40)>0,]
## calculate the new immigrant share
imm_la[,newimm:= (share_impact-initial_share) ]

setnames(imm_la, old=c("initial_share", "non_imm","newimm"), new=c("Initial Immigrant Skill Set", "Other Skill Set", "Immigration"))


pal_color <- c("orange", "#377EB8", "#4DAF4A")

save_immigration_figure <- function(filename, width, height, units = "in",
                                    plot = ggplot2::last_plot()) {
  output_path <- save_counterfactual_plot(
    filename,
    legacy_filename = filename,
    width = width,
    height = height,
    units = units,
    plot = plot
  )
  cf_log(paste("Wrote", output_path))
  invisible(output_path)
}

make_pie_slices <- function(data, x_col, y_col, fill_cols, radius = 0.22) {
  pie_data <- copy(data)
  pie_data[, pie_id := .I]
  pie_data <- melt(
    pie_data,
    id.vars = c("pie_id", x_col, y_col),
    measure.vars = fill_cols,
    variable.name = "slice",
    value.name = "share"
  )
  pie_data[, share := pmax(share, 0)]
  pie_data[, share_total := sum(share), by = pie_id]
  pie_data <- pie_data[share_total > 0]
  pie_data[, share := share / share_total]
  pie_data[, end := cumsum(share) * 2 * pi, by = pie_id]
  pie_data[, start := shift(end, fill = 0), by = pie_id]
  pie_data[, `:=`(
    x0 = get(x_col),
    y0 = get(y_col),
    r = radius,
    slice = factor(slice, levels = fill_cols)
  )]
  pie_data
}

geom_immigration_pies <- function(data, x_col, y_col, fill_cols, radius = 0.22) {
  ## group = pie_id is what makes overlapping pies layer cleanly: geom_arc_bar
  ## draws one group at a time, so grouping by pie draws each pie atomically.
  ## Without it ggplot groups by the fill (slice), drawing every pie's slice-1,
  ## then every pie's slice-2, ... -- so a wedge of one pie renders on top of a
  ## different overlapping pie's wedge (the interleaving artifact).
  ggforce::geom_arc_bar(
    aes(
      x0 = x0,
      y0 = y0,
      r0 = 0,
      r = r,
      start = start,
      end = end,
      fill = slice,
      group = pie_id
    ),
    data = make_pie_slices(data, x_col, y_col, fill_cols, radius),
    inherit.aes = FALSE
  )
}

## --- axis helpers for the standardized pie coordinate system ----------------
## Pies are drawn at z-scored coordinates so coord_fixed() keeps them circular.
## To label the axes in ORIGINAL units, place ticks at "pretty" values on the
## original scale and map them back to standardized positions via the fitted
## lm(original ~ standardized): original = a + b * standardized, so a tick at
## original value v sits at standardized position (v - a) / b. This keeps the
## labels round and unique. (The previous code rounded the proportion BEFORE
## scaling to percent -- round(value, 1) -- which collapsed every sub-1% tick to
## "0%", e.g. the realloc price axis printed "0%" at every break.)
std_axis_breaks <- function(model, orig_values, n = 5) {
  a <- coef(model)[1]
  b <- coef(model)[2]
  vals <- scales::pretty_breaks(n = n)(range(orig_values, na.rm = TRUE))
  list(at = (vals - a) / b, vals = vals)
}
fmt_pct    <- function(v) ifelse(is.na(v), NA_character_, paste0(round(100 * v, 1), "%"))
fmt_gamma  <- function(v) sprintf("%.1f", v)
fmt_sindex <- function(v) sprintf("%.2f", v)

## reallocation logic
## high immigrant firms tend to have higher coordination cost
ggplot(aes(x=log_gamma, y=`Initial Immigrant Skill Set` ), data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]) + 
  geom_point(size=2) +ylab("Initial Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  geom_smooth(method="lm", formula=y ~ x, se=FALSE, linewidth=1)+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_realloc_scatter.png",
  width=12, height=8, units="in"
)

## they reduce prices after immigration
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), sdelta_price:=scale(delta_price)]

m2 <- lm(delta_price ~ sdelta_price, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(log_gamma ~ slog_gamma, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$delta_price)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "slog_gamma", "sdelta_price", c("Initial Immigrant Skill Set", "Other Skill Set"))+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_gamma(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pct(ax_y$vals)) +  geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ylab("Price Change")+xlab("Log Organization Cost (Gamma)")+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_realloc_price.png",
  width=14, height=12, units="in"
)


## this reduces market share and shifts labor towards high coord cost firms


imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), sdelta_share:=scale(delta_share)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(log_gamma ~ slog_gamma, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$delta_share)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "slog_gamma", "sdelta_share", c("Initial Immigrant Skill Set", "Other Skill Set"))+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_gamma(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pct(ax_y$vals)) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ylab("Market Share Change")+xlab("Log Organization Cost (Gamma)")+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_realloc_marketshare.png",
  width=14, height=12, units="in"
)

## these firms are less specialized
## this reduces aggregate labor productivity

imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), ss_index:=scale(s_index)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(s_index ~ ss_index, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$s_index)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$delta_share)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "ss_index", "sdelta_share", c("Initial Immigrant Skill Set", "Other Skill Set"))+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_sindex(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pct(ax_y$vals)) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+geom_smooth(aes(x=ss_index, y=sdelta_share),data=imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)],method='lm', formula=y ~ x, se=FALSE, linewidth=1.5, color='red', linetype='dashed')+
  scale_fill_manual(values = pal_color)+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Market Share Change")+xlab("S-Index")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_realloc_sindex.png",
  width=14, height=12, units="in"
)

## reorg

ggplot(aes(x=log_gamma, y=Immigration ), data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]) + 
  geom_point(size=2) +ylab("Additional Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_reorg_scatter.png",
  width=12, height=8, units="in"
)


## prices
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_price:=scale(delta_price)]

m2 <- lm(delta_price ~ sdelta_price, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(log_gamma~ slog_gamma, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$delta_price)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "slog_gamma", "sdelta_price", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_gamma(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pct(ax_y$vals)) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Price Change")+xlab("Log Organization Cost (Gamma)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_reorg_price.png",
  width=14, height=12, units="in"
)


##market shares
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_share:=scale(delta_share)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(log_gamma~ slog_gamma, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$delta_share)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "slog_gamma", "sdelta_share", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_gamma(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pct(ax_y$vals)) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed()+
  ylab('Market Share Change')+xlab("Log Organization Cost (Gamma)")+
  theme_bw()+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_reorg_marketshare.png",
  width=14, height=12, units="in"
)
  

# specialization

imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), ss_index:=scale(s_index)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_share:=scale(delta_share)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(s_index~ ss_index, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$s_index)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$delta_share)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "ss_index", "sdelta_share", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_sindex(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pct(ax_y$vals)) +  geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+geom_smooth(aes(x=ss_index, y=sdelta_share),data=imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)],method='lm', formula=y ~ x, se=FALSE, linewidth=1.5, color='red', linetype='dashed')+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Market Share Change")+xlab("S-Index")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_reorg_sindex.png",
  width=14, height=12, units="in"
)


## ===========================================================================
## Merger (increased-concentration) counterfactual for Los Angeles.
## The merger counterfactual (17_counterfactual_merger.R) halves market weights
## to mimic post-merger concentration, then re-solves wages. We reconstruct the
## LA org snapshots on the halved weights; the baseline `orig_struct` above was
## built before any halving, so it remains the pre-merger comparison point.
## These are plain point plots in natural coordinates -- no pies, so none of the
## immigrant-share decomposition / standardized-coordinate machinery is needed.
## ===========================================================================
cf_log("Loading merger wage solution")
wage_vect_merger <- read_counterfactual_rds(
  "17_wages_merger.rds",
  legacy_filenames = "17_wages_merger.rds",
  description = "merger counterfactual wages"
)

## Halve market weights to mimic post-merger concentration (see 17_*). The
## get_everything_* closures read this global `working_data`, so the merger
## snapshots below are built on the halved weights.
working_data[, weight := weight / 2]

cf_log("Reconstructing merger reallocation organization")
realloc_struct_m <- build_counterfactual_structure_snapshot(
  get_everything_realloc,
  wage_vect_merger,
  solution_type = "realloc"
)

cf_log("Reconstructing merger reorganization organization")
reorg_struct_m <- build_counterfactual_structure_snapshot(
  get_everything_reorg,
  wage_vect_merger,
  solution_type = "reorg"
)

cf_log("Preparing Los Angeles merger figure panel")
reorg_struct_m[['6037']][, sol_type := "Reorganization"]
realloc_struct_m[['6037']][, sol_type := "Reallocation"]

merger_la <- rbind(reorg_struct_m[['6037']], realloc_struct_m[['6037']])
merger_la <- merge(
  merger_la,
  orig_struct[['6037']][, c("location_id", "initial_price", "initial_s_index", "initial_newshare")],
  by = "location_id"
)
merger_la[, delta_share := (new_share - initial_newshare) / initial_newshare]
merger_la[, delta_price := (newprice - initial_price) / initial_price]
merger_la[, log_gamma   := log(gamma_invert)]
## exclude the firm with 0 market share (all task_4), as in the immigration panel
merger_la <- merger_la[round(new_share, digits = 40) > 0, ]

## Point-plot builder mirroring the immigration figures' look, minus the pies.
make_merger_fig <- function(stype, x_col, y_col, x_lab, y_lab, filename,
                            add_lm = FALSE) {
  dat <- merger_la[sol_type == stype & is.finite(gamma_invert)]
  p <- ggplot(dat, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
    scale_y_continuous(labels = fmt_pct) +
    xlab(x_lab) + ylab(y_lab) + theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), text = element_text(size = 36))
  if (add_lm) {
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                         linewidth = 1.5, color = "red", linetype = "dashed")
  }
  save_immigration_figure(filename, width = 12, height = 8, units = "in", plot = p)
}

## prices and market shares fall most for high coordination-cost firms; the
## s-index panel carries the productivity story (market-share shift vs. spec.).
make_merger_fig("Reallocation", "log_gamma", "delta_price",
                "Log Organization Cost (Gamma)", "Price Change",
                "19_merger_realloc_price.png")
make_merger_fig("Reallocation", "log_gamma", "delta_share",
                "Log Organization Cost (Gamma)", "Market Share Change",
                "19_merger_realloc_marketshare.png")
make_merger_fig("Reallocation", "s_index", "delta_share",
                "S-Index", "Market Share Change",
                "19_merger_realloc_sindex.png", add_lm = TRUE)

make_merger_fig("Reorganization", "log_gamma", "delta_price",
                "Log Organization Cost (Gamma)", "Price Change",
                "19_merger_reorg_price.png")
make_merger_fig("Reorganization", "log_gamma", "delta_share",
                "Log Organization Cost (Gamma)", "Market Share Change",
                "19_merger_reorg_marketshare.png")
make_merger_fig("Reorganization", "s_index", "delta_share",
                "S-Index", "Market Share Change",
                "19_merger_reorg_sindex.png", add_lm = TRUE)
