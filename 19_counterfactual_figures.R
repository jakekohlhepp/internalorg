## Plot the Los Angeles immigration counterfactual margins.
## The earlier counterfactual scripts solve wages and productivity panels; this
## script reconstructs firm-level organization snapshots only for the figure
## decomposition.
##
## Weighting design: the aggregate counterfactual effects in 18_ are
## labor-weighted (avg_labor x CSPOP x share x weight) firm averages, so the
## figures encode that weight rather than treating firms symmetrically:
## pie/point AREA = baseline market weight, the market-share panels plot the
## CHANGE in normalized market weight (percentage points, the between-firm
## margin that moves the 18_ aggregates), and fitted lines are weighted by
## baseline market weight.
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



## Focus on Los Angeles County. The immigration counterfactual increases the
## supply of the county's lowest-wage worker type by 5% of total county labor;
## the type is derived from the 13_ baseline solve exactly as in
## 16_counterfactual_immigration.R (currently type 4 for LA). The immigrant
## time share at a firm is the sum of the B_<task>_<worker> columns for that
## worker type.
cf_log("Preparing Los Angeles immigration figure panel")

la_imm_type <- counterfactual_lowest_wage_types(initial_wages)[["6037"]]
la_imm_b_cols <- paste0("B_", seq_len(CONFIG$n_task_types), "_", la_imm_type)
cf_log(paste0("LA immigration target type (lowest baseline wage): ", la_imm_type))

reorg_struct[['6037']][, sol_type:="Reorganization"]
realloc_struct[['6037']][, sol_type:="Reallocation"]
orig_struct[['6037']][, initial_share:=rowSums(.SD), .SDcols=la_imm_b_cols][, initial_price:=newprice][, initial_s_index:=s_index][, initial_newshare:=new_share]

## Baseline labor productivity (sum of B x column-swept theta, the same firm
## statistic 18_ aggregates). Used to annotate the high-productivity outlier
## that carries most of the reallocation between-firm effect. new_theta does
## not depend on wages, so any wage vector reproduces the county theta block.
la_swept_theta <- local({
  th <- build_market_matrices(rep(0, CONFIG$n_worker_types), "6037")$new_theta
  sweep(th, 2, apply(th, 2, min), FUN = "-")
})
orig_struct[['6037']][, initial_prod := as.numeric(
  as.matrix(.SD) %*% as.vector(la_swept_theta)), .SDcols = b_field_names]

imm_la<-rbind(reorg_struct[['6037']],realloc_struct[['6037']] )
imm_la[, share_impact:=rowSums(.SD), .SDcols=la_imm_b_cols]
imm_la<-merge(imm_la,orig_struct[['6037']][, c("location_id", "initial_price", "initial_s_index","initial_newshare","initial_share","initial_prod")], by="location_id" )


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

## Market (labor) weights: the aggregation weight used by 18_'s productivity
## panels (avg_labor x CSPOP x share x weight), normalized within sol_type.
## delta_weight is the between-firm reallocation margin in percentage points
## of the county market; pie AREA below encodes the baseline weight w_0.
## (Firms with non-finite gamma stay in the normalization even though they are
## dropped from the plots.)
imm_la[, mult_cf := avg_labor * CSPOP * new_share * weight]
imm_la[, mult_0  := avg_labor * CSPOP * initial_newshare * weight]
imm_la[, w_cf := mult_cf / sum(mult_cf), by = sol_type]
imm_la[, w_0  := mult_0  / sum(mult_0),  by = sol_type]
imm_la[, delta_weight := w_cf - w_0]
## Pie radius in the standardized plotting coords: area ~ baseline weight,
## floored so the smallest firms stay visible.
imm_la[, pie_r := 0.42 * pmax(sqrt(w_0 / max(w_0)), 0.12), by = sol_type]

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

make_pie_slices <- function(data, x_col, y_col, fill_cols, radius = 0.22,
                            radius_col = NULL) {
  pie_data <- copy(data)
  ## Draw big pies first so small firms render on top and stay visible.
  if (!is.null(radius_col)) setorderv(pie_data, radius_col, order = -1L)
  pie_data[, pie_id := .I]
  pie_data <- melt(
    pie_data,
    id.vars = c("pie_id", x_col, y_col, radius_col),
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
    r = if (is.null(radius_col)) radius else get(radius_col),
    slice = factor(slice, levels = fill_cols)
  )]
  pie_data
}

geom_immigration_pies <- function(data, x_col, y_col, fill_cols, radius = 0.22,
                                  radius_col = NULL) {
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
    data = make_pie_slices(data, x_col, y_col, fill_cols, radius, radius_col),
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
fmt_pp     <- function(v) ifelse(is.na(v), NA_character_,
                                 paste0(format(round(100 * v, 2), trim = TRUE), " pp"))
fmt_gamma  <- function(v) sprintf("%.1f", v)
fmt_sindex <- function(v) sprintf("%.2f", v)

## reallocation logic
## firms intensive in the immigrant skill set tend to have higher coordination
## cost. Point area = baseline market weight; the fit is weight-weighted so the
## line reflects the market-level relationship, not the average firm.
## (Both plotted variables are baseline objects, so the sol_type subset is
## immaterial; use the Reallocation rows for consistency with the filename.)
ggplot(aes(x=log_gamma, y=`Initial Immigrant Skill Set` ), data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]) +
  geom_point(aes(size=w_0)) + scale_size_area(max_size = 12, guide = "none") +
  ylab("Initial Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  geom_smooth(aes(weight=w_0), method="lm", formula=y ~ x, se=FALSE, linewidth=1)+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_realloc_scatter.png",
  width=12, height=8, units="in"
)

## with organizations frozen, the shocked type's wage falls steeply and firms
## intensive in that type cut prices most; pie area = baseline market weight
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), sdelta_price:=scale(delta_price)]

m2 <- lm(delta_price ~ sdelta_price, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(log_gamma ~ slog_gamma, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$delta_price)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "slog_gamma", "sdelta_price", c("Initial Immigrant Skill Set", "Other Skill Set"), radius_col = "pie_r")+
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


## the price cuts reallocate ABSOLUTE market weight (pp of the county market,
## the between-firm margin behind 18_'s productivity change) toward firms
## intensive in the immigrant skill set


imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), sdelta_weight:=scale(delta_weight)]

m2 <- lm(delta_weight ~ sdelta_weight, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(log_gamma ~ slog_gamma, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$delta_weight)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "slog_gamma", "sdelta_weight", c("Initial Immigrant Skill Set", "Other Skill Set"), radius_col = "pie_r")+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_gamma(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pp(ax_y$vals)) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ylab("Market Share Change (p.p.)")+xlab("Log Organization Cost (Gamma)")+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_realloc_marketshare.png",
  width=14, height=12, units="in"
)

## on average the weight moves toward LESS specialized firms (the s-index
## composition falls), but the labor-weighted shift also favors
## high-productivity firms -- dominated by one high-s-index outlier with
## ~3.5x county-average productivity -- so aggregate labor productivity
## RISES under reallocation (18_tot). The weighted red fit and the
## annotation make that visible; an unweighted fit of relative share
## changes would suggest the opposite sign.

imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), ss_index:=scale(s_index)]

m2 <- lm(delta_weight ~ sdelta_weight, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(s_index ~ ss_index, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$s_index)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)]$delta_weight)

la_realloc <- imm_la[sol_type == "Reallocation" & is.finite(gamma_invert)]
la_star <- la_realloc[which.max(initial_prod)]
la_star_ratio <- la_star$initial_prod /
  weighted.mean(la_realloc$initial_prod, la_realloc$w_0)

ggplot() +
  geom_immigration_pies(la_realloc, "ss_index", "sdelta_weight", c("Initial Immigrant Skill Set", "Other Skill Set"), radius_col = "pie_r")+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_sindex(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pp(ax_y$vals)) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+geom_smooth(aes(x=ss_index, y=sdelta_weight, weight=w_0),data=la_realloc,method='lm', formula=y ~ x, se=FALSE, linewidth=1.5, color='red', linetype='dashed')+
  annotate("text", x = la_star$ss_index + 0.1, y = la_star$sdelta_weight + 0.62,
           label = sprintf("%.1fx avg. productivity", la_star_ratio),
           size = 10, hjust = 1) +
  annotate("segment", x = la_star$ss_index, xend = la_star$ss_index,
           y = la_star$sdelta_weight + 0.48,
           yend = la_star$sdelta_weight + la_star$pie_r + 0.04,
           linewidth = 0.8) +
  scale_fill_manual(values = pal_color)+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Market Share Change (p.p.)")+xlab("S-Index")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_realloc_sindex.png",
  width=14, height=12, units="in"
)

## reorg
## when firms may re-optimize, the shock is absorbed WITHIN firms: they tilt
## task assignment toward the now-lower-wage immigrant skill set (y = change in
## that type's time share). Point area = baseline market weight.

ggplot(aes(x=log_gamma, y=Immigration ), data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]) +
  geom_point(aes(size=w_0)) + scale_size_area(max_size = 12, guide = "none") +
  ylab("Additional Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_immigration_reorg_scatter.png",
  width=12, height=8, units="in"
)


## prices: all wages fall a little, every firm passes costs through, so price
## cuts are broad and shallow rather than concentrated in immigrant-intensive
## firms as under reallocation
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_price:=scale(delta_price)]

m2 <- lm(delta_price ~ sdelta_price, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(log_gamma~ slog_gamma, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$delta_price)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "slog_gamma", "sdelta_price", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"), radius_col = "pie_r")+
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


##market shares: with every firm able to absorb the low-wage type internally, the
## reallocation margin largely shuts down -- the pp weight changes here are an
## order of magnitude smaller than under reallocation (see the axis ticks)
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_weight:=scale(delta_weight)]

m2 <- lm(delta_weight ~ sdelta_weight, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(log_gamma~ slog_gamma, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$log_gamma)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$delta_weight)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "slog_gamma", "sdelta_weight", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"), radius_col = "pie_r")+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_gamma(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pp(ax_y$vals)) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed()+
  ylab('Market Share Change (p.p.)')+xlab("Log Organization Cost (Gamma)")+
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
  

# specialization: the productivity effect under reorganization is WITHIN-firm
# (de-specialization as firms absorb the low-wage type -- the green wedges),
# not between-firm reallocation; the weighted fit shows the near-zero
# market-weighted share/s-index relationship (unweighted it looks positive)

imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), ss_index:=scale(s_index)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_weight:=scale(delta_weight)]

m2 <- lm(delta_weight ~ sdelta_weight, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(s_index~ ss_index, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
ax_x <- std_axis_breaks(m1, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$s_index)
ax_y <- std_axis_breaks(m2, imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]$delta_weight)

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "ss_index", "sdelta_weight", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"), radius_col = "pie_r")+
  scale_x_continuous(breaks = ax_x$at, labels = fmt_sindex(ax_x$vals)) +
  scale_y_continuous(breaks = ax_y$at, labels = fmt_pp(ax_y$vals)) +  geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+geom_smooth(aes(x=ss_index, y=sdelta_weight, weight=w_0),data=imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)],method='lm', formula=y ~ x, se=FALSE, linewidth=1.5, color='red', linetype='dashed')+
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
## Market (labor) weights, as in the immigration panels. The post-merger
## `weight` column is already halved, but halving is uniform across firms so
## it cancels in the normalization; delta_weight isolates the reallocation.
merger_la[, mult_cf := avg_labor * CSPOP * new_share * weight]
merger_la[, mult_0  := avg_labor * CSPOP * initial_newshare * weight]
merger_la[, w_cf := mult_cf / sum(mult_cf), by = sol_type]
merger_la[, w_0  := mult_0  / sum(mult_0),  by = sol_type]
merger_la[, delta_weight := w_cf - w_0]
## exclude the firm with 0 market share (all task_4), as in the immigration panel
merger_la <- merger_la[round(new_share, digits = 40) > 0, ]

## Point-plot builder mirroring the immigration figures' look, minus the pies.
## Point area = baseline market weight; fitted lines are weight-weighted.
make_merger_fig <- function(stype, x_col, y_col, x_lab, y_lab, filename,
                            add_lm = FALSE, y_fmt = fmt_pct) {
  dat <- merger_la[sol_type == stype & is.finite(gamma_invert)]
  p <- ggplot(dat, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(aes(size = w_0)) +
    scale_size_area(max_size = 10, guide = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
    scale_y_continuous(labels = y_fmt) +
    xlab(x_lab) + ylab(y_lab) + theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), text = element_text(size = 36))
  if (add_lm) {
    p <- p + geom_smooth(aes(weight = w_0), method = "lm", formula = y ~ x,
                         se = FALSE, linewidth = 1.5, color = "red",
                         linetype = "dashed")
  }
  save_immigration_figure(filename, width = 12, height = 8, units = "in", plot = p)
}

## prices: relative per-firm changes; market-share panels: absolute weight
## changes in pp of the county market (the between-firm margin behind the 18_
## aggregates); the s-index panel carries the composition story.
make_merger_fig("Reallocation", "log_gamma", "delta_price",
                "Log Organization Cost (Gamma)", "Price Change",
                "19_merger_realloc_price.png")
make_merger_fig("Reallocation", "log_gamma", "delta_weight",
                "Log Organization Cost (Gamma)", "Market Share Change (p.p.)",
                "19_merger_realloc_marketshare.png", y_fmt = fmt_pp)
make_merger_fig("Reallocation", "s_index", "delta_weight",
                "S-Index", "Market Share Change (p.p.)",
                "19_merger_realloc_sindex.png", add_lm = TRUE, y_fmt = fmt_pp)

make_merger_fig("Reorganization", "log_gamma", "delta_price",
                "Log Organization Cost (Gamma)", "Price Change",
                "19_merger_reorg_price.png")
make_merger_fig("Reorganization", "log_gamma", "delta_weight",
                "Log Organization Cost (Gamma)", "Market Share Change (p.p.)",
                "19_merger_reorg_marketshare.png", y_fmt = fmt_pp)
make_merger_fig("Reorganization", "s_index", "delta_weight",
                "S-Index", "Market Share Change (p.p.)",
                "19_merger_reorg_sindex.png", add_lm = TRUE, y_fmt = fmt_pp)


## ===========================================================================
## Productivity-decomposition figures.
## Prototyped in diagnostics/proto_19_prod_figures.R (immigration) and
## diagnostics/proto_19_merger_figures.R (merger). Shared encodings across all
## panels: Reallocation = blue, Reorganization = orange; point area = the
## baseline market weight that 18_ aggregates with (avg_labor x CSPOP x share
## x weight); firm productivity in dollars of consumer-quality premium per
## labor hour (theta / |rho|, the 22_ unit convention).
## Outputs: 19_prod_reversal_dumbbell.png,
##          19_{immigration,merger}_between_contrib.png,
##          19_{immigration,merger}_within_firm.png,
##          19_{immigration,merger}_wage_response.png
## NOTE: the merger section above halves working_data$weight; everything below
## reads the saved 13-17 panels, so that mutation is irrelevant here.
## ===========================================================================
cf_log("Building productivity-decomposition figures")

COL_REALLOC <- "#377EB8"
COL_REORG   <- "#E69F00"
scen_fill <- scale_fill_manual(values = c(Reallocation = COL_REALLOC,
                                          Reorganization = COL_REORG))
prod_theme <- theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

fq_prod <- get_counterfactual_focus_quarter()
county_disp <- c(`17031` = "Cook", `36061` = "New York", `6037` = "Los Angeles")
n_wt <- CONFIG$n_worker_types
dedup_cols <- function(dt) dt[, !duplicated(names(dt)), with = FALSE]
read_prod_panel <- function(fname) {
  d <- dedup_cols(as.data.table(read_counterfactual_rds(
    fname, legacy_filenames = fname, description = fname)))
  if ("quarter_year" %in% names(d)) d <- d[quarter_year == fq_prod]
  d
}

## Immigration native fractions, mirroring 16_/18_: immigrants add 5% of
## county labor to the lowest-wage type; the native fraction downweights that
## type so labor_prod is measured on a native-labor basis.
tl13 <- as.data.table(read_counterfactual_rds(
  "13_total_labor.rds", legacy_filenames = "13_total_labor.rds",
  description = "baseline total labor"))[quarter_year == fq_prod]
tot_labor_cols <- counterfactual_tot_labor_field_names(CONFIG)
imm_targets <- counterfactual_lowest_wage_types(initial_wages)
nf_list <- lapply(names(imm_targets), function(cc) {
  base <- as.numeric(tl13[county == cc, ..tot_labor_cols])
  k <- imm_targets[[cc]]
  nf <- rep(1, n_wt)
  nf[k] <- base[k] / (base[k] + 0.05 * sum(base))
  nf
})
names(nf_list) <- names(imm_targets)

## Firm-level frame: tot_prod = sum of the (column-swept-theta-scaled) B
## fields, i.e. quality output per labor hour; mult = 18_'s aggregation
## weight; ns = native share (1 unless the immigration basis is requested).
prod_frame <- function(d, use_nf = FALSE) {
  d <- copy(d)
  d[, tot_prod := Reduce(`+`, .SD), .SDcols = b_field_names]
  d[, mult := avg_labor * CSPOP * new_share * weight]
  d[, ns := 1.0]
  if (use_nf) for (cc in names(nf_list)) {
    nfv <- nf_list[[cc]]
    d[as.character(county) == cc,
      ns := Reduce(`+`, lapply(seq_len(n_wt),
                               function(k) get(e_field_names[k]) * nfv[k]))]
  }
  d
}
county_prod <- function(d) {
  d[, .(labor_prod = sum(tot_prod * mult * ns) / sum(mult * ns)),
    by = .(county = as.character(county), sol_type)]
}

## ---- (a) all-county, all-counterfactual dumbbell ---------------------------
base_prod <- county_prod(prod_frame(read_prod_panel("13_prod_initial.rds")))
base_prod <- base_prod[, .(initial_prod = labor_prod[1]), by = county]

cf_specs <- list(
  list(cf = "Immigration",          file = "16_prod_immigration.rds", nf = TRUE),
  list(cf = "Incr. Concentration",  file = "17_prod_merger.rds",      nf = FALSE),
  list(cf = "Management Diffusion", file = "14_prod_diffusion.rds",   nf = FALSE),
  list(cf = "Sales Tax",            file = "15_prod_salestax.rds",    nf = FALSE))
dumb <- rbindlist(lapply(cf_specs, function(s) {
  cp <- county_prod(prod_frame(read_prod_panel(s$file), use_nf = s$nf))
  cp <- merge(cp, base_prod, by = "county")
  cp[, .(cf = s$cf, county, sol_type, dprod = labor_prod / initial_prod - 1)]
}))
dumb <- dcast(dumb, cf + county ~ sol_type, value.var = "dprod")
dumb[, county_name := county_disp[county]]
dumb[, variant := "main"]

## Optional robustness row: the LA immigration cell under the pre-91c48dd
## type-1 target, if the diagnostic probe output is present.
dumb_sub <- "Labor-weighted mean quality output per hour; arrow: reallocation-only → full reorganization"
probe_path <- "diagnostics/imm_la_target_probe_results.rds"
if (file.exists(probe_path)) {
  pb <- as.data.table(readRDS(probe_path))[scenario == "B_t1_5pctTotal"]
  if (nrow(pb) == 1) {
    dumb <- rbind(dumb, data.table(
      cf = "Immigration", county = "6037", realloc = pb$dprod_nat_rl,
      reorg = pb$dprod_nat_rg, county_name = "Los Angeles (type-1 target)",
      variant = "alt"))
    dumb_sub <- paste0(dumb_sub,
      "\nFaded: LA immigration under the alternative type-1 target")
  }
} else {
  cf_log("Probe rds absent; dumbbell omits the LA type-1 robustness row", "WARN")
}
dumb[, county_name := factor(county_name, levels = rev(c(
  "Cook", "New York", "Los Angeles", "Los Angeles (type-1 target)")))]
dumb[, cf := factor(cf, levels = c("Immigration", "Incr. Concentration",
                                   "Management Diffusion", "Sales Tax"))]
dlong <- melt(dumb, id.vars = c("cf", "county_name", "variant"),
              measure.vars = c("realloc", "reorg"),
              variable.name = "scenario", value.name = "dprod")
dlong[, scenario := fifelse(scenario == "realloc",
                            "Reallocation", "Reorganization")]

p_dumb <- ggplot(dumb, aes(y = county_name)) +
  geom_vline(xintercept = 0, linewidth = 0.6, color = "grey30") +
  geom_segment(aes(x = realloc, xend = reorg, yend = county_name,
                   alpha = variant),
               linewidth = 1.1, color = "grey45",
               arrow = arrow(length = unit(9, "pt"), type = "closed")) +
  geom_point(data = dlong, aes(x = dprod, fill = scenario, alpha = variant),
             size = 5.5, shape = 21, color = "white", stroke = 0.8) +
  facet_grid(cf ~ ., scales = "free_y", space = "free_y",
             labeller = label_wrap_gen(14)) +
  scale_alpha_manual(values = c(main = 1, alt = 0.45), guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(-0.02, 0.08, 0.02)) +
  scen_fill + prod_theme +
  theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
  labs(x = "Labor productivity change vs. baseline", y = NULL,
       title = "Reorganization reverses or amplifies the productivity impact",
       subtitle = dumb_sub)
save_immigration_figure("19_prod_reversal_dumbbell.png",
                        width = 12, height = 10, plot = p_dumb)

## ---- LA firm-level frames for the immigration and merger panels -----------
la_shock_frame <- function(cf_file, use_nf, e_col) {
  mk1 <- function(d, lab) {
    d <- d[as.character(county) == "6037"]
    cols <- c("location_id", "tot_prod", "s_index", "mult", "ns", "new_share",
              "newprice", "weight", e_col)
    o <- d[, ..cols]
    setnames(o, setdiff(cols, "location_id"),
             paste0(setdiff(cols, "location_id"), "_", lab))
    o
  }
  cfp <- prod_frame(read_prod_panel(cf_file), use_nf = use_nf)
  bd  <- prod_frame(read_prod_panel("13_prod_initial.rds"), use_nf = use_nf)
  f <- Reduce(function(x, y) merge(x, y, by = "location_id"), list(
    mk1(bd, "b"),
    mk1(cfp[sol_type == "realloc"], "rl"),
    mk1(cfp[sol_type == "reorg"],   "rg")))
  ## Weights: baseline on the full basis (no immigrants at baseline); the
  ## counterfactual weights carry the native-share basis when requested.
  f[, w0  := mult_b / sum(mult_b)]
  f[, wrl := (mult_rl * ns_rl) / sum(mult_rl * ns_rl)]
  f[, wrg := (mult_rg * ns_rg) / sum(mult_rg * ns_rg)]
  f
}

## ---- (b) between-firm contribution plots ----------------------------------
between_fig <- function(f, ttl, sub_shock, filename) {
  P0 <- f[, sum(tot_prod_b * w0)]
  rho_la <- abs(rho[["6037"]])
  cb <- rbind(
    f[, .(scenario = "Reallocation", p_dollar = tot_prod_b / rho_la, w0,
          contrib = 100 * (wrl - w0) * (tot_prod_b - P0) / P0)],
    f[, .(scenario = "Reorganization", p_dollar = tot_prod_b / rho_la, w0,
          contrib = 100 * (wrg - w0) * (tot_prod_b - P0) / P0)])
  sums <- cb[, .(tot = sum(contrib)), by = scenario]
  sums[, lab := sprintf("between-firm total: %+.1f pp", tot)]
  star_i <- f[, which.max(tot_prod_b)]
  star <- cb[scenario == "Reallocation"][star_i]
  loo <- f[-star_i]
  loo_rl <- loo[, sum(tot_prod_rl * wrl) / sum(wrl)] /
            loo[, sum(tot_prod_b * w0) / sum(w0)] - 1
  note_df <- data.table(
    scenario = "Reallocation", x = star$p_dollar * 0.93, y = star$contrib,
    lab = sprintf("one salon — drop it and reallocation is %+.1f%%",
                  100 * loo_rl))
  p <- ggplot(cb, aes(x = p_dollar, y = contrib)) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
    geom_point(aes(size = w0, fill = scenario), shape = 21,
               color = "white", stroke = 0.6, alpha = 0.95,
               show.legend = FALSE) +
    geom_text(data = sums, aes(label = lab), x = -Inf, y = Inf,
              hjust = -0.05, vjust = 1.6, size = 6.5, fontface = "bold") +
    geom_text(data = note_df, aes(x = x, y = y, label = lab),
              hjust = 1, size = 5.5, inherit.aes = FALSE) +
    facet_wrap(~scenario) +
    scale_x_log10(breaks = c(100, 200, 300, 500, 750),
                  labels = scales::dollar_format(accuracy = 1)) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
    scale_size_area(max_size = 15, guide = "none") +
    scen_fill + prod_theme +
    labs(x = "Baseline labor productivity ($ quality premium per hour, log scale)",
         y = "Contribution to aggregate\nproductivity change (pp)",
         title = ttl,
         subtitle = paste0(sub_shock,
           "\nPoint area = baseline market weight; contributions sum to the between-firm effect"))
  save_immigration_figure(filename, width = 14, height = 7.5, plot = p)
}

f_imm <- la_shock_frame("16_prod_immigration.rds", use_nf = TRUE, e_col = "E_4")
f_mrg <- la_shock_frame("17_prod_merger.rds", use_nf = FALSE, e_col = "E_1")

between_fig(f_imm,
  ttl = "Immigration (Los Angeles): who gains market weight?",
  sub_shock = "+5% of county labor supply added to the lowest-wage skill set (type 4, nail/spa)",
  filename = "19_immigration_between_contrib.png")
between_fig(f_mrg,
  ttl = "Merger (Los Angeles): the reallocation gain is one salon",
  sub_shock = "Half of salons exit; survivors absorb the county labor force",
  filename = "19_merger_between_contrib.png")

## ---- (c) within-firm panels ------------------------------------------------
within_fig <- function(f, e_from, e_to, x_lab, ttl, sub_shock, filename) {
  wn <- f[, .(dE = 100 * (get(e_to) - get(e_from)),
              dP = 100 * (tot_prod_rg - tot_prod_rl) /
                   sum(tot_prod_b * w0),
              dS = s_index_rg - s_index_rl,
              wbar = (wrl + wrg) / 2)]
  within_tot <- wn[, sum(wbar * dP)]
  p <- ggplot(wn, aes(x = dE, y = dP)) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
    geom_vline(xintercept = 0, linewidth = 0.4, color = "grey70",
               linetype = "dashed") +
    geom_point(aes(size = wbar, fill = dS), shape = 21, color = "grey35",
               stroke = 0.5) +
    scale_fill_gradient2(low = "#762A83", mid = "grey92", high = "#1B7837",
                         midpoint = 0, name = "Δ s-index") +
    scale_size_area(max_size = 15, guide = "none") +
    annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.6,
             size = 6.5, fontface = "bold",
             label = sprintf("labor-weighted within-firm total: %+.1f pp",
                             within_tot)) +
    prod_theme + theme(legend.position = "right",
                       legend.title = element_text(size = 18)) +
    labs(x = x_lab,
         y = "Within-firm productivity change\n(reorg − realloc, % of baseline)",
         title = ttl,
         subtitle = paste0(sub_shock,
           "\nPoint area = market weight; green = deeper specialization, purple = de-specialization"))
  save_immigration_figure(filename, width = 11.5, height = 7.5, plot = p)
}

within_fig(f_imm, e_from = "E_4_rl", e_to = "E_4_rg",
  x_lab = "Change in the low-wage (nail/spa) type's time share within the firm (pp)",
  ttl = "Immigration (Los Angeles): absorb the low-wage type, de-specialize",
  sub_shock = "+5% of county labor supply added to the lowest-wage skill set (type 4, nail/spa)",
  filename = "19_immigration_within_firm.png")
within_fig(f_mrg, e_from = "E_1_rl", e_to = "E_1_rg",
  x_lab = "Change in the type-1 (generalist) time share within the firm (pp)",
  ttl = "Merger (Los Angeles): shed the generalist type, re-specialize",
  sub_shock = "Half of salons exit; survivors absorb the county labor force",
  filename = "19_merger_within_firm.png")

## ---- (d) wage-response panels ----------------------------------------------
wage_fig <- function(wage_table, f, ttl, shock_line, filename,
                     shock_type = NULL) {
  wcols_ <- paste0("w", seq_len(n_wt))
  wb <- as.numeric(unlist(initial_wages[county == "6037" &
    quarter_year == fq_prod, ..wcols_], use.names = FALSE))
  wr <- as.numeric(wage_table[county == "6037" & quarter_year == fq_prod &
    sol_type == "realloc", ..wcols_])
  wg <- as.numeric(wage_table[county == "6037" & quarter_year == fq_prod &
    sol_type == "reorg", ..wcols_])
  wd <- rbind(
    data.table(type = seq_len(n_wt), scenario = "Reallocation",
               pct = (wr - wb) / wb),
    data.table(type = seq_len(n_wt), scenario = "Reorganization",
               pct = (wg - wb) / wb))
  wd[, type_lab := sprintf("Type %d\n($%.0f)", type, wb[type])]
  dp_rl <- f[, weighted.mean(newprice_rl / newprice_b - 1, w0)]
  dp_rg <- f[, weighted.mean(newprice_rg / newprice_b - 1, w0)]
  p <- ggplot(wd, aes(x = type_lab, y = pct, fill = scenario)) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
    geom_col(position = position_dodge(width = 0.72), width = 0.64) +
    geom_text(aes(label = scales::percent(pct, accuracy = 0.1),
                  vjust = ifelse(pct < 0, 1.35, -0.5)),
              position = position_dodge(width = 0.72), size = 5.2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.18, 0.12))) +
    scen_fill + prod_theme +
    labs(x = NULL, y = "Equilibrium wage change", title = ttl,
         subtitle = sprintf(
           "%s\nCustomer-weighted price change: %+.1f%% (realloc), %+.1f%% (reorg)",
           shock_line, 100 * dp_rl, 100 * dp_rg))
  if (!is.null(shock_type)) {
    p <- p + annotate("text", x = shock_type, y = min(wd$pct) - 0.022,
                      label = "↑ shocked type (lowest wage)",
                      size = 6.5, fontface = "bold")
  }
  save_immigration_figure(filename, width = 12, height = 7.5, plot = p)
}

wage_fig(wage_vect_immigration, f_imm,
  ttl = "Immigration (Los Angeles): frozen organizations force the wage adjustment",
  shock_line = "+5% of county labor supply added to the lowest-wage skill set (type 4, nail/spa)",
  filename = "19_immigration_wage_response.png",
  shock_type = imm_targets[["6037"]])
wage_fig(wage_vect_merger, f_mrg,
  ttl = "Merger (Los Angeles): frozen organizations crash the specialist wages",
  shock_line = "Half of salons exit; survivors absorb the county labor force",
  filename = "19_merger_wage_response.png")

cf_log("Productivity-decomposition figures complete")
