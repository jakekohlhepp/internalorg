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

save_immigration_figure <- function(filename, width, height, units = "in") {
  output_path <- save_counterfactual_plot(
    filename,
    legacy_filename = filename,
    width = width,
    height = height,
    units = units
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
  ggforce::geom_arc_bar(
    aes(
      x0 = x0,
      y0 = y0,
      r0 = 0,
      r = r,
      start = start,
      end = end,
      fill = slice
    ),
    data = make_pie_slices(data, x_col, y_col, fill_cols, radius),
    inherit.aes = FALSE
  )
}

## reallocation logic
## high immigrant firms tend to have higher coordination cost
ggplot(aes(x=log_gamma, y=`Initial Immigrant Skill Set` ), data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]) + 
  geom_point(size=2) +ylab("Initial Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  geom_smooth(method="lm", formula=y ~ x, se=FALSE, linewidth=1)+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_realloc_scatter.png",
  width=12, height=8, units="in"
)

## they reduce prices after immigration
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), sdelta_price:=scale(delta_price)]

m2 <- lm(delta_price ~ sdelta_price, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(log_gamma ~ slog_gamma, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
trans_x <- function(x)round(coef(m1)[1] + coef(m1)[2]*x,digits=1)
trans_y <- function(x) paste0(100*round(coef(m2)[1] + coef(m2)[2]*x,digits=1),"%")

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "slog_gamma", "sdelta_price", c("Initial Immigrant Skill Set", "Other Skill Set"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) +  geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ylab("Price Change")+xlab("Log Organization Cost (Gamma)")+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_realloc_price.png",
  width=14, height=12, units="in"
)


## this reduces market share and shifts labor towards high coord cost firms


imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), sdelta_share:=scale(delta_share)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(log_gamma ~ slog_gamma, data=imm_la[sol_type=="Reallocation"& is.finite(log_gamma)])
trans_x <- function(x)round(coef(m1)[1] + coef(m1)[2]*x,digits=2)
trans_y <- function(x) paste0(100*round(coef(m2)[1] + coef(m2)[2]*x,digits=1),"%")

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "slog_gamma", "sdelta_share", c("Initial Immigrant Skill Set", "Other Skill Set"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ylab("Market Share Change")+xlab("Log Organization Cost (Gamma)")+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_realloc_marketshare.png",
  width=14, height=12, units="in"
)

## these firms are less specialized
## this reduces aggregate labor productivity

imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reallocation" & is.finite(gamma_invert), ss_index:=scale(s_index)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reallocation"& is.finite(gamma_invert)])
m1 <- lm(s_index ~ ss_index, data=imm_la[sol_type=="Reallocation"& is.finite(log_gamma)])
trans_x <- function(x)round(coef(m1)[1] + coef(m1)[2]*x, digits=2)
trans_y <- function(x) paste0(100*round(coef(m2)[1] + coef(m2)[2]*x,digits=1),"%")

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], "ss_index", "sdelta_share", c("Initial Immigrant Skill Set", "Other Skill Set"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+geom_smooth(aes(x=ss_index, y=sdelta_share),data=imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)],method='lm', formula=y ~ x, se=FALSE, linewidth=1.5, color='red', linetype='dashed')+
  scale_fill_manual(values = pal_color)+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Market Share Change")+xlab("S-Index")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_realloc_sindex.png",
  width=14, height=12, units="in"
)

## reorg

ggplot(aes(x=log_gamma, y=Immigration ), data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]) + 
  geom_point(size=2) +ylab("Additional Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_reorg_scatter.png",
  width=12, height=8, units="in"
)


## prices
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_price:=scale(delta_price)]

m2 <- lm(delta_price ~ sdelta_price, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(log_gamma~ slog_gamma, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
trans_x <- function(x)round(coef(m1)[1] + coef(m1)[2]*x,1)
trans_y <- function(x) paste0(100*round(coef(m2)[1] + coef(m2)[2]*x,digits=2),"%")

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "slog_gamma", "sdelta_price", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Price Change")+xlab("Log Organization Cost (Gamma)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_reorg_price.png",
  width=14, height=12, units="in"
)


##market shares
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), slog_gamma:=scale(log_gamma)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_share:=scale(delta_share)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(log_gamma~ slog_gamma, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
trans_x <- function(x)round(coef(m1)[1] + coef(m1)[2]*x,1)
trans_y <- function(x) paste0(100*round(coef(m2)[1] + coef(m2)[2]*x,digits=2),"%")

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "slog_gamma", "sdelta_share", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed()+
  ylab('Market Share Change')+xlab("Log Organization Cost (Gamma)")+
  theme_bw()+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_reorg_marketshare.png",
  width=14, height=12, units="in"
)
  

# specialization

imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), ss_index:=scale(s_index)]
imm_la[sol_type=="Reorganization" & is.finite(gamma_invert), sdelta_share:=scale(delta_share)]

m2 <- lm(delta_share ~ sdelta_share, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
m1 <- lm(s_index~ ss_index, data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)])
trans_x <- function(x)round(coef(m1)[1] + coef(m1)[2]*x,digits=2)
trans_y <- function(x) paste0(100*round(coef(m2)[1] + coef(m2)[2]*x,digits=2),"%")

ggplot() +
  geom_immigration_pies(imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], "ss_index", "sdelta_share", c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) +  geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", linewidth=1)+
  coord_fixed() +theme_bw()+geom_smooth(aes(x=ss_index, y=sdelta_share),data=imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)],method='lm', formula=y ~ x, se=FALSE, linewidth=1.5, color='red', linetype='dashed')+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Market Share Change")+xlab("S-Index")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_immigration_figure(
  "19_reorg_sindex.png",
  width=14, height=12, units="in"
)
