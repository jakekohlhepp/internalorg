## display counterfactual effects for immigration

## compute counterfactuals
## Store only the essentials of the equilibrium:
### 1. Equilibrium wages (to get equilibrium again)
### 2. org Structures (because these require contraction to recover)
## 

## Compute the following objects
## 1. Consumer surplus
## 2. specialization (s-index)
## 3. Specialization (max fraction of time spent on one task)
## 4. sum of theta*B at each firm
## 5. marginal cost of each firm.

## Structure:
### First loop: wages
### Second loop: best-response pricing (contract on lambert's w). 
#### convergence not for sure. but if we end up converging, this is a best response to best responses.
### Third loop: best-response org structure. convergence guaranteed.
innertol<-1e-08
outertol<-1e-04

source("config.R")
source("utils/counterfactuals_core.R")

counterfactual_context <- load_counterfactual_context(
  extra_packages = c("ggplot2", "scatterpie", "kableExtra")
)
attach_counterfactual_context(counterfactual_context)

spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))



get_everything_reorg<-function(wage_guess, cnty, qy){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  new_theta<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
  w_mat<-matrix(wage_guess, ncol=5, nrow=5, byrow=FALSE)
  new_tild_theta<-w_mat+(rho[cnty])^(-1)*new_theta
  new_tild_theta<-sweep(new_tild_theta,2,apply(new_tild_theta,2,min))
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(a1, a2, a3, a4, a5,gamma){
    alpha<-c(a1, a2, a3, a4,a5)
    if (is.finite(gamma) & gamma>0){
      
      alpha<-c(a1, a2, a3, a4,a5)
      ## this function will return matrix given gamma
      A<-exp(-1/gamma*(new_tild_theta) )
      E<-rep(0.2, 5)
      A[A>=Inf]<-1e16
      A[A<=0]<-1e-16
      fxpt<-function(p){
        C<-colSums(t(A)*alpha/colSums(A*p))
        return(p*C)
      }
      #for (i in 1:1000000){
      # E_old<-E
      # E<-fxpt(E_old)
      #if (all(abs(E-E_old)<innertol)) break
      #}
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      
      B<-t(t(A)*alpha/colSums(A*E))*E
    } else if (gamma==0){
      # no frictions
      B<-matrix(0, ncol=5, nrow=5)
      for (col in 1:5){
        B[which.min(new_tild_theta[,col]),col]<-alpha[col]
      }
    } else{
      # max frictions
      B<-matrix(0, ncol=5, nrow=5)
      B[which.min(rowSums(t(t(new_tild_theta)*alpha ))),]<-alpha
    }
    E<-rowSums(B)
    B[abs(B)<1e-16]<-0
    Brel<-t(t(B/E)/alpha)
    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*sum(B*spec_log(Brel)),
                                     0)
    qendog<-sum(B*new_theta)
    return(list(c_endog=cendog,q_endog=qendog,s_index=sum(B*spec_log(Brel)),
                B_1_1=B[1,1], B_1_2=B[2,1], B_1_3=B[3,1], B_1_4=B[4,1], B_1_5=B[5,1],
                B_2_1=B[1,2], B_2_2=B[2,2], B_2_3=B[3,2], B_2_4=B[4,2], B_2_5=B[5,2],
                B_3_1=B[1,3], B_3_2=B[2,3], B_3_3=B[3,3], B_3_4=B[4,3], B_3_5=B[5,3],
                B_4_1=B[1,4], B_4_2=B[2,4], B_4_3=B[3,4], B_4_4=B[4,4], B_4_5=B[5,4],
                B_5_1=B[1,5], B_5_2=B[2,5], B_5_3=B[3,5], B_5_4=B[4,5], B_5_5=B[5,5]))
  })
  
  counter_res[, c("c_endog", "q_endog","s_index",
                  "B_1_1", "B_1_2", "B_1_3", "B_1_4", "B_1_5",
                  "B_2_1", "B_2_2", "B_2_3", "B_2_4", "B_2_5",
                  "B_3_1", "B_3_2", "B_3_3", "B_3_4", "B_3_5",
                  "B_4_1", "B_4_2", "B_4_3", "B_4_4", "B_4_5",
                  "B_5_1", "B_5_2", "B_5_3", "B_5_4", "B_5_5"):= (solve_org(task_mix_1, task_mix_2, task_mix_3, task_mix_4,task_mix_5,
                                                                            gamma_invert)),by=c("location_id")]
  counter_res[, Q:=q_endog*avg_labor+qual_exo]
  counter_res[, C:=c_endog*avg_labor+cost_exo]
  # do not allow negative costs.
  counter_res[C<0, C:=0]
  
  # be careful about sign
  # in original draft, rho is positive.
  # in new draft, rho is negative.
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<- -1/rho[cnty]+C-lambertW0(exp(-1+Q+rho[cnty]*C)/(1+sum(wgt*exp(Q+rho[cnty]*old_p))-exp(Q+rho[cnty]*old_p)) )/rho[cnty]
      if (all(abs(new_p-old_p)<outertol)) break
      old_p<-new_p
    }
    return(new_p)
  }
  
  
  counter_res[, newprice:=best_respond(cust_price, Q,C,weight)]
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  return(counter_res)
  
}

#### initial
orig_struct <- build_counterfactual_structure_snapshot(get_everything_reorg, initial_wages)

get_everything_realloc<-function(wage_guess, cnty, qy){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  new_theta<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
  w_mat<-matrix(wage_guess, ncol=5, nrow=5, byrow=FALSE)
  new_tild_theta<-w_mat+(rho[cnty])^(-1)*new_theta
  new_tild_theta<-sweep(new_tild_theta,2,apply(new_tild_theta,2,min))
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(loc,gamma){
    B<-matrix(as.matrix(orig_struct[[cnty]][location_id==loc,.SD, .SDcols=grep("^B_", colnames(orig_struct[[cnty]]))]), byrow=FALSE,nrow=5, ncol=5)
    E<-rowSums(B)
    Brel<-t(t(B/E)/colSums(B))
    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*sum(B*spec_log(Brel)),
                                     0)
    qendog<-sum(B*new_theta)
    return(list(c_endog=cendog,q_endog=qendog,s_index=sum(B*spec_log(Brel)),
                B_1_1=B[1,1], B_1_2=B[2,1], B_1_3=B[3,1], B_1_4=B[4,1], B_1_5=B[5,1],
                B_2_1=B[1,2], B_2_2=B[2,2], B_2_3=B[3,2], B_2_4=B[4,2], B_2_5=B[5,2],
                B_3_1=B[1,3], B_3_2=B[2,3], B_3_3=B[3,3], B_3_4=B[4,3], B_3_5=B[5,3],
                B_4_1=B[1,4], B_4_2=B[2,4], B_4_3=B[3,4], B_4_4=B[4,4], B_4_5=B[5,4],
                B_5_1=B[1,5], B_5_2=B[2,5], B_5_3=B[3,5], B_5_4=B[4,5], B_5_5=B[5,5]))
  })
  
  counter_res[, c("c_endog", "q_endog","s_index",
                  "B_1_1", "B_1_2", "B_1_3", "B_1_4", "B_1_5",
                  "B_2_1", "B_2_2", "B_2_3", "B_2_4", "B_2_5",
                  "B_3_1", "B_3_2", "B_3_3", "B_3_4", "B_3_5",
                  "B_4_1", "B_4_2", "B_4_3", "B_4_4", "B_4_5",
                  "B_5_1", "B_5_2", "B_5_3", "B_5_4", "B_5_5"):= (solve_org(location_id,
                                                                            gamma_invert)),by=c("location_id")]
  counter_res[, Q:=q_endog*avg_labor+qual_exo]
  counter_res[, C:=c_endog*avg_labor+cost_exo]
  # do not allow negative costs.
  counter_res[C<0, C:=0]
  
  # be careful about sign
  # in original draft, rho is positive.
  # in new draft, rho is negative.
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<- -1/rho[cnty]+C-lambertW0(exp(-1+Q+rho[cnty]*C)/(1+sum(wgt*exp(Q+rho[cnty]*old_p))-exp(Q+rho[cnty]*old_p)) )/rho[cnty]
      if (all(abs(new_p-old_p)<outertol)) break
      old_p<-new_p
    }
    return(new_p)
  }
  
  
  counter_res[, newprice:=best_respond(cust_price, Q,C,weight)]
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  return(counter_res)
  
}



## to get internal structures, solve using wage vector from each equilibrium - initial, realloc, reorg.
wage_vect_immigration<-read_counterfactual_rds(
  "05_04_wages_immigration.rds",
  legacy_filenames = "05_04_wages_immigration.rds",
  description = "immigration counterfactual wages"
)



## immigration realloc
realloc_struct <- build_counterfactual_structure_snapshot(
  get_everything_realloc,
  wage_vect_immigration,
  solution_type = "realloc"
)


## immigration reorg
reorg_struct <- build_counterfactual_structure_snapshot(
  get_everything_reorg,
  wage_vect_immigration,
  solution_type = "reorg"
)



## focus on los angeles county. in this county lowest wage worker is type 1. immigration increases supply by 10%
## only want specialization overall - x-axis, frac type 1 - color inside, marketshare - dot size, 

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

## reallocation logic
## high immigrant firms tend to have higher coordination cost
ggplot(aes(x=log_gamma, y=`Initial Immigrant Skill Set` ), data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]) + 
  geom_point(size=2) +ylab("Initial Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  scale_alpha_manual(values=c(1,0),guide="none")+geom_line(stat='smooth', method='lm', size=1)+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_realloc_scatter.png",
  legacy_filename = "05_07_realloc_scatter.png",
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
  geom_scatterpie(aes(x=slog_gamma, y=sdelta_price, r=0.22), data=imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], cols=c("Initial Immigrant Skill Set", "Other Skill Set"))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) +  geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", size=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ylab("Price Change")+xlab("Log Organization Cost (Gamma)")+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_realloc_price.png",
  legacy_filename = "05_07_realloc_price.png",
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
  geom_scatterpie(aes(x=slog_gamma, y=sdelta_share, r=0.22), data=imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], cols=c("Initial Immigrant Skill Set", "Other Skill Set"))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", size=1)+
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ylab("Market Share Change")+xlab("Log Organization Cost (Gamma)")+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_realloc_marketshare.png",
  legacy_filename = "05_07_realloc_marketshare.png",
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
  geom_scatterpie(aes(x=ss_index, y=sdelta_share, r=0.22), data=imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)], cols=c("Initial Immigrant Skill Set", "Other Skill Set"))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", size=1)+
  coord_fixed() +theme_bw()+geom_line(aes(x=ss_index, y=sdelta_share),data=imm_la[sol_type=="Reallocation" & is.finite(gamma_invert)],stat='smooth',method='lm', size=1.5, color='red', linetype='dashed')+
  scale_fill_manual(values = pal_color)+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Market Share Change")+xlab("S-Index")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_realloc_sindex.png",
  legacy_filename = "05_07_realloc_sindex.png",
  width=14, height=12, units="in"
)

## reorg

ggplot(aes(x=log_gamma, y=Immigration ), data=imm_la[sol_type=="Reorganization"& is.finite(gamma_invert)]) + 
  geom_point(size=2) +ylab("Additional Immigrant Skill Set")+xlab("Log Organization Cost (Gamma)")+
  scale_alpha_manual(values=c(1,0),guide="none")+theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_reorg_scatter.png",
  legacy_filename = "05_07_reorg_scatter.png",
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
  geom_scatterpie(aes(x=slog_gamma, y=sdelta_price, r=0.22), data=imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], cols=c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", size=1)+ 
  coord_fixed() +theme_bw()+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Price Change")+xlab("Log Organization Cost (Gamma)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_reorg_price.png",
  legacy_filename = "05_07_reorg_price.png",
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
  geom_scatterpie(aes(x=slog_gamma, y=sdelta_share, r=0.22), data=imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], cols=c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) + geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", size=1)+
  coord_fixed()+
  ylab('Market Share Change')+xlab("Log Organization Cost (Gamma)")+
  theme_bw()+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_reorg_marketshare.png",
  legacy_filename = "05_07_reorg_marketshare.png",
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
  geom_scatterpie(aes(x=ss_index, y=sdelta_share, r=0.22), data=imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)], cols=c("Initial Immigrant Skill Set","Other Skill Set", "Immigration"))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_x) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),labels = trans_y) +  geom_hline(yintercept= -coef(m2)[1]/coef(m2)[2], linetype="dashed", size=1)+
  coord_fixed() +theme_bw()+geom_line(aes(x=ss_index, y=sdelta_share),data=imm_la[sol_type=="Reorganization" & is.finite(gamma_invert)],stat='smooth',method='lm', size=1.5, color='red', linetype='dashed')+
  scale_fill_manual(values = pal_color)+ 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+ylab("Market Share Change")+xlab("S-Index")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),text = element_text(size = 36))
save_counterfactual_plot(
  "05_07_reorg_sindex.png",
  legacy_filename = "05_07_reorg_sindex.png",
  width=14, height=12, units="in"
)



