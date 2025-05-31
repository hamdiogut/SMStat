pairedttestH <- function(A, B) {
  mylist <- list()
  difference<-(A-B)
  if(getPvalue(difference)>0.05){
    mylist[["Paired_t_test_p_val"]] <- t.test(difference,mu=0)$p.value

    mylist[["Paired_t_test_power"]]<-paired_ttest_power_check(A,B)$Power
    mylist[["Cohen_s"]]<-paired_ttest_power_check(A,B)$Cohen_s_d
    mylist[["Ek_ornek"]]<-paired_ttest_power_check(Weight,Weig)$ek_ornek
    if(t.test(difference,mu=0)$p.value>0.05){
      mylist[["Pt_test_yorum"]] <- "No difference, two are the same,paired t-test"
    } else {
      mylist[["Pt_test_yorum"]] <- "Two are not the same, paired t-test"
    }
  } else {
    mylist[["Paired_Wilk_test_p_val"]] <- wilcox.test(A,B,paired=TRUE)$p.value

    mylist[["Wilk_test_power"]] <- wilcoxon_power_check(A,B)$power_est
    mylist[["Wilk_test_etki_ebad"]] <- wilcoxon_power_check(A,B)$d_est
    mylist[["Wilk_test_ilave_ornek"]] <- wilcoxon_power_check(A,B)$required_n_if_power_low

    if(wilcox.test(A,B,paired=TRUE)$p.value>0.05){
      mylist[["Wi_test_yorum"]] <- "No difference, two are the same, Wilconox"
    } else {
      mylist[["Wi_test_yorum"]] <- "Two are not the same,wilconox"

    } }
  return(mylist)
}
