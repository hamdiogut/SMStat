ttestH <- function(A, B) {
  mylist <- list()

  safe_get_p <- function(x) {
    out <- tryCatch(getPvalue(x), error = function(e) NA_real_)
    formatC(out, format = "f", digits = 3)
  }

  compvar_formatted <- function(x, y) {
    val <- tryCatch(compVar(x, y), error = function(e) NA)
    if (is.logical(val) || is.na(val)) return(val)
    formatC(as.numeric(val), format = "f", digits = 3)
  }

  mylist[["AHam_pval"]] <- safe_get_p(A)
  mylist[["BHam_pval"]] <- safe_get_p(B)

  if (!is.na(getPvalue(A)) && !is.na(getPvalue(B)) && getPvalue(A) > 0.05 && getPvalue(B) > 0.05) {
    mylist[["VarTestHam_pval"]] <- compvar_formatted(A, B)
    mylist[["tTestHam_pval"]] <- tryCatch({
      formatC(t.test(A, B, var.equal = compVar(A, B))$p.value, format = "f", digits = 3)
      ph<-hesapla_power_ttest(A,B)
      mylist[["effect_size"]] <- ph$effect_size
      mylist[["magnitude"]] <- ph$magnitude
      mylist[["power"]] <- ph$power
      mylist[["gerekli_n"]] <- ph$gerekli_n
    }, error = function(e) "NA")
  } else {
    transdata <- testTransf(A, B)
    mylist[["transferred_data_frame"]] <- transdata
    mylist[["ATransferred_pval"]] <- safe_get_p(transdata[[1]])
    mylist[["BTransferred_pval"]] <- safe_get_p(transdata[[2]])

    if (!is.na(getPvalue(transdata[[1]])) && !is.na(getPvalue(transdata[[2]])) &&
        getPvalue(transdata[[1]]) > 0.05 && getPvalue(transdata[[2]]) > 0.05) {
      mylist[["Comp_var_transferred"]] <- compvar_formatted(transdata[[1]], transdata[[2]])
      mylist[["ttest_transferred_pval"]] <- tryCatch({
        formatC(t.test(transdata[[1]], transdata[[2]], var.equal = compVar(transdata[[1]], transdata[[2]]))$p.value,
                format = "f", digits = 3)
        ph<-hesapla_power_ttest(transdata[[1]],transdata[[2]])
        mylist[["effect_size"]] <- ph$effect_size
        mylist[["magnitude"]] <- ph$magnitude
        mylist[["power"]] <- ph$power
        mylist[["gerekli_n"]] <- ph$gerekli_n

      }, error = function(e) "NA")
    } else {
      mylist[["wilcox_test_pval"]] <- tryCatch({
        formatC(wilcox.test(A, B)$p.value, format = "f", digits = 3)
        ph<-hesapla_power_wilcox(A,B)
        mylist[["effect_size"]] <- ph$r_effect_size
        mylist[["magnitude"]] <- ph$magnitude
        mylist[["power"]] <- ph$power
        mylist[["gerekli_n"]] <- ph$gerekli_n
        mylist[["cohen_d_approx"]] <- ph$cohen_d_approx

      }, error = function(e) "NA")
    }
  }

  return(mylist)
}
