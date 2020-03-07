# CI to eta2
if (requireNamespace("MBESS", quietly = TRUE)) {
  cbind(MSBMisc = F_to_PVE_CI(4, 3, 123)[-2],
        MBESS = unlist(MBESS::ci.pvaf(
          4, 3, 123, N = 127, conf.level = 0.9
        ))[c(1, 3)])
}

# CI for d
if (requireNamespace("MBESS", quietly = TRUE)) {
  cbind(MSBMisc = t_to_d_CI(4, 68)[-2],
        MBESS = unlist(MBESS::ci.smd(4, n.1 = 40, n.2 = 30))[-2])
}

# CI for r
res <- cor.test(iris[[1]], iris[[2]])
cbind(MSBMisc = t_to_r_CI(res$statistic, res$parameter)[-2],
      stats = res$conf.int)
