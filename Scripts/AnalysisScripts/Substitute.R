# ============================================================
# Substitute.R —— 替代代码
#
# 用途：原代码依赖 memnet / f.utils 两个包（CRAN 上已不可得或不兼容
#       R 4.6.1），此处按其行为重写所需函数，供各复现脚本 source()。
#
# 用法：source("Scripts/AnalysisScripts/Substitute.R")
#
# ============================================================

# 按 weight 比例混合两种颜色（0 = 全取 col1，1 = 全取 col2）
# 对应 memnet:::cmix 的底层实现
colormixer <- function(col1, col2, weight, format = "hex") {
  a <- col2rgb(col1); b <- col2rgb(col2)
  n <- ncol(a)
  b <- b[, rep(seq_len(ncol(b)), length.out = n), drop = FALSE]
  w <- rep(weight, length.out = n)
  out <- a * (1 - w) + b * w
  rgb(out[1, ], out[2, ], out[3, ], maxColorValue = 255)
}

# 别名：作者代码写作 memnet:::cmix 或 memnet::cmix
cmix <- colormixer

# 加透明度（perc = 1 全不透明）
# 对应原 _helpers/helper.R 中的 colTrans
colTrans <- function(col, perc) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue = 255, alpha = perc * 255)
}

# 画光晕球：叠 10 个由大变小、由实到透的圆点
# 对应原 _helpers/helper.R 中的 globe
globe <- function(x, y, col, prc = 1, cex, min_cex = .01,
                  bias = c(0, 0), res = 10, sh = 0, sh_bias = c(0, 0)) {
  cexs <- seq(cex, cex * min_cex, length = res)
  cols <- colormixer(col, par()$bg, seq(prc, 0, length = res))
  xs <- seq(x, x + bias[1], length = res)
  ys <- seq(y, y + bias[2], length = res)
  if (sh != 0) points(xs[1] + sh_bias[1], ys[1] + sh_bias[2],
                      cex = cexs[1], col = colTrans(par()$bg, 1 - sh), pch = 16)
  for (i in 1:res) {
    points(xs[i], ys[i], cex = cexs[i], col = cols[i], pch = 16)
  }
}
