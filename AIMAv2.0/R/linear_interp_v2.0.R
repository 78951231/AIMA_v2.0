#' Interpolate all data columns to a specified length using linear interpolation.

linear_interp_v2.0 = function(data, data2) {
  spacing = nrow(data) / nrow(data2)
  newdata = data %>%
    map(function(x) approx(x = seq(1,nrow(data)),
                    xout = round(seq(1, nrow(data), spacing)),
                    y = x,
                    method = "linear")$y) %>%
    as.data.frame()
  # Check to make sure lengths are the same
  if (nrow(newdata) != nrow(data2)) {
    newdata = newdata[1:nrow(data2),]
  }
  return(newdata)
}
