qrcode_make <- function (Labels, ErrCorr) 
{
  Xtxt <- gsub("_", "-", Labels)
  if (nchar(Xtxt) <= 1) {
    Xtxt <- paste0("\\s\\s", Xtxt)
    warning("Label is single character or blank. Padding with empty spaces.")
  }
  Xpng <- grid::rasterGrob(abs(qrcode::qr_code(paste0(Xtxt), 
                                                  ErrorCorrectionLevel = ErrCorr, dataOutput = TRUE, plotQRcode = FALSE, 
                                                  mask = 3) - 1), interpolate = FALSE)
  return(Xpng)
}