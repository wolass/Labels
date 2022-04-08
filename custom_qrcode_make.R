custom_qrcode_make <- function (Labels, ErrCorr) 
{
  Xtxt <- gsub("_", "-", Labels)
  if (nchar(Xtxt) <= 1) {
    Xtxt <- paste0("\\s\\s", Xtxt)
    warning("Label is single character or blank. Padding with empty spaces.")
  }
  if(grepl(Xtxt,pattern = "- - -")){
    cat("three dashes")
    Xpng <- grid::rasterGrob(matrix(c(1,1),
                                    nrow =21,ncol=21), interpolate = FALSE)
  } else{
    cat("not empty label")
    Xpng <- grid::rasterGrob(abs(qrcode::qrcode_gen(paste0(Xtxt), 
                                                    ErrorCorrectionLevel = ErrCorr, 
                                                    dataOutput = TRUE, 
                                                    plotQRcode = FALSE, 
                                                    mask = 3
                                                 ) - 1), 
                             interpolate = FALSE)
    
  }
  return(Xpng)
}
