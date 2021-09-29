custom_create_PDF_sub2 <- function (user = FALSE, Labels = NULL, name = "LabelsOut", type = "matrix", 
                                   ErrCorr = "H", Fsz = 12, Across = TRUE, ERows = 0, ECols = 0, 
                                   trunc = TRUE, numrow = 20, numcol = 4, page_width = 8.5, 
                                   page_height = 11, width_margin = 0.25, height_margin = 0.5, 
                                   label_width = NA, label_height = NA, x_space = 0, y_space = 0.5) 
{
  if (length(Labels) == 0) 
    stop("Labels do not exist. Please pass in Labels")
  if (class(Labels)[1] %in% c("character", "integer", "numeric", 
                              "factor")) {
    Labels <- Labels
  }
  else if (class(Labels)[1] == "data.frame") {
    if (any(tolower(names(Labels)) == "label")) {
      Labels <- Labels[, "label"]
    }
    else {
      warning("Cannot find a label column. Using first column as label input.")
      Labels <- Labels[, 1]
    }
  }
  else {
    stop("Label input not a vector or a data frame. Please check your input.")
  }
  if (all(vapply(c(numcol, numrow, Fsz, ERows, ECols, trunc, 
                   page_width, page_height, height_margin, width_margin, 
                   x_space, y_space), is.numeric, logical(1L))) != TRUE) {
    stop("One or more numerical parameters are not numeric")
  }
  labelLength <- max(nchar(paste(Labels)))
  if (x_space > 1 | x_space < 0) 
    stop("ERROR: x_space value out of bounds. Must be between 0 and 1")
  if (y_space < 0 | y_space > 1) 
    stop("ERROR: y_space value out of bounds. Must be between 0 and 1")
  on.exit(grDevices::graphics.off())
  if (user == TRUE) {
    name <- string_input("Please enter name for PDF output file: ")
    Fsz <- numeric_input("Please enter a font size: ", integer = FALSE)
    ErrCorr <- switch(fake_menu(c("L (up to 7% damage)", 
                                  "M (up to 15% damage)", "Q (up to 25% damage)", 
                                  "H (up to 30% damage)"), "Select an error correction level. "), 
                      "L", "M", "Q", "H")
    Advanced <- switch(fake_menu(c("Yes", "No"), "Edit advanced parameters?"), 
                       TRUE, FALSE)
    if (Advanced) {
      type <- switch(fake_menu(c("Matrix QR Code", "Linear"), 
                               "Type of Barcode: "), "matrix", "linear")
      Across <- switch(fake_menu(c("Yes", "No"), "Print labels across?"), 
                       TRUE, FALSE)
      trunc <- switch(fake_menu(c("Yes", "No"), "Truncate longer labels into multiple lines if necessary?"), 
                      TRUE, FALSE)
      ERows <- numeric_input("Number of rows to skip? (enter 0 for default): ")
      ECols <- numeric_input("Number of cols to skip? (enter 0 for default): ")
      numrow <- numeric_input("Number of rows per page: ")
      numcol <- numeric_input("Number of col per page: ")
      height_margin <- numeric_input("Please enter the height margin of page (in inches): ", 
                                     integer = FALSE)
      width_margin <- numeric_input("Please enter the width margin of page (in inches): ", 
                                    integer = FALSE)
      label_width <- numeric_input("Please enter the width of the label (in inches): ", 
                                   integer = FALSE)
      label_height <- numeric_input("Please enter the height of the label (in inches): ", 
                                    integer = FALSE)
      if (type == "matrix") {
        space <- switch(numeric_input("Change distances between QR code and text label?\n 1: Yes \n 2: No"), 
                        TRUE, FALSE)
      }
      x_space <- 0
      y_space <- 0.5
      if (space) {
        x_space <- numeric_input("Please enter a number between 0 and 1 for \n \n                                 horizontal distance between QR code and label: ", 
                                 integer = FALSE)
        while (x_space > 1) {
          noquote(print("Invalid input"))
          x_space <- numeric_input("Please enter a number between 0 and 1: ", 
                                   integer = FALSE)
        }
        y_space <- numeric_input("Please enter a distance between 0 and 1 for \n\n                                 vertical distance from bottom: ", 
                                 integer = FALSE)
        while (y_space > 1) {
          noquote(print("Invalid input"))
          numeric_input("Please enter a distance between 0 and 1: ", 
                        integer = FALSE)
        }
      }
    }
  }
  width_margin <- page_width - width_margin * 2
  height_margin <- page_height - height_margin * 2
  if (!is.numeric(label_width)) {
    label_width <- width_margin/numcol
  }
  if (!is.numeric(label_height)) {
    label_height <- height_margin/numrow
  }
  if (type == "linear" & label_width/labelLength < 0.03) 
    warning("Linear barcodes created will have bar width smaller than 0.03 inches. \n  Increase label width to make them readable by all scanners.")
  column_space <- (width_margin - label_width * numcol)/(numcol - 
                                                           1)
  row_space <- (height_margin - label_height * numrow)/(numrow - 
                                                          1)
  barcode_layout <- grid::grid.layout(numrow, numcol, widths = grid::unit(c(rep(label_width + 
                                                                                  column_space, numcol - 1), label_width), "in"), heights = grid::unit(c(rep(label_height + 
                                                                                                                                                               row_space, numrow - 1), label_height), "in"))
  if (type == "linear") {
    code_vp <- grid::viewport(x = grid::unit(0.05, "npc"), 
                              y = grid::unit(0.8, "npc"), width = grid::unit(0.9 * 
                                                                               label_width, "in"), height = grid::unit(0.8 * 
                                                                                                                         label_height, "in"), just = c("left", "top"))
    text_height <- ifelse(Fsz/72 > label_height * 0.3, label_height * 
                            0.3, Fsz/72)
    label_vp <- grid::viewport(x = grid::unit(0.5, "npc"), 
                               y = grid::unit(1, "npc"), width = grid::unit(1, 
                                                                            "npc"), height = grid::unit(text_height, "in"), 
                               just = c("centre", "top"))
    Fsz <- ifelse(Fsz/72 > label_height * 0.3, label_height * 
                    72 * 0.3, Fsz)
    label_plots <- sapply(as.character(Labels), code_128_make, 
                          USE.NAMES = TRUE, simplify = FALSE)
  }
  else if (type == "matrix") {
    code_vp <- grid::viewport(x = grid::unit(0.05, "npc"), 
                              y = grid::unit(0.8, "npc"), 
                              width = grid::unit(0.3 *label_width, "in"), 
                              height = grid::unit(0.6 * label_height, "in"), 
                              just = c("left", "top"))
    
    label_vp <- grid::viewport(x = grid::unit((0.4 + 0.6 * x_space) * label_width, "in"), 
                               y = grid::unit(y_space, "npc"), 
                               width = grid::unit(0.4, "npc"), 
                               height = grid::unit(0.8,"npc"), 
                               just = c("left", "center"))
    sub_vp <- grid::viewport(x = grid::unit(0.05,"npc"), 
                             y = grid::unit(0.6 *label_height, "in"),
                             width = grid::unit(0.3 *label_width, "in"), 
                             height = grid::unit(0.05 *label_height, "in"),
                             just = c("left", "top"))
    
    label_plots <- sapply(as.character(Labels), custom_qrcode_make, 
                          ErrCorr = ErrCorr, USE.NAMES = TRUE, simplify = FALSE)
  }
  else {
    stop("Barcode type must be linear or matrix")
  }
  if (Across) {
    positions <- expand.grid(x = 1:numcol, y = 1:numrow)
  }
  else {
    positions <- expand.grid(y = 1:numrow, x = 1:numcol)
  }
  duplication <- ceiling(length(Labels)/nrow(positions)) + 
    5
  label_positions <- do.call("rbind", replicate(duplication, 
                                                positions, simplify = FALSE))
  starting_pos_index <- min(which(label_positions$x == ECols + 
                                    1 & label_positions$y == ERows + 1))
  if (ECols > numcol | ERows > numrow) {
    warning("Number of rows/columns to skip greater than number of rows/columns on page. Labels will start in top left corner.")
    starting_pos_index <- 1
  }
  label_positions <- label_positions[seq(starting_pos_index, 
                                         starting_pos_index + length(Labels)), ]
  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, width = page_width, height = page_height, 
                 onefile = TRUE, family = "Courier")
  bc_vp = grid::viewport(layout = barcode_layout)
  grid::pushViewport(bc_vp)
  for (i in seq(1, length(label_plots))) {
    Xsplt <- names(label_plots[i])
    lab_pos <- label_positions[i, ]
    if (all(i != 1 & lab_pos == c(1, 1))) {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(width = grid::unit(page_width, 
                                                           "in"), height = grid::unit(page_height, "in")))
      grid::pushViewport(bc_vp)
    }
    if (trunc == TRUE) {
      if (nchar(Xsplt) > 15) {
        Xsplt <- paste0(substring(Xsplt, seq(1, nchar(Xsplt), 
                                             15), seq(15, nchar(Xsplt) + 15 - 1, 15)), 
                        collapse = "\n")
      }
    }
    grid::pushViewport(grid::viewport(layout.pos.row = lab_pos$y, 
                                      layout.pos.col = lab_pos$x))
    grid::pushViewport(code_vp)
    grid::grid.draw(label_plots[[i]])
    grid::popViewport()
    grid::pushViewport(label_vp)
    if (type == "linear") {
      grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    }
    grid::grid.text(label = Xsplt, gp = grid::gpar(fontsize = Fsz, 
                                                   lineheight = 0.8))
    grid::popViewport()
    grid::pushViewport(sub_vp)
    grid::grid.text(label = substring(Xsplt,7,13),
                    gp = grid::gpar(fontsize = Fsz,
                                    lineheight = 0.8))
    grid::popViewport(2)
    
  }
}
