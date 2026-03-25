library(ggplot2)
library(svglite)
ppl$filename <- NA
fnams <- tolower(gsub("\\.+", "_", make.names(ppl$title)))
allfigs <- list.files(path = "images")
for(p in 1:nrow(ppl)){
  match_file <- grep(paste0("^", fnams[p], "\\."), allfigs, value = TRUE)
  if(length(match_file) > 1) stop("Error matching figures")
  if(length(match_file) == 1) ppl$filename[p] <- match_file
}

for(p in which(is.na(ppl$filename))){
  # p = 34

  success <- FALSE

  if(!is.na(ppl$picture[p])){
    sourcefile <- ppl$picture[p]
    fext <- tools::file_ext(sourcefile)
    if(nchar(fext) > 2){
      destfile <- paste0(fnams[p], ".", fext)
      dld <- try(download.file(sourcefile, file.path("images", destfile), method = "wget"))
      success <- isTRUE(dld == 0)
    }
  }

  if(!success){
    clr <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 1)
    g <- ggplot2::ggplot(data = NULL) + ggplot2::geom_point(ggplot2::aes(x = 0, y = 0), fill = colorspace::lighten(clr), color = colorspace::darken(clr), stroke = 4, alpha =.7, shape = 21, size = 50) + ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = gsub('\\b(\\pL)\\pL{2,}|.','\\1', ppl$title[p], perl = TRUE)), color = "white", size = 14) + ggplot2::theme_void() + ggplot2::theme(legend.position = "none")
    destfile <- paste0(fnams[p], ".svg")
    ggplot2::ggsave(file.path("images", ppl$filename[p]), g, device = "svg", width = 2, height = 2)
    }
  ppl$filename[p] <- destfile
}

