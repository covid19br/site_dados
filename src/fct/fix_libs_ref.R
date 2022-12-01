fix_libs_ref <- function(file, root) {
    content <- readLines(file)
    for (i in 1:length(content)) {
        nova <- gsub('src="libs', paste0('src="', root, 'libs'), content[i])
        nova <- gsub('href="libs', paste0('href="', root, 'libs'), nova)
        content[[i]] <- nova
    }
    writeLines(content, file)
}
