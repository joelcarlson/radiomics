.onAttach <- function(pkg) {
  txt <- paste("\n",
               pkg,": Functions for texture analysis of greyscale images.\n",
               "Enter ?calc_features to see available texture features.",
               sep="")
  packageStartupMessage(txt)
}