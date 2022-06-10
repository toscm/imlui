caterr <- function(..., end = "\n") {
  cat(..., end, file = stderr())
}
cat2e <- function(...) {
  toscutil::cat2(..., file = stderr())
}
cat0e <- function(...) {
  toscutil::cat0(..., file = stderr())
}
catfe <- function(...) {
  toscutil::catf(..., file = stderr())
}
catne <- function(...) {
  toscutil::catn(..., file = stderr())
}
cat00e <- function(...) {
  toscutil::cat0(..., file = stderr())
}
cat0ne <- function(...) {
  toscutil::cat0n(..., file = stderr())
}
catfne <- function(...) {
  toscutil::catfn(..., file = stderr())
}
catnne <- function(...) {
  toscutil::catnn(..., file = stderr())
}
catsne <- function(...) {
  toscutil::catsn(..., file = stderr())
}
catsse <- function(..., sep = " ", end = " ") {
  toscutil::cat2(..., sep = sep, end = end, file = stderr())
}
cat0se <- function(..., sep = "", end = " ") {
  toscutil::cat2(..., sep = sep, end = end, file = stderr())
}
logerr <- function(...) {
  cat0se(toscutil::now(), ":")
  caterr(...)
}
log2e <- function(...) {
  cat0se(toscutil::now(), ":")
  cat2e(...)
}
log0e <- function(...) {
  cat0se(toscutil::now(), ":")
  cat0e(...)
}
logfe <- function(...) {
  cat0se(toscutil::now(), ":")
  catfe(...)
}
logne <- function(...) {
  cat0se(toscutil::now(), ":")
  catne(...)
}
log00e <- function(...) {
  cat0se(toscutil::now(), ":")
  cat00e(...)
}
log0ne <- function(...) {
  cat0se(toscutil::now(), ":")
  cat0ne(...)
}
logfne <- function(...) {
  cat0se(toscutil::now(), ":")
  catfne(...)
}
lognne <- function(...) {
  cat0se(toscutil::now(), ":")
  catnne(...)
}
logsne <- function(...) {
  cat0se(toscutil::now(), ":")
  catsne(...)
}
logsse <- function(...) {
  cat0se(toscutil::now(), ":")
  catsse(...)
}
log0se <- function(...) {
  cat0se(toscutil::now(), ":")
  cat0se(...)
}
