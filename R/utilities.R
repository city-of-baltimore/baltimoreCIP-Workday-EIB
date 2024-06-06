reduce_seq_len <- function(.x, .f, ..., .init, .dir = c("forward", "backward")) {
  reduce(.x = seq_len(length(.x)), .f = .f, ..., .init = .init, .dir = .dir)
}
