df <- data.frame(
  a = letters[c(1,1:9)],
  b = 51:60
)

vlookup(c("a", "e", "c"), df, key = "a", value = "b")

vlookup(c("a", "e", "c"), df, key = "a", value = "b", add = TRUE)
