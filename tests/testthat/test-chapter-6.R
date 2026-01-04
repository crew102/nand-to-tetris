# Chapter 6: Assembler

test_that("Assembler handles Add.asm, Max.asm, Rect.asm", {
  add_expected <- readLines(here::here("tests/cmp-files/05/Add.hack"))
  max_expected <- readLines(here::here("tests/cmp-files/05/Max.hack"))
  rect_expected <- readLines(here::here("tests/cmp-files/05/Rect.hack"))

  add_actual <- assemble_file(here::here("tests/cmp-files/06/add/Add.asm"))
  max_actual <- assemble_file(here::here("tests/cmp-files/06/max/Max.asm"))
  rect_actual <- assemble_file(here::here("tests/cmp-files/06/rect/Rect.asm"))

  expect_equal(add_actual, add_expected)
  expect_equal(max_actual, max_expected)
  expect_equal(rect_actual, rect_expected)
})

test_that("Assembler allocates variables and resolves labels", {
  to_bits <- function(n) {
    paste0(rev(as.integer(intToBits(as.integer(n))[1:16])), collapse = "")
  }
  lines <- c(
    "@var",
    "D=1",
    "(LOOP)",
    "@var",
    "M=D",
    "@LOOP",
    "0;JMP"
  )
  codes <- assemble_lines(lines)
  expect_equal(codes[1], to_bits(16)) # first reference allocates at 16
  expect_equal(codes[3], to_bits(16)) # reuse same variable
  expect_equal(codes[5], to_bits(2)) # label refers to ROM address 2
})

# ADDITIONAL TESTS
################################################################################

test_that("Assembler handles numeric A-instructions", {
  to_bits <- function(n) {
    paste0(rev(as.integer(intToBits(as.integer(n))[1:16])), collapse = "")
  }
  lines <- c("@5", "@42", "@0")
  codes <- assemble_lines(lines)
  expect_equal(codes[1], to_bits(5))
  expect_equal(codes[2], to_bits(42))
  expect_equal(codes[3], to_bits(0))
})

test_that("Assembler handles C-instructions with different destinations", {
  to_bits <- function(n) {
    paste0(rev(as.integer(intToBits(as.integer(n))[1:16])), collapse = "")
  }
  # C-instruction format: 111a cccccc ddd jjj
  # D=1 should be: 111 0 000001 010 000
  lines <- c("D=1", "M=1", "A=1", "MD=1", "AM=1", "AD=1", "AMD=1")
  codes <- assemble_lines(lines)

  # All should start with 111 (instruction type)
  for (code in codes) {
    expect_equal(substr(code, 1, 3), "111")
  }
})

test_that("Assembler handles jump instructions", {
  to_bits <- function(n) {
    paste0(rev(as.integer(intToBits(as.integer(n))[1:16])), collapse = "")
  }
  # 0;JMP should have jump bits set
  lines <- c("0;JMP", "D;JGT", "D;JEQ", "D;JLT")
  codes <- assemble_lines(lines)

  # All should be C-instructions (start with 111)
  for (code in codes) {
    expect_equal(substr(code, 1, 3), "111")
  }
})

test_that("Assembler handles pre-defined symbols", {
  to_bits <- function(n) {
    paste0(rev(as.integer(intToBits(as.integer(n))[1:16])), collapse = "")
  }
  # Test some pre-defined symbols
  lines <- c("@R0", "@R1", "@SCREEN", "@KBD", "@SP", "@LCL", "@ARG", "@THIS", "@THAT")
  codes <- assemble_lines(lines)

  # R0 should be 0, R1 should be 1, etc.
  expect_equal(codes[1], to_bits(0))
  expect_equal(codes[2], to_bits(1))
  # SCREEN is 16384, KBD is 24576
  expect_equal(codes[3], to_bits(16384))
  expect_equal(codes[4], to_bits(24576))
})

test_that("Assembler handles variable allocation sequentially", {
  to_bits <- function(n) {
    paste0(rev(as.integer(intToBits(as.integer(n))[1:16])), collapse = "")
  }
  lines <- c("@var1", "@var2", "@var3")
  codes <- assemble_lines(lines)

  # Variables should be allocated starting at 16, 17, 18
  expect_equal(codes[1], to_bits(16))
  expect_equal(codes[2], to_bits(17))
  expect_equal(codes[3], to_bits(18))
})

test_that("Assembler reuses variable addresses", {
  to_bits <- function(n) {
    paste0(rev(as.integer(intToBits(as.integer(n))[1:16])), collapse = "")
  }
  lines <- c("@x", "D=1", "@y", "M=D", "@x", "M=D")
  codes <- assemble_lines(lines)

  # First @x should be 16, @y should be 17, second @x should reuse 16
  expect_equal(codes[1], to_bits(16))
  expect_equal(codes[3], to_bits(17))
  expect_equal(codes[5], to_bits(16))
})
