# Chapter 5: CPU, Memory, Computer

test_that("CPU loads A on A-instruction and increments PC", {
  to_bits <- function(x) {
    rev(as.logical(intToBits(as.integer(x))[1:16]))
  }
  cpu_chip <- cpu()
  instr_a5 <- c(FALSE, to_bits(5)[2:16]) # MSB 0, value 5
  cpu_chip$tick_cycle(inM = rep(FALSE, 16), instruction = instr_a5, reset = FALSE)
  expect_equal(cpu_chip$out()$pc, to_bits(1)) # PC incremented to 1
  # After ticking, A should hold 5
  expect_equal(cpu_chip$out()$addressM, c(FALSE, to_bits(5)[2:16]))
})

test_that("CPU executes D=D+1 and writes to M when dest M set", {
  to_bits <- function(x) rev(as.logical(intToBits(as.integer(x))[1:16]))
  cpu_chip <- cpu()
  # A-instruction @0
  instr_a0 <- c(FALSE, to_bits(0)[2:16])
  cpu_chip$tick_cycle(rep(FALSE, 16), instr_a0, FALSE)
  # C-instruction: D=D+1 (a=0, comp=011111, dest: D only)
  instr_c_d_inc <- c(
    TRUE, TRUE, TRUE, FALSE, # 111a
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, # c1..c6 = 0 1 1 1 1 1
    FALSE, TRUE, FALSE, # dest = 010 (D)
    FALSE, FALSE, FALSE # jump = 000
  )
  cpu_chip$tick_cycle(rep(FALSE, 16), instr_c_d_inc, FALSE)
  # C-instruction: M=D (a=0, comp=D = 001100, dest M)
  instr_c_m_eq_d <- c(
    TRUE, TRUE, TRUE, FALSE,
    FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, # D
    FALSE, FALSE, TRUE, # dest M
    FALSE, FALSE, FALSE
  )
  cpu_chip$tick_cycle(rep(FALSE, 16), instr_c_m_eq_d, FALSE)
  expect_true(cpu_chip$out()$writeM)
  expect_equal(cpu_chip$out()$outM, to_bits(1))
})

test_that("Memory routes RAM, Screen, and Keyboard", {
  to_bits <- function(x) rev(as.logical(intToBits(as.integer(x))[1:16]))
  mem <- memory()
  zeros15 <- rep(FALSE, 15)
  ones16 <- to_bits(1)

  # Write to RAM address 0
  mem$tick_cycle(in_ = ones16, load = TRUE, address = zeros15, keyboard = rep(FALSE, 16))
  expect_equal(mem$out(zeros15, rep(FALSE, 16)), ones16)

  # Write to Screen address 0 (address prefix 10)
  screen_addr0 <- c(TRUE, FALSE, rep(FALSE, 13))
  mem$tick_cycle(in_ = to_bits(3), load = TRUE, address = screen_addr0, keyboard = rep(FALSE, 16))
  expect_equal(mem$out(screen_addr0, rep(FALSE, 16)), to_bits(3))

  # Keyboard read (address prefix 11)
  kb_addr <- c(TRUE, TRUE, rep(FALSE, 13))
  kb_val <- to_bits(42)
  expect_equal(mem$out(kb_addr, kb_val), kb_val)
})

# ADDITIONAL TESTS
################################################################################

test_that("Memory preserves values when load is false", {
  to_bits <- function(x) rev(as.logical(intToBits(as.integer(x))[1:16]))
  mem <- memory()
  zeros15 <- rep(FALSE, 15)
  val1 <- to_bits(123)
  val2 <- to_bits(456)

  # Write value 1
  mem$tick_cycle(in_ = val1, load = TRUE, address = zeros15, keyboard = rep(FALSE, 16))
  expect_equal(mem$out(zeros15, rep(FALSE, 16)), val1)

  # Try to write value 2 with load = FALSE
  mem$tick_cycle(in_ = val2, load = FALSE, address = zeros15, keyboard = rep(FALSE, 16))
  # Should still have value 1
  expect_equal(mem$out(zeros15, rep(FALSE, 16)), val1)
})

test_that("Memory handles multiple RAM addresses independently", {
  to_bits <- function(x) rev(as.logical(intToBits(as.integer(x))[1:16]))
  mem <- memory()

  addr0 <- rep(FALSE, 15)
  addr1 <- c(rep(FALSE, 14), TRUE)
  val0 <- to_bits(100)
  val1 <- to_bits(200)

  # Write to address 0
  mem$tick_cycle(in_ = val0, load = TRUE, address = addr0, keyboard = rep(FALSE, 16))
  # Write to address 1
  mem$tick_cycle(in_ = val1, load = TRUE, address = addr1, keyboard = rep(FALSE, 16))

  # Both addresses should have their respective values
  expect_equal(mem$out(addr0, rep(FALSE, 16)), val0)
  expect_equal(mem$out(addr1, rep(FALSE, 16)), val1)
})

test_that("CPU reset sets PC to zero", {
  to_bits <- function(x) rev(as.logical(intToBits(as.integer(x))[1:16]))
  cpu_chip <- cpu()

  # Execute some instructions to move PC
  instr_a5 <- c(FALSE, to_bits(5)[2:16])
  cpu_chip$tick_cycle(inM = rep(FALSE, 16), instruction = instr_a5, reset = FALSE)
  expect_equal(cpu_chip$out()$pc, to_bits(1))

  # Reset
  cpu_chip$tick_cycle(inM = rep(FALSE, 16), instruction = rep(FALSE, 16), reset = TRUE)
  expect_equal(cpu_chip$out()$pc, to_bits(0))
})

test_that("CPU A-instruction sets addressM correctly", {
  to_bits <- function(x) rev(as.logical(intToBits(as.integer(x))[1:16]))
  cpu_chip <- cpu()

  # A-instruction @42
  instr_a42 <- c(FALSE, to_bits(42)[2:16])
  cpu_chip$tick_cycle(inM = rep(FALSE, 16), instruction = instr_a42, reset = FALSE)

  # addressM should be the 15-bit value (MSB is always 0 for A-instruction)
  expect_equal(cpu_chip$out()$addressM, c(FALSE, to_bits(42)[2:16]))
})
