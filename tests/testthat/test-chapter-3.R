# Chapter 3: Sequential Logic
# Tests for sequential chips that maintain state over time

# Helper function to convert integer to 16-bit boolean vector
# Handles negative numbers using two's complement
int_to_bits <- function(x) {
  if (x < 0) {
    x <- 65536 + x
  }
  rev(as.logical(intToBits(as.integer(x))[1:16]))
}

# Helper function to convert address integer to boolean vector
int_to_address <- function(x, width) {
  rev(as.logical(intToBits(as.integer(x))[1:width]))
}

# BIT
################################################################################

test_that("bit initializes to FALSE", {
  b <- bit()
  expect_false(b$out())
})

test_that("bit stores value when load is true", {
  b <- bit()

  # Store TRUE
  b$tick_cycle(TRUE, TRUE)
  expect_true(b$out())

  # Value persists when load is FALSE
  b$tick_cycle(FALSE, FALSE)
  expect_true(b$out())

  # Store FALSE
  b$tick_cycle(FALSE, TRUE)
  expect_false(b$out())

  # Value persists
  b$tick_cycle(TRUE, FALSE)
  expect_false(b$out())
})

test_that("bit ignores input when load is false", {
  b <- bit()

  # Store TRUE
  b$tick_cycle(TRUE, TRUE)
  expect_true(b$out())

  # Try to change with load=FALSE (should be ignored)
  b$tick_cycle(FALSE, FALSE)
  expect_true(b$out())
})

# REGISTER
################################################################################

test_that("register initializes to all FALSE", {
  reg <- register()
  expect_equal(reg$out(), rep(FALSE, 16))
})

test_that("register stores 16-bit values", {
  reg <- register()

  # Store a value
  val <- int_to_bits(12345)
  reg$tick_cycle(val, TRUE)
  expect_equal(reg$out(), val)

  # Value persists when load=FALSE
  reg$tick_cycle(int_to_bits(0), FALSE)
  expect_equal(reg$out(), val)

  # Store new value
  val2 <- int_to_bits(-1000)
  reg$tick_cycle(val2, TRUE)
  expect_equal(reg$out(), val2)
})

test_that("register handles negative values correctly", {
  reg <- register()
  val <- int_to_bits(-32123)
  reg$tick_cycle(val, TRUE)
  expect_equal(reg$out(), val)
})

# RAM8
################################################################################

test_that("ram_8 stores values at different addresses", {
  ram <- ram_8()
  val1 <- int_to_bits(111)
  val2 <- int_to_bits(222)
  val3 <- int_to_bits(333)

  # Store at address 0
  ram$tick_cycle(val1, TRUE, int_to_address(0, 3))
  expect_equal(ram$out(int_to_address(0, 3)), val1)

  # Store at address 1
  ram$tick_cycle(val2, TRUE, int_to_address(1, 3))
  expect_equal(ram$out(int_to_address(1, 3)), val2)

  # Address 0 should still have val1
  expect_equal(ram$out(int_to_address(0, 3)), val1)

  # Store at address 7
  ram$tick_cycle(val3, TRUE, int_to_address(7, 3))
  expect_equal(ram$out(int_to_address(7, 3)), val3)
})

test_that("ram_8 ignores writes when load is false", {
  ram <- ram_8()
  val1 <- int_to_bits(100)
  val2 <- int_to_bits(200)
  addr <- int_to_address(3, 3)

  # Store val1
  ram$tick_cycle(val1, TRUE, addr)
  expect_equal(ram$out(addr), val1)

  # Try to write val2 with load=FALSE
  ram$tick_cycle(val2, FALSE, addr)
  expect_equal(ram$out(addr), val1)
})

# RAM64
################################################################################

test_that("ram_64 stores and retrieves values", {
  ram <- ram_64()
  val <- int_to_bits(5555)
  addr <- int_to_address(42, 6)

  ram$tick_cycle(val, TRUE, addr)
  expect_equal(ram$out(addr), val)
})

test_that("ram_64 stores at multiple addresses", {
  ram <- ram_64()
  val1 <- int_to_bits(1111)
  val2 <- int_to_bits(2222)

  ram$tick_cycle(val1, TRUE, int_to_address(0, 6))
  ram$tick_cycle(val2, TRUE, int_to_address(63, 6))

  expect_equal(ram$out(int_to_address(0, 6)), val1)
  expect_equal(ram$out(int_to_address(63, 6)), val2)
})

# RAM512, RAM4K, RAM16K - Property-based tests (cmp tests too slow)
################################################################################

test_that("ram_512 stores and retrieves values", {
  skip_on_cran()
  ram <- ram_512()
  val <- int_to_bits(5555)
  addr <- int_to_address(100, 9)

  ram$tick_cycle(val, TRUE, addr)
  expect_equal(ram$out(addr), val)
})

test_that("ram_4k stores and retrieves values", {
  skip_on_cran()
  ram <- ram_4k()
  val <- int_to_bits(7777)
  addr <- int_to_address(500, 12)

  ram$tick_cycle(val, TRUE, addr)
  expect_equal(ram$out(addr), val)
})

test_that("ram_16k stores and retrieves values", {
  skip_on_cran()
  ram <- ram_16k()
  val <- int_to_bits(9999)
  addr <- int_to_address(1000, 14)

  ram$tick_cycle(val, TRUE, addr)
  expect_equal(ram$out(addr), val)
})

# PROGRAM COUNTER (PC)
################################################################################

test_that("pc initializes to zero", {
  pc_chip <- pc()
  expect_equal(pc_chip$out(), int_to_bits(0))
})

test_that("pc increments when inc is true", {
  pc_chip <- pc()
  zeros <- int_to_bits(0)

  # Increment
  pc_chip$tick_cycle(zeros, FALSE, TRUE, FALSE)
  expect_equal(pc_chip$out(), int_to_bits(1))

  pc_chip$tick_cycle(zeros, FALSE, TRUE, FALSE)
  expect_equal(pc_chip$out(), int_to_bits(2))

  pc_chip$tick_cycle(zeros, FALSE, TRUE, FALSE)
  expect_equal(pc_chip$out(), int_to_bits(3))
})

test_that("pc resets to zero", {
  pc_chip <- pc()
  zeros <- int_to_bits(0)

  # Increment a few times
  pc_chip$tick_cycle(zeros, FALSE, TRUE, FALSE)
  pc_chip$tick_cycle(zeros, FALSE, TRUE, FALSE)
  expect_equal(pc_chip$out(), int_to_bits(2))

  # Reset
  pc_chip$tick_cycle(zeros, FALSE, FALSE, TRUE)
  expect_equal(pc_chip$out(), int_to_bits(0))
})

test_that("pc loads value when load is true", {
  pc_chip <- pc()
  val <- int_to_bits(100)

  pc_chip$tick_cycle(val, TRUE, FALSE, FALSE)
  expect_equal(pc_chip$out(), val)

  # Continue incrementing from loaded value
  pc_chip$tick_cycle(int_to_bits(0), FALSE, TRUE, FALSE)
  expect_equal(pc_chip$out(), int_to_bits(101))
})

test_that("pc reset has priority over load and inc", {
  pc_chip <- pc()
  val <- int_to_bits(100)

  # Load a value first
  pc_chip$tick_cycle(val, TRUE, FALSE, FALSE)
  expect_equal(pc_chip$out(), val)

  # Reset with load and inc also true
  pc_chip$tick_cycle(int_to_bits(999), TRUE, TRUE, TRUE)
  expect_equal(pc_chip$out(), int_to_bits(0))
})

test_that("pc load has priority over inc", {
  pc_chip <- pc()
  val <- int_to_bits(50)

  # Increment first
  pc_chip$tick_cycle(int_to_bits(0), FALSE, TRUE, FALSE)
  expect_equal(pc_chip$out(), int_to_bits(1))

  # Load with inc also true - load should take priority
  pc_chip$tick_cycle(val, TRUE, TRUE, FALSE)
  expect_equal(pc_chip$out(), val)
})

test_that("pc holds value when no control signals", {
  pc_chip <- pc()
  val <- int_to_bits(25)

  # Load a value
  pc_chip$tick_cycle(val, TRUE, FALSE, FALSE)
  expect_equal(pc_chip$out(), val)

  # No control signals - should hold
  pc_chip$tick_cycle(int_to_bits(999), FALSE, FALSE, FALSE)
  expect_equal(pc_chip$out(), val)
})

test_that("pc can increment many times", {
  pc_chip <- pc()

  for (i in 1:20) {
    pc_chip$tick_cycle(int_to_bits(0), FALSE, TRUE, FALSE)
    expect_equal(pc_chip$out(), int_to_bits(i))
  }
})
