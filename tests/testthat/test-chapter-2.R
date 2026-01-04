# ARITHMETIC CHIPS
################################################################################

test_that("half_adder works", {
  # Test with comparison file
  cmp_file <- read_cmp_file(here("tests/cmp-files/02/HalfAdder.cmp"))
  run_test <- function() {
    cmp_file <- pick(everything())
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    result <- half_adder(bs$a, bs$b)
    result$sum == bs$sum && result$carry == bs$car
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test())
  expect_true(all(tests$test_pass))

  # Also test with random inputs
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    result <- half_adder(a, b)
    expected_sum <- xor(a, b)
    expected_carry <- and(a, b)
    result$sum == expected_sum && result$carry == expected_carry
  })
})

test_that("full_adder works", {
  # Test with comparison file
  cmp_file <- read_cmp_file(here("tests/cmp-files/02/FullAdder.cmp"))
  run_test <- function() {
    cmp_file <- pick(everything())
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    result <- full_adder(bs$a, bs$b, bs$c)
    result$sum == bs$sum && result$carry == bs$carry
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test())
  expect_true(all(tests$test_pass))

  # Also test with random inputs
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    c <- rand_bool(1)
    result <- full_adder(a, b, c)
    # Full adder: sum = a XOR b XOR c, carry = (a AND b) OR ((a XOR b) AND c)
    expected_sum <- xor(xor(a, b), c)
    expected_carry <- or(and(a, b), and(xor(a, b), c))
    result$sum == expected_sum && result$carry == expected_carry
  })
})

test_that("add_16 works", {
  # Test with comparison file
  cmp_file <- read_cmp_file(here("tests/cmp-files/02/Add16.cmp"))
  run_test <- function(cmp_file) {
    cmp_file <- pick(everything())
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    output <- add_16(bs$a, bs$b)
    expected <- bs$out
    identical(output, expected)
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test(.))
  expect_true(all(tests$test_pass))

  # Also test with random inputs - test commutativity
  expect_all_true({
    bus_a <- rand_bool(16)
    bus_b <- rand_bool(16)
    # Addition should be commutative
    result1 <- add_16(bus_a, bus_b)
    result2 <- add_16(bus_b, bus_a)
    identical(result1, result2)
  })

  # Test that adding zero gives the same value
  expect_all_true({
    bus_a <- rand_bool(16)
    zero <- rep(FALSE, 16)
    result <- add_16(bus_a, zero)
    identical(result, bus_a)
  })
})

test_that("inc_16 works", {
  # Test with comparison file
  cmp_file <- read_cmp_file(here("tests/cmp-files/02/Inc16.cmp"))
  run_test <- function(cmp_file) {
    cmp_file <- pick(everything())
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    output <- inc_16(bs$in.)
    expected <- bs$out
    identical(output, expected)
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test(.))
  expect_true(all(tests$test_pass))

  # Also test that inc_16 is equivalent to add_16 with 1
  expect_all_true({
    bus_in <- rand_bool(16)
    output1 <- inc_16(bus_in)
    one <- c(rep(FALSE, 15), TRUE)
    output2 <- add_16(bus_in, one)
    identical(output1, output2)
  })
})

test_that("alu works with basic tests", {
  # Test with ALU-basic.cmp (note: this file doesn't have zr/ng columns)
  cmp_file <- read_cmp_file(here("tests/cmp-files/02/ALU-basic.cmp"))
  run_test <- function(cmp_file) {
    cmp_file <- pick(everything())
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    result <- alu(
      bs$x, bs$y,
      bs$zx, bs$nx, bs$zy, bs$ny,
      bs$f, bs$no
    )
    # ALU-basic.cmp only has output, not flags
    identical(result$out, bs$out)
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test(.))
  expect_true(all(tests$test_pass))
})

test_that("alu works with full tests", {
  # Test with ALU.cmp (more comprehensive)
  cmp_file <- read_cmp_file(here("tests/cmp-files/02/ALU.cmp"))
  run_test <- function(cmp_file) {
    cmp_file <- pick(everything())
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    result <- alu(
      bs$x, bs$y,
      bs$zx, bs$nx, bs$zy, bs$ny,
      bs$f, bs$no
    )
    identical(result$out, bs$out) &&
      result$zr == bs$zr &&
      result$ng == bs$ng
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test(.))
  expect_true(all(tests$test_pass))
})

# ADDITIONAL PROPERTY-BASED TESTS
################################################################################

test_that("half_adder sum and carry are mutually exclusive", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    result <- half_adder(a, b)
    # Sum and carry cannot both be true
    !(result$sum && result$carry)
  })
})

test_that("full_adder handles all carry scenarios", {
  # Test that full adder correctly handles carry propagation
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    c <- rand_bool(1)
    result <- full_adder(a, b, c)
    # Verify sum is correct: a XOR b XOR c
    expected_sum <- xor(xor(a, b), c)
    result$sum == expected_sum
  })
})

test_that("add_16 handles overflow correctly", {
  # Test adding maximum values
  max_val <- rep(TRUE, 16)
  one <- c(rep(FALSE, 15), TRUE)
  result <- add_16(max_val, one)
  # Should wrap around to all zeros (overflow)
  expected <- rep(FALSE, 16)
  expect_equal(result, expected)
})

test_that("add_16 is associative", {
  expect_all_true({
    a <- rand_bool(16)
    b <- rand_bool(16)
    c <- rand_bool(16)
    # (a + b) + c should equal a + (b + c) for addition
    ab <- add_16(a, b)
    ab_c <- add_16(ab, c)
    bc <- add_16(b, c)
    a_bc <- add_16(a, bc)
    identical(ab_c, a_bc)
  })
})

test_that("inc_16 increments by exactly one", {
  expect_all_true({
    bus_in <- rand_bool(16)
    output <- inc_16(bus_in)
    # Verify it's exactly one more (modulo 2^16)
    one <- c(rep(FALSE, 15), TRUE)
    expected <- add_16(bus_in, one)
    identical(output, expected)
  })
})

test_that("inc_16 of zero gives one", {
  zeros <- rep(FALSE, 16)
  one <- c(rep(FALSE, 15), TRUE)
  expect_equal(inc_16(zeros), one)
})

test_that("inc_16 of max value wraps to zero", {
  max_val <- rep(TRUE, 16)
  zeros <- rep(FALSE, 16)
  expect_equal(inc_16(max_val), zeros)
})
