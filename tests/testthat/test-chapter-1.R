# SINGLE-BIT
################################################################################

test_that("nand works", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    expected <- !(a && b)
    nand(a, b) == expected
  })
})

test_that("not works", {
  expect_all_true({
    a <- rand_bool(1)
    expected <- !a
    not(a) == expected
  })
})

test_that("and works", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    expected <- a && b
    and(a, b) == expected
  })
})

test_that("or works", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    expected <- a || b
    or(a, b) == expected
  })
})

test_that("xor works", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    expected <- (a || b) && !(a && b)
    xor(a, b) == expected
  })
})

test_that("multiplex works", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    sel <- rand_bool(1)
    expected <- if (sel) b else a
    multiplex(a, b, sel) == expected
  })
})

test_that("dmultiplex works", {
  expect_all_true({
    in_ <- rand_bool(1)
    sel <- rand_bool(1)
    expected <- c(!sel && in_, sel && in_)
    identical(dmultiplex(in_, sel), expected)
  })
})

# MULTI-BIT
################################################################################

test_that("and_16 works", {
  expect_all_true({
    bus_a <- rand_bool(16)
    bus_b <- rand_bool(16)
    expected <- bus_a & bus_b
    identical(and_16(bus_a, bus_b), expected)
  })
})

test_that("not_16 works", {
  expect_all_true({
    bus_a <- rand_bool(16)
    expected <- !bus_a
    identical(not_16(bus_a), expected)
  })
})

test_that("or_16 works", {
  expect_all_true({
    bus_a <- rand_bool(16)
    bus_b <- rand_bool(16)
    expected <- bus_a | bus_b
    identical(or_16(bus_a, bus_b), expected)
  })
})

test_that("multiplex_16 works", {
  expect_all_true({
    bus_a <- rand_bool(16)
    bus_b <- rand_bool(16)
    sel <- rand_bool(1)
    expected <- if (sel) bus_b else bus_a
    identical(multiplex_16(bus_a, bus_b, sel), expected)
  })
})

# MULTI-WAY
################################################################################

test_that("or_8way works", {
  expect_all_true({
    bus_a <- rand_bool(8)
    expected <- any(bus_a)
    or_8way(bus_a) == expected
  })

  expect_false(or_8way(rep(FALSE, 8)))
})

# MULTI-BIT and MULTI-WAY
################################################################################

test_that("multiplexor_4way_16 works", {
  expect_all_true({
    bus_a <- rand_bool(16)
    bus_b <- rand_bool(16)
    bus_c <- rand_bool(16)
    bus_d <- rand_bool(16)
    sel <- rand_bool(2)

    # If sel1 is on, we're def talking about second two bits
    group <- if (sel[1]) list(bus_c, bus_d) else list(bus_a, bus_b)
    # Now just choose the bus that corresponds to the sel2 bit
    expected <- if (sel[2]) group[[2]] else group[[1]]
    output <- multiplexor_4way_16(
      bus_a, bus_b, bus_c, bus_d,
      sel
    )
    identical(output, expected)
  })

  # Another check, in case I didn't mimic the logic correctly in above test
  cmp_file <- read_cmp_file(here("tests/cmp-files/01/Mux4Way16.cmp"))
  run_test <- function(cmp_file) {
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    output <- multiplexor_4way_16(bs$a, bs$b, bs$c, bs$d, bs$sel)
    expected <- bs$out
    identical(output, expected)
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test(.))
  expect_true(all(tests$test_pass))
})

test_that("multiplexor_8way_16 works", {
  expect_all_true({
    bus_a <- rand_bool(16)
    bus_b <- rand_bool(16)
    bus_c <- rand_bool(16)
    bus_d <- rand_bool(16)
    bus_e <- rand_bool(16)
    bus_f <- rand_bool(16)
    bus_g <- rand_bool(16)
    bus_h <- rand_bool(16)

    sel <- rand_bool(3)

    group <- if (sel[1]) {
      list(bus_e, bus_f, bus_g, bus_h)
    } else {
      list(bus_a, bus_b, bus_c, bus_d)
    }
    subgroup <- if (sel[2]) {
      list(group[[3]], group[[4]])
    } else {
      list(group[[1]], group[[2]])
    }
    expected <- if (sel[3]) subgroup[[2]] else subgroup[[1]]

    identical(
      multiplexor_8way_16(
        bus_a, bus_b, bus_c, bus_d,
        bus_e, bus_f, bus_g, bus_h,
        sel
      ),
      expected
    )
  })

  # Another check, in case I didn't mimic the logic correctly in above test
  cmp_file <- read_cmp_file(here("tests/cmp-files/01/Mux8Way16.cmp"))
  run_test <- function(cmp_file) {
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    output <- multiplexor_8way_16(
      bs$a, bs$b, bs$c, bs$d,
      bs$e, bs$f, bs$g, bs$h,
      bs$sel
    )
    expected <- bs$out
    identical(output, expected)
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test(.))
  expect_true(all(tests$test_pass))
})

test_that("dmultiplex_4way works", {
  expect_all_true({
    in_ <- rand_bool(1)
    sel <- rand_bool(2)
    # Clever way ChatGPT figured out how to determine index from two bits (bools)
    idx <- 1 + 2 * as.integer(sel[1]) + as.integer(sel[2])
    expected <- logical(4)
    expected[idx] <- in_
    identical(
      dmultiplex_4way(in_, sel),
      expected
    )
  })

  cmp_file <- read_cmp_file(here("tests/cmp-files/01/DMux4Way.cmp"))
  run_test <- function() {
    cmp_file <- pick(everything()) # Get current row, not entire data frame
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    # TODO: Consider refactoring so we're consistent in how we match the API
    # spec'd in the book re: outputs being buses of bits (vectors) or named
    # outputs
    identical(
      dmultiplex_4way(bs$in., bs$sel),
      c(bs$a, bs$b, bs$c, bs$d)
    )
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test())
  expect_true(all(tests$test_pass))
})

test_that("dmultiplex_8way works", {
  expect_all_true({
    in_ <- rand_bool(1)
    sel <- rand_bool(3)
    idx <- 1 + 4 * as.integer(sel[1]) + 2 * as.integer(sel[2]) + as.integer(sel[3])
    expected <- logical(8)
    expected[idx] <- in_
    identical(dmultiplex_8way(in_, sel), expected)
  })

  cmp_file <- read_cmp_file(here("tests/cmp-files/01/DMux8Way.cmp"))
  run_test <- function() {
    cmp_file <- pick(everything()) # Get current row, not entire data frame
    bs <- sapply(cmp_file, str_to_bool, USE.NAMES = TRUE, simplify = FALSE)
    identical(
      dmultiplex_8way(bs$in., bs$sel),
      c(bs$a, bs$b, bs$c, bs$d, bs$e, bs$f, bs$g, bs$h)
    )
  }
  tests <- cmp_file %>%
    rowwise() %>%
    mutate(test_pass = run_test())
  expect_true(all(tests$test_pass))
})

# ADDITIONAL PROPERTY-BASED TESTS
################################################################################

test_that("nand is commutative", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    nand(a, b) == nand(b, a)
  })
})

test_that("nand with same input gives not", {
  expect_all_true({
    a <- rand_bool(1)
    nand(a, a) == not(a)
  })
})

test_that("and is commutative", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    and(a, b) == and(b, a)
  })
})

test_that("or is commutative", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    or(a, b) == or(b, a)
  })
})

test_that("xor is commutative", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    xor(a, b) == xor(b, a)
  })
})

test_that("xor with same input gives false", {
  expect_all_true({
    a <- rand_bool(1)
    xor(a, a) == FALSE
  })
})

test_that("multiplex with sel=0 returns first input", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    multiplex(a, b, FALSE) == a
  })
})

test_that("multiplex with sel=1 returns second input", {
  expect_all_true({
    a <- rand_bool(1)
    b <- rand_bool(1)
    multiplex(a, b, TRUE) == b
  })
})

test_that("dmultiplex with in=0 gives all zeros", {
  expect_all_true({
    sel <- rand_bool(1)
    result <- dmultiplex(FALSE, sel)
    identical(result, c(FALSE, FALSE))
  })
})

test_that("and_16 with all zeros gives all zeros", {
  zeros <- rep(FALSE, 16)
  ones <- rep(TRUE, 16)
  expect_equal(and_16(zeros, zeros), zeros)
  expect_equal(and_16(zeros, ones), zeros)
  expect_equal(and_16(ones, zeros), zeros)
})

test_that("and_16 with all ones gives all ones", {
  ones <- rep(TRUE, 16)
  expect_equal(and_16(ones, ones), ones)
})

test_that("or_16 with all zeros gives all zeros", {
  zeros <- rep(FALSE, 16)
  expect_equal(or_16(zeros, zeros), zeros)
})

test_that("or_16 with all ones gives all ones", {
  ones <- rep(TRUE, 16)
  zeros <- rep(FALSE, 16)
  expect_equal(or_16(ones, zeros), ones)
  expect_equal(or_16(zeros, ones), ones)
  expect_equal(or_16(ones, ones), ones)
})

test_that("not_16 double negation is identity", {
  expect_all_true({
    bus <- rand_bool(16)
    identical(not_16(not_16(bus)), bus)
  })
})

test_that("multiplex_16 is idempotent with same inputs", {
  expect_all_true({
    bus <- rand_bool(16)
    sel <- rand_bool(1)
    result1 <- multiplex_16(bus, bus, sel)
    identical(result1, bus)
  })
})

test_that("or_8way with all zeros gives false", {
  zeros <- rep(FALSE, 8)
  expect_false(or_8way(zeros))
})

test_that("or_8way with any one gives true", {
  for (i in 1:8) {
    bus <- rep(FALSE, 8)
    bus[i] <- TRUE
    expect_true(or_8way(bus))
  }
})

test_that("or_8way with all ones gives true", {
  ones <- rep(TRUE, 8)
  expect_true(or_8way(ones))
})
