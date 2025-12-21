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
