# MULTI-BIT (array) inputs, AKA buses

# These are just a bunch of `and` gates packaged together, working independently.
# Think of this as 16 `and` gates stacked on top of each other. On one side you
# have 16 input pins all aligned vertically, which is referred to as the `a` bus.
# So even though a regular `and` gate has 2 inputs (a, b) and 1 output, that doesn't
# mean we can't stack a whole bunch of `and` gates on top of each other, the
# first element of the stack being a_1, b_1, out_1. The vertically-aligned
# pins on one side are bus_a, the other the b bus.

# Another way to think of it: we get one input for an and gate from element i
# of the first bus, and the other input from element i of the second bus

# Example logic:
# |        a         |        b         |       out        |
# | 0011110011000011 | 0000111111110000 | 0000110011000000 |

# think of it like this (vertical stacking logic)
# |        a
# | 0011110011000011
# |        b
# | 0000111111110000
# |        out
# | 0000110011000000

# More examples:
# |        a         |        b         |       out        |
# | 0000000000000000 | 0000000000000000 | 0000000000000000 |
# | 0000000000000000 | 1111111111111111 | 0000000000000000 |
# | 1111111111111111 | 1111111111111111 | 1111111111111111 |
# | 1010101010101010 | 0101010101010101 | 0000000000000000 |
# | 0001001000110100 | 1001100001110110 | 0001000000110100 |

# HDL: And16.hdl
and_16 <- function(bus_a = logical(16), bus_b = logical(16)) {
  sapply(1:16, function(x) and(bus_a[x], bus_b[x]))
}

# HDL: Not16.hdl
not_16 <- function(bus_a = logical(16)) {
  sapply(1:16, function(x) not(bus_a[x]))
}

# HDL: Or16.hdl
or_16 <- function(bus_a = logical(16), bus_b = logical(16)) {
  sapply(1:16, function(x) or(bus_a[x], bus_b[x]))
}

# Multi-bit multiplexor examples:
# So for this, we're just imagining a bunch of multiplexors (16 of them). The
# inputs for those multiplexors come from a[i], b[i] on the a and b buses, and
# the selector comes from sel.
# |        a         |        b         |sel|       out        |
# | 1001100001110110 | 0000000000000000 | 0 | 1001100001110110 |

# | 0000000000000000 | 0000000000000000 | 0 | 0000000000000000 |
# | 0000000000000000 | 0000000000000000 | 1 | 0000000000000000 |
# | 0000000000000000 | 0001001000110100 | 0 | 0000000000000000 |
# | 0000000000000000 | 0001001000110100 | 1 | 0001001000110100 |
# | 1001100001110110 | 0000000000000000 | 1 | 0000000000000000 |
# | 1010101010101010 | 0101010101010101 | 0 | 1010101010101010 |
# | 1010101010101010 | 0101010101010101 | 1 | 0101010101010101 |
# HDL: Mux16.hdl
multiplex_16 <- function(bus_a = logical(16), bus_b = logical(16),
                         sel = logical(1)) {
  sapply(1:16, function(x) multiplex(bus_a[x], bus_b[x], sel))
}

# WAYs
# For ways, they're different from earlier gates in that multiple bits within
# an array of bits act together to define some logic. For the or_8way below,
# any can be true. For multiplexor_4way_16, we have the two selector bits create
# 4 different "ways," i.e., four different options for which bus to forward along

# HDL: Or8Way.hdl
or_8way <- function(bus_a = logical(8)) {
  a <- or(bus_a[1], bus_a[2])
  b <- or(bus_a[3], bus_a[4])
  c <- or(bus_a[5], bus_a[6])
  d <- or(bus_a[7], bus_a[8])

  x <- or(a, b)
  y <- or(c, d)

  or(x, y)
}

# multiplexor, 16 bit, 4 way.
# multiplexor, think "selector"
# so all this is is allowing us to select 4 different options (as spec'd by the
# 4 different sel combos), with each option referring to 16 bits (i.e., a bus).

# Before our multiplexor selected a single input bit to forward along. now we're
# asking it to forward an entire bus of independent (but chunked) bits.
# |        a         |        b         |        c         |        d         | sel  |       out        |
# | 0001001000110100 | 1001100001110110 | 1010101010101010 | 0101010101010101 |  00  | 0001001000110100 |

# | 0000000000000000 | 0000000000000000 | 0000000000000000 | 0000000000000000 |  00  | 0000000000000000 | A
# | 0000000000000000 | 0000000000000000 | 0000000000000000 | 0000000000000000 |  01  | 0000000000000000 | B
# | 0000000000000000 | 0000000000000000 | 0000000000000000 | 0000000000000000 |  10  | 0000000000000000 | C
# | 0000000000000000 | 0000000000000000 | 0000000000000000 | 0000000000000000 |  11  | 0000000000000000 | D
# | 0001001000110100 | 1001100001110110 | 1010101010101010 | 0101010101010101 |  01  | 1001100001110110 |
# | 0001001000110100 | 1001100001110110 | 1010101010101010 | 0101010101010101 |  10  | 1010101010101010 |
# | 0001001000110100 | 1001100001110110 | 1010101010101010 | 0101010101010101 |  11  | 0101010101010101 |

# use one selector (say, sel_1) to choose between a/b, c/d. use another selector
# (sel_2) to choose between resulting outputs.

# HDL: Mux4Way16.hdl
multiplexor_4way_16 <- function(bus_a = logical(16), bus_b = logical(16),
                                bus_c = logical(16), bus_d = logical(16),
                                sel = logical(2)) {
  a_or_b <- multiplex_16(bus_a, bus_b, sel[2])
  c_or_d <- multiplex_16(bus_c, bus_d, sel[2])
  multiplex_16(a_or_b, c_or_d, sel[1])
}

### 000, 001, 010, 011
### 100, 101, 110, 111
# HDL: Mux8Way16.hdl
multiplexor_8way_16 <- function(bus_a = logical(16), bus_b = logical(16),
                                bus_c = logical(16), bus_d = logical(16),
                                bus_e = logical(16), bus_f = logical(16),
                                bus_g = logical(16), bus_h = logical(16),
                                sel = logical(3)) {
  one <- multiplexor_4way_16(bus_a, bus_b, bus_c, bus_d, sel[2:3])
  two <- multiplexor_4way_16(bus_e, bus_f, bus_g, bus_h, sel[2:3])
  multiplex_16(one, two, sel[1])
}

# 4 different options for selector (2 bits), corresponding to whether in_ should
# be forwarded to each of the 4 different possible outputs
# HDL: DMux4Way.hdl
dmultiplex_4way <- function(in_, sel = logical(2)) {
  c(
    and(in_, and(not(sel[1]), not(sel[2]))),
    and(in_, and(not(sel[1]), sel[2])),
    and(in_, and(sel[1], not(sel[2]))),
    and(in_, and(sel[1], sel[2]))
  )
}

# Now with 8 different output pin options
# HDL: DMux8Way.hdl
dmultiplex_8way <- function(in_, sel = logical(3)) {
  x <- dmultiplex_4way(in_, sel[2:3])
  c(
    and(not(sel[1]), x[1]),
    and(not(sel[1]), x[2]),
    and(not(sel[1]), x[3]),
    and(not(sel[1]), x[4]),
    and(sel[1], x[1]),
    and(sel[1], x[2]),
    and(sel[1], x[3]),
    and(sel[1], x[4])
  )
}
