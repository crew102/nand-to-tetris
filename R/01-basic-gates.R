# ---- Intro ----
# Reminder what I'm doing below. I'm assuming I can use as many
# physical chips that behave like nand in the invariant.R file. I'm also assuming
# that I get electricity into some input pins (a, a and b, a, b, sel, etc.). I
# want to arrange the nand gates such that I can logically think about how electricity
# would move through the system if it was turned on for the input pins. I
# use arrangements of nand gates as building blocks to create more chips.
# Everything is just a combination of input electricity and nand gates.

# HDL: Not.hdl
not <- function(a = logical(1)) {
  nand(a, a)
}

# HDL: And.hdl
and <- function(a = logical(1), b = logical(1)) {
  not(nand(a, b))
}

# ! F F
# HDL: Or.hdl
or <- function(a = logical(1), b = logical(1)) {
  not(and(not(a), not(b)))
}

# xor is T when values are opposite, F otherwise
# TT or FF -> F
# So let's `not` the arrangement created above for either case (both being the same)
# HDL: Xor.hdl
xor <- function(a = logical(1), b = logical(1)) {
  both_ts <- and(a, b)
  both_fs <- and(not(a), not(b))
  ts_or_fs <- or(both_ts, both_fs)
  not(ts_or_fs)
}

# When sel is F, provide a's value. When sel is T, provide b's value.
# Think about it this way, how can we make sure we generate a T in the 
# arrangements we need to (indicated by XXXX below) without leaking T's into 
# arrangements that should be F.

# | s | a | b |   Y |
# |---|---|---|  ---|
# | 0 | 0 | 0 |   0 |
# | 0 | 0 | 1 |   0 |
# | 0 | 1 | 0 |   1 | XXXX
# | 0 | 1 | 1 |   1 | XXXX
# | 1 | 0 | 0 |   0 |
# | 1 | 0 | 1 |   1 | XXXX
# | 1 | 1 | 0 |   0 |
# | 1 | 1 | 1 |   1 | XXXX
# HDL: Mux.hdl
multiplex <- function(a = logical(1), b = logical(1), sel = logical(1)) {
  a_on <- and(not(sel), a)
  b_on <- and(sel, b)
  or(a_on, b_on)
}

# Here we just forward the `a` value to one of two possible output bits, depending
# on value of selector.

# This is the first chip we return a vector of bits instead of a single bit.

# `sel` chooses which of your output pins could possibly be turned on. so
# don't think of it as "sel is off, therefore no outs can be on." instead, sel
# is choosing which output bit to channel the `in` value to, either out1
# (sel = 0) or out2 (sel = 1).

# Think through how either one of the output bits could be T

# out1 could be True only if sel is 0 and in is 1
# out2 could be T only if sel is 1 and in is 1

#    sel in  out1 out2
#   |---|---|----|----|
#   | 0 | 0 |  0 |  0 |
#   | 0 | 1 |  1 |  0 |....  sel is 0 and in (aka a) is 1
#   | 1 | 0 |  0 |  0 |....  sel is 1 but doesn't correspond to input (which selects out2)
#   | 1 | 1 |  0 |  1 |....  sel is 1 and in is 1

# HDL: DMux.hdl
dmultiplex <- function(in_ = logical(1), sel = logical(1)) {
  c(
    # selector is 0 (out1 is 1) and a is 1
    and(not(sel), in_),
    # selector is 1 and input is 1
    and(sel, in_)
  )
}
