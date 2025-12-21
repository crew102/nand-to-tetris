# ARITHMETIC CHIPS
# Chapter 2: Boolean Arithmetic
# These chips perform arithmetic operations, building from basic gates
# to create adders, incrementers, and the Arithmetic Logic Unit (ALU)

# ---- Basic Adders ----

# Half Adder
# Takes two bits (a, b) and outputs sum and carry
# sum = a XOR b
# carry = a AND b
# HDL: HalfAdder.hdl
half_adder <- function(a = logical(1), b = logical(1)) {
  sum <- xor(a, b)
  carry <- and(a, b)
  list(sum = sum, carry = carry)
}

# Full Adder
# Takes three bits (a, b, c) where c is carry-in
# Outputs sum and carry-out
# Can be built using two HalfAdders
# HDL: FullAdder.hdl
full_adder <- function(a = logical(1), b = logical(1), c = logical(1)) {
  # First half adder: a + b
  ha1 <- half_adder(a, b)
  # Second half adder: (a+b) + c
  ha2 <- half_adder(ha1$sum, c)
  # Final sum is from second half adder
  sum <- ha2$sum
  # Carry is either from first half adder OR second half adder
  carry <- or(ha1$carry, ha2$carry)
  list(sum = sum, carry = carry)
}

# ---- Multi-bit Arithmetic ----

# 16-bit Adder
# Takes two 16-bit buses and outputs a 16-bit sum
# Built using FullAdders in ripple-carry fashion
# Each full adder's carry output feeds into the next full adder's carry input
# Process each bit position from least significant (index 16) to most (index 1)
# HDL: Add16.hdl
add_16 <- function(bus_a = logical(16), bus_b = logical(16)) {
  # Chain 16 full adders together, LSB (index 16) to MSB (index 1)
  fa16 <- full_adder(bus_a[16], bus_b[16], FALSE)
  fa15 <- full_adder(bus_a[15], bus_b[15], fa16$carry)
  fa14 <- full_adder(bus_a[14], bus_b[14], fa15$carry)
  fa13 <- full_adder(bus_a[13], bus_b[13], fa14$carry)
  fa12 <- full_adder(bus_a[12], bus_b[12], fa13$carry)
  fa11 <- full_adder(bus_a[11], bus_b[11], fa12$carry)
  fa10 <- full_adder(bus_a[10], bus_b[10], fa11$carry)
  fa9 <- full_adder(bus_a[9], bus_b[9], fa10$carry)
  fa8 <- full_adder(bus_a[8], bus_b[8], fa9$carry)
  fa7 <- full_adder(bus_a[7], bus_b[7], fa8$carry)
  fa6 <- full_adder(bus_a[6], bus_b[6], fa7$carry)
  fa5 <- full_adder(bus_a[5], bus_b[5], fa6$carry)
  fa4 <- full_adder(bus_a[4], bus_b[4], fa5$carry)
  fa3 <- full_adder(bus_a[3], bus_b[3], fa4$carry)
  fa2 <- full_adder(bus_a[2], bus_b[2], fa3$carry)
  fa1 <- full_adder(bus_a[1], bus_b[1], fa2$carry)

  # Combine all sum bits from MSB to LSB
  c(
    fa1$sum, fa2$sum, fa3$sum, fa4$sum, fa5$sum, fa6$sum, fa7$sum, fa8$sum,
    fa9$sum, fa10$sum, fa11$sum, fa12$sum, fa13$sum, fa14$sum, fa15$sum, fa16$sum
  )
}

# 16-bit Incrementer
# Adds 1 to the input
# Can be implemented using Add16 with one input being 1
# HDL: Inc16.hdl
inc_16 <- function(bus_in = logical(16)) {
  one <- c(rep(FALSE, 15), TRUE) # 0000000000000001
  add_16(bus_in, one)
}

# ---- Arithmetic Logic Unit ----

# Arithmetic Logic Unit (ALU)
# Performs various arithmetic and logical operations based on control bits
# Inputs:
#   x, y: 16-bit input buses
#   zx: zero x (if 1, set x to 0)
#   nx: negate x (if 1, negate x)
#   zy: zero y (if 1, set y to 0)
#   ny: negate y (if 1, negate y)
#   f: function (if 1, compute x+y; if 0, compute x&y)
#   no: negate output (if 1, negate output)
# Outputs:
#   out: 16-bit output
#   zr: zero flag (1 if out == 0, else 0)
#   ng: negative flag (1 if out < 0, i.e., MSB is 1, else 0)
# HDL: ALU.hdl
alu <- function(x = logical(16), y = logical(16),
                zx = logical(1), nx = logical(1),
                zy = logical(1), ny = logical(1),
                f = logical(1), no = logical(1)) {
  # Step 1: Zero x if zx is set - use multiplex to choose between x and all zeros
  zeros <- rep(FALSE, 16)
  x_zeroed <- multiplex_16(bus_a = x, bus_b = zeros, sel = zx)

  # Step 2: Negate x if nx is set - use multiplex to choose between x_zeroed and its negation
  x_negated <- not_16(x_zeroed)
  x_processed <- multiplex_16(bus_a = x_zeroed, bus_b = x_negated, sel = nx)

  # Step 3: Zero y if zy is set - use multiplex to choose between y and all zeros
  y_zeroed <- multiplex_16(bus_a = y, bus_b = zeros, sel = zy)

  # Step 4: Negate y if ny is set - use multiplex to choose between y_zeroed and its negation
  y_negated <- not_16(y_zeroed)
  y_processed <- multiplex_16(bus_a = y_zeroed, bus_b = y_negated, sel = ny)

  # Step 5: Compute function - use multiplex to choose between x&y and x+y
  # If f == 0: compute x & y (bitwise AND)
  # If f == 1: compute x + y (addition)
  xy_and <- and_16(x_processed, y_processed)
  xy_add <- add_16(x_processed, y_processed)
  result <- multiplex_16(bus_a = xy_and, bus_b = xy_add, sel = f)

  # Step 6: Negate output if no is set - use multiplex to choose between result and its negation
  result_negated <- not_16(result)
  out <- multiplex_16(bus_a = result, bus_b = result_negated, sel = no)

  # Step 7: Compute flags
  # zr: 1 if all bits are 0, else 0
  # Use Or8Way twice to check if any bits are set, then negate
  lower_8 <- or_8way(out[9:16])
  upper_8 <- or_8way(out[1:8])
  any_bit_set <- or(lower_8, upper_8)
  zr <- not(any_bit_set)

  # ng: 1 if MSB (first bit) is 1, else 0
  ng <- out[1]

  list(out = out, zr = zr, ng = ng)
}
