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
# Process each bit position from least significant (index 16) to most (index 1)
# In HDL, we typically work from bit 0 (LSB) to bit 15 (MSB)
# In R, we'll work from index 16 (LSB) to index 1 (MSB)
# HDL: Add16.hdl
add_16 <- function(bus_a = logical(16), bus_b = logical(16)) {
  # Start with no carry-in
  carry_in <- FALSE

  # Process each bit position from least significant (index 16) to most (index 1)
  result <- logical(16)

  for (i in 16:1) {
      fa <- full_adder(bus_a[i], bus_b[i], carry_in)
      result[i] <- fa$sum
      carry_in <- fa$carry
  }

  result
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
  # Step 1: Zero x if zx is set
  x_zeroed <- if (zx) rep(FALSE, 16) else x

  # Step 2: Negate x if nx is set
  x_processed <- if (nx) not_16(x_zeroed) else x_zeroed

  # Step 3: Zero y if zy is set
  y_zeroed <- if (zy) rep(FALSE, 16) else y

  # Step 4: Negate y if ny is set
  y_processed <- if (ny) not_16(y_zeroed) else y_zeroed

  # Step 5: Compute function
  # If f == 1: compute x + y (addition)
  # If f == 0: compute x & y (bitwise AND)
  if (f) {
      result <- add_16(x_processed, y_processed)
  } else {
      result <- and_16(x_processed, y_processed)
  }

  # Step 6: Negate output if no is set
  out <- if (no) not_16(result) else result

  # Step 7: Compute flags
  # zr: 1 if all bits are 0, else 0
  zr <- !any(out)

  # ng: 1 if MSB (first bit) is 1, else 0
  ng <- out[1]

  list(out = out, zr = zr, ng = ng)
}
