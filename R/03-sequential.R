# SEQUENTIAL CHIPS
# Chapter 3: Sequential Logic
# These chips introduce state by using the DFF primitive. We model state via
# closures: each constructor returns a stateful object with `out()` and `tick()`.

# ---- Primitive ----

# Data Flip-Flop (DFF) built from NAND gates using a two-phase master-slave latch
# We expose explicit phases: phase "A" updates master (transparent), phase "B"
# updates slave (transparent). tick_cycle runs A then B. No control flow is used;
# gating is done via multiplexors.
# HDL: DFF.hdl
dff <- function(init = logical(1)) {
  # Helper: one gated D latch built from NAND feedback
  # For a D latch, Q follows D when enabled. We implement this directly
  # using multiplexors to avoid SR latch feedback timing issues.
  latch <- function(q0 = logical(1)) {
    q_state <- q0
    qbar_state <- not(q0)
    list(
      out = function() q_state,
      tick = function(d = logical(1), enable = logical(1)) {
        # When enabled, Q = D and Q' = NOT(D)
        # When not enabled, hold current state
        q_next <- multiplex(q_state, d, enable)
        qbar_next <- multiplex(qbar_state, not(d), enable)
        q_state <<- q_next
        qbar_state <<- qbar_next
        q_next
      }
    )
  }

  master <- latch(init)
  slave <- latch(init)

  tick_phase <- function(in_ = logical(1), phase = "A") {
    phase_a <- phase == "A"
    phase_b <- phase == "B"
    master$tick(in_, phase_a) # master transparent on A
    slave$tick(master$out(), phase_b) # slave transparent on B
    slave$out()
  }

  list(
    out = function() slave$out(),
    tick_phase = tick_phase,
    tick_cycle = function(in_ = logical(1)) {
      tick_phase(in_, "A")
      tick_phase(in_, "B")
      slave$out()
    }
  )
}

# ---- Bit and Register ----

# Bit
# 1-bit register with load control.
# Exposes phase-based ticking and convenience full-cycle ticking.
# When load = 1, stores `in_`; otherwise holds previous state.
# HDL: Bit.hdl
bit <- function(init = logical(1)) {
  cell <- dff(init)
  
  tick_phase_local <- function(in_ = logical(1), load = logical(1), phase = "A") {
    next_val <- multiplex(cell$out(), in_, load)
    cell$tick_phase(next_val, phase)
    next_val
  }
  
  tick_cycle_local <- function(in_ = logical(1), load = logical(1)) {
    tick_phase_local(in_, load, "A")
    tick_phase_local(in_, load, "B")
    cell$out()
  }
  
  list(
    out = function() cell$out(),
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# 16-bit Register
# Bundles 16 Bit cells with a shared load signal.
# Exposes phase-based ticking and convenience full-cycle ticking.
# HDL: Register.hdl
register <- function(init = logical(16)) {
  b1 <- bit(init[1])
  b2 <- bit(init[2])
  b3 <- bit(init[3])
  b4 <- bit(init[4])
  b5 <- bit(init[5])
  b6 <- bit(init[6])
  b7 <- bit(init[7])
  b8 <- bit(init[8])
  b9 <- bit(init[9])
  b10 <- bit(init[10])
  b11 <- bit(init[11])
  b12 <- bit(init[12])
  b13 <- bit(init[13])
  b14 <- bit(init[14])
  b15 <- bit(init[15])
  b16 <- bit(init[16])

  tick_phase_local <- function(in_ = logical(16), load = logical(1), phase = "A") {
    n1 <- multiplex(b1$out(), in_[1], load)
    n2 <- multiplex(b2$out(), in_[2], load)
    n3 <- multiplex(b3$out(), in_[3], load)
    n4 <- multiplex(b4$out(), in_[4], load)
    n5 <- multiplex(b5$out(), in_[5], load)
    n6 <- multiplex(b6$out(), in_[6], load)
    n7 <- multiplex(b7$out(), in_[7], load)
    n8 <- multiplex(b8$out(), in_[8], load)
    n9 <- multiplex(b9$out(), in_[9], load)
    n10 <- multiplex(b10$out(), in_[10], load)
    n11 <- multiplex(b11$out(), in_[11], load)
    n12 <- multiplex(b12$out(), in_[12], load)
    n13 <- multiplex(b13$out(), in_[13], load)
    n14 <- multiplex(b14$out(), in_[14], load)
    n15 <- multiplex(b15$out(), in_[15], load)
    n16 <- multiplex(b16$out(), in_[16], load)

    b1$tick_phase(n1, load, phase)
    b2$tick_phase(n2, load, phase)
    b3$tick_phase(n3, load, phase)
    b4$tick_phase(n4, load, phase)
    b5$tick_phase(n5, load, phase)
    b6$tick_phase(n6, load, phase)
    b7$tick_phase(n7, load, phase)
    b8$tick_phase(n8, load, phase)
    b9$tick_phase(n9, load, phase)
    b10$tick_phase(n10, load, phase)
    b11$tick_phase(n11, load, phase)
    b12$tick_phase(n12, load, phase)
    b13$tick_phase(n13, load, phase)
    b14$tick_phase(n14, load, phase)
    b15$tick_phase(n15, load, phase)
    b16$tick_phase(n16, load, phase)

    c(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)
  }
  
  tick_cycle_local <- function(in_ = logical(16), load = logical(1)) {
    tick_phase_local(in_, load, "A")
    tick_phase_local(in_, load, "B")
    c(
      b1$out(), b2$out(), b3$out(), b4$out(),
      b5$out(), b6$out(), b7$out(), b8$out(),
      b9$out(), b10$out(), b11$out(), b12$out(),
      b13$out(), b14$out(), b15$out(), b16$out()
    )
  }

  list(
    out = function() {
      c(
        b1$out(), b2$out(), b3$out(), b4$out(),
        b5$out(), b6$out(), b7$out(), b8$out(),
        b9$out(), b10$out(), b11$out(), b12$out(),
        b13$out(), b14$out(), b15$out(), b16$out()
      )
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# ---- RAM Family ----

# RAM8
# 8 registers of 16 bits each. 3-bit address selects which register to read/write.
# HDL: RAM8.hdl
ram_8 <- function() {
  r1 <- register()
  r2 <- register()
  r3 <- register()
  r4 <- register()
  r5 <- register()
  r6 <- register()
  r7 <- register()
  r8 <- register()

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(3), phase = "A") {
    loads <- dmultiplex_8way(load, address)
    r1$tick_phase(in_, loads[1], phase)
    r2$tick_phase(in_, loads[2], phase)
    r3$tick_phase(in_, loads[3], phase)
    r4$tick_phase(in_, loads[4], phase)
    r5$tick_phase(in_, loads[5], phase)
    r6$tick_phase(in_, loads[6], phase)
    r7$tick_phase(in_, loads[7], phase)
    r8$tick_phase(in_, loads[8], phase)
  }
  
  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(3)) {
    tick_phase_local(in_, load, address, "A")
    tick_phase_local(in_, load, address, "B")
  }

  list(
    out = function(address = logical(3)) {
      multiplexor_8way_16(
        r1$out(), r2$out(), r3$out(), r4$out(),
        r5$out(), r6$out(), r7$out(), r8$out(),
        address
      )
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# RAM64
# 8 RAM8 chips. Upper 3 address bits choose the RAM8; lower 3 choose the register.
# HDL: RAM64.hdl
ram_64 <- function() {
  m1 <- ram_8()
  m2 <- ram_8()
  m3 <- ram_8()
  m4 <- ram_8()
  m5 <- ram_8()
  m6 <- ram_8()
  m7 <- ram_8()
  m8 <- ram_8()

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(6), phase = "A") {
    high <- address[1:3]
    low <- address[4:6]
    loads <- dmultiplex_8way(load, high)
    m1$tick_phase(in_, loads[1], low, phase)
    m2$tick_phase(in_, loads[2], low, phase)
    m3$tick_phase(in_, loads[3], low, phase)
    m4$tick_phase(in_, loads[4], low, phase)
    m5$tick_phase(in_, loads[5], low, phase)
    m6$tick_phase(in_, loads[6], low, phase)
    m7$tick_phase(in_, loads[7], low, phase)
    m8$tick_phase(in_, loads[8], low, phase)
  }
  
  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(6)) {
    tick_phase_local(in_, load, address, "A")
    tick_phase_local(in_, load, address, "B")
  }

  list(
    out = function(address = logical(6)) {
      high <- address[1:3]
      low <- address[4:6]
      multiplexor_8way_16(
        m1$out(low), m2$out(low), m3$out(low), m4$out(low),
        m5$out(low), m6$out(low), m7$out(low), m8$out(low),
        high
      )
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# RAM512
# 8 RAM64 chips. Address: upper 3 bits select chip, lower 6 bits select word.
# HDL: RAM512.hdl
ram_512 <- function() {
  m1 <- ram_64()
  m2 <- ram_64()
  m3 <- ram_64()
  m4 <- ram_64()
  m5 <- ram_64()
  m6 <- ram_64()
  m7 <- ram_64()
  m8 <- ram_64()

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(9), phase = "A") {
    high <- address[1:3]
    low <- address[4:9]
    loads <- dmultiplex_8way(load, high)
    m1$tick_phase(in_, loads[1], low, phase)
    m2$tick_phase(in_, loads[2], low, phase)
    m3$tick_phase(in_, loads[3], low, phase)
    m4$tick_phase(in_, loads[4], low, phase)
    m5$tick_phase(in_, loads[5], low, phase)
    m6$tick_phase(in_, loads[6], low, phase)
    m7$tick_phase(in_, loads[7], low, phase)
    m8$tick_phase(in_, loads[8], low, phase)
  }
  
  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(9)) {
    tick_phase_local(in_, load, address, "A")
    tick_phase_local(in_, load, address, "B")
  }

  list(
    out = function(address = logical(9)) {
      high <- address[1:3]
      low <- address[4:9]
      multiplexor_8way_16(
        m1$out(low), m2$out(low), m3$out(low), m4$out(low),
        m5$out(low), m6$out(low), m7$out(low), m8$out(low),
        high
      )
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# RAM4K
# 8 RAM512 chips. Address: upper 3 bits select chip, lower 9 bits select word.
# HDL: RAM4K.hdl
ram_4k <- function() {
  m1 <- ram_512()
  m2 <- ram_512()
  m3 <- ram_512()
  m4 <- ram_512()
  m5 <- ram_512()
  m6 <- ram_512()
  m7 <- ram_512()
  m8 <- ram_512()

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(12), phase = "A") {
    high <- address[1:3]
    low <- address[4:12]
    loads <- dmultiplex_8way(load, high)
    m1$tick_phase(in_, loads[1], low, phase)
    m2$tick_phase(in_, loads[2], low, phase)
    m3$tick_phase(in_, loads[3], low, phase)
    m4$tick_phase(in_, loads[4], low, phase)
    m5$tick_phase(in_, loads[5], low, phase)
    m6$tick_phase(in_, loads[6], low, phase)
    m7$tick_phase(in_, loads[7], low, phase)
    m8$tick_phase(in_, loads[8], low, phase)
  }
  
  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(12)) {
    tick_phase_local(in_, load, address, "A")
    tick_phase_local(in_, load, address, "B")
  }

  list(
    out = function(address = logical(12)) {
      high <- address[1:3]
      low <- address[4:12]
      multiplexor_8way_16(
        m1$out(low), m2$out(low), m3$out(low), m4$out(low),
        m5$out(low), m6$out(low), m7$out(low), m8$out(low),
        high
      )
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# RAM16K
# 4 RAM4K chips. Address: upper 2 bits select chip, lower 12 bits select word.
# HDL: RAM16K.hdl
ram_16k <- function() {
  m1 <- ram_4k()
  m2 <- ram_4k()
  m3 <- ram_4k()
  m4 <- ram_4k()

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(14), phase = "A") {
    high <- address[1:2]
    low <- address[3:14]
    loads <- dmultiplex_4way(load, high)
    m1$tick_phase(in_, loads[1], low, phase)
    m2$tick_phase(in_, loads[2], low, phase)
    m3$tick_phase(in_, loads[3], low, phase)
    m4$tick_phase(in_, loads[4], low, phase)
  }
  
  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(14)) {
    tick_phase_local(in_, load, address, "A")
    tick_phase_local(in_, load, address, "B")
  }

  list(
    out = function(address = logical(14)) {
      high <- address[1:2]
      low <- address[3:14]
      multiplexor_4way_16(
        m1$out(low), m2$out(low), m3$out(low), m4$out(low),
        high
      )
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# ---- Program Counter ----

# Program Counter (PC)
# 16-bit register with control signals:
# - inc: increment current value
# - load: load external input
# - reset: set to 0
# Priority: reset > load > inc > hold
# HDL: PC.hdl
pc <- function(init = logical(16)) {
  reg <- register(init)

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               inc = logical(1), reset = logical(1), phase = "A") {
    
    current <- reg$out()
    zeros <- rep(FALSE, 16)

    load_or_hold <- multiplex_16(current, in_, load)
    inc_sel <- and(inc, not(load))
    incremented <- inc_16(current)
    inc_stage <- multiplex_16(load_or_hold, incremented, inc_sel)
    next_val <- multiplex_16(inc_stage, zeros, reset)

    reg$tick_phase(next_val, TRUE, phase)
    next_val
  }
  
  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               inc = logical(1), reset = logical(1)) {
    
    tick_phase_local(in_, load, inc, reset, "A")
    tick_phase_local(in_, load, inc, reset, "B")
    reg$out()
  }

  list(
    out = function() reg$out(),
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}
