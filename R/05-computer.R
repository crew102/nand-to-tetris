# CPU, Memory, and Computer (Chapter 5)
# Sequential components built from NAND, previous combinational chips, and the
# two-phase sequential primitives. No control flow; all selection via
# multiplexors/demultiplexors.

# ---- RAM8K helper (for Screen) ----

# RAM8K: two RAM4K chips selected by the top address bit (13-bit address)
# HDL: (not standard, helper for Screen)
ram_8k <- function() {
  lo <- ram_4k()
  hi <- ram_4k()

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(13), phase = "A") {
    bank_sel <- address[1]
    offset <- address[2:13]
    loads <- dmultiplex(load, bank_sel)
    lo$tick_phase(in_, loads[1], offset, phase)
    hi$tick_phase(in_, loads[2], offset, phase)
  }

  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(13)) {
    tick_phase_local(in_, load, address, "A")
    tick_phase_local(in_, load, address, "B")
  }

  list(
    out = function(address = logical(13)) {
      bank_sel <- address[1]
      offset <- address[2:13]
      multiplex_16(lo$out(offset), hi$out(offset), bank_sel)
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# ---- Memory ----

# Memory chip: combines RAM16K, Screen (RAM8K), and Keyboard input.
# Address[1:15] (MSB first). Regions:
#  - 0xxxx... : RAM16K (14-bit offset)
#  - 10xxx... : Screen (8K, 13-bit offset)
#  - 11xxx... : Keyboard (read-only; value provided via keyboard input)
# HDL: Memory.hdl
memory <- function() {
  ram <- ram_16k()
  screen <- ram_8k()
  keyboard_cell <- register() # holds latest keyboard sample
  zeros <- rep(FALSE, 16)

  tick_phase_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(15),
                               keyboard = logical(16), phase = "A") {
    # Update keyboard register with provided value every phase
    keyboard_cell$tick_phase(keyboard, TRUE, phase)

    region_sel <- address[1:2] # 00 RAM, 01 unused, 10 Screen, 11 Keyboard
    lower_ram_addr <- address[2:15] # 14 bits for RAM16K
    screen_addr <- address[3:15] # 13 bits for Screen

    loads <- dmultiplex_4way(load, region_sel)
    ram$tick_phase(in_, loads[1], lower_ram_addr, phase)
    screen$tick_phase(in_, loads[3], screen_addr, phase)
    # Keyboard is read-only; ignore loads[4]
  }

  tick_cycle_local <- function(in_ = logical(16), load = logical(1), 
                               address = logical(15),
                               keyboard = logical(16)) {
    tick_phase_local(in_, load, address, keyboard, "A")
    tick_phase_local(in_, load, address, keyboard, "B")
  }

  list(
    out = function(address = logical(15), keyboard = logical(16)) {
      region_sel <- address[1:2]
      lower_ram_addr <- address[2:15]
      screen_addr <- address[3:15]

      ram_out <- ram$out(lower_ram_addr)
      screen_out <- screen$out(screen_addr)
      keyboard_out <- keyboard

      multiplexor_4way_16(
        ram_out,
        zeros,
        screen_out,
        keyboard_out,
        region_sel
      )
    },
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# ---- CPU ----

# Hack CPU (A-register, D-register, PC) using existing ALU.
# Inputs:
#   inM: M value (memory at address A)
#   instruction: 16-bit instruction
#   reset: reset PC
# Outputs:
#   outM: value to write to memory
#   writeM: write enable
#   addressM: A-register (memory address)
#   pc: program counter
# HDL: CPU.hdl
cpu <- function() {
  reg_a <- register()
  reg_d <- register()
  prog_counter <- pc()
  zeros <- rep(FALSE, 16)
  last_out <- list(
    outM = zeros, writeM = FALSE, addressM = reg_a$out(), pc = prog_counter$out()
  )

  tick_phase_local <- function(inM = logical(16), instruction = logical(16),
                               reset = logical(1), phase = "A") {
    # Decode instruction
    is_c_prefix <- and(instruction[1], and(instruction[2], instruction[3]))
    is_a_instr <- not(is_c_prefix)
    a_bit <- instruction[4]

    # ALU inputs
    a_out <- reg_a$out()
    d_out <- reg_d$out()
    y_bus <- multiplex_16(a_out, inM, a_bit)
    alu_out <- alu(
      x = d_out,
      y = y_bus,
      zx = instruction[5],
      nx = instruction[6],
      zy = instruction[7],
      ny = instruction[8],
      f = instruction[9],
      no = instruction[10]
    )

    # Destinations
    dest_a <- instruction[11]
    dest_d <- instruction[12]
    dest_m <- instruction[13]

    load_a_from_instr <- is_a_instr
    load_a_from_c <- and(is_c_prefix, dest_a)
    load_a <- or(load_a_from_instr, load_a_from_c)

    a_value_instr <- c(FALSE, instruction[2:16])
    a_input <- multiplex_16(alu_out$out, a_value_instr, load_a_from_instr)

    load_d <- and(is_c_prefix, dest_d)
    write_m <- and(is_c_prefix, dest_m)

    # Jump logic
    j1 <- instruction[14]
    j2 <- instruction[15]
    j3 <- instruction[16]
    neg_flag <- alu_out$ng
    zero_flag <- alu_out$zr
    gt_flag <- and(not(neg_flag), not(zero_flag))
    jump_cond <- or(or(and(j3, gt_flag), and(j2, zero_flag)), and(j1, neg_flag))

    pc_load <- jump_cond
    pc_inc <- not(pc_load)
    pc_input <- reg_a$out()

    # Tick registers
    reg_a$tick_phase(a_input, load_a, phase)
    reg_d$tick_phase(alu_out$out, load_d, phase)
    prog_counter$tick_phase(pc_input, pc_load, pc_inc, reset, phase)

    # Outputs (combinational, based on current state and instruction)
    last_out <<- list(
      outM = alu_out$out,
      writeM = write_m,
      addressM = reg_a$out(),
      pc = prog_counter$out()
    )
    last_out
  }

  tick_cycle_local <- function(inM = logical(16), instruction = logical(16),
                               reset = logical(1)) {
    res_a <- tick_phase_local(inM, instruction, reset, "A")
    res_b <- tick_phase_local(inM, instruction, reset, "B")
    res_b
  }

  list(
    out = function() last_out,
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local
  )
}

# ---- Computer ----

# Computer ties together CPU, Memory, and a ROM provider.
# ROM is provided as a function rom_read(address) returning a 16-bit logical vector.
# HDL: Computer.hdl
computer <- function(rom_read = function(addr = logical(16)) rep(FALSE, 16)) {
  mem <- memory()
  cpu_chip <- cpu()

  tick_phase_local <- function(reset = logical(1), keyboard = logical(16), phase = "A") {
    pc_addr <- cpu_chip$out()$pc
    instr <- rom_read(pc_addr)
    mem_out <- mem$out(pc_addr, keyboard)
    cpu_res <- cpu_chip$tick_phase(mem_out, instr, reset, phase)
    mem$tick_phase(cpu_res$outM, cpu_res$writeM, cpu_res$addressM[2:16], keyboard, phase)
    cpu_res
  }

  tick_cycle_local <- function(reset = logical(1), keyboard = logical(16)) {
    res_a <- tick_phase_local(reset, keyboard, "A")
    res_b <- tick_phase_local(reset, keyboard, "B")
    res_b
  }

  list(
    tick_phase = tick_phase_local,
    tick_cycle = tick_cycle_local,
    out = function() cpu_chip$out()
  )
}
