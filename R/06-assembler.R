# Hack Assembler (Chapter 6)
# Translates Hack assembly into 16-bit binary machine code.

trim_comment <- function(line) {
  no_comment <- sub("//.*", "", line)
  str_trim(no_comment, side = "both")
}

is_numeric <- function(text) {
  grepl("^[0-9]+$", text)
}

comp_bits <- function(comp) {
  table <- list(
    "0" = "0101010",
    "1" = "0111111",
    "-1" = "0111010",
    "D" = "0001100",
    "A" = "0110000",
    "M" = "1110000",
    "!D" = "0001101",
    "!A" = "0110001",
    "!M" = "1110001",
    "-D" = "0001111",
    "-A" = "0110011",
    "-M" = "1110011",
    "D+1" = "0011111",
    "A+1" = "0110111",
    "M+1" = "1110111",
    "D-1" = "0001110",
    "A-1" = "0110010",
    "M-1" = "1110010",
    "D+A" = "0000010",
    "D+M" = "1000010",
    "D-A" = "0010011",
    "D-M" = "1010011",
    "A-D" = "0000111",
    "M-D" = "1000111",
    "D&A" = "0000000",
    "D&M" = "1000000",
    "D|A" = "0010101",
    "D|M" = "1010101"
  )
  table[[comp]]
}

dest_bits <- function(dest) {
  if (dest == "") {
    return("000")
  }
  table <- list(
    "M" = "001",
    "D" = "010",
    "MD" = "011",
    "A" = "100",
    "AM" = "101",
    "AD" = "110",
    "AMD" = "111"
  )
  table[[dest]]
}

jump_bits <- function(jump) {
  if (jump == "") {
    return("000")
  }
  table <- list(
    "JGT" = "001",
    "JEQ" = "010",
    "JGE" = "011",
    "JLT" = "100",
    "JNE" = "101",
    "JLE" = "110",
    "JMP" = "111"
  )
  table[[jump]]
}

default_symbols <- function() {
  base <- c(
    paste0("R", 0:15),
    "SP", "LCL", "ARG", "THIS", "THAT", "SCREEN", "KBD"
  )
  values <- c(0:15, 0, 1, 2, 3, 4, 16384, 24576)
  setNames(as.list(values), base)
}

first_pass_symbols <- function(lines) {
  symbols <- default_symbols()
  rom <- 0L
  for (ln in lines) {
    if (ln == "") next
    if (startsWith(ln, "(") && endsWith(ln, ")")) {
      label <- substr(ln, 2, nchar(ln) - 1)
      symbols[[label]] <- rom
    } else {
      rom <- rom + 1L
    }
  }
  symbols
}

binary_16 <- function(num) {
  bits <- rev(as.integer(intToBits(as.integer(num))[1:16]))
  paste0(bits, collapse = "")
}

assemble_instruction <- function(instr, symbols, next_var_addr) {
  if (startsWith(instr, "@")) {
    sym <- substr(instr, 2, nchar(instr))
    addr <- if (is_numeric(sym)) {
      as.integer(sym)
    } else if (!is.null(symbols[[sym]])) {
      symbols[[sym]]
    } else {
      symbols[[sym]] <- next_var_addr$value
      next_var_addr$value <- next_var_addr$value + 1L
      symbols[[sym]]
    }
    list(code = binary_16(addr), symbols = symbols, next_var_addr = next_var_addr)
  } else {
    parts <- strsplit(instr, ";")[[1]]
    dest_comp <- parts[1]
    jump_part <- if (length(parts) == 2) parts[2] else ""
    dest_split <- strsplit(dest_comp, "=")[[1]]
    if (length(dest_split) == 2) {
      dest_part <- dest_split[1]
      comp_part <- dest_split[2]
    } else {
      dest_part <- ""
      comp_part <- dest_split[1]
    }
    comp_code <- comp_bits(comp_part)
    dest_code <- dest_bits(dest_part)
    jump_code <- jump_bits(jump_part)
    list(code = paste0("111", comp_code, dest_code, jump_code), symbols = symbols, next_var_addr = next_var_addr)
  }
}

assemble_lines <- function(lines) {
  cleaned <- vapply(lines, trim_comment, character(1))
  cleaned <- cleaned[cleaned != ""]

  symbols <- first_pass_symbols(cleaned)
  next_var_addr <- list(value = 16L)

  codes <- character(0)
  for (ln in cleaned) {
    if (startsWith(ln, "(") && endsWith(ln, ")")) {
      next
    }
    res <- assemble_instruction(ln, symbols, next_var_addr)
    symbols <- res$symbols
    next_var_addr <- res$next_var_addr
    codes <- c(codes, res$code)
  }
  codes
}

assemble_file <- function(path) {
  assemble_lines(readLines(path, warn = FALSE))
}
