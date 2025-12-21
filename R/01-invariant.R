# BASE GATE
# This is what we assume is possible to physically implement with some black
# box (i.e., using hardware and laws of physics).
# Two input pins, one output pin
# When both input pins have voltage in them, the output pin doesn't have voltage,
# otherwise it does.
nand <- function(a = logical(1), b = logical(1)) {
  !(a && b)
}
