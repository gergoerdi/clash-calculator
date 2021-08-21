# Calculator

This repository implements a simple desktop calculator in
Clash. Output is shown on a 4-digit seven segment display, and input
is via a 4x4 keypad. Alternatively, serial I/O can also be used, using
the included 9600 bps UART.

This code is part of the book *Retrocomputing with Clash: Haskell for
FPGA Hardware Design* at <https://unsafePerform.IO/retroclash/>.

## Building

Use `stack build` to build and `stack run` to run the simulation.

For the hardware version, use the included Shakefile. An easy way to
run it is via the provided `mk` shell script. Create a `build.mk` file
with the following content:

    TARGET = nexys-a7-50t
    VIVADO_ROOT = /opt/somewhere/where/vivado/is/installed
    ISE_ROOT = /opt/somewhere/where/ise/is/installed
    
Alternatively, if you have Vivado/ISE installed in Docker or similar, you
can create a wrapper script and use that by setting `VIVADO` instead
of `VIVADO_ROOT`:

    TARGET = nexys-a7-50t
    VIVADO = /usr/local/lib/docker-scripts/xilinx-vivado-2019.1-ubuntu-18.04/run
    ISE = /usr/local/lib/docker-scripts/xilinx-ise-14.7-ubuntu-12.04/run

## Supported target boards

### Nexys A7-50T

A Xilinx 7-series FPGA based development board with integrated
seven-segment display. A 4x4 keypad with a PMOD connector is available
as an add-on; the code assumes this keypad is connected to PMOD port A
of the Nexys A7. Serial I/O is supported via an onboard serial over USB
bridge.

### Papilio Pro & Papilio One

Papilio Pro uses a Xilinx 6-series FPGA, and the Papilio One uses a
3-series one. Both of these have a serial over USB bridge. The
LogicStart MegaWing expansion board can be used for seven-segment
output. 

### Other targets

Adding support to other Vivado or ISE based FPGA dev boards is very
straightforward with the included Shake rules.

Targeting other FPGA toolchains will require adding support in the
Shake rules. Alternatively, you can always just run Clash, and import
the resulting Verilog files into your FPGA toolchain in a
non-automated way.
