// Top-level driver for "verilated" objects (Verilog compiled with verilator)

#include <verilated.h>

#include "VmkTbSoc.h"


vluint64_t main_time = 0;    // Current simulation time

double sc_time_stamp () {    // Called by $time in Verilog
    return main_time;
}

int main (int argc, char **argv, char **env) {
    Verilated::commandArgs (argc, argv);    // remember args

    VmkTbSoc* mkTbSoc = new VmkTbSoc;    // create instance of model

    mkTbSoc->RST_N = 0;    // assert reset
    mkTbSoc->CLK = 0;

    while (! Verilated::gotFinish ()) {

	if (main_time == 1) {
	    mkTbSoc->CLK = 1;
	}
	else if (main_time == 2) {
	    mkTbSoc->RST_N = 1;    // Deassert reset
	}

	// Toggle clock
	if ((main_time % 10) == 5) {
	    mkTbSoc->CLK = 0;
	}
	else if ((main_time % 10) == 0) {
	    mkTbSoc->CLK = 1;
	}


	mkTbSoc->eval ();
	main_time++;
    }

    mkTbSoc->final ();    // Done simulating

    delete mkTbSoc;
    mkTbSoc = NULL;

    exit (0);
}
