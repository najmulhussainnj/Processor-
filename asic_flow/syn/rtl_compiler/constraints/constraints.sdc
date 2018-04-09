### Main Clock
dc::create_clock -name CLK -period 1000 -waveform {0 500} [dc::get_ports CLK]

### I/O constraints
dc::set_output_delay 600 -max -clock CLK [dc::all_outputs]
dc::set_input_delay 600 -max -clock CLK [dc::all_inputs]
dc::set_clock_uncertainty 150 CLK
