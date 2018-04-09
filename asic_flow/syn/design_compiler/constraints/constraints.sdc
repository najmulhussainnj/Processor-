
create_clock -name CLK -period 1000 -waveform {0 500} [get_ports CLK]

    set_input_delay  600 -clock CLK [all_inputs]

    set_output_delay 600 -clock CLK [all_outputs]

set_clock_uncertainty 150 CLK


