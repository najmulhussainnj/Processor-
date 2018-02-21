
create_clock -name CLK -period 1428 -waveform {0 714} [get_ports CLK]

    

    set_input_delay  856 -clock CLK [all_inputs]

    set_output_delay 856 -clock CLK [all_outputs]



set_clock_uncertainty 150 CLK


