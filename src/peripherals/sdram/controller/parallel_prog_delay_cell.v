
module parallel_prog_delay_cell ( in_clk, delay_config_reg, delayed_clk);
    input in_clk;
    input [3:0] delay_config_reg;
    output delayed_clk;

    wire [7:0] wr_temp_delayed_clk;
	wire xor_outp_clk;

	xor(xor_outp_clk, delay_config_reg[3], in_clk);

	assign wr_temp_delayed_clk[0]= xor_outp_clk;
	delay_chain#(3) chain1 (.out(wr_temp_delayed_clk[1]),.in(xor_outp_clk));
	delay_chain#(6) chain2 (.out(wr_temp_delayed_clk[2]),.in(xor_outp_clk));
	delay_chain#(12) chain3 (.out(wr_temp_delayed_clk[3]),.in(xor_outp_clk));
	delay_chain#(18) chain4 (.out(wr_temp_delayed_clk[4]),.in(xor_outp_clk));
	delay_chain#(26) chain5 (.out(wr_temp_delayed_clk[5]),.in(xor_outp_clk));
	delay_chain#(38) chain6 (.out(wr_temp_delayed_clk[6]),.in(xor_outp_clk));
	delay_chain#(50) chain7 (.out(wr_temp_delayed_clk[7]),.in(xor_outp_clk));

	assign delayed_clk= wr_temp_delayed_clk[delay_config_reg[2:0]];

endmodule

module delay_chain (in, out);
	parameter Depth=0;
	input in;
	output out;
	wire [Depth-1:0] wr_inter;

	buf(wr_inter[0],in); // replace this with the buffer from the ASIC library
	genvar i;
	generate 
		for(i=1; i<= Depth-1; i=i+1)
		begin: gen_delay_buffer_chains
			buf(wr_inter[i],wr_inter[i-1]); // replace this with the buffer form the ASIC library.
		end
	endgenerate

	assign out= wr_inter[Depth-1];
endmodule

