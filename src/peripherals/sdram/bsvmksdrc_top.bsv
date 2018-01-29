/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*/
// Bluespec wrapper, created by Import BVI Wizard
// Created on: Mon Jul 03 20:03:44 IST 2017
// Created by: vishvesh
// Bluespec version: 2017.03.beta1 2017-03-16 35049
`define SDR_RFSH_TIMER_W    12
`define SDR_RFSH_ROW_CNT_W  3

interface Ifc_sdram;
    (*always_ready, always_enabled*)
    method Action iapp_req (bit app_req);
    (*always_ready, always_enabled*)
    method Action iapp_req_wrap (bit app_req_wrap);
    (*always_ready, always_enabled*)
	method Action icfg_sdr_width (Bit#(2) cfg_sdr_width);
    (*always_ready, always_enabled*)
	method Action icfg_colbits (Bit#(2) cfg_colbits);
    (*always_ready, always_enabled*)
	method Action icfg_sdr_tras_d (Bit#(4) cfg_sdr_tras_d);
	(*always_ready, always_enabled*)
	method Action icfg_sdr_trp_d (Bit#(4) cfg_sdr_trp_d);
	(*always_ready, always_enabled*)
	method Action icfg_sdr_trcd_d (Bit#(4) cfg_sdr_trcd_d);
	(*always_ready, always_enabled*)
	method Action icfg_sdr_en (bit cfg_sdr_en);
	(*always_ready, always_enabled*)
	method Action icfg_req_depth (Bit#(2) cfg_req_depth);
	(*always_ready, always_enabled*)
	method Action icfg_sdr_mode_reg (Bit#(13) cfg_sdr_mode_reg);
	(*always_ready, always_enabled*)
	method Action icfg_sdr_cas (Bit#(3) cfg_sdr_cas);
	(*always_ready, always_enabled*)
	method Action icfg_sdr_trcar_d (Bit#(4) cfg_sdr_trcar_d);
	(*always_ready, always_enabled*)
	method Action icfg_sdr_twr_d (Bit#(4) cfg_sdr_twr_d);
	(*always_ready, always_enabled*)
    method Action icfg_sdr_rfsh (Bit#(`SDR_RFSH_TIMER_W) cfg_sdr_rfsh);
	(*always_ready, always_enabled*)
    method Action icfg_sdr_rfmax (Bit#(`SDR_RFSH_ROW_CNT_W) cfg_sdr_rfmax);
	(*always_ready, always_enabled*)
	method Action iapp_req_addr (Bit#(26) app_req_addr);
	(*always_ready, always_enabled*)
	method Action iapp_req_len (Bit#(9) app_req_len);
	(*always_ready, always_enabled*)
	method Action iapp_req_wr_n (bit app_req_wr_n);
	(*always_ready, always_enabled*)
	method Action iapp_wr_en_n (Bit#(8) app_wr_en_n);
	(*always_ready, always_enabled*)
	method Action iapp_wr_data (Bit#(64) app_wr_data);
	(*always_ready, always_enabled*)
    method Action ipad_sdr_din (Bit#(64) pad_sdr_din);
	(*always_enabled*)
	method Bool osdr_cke ();
	(*always_enabled*)
	method Bool osdr_cs_n ();
	(*always_enabled*)
	method Bool osdr_ras_n ();
	(*always_enabled*)
	method Bool osdr_cas_n ();
	(*always_enabled*)
	method Bool osdr_we_n ();
	(*always_enabled*)
	method Bit#(8) osdr_dqm ();
	(*always_enabled*)
	method Bit#(2) osdr_ba ();
	(*always_enabled*)
	method Bit#(13) osdr_addr ();
	(*always_enabled*)
	method Bit#(64) osdr_dout ();
	(*always_enabled*)
	method Bit#(8) osdr_den_n ();
	(*always_enabled*)
	method Bool osdr_init_done ();
	(*always_enabled*)
	method Bool oapp_req_ack ();
	(*always_enabled*)
	method Bool oapp_wr_next_req ();
	(*always_enabled*)
	method Bool oapp_rd_valid ();
	(*always_enabled*)
	method Bool oapp_last_rd ();
	(*always_enabled*)
	method Bool oapp_last_wr ();
	(*always_enabled*)
	method Bit#(64) oapp_rd_data ();
endinterface

import "BVI" sdrc_top =
module mksdrc_top  (Ifc_sdram);

	parameter APP_AW = 26;
	parameter APP_DW = 64;
	parameter APP_BW = 8;
	parameter APP_RW = 9;
	parameter SDR_DW = 64;
	parameter SDR_BW = 8;
	parameter dw = 64;
	parameter tw = 8;
	parameter bl = 9;

	default_clock clk_sdram_clk;
	default_reset rst_sdram_resetn;

	input_clock clk_sdram_clk (sdram_clk)  <- exposeCurrentClock;
	input_reset rst_sdram_resetn (sdram_resetn) clocked_by(clk_sdram_clk)  <- exposeCurrentReset;


    method ipad_sdr_din (pad_sdr_din)
        enable((*inhigh*)ipad_sdr_din_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
    method iapp_req (app_req )
		 enable((*inhigh*)iapp_req_enable)  clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
    method iapp_req_wrap (app_req_wrap )
		 enable((*inhigh*)iapp_req_wrap_enable)  clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_width (cfg_sdr_width /*3:0*/)
		 enable((*inhigh*)icfg_sdr_width_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_colbits (cfg_colbits /*3:0*/)
		 enable((*inhigh*)icfg_colbits_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_tras_d (cfg_sdr_tras_d /*3:0*/)
		 enable((*inhigh*)icfg_sdr_tras_d_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_trp_d (cfg_sdr_trp_d /*3:0*/)
		 enable((*inhigh*)icfg_sdr_trp_d_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_trcd_d (cfg_sdr_trcd_d /*3:0*/)
		 enable((*inhigh*)icfg_sdr_trcd_d_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_en (cfg_sdr_en )
		 enable((*inhigh*)icfg_sdr_en_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_req_depth (cfg_req_depth /*1:0*/)
		 enable((*inhigh*)icfg_req_depth_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_mode_reg (cfg_sdr_mode_reg /*12:0*/)
		 enable((*inhigh*)icfg_sdr_mode_reg_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_cas (cfg_sdr_cas /*2:0*/)
		 enable((*inhigh*)icfg_sdr_cas_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_trcar_d (cfg_sdr_trcar_d /*3:0*/)
		 enable((*inhigh*)icfg_sdr_trcar_d_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_twr_d (cfg_sdr_twr_d /*3:0*/)
		 enable((*inhigh*)icfg_sdr_twr_d_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_rfsh (cfg_sdr_rfsh /*`SDR_RFSH_TIMER_W-1:0*/)
		 enable((*inhigh*)icfg_sdr_rfsh_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method icfg_sdr_rfmax (cfg_sdr_rfmax /*`SDR_RFSH_ROW_CNT_W-1:0*/)
		 enable((*inhigh*)icfg_sdr_rfmax_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method iapp_req_addr (app_req_addr /*APP_AW-1:0*/)
		 enable((*inhigh*)iapp_req_addr_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method iapp_req_len (app_req_len /*bl-1:0*/)
		 enable((*inhigh*)iapp_req_len_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method iapp_req_wr_n (app_req_wr_n )
		 enable((*inhigh*)iapp_req_wr_n_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method iapp_wr_en_n (app_wr_en_n /*dw/8-1:0*/)
		 enable((*inhigh*)iapp_wr_en_n_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method iapp_wr_data (app_wr_data /*dw-1:0*/)
		 enable((*inhigh*)iapp_wr_data_enable) clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);

	method sdr_dout osdr_dout ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_den_n osdr_den_n ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_cke osdr_cke ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_cs_n osdr_cs_n ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_ras_n osdr_ras_n ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_cas_n osdr_cas_n ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_we_n osdr_we_n ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_dqm /* SDR_BW-1 : 0 */ osdr_dqm ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_ba /* 1 : 0 */ osdr_ba ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_addr /* 12 : 0 */ osdr_addr ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method sdr_init_done osdr_init_done ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method app_req_ack oapp_req_ack ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method app_wr_next_req oapp_wr_next_req ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method app_rd_valid oapp_rd_valid ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method app_last_rd oapp_last_rd ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method app_last_wr oapp_last_wr ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);
	method app_rd_data /* dw-1 : 0 */ oapp_rd_data ()
		 clocked_by(clk_sdram_clk) reset_by(rst_sdram_resetn);


    schedule icfg_colbits C icfg_colbits;
    schedule icfg_colbits CF icfg_sdr_width;
    schedule icfg_colbits CF ipad_sdr_din;
    schedule icfg_colbits CF iapp_req;
	schedule icfg_colbits CF iapp_req_addr;
	schedule icfg_colbits CF iapp_req_len;
	schedule icfg_colbits CF iapp_req_wr_n;
	schedule icfg_colbits CF iapp_wr_en_n;
	schedule icfg_colbits CF iapp_wr_data;
	schedule icfg_colbits CF icfg_sdr_tras_d;
	schedule icfg_colbits CF icfg_sdr_trp_d;
	schedule icfg_colbits CF icfg_sdr_trcd_d;
	schedule icfg_colbits CF icfg_sdr_en;
	schedule icfg_colbits CF icfg_req_depth;
	schedule icfg_colbits CF icfg_sdr_mode_reg;
	schedule icfg_colbits CF icfg_sdr_cas;
	schedule icfg_colbits CF icfg_sdr_trcar_d;
	schedule icfg_colbits CF icfg_sdr_twr_d;
	schedule icfg_colbits CF icfg_sdr_rfmax;
	schedule icfg_colbits CF icfg_sdr_rfsh;
	schedule icfg_colbits CF iapp_req_wrap;
	schedule osdr_cke CF icfg_colbits;
	schedule osdr_cs_n CF icfg_colbits;
	schedule osdr_ras_n CF icfg_colbits;
	schedule osdr_cas_n CF icfg_colbits;
	schedule osdr_we_n CF icfg_colbits;
	schedule osdr_dqm CF icfg_colbits;
	schedule osdr_ba CF icfg_colbits;
	schedule osdr_addr CF icfg_colbits;
	schedule osdr_init_done CF icfg_colbits;
	schedule oapp_req_ack CF icfg_colbits;
	schedule oapp_wr_next_req CF icfg_colbits;
	schedule oapp_rd_valid CF icfg_colbits;
	schedule oapp_last_rd CF icfg_colbits;
	schedule oapp_last_wr CF icfg_colbits;
	schedule oapp_rd_data CF icfg_colbits;



    schedule icfg_sdr_width C icfg_sdr_width;
    schedule icfg_sdr_width CF ipad_sdr_din;
    schedule icfg_sdr_width CF iapp_req;
	schedule icfg_sdr_width CF iapp_req_addr;
	schedule icfg_sdr_width CF iapp_req_len;
	schedule icfg_sdr_width CF iapp_req_wr_n;
	schedule icfg_sdr_width CF iapp_wr_en_n;
	schedule icfg_sdr_width CF iapp_wr_data;
	schedule icfg_sdr_width CF icfg_sdr_tras_d;
	schedule icfg_sdr_width CF icfg_sdr_trp_d;
	schedule icfg_sdr_width CF icfg_sdr_trcd_d;
	schedule icfg_sdr_width CF icfg_sdr_en;
	schedule icfg_sdr_width CF icfg_req_depth;
	schedule icfg_sdr_width CF icfg_sdr_mode_reg;
	schedule icfg_sdr_width CF icfg_sdr_cas;
	schedule icfg_sdr_width CF icfg_sdr_trcar_d;
	schedule icfg_sdr_width CF icfg_sdr_twr_d;
	schedule icfg_sdr_width CF icfg_sdr_rfmax;
	schedule icfg_sdr_width CF icfg_sdr_rfsh;
	schedule icfg_sdr_width CF iapp_req_wrap;
	schedule osdr_cke CF icfg_sdr_width;
	schedule osdr_cs_n CF icfg_sdr_width;
	schedule osdr_ras_n CF icfg_sdr_width;
	schedule osdr_cas_n CF icfg_sdr_width;
	schedule osdr_we_n CF icfg_sdr_width;
	schedule osdr_dqm CF icfg_sdr_width;
	schedule osdr_ba CF icfg_sdr_width;
	schedule osdr_addr CF icfg_sdr_width;
	schedule osdr_init_done CF icfg_sdr_width;
	schedule oapp_req_ack CF icfg_sdr_width;
	schedule oapp_wr_next_req CF icfg_sdr_width;
	schedule oapp_rd_valid CF icfg_sdr_width;
	schedule oapp_last_rd CF icfg_sdr_width;
	schedule oapp_last_wr CF icfg_sdr_width;
	schedule oapp_rd_data CF icfg_sdr_width;
    
    schedule ipad_sdr_din C ipad_sdr_din;
    schedule ipad_sdr_din CF iapp_req;
	schedule ipad_sdr_din CF iapp_req_addr;
	schedule ipad_sdr_din CF iapp_req_len;
	schedule ipad_sdr_din CF iapp_req_wr_n;
	schedule ipad_sdr_din CF iapp_wr_en_n;
	schedule ipad_sdr_din CF iapp_wr_data;
	schedule ipad_sdr_din CF icfg_sdr_tras_d;
	schedule ipad_sdr_din CF icfg_sdr_trp_d;
	schedule ipad_sdr_din CF icfg_sdr_trcd_d;
	schedule ipad_sdr_din CF icfg_sdr_en;
	schedule ipad_sdr_din CF icfg_req_depth;
	schedule ipad_sdr_din CF icfg_sdr_mode_reg;
	schedule ipad_sdr_din CF icfg_sdr_cas;
	schedule ipad_sdr_din CF icfg_sdr_trcar_d;
	schedule ipad_sdr_din CF icfg_sdr_twr_d;
	schedule ipad_sdr_din CF icfg_sdr_rfmax;
	schedule ipad_sdr_din CF icfg_sdr_rfsh;
	schedule ipad_sdr_din CF iapp_req_wrap;
	schedule osdr_cke CF ipad_sdr_din;
	schedule osdr_cs_n CF ipad_sdr_din;
	schedule osdr_ras_n CF ipad_sdr_din;
	schedule osdr_cas_n CF ipad_sdr_din;
	schedule osdr_we_n CF ipad_sdr_din;
	schedule osdr_dqm CF ipad_sdr_din;
	schedule osdr_ba CF ipad_sdr_din;
	schedule osdr_addr CF ipad_sdr_din;
	schedule osdr_init_done CF ipad_sdr_din;
	schedule oapp_req_ack CF ipad_sdr_din;
	schedule oapp_wr_next_req CF ipad_sdr_din;
	schedule oapp_rd_valid CF ipad_sdr_din;
	schedule oapp_last_rd CF ipad_sdr_din;
	schedule oapp_last_wr CF ipad_sdr_din;
	schedule oapp_rd_data CF ipad_sdr_din;

    schedule iapp_req_wrap C iapp_req_wrap;
    schedule iapp_req_wrap CF iapp_req;
	schedule iapp_req_wrap CF iapp_req_addr;
	schedule iapp_req_wrap CF iapp_req_len;
	schedule iapp_req_wrap CF iapp_req_wr_n;
	schedule iapp_req_wrap CF iapp_wr_en_n;
	schedule iapp_req_wrap CF iapp_wr_data;
	schedule iapp_req_wrap CF icfg_sdr_tras_d;
	schedule iapp_req_wrap CF icfg_sdr_trp_d;
	schedule iapp_req_wrap CF icfg_sdr_trcd_d;
	schedule iapp_req_wrap CF icfg_sdr_en;
	schedule iapp_req_wrap CF icfg_req_depth;
	schedule iapp_req_wrap CF icfg_sdr_mode_reg;
	schedule iapp_req_wrap CF icfg_sdr_cas;
	schedule iapp_req_wrap CF icfg_sdr_trcar_d;
	schedule iapp_req_wrap CF icfg_sdr_twr_d;
	schedule iapp_req_wrap CF icfg_sdr_rfmax;
	schedule iapp_req_wrap CF icfg_sdr_rfsh;
	schedule osdr_cke CF iapp_req_wrap;
	schedule osdr_cs_n CF iapp_req_wrap;
	schedule osdr_ras_n CF iapp_req_wrap;
	schedule osdr_cas_n CF iapp_req_wrap;
	schedule osdr_we_n CF iapp_req_wrap;
	schedule osdr_dqm CF iapp_req_wrap;
	schedule osdr_ba CF iapp_req_wrap;
	schedule osdr_addr CF iapp_req_wrap;
	schedule osdr_init_done CF iapp_req_wrap;
	schedule oapp_req_ack CF iapp_req_wrap;
	schedule oapp_wr_next_req CF iapp_req_wrap;
	schedule oapp_rd_valid CF iapp_req_wrap;
	schedule oapp_last_rd CF iapp_req_wrap;
	schedule oapp_last_wr CF iapp_req_wrap;
	schedule oapp_rd_data CF iapp_req_wrap;
    schedule osdr_dout CF iapp_req_wrap;
    schedule osdr_den_n CF iapp_req_wrap; 
     
    schedule iapp_req C iapp_req;
	schedule iapp_req CF iapp_req_addr;
	schedule iapp_req CF iapp_req_len;
	schedule iapp_req CF iapp_req_wr_n;
	schedule iapp_req CF iapp_wr_en_n;
	schedule iapp_req CF iapp_wr_data;
	schedule iapp_req CF icfg_sdr_tras_d;
	schedule iapp_req CF icfg_sdr_trp_d;
	schedule iapp_req CF icfg_sdr_trcd_d;
	schedule iapp_req CF icfg_sdr_en;
	schedule iapp_req CF icfg_req_depth;
	schedule iapp_req CF icfg_sdr_mode_reg;
	schedule iapp_req CF icfg_sdr_cas;
	schedule iapp_req CF icfg_sdr_trcar_d;
	schedule iapp_req CF icfg_sdr_twr_d;
	schedule iapp_req CF icfg_sdr_rfmax;
	schedule iapp_req CF icfg_sdr_rfsh;
	schedule osdr_cke CF iapp_req;
	schedule osdr_cs_n CF iapp_req;
	schedule osdr_ras_n CF iapp_req;
	schedule osdr_cas_n CF iapp_req;
	schedule osdr_we_n CF iapp_req;
	schedule osdr_dqm CF iapp_req;
	schedule osdr_ba CF iapp_req;
	schedule osdr_addr CF iapp_req;
	schedule osdr_init_done CF iapp_req;
	schedule oapp_req_ack CF iapp_req;
	schedule oapp_wr_next_req CF iapp_req;
	schedule oapp_rd_valid CF iapp_req;
	schedule oapp_last_rd CF iapp_req;
	schedule oapp_last_wr CF iapp_req;
	schedule oapp_rd_data CF iapp_req;
    schedule osdr_dout CF iapp_req;
    schedule osdr_den_n CF iapp_req;

    schedule icfg_sdr_tras_d C icfg_sdr_tras_d;
	schedule icfg_sdr_tras_d CF icfg_sdr_trp_d;
	schedule icfg_sdr_tras_d CF icfg_sdr_trcd_d;
	schedule icfg_sdr_tras_d CF icfg_sdr_en;
	schedule icfg_sdr_tras_d CF icfg_req_depth;
	schedule icfg_sdr_tras_d CF icfg_sdr_mode_reg;
	schedule icfg_sdr_tras_d CF icfg_sdr_cas;
	schedule icfg_sdr_tras_d CF icfg_sdr_trcar_d;
	schedule icfg_sdr_tras_d CF icfg_sdr_twr_d;
	schedule icfg_sdr_tras_d CF icfg_sdr_rfsh;
	schedule icfg_sdr_tras_d CF icfg_sdr_rfmax;
	schedule icfg_sdr_tras_d CF iapp_req_addr;
	schedule icfg_sdr_tras_d CF iapp_req_len;
	schedule icfg_sdr_tras_d CF iapp_req_wr_n;
	schedule icfg_sdr_tras_d CF iapp_wr_en_n;
	schedule icfg_sdr_tras_d CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_tras_d;
	schedule osdr_cs_n CF icfg_sdr_tras_d;
	schedule osdr_ras_n CF icfg_sdr_tras_d;
	schedule osdr_cas_n CF icfg_sdr_tras_d;
	schedule osdr_we_n CF icfg_sdr_tras_d;
	schedule osdr_dqm CF icfg_sdr_tras_d;
	schedule osdr_ba CF icfg_sdr_tras_d;
	schedule osdr_addr CF icfg_sdr_tras_d;
	schedule osdr_init_done CF icfg_sdr_tras_d;
	schedule oapp_req_ack CF icfg_sdr_tras_d;
	schedule oapp_wr_next_req CF icfg_sdr_tras_d;
	schedule oapp_rd_valid CF icfg_sdr_tras_d;
	schedule oapp_last_rd CF icfg_sdr_tras_d;
	schedule oapp_last_wr CF icfg_sdr_tras_d;
	schedule oapp_rd_data CF icfg_sdr_tras_d;
    schedule osdr_dout CF icfg_sdr_tras_d;
    schedule osdr_den_n CF icfg_sdr_tras_d;


    schedule icfg_sdr_trp_d C icfg_sdr_trp_d;
	schedule icfg_sdr_trp_d CF icfg_sdr_trcd_d;
	schedule icfg_sdr_trp_d CF icfg_sdr_en;
	schedule icfg_sdr_trp_d CF icfg_req_depth;
	schedule icfg_sdr_trp_d CF icfg_sdr_mode_reg;
	schedule icfg_sdr_trp_d CF icfg_sdr_cas;
	schedule icfg_sdr_trp_d CF icfg_sdr_trcar_d;
	schedule icfg_sdr_trp_d CF icfg_sdr_twr_d;
	schedule icfg_sdr_trp_d CF icfg_sdr_rfsh;
	schedule icfg_sdr_trp_d CF icfg_sdr_rfmax;
	schedule icfg_sdr_trp_d CF iapp_req_addr;
	schedule icfg_sdr_trp_d CF iapp_req_len;
	schedule icfg_sdr_trp_d CF iapp_req_wr_n;
	schedule icfg_sdr_trp_d CF iapp_wr_en_n;
	schedule icfg_sdr_trp_d CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_trp_d;
	schedule osdr_cs_n CF icfg_sdr_trp_d;
	schedule osdr_ras_n CF icfg_sdr_trp_d;
	schedule osdr_cas_n CF icfg_sdr_trp_d;
	schedule osdr_we_n CF icfg_sdr_trp_d;
	schedule osdr_dqm CF icfg_sdr_trp_d;
	schedule osdr_ba CF icfg_sdr_trp_d;
	schedule osdr_addr CF icfg_sdr_trp_d;
	schedule osdr_init_done CF icfg_sdr_trp_d;
	schedule oapp_req_ack CF icfg_sdr_trp_d;
	schedule oapp_wr_next_req CF icfg_sdr_trp_d;
	schedule oapp_rd_valid CF icfg_sdr_trp_d;
	schedule oapp_last_rd CF icfg_sdr_trp_d;
	schedule oapp_last_wr CF icfg_sdr_trp_d;
	schedule oapp_rd_data CF icfg_sdr_trp_d;
    schedule osdr_dout CF icfg_sdr_trp_d;
    schedule osdr_den_n CF icfg_sdr_trp_d;



    schedule icfg_sdr_trcd_d C icfg_sdr_trcd_d;
	schedule icfg_sdr_trcd_d CF icfg_sdr_en;
	schedule icfg_sdr_trcd_d CF icfg_req_depth;
	schedule icfg_sdr_trcd_d CF icfg_sdr_mode_reg;
	schedule icfg_sdr_trcd_d CF icfg_sdr_cas;
	schedule icfg_sdr_trcd_d CF icfg_sdr_trcar_d;
	schedule icfg_sdr_trcd_d CF icfg_sdr_twr_d;
	schedule icfg_sdr_trcd_d CF icfg_sdr_rfsh;
	schedule icfg_sdr_trcd_d CF icfg_sdr_rfmax;
	schedule icfg_sdr_trcd_d CF iapp_req_addr;
	schedule icfg_sdr_trcd_d CF iapp_req_len;
	schedule icfg_sdr_trcd_d CF iapp_req_wr_n;
	schedule icfg_sdr_trcd_d CF iapp_wr_en_n;
	schedule icfg_sdr_trcd_d CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_trcd_d;
	schedule osdr_cs_n CF icfg_sdr_trcd_d;
	schedule osdr_ras_n CF icfg_sdr_trcd_d;
	schedule osdr_cas_n CF icfg_sdr_trcd_d;
	schedule osdr_we_n CF icfg_sdr_trcd_d;
	schedule osdr_dqm CF icfg_sdr_trcd_d;
	schedule osdr_ba CF icfg_sdr_trcd_d;
	schedule osdr_addr CF icfg_sdr_trcd_d;
	schedule osdr_init_done CF icfg_sdr_trcd_d;
	schedule oapp_req_ack CF icfg_sdr_trcd_d;
	schedule oapp_wr_next_req CF icfg_sdr_trcd_d;
	schedule oapp_rd_valid CF icfg_sdr_trcd_d;
	schedule oapp_last_rd CF icfg_sdr_trcd_d;
	schedule oapp_last_wr CF icfg_sdr_trcd_d;
	schedule oapp_rd_data CF icfg_sdr_trcd_d;
    schedule osdr_dout CF icfg_sdr_trcd_d;
    schedule osdr_den_n CF icfg_sdr_trcd_d;

    schedule icfg_sdr_en C icfg_sdr_en;
	schedule icfg_sdr_en CF icfg_req_depth;
	schedule icfg_sdr_en CF icfg_sdr_mode_reg;
	schedule icfg_sdr_en CF icfg_sdr_cas;
	schedule icfg_sdr_en CF icfg_sdr_trcar_d;
	schedule icfg_sdr_en CF icfg_sdr_twr_d;
	schedule icfg_sdr_en CF icfg_sdr_rfsh;
	schedule icfg_sdr_en CF icfg_sdr_rfmax;
	schedule icfg_sdr_en CF iapp_req_addr;
	schedule icfg_sdr_en CF iapp_req_len;
	schedule icfg_sdr_en CF iapp_req_wr_n;
	schedule icfg_sdr_en CF iapp_wr_en_n;
	schedule icfg_sdr_en CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_en;
	schedule osdr_cs_n CF icfg_sdr_en;
	schedule osdr_ras_n CF icfg_sdr_en;
	schedule osdr_cas_n CF icfg_sdr_en;
	schedule osdr_we_n CF icfg_sdr_en;
	schedule osdr_dqm CF icfg_sdr_en;
	schedule osdr_ba CF icfg_sdr_en;
	schedule osdr_addr CF icfg_sdr_en;
	schedule osdr_init_done CF icfg_sdr_en;
	schedule oapp_req_ack CF icfg_sdr_en;
	schedule oapp_wr_next_req CF icfg_sdr_en;
	schedule oapp_rd_valid CF icfg_sdr_en;
	schedule oapp_last_rd CF icfg_sdr_en;
	schedule oapp_last_wr CF icfg_sdr_en;
	schedule oapp_rd_data CF icfg_sdr_en;
    schedule osdr_dout CF icfg_sdr_en;
    schedule osdr_den_n CF icfg_sdr_en;

    schedule icfg_req_depth C icfg_req_depth;
	schedule icfg_req_depth CF icfg_sdr_mode_reg;
	schedule icfg_req_depth CF icfg_sdr_cas;
	schedule icfg_req_depth CF icfg_sdr_trcar_d;
	schedule icfg_req_depth CF icfg_sdr_twr_d;
	schedule icfg_req_depth CF icfg_sdr_rfsh;
	schedule icfg_req_depth CF icfg_sdr_rfmax;
	schedule icfg_req_depth CF iapp_req_addr;
	schedule icfg_req_depth CF iapp_req_len;
	schedule icfg_req_depth CF iapp_req_wr_n;
	schedule icfg_req_depth CF iapp_wr_en_n;
	schedule icfg_req_depth CF iapp_wr_data;
	schedule osdr_cke CF icfg_req_depth;
	schedule osdr_cs_n CF icfg_req_depth;
	schedule osdr_ras_n CF icfg_req_depth;
	schedule osdr_cas_n CF icfg_req_depth;
	schedule osdr_we_n CF icfg_req_depth;
	schedule osdr_dqm CF icfg_req_depth;
	schedule osdr_ba CF icfg_req_depth;
	schedule osdr_addr CF icfg_req_depth;
	schedule osdr_init_done CF icfg_req_depth;
	schedule oapp_req_ack CF icfg_req_depth;
	schedule oapp_wr_next_req CF icfg_req_depth;
	schedule oapp_rd_valid CF icfg_req_depth;
	schedule oapp_last_rd CF icfg_req_depth;
	schedule oapp_last_wr CF icfg_req_depth;
	schedule oapp_rd_data CF icfg_req_depth;
    schedule osdr_dout CF icfg_req_depth;
    schedule osdr_den_n CF icfg_req_depth;

    schedule icfg_sdr_mode_reg C icfg_sdr_mode_reg;
	schedule icfg_sdr_mode_reg CF icfg_sdr_cas;
	schedule icfg_sdr_mode_reg CF icfg_sdr_trcar_d;
	schedule icfg_sdr_mode_reg CF icfg_sdr_twr_d;
	schedule icfg_sdr_mode_reg CF icfg_sdr_rfsh;
	schedule icfg_sdr_mode_reg CF icfg_sdr_rfmax;
	schedule icfg_sdr_mode_reg CF iapp_req_addr;
	schedule icfg_sdr_mode_reg CF iapp_req_len;
	schedule icfg_sdr_mode_reg CF iapp_req_wr_n;
	schedule icfg_sdr_mode_reg CF iapp_wr_en_n;
	schedule icfg_sdr_mode_reg CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_mode_reg;
	schedule osdr_cs_n CF icfg_sdr_mode_reg;
	schedule osdr_ras_n CF icfg_sdr_mode_reg;
	schedule osdr_cas_n CF icfg_sdr_mode_reg;
	schedule osdr_we_n CF icfg_sdr_mode_reg;
	schedule osdr_dqm CF icfg_sdr_mode_reg;
	schedule osdr_ba CF icfg_sdr_mode_reg;
	schedule osdr_addr CF icfg_sdr_mode_reg;
	schedule osdr_init_done CF icfg_sdr_mode_reg;
	schedule oapp_req_ack CF icfg_sdr_mode_reg;
	schedule oapp_wr_next_req CF icfg_sdr_mode_reg;
	schedule oapp_rd_valid CF icfg_sdr_mode_reg;
	schedule oapp_last_rd CF icfg_sdr_mode_reg;
	schedule oapp_last_wr CF icfg_sdr_mode_reg;
	schedule oapp_rd_data CF icfg_sdr_mode_reg;
    schedule osdr_dout CF icfg_sdr_mode_reg;
    schedule osdr_den_n CF icfg_sdr_mode_reg;

    schedule icfg_sdr_cas C icfg_sdr_cas;
	schedule icfg_sdr_cas CF icfg_sdr_trcar_d;
	schedule icfg_sdr_cas CF icfg_sdr_twr_d;
	schedule icfg_sdr_cas CF icfg_sdr_rfsh;
	schedule icfg_sdr_cas CF icfg_sdr_rfmax;
	schedule icfg_sdr_cas CF iapp_req_addr;
	schedule icfg_sdr_cas CF iapp_req_len;
	schedule icfg_sdr_cas CF iapp_req_wr_n;
	schedule icfg_sdr_cas CF iapp_wr_en_n;
	schedule icfg_sdr_cas CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_cas;
	schedule osdr_cs_n CF icfg_sdr_cas;
	schedule osdr_ras_n CF icfg_sdr_cas;
	schedule osdr_cas_n CF icfg_sdr_cas;
	schedule osdr_we_n CF icfg_sdr_cas;
	schedule osdr_dqm CF icfg_sdr_cas;
	schedule osdr_ba CF icfg_sdr_cas;
	schedule osdr_addr CF icfg_sdr_cas;
	schedule osdr_init_done CF icfg_sdr_cas;
	schedule oapp_req_ack CF icfg_sdr_cas;
	schedule oapp_wr_next_req CF icfg_sdr_cas;
	schedule oapp_rd_valid CF icfg_sdr_cas;
	schedule oapp_last_rd CF icfg_sdr_cas;
	schedule oapp_last_wr CF icfg_sdr_cas;
	schedule oapp_rd_data CF icfg_sdr_cas;
    schedule osdr_dout CF icfg_sdr_cas;
    schedule osdr_den_n CF icfg_sdr_cas;

    schedule icfg_sdr_trcar_d C icfg_sdr_trcar_d;
	schedule icfg_sdr_trcar_d CF icfg_sdr_twr_d;
	schedule icfg_sdr_trcar_d CF icfg_sdr_rfsh;
	schedule icfg_sdr_trcar_d CF icfg_sdr_rfmax;
	schedule icfg_sdr_trcar_d CF iapp_req_addr;
	schedule icfg_sdr_trcar_d CF iapp_req_len;
	schedule icfg_sdr_trcar_d CF iapp_req_wr_n;
	schedule icfg_sdr_trcar_d CF iapp_wr_en_n;
	schedule icfg_sdr_trcar_d CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_trcar_d;
	schedule osdr_cs_n CF icfg_sdr_trcar_d;
	schedule osdr_ras_n CF icfg_sdr_trcar_d;
	schedule osdr_cas_n CF icfg_sdr_trcar_d;
	schedule osdr_we_n CF icfg_sdr_trcar_d;
	schedule osdr_dqm CF icfg_sdr_trcar_d;
	schedule osdr_ba CF icfg_sdr_trcar_d;
	schedule osdr_addr CF icfg_sdr_trcar_d;
	schedule osdr_init_done CF icfg_sdr_trcar_d;
	schedule oapp_req_ack CF icfg_sdr_trcar_d;
	schedule oapp_wr_next_req CF icfg_sdr_trcar_d;
	schedule oapp_rd_valid CF icfg_sdr_trcar_d;
	schedule oapp_last_rd CF icfg_sdr_trcar_d;
	schedule oapp_last_wr CF icfg_sdr_trcar_d;
	schedule oapp_rd_data CF icfg_sdr_trcar_d;
    schedule osdr_dout CF icfg_sdr_trcar_d;
    schedule osdr_den_n CF icfg_sdr_trcar_d;

    schedule icfg_sdr_twr_d C icfg_sdr_twr_d;
	schedule icfg_sdr_twr_d CF icfg_sdr_rfsh;
	schedule icfg_sdr_twr_d CF icfg_sdr_rfmax;
	schedule icfg_sdr_twr_d CF iapp_req_addr;
	schedule icfg_sdr_twr_d CF iapp_req_len;
	schedule icfg_sdr_twr_d CF iapp_req_wr_n;
	schedule icfg_sdr_twr_d CF iapp_wr_en_n;
	schedule icfg_sdr_twr_d CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_twr_d;
	schedule osdr_cs_n CF icfg_sdr_twr_d;
	schedule osdr_ras_n CF icfg_sdr_twr_d;
	schedule osdr_cas_n CF icfg_sdr_twr_d;
	schedule osdr_we_n CF icfg_sdr_twr_d;
	schedule osdr_dqm CF icfg_sdr_twr_d;
	schedule osdr_ba CF icfg_sdr_twr_d;
	schedule osdr_addr CF icfg_sdr_twr_d;
	schedule osdr_init_done CF icfg_sdr_twr_d;
	schedule oapp_req_ack CF icfg_sdr_twr_d;
	schedule oapp_wr_next_req CF icfg_sdr_twr_d;
	schedule oapp_rd_valid CF icfg_sdr_twr_d;
	schedule oapp_last_rd CF icfg_sdr_twr_d;
	schedule oapp_last_wr CF icfg_sdr_twr_d;
	schedule oapp_rd_data CF icfg_sdr_twr_d;
    schedule osdr_dout CF icfg_sdr_twr_d;
    schedule osdr_den_n CF icfg_sdr_twr_d;

    schedule icfg_sdr_rfsh C icfg_sdr_rfsh;
	schedule icfg_sdr_rfsh CF icfg_sdr_rfmax;
	schedule icfg_sdr_rfsh CF iapp_req_addr;
	schedule icfg_sdr_rfsh CF iapp_req_len;
	schedule icfg_sdr_rfsh CF iapp_req_wr_n;
	schedule icfg_sdr_rfsh CF iapp_wr_en_n;
	schedule icfg_sdr_rfsh CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_rfsh;
	schedule osdr_cs_n CF icfg_sdr_rfsh;
	schedule osdr_ras_n CF icfg_sdr_rfsh;
	schedule osdr_cas_n CF icfg_sdr_rfsh;
	schedule osdr_we_n CF icfg_sdr_rfsh;
	schedule osdr_dqm CF icfg_sdr_rfsh;
	schedule osdr_ba CF icfg_sdr_rfsh;
	schedule osdr_addr CF icfg_sdr_rfsh;
	schedule osdr_init_done CF icfg_sdr_rfsh;
	schedule oapp_req_ack CF icfg_sdr_rfsh;
	schedule oapp_wr_next_req CF icfg_sdr_rfsh;
	schedule oapp_rd_valid CF icfg_sdr_rfsh;
	schedule oapp_last_rd CF icfg_sdr_rfsh;
	schedule oapp_last_wr CF icfg_sdr_rfsh;
	schedule oapp_rd_data CF icfg_sdr_rfsh;
    schedule osdr_dout CF icfg_sdr_rfsh;
    schedule osdr_den_n CF icfg_sdr_rfsh;

    schedule icfg_sdr_rfmax C icfg_sdr_rfmax;
	schedule icfg_sdr_rfmax CF iapp_req_addr;
	schedule icfg_sdr_rfmax CF iapp_req_len;
	schedule icfg_sdr_rfmax CF iapp_req_wr_n;
	schedule icfg_sdr_rfmax CF iapp_wr_en_n;
	schedule icfg_sdr_rfmax CF iapp_wr_data;
	schedule osdr_cke CF icfg_sdr_rfmax;
	schedule osdr_cs_n CF icfg_sdr_rfmax;
	schedule osdr_ras_n CF icfg_sdr_rfmax;
	schedule osdr_cas_n CF icfg_sdr_rfmax;
	schedule osdr_we_n CF icfg_sdr_rfmax;
	schedule osdr_dqm CF icfg_sdr_rfmax;
	schedule osdr_ba CF icfg_sdr_rfmax;
	schedule osdr_addr CF icfg_sdr_rfmax;
	schedule osdr_init_done CF icfg_sdr_rfmax;
	schedule oapp_req_ack CF icfg_sdr_rfmax;
	schedule oapp_wr_next_req CF icfg_sdr_rfmax;
	schedule oapp_rd_valid CF icfg_sdr_rfmax;
	schedule oapp_last_rd CF icfg_sdr_rfmax;
	schedule oapp_last_wr CF icfg_sdr_rfmax;
	schedule oapp_rd_data CF icfg_sdr_rfmax;
    schedule osdr_dout CF icfg_sdr_rfmax;
    schedule osdr_den_n CF icfg_sdr_rfmax;

    schedule iapp_req_addr C iapp_req_addr;
	schedule iapp_req_addr CF iapp_req_len;
	schedule iapp_req_addr CF iapp_req_wr_n;
	schedule iapp_req_addr CF iapp_wr_en_n;
	schedule iapp_req_addr CF iapp_wr_data;
	schedule osdr_cke CF iapp_req_addr;
	schedule osdr_cs_n CF iapp_req_addr;
	schedule osdr_ras_n CF iapp_req_addr;
	schedule osdr_cas_n CF iapp_req_addr;
	schedule osdr_we_n CF iapp_req_addr;
	schedule osdr_dqm CF iapp_req_addr;
	schedule osdr_ba CF iapp_req_addr;
	schedule osdr_addr CF iapp_req_addr;
	schedule osdr_init_done CF iapp_req_addr;
	schedule oapp_req_ack CF iapp_req_addr;
	schedule oapp_wr_next_req CF iapp_req_addr;
	schedule oapp_rd_valid CF iapp_req_addr;
	schedule oapp_last_rd CF iapp_req_addr;
	schedule oapp_last_wr CF iapp_req_addr;
	schedule oapp_rd_data CF iapp_req_addr;
    schedule osdr_dout CF iapp_req_addr;
    schedule osdr_den_n CF iapp_req_addr;

    schedule iapp_req_len C iapp_req_len;
	schedule iapp_req_len CF iapp_req_wr_n;
	schedule iapp_req_len CF iapp_wr_en_n;
	schedule iapp_req_len CF iapp_wr_data;
	schedule osdr_cke CF iapp_req_len;
	schedule osdr_cs_n CF iapp_req_len;
	schedule osdr_ras_n CF iapp_req_len;
	schedule osdr_cas_n CF iapp_req_len;
	schedule osdr_we_n CF iapp_req_len;
	schedule osdr_dqm CF iapp_req_len;
	schedule osdr_ba CF iapp_req_len;
	schedule osdr_addr CF iapp_req_len;
	schedule osdr_init_done CF iapp_req_len;
	schedule oapp_req_ack CF iapp_req_len;
	schedule oapp_wr_next_req CF iapp_req_len;
	schedule oapp_rd_valid CF iapp_req_len;
	schedule oapp_last_rd CF iapp_req_len;
	schedule oapp_last_wr CF iapp_req_len;
	schedule oapp_rd_data CF iapp_req_len;
    schedule osdr_dout CF iapp_req_len;
    schedule osdr_den_n CF iapp_req_len;

    schedule iapp_req_wr_n C iapp_req_wr_n;
	schedule iapp_req_wr_n CF iapp_wr_en_n;
	schedule iapp_req_wr_n CF iapp_wr_data;
	schedule osdr_cke CF iapp_req_wr_n;
	schedule osdr_cs_n CF iapp_req_wr_n;
	schedule osdr_ras_n CF iapp_req_wr_n;
	schedule osdr_cas_n CF iapp_req_wr_n;
	schedule osdr_we_n CF iapp_req_wr_n;
	schedule osdr_dqm CF iapp_req_wr_n;
	schedule osdr_ba CF iapp_req_wr_n;
	schedule osdr_addr CF iapp_req_wr_n;
	schedule osdr_init_done CF iapp_req_wr_n;
	schedule oapp_req_ack CF iapp_req_wr_n;
	schedule oapp_wr_next_req CF iapp_req_wr_n;
	schedule oapp_rd_valid CF iapp_req_wr_n;
	schedule oapp_last_rd CF iapp_req_wr_n;
	schedule oapp_last_wr CF iapp_req_wr_n;
	schedule oapp_rd_data CF iapp_req_wr_n;
    schedule osdr_dout CF iapp_req_wr_n;
    schedule osdr_den_n CF iapp_req_wr_n;

    schedule iapp_wr_en_n C iapp_wr_en_n;
	schedule iapp_wr_en_n CF iapp_wr_data;
	schedule osdr_cke CF iapp_wr_en_n;
	schedule osdr_cs_n CF iapp_wr_en_n;
	schedule osdr_ras_n CF iapp_wr_en_n;
	schedule osdr_cas_n CF iapp_wr_en_n;
	schedule osdr_we_n CF iapp_wr_en_n;
	schedule osdr_dqm CF iapp_wr_en_n;
	schedule osdr_ba CF iapp_wr_en_n;
	schedule osdr_addr CF iapp_wr_en_n;
	schedule osdr_init_done CF iapp_wr_en_n;
	schedule oapp_req_ack CF iapp_wr_en_n;
	schedule oapp_wr_next_req CF iapp_wr_en_n;
	schedule oapp_rd_valid CF iapp_wr_en_n;
	schedule oapp_last_rd CF iapp_wr_en_n;
	schedule oapp_last_wr CF iapp_wr_en_n;
	schedule oapp_rd_data CF iapp_wr_en_n;
    schedule osdr_dout CF iapp_wr_en_n;
    schedule osdr_den_n CF iapp_wr_en_n;

    schedule iapp_wr_data C iapp_wr_data;
	schedule osdr_cke CF iapp_wr_data;
	schedule osdr_cs_n CF iapp_wr_data;
	schedule osdr_ras_n CF iapp_wr_data;
	schedule osdr_cas_n CF iapp_wr_data;
	schedule osdr_we_n CF iapp_wr_data;
	schedule osdr_dqm CF iapp_wr_data;
	schedule osdr_ba CF iapp_wr_data;
	schedule osdr_addr CF iapp_wr_data;
	schedule osdr_init_done CF iapp_wr_data;
	schedule oapp_req_ack CF iapp_wr_data;
	schedule oapp_wr_next_req CF iapp_wr_data;
	schedule oapp_rd_valid CF iapp_wr_data;
	schedule oapp_last_rd CF iapp_wr_data;
	schedule oapp_last_wr CF iapp_wr_data;
	schedule oapp_rd_data CF iapp_wr_data;
    schedule osdr_dout CF iapp_wr_data;
    schedule osdr_den_n CF iapp_wr_data;

	schedule osdr_cke CF osdr_cke;
	schedule osdr_cke CF osdr_cs_n;
	schedule osdr_cke CF osdr_ras_n;
	schedule osdr_cke CF osdr_cas_n;
	schedule osdr_cke CF osdr_we_n;
	schedule osdr_cke CF osdr_dqm;
	schedule osdr_cke CF osdr_ba;
	schedule osdr_cke CF osdr_addr;
	schedule osdr_cke CF osdr_init_done;
	schedule osdr_cke CF oapp_req_ack;
	schedule osdr_cke CF oapp_wr_next_req;
	schedule osdr_cke CF oapp_rd_valid;
	schedule osdr_cke CF oapp_last_rd;
	schedule osdr_cke CF oapp_last_wr;
	schedule osdr_cke CF oapp_rd_data;
	schedule osdr_cs_n CF osdr_cs_n;
	schedule osdr_cs_n CF osdr_ras_n;
	schedule osdr_cs_n CF osdr_cas_n;
	schedule osdr_cs_n CF osdr_we_n;
	schedule osdr_cs_n CF osdr_dqm;
	schedule osdr_cs_n CF osdr_ba;
	schedule osdr_cs_n CF osdr_addr;
	schedule osdr_cs_n CF osdr_init_done;
	schedule osdr_cs_n CF oapp_req_ack;
	schedule osdr_cs_n CF oapp_wr_next_req;
	schedule osdr_cs_n CF oapp_rd_valid;
	schedule osdr_cs_n CF oapp_last_rd;
	schedule osdr_cs_n CF oapp_last_wr;
	schedule osdr_cs_n CF oapp_rd_data;
	schedule osdr_ras_n CF osdr_ras_n;
	schedule osdr_ras_n CF osdr_cas_n;
	schedule osdr_ras_n CF osdr_we_n;
	schedule osdr_ras_n CF osdr_dqm;
	schedule osdr_ras_n CF osdr_ba;
	schedule osdr_ras_n CF osdr_addr;
	schedule osdr_ras_n CF osdr_init_done;
	schedule osdr_ras_n CF oapp_req_ack;
	schedule osdr_ras_n CF oapp_wr_next_req;
	schedule osdr_ras_n CF oapp_rd_valid;
	schedule osdr_ras_n CF oapp_last_rd;
	schedule osdr_ras_n CF oapp_last_wr;
	schedule osdr_ras_n CF oapp_rd_data;
	schedule osdr_cas_n CF osdr_cas_n;
	schedule osdr_cas_n CF osdr_we_n;
	schedule osdr_cas_n CF osdr_dqm;
	schedule osdr_cas_n CF osdr_ba;
	schedule osdr_cas_n CF osdr_addr;
	schedule osdr_cas_n CF osdr_init_done;
	schedule osdr_cas_n CF oapp_req_ack;
	schedule osdr_cas_n CF oapp_wr_next_req;
	schedule osdr_cas_n CF oapp_rd_valid;
	schedule osdr_cas_n CF oapp_last_rd;
	schedule osdr_cas_n CF oapp_last_wr;
	schedule osdr_cas_n CF oapp_rd_data;
	schedule osdr_we_n CF osdr_we_n;
	schedule osdr_we_n CF osdr_dqm;
	schedule osdr_we_n CF osdr_ba;
	schedule osdr_we_n CF osdr_addr;
	schedule osdr_we_n CF osdr_init_done;
	schedule osdr_we_n CF oapp_req_ack;
	schedule osdr_we_n CF oapp_wr_next_req;
	schedule osdr_we_n CF oapp_rd_valid;
	schedule osdr_we_n CF oapp_last_rd;
	schedule osdr_we_n CF oapp_last_wr;
	schedule osdr_we_n CF oapp_rd_data;
	schedule osdr_dqm CF osdr_dqm;
	schedule osdr_dqm CF osdr_ba;
	schedule osdr_dqm CF osdr_addr;
	schedule osdr_dqm CF osdr_init_done;
	schedule osdr_dqm CF oapp_req_ack;
	schedule osdr_dqm CF oapp_wr_next_req;
	schedule osdr_dqm CF oapp_rd_valid;
	schedule osdr_dqm CF oapp_last_rd;
	schedule osdr_dqm CF oapp_last_wr;
	schedule osdr_dqm CF oapp_rd_data;
	schedule osdr_ba CF osdr_ba;
	schedule osdr_ba CF osdr_addr;
	schedule osdr_ba CF osdr_init_done;
	schedule osdr_ba CF oapp_req_ack;
	schedule osdr_ba CF oapp_wr_next_req;
	schedule osdr_ba CF oapp_rd_valid;
	schedule osdr_ba CF oapp_last_rd;
	schedule osdr_ba CF oapp_last_wr;
	schedule osdr_ba CF oapp_rd_data;
	schedule osdr_addr CF osdr_addr;
	schedule osdr_addr CF osdr_init_done;
	schedule osdr_addr CF oapp_req_ack;
	schedule osdr_addr CF oapp_wr_next_req;
	schedule osdr_addr CF oapp_rd_valid;
	schedule osdr_addr CF oapp_last_rd;
	schedule osdr_addr CF oapp_last_wr;
	schedule osdr_addr CF oapp_rd_data;
	schedule osdr_init_done CF osdr_init_done;
	schedule osdr_init_done CF oapp_req_ack;
	schedule osdr_init_done CF oapp_wr_next_req;
	schedule osdr_init_done CF oapp_rd_valid;
	schedule osdr_init_done CF oapp_last_rd;
	schedule osdr_init_done CF oapp_last_wr;
	schedule osdr_init_done CF oapp_rd_data;
	schedule oapp_req_ack CF oapp_req_ack;
	schedule oapp_req_ack CF oapp_wr_next_req;
	schedule oapp_req_ack CF oapp_rd_valid;
	schedule oapp_req_ack CF oapp_last_rd;
	schedule oapp_req_ack CF oapp_last_wr;
	schedule oapp_req_ack CF oapp_rd_data;
	schedule oapp_wr_next_req CF oapp_wr_next_req;
	schedule oapp_wr_next_req CF oapp_rd_valid;
	schedule oapp_wr_next_req CF oapp_last_rd;
	schedule oapp_wr_next_req CF oapp_last_wr;
	schedule oapp_wr_next_req CF oapp_rd_data;
	schedule oapp_rd_valid CF oapp_rd_valid;
	schedule oapp_rd_valid CF oapp_last_rd;
	schedule oapp_rd_valid CF oapp_last_wr;
	schedule oapp_rd_valid CF oapp_rd_data;
	schedule oapp_last_rd CF oapp_last_rd;
	schedule oapp_last_rd CF oapp_last_wr;
	schedule oapp_last_rd CF oapp_rd_data;
	schedule oapp_last_wr CF oapp_last_wr;
	schedule oapp_last_wr CF oapp_rd_data;
	schedule oapp_rd_data CF oapp_rd_data;
endmodule

