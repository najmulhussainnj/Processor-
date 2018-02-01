/*********************************************************************
                                                              
  SDRAM Controller top File                                  
                                                              
  This file is part of the sdram controller project           
  http://www.opencores.org/cores/sdr_ctrl/                    
                                                              
  Description: SDRAM Controller Top Module.
    Support 81/6/32 Bit SDRAM.
    Column Address is Programmable
    Bank Bit are 2 Bit
    Row Bits are 12 Bits

    This block integrate following sub modules

    sdrc_core   
        SDRAM Controller file
    wb2sdrc    
        This module transalate the bus protocl from wishbone to custome
	sdram controller
                                                              
  To Do:                                                      
    nothing                                                   
                                                              
  Author(s): Dinesh Annayya, dinesha@opencores.org                 
  Version  : 0.0 - 8th Jan 2012
                Initial version with 16/32 Bit SDRAM Support
           : 0.1 - 24th Jan 2012
	         8 Bit SDRAM Support is added
	     0.2 - 31st Jan 2012
	         sdram_dq and sdram_pad_clk are internally generated
	     0.3 - 26th April 2013
                  Sdram Address witdh is increased from 12 to 13bits

                                                             
 Copyright (C) 2000 Authors and OPENCORES.ORG                
                                                             
 This source file may be used and distributed without         
 restriction provided that this copyright statement is not    
 removed from the file and that any derivative work contains  
 the original copyright notice and the associated disclaimer. 
                                                              
 This source file is free software; you can redistribute it   
 and/or modify it under the terms of the GNU Lesser General   
 Public License as published by the Free Software Foundation; 
 either version 2.1 of the License, or (at your option) any   
later version.                                               
                                                              
 This source is distributed in the hope that it will be       
 useful, but WITHOUT ANY WARRANTY; without even the implied   
 warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      
 PURPOSE.  See the GNU Lesser General Public License for more 
 details.                                                     
                                                              
 You should have received a copy of the GNU Lesser General    
 Public License along with this source; if not, download it   
 from http://www.opencores.org/lgpl.shtml                     
                                                              
*******************************************************************/


`timescale 1ns/1ps
module sdrc_top 
           (
                   cfg_sdr_width       ,
                   cfg_colbits         ,
                    
                // Input to sdram ctrl
                    app_req        ,                   
                    app_req_addr   ,
                    app_req_len    ,
                    app_req_wr_n   ,
                    app_req_wrap   ,
                    app_req_ack    ,                  
//                  app_busy_n     ,                   
                    app_wr_en_n    ,                   
                    app_wr_next_req,                   
                    app_rd_valid   ,                   
                    app_last_rd    ,                    
                    app_last_wr,
                    app_wr_data    ,
                    app_rd_data    ,
                    delay_config_reg2,

		/* Interface to SDRAMs */
                    sdram_clk           ,
                    sdram_resetn        ,
                    sdr_cs_n            ,
                    sdr_cke             ,
                    sdr_ras_n           ,
                    sdr_cas_n           ,
                    sdr_we_n            ,
                    sdr_dqm             ,
                    sdr_ba              ,
                    sdr_addr            , 
                    pad_sdr_din         , // SDRA Data Input
                    sdr_dout            , // SDRAM Data Output
                    sdr_den_n           , // SDRAM Data Output enable
                    
		/* Parameters */
                    sdr_init_done       ,
                    cfg_req_depth       ,	        //how many req. buffer should hold
                    cfg_sdr_en          ,
                    cfg_sdr_mode_reg    ,
                    cfg_sdr_tras_d      ,
                    cfg_sdr_trp_d       ,
                    cfg_sdr_trcd_d      ,
                    cfg_sdr_cas         ,
                    cfg_sdr_trcar_d     ,
                    cfg_sdr_twr_d       ,
                    cfg_sdr_rfsh        ,
    	            cfg_sdr_rfmax
	    );
  
  `define SDR_REQ_ID_W       4

`define SDR_RFSH_TIMER_W    12
`define SDR_RFSH_ROW_CNT_W   3

// B2X Command

`define OP_PRE           2'b00
`define OP_ACT           2'b01
`define OP_RD            2'b10
`define OP_WR            2'b11

// SDRAM Commands (CS_N, RAS_N, CAS_N, WE_N)

`define SDR_DESEL        4'b1111
`define SDR_NOOP         4'b0111
`define SDR_ACTIVATE     4'b0011
`define SDR_READ         4'b0101
`define SDR_WRITE        4'b0100
`define SDR_BT           4'b0110
`define SDR_PRECHARGE    4'b0010
`define SDR_REFRESH      4'b0001
`define SDR_MODE         4'b0000

`define  ASIC            1'b1
`define  FPGA            1'b0
`define  TARGET_DESIGN   `ASIC
// 12 bit subtractor is not feasibile for FPGA, so changed to 6 bits
`define  REQ_BW    (`TARGET_DESIGN == `FPGA) ? 6 : 12   //  Request Width

parameter      APP_AW   = 26;  // Application Address Width
parameter      APP_DW   = 64;  // Application Data Width 
parameter      APP_BW   = 8;   // Application Byte Width
parameter      APP_RW   = 9;   // Application Request Width

parameter      SDR_DW   = 64;  // SDR Data Width 
parameter      SDR_BW   = 8;   // SDR Byte Width
             
parameter      dw       = 64;  // data width
parameter      tw       = 8;   // tag id width
parameter      bl       = 9;   // burst_lenght_width 

//-----------------------------------------------
// Global Variable
// ----------------------------------------------
input                   sdram_clk          ; // SDRAM Clock 
input                   sdram_resetn       ; // Reset Signal
input [1:0]             cfg_sdr_width      ; // 2'b00 - 32 Bit SDR, 2'b01 - 16 Bit SDR, 2'b1x - 8 Bit
input [1:0]             cfg_colbits        ; // 2'b00 - 8 Bit column address, 
                                             // 2'b01 - 9 Bit, 10 - 10 bit, 11 - 11Bits

//--------------------------------------
// Wish Bone Interface
// -------------------------------------      
//input                   wb_rst_i           ;
//input                   wb_clk_i           ;
//
//input                   wb_stb_i           ;
//output                  wb_ack_o           ;
//input [APP_AW-1:0]            wb_addr_i          ;
//input                   wb_we_i            ; // 1 - Write, 0 - Read
//input [dw-1:0]          wb_dat_i           ;
//input [dw/8-1:0]        wb_sel_i           ; // Byte enable
//output [dw-1:0]         wb_dat_o           ;
//input                   wb_cyc_i           ;
//input  [2:0]            wb_cti_i           ;

//------------------------------------------------
// Interface to SDRAMs
//------------------------------------------------
output                  sdr_cke             ; // SDRAM CKE
output 			        sdr_cs_n            ; // SDRAM Chip Select
output                  sdr_ras_n           ; // SDRAM ras
output                  sdr_cas_n           ; // SDRAM cas
output			        sdr_we_n            ; // SDRAM write enable
output [SDR_BW-1:0] 	sdr_dqm             ; // SDRAM Data Mask
output [1:0] 		    sdr_ba              ; // SDRAM Bank Enable
output [12:0] 		    sdr_addr            ; // SDRAM Address
input [SDR_DW-1:0] 	    pad_sdr_din         ; // SDRA Data Input
output [SDR_DW-1:0] 	sdr_dout            ; // SDRAM Data Output
output [SDR_BW-1:0] 	sdr_den_n           ; // SDRAM Data Output enable
//------------------------------------------------
// Configuration Parameter // vis: changed input and output pins to wire
//------------------------------------------------

output                           sdr_init_done       ; // Indicate SDRAM Initialisation Done
input [3:0] 	                 cfg_sdr_tras_d      ; // Active to precharge delay
input [3:0]                      cfg_sdr_trp_d       ; // Precharge to active delay
input [3:0]                      cfg_sdr_trcd_d      ; // Active to R/W delay
input 			                 cfg_sdr_en          ; // Enable SDRAM controller
input [1:0] 		             cfg_req_depth       ; // Maximum Request accepted by SDRAM controller
input [12:0]       		         cfg_sdr_mode_reg    ;
input [2:0] 			         cfg_sdr_cas         ; // SDRAM CAS Latency
input [3:0]       			     cfg_sdr_trcar_d     ; // Auto-refresh period
input [3:0]                      cfg_sdr_twr_d       ; // Write recovery delay
input [`SDR_RFSH_TIMER_W-1 : 0]  cfg_sdr_rfsh;
input [`SDR_RFSH_ROW_CNT_W -1 : 0] cfg_sdr_rfmax;


/*
assign cfg_req_depth  = 2'h3;	        //how many req. buffer should hold
assign cfg_sdr_en = 1'b1;
assign cfg_sdr_mode_reg = 13'h033;
assign cfg_sdr_tras_d = 4'h4;
assign cfg_sdr_trp_d = 4'h2;
assign cfg_sdr_trcd_d = 4'h2;
assign cfg_sdr_cas = 3'h3;
assign cfg_sdr_trcar_d = 4'h7;
assign cfg_sdr_twr_d = 4'h1;
assign cfg_sdr_rfsh = 12'h100; // reduced from 12'hC35
assign cfg_sdr_rfmax = 3'h6;*/
//assign cfg_colbits = 2'b00;
//assign cfg_sdr_width = 2'b00;

//--------------------------------------------
// SDRAM controller Interface 
//--------------------------------------------
input                  app_req            ; // SDRAM request
input                  app_req_wrap       ; // SDRAM wrap 
input [APP_AW-1:0]     app_req_addr       ; // SDRAM Request Address
input [bl-1:0]         app_req_len        ;
input                  app_req_wr_n       ; // 0 - Write, 1 -> Read
output                 app_req_ack        ; // SDRAM request Accepted
wire                   app_busy_n         ; // 0 -> sdr busy
input [dw/8-1:0]       app_wr_en_n        ; // Active low sdr byte-wise write data valid
output                 app_wr_next_req    ; // Ready to accept the next write
output                 app_rd_valid       ; // sdr read valid
output                 app_last_rd        ; // Indicate last Read of Burst Transfer
output                 app_last_wr        ; // Indicate last Write of Burst Transfer
input [dw-1:0]         app_wr_data        ; // sdr write data
output  [dw-1:0]       app_rd_data        ; // sdr read data

input [3:0] delay_config_reg2;

/****************************************
*  These logic has to be implemented using Pads
*  **************************************/
/*wire  [SDR_DW-1:0]    pad_sdr_din         ; // SDRA Data Input
wire  [SDR_DW-1:0]    sdr_dout            ; // SDRAM Data Output
wire  [SDR_BW-1:0]    sdr_den_n           ; // SDRAM Data Output enable
wire [SDR_DW-1:0] 	  sdr_dq              ; // SDRA Data Input/output*/

/*assign sdr_dq0 = sdr_dq[31:0];
assign sdr_dq1 = sdr_dq[63:32];


assign   sdr_dq = (&sdr_den_n == 1'b0) ? sdr_dout :  {SDR_DW{1'bz}}; 
assign   pad_sdr_din = {sdr_dq1, sdr_dq0};*/



// sdram pad clock is routed back through pad
// SDRAM Clock from Pad, used for registering Read Data
wire sdram_pad_clk;
parallel_prog_delay_cell delay_inst_for_sdram_clk_pad(.in_clk(sdram_clk), .delay_config_reg(delay_config_reg2), .delayed_clk(sdram_pad_clk));



sdrc_core #(.SDR_DW(SDR_DW) , .SDR_BW(SDR_BW)) u_sdrc_core (
          .clk                (sdram_clk          ) ,
          .pad_clk            (sdram_pad_clk      ) ,
          .reset_n            (sdram_resetn       ) ,
          .sdr_width          (cfg_sdr_width      ) ,
          .cfg_colbits        (cfg_colbits        ) ,

 		/* Request from app */
          .app_req            (app_req            ) ,// Transfer Request
          .app_req_addr       (app_req_addr       ) ,// SDRAM Address
          .app_req_len        (app_req_len        ) ,// Burst Length (in 16 bit words)
          .app_req_wrap       (app_req_wrap       ) ,// Wrap mode request 
          .app_req_wr_n	      (app_req_wr_n       ) ,// 0 => Write request, 1 => read req
          .app_req_ack        (app_req_ack        ) ,// Request has been accepted
          .cfg_req_depth      (cfg_req_depth      ) ,//how many req. buffer should hold
 		
          .app_wr_data        (app_wr_data        ) ,
          .app_wr_en_n        (app_wr_en_n        ) ,
          .app_rd_data        (app_rd_data        ) ,
          .app_rd_valid       (app_rd_valid       ) ,
		  .app_last_rd        (app_last_rd        ) ,
          .app_last_wr        (app_last_wr        ) ,
          .app_wr_next_req    (app_wr_next_req    ) ,
          .sdr_init_done      (sdr_init_done      ) ,
          .app_req_dma_last   (app_req            ) ,
 
 		/* Interface to SDRAMs */
          .sdr_cs_n           (sdr_cs_n           ) ,
          .sdr_cke            (sdr_cke            ) ,
          .sdr_ras_n          (sdr_ras_n          ) ,
          .sdr_cas_n          (sdr_cas_n          ) ,
          .sdr_we_n           (sdr_we_n           ) ,
          .sdr_dqm            (sdr_dqm            ) ,
          .sdr_ba             (sdr_ba             ) ,
          .sdr_addr           (sdr_addr           ) , 
          .pad_sdr_din        (pad_sdr_din        ) ,
          .sdr_dout           (sdr_dout           ) ,
          .sdr_den_n          (sdr_den_n          ) ,
 
 		/* Parameters */
          .cfg_sdr_en         (cfg_sdr_en         ) ,
          .cfg_sdr_mode_reg   (cfg_sdr_mode_reg   ) ,
          .cfg_sdr_tras_d     (cfg_sdr_tras_d     ) ,
          .cfg_sdr_trp_d      (cfg_sdr_trp_d      ) ,
          .cfg_sdr_trcd_d     (cfg_sdr_trcd_d     ) ,
          .cfg_sdr_cas        (cfg_sdr_cas        ) ,
          .cfg_sdr_trcar_d    (cfg_sdr_trcar_d    ) ,
          .cfg_sdr_twr_d      (cfg_sdr_twr_d      ) ,
          .cfg_sdr_rfsh       (cfg_sdr_rfsh       ) ,
          .cfg_sdr_rfmax      (cfg_sdr_rfmax      ) 
	       );
   
endmodule // sdrc_core
