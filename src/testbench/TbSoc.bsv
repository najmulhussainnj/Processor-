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
package TbSoc;
	`include "defines.bsv"
	`include "defined_parameters.bsv"
	import defined_types::*;
	import Soc::*;
	import GetPut            ::*;
	`ifdef Debug import DebugModule::*; `endif
	import Uart16550::*;
	import RS232_modified::*;
	import Clocks::*;
        import RBB_Shakti::*;
	import FIFO:: *;
	import Vector::*;
	import SpecialFIFOs::*;
	import DReg::*;
	import Clocks::*;
	import Connectable::*;
	import TriState::*;
	import slow_peripherals::*;
        `ifdef VME
		import Memory_vme_32::*;
		import Memory_vme_16::*;
		import vme_master::*;
	`endif
         `ifdef SDRAM	
		import bsvmksdram_model_wrapper::*;
		import sdr_top::*;
	`endif
    `ifdef FlexBus_verify
         import Semi_FIFOF :: *;
         
         import AXI4_Types   :: *;
         import FlexBus_Types :: *;
         import FlexBus_Slave_to_AXI4_Master_Fabric_Types :: *;
         import Memory_AXI4 :: *;
    `endif
   	`ifdef QSPI0 
			import qspi::*;
         `ifdef micron
				import bsvmkMicronFlashWrapper::*;
			`else
				import bsvmkCypressFlashWrapper::*;
          `endif
		`endif
		`ifdef I2C0
			import I2C_top			 :: *;
                        import bsvmkM24AA1025 ::*;
		`endif
		`ifdef PLIC
			import gpio				::*;
			import plic				::*;
		`endif
		`ifdef HYPER
			import hyperflash_bsv_wrapper ::*;
		`endif
		`ifdef AXIEXP
			import sample_axiexpslave::*;
		`endif

	module mkTbSoc(Empty);

		MakeClockIfc#(Bit#(1)) tck_clk <-mkUngatedClock(1);
		MakeResetIfc trst <-mkReset(0,False,tck_clk.new_clk);

		`ifdef Openocd
			Bit#(1) reset_vector_switch=0; // if 1 then go to bootrom. 0 go to debug module
		`else 
			Bit#(1) reset_vector_switch=1; // if 1 then go to bootrom. 0 go to debug module
		`endif
		Bit#(39) reset_vector=reset_vector_switch==1?'h1000:0;  // default boot-rom address.

		Clock defaultclk <- exposeCurrentClock;
		Reset defaultreset<-exposeCurrentReset;
		ClockDividerIfc clkdiv <-mkClockDivider(3,clocked_by defaultclk);
		Clock clk333=clkdiv.slowClock;
		Reset rst1 <-mkAsyncReset(0,defaultreset,clk333);

		ClockDividerIfc clk333_invert <-mkClockInverter(clocked_by clk333, reset_by rst1);
		Clock clk333_neg=clk333_invert.slowClock;
		Reset rst2 <-mkAsyncReset(0,defaultreset,clk333_neg);

		ClockDividerIfc clk166 <-mkClockDivider(2,clocked_by clk333,reset_by rst1);
		Clock clk0=clk166.slowClock;
		Reset rst0 <- mkAsyncResetFromCR (0, clk0);
		
		Clock uart_clock <-mkAbsoluteClock(0,20);
		Reset uart_reset <-mkAsyncResetFromCR(1,uart_clock);
	
		ClockDividerIfc slow_clock <- mkClockDivider(2);
		Reset slow_reset <-mkAsyncResetFromCR(0,slow_clock.slowClock);

		Ifc_Soc soc <-mkSoc(reset_vector, slow_clock.slowClock, slow_reset, uart_clock, uart_reset,
                        clk0, tck_clk.new_clk, trst.new_rst 
                        `ifdef PWM_AXI4Lite ,slow_clock.slowClock `endif );
		`ifdef SDRAM Ifc_sdram_model sdram_bfmlsb <-mksdram_model_wrapper("code.mem.LSB",clocked_by clk0, reset_by rst0); `endif
		`ifdef SDRAM Ifc_sdram_model sdram_bfmmsb <-mksdram_model_wrapper("code.mem.MSB",clocked_by clk0, reset_by rst0); `endif
		`ifdef AXIEXP Ifc_sample_axiexpslave axiexpslave <-mksample_axiexpslave(clocked_by slow_clock.slowClock,reset_by slow_reset); `endif
		 `ifdef VME

			Vme_slave#(32'h40000000,`Addr_space) vme_memory<-mkMemory("vme_test.mem","VMEMEM");`endif
//		 	Vme_slave#(32'h40000000,`Addr_space) vme_memory <-mkMemory("test_vme.mem","MainMEM");
//			Memory_vme_16#(32'h40000000,`Addr_space) vme_memory<-mkMemory_16("test_vme.mem","MainMEM");


rule connect_boot;
			soc.boot_sequence('b1);	
		endrule

	`ifdef Debug
		rule drive_constants;
		`ifndef Openocd
			soc.tdi_i(0);
			soc.tms_i(0);
			soc.bs_chain_i(0);
		`endif
		endrule
	`endif

		`ifdef AXIEXP
			mkConnection(soc.slow_ios.axiexp1_out,axiexpslave.from_slave);
			mkConnection(axiexpslave.to_slave,soc.slow_ios.axiexp1_in);
		`endif

		`ifdef PLIC
			rule connect_gpio;
                //rg_gpio_counter <= rg_gpio_counter + 1;
                //Vector#(`IONum,Bit#(1)) interrupt_vector = replicate(0);
                //    if(rg_gpio_counter == 'd1000) begin 
                //        $display($time,"TbSoc Setting 27th bit as 1");
                //        interrupt_vector[27]=1;
                //    end
                //    if(rg_gpio_counter == 'd2000) begin
                //        $display($time,"TbSoc setting 28th bit as 1");
                //        interrupt_vector[28]=1;
                //    end
                //    if(rg_gpio_counter == 'd3000) begin
                //        $display($time,"TbSoc setting 29th bit as 1");
                //        interrupt_vector[29]=1;
                //    end
                //    if(rg_gpio_counter == 'd4000)begin
                //        $display($time,"TbSoc setting 30th bit as 1");
                //        interrupt_vector[30]=1;
                //    end
                //    if(rg_gpio_counter == 'd5000)begin
                //        $display($time,"TbSoc setting 31st bit as 1");
                //        interrupt_vector[31]=1;
                //    end
                //soc.gpio_in(interrupt_vector);
				soc.slow_ios.gpio_in(replicate(0));
			endrule

		`endif
		/*================= HYPERFLASHC Connections ========*/
		`ifdef HYPER
		rule connect_hyperflash;
			soc.ifc_flash.iRPC_INT_N(0);
			soc.ifc_flash.iRPC_RSTO_N(0);
			soc.ifc_flash.rpc_dq_in(0);
			soc.ifc_flash.rpc_rwds_in(0);
		endrule
		`endif
		/*================= I2C connections ============ */
       `ifdef I2C0
              TriState#(Bit#(1)) line_SCL0 <- mkTriState(soc.slow_ios.i2c0_out.scl_out_en,soc.slow_ios.i2c0_out.scl_out,clocked_by slow_clock.slowClock, reset_by slow_reset);
              TriState#(Bit#(1)) line_SDA0 <- mkTriState(soc.slow_ios.i2c0_out.sda_out_en,soc.slow_ios.i2c0_out.sda_out,clocked_by slow_clock.slowClock, reset_by slow_reset);
              TriState#(Bit#(1)) linescl0 <- mkTriState(False,1'b1,clocked_by slow_clock.slowClock, reset_by slow_reset);
              IFC_EEPROM i2c_bfm_slave0 <- mkM24AA1025(clocked_by slow_clock.slowClock, reset_by slow_reset); 
              
              mkConnection(line_SDA0.io,i2c_bfm_slave0.linesda);
              mkConnection(line_SCL0.io,linescl0.io);

            rule send_sda_connect_i2c0;
				soc.slow_ios.i2c0_out.scl_in(line_SCL0._read);
                soc.slow_ios.i2c0_out.sda_in(line_SDA0._read);
            endrule
            
            rule connect_i2c0;
              i2c_bfm_slave0.iSCL(linescl0);
              i2c_bfm_slave0.iA0(1'b0);
              i2c_bfm_slave0.iA1(1'b0);
              i2c_bfm_slave0.iA2(1'b1);
              i2c_bfm_slave0.iWP(1'b0);
            endrule
        `endif

        `ifdef I2C1
              TriState#(Bit#(1)) line_SCL1 <- mkTriState(soc.slow_ios.i2c1_out.scl_out_en,soc.slow_ios.i2c1_out.scl_out,clocked_by slow_clock.slowClock, reset_by slow_reset);
              TriState#(Bit#(1)) line_SDA1 <- mkTriState(soc.slow_ios.i2c1_out.sda_out_en,soc.slow_ios.i2c1_out.sda_out,clocked_by slow_clock.slowClock, reset_by slow_reset);
              TriState#(Bit#(1)) linescl1 <- mkTriState(False,1'b1,clocked_by slow_clock.slowClock, reset_by slow_reset);
              IFC_EEPROM i2c_bfm_slave1 <- mkM24AA1025(clocked_by slow_clock.slowClock, reset_by slow_reset); 
              
              mkConnection(line_SDA1.io,i2c_bfm_slave1.linesda);
              mkConnection(line_SCL1.io,linescl1.io);

            rule send_sda_connect_i2c1;
		soc.slow_ios.i2c1_out.scl_in(line_SCL1._read);
               soc.slow_ios.i2c1_out.sda_in(line_SDA1._read);
            endrule
            
            rule connect_i2c1;
              i2c_bfm_slave1.iSCL(linescl1);
              i2c_bfm_slave1.iA0(1'b0);
              i2c_bfm_slave1.iA1(1'b0);
              i2c_bfm_slave1.iA2(1'b1);
              i2c_bfm_slave1.iWP(1'b0);
            endrule
        `endif
		/*============================================== */
        /********************** Micron FLASH Connections *****************/
      `ifdef micron
		`ifdef QSPI0
			Ifc_MicronFlashWrapper flash0 <-mkMicronFlashWrapper(clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi0tri_sio0<-mkTriState(soc.slow_ios.qspi0_out.io_enable[0]==1,soc.slow_ios.qspi0_out.io_o[0],clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi0tri_sio1<-mkTriState(soc.slow_ios.qspi0_out.io_enable[1]==1,soc.slow_ios.qspi0_out.io_o[1],clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi0tri_sio2<-mkTriState(soc.slow_ios.qspi0_out.io_enable[2]==1,soc.slow_ios.qspi0_out.io_o[2],clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi0tri_sio3<-mkTriState(soc.slow_ios.qspi0_out.io_enable[3]==1,soc.slow_ios.qspi0_out.io_o[3],clocked_by slow_clock.slowClock, reset_by slow_reset);
			mkConnection(qspi0tri_sio0.io,flash0.dq0);
        	mkConnection(qspi0tri_sio1.io,flash0.dq1);
        	mkConnection(qspi0tri_sio2.io,flash0.vpp_w_dq2);
        	mkConnection(qspi0tri_sio3.io,flash0.hold_dq3);
        	rule connect_flash0_ports1;
        	    flash0.iS(soc.slow_ios.qspi0_out.ncs_o);
        	    flash0.iC(soc.slow_ios.qspi0_out.clk_o);
        	    flash0.iVcc('d1800);
        	endrule
        	rule connect_flash0_input_ports;
        	    soc.slow_ios.spi0_out.io_i({qspi0tri_sio3._read,qspi0tri_sio2._read,qspi0tri_sio1._read,qspi0tri_sio0._read});
        	endrule
		`endif
		`ifdef QSPI1
			Ifc_MicronFlashWrapper flash1 <-mkMicronFlashWrapper(clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi1tri_sio0<-mkTriState(soc.slow_ios.qspi1_out.io_enable[0]==1,soc.qspi1_out.io_o[0],clocked_by slow_clock.slowClock,reset_by slow_reset);
			TriState#(Bit#(1)) qspi1tri_sio1<-mkTriState(soc.slow_ios.qspi1_out.io_enable[1]==1,soc.qspi1_out.io_o[1],clocked_by slow_clock.slowClock,reset_by slow_reset);
			TriState#(Bit#(1)) qspi1tri_sio2<-mkTriState(soc.slow_ios.qspi1_out.io_enable[2]==1,soc.qspi1_out.io_o[2],clocked_by slow_clock.slowClock,reset_by slow_reset);
			TriState#(Bit#(1)) qspi1tri_sio3<-mkTriState(soc.slow_ios.qspi1_out.io_enable[3]==1,soc.qspi1_out.io_o[3],clocked_by slow_clock.slowClock,reset_by slow_reset);
			mkConnection(qspi1tri_sio0.io,flash1.dq0);
        	mkConnection(qspi1tri_sio1.io,flash1.dq1);
        	mkConnection(qspi1tri_sio2.io,flash1.vpp_w_dq2);
        	mkConnection(qspi1tri_sio3.io,flash1.hold_dq3);
        	rule connect_flash1_ports1;
        	    flash1.iS(soc.slow_ios.qspi1_out.ncs_o);
        	    flash1.iC(soc.slow_ios.qspi1_out.clk_o);
        	    flash1.iVcc('d1800);
        	endrule
        	rule connect_flash1_input_ports;
        	    soc.slow_ios.qspi1_out.io_i({qspi1tri_sio3._read,qspi1tri_sio2._read,qspi1tri_sio1._read,qspi1tri_sio0._read});
        	endrule
		`endif
        /*================================================================*/
        /*=================== Cypress Flash Connections ==================*/
      `else //Cypress BFM
        `ifdef QSPI0
            Ifc_FlashWrapper flash0 <- mkCypressFlashWrapper(clocked_by slow_clock.slowClock, reset_by slow_reset);
				TriState#(Bit#(1)) qspi0tri_sio0<-mkTriState(soc.slow_ios.qspi0_out.io_enable[0]==1,soc.slow_ios.qspi0_out.io_o[0],clocked_by slow_clock.slowClock, reset_by slow_reset);
				TriState#(Bit#(1)) qspi0tri_sio1<-mkTriState(soc.slow_ios.qspi0_out.io_enable[1]==1,soc.slow_ios.qspi0_out.io_o[1],clocked_by slow_clock.slowClock, reset_by slow_reset);
				TriState#(Bit#(1)) qspi0tri_sio2<-mkTriState(soc.slow_ios.qspi0_out.io_enable[2]==1,soc.slow_ios.qspi0_out.io_o[2],clocked_by slow_clock.slowClock, reset_by slow_reset);
				TriState#(Bit#(1)) qspi0tri_sio3<-mkTriState(soc.slow_ios.qspi0_out.io_enable[3]==1,soc.slow_ios.qspi0_out.io_o[3],clocked_by slow_clock.slowClock, reset_by slow_reset);
				mkConnection(qspi0tri_sio0.io,flash0.si);
        		mkConnection(qspi0tri_sio1.io,flash0.so);
        		mkConnection(qspi0tri_sio2.io,flash0.wpNeg);
        		mkConnection(qspi0tri_sio3.io,flash0.resetNeg);
        		rule connect_flash0_ports1;
        		    flash0.iCSNeg(soc.slow_ios.qspi0_out.ncs_o);
        		    flash0.iSCK(soc.slow_ios.qspi0_out.clk_o);
        		endrule
        		rule connect_flash0_input_ports;
        		    soc.slow_ios.qspi0_out.io_i({qspi0tri_sio3._read,qspi0tri_sio2._read,qspi0tri_sio1._read,qspi0tri_sio0._read});
        		endrule
        `endif
        `ifdef QSPI1
            Ifc_FlashWrapper flash1 <- mkCypressFlashWrapper(clocked_by slow_clock.slowClock, reset_by slow_reset);
        	TriState#(Bit#(1)) qspi1tri_sio0<-mkTriState(soc.slow_ios.qspi1_out.io_enable[0]==1,soc.slow_ios.qspi1_out.io_o[0],clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi1tri_sio1<-mkTriState(soc.slow_ios.qspi1_out.io_enable[1]==1,soc.slow_ios.qspi1_out.io_o[1],clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi1tri_sio2<-mkTriState(soc.slow_ios.qspi1_out.io_enable[2]==1,soc.slow_ios.qspi1_out.io_o[2],clocked_by slow_clock.slowClock, reset_by slow_reset);
			TriState#(Bit#(1)) qspi1tri_sio3<-mkTriState(soc.slow_ios.qspi1_out.io_enable[3]==1,soc.slow_ios.qspi1_out.io_o[3],clocked_by slow_clock.slowClock, reset_by slow_reset);
			mkConnection(qspi1tri_sio0.io,flash1.si);
        	mkConnection(qspi1tri_sio1.io,flash1.so);
        	mkConnection(qspi1tri_sio2.io,flash1.wpNeg);
        	mkConnection(qspi1tri_sio3.io,flash1.resetNeg);
        	rule connect_flash1_ports1;
        	    flash1.iCSNeg(soc.slow_ios.qspi1_out.ncs_o);
        	    flash1.iSCK(soc.slow_ios.qspi1_out.clk_o);
        	endrule
        	rule connect_flash1_input_ports;
        	    soc.slow_ios.qspi1_out.io_i({qspi1tri_sio3._read,qspi1tri_sio2._read,qspi1tri_sio1._read,qspi1tri_sio0._read});
        	endrule
        `endif
      `endif

		/*********************** SDRAM Connections ****************/
		`ifdef SDRAM	
		TriState#(Bit#(32)) tri_sio0<-mkTriState(soc.sdram_out.osdr_den_n[0]==0,soc.sdram_out.osdr_dout[31:0], clocked_by clk0, reset_by rst0);
		TriState#(Bit#(32)) tri_sio1<-mkTriState(soc.sdram_out.osdr_den_n[0]==0,soc.sdram_out.osdr_dout[63:32], clocked_by clk0, reset_by rst0);
		mkConnection(tri_sio0.io,sdram_bfmlsb.dq);
		mkConnection(tri_sio1.io,sdram_bfmmsb.dq);

		rule rl_connect_input_datapins;
			soc.sdram_out.ipad_sdr_din({tri_sio1._read,tri_sio0._read});
		endrule
		rule rl_iAddr_connection;
          let in = soc.sdram_out.osdr_addr();
         sdram_bfmlsb.iAddr(truncate(in));
         sdram_bfmmsb.iAddr(truncate(in));
      endrule

      rule rl_iBa_connection;
          let in = soc.sdram_out.osdr_ba();
          sdram_bfmlsb.iBa(in);
          sdram_bfmmsb.iBa(in);
      endrule

      rule rl_iCke_connection;
          let in = soc.sdram_out.osdr_cke();
          sdram_bfmlsb.iCke(pack(in));
          sdram_bfmmsb.iCke(pack(in));
      endrule

      rule rl_iCs_n_connection;
          let in = soc.sdram_out.osdr_cs_n();
          sdram_bfmlsb.iCs_n(pack(in));
          sdram_bfmmsb.iCs_n(pack(in));
      endrule

      rule rl_iRas_n_connection;
          let in = soc.sdram_out.osdr_ras_n();
          sdram_bfmlsb.iRas_n(pack(in));
          sdram_bfmmsb.iRas_n(pack(in));
      endrule

      rule rl_iCas_n_connection;
          let in = soc.sdram_out.osdr_cas_n();
          sdram_bfmlsb.iCas_n(pack(in));
          sdram_bfmmsb.iCas_n(pack(in));
      endrule

      rule rl_iWe_n_connection;
          let in = soc.sdram_out.osdr_we_n();
          sdram_bfmlsb.iWe_n(pack(in));
          sdram_bfmmsb.iWe_n(pack(in));
      endrule

      rule rl_iDqm_connection;
          let in = soc.sdram_out.osdr_dqm();
          sdram_bfmlsb.iDqm(extend(in[3:0]));
          sdram_bfmmsb.iDqm(extend(in[7:4]));
      endrule
	`endif
		/*******************************************************/


	`ifdef Openocd
		Reg#(Bit#(2)) rg_init <- mkReg(0);
		Wire#(Maybe#(Bit#(5))) wr_cmd<-mkDWire(tagged Invalid);
		SyncPulseIfc read_tdo <-mkSyncPulseFromCC(tck_clk.new_clk);
		CrossingReg#(Bit#(1)) tdi<-mkNullCrossingReg(tck_clk.new_clk,0);
		CrossingReg#(Bit#(1)) tms<-mkNullCrossingReg(tck_clk.new_clk,0);
		CrossingReg#(Bit#(1)) rg_tdo<-mkNullCrossingReg(defaultclk,0,clocked_by tck_clk.new_clk, reset_by trst.new_rst);
		rule initialize(rg_init==0);
	  		let x<- init1;
			`ifdef verbose $display("DTM: Initialize"); `endif
			if(x!=0) begin
			`ifdef verbose 	$display($time,"\tDTM: Initialization failed"); `endif
				$finish(0);
			end
			else begin
	  			rg_init<=1;
			end
	  	endrule

		rule capture_tdo;
			rg_tdo<=soc.tdo;
		endrule

		rule connect_tck(rg_init == 1);
			let lv_cmd<- get_frame();
			wr_cmd<= tagged Valid (truncate(lv_cmd));
			tck_clk.setClockValue(lv_cmd[2]);
			if(lv_cmd[4]==1)
				trst.assertReset();
			tdi<=lv_cmd[0];
			tms<=lv_cmd[1];
		endrule

		rule connect_tap_inputs;
			soc.tdi_i(tdi.crossed);
			soc.tms_i(tms.crossed);
            soc.bs_chain_i(0);  
		endrule
		
		rule handle_tdo(wr_cmd matches tagged Valid .x);
			if(x[3]==1)
				send_tdo(rg_tdo.crossed);
		endrule

`endif
		Reg#(Bit#(32)) rg_counter<-mkReg(0);
		UART#(16) uart <-mkUART(8,NONE,STOP_1,`BAUD_RATE, clocked_by uart_clock, reset_by uart_reset); // charasize,Parity,Stop Bits,BaudRate
		let reg_dump <- mkReg(InvalidFile,clocked_by uart_clock , reset_by uart_reset) ;
		Reg#(Bit#(1)) rg_count <-mkReg(0,clocked_by uart_clock , reset_by uart_reset);
		Reg#(Bit#(64)) rg <-mkReg(0);
    	rule open_file(rg_count==0);    	
    	    String reg_dumpFile = "app_log" ;
    	    File lfh <- $fopen( reg_dumpFile, "w" ) ;
    	    if ( lfh == InvalidFile )begin
    	        `ifdef verbose $display("cannot open %s", reg_dumpFile); `endif
    	        $finish(0);
    	    end
    	    reg_dump <= lfh ;
    	    rg_count <= 1 ;
    	endrule

		`ifdef UART1
			//Use UART1 (Bluespec UART) to generate app_log
			rule connect_sin;
				soc.slow_ios.uart1_coe.sin(uart.rs232.sout);
	   	endrule
	/*		rule connect_sout;
				uart.rs232.sin(soc.slow_ios.uart1_coe.sout);
	   	endrule
	*/
		`endif
`ifdef VME		
//Connection b/w vme_controller and slave	
	rule connect_address_slaves;
		vme_memory.rd_addr(soc.proc_dbus.wr_addr());
	endrule

 

	rule connect_master_slave_vme_memory_1;
		vme_memory.rd_byte_31_24(soc.proc_dbus.wr_byte_31_24());
	endrule

	rule connect_master_slave_vme_memory_2;
		vme_memory.rd_byte_23_16(soc.proc_dbus.wr_byte_23_16());
	endrule

	rule connect_master_slave_vme_memory_3;
		vme_memory.rd_byte_15_8 (soc.proc_dbus.wr_byte_15_8());
	endrule

	rule connect_master_slave_vme_memory_4;
		vme_memory.rd_byte_7_0  (soc.proc_dbus.wr_byte_7_0());
	endrule


	rule connect_master_slave_vme_memory_5;
		soc.proc_dbus.rd_byte_31_24(vme_memory.wr_byte_31_24());
	endrule

	rule connect_master_slave_vme_memory_6;
		soc.proc_dbus.rd_byte_23_16(vme_memory.wr_byte_23_16());
	endrule

	rule connect_master_slave_vme_memory_7;
		soc.proc_dbus.rd_byte_15_8 (vme_memory.wr_byte_15_8());
	endrule

	rule connect_master_slave_vme_memory_8;
		soc.proc_dbus.rd_byte_7_0  (vme_memory.wr_byte_7_0());
	endrule


	rule cntl_word_master_vme_memory;
		vme_memory.rd_as_l(soc.proc_ifc.wr_as_l());
		vme_memory.rd_ds_l(soc.proc_ifc.wr_ds_l());
		vme_memory.rd_siz0(soc.proc_dbus.wr_siz0());
		vme_memory.rd_siz1(soc.proc_dbus.wr_siz1());
		vme_memory.rd_wr_l(soc.proc_ifc.wr_wr_l());
	endrule


	rule cntl_word_vme_memory_master;
		soc.proc_ifc.rd_dsack_0_l(vme_memory.wr_dsack_0_l());
		soc.proc_ifc.rd_dsack_1_l(vme_memory. wr_dsack_1_l());
		soc.proc_ifc.rd_berr_l(vme_memory.wr_berr_l());
		soc.proc_ifc.rd_halt_l(vme_memory.wr_halt_l());
	endrule

`endif

`ifdef FlexBus_verify
 //rule drive_flexbus_inputs;
    //soc.flexbus_out.m_TAn(1'b1);
    //soc.flexbus_out.m_din(32'haaaaaaaa);
 //endrule

 Memory_IFC#(`SDRAMMemBase,`Addr_space) main_memory <- mkMemory("code.mem.MSB","code.mem.LSB","MainMEM");

 FlexBus_Slave_to_AXI4_Master_Fabric_IFC#(32,64,0) verfn_ifc <- mkFlexBus_Slave_to_AXI4_Master_Fabric;

 mkConnection(soc.flexbus_out,verfn_ifc.flexbus_side);

 mkConnection(verfn_ifc.axi_side, main_memory.axi_slave);
 `endif

		
		`ifdef UART0
			rule connect_uart0_sin;	//dummy rule to connect uart0 sin since its always_enabled
				soc.slow_ios.uart0_coe.modem_input(uart.rs232.sout,0,0,0,0);
			endrule
			rule connect_uart0_sout;
				uart.rs232.sin(soc.slow_ios.uart0_coe.modem_output_stx);
			endrule
		`endif
		

		rule write_recieved_character_in_file(rg_count!=0);
    	  let data<-uart.tx.get;
	 	`ifdef verbose   $display("HOST: Received data: %h",data[7:0]); `endif
    	  $fwrite(reg_dump,"%c",data);
    	endrule

		rule increment_counter;
			rg_counter<=rg_counter+1;
		endrule
		`ifdef simulate
		`ifdef verbose 
		rule put_endofline;
			`ifdef verbose $display($time,"\n",$time,"\n");  `endif
		endrule
		`endif
		`endif

	endmodule
endpackage
