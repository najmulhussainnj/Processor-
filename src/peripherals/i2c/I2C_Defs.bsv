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
/* This file defines the functions and the types required to implement I2C routines */
// ================================================
// Types

// Chip Clock Frequencies -- Assuming 50MHz Processor operating frequencies
`define CLK3    15
`define CLK443  9
`define CLK6    6
`define CLK8    4
`define CLK12   2

//Bus Clock Frequencies -- Assuming 8MHz Chip Clock Frequency
`define SCL90   81
`define SCL45   180
`define SCL11   692
`define SCL1    6992


typedef union tagged {
  void Dead;
  void Idle;
  void STA;
  void NAck_1;
  void RTSTA;
  void SendAddr;
  void Intrpt;
  void SendData;
  void ReadData;
  void ResetI2C;
  void BError;
  void Ack;
  void NAck;
  void SwitchMode;
  void MultipleTrans;
  void End;
  void Idleready;
} MTrans_State deriving (Bits, Eq);

typedef union tagged {
  void Dead;
  void Idle;
  void STA;
  void SendAddr;
  void Intrpt;
  void ReadData;
  void Ack;
  void NAck;
  void End;
} MRecv_State  deriving (Bits, Eq);

typedef enum    {
    Write,
    Read
} Transaction deriving (Bits, Eq);

// ================================================
// I2C Registers Map -- Custom for now. Should Adhere to the Driver
typedef enum {
    //Registers
     S2         = 'h00,
     Control    = 'h08,
     S0         = 'h10,
     Status     = 'h18,
     S01        = 'h20,
     S3         = 'h28,
     Time       = 'h30,
     SCL        = 'h38,     
     DRV0       = 'h40,     
     DRV1       = 'h48,     
     DRV2       = 'h50,     
     PD         = 'h58,     
     PPEN       = 'h60,     
     PRG_SLEW   = 'h68,     
     PUQ        = 'h70,     
     PWRUPZHL   = 'h78,     
     PWRUP_PULL_EN= 'h80     
}I2C deriving (Bits, Eq);

typedef Bit#(8) I2C_RegWidth;

