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
////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010  Bluespec, Inc.   ALL RIGHTS RESERVED.
////////////////////////////////////////////////////////////////////////////////
//  Filename      : GDefines.bsv
//  Description   : Global type definitions
////////////////////////////////////////////////////////////////////////////////

// Notes :

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Vector            ::*;
import GetPut            ::*;
import DefaultValue      ::*;
import FShow             ::*;
import FIFOF             ::*;
import TLM2              ::*;
import ClientServer      ::*;

`include "ARM.defines"

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef TLMAddr#(`ARM_PRM)          MemAddr;
typedef TLMData#(`ARM_PRM)          MemData;
typedef TLMByteEn#(`ARM_PRM)        MemBen;



////////////////////////////////////////////////////////////////////////////////
/// Bus Request type
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   Bool         first;
   Bool         reduced;
   Bool         inst;
   Bool         write;
   Bool         cop;
   Bool         gdb;
   Bool         c;
   Bool         b;
   Bool         usermode;
   Bool         wrap;
   UInt#(6)     id;
   UInt#(10)    burst;
   Bit#(4)      byteen;
   Bit#(32)     address;
   MemData      data;
} BusRequest deriving (Bits, Eq);

instance DefaultValue#(BusRequest);
   defaultValue = BusRequest {
      first:      True,
      reduced:    False,
      inst:       False,
      write:      False,
      cop:        False,
      gdb:        False,
      c:          False,
      b:          False,
      usermode:   False,
      wrap:       False,
      id:         0,
      burst:      1,
      byteen:     0,
      address:    ?,
      data:       ?
      };
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Bus Response Type
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   Bool         error;
   Bool         cop;
   Bool         gdb;
   Bool         write;
   UInt#(6)     id;
   MemData      data;
} BusResponse deriving (Bits, Eq);

instance DefaultValue#(BusResponse);
   defaultValue = BusResponse {
      error:    True,
      cop:      False,
      gdb:      False,
      write:    False,
      id:       0,
      data:     0
      };
endinstance


////////////////////////////////////////////////////////////////////////////////
/// Overloading Function for TLM conversions
////////////////////////////////////////////////////////////////////////////////
instance TLMRequestTC#(BusRequest, `ARM_PRM);
   function toTLMRequest(x);
      if (x.first) begin
	 RequestDescriptor#(`ARM_PRM) request = createBasicRequestDescriptor;
	 request.custom[11]       = pack(x.reduced);
	 request.custom[10]       = pack(x.gdb);
	 request.custom[9]        = pack(x.cop);
	 request.custom[8]        = 0;
	 request.custom[7]        = 0;
	 request.custom[6]        = 0;
	 request.custom[5]        = pack(x.c);
	 request.custom[4]        = pack(x.c);
	 request.custom[3]        = pack(x.b);
	 request.custom[2]        = pack(x.inst);
	 request.custom[1]        = 1;
	 request.custom[0]        = pack(!x.usermode);
	 request.burst_mode       = (x.wrap) ? WRAP : INCR;
	 request.transaction_id   = pack(x.id);
	 request.burst_size       = truncate(pack(countOnesAlt(x.byteen) - 1));
	 request.burst_length     = x.burst;
	 request.byte_enable      = x.byteen;
	 request.command          = (x.write) ? WRITE : READ;
	 request.addr             = pack(x.address);
	 request.data             = x.data;
	 return (tagged Descriptor request);
      end
      else begin
	 RequestData#(`ARM_PRM) request = unpack(0);
	 request.custom[11]       = pack(x.reduced);
	 request.custom[10]       = pack(x.gdb);
	 request.custom[9]        = pack(x.cop);
	 request.custom[8]        = 0;
	 request.custom[7]        = 0;
	 request.custom[6]        = 0;
	 request.custom[5]        = pack(x.c);
	 request.custom[4]        = pack(x.c);
	 request.custom[3]        = pack(x.b);
	 request.custom[2]        = pack(x.inst);
	 request.custom[1]        = 1;
	 request.custom[0]        = pack(!x.usermode);
	 request.transaction_id   = pack(x.id);
	 request.data             = x.data;
	 return (tagged Data request);
      end
   endfunction

   function fromTLMRequest(x);
      case(x) matches
	 tagged Descriptor .d: begin
	    return BusRequest {
	       first:    True,
	       reduced:  unpack(d.custom[11]),
	       inst:     unpack(d.custom[2]),
	       write:    (d.command == WRITE),
	       cop:      unpack(d.custom[9]),
	       gdb:      unpack(d.custom[10]),
	       c:        unpack(d.custom[4]),
	       b:        unpack(d.custom[3]),
	       usermode: unpack(~d.custom[0]),
	       wrap:     d.burst_mode == WRAP,
	       id:       unpack(d.transaction_id),
	       burst:    d.burst_length,
	       byteen:   calculateByteEnables(d),
	       address:  unpack(d.addr),
	       data:     d.data
	       };
	 end
	 tagged Data       .d: begin
	    return BusRequest {
	       first:    False,
	       reduced:  unpack(d.custom[11]),
	       inst:     unpack(d.custom[2]),
	       write:    ?,
	       cop:      unpack(d.custom[9]),
	       gdb:      unpack(d.custom[10]),
	       c:        unpack(d.custom[4]),
	       b:        unpack(d.custom[3]),
	       usermode: unpack(~d.custom[0]),
	       wrap:     ?,
	       id:       unpack(d.transaction_id),
	       burst:    ?,
	       byteen:   '1,
	       address:  0,
	       data:     d.data
	       };
	 end
      endcase
   endfunction
endinstance

instance TLMResponseTC#(BusResponse, `ARM_PRM);
   function toTLMResponse(x);
      TLMResponse#(`ARM_PRM) response = unpack(0);
      response.command        = (x.write) ? WRITE : READ;
      response.data           = x.data;
      response.status         = (x.error) ? ERROR : SUCCESS;
      response.custom[10]     = pack(x.gdb);
      response.custom[9]      = pack(x.cop);
      response.transaction_id = pack(x.id);
      return response;
   endfunction
   function fromTLMResponse(x);
      return BusResponse {
         error: (x.status != SUCCESS),
	 gdb:   unpack(x.custom[10]),
	 cop:   unpack(x.custom[9]),
	 write: (x.command == WRITE),
	 id:    unpack(x.transaction_id),
	 data:  x.data
	 };
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
function Bit#(4) calculateByteEnables(RequestDescriptor#(`ARM_PRM) desc);
   let offset_mask = 3 - desc.burst_size;
   let offset      = desc.addr & zeroExtend(offset_mask);
   let maskp1      = 1 << getTLMBurstSize(desc);
   return  (maskp1-1) << offset;
endfunction
