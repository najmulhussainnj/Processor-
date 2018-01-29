/*
Copyright (c) 2017, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Vinod.G, Ayush Mittal
Email ID : g.vinod1993@gmail.com, 29ayush@gmail.com

This is a standalone C-Program which has basic routines to read/write into a Microchip BFM through I2C controller. 
References have been taken from i2c-algo-pcf.c which can be found in linux kernel sources under the subdirectory path-to-linux-dir/drivers/i2c/algos/ 
The software basically polls the configuration registers to see what has happened and does the next course of steps. This code does 32 byte write to the EEPROM and reads the same and checks if written value is correctly read to ensure functional correctness. 
TODO : Check with interrupts.
*/

//#include <stdlib.h> 
#include "i2c.h"
/* If you want your simulation to run faster - Activate this Macro so there will be no printf's except the final check which says PASS or FAIL */
#ifdef IGNORE_PRINTF
#define printf(fmt, ...) (0)
#endif

#define writebytes 34 
#define readbytes 32

int main()
{
    printf("\tI2C: Starting Transaction\n");
    char writebuf[writebytes];
	char writebuf1[2] = {0,0};
	
	int slaveaddr = 160;
	unsigned char temp = 0;
	char readbuf[readbytes];
//	srand(0); 
	int i;
	int status;
    int data = 0xfa;
	for (i = 0; i < writebytes; i++) {
	    writebuf[i] = data;
        data++;
	}
	writebuf[0] = 0;
	writebuf[1] = 0;

    printf("\tStart of Read/Write Test Sequence -- Objective : Generating Random Data and Writing it into the Microchip EEPROM BFM and Reading the same\n");
    printf("\tRandom Write Data Generated - Which are\n");

    for(i = 0; i < writebytes; ++i) {
        printf("\tByte%d : 0x%x\n",i,writebuf[i]);
    }

    if(shakti_init_i2c())	
    { 
        printf("\tSomething Wrong In Initialization\n"); 
        return 0; 
    }
	else 
        printf("\tIntilization Happened Fine\n");

    //Implementing Read and Write through Polling for now 
    while(wait_for_bb())
    {
     printf("\tError in Waiting for BB\n");
     return 0;
    }

    printf("\tSetting Slave Address\n");
    set_i2c_shakti(i2c_data,slaveaddr);
    printf("\tSlave Address set\n");
    temp = get_i2c_shakti(i2c_data); //Will this work?
    printf("\tSet slave address read again, which is 0x%x\n",temp);
    if(slaveaddr != (int)temp)
        printf("\tSomewhere something is wrong with the controller - it is giving out some random address -- Diagnose\n");
    i2c_start(); //Asking the controller to send a start signal to initiate the transaction
    if(shakti_sendbytes(writebuf, writebytes,1,0)!=writebytes){
        printf("\tSomething wrong in sending bytes to write -- Diagnose\n");
        return 0;
    }
    else{
        printf("\tFirst-set of Data sent\n");
    }
	    waitfor(90000);  //Necessary evil since you need to give time to the EEPROM to store the data -- It's operating at 100KHz lol :P Actually should be 900000
	
    while(wait_for_bb()) //Adding this for completeness -- Not useful until multi masters come into play
    { 
      printf("\twaiting for bb-2\n");
    } 
    set_i2c_shakti(i2c_data,slaveaddr);
    i2c_start();
    printf("\tSlave Addr 2 set - This is to increment the memory counter in the EEPROM\n");
	while(wait_for_pin(&status)) 
    { 
        printf("\twaiting for pin\n");
    } 
	if(shakti_sendbytes(writebuf1, 2, 0,0)!=2) 
    { 
        printf("\tSomething wrong in sending bytes to write -- Diagnose\n"); 
        return 0;
    }
	while(wait_for_pin(&status)) 
    { 
        printf("\twaiting for pin-2\n");
    } 
	set_i2c_shakti(i2c_data,slaveaddr + 1); //After repeated start
    
	while(wait_for_pin(&status)) 
    { 
        printf("\twaiting for pin-3\n");
    } 			
	if(shakti_readbytes( readbuf, readbytes, 1)!= readbytes) 
    { 
        printf("\tSomething wrong in reading bytes\n -- Diagnose"); 
        return 0;
    }
    printf("\tPrinting the read bytes\n");
    for(i=0;i<32;i++)
    {
        printf("\tByte%d: 0x%x\n",i,readbuf[i]);
    }
    for(i=0;i<32;i++)
	{	
		if(readbuf[i] != writebuf[i+2]){
			printf("\tBytes written and read don't match error at %d\n",i);
            printf("\tWritebuf is 0x%x Readbuf is 0x%x",writebuf[i],readbuf[i]);
            return 0;
        }
	}
    printf("\tI2C Microchip EEPROM successfully written and Read with 32 Bytes of Data -- Rejoice\n");
    printf("***** Vinod there is problem in your I2C");
    return 0;
}

