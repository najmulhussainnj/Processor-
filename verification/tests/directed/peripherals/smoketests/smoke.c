/* This is a simple test to make sure that every peripheral's basic functionality is not broken. Will be run as a part of continuous regression */
#include "i2c.h"
#include "qspi.h"
#include "qspi_cypress.h"
#include "dma.h"
#include "platform.h"
#include "plic_driver.h"
#include "plic_driver.c"
#include "encoding.h"
#include <unistd.h>
#define DMA_INTERRUPTS (DMA_CCR_TEIE|DMA_CCR_HTIE|DMA_CCR_TCIE|DMA_CCR_EN)
#define readbytes 2
void wait_for_dma_interrupt()
{
    printf("\t Waiting for DMA interrupt ISR value: %08x\n",*dma_isr);
	while(((*dma_isr)&0xF00)!=0x700 /*|| ((*dma_isr)&0xF0000)!=0x70000*/) {	//transfer not complete for channel 3 or 5
		//waitfor(10);
	//	if((*dma_isr)&0xF0000==0x70000)
	//		*dma_ccr5= 0x0;	//disabling the DMA channel
		if((*dma_isr)&0xF00==0x700)
			*dma_ccr3= 0x0;	//disabling the DMA channel
			printf("Wating for dma channel3 operation to finish dma_isr: %08x\n", *dma_isr);
	}
	*dma_ccr3= 0x0;	//disabling the DMA channel
    *dma_ifcr=0xF00; //Clearing the interrupt flag
	*dma_ccr4= 0x0;
	*dma_ccr5= 0x0;	//disabling the DMA channel
	return;
}

int main(){
 #ifdef UART0
    uart_init();
 #endif
//------------------------------------------------------------------
// Test-1 I2C Single R/W
//------------------------------------------------------------------
    char writebuf1[2] = {0,0};
    char readbuf[readbytes];
	int slaveaddr = 160; 
    if(shakti_init_i2c())	
    { 
        printf("\tInitialization Failed\n"); 
        return 0; 
    }
    while(wait_for_bb())
    {
     printf("\tError in Waiting for BB\n");
     return 0;
    }
    set_i2c_shakti(i2c_data,slaveaddr);
    i2c_start();
    waitfor(100);
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
	if(shakti_readbytes(readbuf, readbytes, 1)!= readbytes) 
    { 
        printf("\tSomething wrong in reading bytes\n -- Diagnose"); 
        return 0;
    } 
    if(readbuf[0]!='S')
        printf("\tI2C test Failed: readbuf: %08x\n", readbuf[0]);
//------------------------------------------------------------------
// Test-2 QSPI Single R/W
//------------------------------------------------------------------
    int i = 0, write_address = 0x0, ar_read = 0;
    char write_data[4] = {'T','E','S','T'};
    char read_data;
    qspi_init(27,0,3,1,15);
    waitfor(500);
    if(cypressflashIdentification()){
        printf("\tQSPI: Device Discovery failed, Diagnose \n");
    }
    if(cypressEraseSector(0x21, write_address))
        printf("\t Erase Sector Failed -- Diagnose \n");
    else
       printf("\t Erase Completed Successfully \n");
    
    CypressPageProgram(write_data[3], write_data[2], write_data[1], write_data[0], write_address); 

    for(i=0;i<4;++i){
        read_data = CypressReadSingleSPI(ar_read, 0x4);
        waitfor(100);
        ar_read+=4;
        if(read_data != write_data[i])
            printf("\tQSPI test Failed\n");
    }
//------------------------------------------------------------------
// Test-3 DMA Simple Transaction test
//------------------------------------------------------------------
    int dma_data[10]={0xcafe0,0xcafe1,0xcafe2,0xcafe3,0xcafe4,0xcafe5,0xcafe6,0xcafe7,0xcafe8,0xcafe9};
    int *a = (int*) 0x80006000; //Input from BRAM
    int *b = (int*) 0x20000; //Output to TCM

    for(i=0;i<10;++i){
        *(a+i)=dma_data[i];
    }
    __asm__("fence\n\t");
	*dma_cndtr3= 0x0A;
	*dma_cmar3= a;
	*dma_cpar3= b;
    *dma_ccr3= (DMA_CCR_BURST_LEN(10)|DMA_CCR_MEM2MEM|DMA_CCR_PL(2)|DMA_CCR_MSIZE(DMA_FOURBYTE)|DMA_CCR_PSIZE(DMA_FOURBYTE)|DMA_CCR_MINC|DMA_CCR_PINC|DMA_CCR_DIR|DMA_INTERRUPTS);
    printf("\t DMA_CCR3 value: %08x\n",*dma_ccr3);
    wait_for_dma_interrupt();
    printf("\t Clearing Interrupt Flags\n");
    *dma_ifcr=0;

    for(i=0;i<10;++i){
        if(*(a+i)!=*(b+i))
            printf("\tDMA Transaction Failed at %d a: %08x b: %08x \n", i, *(a+i), *(b+i));
    }
//-------------------------------------------------------------------
// Test-4 PLIC Simple Interrupt Test ????
//-------------------------------------------------------------------

    printf("\tTests Done, Check Statements above\n");
    return 0;
}
