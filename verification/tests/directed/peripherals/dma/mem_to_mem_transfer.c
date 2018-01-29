#include "dma.h"
#include <stdint.h>

#define DMA_INTERRUPTS (DMA_CCR_TEIE|DMA_CCR_HTIE|DMA_CCR_TCIE|DMA_CCR_EN)




void waitfor(unsigned int secs) {
	unsigned int time = 0;
	while(time++ < secs);
}

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

int main()
{
    printf("Starting DMA mem to mem transfer\n");

	//int a[10]={0xbabe0, 0xbabe1, 0xbabe2, 0xbabe3, 0xbabe4, 0xbabe5, 0xbabe6, 0xbabe7, 0xbabe8, 0xbabe9};
	//int b[10];
	//int c[15]={0xcafe0, 0xcafe1, 0xcafe2, 0xcafe3, 0xcafe4, 0xcafe5, 0xcafe6, 0xcafe7, 0xcafe8, 0xcafe9, 0xcafea, 0xcafeb, 0xcafec, 0xcafed, 0xcafee};
	//int d[15];
    
    int *a = (int*) 0x80003000; //Input from BRAM
    int *c = (int*) 0x80010000; //Input from BRAM
    int *b = (int*) 0x20000; //Output to TCM
    int *d = (int*) 0x21000; //Output to TCM

    int i;
    int dum1_4byte=0x000000f0;
    int dum2_4byte=0x000000d0;
    int16_t dum1_2byte = 0x00f0;
    int8_t dum1_byte = 0xf0;

   for(i=0;i<1000;++i){
            *(a+i)= dum1_4byte+i;
        //   *(c+i)= dum2_4byte+2*i;
        }
 
    __asm__("fence\n\t");
        printf("Address of a: %08x c: %08x b: %08x d:%08x",a,c,b,d);

    for(int burst = 0; burst < 256; ++burst){

        
	    *dma_cndtr3= 0xFA0;
	    *dma_cmar3= a;
	    *dma_cpar3= b;

	    /**dma_cndtr5= 0xFA0;
	    *dma_cmar5= c;
	    *dma_cpar5= d;
	    *dma_ccr5= 0x00086ADF; */
	    //*dma_ccr3= 0x00076ADF; 
        *dma_ccr3= (DMA_CCR_BURST_LEN(burst)|DMA_CCR_MEM2MEM|DMA_CCR_PL(2)|DMA_CCR_MSIZE(DMA_FOURBYTE)|DMA_CCR_PSIZE(DMA_FOURBYTE)|DMA_CCR_MINC|DMA_CCR_PINC|DMA_CCR_DIR|DMA_INTERRUPTS);
        printf("\t DMA_CCR3 value: %08x\n",*dma_ccr3);
	    wait_for_dma_interrupt();
        printf("\t Clearing Interrupt Flags\n");
        *dma_ifcr=0;
	    
        //Copying 1000 elements from A to B and C to D
        for(i=0;i<1000;i++){
            if(*(a+i)!=*(b+i)){
                printf("\tDMA has gone wrong somewhere in copying a: %08x to b: %08x i:%d burst: %d\n",*(a+i),*(b+i),i,burst);
                return -1;
            }
            //if(*(c+i)!=*(d+i)){
            //    printf("\tDMA has gone wrong somewhere in copying c: %08x to d: %08x i:%d\n",*(c+i),*(d+i),i);
            //    return -1;
            //}
        }

        //a = (int*)0x80003000;
        //b = (int*)0x20000;

        printf("\t Burst Value: %d seems to pass\n",burst);
        for(i=0;i<1000;++i){
            *(b+i) = 0;
        }
        //a = (int*)0x80003000;
        //b = (int*)0x20000;


    }
        printf("\t DMA copy from BRAM to TCM seems to work for all bursts\n");


	    
    
    return 0;
}
