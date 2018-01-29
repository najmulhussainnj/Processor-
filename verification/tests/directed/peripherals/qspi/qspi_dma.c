#include "qspi.h"
#include "../dma/dma.h"


#define DMA_INTERRUPTS (DMA_CCR_TEIE|DMA_CCR_HTIE|DMA_CCR_TCIE|DMA_CCR_EN)

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
    int* destination_address = (int*)(0x20000);
    int* source_address      = (int*)(0x90100004);
    int i = 0; 
    waitfor(100); //Time for Micron Flash to get ready
    int status=0;
    int read_id=0;     
    qspi_init(27,0,3,1,15);
    //Read ID Command

    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
    reset_interrupt_flags();
    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
    reset_interrupt_flags();
    micron_read_id_cmd(status,read_id);
    reset_interrupt_flags();
    
    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
    reset_interrupt_flags();
    
    if(micron_enable_4byte_addressing(status)){
        printf("Panic: Enable 4 byte addressing command failed to execute");
        return -1;
    }
    reset_interrupt_flags();        
    
    
   // set_qspi_shakti32(dlr,0x4);
   // set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(SINGLE)|CCR_DCYC(7)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x0B)));
   // set_qspi_shakti32(ar,0x100000); //Address where the Config_string is situated in the micron nand flash memory waitfor(1000);
//    printf("Status : %08x dcr : %08x cr : %08x ccr : %08x dlr: %08x ar: %08x",status,*dcr,*cr,*ccr,*dlr,*ar);
   // wait_for_tcf(status);
    
    //int read_data = get_qspi_shakti(dr);
    //*destination_address = get_qspi_shakti(dr);
    //printf("\tRead data before start of XIP is : %08x\n",*config_string);
    //reset_interrupt_flags();
 
    if(micron_write_enable(status)){
        printf("Panic: Command Failed to execute");
        return -1;
    }
    reset_interrupt_flags();



    int xip_value = 0x93; //Quad I/O Mode with 8 Dummy Cycles. SDR Mode
    if(micron_configure_xip_volatile(status,xip_value)){
        printf("Panic: XIP Volatile Reg not written, Command Failed");
        return -1;
    }

    reset_interrupt_flags();

    printf("\t Quad I/O Mode with Dummy Confirmation bit to enable XIP\n");
    int m = (CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(QUAD)|CCR_DUMMY_CONFIRMATION|CCR_DCYC(8)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xEC));
    printf("\t Setting CCR: %08x\n",m);
    printf("\t Random PRINT\n");
    set_qspi_shakti32(ccr,m);
    set_qspi_shakti32(dlr,0x4);
    set_qspi_shakti32(ar,0x100000); //Address where the Config_string is situated in the micron nand flash memory
    printf("Status : %d dcr : %d cr : %d ccr : %d dlr: %d ar: %d",status,*dcr,*cr,*ccr,*dlr,*ar);
    wait_for_tcf(status);
    //int read_data = get_qspi_shakti(dr);
    *destination_address = get_qspi_shakti(dr);
    printf("\tRead data is : %x\n",*destination_address);
    destination_address++; //Next location in config string -- 0x1004
    reset_interrupt_flags();
    //Initializing DMA to begin the Burst Transfer -- Transferring some dummy 60 bytes
    *dma_cselr = 0x00000300;
    *dma_cndtr3= 0x10C;
    printf("\t Trying XIP now\n");
    set_qspi_shakti32(ccr, (CCR_FMODE(CCR_FMODE_MMAPD)|CCR_DMODE(QUAD)|CCR_DCYC(8)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(NDATA)));
    waitfor(100);
    int dum_data;

    asm volatile("fence");
    *dma_cmar3 = destination_address;
    *dma_cpar3 = source_address;
    *dma_ccr3= (DMA_CCR_BURST_LEN(67)|DMA_CCR_PL(2)|DMA_CCR_MSIZE(DMA_FOURBYTE)|DMA_CCR_PSIZE(DMA_FOURBYTE)|DMA_CCR_MINC|DMA_CCR_PINC|DMA_INTERRUPTS);
    wait_for_dma_interrupt();
    printf("\t Clearing Interrupt Flags \n");
    *dma_ifcr=0;
    source_address = (int*) 0x90100000;
    destination_address = (int*) 0x20000;
    for(i=0; i<60;++i){
        //printf("\t Read Value: %08x \n",*(destination_address++));
//        if(*(source_address+i)!=*(destination_address+i)){
            printf("\t Read Values are, source_value : %08x destination_value : %08x i: %08x\n", *(source_address+i),*(destination_address+i),i);
   //         return -1;
  //       }
    }
    return 0;
}
