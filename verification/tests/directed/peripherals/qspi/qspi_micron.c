#include "qspi.h"

#define MEM_TYPE_N25Q256_ID 0x20BA1910


int flashIdentificationDevice(){
	printf("\tReading the ID register and discovering the Flash Device\n");
	set_qspi_shakti32(dlr,4);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x9E)|CCR_DMODE(SINGLE)));
    int status = 0; // Useless Variable but still!!!!
    int ret = wait_for_tcf(status);
    int value = get_qspi_shakti(dr);
    reset_interrupt_flags();
    if(value == MEM_TYPE_N25Q256_ID){
    	printf("\tN25Q256 Device Detected\n");
    	return 0;
    }
    else{
    	printf("\t Device Not Detected - Diagnose %08x",value);
    	return -1;
    }
}

int flashMemInit(){   //Supposedly a set of routines to check if the memory/interface or whatever is proper
	int ret = flashIdentificationDevice();
	if(ret==-1){
		printf("Flash Mem Init Failed -- Quitting Program, Diagnose");
		return ret;
	}
    return 0;
	//to fill in code
}

int flashReadStatusRegister(){
    printf("\tReading the Status bits of the Flash\n");
    set_qspi_shakti32(dlr,4);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x05)|CCR_DMODE(SINGLE)));
    int status = 0;
    int ret = wait_for_tcf(status);
    waitfor(100);
    int value = get_qspi_shakti(dr);
    reset_interrupt_flags();
    if(ret){
        printf("\tRead Status Register Failed\n");
        return -1;
        }
    else 
    	return value;
}

int flashReadFlagRegister(){
	return 0;
}


int flashWriteEnable(){
    printf("\tWrite Enable\n");
    set_qspi_shakti32(ccr,(CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x06)));
    int ret = wait_for_tcf(0); //Indicating the completion of command -- Currently polling
    reset_interrupt_flags();
    return ret; 
}

int flashEnable4ByteAddressingMode(){  //Enable 4-byte addressing Mode and read the status to verify that it has happened correctly

    if(flashWriteEnable()){
        printf("\t Write Enable Failed \n");
        return -1;
    }
    waitfor(100);
    set_qspi_shakti32(ccr,(CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xB7)));
    int status =0; 
    int ret = wait_for_tcf(status);
    reset_interrupt_flags();
    waitfor(100);
    //Checking Phase
    status = flashReadStatusRegister();
    printf("\t Status Value: %08x\n",status);
    if(status & 1)
        printf("\t 4-Byte Addressing Mode is Enabled\n");
    else
        printf("\t 4-byte Addressing mode not Enabled\n");
}


int flashReadSingleSPI(int dummy_cycles, int read_address, int instruction, int data_words, int adsize){
    set_qspi_shakti32(dlr,data_words);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(SINGLE)|CCR_DCYC(dummy_cycles)|CCR_ADMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_ADSIZE(adsize)|CCR_INSTRUCTION(instruction)));
    set_qspi_shakti32(ar,read_address);
    int status = 0;
    int ret = wait_for_tcf(status);
    int value = get_qspi_shakti(dr);
    printf("\tThe Read Value: %08x\n",value);
    if(ret){
        printf("\t Read Value Failed \n");
        return -1;
    }
    reset_interrupt_flags();
    return value;
}

int flashReadQuadSPI(int dummy_cycles, int read_address, int instruction, int data_words, int adsize){
     set_qspi_shakti32(dlr,data_words);
     set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(QUAD)|CCR_DCYC(dummy_cycles)|CCR_ADSIZE(adsize)|CCR_ADMODE(QUAD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(instruction)));
     set_qspi_shakti32(ar,read_address);
     int status = 0;
     int ret = wait_for_tcf(status);
     int value = get_qspi_shakti(dr);
     printf("\tThe Read Value: %08x\n",value);
     if(ret){
         printf("\t Read Value Failed \n");
         return -1; 
     }   
     reset_interrupt_flags();
     return value;
 }

int flashWriteVolatileConfigReg(int value){
    printf("\t Setting Volatile Configuration Register with the Value: %08x",value);
    set_qspi_shakti32(dlr,DL(1));
    set_qspi_shakti8(dr,value);  //The value to be written into the VECR register to enable XIP. Indicating XIP to operate in Quad Mode
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDWR)|CCR_DMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x81)));
    waitfor(50);
    int status=0;
    int ret = wait_for_tcf(status);
    printf("Status : %d dcr : %d cr : %d ccr : %d dlr: %d dr: %d\n",status,*dcr,*cr,*ccr,*dlr,*dr);
    reset_interrupt_flags();
    waitfor(50);  //Giving Micron time to store the data
    return ret;
}
 

int main()
{
    qspi_init(27,0,3,1,15);
    uart_init();
    int ar_read,i,j;
    waitfor(100); //Time for Micron to start, maybe?
    if(flashMemInit()) //Because of STARTUPE2 primitive, the run fails for the first time it is programmed since three clock cycles are skipped. Run again
        return -1;  //Didn't work

    //Scenarios
    // Fast Read Command (Single SPI Read) is working with 7 Dummy Cycles
    // Quad Fast Read I/O (Quad SPI Read) is working with 9 Dummy Cycles
    
    waitfor(100);
    flashEnable4ByteAddressingMode();

    //Scenario-1

    printf("\t SINGLE SPI read with Three Byte with 7 Dummy Cycles\n");

    ar_read=0;
    for(i=0;i<512;++i){
        flashReadSingleSPI(7,ar_read,0x0B,4,THREEBYTE);
        waitfor(100);
        ar_read+=4;
    }

    printf("\n\n\n");

    printf("\t Quad SPI read, Three Byte Address with 9!! Dummy Cycles 0XEB as the instruction\n");

    ar_read=0;
    for(i=0;i<512;++i){
        flashReadQuadSPI(9,ar_read,0xEB,4,THREEBYTE);
        waitfor(100);
        ar_read+=4;
    }

    printf("\n\n\n");

    //XIP Mode
    int xip_value = 0x93;
    if(flashWriteVolatileConfigReg(xip_value)){
        printf("\t Volatile Configuration Register not Set -- Diagnose\n");
        return -1;
    }
    reset_interrupt_flags();
    printf("\t Quad I/O Mode with Dummy Confirmation bit to enable XIP\n");
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(QUAD)|CCR_DUMMY_CONFIRMATION|CCR_DCYC(9)|CCR_ADSIZE(THREEBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xEB)));
    set_qspi_shakti32(dlr,0x4);
    set_qspi_shakti32(ar,0x000000); //Address where the Config_string is situated in the micron nand flash memory
    int status=0;
    wait_for_tcf(status);
    waitfor(100); 
    
    int *config_string = (const int*)0x40000000;
    int* read_data = (int*)(0x90000004); //Read data register after the first element is read through QUAD mode
    *config_string = get_qspi_shakti(dr);
    printf("\tRead data is : %08x\n",*config_string);
    config_string++;
    reset_interrupt_flags(); 
    printf("\t Trying XIP now\n");
    set_qspi_shakti32(ccr, (CCR_FMODE(CCR_FMODE_MMAPD)|CCR_DMODE(QUAD)|CCR_DCYC(8)|CCR_ADSIZE(THREEBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(NDATA)));
    waitfor(25);
    int dum_data;

    for(i=0;i<67;++i) {
         dum_data = get_qspi_shakti(read_data);
         waitfor(100);
         *config_string = dum_data;
         config_string++;
         read_data++;
         waitfor(100);
         reset_interrupt_flags();
         waitfor(10);
    }
    config_string = (const int*)(0x40000000);
    for(i=0;i<67;++i){
        printf("Data: %08x Address: %08x \n",*config_string, config_string);
        config_string++;
    }
    uart_finish();
	return 0;
}
