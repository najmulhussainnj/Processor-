#include "qspi.h"
#include "cypress_data.h"
#define CYPRESS_MEM_ID 0x0102204D 
char fail_bit = 0;
int status = 0;
int read_data = 0;
//This function is used to identify the flash device - Currently only S25FS512S is discovered
int cypressflashIdentification(){
    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
	printf("\tQSPI: Reading the ID register and discovering the Flash Device\n");
    set_qspi_shakti32(dlr,4);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x9F)|CCR_DMODE(SINGLE)));
    int ret = wait_for_tcf(status);
    int value = get_qspi_shakti(dr);

    if(value==CYPRESS_MEM_ID)
        printf("\tCypress: S25FS512S Detected \n");
    else
        return -1;

    printf("\tQSPI: Actual Read ID: %08x\n",value);
    reset_interrupt_flags();
    return ret;
}

//Read the Status Reg - statusReg Arg states which status reg among the two to be read.
//If read status reg is false, fail_bit is set, which should be unset after read
int cypressReadStatusRegister(int statusReg){
    //printf("\tQSPI: Reading the Status Register with instruction :%08x\n",statusReg);
    set_qspi_shakti32(dlr,0x4);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(statusReg)));
    waitfor(50);
    int ret_value = wait_for_tcf(status);
    if(ret_value){
        printf("\tQSPI: Read Status Register Failed \n");
        fail_bit = 1;
    }
    reset_interrupt_flags();
    return get_qspi_shakti(dr); 
}

//This function is used to Erase Sectors in the flash device
//Based on the Command Input, this will perform either a 4KB Erase or a complete Sector Erase
//Should add Functionality, when the command is 4KB Erase, The device should check if CRV3[3] is set, if set the command should not be executed -- 4KB Erase is device specific and should be used only if the device supports it
int cypressEraseSector(int command, int address){
    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDWR)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(command)));
    set_qspi_shakti32(ar,address);
    waitfor(150);
    int ret = wait_for_tcf(status);
    reset_interrupt_flags();
    return wait_for_wip(); //For sector erase maybe estat should be checked
}

//This function will erase the complete Flash Memory and set all blocks to FFh
/*int cypressBulkErase(int address){
    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDWR)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xC7)));
    set_qspi_shakti32(ar,address);
    d
    d
    dd
    waitfor(150);
    wait_for_tcf(status);
    reset_interrupt_flags();
    return wait_for_estat();
}*/

//This function, checks the status registers periodically and will update if the erase functionality has been successfully completed or not by the flash device
/*int wait_for_estat(){
    int status1, status2;
    printf("\t Wait for Estat \n");
    do {
        status1 = cypressReadStatusRegister(0x05);
        if(check_fail_bit()){
            return -1;
        }
        waitfor(100000); //Gap between Every Status read
        status2 = cypressReadStatusRegister(0x07);
        if(check_fail_bit()){
            return -1;
        }
        
        if(status1 & 0x20){
            printf("\tQSPI: Erase Error - Diagnose\n");
            return -1;
        }
        printf("\tStatus1: %08x Status2: %08x\n",status1,status2);
        waitfor(100000); //Wait between every status read
    }while(!(status2 & 0x04));
    printf("\tQSPI: Erase Command Successfully Completed\n");
    return 0;
}*/

int wait_for_wip(){
    int status1;
    do{
        printf("\t Waiting for Wip \n");
        status1 = cypressReadStatusRegister(0x05);
        if(check_fail_bit())
            return -1;
        if(status1 & 0x40){
            printf("\tQSPI: Programming Error - Diagnose\n");
            return -1;
        }
        waitfor(1000);
    }while(status1 & 0x01);
    printf("\t QSPI: Page Program/Erase Command Successfully Completed\n");
    return 0;
}

//This function does the Page Program Operation in Cypress Flash Memory -- For now, Page Program (PP) is executed as 4 32-bit words per transfer (16 Byte Transfer). Though PP allows upto 256 Bytes to be transferred at one shot, we are currently limited by our FIFO size and hence 16 bytes per transfer. ECC is enabled if the transfer is >= 16 bytes
//In PP, values are expected to be transferred in bendian format.
int CypressPageProgram(int value1, int value2, int value3, int value4, int address){
    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
    reset_interrupt_flags(); 
   
    set_qspi_shakti32(dlr,DL(16));
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDWR)|CCR_DMODE(SINGLE)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x12))); 
    set_qspi_shakti32(ar, address);
    set_qspi_shakti32(dr,value1);
    set_qspi_shakti32(dr,value2);
    set_qspi_shakti32(dr,value3);
    set_qspi_shakti32(dr,value4);
    waitfor(150); 
    wait_for_tcf(status);
    reset_interrupt_flags();
    return wait_for_wip(); // Function which checks if WIP is done, indicating completion of Page Program
}

int CypressReadSingleSPI(int address, int data_length){
    set_qspi_shakti32(dlr, data_length); //Data Length Register can have max of 16
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(SINGLE)|CCR_DCYC(7)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x0C)));
    set_qspi_shakti32(ar,address); 
   // waitfor(1000);
   // printf("Status : %08x dcr : %08x cr : %08x ccr : %08x dlr: %08x ar: %08x\n",status,*dcr,*cr,*ccr,*dlr,*ar);
    int ret = wait_for_tcf(status);
    read_data = get_qspi_shakti(dr);
    if(ret){
        printf("\tQSPI: Read Value failed\n");
        return -1;
    }
    //*config_string = get_qspi_shakti(dr);
    //printf("\t Read Data is %08x\n",read_data);
    reset_interrupt_flags();
    return read_data;
}

int CypressReadDualSPI(int address, int data_length){
    set_qspi_shakti32(dlr,data_length); //DLR
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(DOUBLE)|CCR_DCYC(11)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(DOUBLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xBC)));
    set_qspi_shakti32(ar,address); 
//    printf("Status : %08x dcr : %08x cr : %08x ccr : %08x dlr: %08x ar: %08x\n",status,*dcr,*cr,*ccr,*dlr,*ar);
    int ret = wait_for_tcf(status);
    read_data = get_qspi_shakti(dr);
    if(ret){
        printf("\t QSPI: Read Value failed\n");
        return -1;
    }
    //*config_string = get_qspi_shakti(dr);
    //printf("\t Read Data is %08x\n",read_data);
    reset_interrupt_flags();
    return read_data;
}

//Before Executing Quad SPI Read, the Quad bit needs to be set in VECR Reg
int CypressReadQuadSPI(int address, int data_length){
    set_qspi_shakti32(dlr,data_length); //DLR
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(QUAD)|CCR_DCYC(9)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xEC)));
    set_qspi_shakti32(ar,address); //Address where the Config_string is situated in the micron nand flash memory 
    waitfor(1000);
    int ret = wait_for_tcf(status);
//    printf("Status : %08x dcr : %08x cr : %08x ccr : %08x dlr: %08x ar: %08x\n",status,*dcr,*cr,*ccr,*dlr,*ar);
    
    read_data = get_qspi_shakti(dr);
    if(ret){
        printf("\t QSPI: Read Value failed\n");
        return -1;
    }
    //*config_string = get_qspi_shakti(dr);
    //printf("\t Read Data is %08x\n",read_data);
    reset_interrupt_flags();
    return read_data;
}

int setVCR(int crvalue, int srvalue){
    if(micron_write_enable(status)){
        printf("Panic: Write Enable Command Failed to execute");
        return -1;
    }
    reset_interrupt_flags();
    set_qspi_shakti32(dlr,DL(2));
    set_qspi_shakti8(dr,crvalue);  
    set_qspi_shakti8(dr,srvalue); 
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDWR)|CCR_DMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x01)));
    waitfor(50);
    wait_for_tcf(status);
    printf("Status : %08x dcr : %08x cr : %08x ccr : %08x dlr: %08x dr: %08x\n",status,*dcr,*cr,*ccr,*dlr,*dr);
    waitfor(50);  //Giving Micron time to store the data
    reset_interrupt_flags();
    return wait_for_wip(); // Function which checks if WIP is done, indicating completion of Page Program

}


//This function can be used to enter the XIP (or Continuous Read Mode) for Cypress Flash Devices -- This can be used as the first read followed by just addresses or even be used as a dummy read to just get into Memory Mapped Mode in QSPI Controller
int cypressEnterXIP(int address, int data_length){
    set_qspi_shakti32(dcr,DCR_MODE_BYTE(0xA0)|DCR_FSIZE(27)|DCR_CSHT(0)|DCR_CKMODE);
    set_qspi_shakti32(dlr,data_length); //DLR
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(QUAD)|CCR_DUMMY_CONFIRMATION|CCR_DCYC(9)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xEC)));
    set_qspi_shakti32(ar,address); //Address where the Config_string is situated in the micron nand flash memory 
    waitfor(1000);
    wait_for_tcf(status);
    printf("Status : %08x dcr : %08x cr : %08x ccr : %08x dlr: %08x ar: %08x\n",status,*dcr,*cr,*ccr,*dlr,*ar);
    
    read_data = get_qspi_shakti(dr);
    //*config_string = get_qspi_shakti(dr);
    printf("\t Read Data is %08x\n",read_data);
    reset_interrupt_flags();
    set_qspi_shakti32(ccr, (CCR_FMODE(CCR_FMODE_MMAPD)|CCR_DMODE(QUAD)|CCR_DUMMY_CONFIRMATION|CCR_DCYC(9)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(NDATA))); //Entering MMAPD
    return 0;
}

int cypressExitXIP(){
    set_qspi_shakti32(cr, CR_ABORT);
    printf("\t ABORT GENERATED \n");
    waitfor(100);
    qspi_init(27,0,3,1,15);
    return 0;
}


//Fail bit checking, indicating if a status register read fails
int check_fail_bit(){
  if(fail_bit){
        fail_bit = 0;
        return -1;
  }
    else{
        fail_bit = 0;
        return 0;
    }
}


int main(){
    //Device Discovery
    uart_init();
    int i = 0, write_address = 0x0;
    waitfor(100); //Power-up cycle for Cypress Flash
    int* ddr_address = (int*) 0x80000000;
    int* xip_address = (int*) 0x90000000;
    qspi_init(27,0,5,1,15);
    if(cypressflashIdentification()){
        printf("\tQSPI: Device Discovery failed, Diagnose \n");
    }
    
    //Erasing and Writing a bunch of Data
    for(i=0;i<4096;i+=4){
       if(i%1024==0){
           printf("\t Beginning of 4KB Sector Erase \n");
        if(cypressEraseSector(0x21, write_address))
            printf("\t Erase Sector Failed -- Diagnose \n");
        else
            printf("\t Erase Completed Successfully \n");
        }
       CypressPageProgram(write_data[i], write_data[i+1], write_data[i+2], write_data[i+3], write_address); 
        write_address+=16;
    }
 
    int ar_read=0;
    int read_data = 0;
    printf("\t Single SPI \n");
    for(i=0;i<4096;++i){
        read_data =CypressReadSingleSPI(ar_read, 0x4);
        printf("\t Read Data: %08x\n",read_data);
        waitfor(100);
        ar_read+=4;
    }

    printf("\n\n\n");

    printf("\t Dual SPI \n");
    ar_read = 0; 
    for(i=0;i<4096;++i){
        read_data =CypressReadDualSPI(ar_read, 0x4);
        printf("\t Read Data: %08x\n",read_data);
        waitfor(100);
        ar_read+=4;
    } 

    printf("\n\n\n");
   
    setVCR(0x02,0x02);  //To Enable Quad Bit

    waitfor(100000); //Arbit Delay -- So that the Quad Bit is actually set -- Empirically this delay is required to guarantee no data-loss

    printf("\t Quad SPI \n");
    ar_read=0;
    for(i=0;i<4096;++i){
        read_data =CypressReadQuadSPI(ar_read, 0x4);
        printf("\t Read Data: %08x\n",read_data);
        waitfor(100);
        ar_read+=4;
    }  
   
    //Trying XIP
    printf("\t Entering XIP Mode \n");
    cypressEnterXIP(0x0,0x4);

    for(i=0; i< 4096; ++i){
        read_data = *(xip_address);
        printf("\t Read Data: %08x\n",read_data);
        xip_address++;
        waitfor(100);
    }
 
    cypressExitXIP();

    asm volatile("ebreak");

//    return 0;
}
