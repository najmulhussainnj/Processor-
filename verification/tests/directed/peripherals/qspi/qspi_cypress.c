#include "qspi.h"
#include "cypress_data.h"
#include "qspi_cypress.h"

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
