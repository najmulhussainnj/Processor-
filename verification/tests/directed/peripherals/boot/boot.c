//Boot Sequence
#include "i2c.h"
#include <stdint.h>
#define readbytes 16384

long int* AxiExpBaseAddr = (long int*) 0xC0000000;

uintptr_t handle_trap(uintptr_t cause, uintptr_t epc, uintptr_t regs[32])
{
  printf("\t BootMgr: Trap Taken: cause: %08x epc: %08x booting from AxiExp\n", cause,epc);
  BootfromAxiExp();
}


void BootfromAxiExp(long int* tcm_address){
   printf("\t BootMgr: Boot beginning from AxiExp\n");
   long int value = *AxiExpBaseAddr;
//
//   //The first 64 bits is a dummy check string, where the lower half of the string is simply 0xdeadbeef and the upper half represents the number of data-bytes to be transferred.
//   long int valueL = (value&0xFFFFFFFF);
//   long int valueU = ((value >> 32)&0xFFFFFFFF);
//   while(valueL!=0xdeadbeef){
//       printf("\t BootMgr: Check Strings Don't match Looping\n");  //Looping for now, could actually boot from UART
//		 value = *AxiExpBaseAddr;
//		 valueL = (value&0xFFFFFFFF);
//		 valueU = ((value >> 32)&0xFFFFFFFF);
//	}
//       AxiExpBaseAddr++;
		 printf("\t BootMgr: Copying 2048 double-words from AXIEXP\n");
       for(int i = 0; i < 2048; ++i){
            value = *AxiExpBaseAddr;
            AxiExpBaseAddr++;
            *tcm_address = value;
            tcm_address++;
       }
       jumpToTCM();
}


void jumpToTCM(){

char* tcm_addr = (char*) 0x20000;
printf("\t BootMgr: Fencing the Cache and Jumping to TCM tcm_addr: %08x Value: %08x \n",tcm_addr, *tcm_addr);
asm volatile("fence.i");
printf("\t BootMgr: Fence Complete \n");
//asm volatile("fence.i" "\n\t");
int i = 0;
/*for(i = 0; i < 16384; ++i){
    printf("\t TCM Value: %08x Address: %08x \n", *tcm_addr, tcm_addr++); 
}*/
asm volatile( "li x30, 0x20000" "\n\t"
              "jr x30" "\n\t"
              :
              :
              :"x30","cc","memory"
               
            );
}


int main(){
    uart_init();   
    printf("\n\
                                                                                       S \n\
                                                                                      SS \n\
                                                                                     SS \n\
                                                                                    SSS \n\
                                                                                   SSS \n\
                                                                                  SSSS \n\
                                                                                 SSSSS \n\
                                                                                SSSSS \n\
                                                                               SSSSSS \n\
                                                                              SSSSSS \n\
                                                                             SSSSSSS \n\
                                                                            SSSSSSS \n\
                                                                           SSSSSSSS \n\
                                                                          SSSSSSSS \n\
                                                                         SSSSSSSSS \n\
                                                                        SSSSSSSSS \n\
                                                                       SSSSSSSSSSS \n\
                                                                      SSSSSSSSSSS \n\
                                                                     SSSSSSSSSSSSSSSSSSSSSSS \n\
                                                                    SSSSSSSSSSSSSSSSSSSSSSS \n\
                                                                   SSSSSSSSSSSSSSSSSSSSSSS \n\
                                                                  SSSSSSSSSSSSSSSSSSSSSSS \n\
                                                                             SSSSSSSSSSS \n\
                                                                            SSSSSSSSSSS \n\
                                                                            SSSSSSSSSS \n\
                                                                           SSSSSSSSSS \n\
                                                                           SSSSSSSSS \n\
                                                                          SSSSSSSSS \n\
                                                                          SSSSSSSS \n\
                                                                         SSSSSSSS \n\
                                                                         SSSSSSS \n\
                                                                        SSSSSSS \n\
                                                                        SSSSSS \n\
                                                                       SSSSS \n\
                                                                       SSSS \n\
                                                                      SSS \n\
                                                                      SS \n\
                                                                     SS \n\
                                                                     S \n\n\
                                                         SHAKTI PROCESSORS: C-CLASS\n");
    printf("\t BootMgr: Beginning Boot Sequence \n");

    char* tcm_address      = (char*) 0x20000;
    char* modem_ctrl_reg   = (char*) 0x11210;
    char* uart_fifo_ctrl   = (char*) 0x11208;
    char* uart_line_status = (char*) 0x11228;
    int total = 16384;
    register char boot_select asm("a5");
    int i = 0, status=0;
    unsigned char temp=0;
    asm volatile("csrr a5, 0xf15" "\n\t"
                 :
                 :
                 :"a5","cc","memory");

    if((boot_select & 0x1)) { //Boot from I2C
        printf("\t BootMgr: Booting from I2C\n");
        char writebuf1[2] = {0,0};
        int slaveaddr = 160;

      if(shakti_init_i2c()){ //Initializing the Controller //Should set the SDA and SCL appropriately
          printf("\t BootMgr: Init I2C Failed, Booting from AXIExp\n");
          BootfromAxiExp((long int*)tcm_address);
          //return 0;
      }
       while(wait_for_bb())
       {
        printf("\t BootMgr: I2C Bus Unusually Busy, Booting from AXIExp\n");
        BootfromAxiExp((long int*)tcm_address);
        //return 0;
       }
       set_i2c_shakti(i2c_data,slaveaddr);
       waitfor(10);
       printf("\t I2C: Slave Address set to %d \n",slaveaddr);
       temp = get_i2c_shakti(i2c_data); //Will this work?
       if(slaveaddr != (int)temp){
        printf("\t Wrong ACTUAL SLVADDR: %d CONTROLLER: %d \n",slaveaddr,temp);
        printf("\t BootMgr: I2C problem, Booting from AXIExp\n");
        BootfromAxiExp((long int*)tcm_address);
       }
  
        i2c_start(); //Asking the controller to send a start signal to initiate the transaction
        if(shakti_sendbytes(writebuf1,2,1,0)!=2){
            printf("\t START ADDR FAILED\n");
            printf("\t BootMgr: Booting from AXIExp\n");
            BootfromAxiExp((long int*)tcm_address);
            //return 0;
        }
 //       while(wait_for_pin(&status)) 
 //           printf("\tWFPin\n");
        waitfor(90000);  //Necessary evil since you need to give time to the EEPROM to store the data -- It's operating at 100KHz lol :P Actually should be 900000
	
         while(wait_for_bb()) //Adding this for completeness -- Not useful until multi masters come into play
         { 
              printf("\t I2C: Waiting for bb-2\n");
         }
	    set_i2c_shakti(i2c_data,slaveaddr + 1); //After repeated start
        i2c_start();
        while(wait_for_pin(&status)) 
            printf("\t WFPin\n");
        if(shakti_readbytes(tcm_address,total,1)!= total) 
        { 
            printf("\t I2C Read Failed\n"); 
            printf("\t BootMgr: Booting from AXIExp\n");
            BootfromAxiExp((long int*)tcm_address);
            //return 0;
        }
        else{
            printf("\t All I2C reads done, jumping to TCM\n");
        }
                
        //Jump to TCM Address
    }
    else {
          printf("\t BootMgr: Booting from UART \n");
          char local_modem, lower_nibble, upper_nibble, value,uart_status;
          for(i=0;i<((total)<<1);++i){ //total is strictly a power of 2 
            local_modem = *(modem_ctrl_reg);
            *(modem_ctrl_reg) = (local_modem | 0x03); //Asserting RTS high
            value = getchar();
            local_modem = *(modem_ctrl_reg);
            *(modem_ctrl_reg) = (local_modem & 0x1C); //Asserting RTS low
            //Post processing of the received char
            if(!(i&0x1)){   
                if(value >= '0' && value <= '9')
                    upper_nibble = value - '0';
                else if(value >= 'A' && value <= 'F')
                    upper_nibble = value - 'A' + 10;
                else if(value >= 'a' && value <= 'f')
                    upper_nibble = value - 'a' + 10;
                else {
                    BootfromAxiExp((long int*)tcm_address);
                    //printf("\t Wrong Value received \n");
                 //   asm volatile("j trap_entry");
                }
                
            }
            else{
                if(value >= '0' && value <= '9')
                    lower_nibble = value - '0';
                else if(value >= 'A' && value <= 'F')
                    lower_nibble = value - 'A' + 10;
                else if(value >= 'a' && value <= 'f')
                    lower_nibble = value - 'a' + 10;
                else {
                     BootfromAxiExp((long int*)tcm_address);
                    //printf("\t Wrong Value received \n");
                }

            *(tcm_address) = (upper_nibble << 4) | (lower_nibble & 0x0F);
            tcm_address++;
            }

           // *tcm_address = value;
          }
          //Commenting for now
          /*
          *(uart_fifo_ctrl) = 0x07;
          *(uart_fifo_ctrl) = 0x01;
         for(i=0; i < 1000; ++i){
            uart_status = *(uart_line_status);
            if(uart_status & 0x1)
                getchar();
          }*/

            
    }
    jumpToTCM();
}
