#include "i2c.h"
#include "../DMA/dma.h" //TODO change this relative path as well

#ifdef IGNORE_PRINTF
#define printf(fmt, ...) (0)
#endif

#define writebytes 34 
#define readbytes 33

void wait_for_dma_interrupt_i2c()
{
	while(((*dma_isr)&0xF00)!=0x700) {	//transfer not complete for channel 3 or 5
		//waitfor(10);
		//if((*dma_isr)&0xF00==0x300)
		//	*dma_ccr3= 0x0;	//disabling the DMA channel
			printf("\tWating for dma channel3 operation to finish\n");
	}
    printf("\tDMA disabled");
	*dma_ccr3= 0x0;	//disabling the DMA channel
    *dma_ifcr=0xF00; //Clearing the interrupt flag
    *dma_ifcr=0x0; //Clearing the interrupt flag
	return;
}

int main()
{
    //First initialize the I2C
    printf("\tI2C: Starting Transaction\n");
    int slaveaddr = 160;
    char readbuf[33];
    long int writebuf1[2] = {0,0};
    long int writebufco[32] ;
    int i =0,status;
    int count5 = 0;
	char temp;
   	
    char writebuf[writebytes] ; 
    writebuf[0] = 0x0;
    writebuf[1] = 0x0;

    if(shakti_init_i2c())	
    { 
        printf("\tSomething Wrong In Initialization\n"); 
        return 0; 
    }
	else 
        printf("\tIntilization Happened Fine\n");
    
    int k =0 ;
    ////////////////////Initialization Happened 
    
    for(int j=0; j< 1; j++)
    {
    
    for (i = 2; i < writebytes; i++) {
	    writebuf[i] = rand() % 255 ; 
	    writebufco[count5] = writebuf[i];
	    count5++;
	}

    writebuf[0] = k >> 8;
    writebuf[1] = k & 255 ; 
    k = k + 32;
    
    printf("\tConfiguring DMA channel 3 to write the Data from Memory to I2C");
    
    
    
    
    //Slave Address and testing
    printf("\tSetting Slave Address\n");
    set_i2c_shakti(i2c_data,slaveaddr);
    printf("\tSlave Address set\n");
    temp = get_i2c_shakti(i2c_data); 
    printf("\tSet slave address read again, which is 0x%x\n",temp);
    if(slaveaddr != (int)temp)
        printf("\tSomewhere something is wrong with the controller - it is giving out some random address -- Diagnose\n");    
    //Slave Address and testing :Done     
	    
	    while(wait_for_bb())
    {	
    	printf("Error While waiting for bb");
    	return 0;
    }	    
	    
    i2c_start_eni(); //Asking the controller to send a start signal to initiate the transaction

//	waitfor(20000); // giving time to send slave address :@: don't needed dma will anyways wait for int plus this way that time can be used for dma init.
  
    // :@: DMA Enable
    set_dma_shakti(dma_cndtr3,0x22);
    set_dma_shakti(dma_cmar3,(long int *)&writebuf);
    set_dma_shakti(dma_cpar3,(long int *)I2C_DATA);
    set_dma_shakti(dma_ccr3,0x0000009F);
    printf("dma_cmar3: %x\n",&writebuf);
       
      
    //waitfor(2000); // :@: what for
    //initialize the DMA and let it do the transaction between memory to I2C
    wait_for_dma_interrupt_i2c();  //What if there is a problem in transaction -- then I have to signal a stop to I2C
   
    // :@: DMA Disabled
    
    printf("\tDMA interrupt came -- indicating transfer complete -- Should check it by reading it");
    while(wait_for_pin(&status)) 
    { 
        printf("\t Location : 1 :Error");
        return 0;
    } 
    

    i2c_stop();
    printf("Started waiting for write cycle");
    waitfor(60000);  // :@: For BFM Write Cycle Not For Driver
    printf("Ended waiting write cycle");
    }
    
    
    printf("32B written : time to read");
    
    set_i2c_shakti(i2c_data,slaveaddr);
    i2c_start_eni();

    while(wait_for_pin(&status)) 
    { 
        printf("\tLocation 2 error");
        return 0;
    } 
    if(shakti_sendbytes(writebuf1, 2, 0,1)!=2) 
    { 
        printf("\tSomething wrong in sending bytes to write -- Diagnose\n"); 
        return 0;
    }
    
    while(wait_for_pin(&status)) 
    { 
        printf("\tLocation 3 error");
        return 0;
    }
    
    printf("\tSecond set of data written to I2C - shift mode and stop\n");
    
    set_i2c_shakti(i2c_data,slaveaddr + 1); //After repeated start
    
    //In read bytes there is a dummy read phase and last byte read phase which cannot happen through bulk DMA transfer. These happen through normal polling through software before giving control to the DMA to issue bulk read //:@: Dummy Read I m doing using DMA itself

    while(wait_for_pin(&status)) 
    { 
        printf("\twaiting for pin-3\n");
    }

    set_dma_shakti(dma_cndtr3,0x1F);
    set_dma_shakti(dma_cmar3,&readbuf);
    set_dma_shakti(dma_cpar3,(long int*)I2C_DATA);
    set_dma_shakti(dma_ifcr,0);
    set_dma_shakti(dma_ccr3,0x0000008F);
    
    wait_for_dma_interrupt_i2c();
    
    while(wait_for_pin(&status)) 
    { 
        printf("\tLocation : 4 error\n"); return 0;
    }
    
    //Turning ACK OFF
    set_i2c_shakti(i2c_control, I2C_SHAKTI_ESO);
    
    readbuf[31] = get_i2c_shakti(i2c_data); /* Second Last Read*/
    
    while(wait_for_pin(&status)) 
    { 
        printf("\twaiting for pin-3\n");
    }
    readbuf[32] = get_i2c_shakti(i2c_data); /* Final read */
    i2c_stop();
    
    //Reading Phase Ended, Now Comparison.

	printf("Print read & write bytes \n\n ");    
	for(i=0;i<32;i++)
	   printf("\n\tNo. %d Writebuf is 0x%x Readbuf is 0x%x\n",i,writebufco[i],readbuf[i+1]);	
	   
	printf("Ending print read & write bytes \n\n ");
	
	for(i=0;i<32;i++) 
	{	
		if(readbuf[i+1] != writebufco[i]){
			printf("\tBytes written and read don't match error at %d\n",i);
		    return 0;
		}
	}
	printf("Enjoy Its working");
    ////Reading the same through DMA and checking sanity

    return 0;
}
