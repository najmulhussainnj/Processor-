#ifndef QSPI_H
#define QSPI_H

#include<stdlib.h>
#include<stdint.h>

#define DEF_TIMEOUT 60

//Memory Maps
#define CR      0x11800 
#define DCR     0x11804
#define SR      0x11808
#define FCR     0x1180c
#define DLR     0x11810
#define CCR     0x11814
#define AR      0x11818
#define ABR     0x1181c
#define DR      0x11820
#define PSMKR   0x11824
#define PSMAR   0x11828
#define PIR     0x1182c
#define LPRT    0x11830
#define STARTMM 0x90000000
#define ENDMM   0x9FFFFFFF
//Defines for configuring the registers at ease
//Bit vectors for all the parameters in the CR
#define CR_PRESCALER(x)   (x<<24)
#define CR_PMM            (1<<23)
#define CR_APMS           (1<<22)
#define CR_TOIE           (1<<20)
#define CR_SMIE           (1<<19)
#define CR_FTIE           (1<<18)
#define CR_TCIE           (1<<17)
#define CR_TEIE           (1<<16)
#define CR_FTHRES(x)      (x<<8 )
#define CR_FSEL           (1<<7 )
#define CR_DFM            (1<<6 )
#define CR_SSHIFT         (1<<4 )
#define CR_TCEN           (1<<3 )
#define CR_DMAEN          (1<<2 )
#define CR_ABORT          (1<<1 )
#define CR_EN             (1<<0 )

//Bit vectors for DCR 
#define DCR_FSIZE(x)       (x<<16) 
#define DCR_MODE_BYTE(x)   (x<<21)
#define DCR_CSHT(x)        (x<<8 )
#define DCR_CKMODE         0x1 

//Bit vectors for status register
#define SR_FLEVEL(x)      (x<<8) 
#define SR_TOF            (1<<4)
#define SR_SMF            (1<<3)
#define SR_FTF            (1<<2)
#define SR_TCF            (1<<1)
#define SR_TEF            (1<<0)

//Bit vectors for flag clear register 
#define FCR_CTOF (1<<4)
#define FCR_CSMF (1<<3)
#define FCR_CTCF (1<<1)
#define FCR_CTEF (1<<0)

//Bit vectors for DLR
#define DL(x)  x  //Useless -- but for better readability of the code

//Bit vectors for CCR
#define CCR_DDRM                   (1<<31) 
#define CCR_DHHC                   (1<<30)
#define CCR_DUMMY_BIT              (1<<29) // Needed by Micron Flash Memories
#define CCR_SIOO                   (1<<28)
#define CCR_FMODE(x)               (x<<26)
#define CCR_DMODE(x)               (x<<24)
#define CCR_DUMMY_CONFIRMATION     (1<<23) // Needed by Micron Flash Memories
#define CCR_DCYC(x)                (x<<18)
#define CCR_ABSIZE(x)              (x<<16)
#define CCR_ABMODE(x)              (x<<14)
#define CCR_ADSIZE(x)              (x<<12)
#define CCR_ADMODE(x)              (x<<10)
#define CCR_IMODE(x)               (x<<8 )
#define CCR_INSTRUCTION(x)         (x<<0 )

#define CCR_FMODE_INDWR 0x0
#define CCR_FMODE_INDRD 0x1
#define CCR_FMODE_APOLL 0x2
#define CCR_FMODE_MMAPD 0x3

#define NDATA  0x0
#define SINGLE 0x1
#define DOUBLE 0x2
#define QUAD   0x3

#define BYTE      0x0
#define TWOBYTE   0x1
#define THREEBYTE 0x2
#define FOURBYTE  0x3

int* cr       =      (const int*) CR;
int* dcr      =      (const int*) DCR;
int* sr       =      (const int*) SR;
int* fcr      =      (const int*) FCR;
int* ccr      =      (const int*) CCR;
int* ar       =      (const int*) AR;
int* abr      =      (const int*) ABR;
int* dr       =      (const int*) DR;
int* dlr      =      (const int*) DLR;
int* psmkr    =      (const int*) PSMKR;
int* pir      =      (const int*) PIR;
int* lprt     =      (const int*) LPRT;
int* startmm  =      (const int*) STARTMM;
int* endmm    =      (const int*) ENDMM;

void set_qspi_shakti32(int* addr, int val)
{
    *addr = val;
}

void set_qspi_shakti16(int16_t* addr, int16_t val)
{
    *addr = val;
}

void set_qspi_shakti8(char* addr, char val)
{
    *addr= val;
}

int get_qspi_shakti(int* addr)
{
 return *addr;
}

void waitfor(unsigned int secs) {
	unsigned int time = 0;
	while(time++ < secs);
}

void qspi_init(int fsize, int csht, int prescaler, int enable_interrupts, int fthreshold){
    int int_vector = enable_interrupts? (CR_TOIE|CR_SMIE|CR_FTIE|CR_TCIE|CR_TEIE) : 0; 
    set_qspi_shakti32(dcr,(DCR_FSIZE(fsize)|DCR_CSHT(csht)|DCR_CKMODE)); 
    set_qspi_shakti32(cr,(CR_PRESCALER(prescaler)|int_vector|CR_FTHRES(fthreshold)|CR_EN));
}

void reset_interrupt_flags(){
    set_qspi_shakti32(fcr,(FCR_CTOF|FCR_CSMF|FCR_CTCF|FCR_CTEF)); //Resetting all the flags
}

int wait_for_tcf(int status){
    int timeout = DEF_TIMEOUT; 

    status = get_qspi_shakti(sr);
    
    //printf("status : %d looping?",status);
    
    while(!( status & 0x02 ) && --timeout){
        waitfor(1000);
        status=get_qspi_shakti(sr);
        printf("status: %d in loop",status);
    }
    if(timeout==0){
        printf("Request Timed out");
        return -1;
    }
    return 0;
}

int micron_write_enable(int status){
    printf("\tWrite Enable\n");
    set_qspi_shakti32(ccr,(CCR_ADSIZE(FOURBYTE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x06)));
    int ret = wait_for_tcf(status); //Indicating the completion of command -- Currently polling
    printf("Status : %d dcr : %d cr: %d ccr: %d \n",status,*dcr,*cr,*ccr);
    return ret;
}

int micron_enable_4byte_addressing(int status){
    printf("\t Enable 4 byte address \n");
    set_qspi_shakti32(ccr,(CCR_ADSIZE(FOURBYTE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xB7)));
    int ret = wait_for_tcf(status); //Indicating the completion of command -- Currently polling
    printf("Status : %d dcr : %d cr: %d ccr: %d \n",status,*dcr,*cr,*ccr);
    return ret;
}

int micron_configure_xip_volatile(int status, int value){
    printf("\tWrite Volatile Configuration Register\n");
    set_qspi_shakti32(dlr,DL(1));
    set_qspi_shakti8(dr,value);  //The value to be written into the VECR register to enable XIP. Indicating XIP to operate in Quad Mode
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDWR)|CCR_DMODE(SINGLE)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x81)));
    waitfor(50);
    int ret = wait_for_tcf(status);
    printf("Status : %d dcr : %d cr : %d ccr : %d dlr: %d dr: %d\n",status,*dcr,*cr,*ccr,*dlr,*dr);
    waitfor(50);  //Giving Micron time to store the data
    return ret;
}

int micron_disable_xip_volatile(int status, int value){
    printf("\tWrite Volatile Configuration Register to exit XIP\n");
    set_qspi_shakti32(cr,(CR_PRESCALER(0x3)|CR_TOIE|CR_TCIE|CR_TEIE|CR_SMIE|CR_FTIE|CR_ABORT|CR_EN));
    waitfor(30);
    qspi_init(27,0,3,1,15);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_DMODE(QUAD)|CCR_DUMMY_BIT|CCR_DUMMY_CONFIRMATION|CCR_DCYC(8)|CCR_ADSIZE(FOURBYTE)|CCR_ADMODE(QUAD)|CCR_IMODE(NDATA)));
    set_qspi_shakti32(ar,0x00000); //Dummy address 
    set_qspi_shakti32(dlr,1);
    int ret = wait_for_tcf(status);
    waitfor(50);
    reset_interrupt_flags();
    return ret;
}

int micron_read_id_cmd(int status, int value){
    printf("\tRead ID Command to see if the Protocol is Proper\n");
    set_qspi_shakti32(dlr,4);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0x9E)|CCR_DMODE(SINGLE)));
    int ret = wait_for_tcf(status);
    value = get_qspi_shakti(dr);
    printf("Read ID: %08x",value);
    return ret;
}

int micron_read_configuration_register(int status, int value){
    printf("\tRead ID Command to see if the Protocol is Proper\n");
    set_qspi_shakti32(dlr,4);
    set_qspi_shakti32(ccr,(CCR_FMODE(CCR_FMODE_INDRD)|CCR_IMODE(SINGLE)|CCR_INSTRUCTION(0xBE)|CCR_DMODE(SINGLE)));
    int ret = wait_for_tcf(status);
    value = get_qspi_shakti(dr);
    printf("Configuration Register Value: %08x",value);
    return ret;
}

#endif
