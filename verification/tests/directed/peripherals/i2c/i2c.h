#ifndef I2C_H
#define I2C_H

#define ETIMEOUT -60
#define DEF_TIMEOUT 60
#define ETIMEDOUT -80
#define ENXIO -82
#define EREMOTEIO -81

#define I2C_SHAKTI_PIN	0x80
#define I2C_SHAKTI_ESO	0x40
#define I2C_SHAKTI_ES1	0x20
#define I2C_SHAKTI_ES2	0x10
#define I2C_SHAKTI_ENI	0x08
#define I2C_SHAKTI_STA	0x04
#define I2C_SHAKTI_STO	0x02
#define I2C_SHAKTI_ACK	0x01

#define I2C_SHAKTI_INI 0x40   /* 1 if not initialized */
#define I2C_SHAKTI_STS 0x20
#define I2C_SHAKTI_BER 0x10
#define I2C_SHAKTI_AD0 0x08
#define I2C_SHAKTI_LRB 0x08
#define I2C_SHAKTI_AAS 0x04
#define I2C_SHAKTI_LAB 0x02
#define I2C_SHAKTI_BB  0x01

#define I2C_SHAKTI_START         (I2C_SHAKTI_PIN | I2C_SHAKTI_ESO | I2C_SHAKTI_STA | I2C_SHAKTI_ACK)
#define I2C_SHAKTI_START_ENI     (I2C_SHAKTI_PIN | I2C_SHAKTI_ESO | I2C_SHAKTI_STA | I2C_SHAKTI_ACK | I2C_SHAKTI_START_ENI)
#define I2C_SHAKTI_STOP          (I2C_SHAKTI_PIN | I2C_SHAKTI_ESO | I2C_SHAKTI_STO | I2C_SHAKTI_ACK)
#define I2C_SHAKTI_REPSTART      (                 I2C_SHAKTI_ESO | I2C_SHAKTI_STA | I2C_SHAKTI_ACK)
#define I2C_SHAKTI_REPSTART_ENI  (                 I2C_SHAKTI_ESO | I2C_SHAKTI_STA | I2C_SHAKTI_ACK | I2C_SHAKTI_REPSTART_ENI)
#define I2C_SHAKTI_IDLE          (I2C_SHAKTI_PIN | I2C_SHAKTI_ESO                  | I2C_SHAKTI_ACK)


//Following the memory map provided
#define I2C_PRESCALE 0x11400
#define I2C_CONTROL 0x11408
#define I2C_DATA 0x11410
#define I2C_STATUS 0x11418
#define I2C_SCL 0x11438

// Hardcoding the pointers with addresss -- let's see if this works 
int* i2c_control = (const int *) I2C_CONTROL;
int* i2c_data    = (const int *) I2C_DATA;
int* i2c_status  = (const int *) I2C_STATUS;
int* i2c_prescale = (const int *) I2C_PRESCALE;
int* i2c_scl = (const int *) I2C_SCL;

int get_i2c_shakti(int *addr)
{
  return *addr;
}

void set_i2c_shakti(int *addr, int val)
{
    *addr = val;
}

void i2c_start()
{
	set_i2c_shakti(i2c_control, I2C_SHAKTI_START);
}

void i2c_start_eni()
{
	set_i2c_shakti(i2c_control, I2C_SHAKTI_START);
}

void i2c_repstart()
{
	set_i2c_shakti(i2c_control, I2C_SHAKTI_REPSTART);
}

void i2c_repstart_eni()
{
	set_i2c_shakti(i2c_control, I2C_SHAKTI_REPSTART);
}

void i2c_stop()
{
	set_i2c_shakti(i2c_control, I2C_SHAKTI_STOP);
}


int shakti_init_i2c()
{
    printf("\tI2C: Initializing the Controller\n");

    /* Doing an initialization sequence as how PCF8584 was supposed to be initialized                                                       */
    /* The Initialization Sequence is as follows                                                                                            */
    /* Reset Minimum 30 Clock Cycles -- Not necessary in our case                                                                           */
    /* Load Byte 80H into Control                                                                                                           */
    /* load Clock Register S2 */ /* We are doing the opposite -- Setting the clock and then the registers -- Doesn't really matter actually */
    /* Send C1H to S1 - Set I2C to Idle mode -- SDA and SCL should be high                                                                  */


    set_i2c_shakti(i2c_prescale,0x0A);  //Setting the I2C clock value to be 1, which will set the clock for module and prescaler clock
    unsigned char temp = get_i2c_shakti(i2c_prescale);
    set_i2c_shakti(i2c_scl,0x0F);  //Setting the I2C clock value to be 1, which will set the clock for module and prescaler clock
    temp = get_i2c_shakti(i2c_scl);
/* Just reading the written value to see if all is well -- Compiler should not optimize this load!!! Compiler can just optimize the store to pointer address followed by load pointer to a register to just an immediate load to the register since clock register is not used anywhere -- but the purpose is lost. Don't give compiler optimizations */

    if((temp | 0x00) != 0x0F){
        printf("\tClock initialization failed\n"); 
        return -ENXIO;
    }
    else{
        printf("\tClock successfully initalized\n");
    }


    /* S1=0x80 S0 selected, serial interface off */
    printf("\tSetting Control Register with 0x80 \n");
	set_i2c_shakti(i2c_control, I2C_SHAKTI_PIN);
    printf("\tControl Register Successfully set \n");

    // Reading set control Register Value to ensure sanctity
    printf("\tReading Control Register \n");
    temp = get_i2c_shakti(i2c_control);
    printf("\tControl Register is Written with 0x%x \n", temp);
    
    if((temp & 0x7f) != 0){
        printf("\tDevice Not Recognized\n");
        return -ENXIO;
    }

    printf("\tWaiting for a specified time\n ");
    waitfor(900); //1 Second software wait -- Should be 900000 but setting to 900 now since simulation is already slow
    printf("\tDone Waiting \n ");

    /* Enable Serial Interface */
    set_i2c_shakti(i2c_control, I2C_SHAKTI_IDLE);
    temp = get_i2c_shakti(i2c_status); 
    printf("\tStatus Reg value is : 0x%x \n",temp);

    /* Check to see if I2C is really in Idle and see if we can access the status register -- If not something wrong in initialization. This also verifies if Control is properly written since zero bit will be initialized to zero*/
    if(temp != (I2C_SHAKTI_PIN | I2C_SHAKTI_BB)){
        printf("\tInitialization failed\n");
        return -ENXIO;
    }
    else 
        printf("\tAll is well till here \n");
    
    printf("\tI2C successfully initialized\n");
}

int wait_for_bb()
{

    printf("\tIs bus busy?\n");
	int timeout = DEF_TIMEOUT;
	int status;

	status = get_i2c_shakti(i2c_status);

	while (!(status & I2C_SHAKTI_BB) && --timeout) {
		waitfor(10000); /* wait for 100 us */
		status = get_i2c_shakti(i2c_status);
	}

	if (timeout == 0) {
        printf("\t Bus busy wait - timed out. Resetting\n");
		return ETIMEDOUT;
	}

	return 0;
}

int wait_for_pin(int *status)
{

	int timeout = DEF_TIMEOUT;

	*status = get_i2c_shakti(i2c_status);

	while ((*status & I2C_SHAKTI_PIN) && --timeout) {
		waitfor(10000); /* wait for 100 us */
		*status = get_i2c_shakti(i2c_status);
	}
	
	if (timeout == 0){
        printf("\tWait for pin timed out\n");
		return ETIMEDOUT;
    }

	return 0;
}

int shakti_sendbytes( const char *buf, int count, int last, int eni)
{
	int wrcount, status, timeout;
    printf("\tStarting Write Transaction -- Did you create tri1 nets for SDA and SCL in verilog?\n");
	for (wrcount=0; wrcount<count; ++wrcount) {
		set_i2c_shakti(i2c_data,buf[wrcount]);
		timeout = wait_for_pin(&status);
		if (timeout) {
            printf("\tTimeout happened - Write did not go through the BFM -- Diagnose\n");
			i2c_stop(); //~
			return EREMOTEIO; 
		}
		if (status & I2C_SHAKTI_LRB) { // What error is this?
			i2c_stop();//~
            printf("\tSome status check failing\n");
			return EREMOTEIO; 
		}
	}
	if (last){
        printf("\tLast byte sent : Issue a stop\n");
		i2c_stop();
    }
	else{
        printf("\tSending Rep Start and doing some other R/W transaction\n");
		if(!eni)
            i2c_repstart();
        else
            i2c_repstart_eni();
    }

	return wrcount;
}

int shakti_readbytes(char *buf, int count, int last)
{
	int i, status;
	int wfp;
    int read_value = 0;
	/* increment number of bytes to read by one -- read dummy byte */
	for (i = 0; i <= count; i++) {
        wfp = wait_for_pin(&status);
		if (wfp) {
			i2c_stop();			
            return -1;
		}

		if ((status & I2C_SHAKTI_LRB) && (i != count)) {
			i2c_stop();
			printf("\tNo ack\n");
			return -1;
		}
	    
        if (i)
			buf[i - 1] = get_i2c_shakti(i2c_data);
		else
			get_i2c_shakti(i2c_data); /* dummy read */

		if (i == count - 1) {
			set_i2c_shakti(i2c_control, I2C_SHAKTI_ESO);
		} else if (i == count) {
			if (last)
				i2c_stop();
			else
				i2c_repstart();
		}

		}

	return i-1; //excluding the dummy read
}

#endif
