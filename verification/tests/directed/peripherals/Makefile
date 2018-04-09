SHELL := /bin/bash # Use bash syntax
XLEN=64

I2C_DIR:=./i2c/
DMA_DIR:=./dma/
I2C_OUT:=./i2c/outputs/
I2C_DMA_OUT:=./i2c/dma_outputs/
DMA_OUT:=./dma/outputs/
CommonDIR:=./common/
QSPI_DIR:=./qspi/
QSPI_OUT:=./qspi/outputs
PLIC_DIR:=./plic/
PLIC_OUT:=./plic/outputs
BOOT_DIR:=./boot/
BOOT_OUT:=./boot/outputs
SMOKE_DIR:=./smoketests/
SMOKE_OUT:=./smoketests/outputs

#ASCII Escape codes
RED='\033[0;31m' #Red
GREEN='\033[0;32m' #Green
NC='\033[0m' # No Color

all: create_dir	
	make i2c.riscv dma_mem2mem i2c_dma.riscv

create_dir:
	@mkdir -p ./i2c/outputs/
	@mkdir -p ./qspi/outputs/
	@mkdir -p ./dma/outputs/
	@mkdir -p ./i2c/dma_outputs/
	@mkdir -p ./plic/outputs/
	@mkdir -p ./uart/outputs/
	@mkdir -p ./boot/outputs/
	@mkdir -p ./smoketests/outputs/

smoketests.riscv: crt.o syscalls.shakti create_dir
	@echo -e "$(GREEN) Compiling Smoke tests $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf -I $(I2C_DIR) -I $(QSPI_DIR) -I $(DMA_DIR) -I $(PLIC_DIR) -I $(CommonDIR) -c $(SMOKE_DIR)/smoke.c -o $(SMOKE_OUT)/smoke.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(CommonDIR)/link.ld $(SMOKE_OUT)/smoke.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(SMOKE_OUT)/smoke.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(SMOKE_OUT)/smoke.shakti > $(SMOKE_OUT)/smoke.dump
	@elf2hex 8 65536 $(SMOKE_OUT)/smoke.shakti 2147483648 > $(SMOKE_OUT)/code.mem
	@cut -c1-8 $(SMOKE_OUT)/code.mem > $(SMOKE_OUT)/code.mem.MSB
	@cut -c9-16 $(SMOKE_OUT)/code.mem > $(SMOKE_OUT)/code.mem.LSB
	@echo "53" > ../../../../bin/i2c.mem


i2c.riscv: crt.o syscalls.shakti create_dir
	@echo -e "$(GREEN) Compiling I2C Polling code $(NC)"
	@echo -e "$(RED) Caveat: I2C Code starts at 0x100000. Configure RTL appropriately $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf  -c $(I2C_DIR)/i2c.c -o $(I2C_OUT)/i2c.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(CommonDIR)/link.ld $(I2C_OUT)/i2c.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(I2C_OUT)/I2C.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(I2C_OUT)/I2C.shakti > $(I2C_OUT)/I2C.dump
	@elf2hex 8 65536 $(I2C_OUT)/I2C.shakti 2147483648 > $(I2C_OUT)/code.mem
	@cut -c1-8 $(I2C_OUT)/code.mem > $(I2C_OUT)/code.mem.MSB
	@cut -c9-16 $(I2C_OUT)/code.mem > $(I2C_OUT)/code.mem.LSB

i2c_eeprom.riscv: crt.o syscalls.shakti create_dir
	@echo -e "$(GREEN) Compiling I2C Polling code $(NC)"
	@echo -e "$(RED) Configure RTL appropriately $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf  -c $(I2C_DIR)/i2c_eeprom_write.c -o $(I2C_OUT)/i2c.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(CommonDIR)/link.ld $(I2C_OUT)/i2c.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(I2C_OUT)/i2c_eeprom.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(I2C_OUT)/i2c_eeprom.shakti > $(I2C_OUT)/i2c_eeprom.dump
	@elf2hex 8 32768 $(I2C_OUT)/i2c_eeprom.shakti 2147483648 > $(I2C_OUT)/code.mem
	@cut -c1-8 $(I2C_OUT)/code.mem > $(I2C_OUT)/code.mem.MSB
	@cut -c9-16 $(I2C_OUT)/code.mem > $(I2C_OUT)/code.mem.LSB

boot.riscv: create_dir 	
	@echo -e "$(GREEN) Compiling BOOT Polling code $(NC)"
	@echo -e "$(RED) Caveat: BOOT Code starts at 0x2000. Configure RTL appropriately $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf  -c $(BOOT_DIR)/boot.c -o $(BOOT_OUT)/boot.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -fno-common -fno-builtin-printf -D__ASSEMBLY__=1 -c $(BOOT_DIR)/crt.S -o $(BOOT_DIR)/crt.o
	@riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -fno-common -fno-builtin-printf  -c $(BOOT_DIR)/syscalls.c -o $(BOOT_DIR)/syscalls.shakti
	@riscv$(XLEN)-unknown-elf-gcc -T $(BOOT_DIR)/bootlink.ld  $(BOOT_OUT)/boot.o $(BOOT_DIR)/syscalls.shakti $(BOOT_DIR)/crt.o -o $(BOOT_OUT)/boot.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(BOOT_OUT)/boot.shakti > $(BOOT_OUT)/boot.dump
	@elf2hex 8 8192 $(BOOT_OUT)/boot.shakti 4096 > $(BOOT_OUT)/boot.mem


qspi_dma.riscv: crt.o syscalls.shakti create_dir
	@echo -e "$(GREEN) Compiling QSPI Pollingcode $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf  -c $(QSPI_DIR)/qspi_dma.c -o $(QSPI_OUT)/qspi_dma.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(CommonDIR)/link.ld $(QSPI_OUT)/qspi_dma.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(QSPI_OUT)/qspi_dma.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(QSPI_OUT)/qspi_dma.shakti > $(QSPI_OUT)/qspi_dma.dump
	@elf2hex 8 65536 $(QSPI_OUT)/qspi_dma.shakti 2147483648 > $(QSPI_OUT)/code.mem
	@cut -c1-8 $(QSPI_OUT)/code.mem > $(QSPI_OUT)/code.mem.MSB
	@cut -c9-16 $(QSPI_OUT)/code.mem > $(QSPI_OUT)/code.mem.LSB

qspi_cypress.riscv: crt.o syscalls.shakti create_dir
	@echo -e "$(GREEN) Compiling QSPI Pollingcode $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf  -c $(QSPI_DIR)/qspi_cypress.c -o $(QSPI_OUT)/qspi.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(CommonDIR)/qspilink.ld $(QSPI_OUT)/qspi.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(QSPI_OUT)/qspi_cypress_write.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(QSPI_OUT)/qspi_cypress_write.shakti > $(QSPI_OUT)/qspi_cypress.dump
	@elf2hex 8 32768 $(QSPI_OUT)/qspi_cypress_write.shakti 2147483648 > $(QSPI_OUT)/code.mem
	@cut -c1-8 $(QSPI_OUT)/code.mem > $(QSPI_OUT)/code.mem.MSB
	@cut -c9-16 $(QSPI_OUT)/code.mem > $(QSPI_OUT)/code.mem.LSB

plic.riscv: crt.o syscalls.shakti create_dir
	@echo -r "$(GREEN) Compiling PLIC Code $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf -I./$(CommonDIR)/ -c $(PLIC_DIR)/global_interrupts.c -o $(PLIC_OUT)/global_interrupts.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(CommonDIR)/link.ld $(PLIC_OUT)/global_interrupts.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(PLIC_OUT)/global_interrupts.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(PLIC_OUT)/global_interrupts.shakti > $(PLIC_OUT)/global_interrupts.dump
	@elf2hex 8 32768 $(PLIC_OUT)/global_interrupts.shakti 2147483648 > $(PLIC_OUT)/code.mem
	@cut -c1-8 $(PLIC_OUT)/code.mem > $(PLIC_OUT)/code.mem.MSB
	@cut -c9-16 $(PLIC_OUT)/code.mem >$(PLIC_OUT)/code.mem.LSB


dma_mem2mem: crt.o syscalls.shakti create_dir
	@echo -e "$(GREEN) Compiling DMA Memory to Memory transfer code $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf  -c $(DMA_DIR)/mem_to_mem_transfer.c -o $(DMA_OUT)/mem_to_mem_transfer.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(CommonDIR)/link.ld $(DMA_OUT)/mem_to_mem_transfer.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(DMA_OUT)/mem_to_mem_transfer.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(DMA_OUT)/mem_to_mem_transfer.shakti > $(DMA_OUT)/mem_to_mem_transfer.dump
	@elf2hex 8 65536 $(DMA_OUT)/mem_to_mem_transfer.shakti 2147483648 > $(DMA_OUT)/code.mem
	@cut -c1-8 $(DMA_OUT)/code.mem > $(DMA_OUT)/code.mem.MSB
	@cut -c9-16 $(DMA_OUT)/code.mem > $(DMA_OUT)/code.mem.LSB

i2c_dma.riscv: crt.o syscalls.shakti create_dir
	@echo -e "$(GREEN) Compiling I2C with DMA code $(NC)"
	@echo -e "$(RED) Caveat: I2C Code starts at 0x100000. Configure RTL appropriately $(NC)"
	@riscv$(XLEN)-unknown-elf-gcc -w -mcmodel=medany -static -std=gnu99 -fno-builtin-printf  -c $(I2C_DIR)/i2c_dma.c -o $(I2C_DMA_OUT)/i2c_dma.o -march=rv$(XLEN)imafd -lm -lgcc
	@riscv$(XLEN)-unknown-elf-gcc -T $(I2C_DIR)/i2clink.ld $(I2C_DMA_OUT)/i2c_dma.o $(CommonDIR)/syscalls.shakti $(CommonDIR)/crt.o -o $(I2C_DMA_OUT)/I2C_DMA.shakti -static -nostartfiles -lm -lgcc
	@riscv$(XLEN)-unknown-elf-objdump -D $(I2C_DMA_OUT)/I2C_DMA.shakti > $(I2C_DMA_OUT)/I2C_DMA.dump
	@elf2hex 8 32768 $(I2C_DMA_OUT)/I2C_DMA.shakti 16777216 > $(I2C_DMA_OUT)/code.mem
	@cut -c1-8 $(I2C_DMA_OUT)/code.mem > $(I2C_DMA_OUT)/code.mem.MSB
	@cut -c9-16 $(I2C_DMA_OUT)/code.mem > $(I2C_DMA_OUT)/code.mem.LSB


crt.o:
	@riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -fno-common -fno-builtin-printf -D__ASSEMBLY__=1 -c $(CommonDIR)/crt.S -o $(CommonDIR)/crt.o

syscalls.shakti:
	@riscv$(XLEN)-unknown-elf-gcc -march=rv$(XLEN)imafd  -mcmodel=medany -static -std=gnu99 -fno-common -fno-builtin-printf  -c $(CommonDIR)/syscalls.c -o $(CommonDIR)/syscalls.shakti

clean: 
	@rm -rf $(I2C_OUT)/* $(I2C_DMA_OUT)/* $(DMA_OUT)/* $(CommonDIR)/*.o $(CommonDIR)/*.shakti $(QSPI_OUT)/* $(PLIC_OUT)/* $(DMA_OUT)/* $(BOOT_OUT)/* $(BOOT_DIR)/*.o $(BOOT_DIR)/*.shakti
	@rm -rf outputs/
	@echo "cleaned"
