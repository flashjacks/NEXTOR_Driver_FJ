# Driver Nextor for Flashjacks 2.2 (Nextor:2.1.2)

Driver for the Flashjacks cartridge.
With this driver loaded in the flashRom, you have access to the SD-Card driver and the Nextor and MSXDOS system.

To compile:

First:
sjasm.exe flashjacks.asm

Second:
mknexrom Nextor-2.1.2.base.dat FLASHIDE.ROM /d:FlashJacks_driver.bin /m:chgbnk.dat



Sources:

https://github.com/Konamiman/Nextor

https://retromsx.com
