# NEXTOR_Driver_FJ
Flashjacks Nextor Driver

Driver for the Flashjacks cartridge.
With this driver loaded in the flashRom, you have access to the SD-Card drive and the Nextor and MSXDOS system.

To compile:

First:
sjasm.exe flashjacks.asm

Second:
mknexrom Nextor-2.1.1-beta1.base.dat FLASHIDE.ROM /d:FlashJacks_driver.bin /m:chgbnk.dat



Sources:

https://github.com/Konamiman/Nextor
