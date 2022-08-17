MEMORY
{
	rom (rxi) : ORIGIN = 0, LENGTH = 0x400000
	ram (wx) : ORIGIN = 0xffff0000, LENGTH = 0x10000
}

SECTIONS
{
	.header ORIGIN(rom) :
	{
		*(.header)
	} > rom
	
	.main ORIGIN(rom) + 0x200 :
	{
		*(.text .code .rodata)
	} > rom
	
	.ram ORIGIN(ram):
	{
		*(.bss)
	} > ram
}