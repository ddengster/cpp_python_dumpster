
#include <iostream>
#include <windows.h>

/******Test for RAM checking. Windows only.*********/

#define TOTAL_PHYSICAL_MEMORY_NEEDED 1000000
#define AVAILABLE_VIRTUAL_MEMORY_NEEDED 1000000000//1000000000

int main()
{
	MEMORYSTATUS status;
	GlobalMemoryStatus(&status);

	//status.dwTotalPhys = total amount of physical memory in bytes
	std::cout << "Total Physical Memory: " << status.dwTotalPhys << std::endl;
	if (status.dwTotalPhys < TOTAL_PHYSICAL_MEMORY_NEEDED)
	{
		std::cout << "Not enough memory! Upgrade your computer nub.\n";
	}

	//available memory in virtual address space of the calling process in bytes
	std::cout << "AvailableVirtual Memory: " << status.dwAvailVirtual << std::endl;
	if (status.dwAvailVirtual < AVAILABLE_VIRTUAL_MEMORY_NEEDED)
	{
		std::cout << "Not enough virtual memory! Close down some programs to free up memory.\n";
	}

	char *buff = new char[AVAILABLE_VIRTUAL_MEMORY_NEEDED];
	if (buff)
		delete[] buff;
	else
	{
		/**
		The system lied to you. When you tried to grab a big block, the system failed to do so.
		Something else is eating memory in the background; tell them to shut down other apps and concentrate on
		your game.
		**/
		std::cout << "The computer lies to me:( " << std::endl;
	}

	return 0;
}