
#include <windows.h>
#include <iostream>

void EnableDebugPriv()
{
	HANDLE hToken;
	LUID debugnameValue;
	TOKEN_PRIVILEGES tkp;

	//tell this program to change privileges
	OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES|TOKEN_QUERY, &hToken);

	//check its own privilege value(in debug mode so can debug?
	LookupPrivilegeValue(NULL, SE_DEBUG_NAME, &debugnameValue);

	//set the privilege struct
	tkp.PrivilegeCount = 1;
	tkp.Privileges[0].Luid = debugnameValue;
	tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

	//adjust the prcoess's privileges
	AdjustTokenPrivileges(hToken, false, &tkp, sizeof(tkp), NULL, NULL);
	CloseHandle(hToken);
}

int main()
{
	EnableDebugPriv(); //enable debug privileges for our program so we can open war3.exe process

	//Get handle of window
	HWND handle_war3 = FindWindow(L"Warcraft III", NULL);

	//check
	if (!handle_war3)
	{
		//MessageBox(0, |"wc3 not found.", "", MB_OK);
		std::cout << "wc3 not found." << std::endl;
		return 0;
	}

	DWORD pid;

	GetWindowThreadProcessId(handle_war3, &pid); //get wc3's process

	HANDLE hopen = OpenProcess(PROCESS_ALL_ACCESS, false, pid); //open it

	//Check to see if we can open wc3 process
	if (!hopen)
	{
		//MessageBox(0, L"Cant open wc3 process :(", "", MB_OK);
		std::cout << "Cant open wc3 process :(" << std::endl;
		return 0;
	}

	//change memory time
	//remember: 6F3A0473	66:B9 0100	MOV CX,1  maphacks! 
	//			6F3A0473	66:B9 0F00	MOV CX,0xF for all player reveal
	
	BYTE data[] = {0xB9, 0x0F, 0x00}; //based on 66:B9 0100 or 66:B9 0F00

	bool success = WriteProcessMemory(hopen, (LPVOID)0x6F3A0473, &data, 3, NULL); //write the data to the function

	if (success)
	{
		//MessageBox(0, L"Hack loaded woots!", "", MB_OK);
		std::cout << "Hack loaded woots!" << std::endl;
	}
	else
	{
		//MessageBox(0, L"Could not write to memory zz", "", MB_OK);
		std::cout << "Could not write to memory zz" << std::endl;
		return 0;
	}

	//cleanup
	CloseHandle(hopen);
	return true;
}