
#include "LogManager/LogManager.h"
#include <fstream>
#include <windows.h>
#include <stdio.h>

DECLARE_ENGINE_NAMESPACE

//NOTE: Unsafe to call this singleton within another singleton's destructor

/* Get Singleton */
LogManager* LogManager::GetInstance(void)
{
	static LogManager logMan;

	return &logMan;
}

LogManager::~LogManager(void)
{
	OUTFILEMAP::iterator file;
	for (file=outFiles.begin(); file!=outFiles.end(); ++file)
	{
		file->second->close();
		delete (file->second);
	}
	outFiles.clear();
}

void LogManager::AppendToLog(const String& text, const String& fileName)
{
	/* Define file address to write to */
	String address(fileName);
	address.append(".log");

	// File not found / not opened before
	if (outFiles.empty() || outFiles.find( fileName ) == outFiles.end())
	{
		outFiles.insert(OUTFILEPAIR(fileName, new std::ofstream(address.c_str())));
	}

	OUTFILEMAP::iterator currentFile = outFiles.find(fileName);
	std::ofstream& current = *(currentFile->second);
	current << text << std::endl;
}

LogFlags::LogFlags()
{
	// Initialise the flags
	m_bGlobalFlag		= true;
	m_bFileFlag			= false;
	m_bLineFlag			= false;
	m_bFunctionFlag		= true;
	m_bFunctionSignFlag = false;
	m_bTimestampFlag	= true;
}

const char LogManager::m_separator[] = "; ";
LogManager	smartlog;

LogManager::LogManager(void)
{
	m_bfirstline = true;
}

void LogManager::SetFilePath(char *file)
{
	strcpy(m_file, file);
}

size_t LogManager::GetFilePath(char *file, size_t buffersize) const
{
	if (buffersize >= strlen(m_file))
	{
		file[0] = '\0';
	}
	else
	{
		strcpy(file, m_file);
	}

	return strlen(m_file);
}

void LogManager::GetFlags(LogFlags& flags) const
{
	flags = m_flags;
}

void LogManager::SetFlags(const LogFlags& flags)
{
	m_flags = flags;
}


LogManager& LogManager::operator << (const bool value)
{
	if (value)
		WriteToFile("true");
	else
		WriteToFile("false");

	return *this;
}

LogManager& LogManager::operator << (const short value)
{
	char buffer[20];
	sprintf(buffer, "%d", (int)value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const unsigned short value)
{
	return *this;
}

LogManager& LogManager::operator << (const int value)
{
	char buffer[20];
	sprintf(buffer, "%d", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const unsigned int value)
{
	char buffer[20];
	sprintf(buffer, "%u", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const long value)
{
	char buffer[20];
	sprintf(buffer, "%d", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const unsigned long value)
{
	char buffer[20];
	sprintf(buffer, "%u", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const float value)
{
	char buffer[20];
	sprintf(buffer, "%f", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const double value)
{
	char buffer[20];
	sprintf(buffer, "%f", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const long double value)
{
	char buffer[20];
	sprintf(buffer, "%f", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const char value)
{
	char buffer[20];
	sprintf(buffer, "%c", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::operator << (const char* value)
{
	WriteToFile(value);
	return *this;
}

LogManager& LogManager::operator << (const void* value)
{
	char buffer[20];
	sprintf(buffer, "%p", value);
	WriteToFile(buffer);
	return *this;
}

LogManager& LogManager::WriteFlags(char *filename, size_t line, char *function, char *functionsign)
{
	char szBuffer [10240];
	char szTmpBuffer [1024];
	szBuffer[0] = '\0';

	if (m_bfirstline)
	{
		m_bfirstline = false;
	}
	else
	{
		strcat(szBuffer, "\r\n");
	}

	if (m_flags.m_bTimestampFlag)
	{
		SYSTEMTIME time;
		::GetLocalTime(&time);

		char szSecond[3], szMonth[3], szDay[3], szHour[3], szMinute[3];
		
		if (time.wSecond < 10)
		{
			szSecond[0] = '0';
			szSecond[1] = '\0';
		}
		else
			szSecond[0] = '\0';
		
		if (time.wMinute < 10)
		{
			szMinute[0] = '0';
			szMinute[1] = '\0';
		}
		else
			szMinute[0] = '\0';
		
		if (time.wHour < 10)
		{
			szHour[0] = '0';
			szHour[1] = '\0';
		}
		else
			szHour[0] = '\0';
		
		if (time.wDay < 10)
		{
			szDay[0] = '0';
			szDay[1] = '\0';
		}
		else
			szDay[0] = '\0';
		
		if (time.wMonth < 10)
		{
			szMonth[0] = '0';
			szMonth[1] = '\0';
		}
		else
			szMonth[0] = '\0';
		
		sprintf(szTmpBuffer, "%s%d/%s%d/%d %s%d:%s%d:%s%d ", 
			szMonth, time.wMonth, szDay, time.wDay, time.wYear, szHour, time.wHour, szMinute, time.wMinute, szSecond, time.wSecond);
		
		strcat(szBuffer, szTmpBuffer);
	}

	if (m_flags.m_bFileFlag)
	{
		sprintf(szTmpBuffer, "File: \"%s\"%s", filename, m_separator);
		strcat(szBuffer, szTmpBuffer);
	}

	if (m_flags.m_bLineFlag)
	{
		sprintf(szTmpBuffer, "Line: %d%s", line, m_separator);
		strcat(szBuffer, szTmpBuffer);
	}

	if (m_flags.m_bFunctionSignFlag)
	{
		sprintf(szTmpBuffer, "%s%s", functionsign, ": ");
		strcat(szBuffer, szTmpBuffer);
	}
	else
	{
		// this is ignored when m_bFunctionSignFlag is true
		sprintf(szTmpBuffer, "%s%s", function, ": ");
		strcat(szBuffer, szTmpBuffer);
	}

	WriteToFile(szBuffer);

	return *this;
}

bool LogManager::WriteToFile(LPCSTR p_log)
{
	if (m_flags.m_bGlobalFlag)
	{
		DWORD dwBytesWritten = 0;
		HANDLE hFile = INVALID_HANDLE_VALUE;

#if _DEBUG
		hFile = ::CreateFile (	m_file, GENERIC_WRITE, FILE_SHARE_READ, NULL,
								OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
#else
		hFile = ::CreateFile (	LPCWSTR(m_file), GENERIC_WRITE, FILE_SHARE_READ, NULL,
								OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
#endif

		if (INVALID_HANDLE_VALUE == hFile)
			return false;

		// append to file
		SetFilePointer(hFile,0,0,FILE_END);

#if _DEBUG
		if (0 == WriteFile(hFile, p_log, lstrlen(p_log), &dwBytesWritten, NULL))
#else
		if (0 == WriteFile(hFile, p_log, lstrlen(LPCWSTR(p_log)), &dwBytesWritten, NULL))
#endif
		{
			CloseHandle(hFile);
			return false;
		}
		CloseHandle(hFile);
	}
	return true;
}

END_ENGINE_NAMESPACE
