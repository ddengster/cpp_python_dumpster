
#ifndef _LOGMANAGER_H
#define _LOGMANAGER_H

#include <string>
#include <map>

#define MAX_PATH_LEN 260

typedef std::string String;
class LogFlags
{
public:
	/**
		 * constructor for LogFlags
		 * Set the member variables of the flag for logging
		 * 
		 */
	LogFlags();

	// Global flag for logs. When set to false, no logs are made
	bool	m_bGlobalFlag;

	// When true, logs the name of the file from where the logging is made
	bool	m_bFileFlag;

	// When true, logs the line number from where the logging is made
	bool	m_bLineFlag;

	// When true, logs the name of the function name
	bool	m_bFunctionFlag;

	// When true, logs the function, including name and its signature
	// When true, m_bFunctionFlag is ignored
	bool	m_bFunctionSignFlag;

	// When true, logs the time stamp
	bool	m_bTimestampFlag;
};

/** \ingroup Misc
 * \class LogManager
 *
 * Used to output lines of text to a .log file, for easier debugging. 
 * Some code here is referenced from www.codeproject.com/KB/mcpp/smartlog.aspx by Adrian-Bogdan Andreias
 *
 * \Header
 */
class LogManager
{
public:
	LogManager(void);
	~LogManager(void);

	static LogManager* GetInstance(void);

	/** 
	 * Add text to the file of the given name
	 * @param text Text content
	 * @param fileName Name of file, without (.log)
	 */
	void AppendToLog(const String& text, const String& fileName="RenderEngine");

	/**
		 * SetFilePath functions.
		 * Sets the file to which the logs are written
		 */
	void SetFilePath(char *file);
		
	/**
		 * GetFilePath functions.
		 * Gets the file to which the logs are written
		 * file will contain the file path if the file path is not longer then m_dwBufferSize
		 * returns length of the path string
		 */
	size_t GetFilePath(char *file, size_t buffersize) const;

	void GetFlags(LogFlags& flags) const;
	void SetFlags(const LogFlags& flags);

	LogManager& operator << (const bool value);

	LogManager& operator << (const short value);

	LogManager& operator << (const unsigned short value);

	LogManager& operator << (const int value);

	LogManager& operator << (const unsigned int value);

	LogManager& operator << (const long value);

	LogManager& operator << (const unsigned long value);

	LogManager& operator << (const float value);

	LogManager& operator << (const double value);

	LogManager& operator << (const long double value);

	LogManager& operator << (const char value);

	LogManager& operator << (const char* value);

	LogManager& operator << (const void* value);
	
	LogManager& WriteFlags(char *filename, size_t line, char *function, char *functionsign);

protected:

	// Protected methods
	bool WriteToFile(const char * p_log);
	char m_file[MAX_PATH_LEN];
	bool m_bfirstline;
	static const char m_separator[];
	LogFlags	m_flags;

private:
	typedef std::pair<const String, std::ofstream*> OUTFILEPAIR;
	typedef std::map<const String, std::ofstream*> OUTFILEMAP;
	OUTFILEMAP outFiles;	//ofstream instances of opened files
};

extern LogManager	smartlog;
#define LOG			smartlog.WriteFlags(__FILE__, __LINE__, __FUNCTION__, __FUNCSIG__)


#endif
