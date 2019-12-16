
#ifndef _PROFILER_H
#define _PROFILER_H

#include <ctime>
#include <map>
#include <vector>

typedef std::string String;
	/** \defgroup Profiling Profiling
 * @{
 */

class Profiler;
/**
 * \class ProfileData
 * Used internally by Profiler to keep track of profiling data.
 */
struct ProfileData
{
private:
	friend class Profiler;
	friend void LogProfileData(void);

	ProfileData(void)
		: timeTaken(0), minimum(9999999.0f), maximum(0), instancesCount(0), timesCalledPerFrame(0), numOfParents(0), isValid(true) {}

	float timeTaken, minimum, maximum;											//Time related parameter
	unsigned int instancesCount, timesCalledPerFrame;								//Number of calls
	unsigned int numOfParents;														//Number of Parents
	std::map<String, ProfileData>::iterator parentIter;					//Keeping Track of immediate parent
	bool isValid;																//Is current Profile running
	std::vector<std::map<String, ProfileData>::iterator> childIterList;	//List of children

	String WriteAsString(const String& profileName);
};

/**
 * \class Profiler
 *
 * Class to measure time taken for specified functions
 * 
 * \Header
 */
class Profiler
{
public:
	Profiler(const String& _profileName);
	~Profiler(void);

private:
	/* Store profile name and profile numbers */
	typedef std::map<String, ProfileData> PROFILEMAP;
	typedef std::pair<String, ProfileData> PROFILEPAIR;

	friend void LogProfileData(void);

	static PROFILEMAP profileList;
	static std::vector<String> profileQuene;
	PROFILEMAP::iterator profileIter;
	clock_t startClock, endClock;
};

/**
 * Function to output all profiled data to a file. The file will be named "profileLog.txt".
 */
void LogProfileData(void);

/** @} */ // End Profiling

/** \page profiler_demo Using the Profiler class
 * \section profilerusage Usage
 * RenderEngine provides a Profiler class that can be used to measure the performance of
 * specific functions or blocks of code.
 *
 * To use the Profiler, create an instance of the Profiler class on the stack,
 * at the point where you wish to start profiling code. You can pass in a description
 * to the Profiler object's constructor.
 * \code
 * void myFunction()
 * {
 *     Profiler profiler("myFunction");
 *     [do processing here...]
 * }
 * \endcode
 * Note that the profiler object will keep track of the time the rest of the code
 * takes up to the point when it is destroyed. In this case, the profiling starts
 * at the point where myFunction begins, and ends at the end of myFunction.
 *
 * Profiler objects can be nested, and they will keep track of the number of times
 * they are called.
 *
 * Finally, to output the Profiler data, do a call to RenderEngine's LogProfileData() at
 * the end of the program.
 * \code
 * LogProfileData();
 * \endcode
 */

#endif
