

#include "Profiler.h"

#include "LogManager.h"
#include <sstream>
#include <math.h>

const float SEC_PER_TICK = 1.0f / float(CLOCKS_PER_SEC);

/* Define storage space in memory for static class variables */
Profiler::PROFILEMAP Profiler::profileList;
std::vector<String> Profiler::profileQuene;

/* Profiling starts */
Profiler::Profiler(const String& _profileName)
{
	/* First Frame */
	static String firstProfile("");
	static bool pastFirstFrame = false;
	if (!pastFirstFrame && firstProfile != "")
	{
		firstProfile = _profileName;
	}
	else if (!pastFirstFrame && _profileName == firstProfile)
	{
		pastFirstFrame = true;
	}

	profileIter = profileList.find(_profileName);
	if (profileList.empty() || profileIter == profileList.end())
	{
		/* All profiles must be created by the 1st iteration/frame */
		if (pastFirstFrame)
		{
			profileIter = profileList.end();
			return;
		}
		/* Create a "Profile" */
		profileList.insert(PROFILEPAIR(_profileName, ProfileData()));
		profileIter = profileList.find(_profileName);
		profileQuene.push_back(_profileName);
	}
	profileIter->second.isValid = true;
	profileIter->second.instancesCount++;

	/* Check for parent */
	profileIter->second.numOfParents = 0;
	PROFILEMAP::iterator parent;
	unsigned int numParentStore = 0;
	for (parent=profileList.begin(); parent!=profileList.end(); ++parent)
	{
		if (parent != profileIter && parent->second.isValid == true)
		{
			if (parent->second.numOfParents >= numParentStore)
			{
				numParentStore = parent->second.numOfParents;
				profileIter->second.parentIter = parent;
			}
			++(profileIter->second.numOfParents);
			//break;
		}
	}
	if (profileIter->second.numOfParents == 0)	//no parents
	{
		profileIter->second.parentIter = profileList.end();
	}

	startClock = clock();	//Get current time
}

/* Profiling stops */
Profiler::~Profiler( void )
{
	if (profileIter == profileList.end()) return;

	endClock = clock();
	unsigned int clockDiff = endClock - startClock;

	float timeDiff = clockDiff * SEC_PER_TICK;

	profileIter->second.timeTaken += timeDiff;

	if (profileIter->second.minimum > timeDiff)
	{
		profileIter->second.minimum = timeDiff;
	}
	if (profileIter->second.maximum < timeDiff)
	{
		profileIter->second.maximum = timeDiff;
	}

	profileIter->second.isValid = false;
}

/* Return a string containing information about the profile */
String ProfileData::WriteAsString( const String& profileName )
{
	std::ostringstream ss;
	//ss.setf(ss.fixed);
	//ss.precision(2);
	ss << "  " << ceil((timeTaken/(float)instancesCount)*1000) << "\t\t|"		//Average Time per iteration
	   << "  " << ceil(minimum*1000)							  << "\t\t|"		//Min time taken
	   << "  " << ceil(maximum*1000)							  << "\t\t|"		//Max time taken
	   << "  "   << profileName									//Profile Name
	   << std::endl;

	return ss.str();
}

void LogProfileData( void )
{
	String result("Time per frame:\n");
	result.append	  ("----------------------------------------------------------------\n");
	result.append	  ("  Ave(ms)\t|  Min(ms)\t|  Max(ms)\t|  Profile Name\n");
	result.append	  ("----------------------------------------------------------------\n");

	String totalTime;

	for (std::vector<String>::iterator iter=Profiler::profileQuene.begin(); iter!=Profiler::profileQuene.end(); ++iter)
	{
		Profiler::PROFILEMAP::iterator profileIter = Profiler::profileList.find(*iter);
		String offset;
		for (unsigned count=profileIter->second.numOfParents; count>0; --count)
		{
			offset.append("   ");
		}

		std::ostringstream temp;
		temp << offset << "Profile \"" << profileIter->first << "\" takes a total of " << profileIter->second.timeTaken << " seconds.\n";
		totalTime.append(temp.str());

		result.append(profileIter->second.WriteAsString(offset.append(profileIter->first)));
	}

	result.append	  ("----------------------------------------------------------------\n");
	result.append("\n\n").append(totalTime);

	LogManager::GetInstance()->AppendToLog(result, "profileLog");
}
