
#include "LogManager/LogManager.h"
#include "Profiler/Profiler.h"
#include <time.h>

using namespace ddengine_RenderEngine;

const int32_t repeats = 100000000;
const int32_t value = 2;

void Addition(int32_t count=10000000)
{
	Profiler startProfile("Addition");
	int32_t store = 0;
	for (int32_t i=0; i<count; ++i)
	{
		store += value;
	}
}

int main()
{
	while ((Real)clock() / (Real)CLOCKS_PER_SEC < 5.f)
	{
		Profiler startProfile("Main Loop");
		Addition();
	}

	LogProfileData();

	return 0;
}