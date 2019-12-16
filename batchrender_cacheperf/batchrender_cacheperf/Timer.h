
#pragma once

long long gFrequencyQuadPart;

#define TIMER_INIT \
  static LARGE_INTEGER gFrequency; \
  QueryPerformanceFrequency(&gFrequency); \
  gFrequencyQuadPart = gFrequency.QuadPart;

#define TIMER_START \
  LARGE_INTEGER t1,t2; \
  QueryPerformanceCounter(&t1);

#define TIMER_STOP(timerVar) \
    QueryPerformanceCounter(&t2); \
    timerVar = (float)(t2.QuadPart - t1.QuadPart) / gFrequencyQuadPart; 

#include <intrin.h>

#define CPUIDMACRO \
{ \
  int cpuidarr[4]; \
  __cpuid(cpuidarr, 0); \
}
