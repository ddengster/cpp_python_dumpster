
#include <iostream>
#include <map>
#include <unordered_map>
#include "flat_map.h"
#include <string>

#include <intrin.h>

using namespace std;
class Object
{
public:
  //string mName;
  int mHandle = 0;
  int mRefCount = 0;
};

#define CPUIDMACRO \
{ \
  int cpuidarr[4]; \
  __cpuid(cpuidarr, 0); \
}

int main()
{
#define COUNT 300
#if 1
  {
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    std::map<string, Object> r1;
    for (int i = 0; i < COUNT; ++i)
    {
      Object a;
      //a.mName = "asdsd";
      //a.mName.append(1, '1' + i);
      char buf[2] = { 0 };
      buf[0] = (char)i;
      r1.insert(std::make_pair(string(buf), a));
    }
    CPUIDMACRO;
    unsigned long long end = __rdtsc();
    cout << end - start << endl;

    CPUIDMACRO;
    unsigned long long start2 = __rdtsc();
    for (auto r : r1)
    {
      ++r.second.mHandle;
      r.second.mRefCount *= 2;
    }
    CPUIDMACRO;
    unsigned long long end2 = __rdtsc();
    cout << end2 - start2 << endl;

    CPUIDMACRO;
    unsigned long long start3 = __rdtsc();
    char buf[2] = { 0 };
    buf[0] = (char)88;
    auto f = r1.find(buf);
    f->second.mHandle *= 2;
    CPUIDMACRO;
    unsigned long long end3 = __rdtsc();
    cout << end3 - start3 << endl;
  }
#endif
  cout << endl;
#if 1
  {
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    chobo::flat_map<string, Object> r1;
    for (int i = 0; i < COUNT; ++i)
    {
      Object a;
      //a.mName = "asdsd";
      //a.mName.append(1, '1' + i);
      //r1.insert(std::make_pair(a.mName, a));
      char buf[2] = { 0 };
      buf[0] = (char)i;
      r1.insert(std::make_pair(string(buf), a));
    }
    CPUIDMACRO;
    unsigned long long end = __rdtsc();
    cout << end - start << endl;

    CPUIDMACRO;
    unsigned long long start2 = __rdtsc();
    for (auto r : r1)
    {
      ++r.second.mHandle;
      r.second.mRefCount *= 2;
    }
    CPUIDMACRO;
    unsigned long long end2 = __rdtsc();
    cout << end2 - start2 << endl;

    CPUIDMACRO;
    unsigned long long start3 = __rdtsc();
    char buf[2] = { 0 };
    buf[0] = (char)88;
    auto f = r1.find(buf);
    f->second.mHandle *= 2;
    CPUIDMACRO;
    unsigned long long end3 = __rdtsc();
    cout << end3 - start3 << endl;
  }
#endif
  cout << endl;
#if 1
  {
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    std::unordered_map<string, Object> r1;
    for (int i = 0; i < COUNT; ++i)
    {
      Object a;
      //a.mName = "asdsd";
      //a.mName.append(1, '1' + i);
      //r1.insert(std::make_pair(a.mName, a));
      char buf[2] = { 0 };
      buf[0] = (char)i;
      r1.insert(std::make_pair(string(buf), a));
    }
    CPUIDMACRO;
    unsigned long long end = __rdtsc();
    cout << end - start << endl;

    CPUIDMACRO;
    unsigned long long start2 = __rdtsc();
    for (auto r : r1)
    {
      ++r.second.mHandle;
      r.second.mRefCount *= 2;
    }
    CPUIDMACRO;
    unsigned long long end2 = __rdtsc();
    cout << end2 - start2 << endl;

    CPUIDMACRO;
    unsigned long long start3 = __rdtsc();
    char buf[2] = { 0 };
    buf[0] = (char)88;
    auto f = r1.find(buf);
    f->second.mHandle *= 2;
    CPUIDMACRO;
    unsigned long long end3 = __rdtsc();
    cout << end3 - start3 << endl;
  }
#endif
  system("pause");
  return 0;
}