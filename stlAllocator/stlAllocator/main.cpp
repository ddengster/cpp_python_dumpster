
#include <iostream>
#include <string>

#include <Windows.h>
#include "Timer.h"

#include "Allocator.h"
#include <vector>

int main()
{
  TIMER_INIT;
  std::string asd("ADSADSA");

  std::vector<int, Allocator<int>> foo = { 10, 20, 30 };

  float t = 0.0f;
  {
    TIMER_START;

    std::string sum;
    for (int i = 0; i < 1000; ++i)
    {
      char v = '0' + (char)i;
      std::string s(&v);

      sum.append(s.c_str());
    }
    TIMER_STOP(t);
    std::cout << "time: " << t;
  }

  {
    
    typedef std::basic_string<char, std::char_traits<char>, PoolAllocator<char> > String;

    TIMER_START;

    String sum;
    for (int i = 0; i < 1000; ++i)
    {
      char v = '0' + (char)i;
      String s(&v);

      sum.append(s.c_str());
    }

    TIMER_STOP(t);
    std::cout << "time: " << t;
    
  }

  delete[] mStringPool;
  return 0;
}