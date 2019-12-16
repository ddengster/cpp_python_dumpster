
#ifndef T1_H
#define T1_H

#include "profiler\Profiler.h"
#include <map>
#include <iostream>
#include <vector>
#include <hash_map>

using namespace std;

class TestObj
{
public:
  float a[30];
};

void test1()
{
  std::vector<TestObj*> v;
  int count = 100;
  TestObj* newobj = 0;
  std::multimap<int, TestObj*> map;
  std::hash_multimap<int, TestObj*> hashmap;

  while ((float)clock() / (float)CLOCKS_PER_SEC < 5.f)
  {
    for (int i=0; i<count; ++i)
    {
      newobj = new TestObj();

       //insert
      {
        Profiler profile("Vector insert profiler");
        v.push_back(newobj);
      }
      
      {
        Profiler profile("Map insert profiler");
        //map.insert(make_pair(i, newobj));
        map.insert(std::pair<int, TestObj*>(i, newobj));
      }
      {
        Profiler profile("HashMap insert profiler");
        //map.insert(make_pair(i, newobj));
        hashmap.insert(std::pair<int, TestObj*>(i, newobj));
      }
      
    }
    hashmap.clear();
  }
}

#endif
