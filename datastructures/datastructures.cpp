
#include <iostream>
#include <string>
#define TEST_PQUEUE 1
//#define TEST_VECTOR 1
//#define TEST_DEQUE 1
//#define TEST_HASHSET 1

#ifdef TEST_VECTOR 
#include <vector>
#include "vector.h"
#endif
#ifdef TEST_DEQUE
#include <deque>
#include "devector.h"
#endif
#ifdef TEST_PQUEUE

//#include "vector.h"
//#include <queue>
#include "priority_queue.h"

#endif

#include <algorithm>
#include <intrin.h>

using namespace std;

#define CPUIDMACRO \
{ \
  int cpuidarr[4]; \
  __cpuid(cpuidarr, 0); \
}
#define ITERATIONS 300000

struct Vec2
{
  Vec2() { }
  //Vec2(int _x) : x(_x) { }
  //static void AB() { }
  int x, y;
  //std::vector<int> a;
  //tn::vector<int> a;
};

struct AB
{
  int k = -1;
  int l[2];
  Vec2 a[2];
  int* kz;
  AB() { }
  AB(int _k) :k(_k) { }
};

#ifdef TEST_VECTOR
tn::vector<int> Ret()
{
  tn::vector<int> asd2;
  asd2.push_back(22);
  asd2.push_back(24);
  return asd2;
}

void VectorTest()
{/*
  {
    typedef std::vector<int> O;
    O a1, a2, a3, a4, a5;
    a1.push_back(1);
    a2.push_back(2);

    tn::vector<O> asd;
    asd.push_back(a1);
    asd.push_back(a2);
    asd.push_back(a3);
    asd.push_back(a4);
    asd.push_back(a5);
  }
  */
  {
    tn::vector<int> vec;
    vec.push_back(10);
    vec.push_back(20);

    tn::vector<int> vec2;
    vec2.push_back(5);
    vec2.push_back(3);
    vec2.push_back(32);
    vec2.push_back(-1);
    vec2.push_back(1);

    vec = vec2;
    tn::vector<int> vec3(vec);
  }

  {
    tn::vector<int> vec4(Ret());
  }

  {
    tn::vector<int> vec4 = Ret();
    vec4.pop_back();
    vec4.pop_back();
    //vec4.pop_back(); //assert
  }

  {
    tn::vector<std::string> vec;
    vec.push_back("asd");
    vec.push_back("asd2");
    vec.push_back("asd3");
    vec.push_back("asd4");
    vec.push_back("asd5");
    //vec.erase(vec.begin());
    vec.erase(vec.begin() + 1, vec.begin() + 3);
  }
  {
    tn::vector<std::string> vec;
    vec.push_back("a1");
    vec.push_back("a2");
    vec.push_back("a3");
    std::vector<std::string> vec2;
    vec2.push_back("b1");
    vec2.push_back("b2");
    vec2.push_back("b3");
    //vec.insert(vec.begin() + 2, vec2.begin(), vec2.end());
    vec.insert(vec.end(), vec2.begin(), vec2.end());

    vec.insert(vec.begin(), { "v1", "v2" });
  }
}

void VectorPerfTest()
{
  cout << "VectorPerf\n";
#if 1
  {
    std::vector<int> vec;
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
      vec.push_back(i);
    unsigned long long end = __rdtsc();
    cout << "stl:" << end - start << endl;
  }
#endif

#if 1
  {
    tn::vector<int> vec2;
    CPUIDMACRO;
    unsigned long long start2 = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
      vec2.push_back(i);
    unsigned long long end2 = __rdtsc();
    cout << "tn: " << end2 - start2 << endl;
  }
#endif

  //complex type test
#if 1
  {
    std::vector<AB> vec;
    vec.reserve(ITERATIONS);
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    
    for (int i = 0; i < ITERATIONS; ++i)
      vec.push_back(AB());
    unsigned long long end = __rdtsc();
    cout << "stl:" << end - start << endl;
  }
#endif

#if 1
  {
    tn::vector<AB> vec2;
    vec2.reserve(ITERATIONS);
    CPUIDMACRO;
    unsigned long long start2 = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
      vec2.push_back(AB());
    unsigned long long end2 = __rdtsc();
    cout << "tn: " << end2 - start2 << endl;
  }
#endif

#if 1
  {
    std::vector<AB> vec;
    vec.reserve(ITERATIONS);
    CPUIDMACRO;
    unsigned long long start = __rdtsc();

    for (int i = 0; i < ITERATIONS; ++i)
      vec.emplace_back();
    unsigned long long end = __rdtsc();
    cout << "stl:" << end - start << endl;
  }
#endif
#if 1
  {
    tn::vector<AB> vec2;
    vec2.reserve(ITERATIONS);
    CPUIDMACRO;
    unsigned long long start2 = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
    {
      vec2.emplace_back();
      /*if (i < 10)
        cout << i << endl;*/
    }
    unsigned long long end2 = __rdtsc();
    cout << "tn: " << end2 - start2 << endl;
  }
#endif
  //cout << sizeof(vec) << endl;  //32 dbg/24 rel
  //cout << sizeof(vec2) << endl; //16

  //check: algo sorts, T: array[]

  //@date: 29/07/2019
  //@results for std::vector vs tn::vector : 
  //tn::vector has 15 functions compiled, std::vector has 80+
  //For type = int,  100% of the time tn::vector performs 33.3-100% better in release
  //For type=AB 100% of the time tn::vector beats std::vector by 50% in release.
}
#endif

#ifdef TEST_DEQUE
void DequeTest()
{
  {
    tn::devector<int> asd2;
    asd2.push_back(1);
    tn::devector<AB> asd;
    AB a;
    //a.a[0].a.push_back(1);
    asd.push_back(a);
    asd.push_back(AB());
    asd.push_back(AB());
  }

#if 1
  tn::devector<int> deq;
  auto p = [](int l, int r) -> bool
  {
    return l < r;
  };
  std::sort(deq.begin(), deq.end(), p);
  int intArray[7] = { 5, 3, 32, -1, 1, 104, 53 };
  std::sort(intArray, intArray + 7, p);
  tn::devector<int> deq2;
  deq2.push_back(5);
  deq2.push_back(3);
  deq2.push_back(32);
  deq2.push_back(-1);
  deq2.push_back(1);
  deq2.push_back(104);
  deq2.push_back(53);
  std::sort(deq2.begin(), deq2.end(), p);
  deq2 = deq;
  tn::devector<int> deq3(deq);

  std::vector<int> k = { 1, 2, 2, 3 };
  tn::devector<int> deq4(k.begin(), k.end());
#endif
  CPUIDMACRO;
  unsigned long long start = __rdtsc();
  int sum = 0;
  for (int i = 0; i < deq.size(); ++i)
    sum += deq[i];

  std::cout << "sum : " << sum << endl;
  unsigned long long end = __rdtsc();
  cout << end - start << endl;
  cout << "sz:" << sizeof(deq) << endl; //24
}

void DequePerfTest()
{
  cout << "DequePerf\n";
  srand(1);
#if 1
  {
    std::deque<int> deq;
CPUIDMACRO;
unsigned long long start = __rdtsc();
for (int i = 0; i < ITERATIONS; ++i)
{
  //deq.push_back(i);
  if ((rand() % 2) == 0)
    deq.push_back(i);
  else
    deq.push_front(i);
  /*if (i < 10)
  {
    for (int i = 0; i < deq.size(); ++i)
    {
      cout << deq[i] << " ";
    }
    cout << endl;
  }*/
  /*if (i % 3 == 0)
    deq.pop_front();*/
}
unsigned long long end = __rdtsc();
cout << "stl:" << end - start << endl;
  }
  //92 functions
#endif

#if 1
  srand(1);
  {
    tn::devector<int> deq;
    //deq.reserve_front(256 * 256);
    //deq.reserve_back(256 * 4);
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
    {
      //deq.push_back(i);
      if ((rand() % 2) == 0)
        deq.push_back(i);
      else
        deq.push_front(i);
      /*if (i < 10)
      {
        for (int i = 0; i < deq.size(); ++i)
        {
          cout << deq[i] << " ";
        }
        cout << endl;
      }*/
      /*if (i % 3 == 0)
        deq.pop_front();*/
    }
    unsigned long long end = __rdtsc();
    cout << "tn :" << end - start << endl;
  }
#endif
#if 1
  {
    std::deque<AB> deq;
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
    {
      deq.emplace_back(i);
    }
    unsigned long long end = __rdtsc();
    cout << "stl:" << end - start << endl;
  }
#endif
#if 1
  {
    tn::devector<AB> deq;
    //deq.reserve(256, 256);
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
    {
      deq.emplace_back(i);
    }
    unsigned long long end = __rdtsc();
    cout << "tn :" << end - start << endl;
  }
#endif
  //@date: 12/06/2019
  //@results for std::deque vs tn::devector :
  //tn::devector has 21 functions compiled, std::deque has 92+

  //under no reserve() conditions, push_back only
  //100% of the time tn::devector outperforms std::deque
  //tn::devector outperforms std::deque by 400%

  //under no reserve() conditions, random push_back and push_front 
  //tn::devector outperforms std::deque by ~25% 100% of the time

  //under reserve() conditions for tn::devector, 
  //tn::devector shows little to no improvements
}
#endif

#ifdef TEST_HASHSET
void HashsetPerfTest()
{

}
#endif

#ifdef TEST_PQUEUE
void PQueueTest()
{
  int myints[] = { 10,20,30,5,15 };
#if 1
  {
    tn::priority_queue<int> v(myints, myints + 5);

    while (!v.empty())
    {
      //cout << v.top() << " ";
      v.pop();
    }

    //cout << endl;

    v.c.push_back(5);
    v.c.push_back(15);
    v.c.push_back(25);
    v.c.push_back(12);
    v.c[0] = 14;
    v.resort();
    while (!v.empty())
    {
      //cout << v.top() << " ";
      v.pop();
    }
    //cout << endl;
  }
#endif
#if 0
  {
    std::priority_queue<int, tn::vector<int>> v(myints, myints + 5);
    while (!v.empty())
    {
      //cout << v.top() << " ";
      v.pop();
    }

    //cout << endl;

    //v.push(5);
    v.push(14);
    v.push(15);
    v.push(25);
    v.push(12);
    while (!v.empty())
    {
      //cout << v.top() << " ";
      v.pop();
    }
    //cout << endl;
  }
#endif
  //since tn::priority_queue is a functionality modification, the compile perf is about the same
  //however we exclude the other noisy stl headers. PQueueTest with std version generates more instructions (1381 vs 1191)
}

void PQueuePerfTest()
{
#if 0
  {
    tn::priority_queue<int> v;
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
    {
      v.emplace(i);
    }
    unsigned long long end = __rdtsc();
    cout << "tn:" << end - start << endl;
  }

  {
    std::priority_queue<int, tn::vector<int>> v;
    CPUIDMACRO;
    unsigned long long start = __rdtsc();
    for (int i = 0; i < ITERATIONS; ++i)
    {
      v.emplace(i);
    }
    unsigned long long end = __rdtsc();
    cout << "stl:" << end - start << endl;
  }
#endif
  //since tn::priority_queue is a functionality modification, the perf is about the same
}
#endif

int main()
{
#ifdef TEST_VECTOR 
  VectorTest();
  VectorPerfTest();

  {
    tn::vector<int> a;
    std::cout << sizeof(a) << std::endl;
    a.reserve(16);
    a.push_back(15);
    a.push_back(17);
    struct Typeless
    {
      unsigned int a;
      unsigned int b;
      void* arr;
    };
    auto t = (Typeless*)&a;
    std::cout << t->a << std::endl;
    std::cout << t->b << std::endl;
    std::cout << t->arr << " " << a.arr << std::endl;
  }
#endif

  /*******************************/
#ifdef TEST_DEQUE
  DequeTest();
  DequePerfTest();
#endif

#ifdef TEST_PQUEUE
  PQueueTest();
  PQueuePerfTest();
#endif

  return 0;
}

