
#include <iostream>
#include "cThreadPool.h"
#include <Windows.h>

#include "Timer.h"

using namespace std;
void Sleep(void* ms)
{
  std::cout << "starting sleep task: " << *(int*)ms << "ms\n";
  Sleep(*(int*)ms);
  std::cout << "ending sleep task!\n";
}

void Done(void* ms)
{
  std::cout << "Sleep task Done! " << *(int*)ms << "ms\n";
}

void RunParticles(void* p)
{
  //std::cout << "P: " << (int)p << "\n";
  //Sleep(1);
  int sum = 1;
  for (unsigned long long i = 0; i < 10000; ++i)
  {
    sum += ((int)p + 1) * i * i;
  }
  //std::cout << "sum: " << sum << std::endl;
  *(int*)p = sum;
}

#define MAX_PARTICLES 20000
int main()
{
  TIMER_INIT;

  cThreadPool tp;
  int particledata[MAX_PARTICLES] = { 0 };
  memset(particledata, 0, sizeof(particledata));

  int mssleep = 10;
  tp.EnqueueTask(Sleep, (void*)&mssleep, Done);

  cThreadPool particles_tp;

  //Instantiate multiple thread pools

  //todo: replace deque with multiple indices and a circular array?
  while (1)
  {/*
    int r = rand() % 8;
    for (int i = 0; i < r; ++i)
    {
      mssleep++;
      mssleep = mssleep % 12;
      tp.EnqueueTask(Sleep, (void*)&mssleep, Done);
    }

    tp.WaitForNoTasks();
    std::cout << "tasks finished\n";
    */
    float time = 0.0f;
    TIMER_START;

    Task s[MAX_PARTICLES];
    for (int i = 0; i < MAX_PARTICLES; ++i)
    {
      s[i].mTaskFunc = RunParticles;
      s[i].mTaskUserData = (void*)(particledata + i);
    }
    particles_tp.EnqueueMultipleTasks(MAX_PARTICLES, s);
    particles_tp.WaitForNoTasks();

    TIMER_STOP(time);
    std::cout << "Time taken: " << time << std::endl;

    //tp.UpdateTasks();
    //system("pause");
    Sleep(1000);
    std::cout << "end loop\n";
  }
  return 0;
}