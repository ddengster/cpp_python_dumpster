
#ifndef _CTHREADPOOL_H
#define _CTHREADPOOL_H

#include <thread>
#include <deque>
#include <mutex>
#include <condition_variable> 

#define THREADPOOL_MAXTHREADCOUNT 4

enum TP_THREAD_STATUS
{
  IS_IDLE = 0,
  IS_RUNNING_TASK
};

struct TP_ThreadStatus
{
  TP_THREAD_STATUS mStatus = IS_IDLE;
  bool mRun = true;
};

typedef void(*TaskFunc)(void*);
typedef void(*TaskFinishFunc)(void*);

struct Task
{
  TaskFunc mTaskFunc = nullptr;
  void* mTaskUserData = nullptr;
  TaskFinishFunc mTaskFinishFunc = nullptr;
  //can add timers here
};

//Some info: http://www.gamecareerguide.com/news/172072/indepth_in_praise_of_idleness.php
//http://www.gamedev.net/page/resources/_/technical/general-programming/multithreading-r3048
//http://stackoverflow.com/questions/4792449/c0x-has-no-semaphores-how-to-synchronize-threads/19659736#19659736
class cThreadPool
{
public:
  cThreadPool(int threadcount = 0);
  ~cThreadPool();

  //!Calls mTaskFinishFunc on all finished tasks. You can call this every update for unbounded tasks (without a strict time limit)
  void UpdateTasks(); 
  //! Wait for all tasks to be completed, then call UpdateTasks. Useful for ensuring that all task need to be completed within a certain time frame
  void WaitForNoTasks();

  int EnqueueTask(TaskFunc mfunc, void* userdata, TaskFinishFunc finishedfunc = nullptr);
  void EnqueueMultipleTasks(int count, Task* taskarry); //locks once, submits all tasks, unlocks

protected:
  friend void TP_Spin(TP_ThreadStatus* status, cThreadPool* tp);

  std::thread mThreads[THREADPOOL_MAXTHREADCOUNT];
  TP_ThreadStatus mThreadStatus[THREADPOOL_MAXTHREADCOUNT];

  std::mutex mTaskQueueMutex;
  std::deque<Task> mTaskQueue;

  std::condition_variable mHaveTaskCV;
  std::mutex mHasTaskMutexSemaphore;
  int mTaskCount;

  std::timed_mutex mTaskDoneQueueMutex;
  std::deque<Task> mTaskDoneQueue;
};


#endif
