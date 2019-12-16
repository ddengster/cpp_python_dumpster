
#include "cThreadPool.h"
#include <iostream>

//If USE_BUSYWAIT is defined, we will busy wait, calling yield() when there is no task. This results in wasted cpu cycles (though slightly less efficient than a event based waiting mode)
//Else we use mHasTaskMutexSemaphore to wait for tasks to appear. 
//#define USE_BUSYWAIT 1

//17/2/2015: Semaphore vs busywait Performance is roughly equal. busywait cpu usage 100%, Semaphore fixes it.

void TP_Spin(TP_ThreadStatus* status, cThreadPool* tp)
{
  status->mStatus = IS_IDLE;
  
  while (status->mRun)
  {
    status->mStatus = IS_IDLE;
#ifndef USE_BUSYWAIT
    if (tp->mTaskCount == 0)
    {
      std::unique_lock<std::mutex> lock(tp->mHasTaskMutexSemaphore); //performance hog to create this every loop
      while (tp->mTaskCount == 0)
        tp->mHaveTaskCV.wait(lock);
    }
#endif
    Task t;
    bool hastask = false;
    //look in current task queue
    
    tp->mTaskQueueMutex.lock();
    if (!tp->mTaskQueue.empty())
    {
      t = tp->mTaskQueue.front();
#ifndef USE_BUSYWAIT
      --tp->mTaskCount;
#endif
      tp->mTaskQueue.pop_front();
      hastask = true;
    }
    tp->mTaskQueueMutex.unlock();
    
    //if there is something in the task q, grab it and switch status
    if (hastask)
    {
      status->mStatus = IS_RUNNING_TASK;
      t.mTaskFunc(t.mTaskUserData);

      tp->mTaskDoneQueueMutex.lock();
      tp->mTaskDoneQueue.push_back(t);
      tp->mTaskDoneQueueMutex.unlock();

      //std::cout << "Current id: " << std::this_thread::get_id() << std::endl;
    }
#ifdef USE_BUSYWAIT
    else
      std::this_thread::yield(); //can replace with a condition variable
#endif
  }
}

cThreadPool::cThreadPool(int threadcount)
{
  mTaskCount = 0;
  if (threadcount == 0)
    threadcount = THREADPOOL_MAXTHREADCOUNT;
  else
    threadcount = threadcount > THREADPOOL_MAXTHREADCOUNT ? THREADPOOL_MAXTHREADCOUNT : threadcount;
  for (int i = 0; i < threadcount; ++i)
  {
    mThreads[i] = std::thread(TP_Spin, mThreadStatus + i, this);
  }
}

cThreadPool::~cThreadPool()
{
  for (int i = 0; i < THREADPOOL_MAXTHREADCOUNT; ++i)
    mThreadStatus[i].mRun = false;
#ifndef USE_BUSYWAIT
  mTaskCount = 1;
  mHaveTaskCV.notify_all();
#endif
  for (int i = 0; i < THREADPOOL_MAXTHREADCOUNT; ++i)
    mThreads[i].join();
}

void cThreadPool::UpdateTasks()
{
  mTaskDoneQueueMutex.lock();
  for (int i = 0; i < mTaskDoneQueue.size(); ++i)
  {
    if (mTaskDoneQueue[i].mTaskFinishFunc)
      mTaskDoneQueue[i].mTaskFinishFunc(mTaskDoneQueue[i].mTaskUserData);
  } 
  mTaskDoneQueue.clear();
  mTaskDoneQueueMutex.unlock();
}

void cThreadPool::WaitForNoTasks()
{
  volatile size_t taskqueuesize = 0; //makes sure that no optimization into a const takes place
  do
  {
    taskqueuesize = mTaskQueue.size();
    //std::cout << mTaskQueue.size();
  } while (taskqueuesize != 0);

  UpdateTasks();
}

int cThreadPool::EnqueueTask(TaskFunc mfunc, void* userdata, TaskFinishFunc finishedfunc)
{
  static int counter = 0;
  Task t;
  t.mTaskFunc = mfunc;
  t.mTaskUserData = userdata;
  t.mTaskFinishFunc = finishedfunc;

  mTaskQueueMutex.lock();
  mTaskQueue.push_back(t);
#ifndef USE_BUSYWAIT
  ++mTaskCount;
#endif
  mTaskQueueMutex.unlock();
#ifndef USE_BUSYWAIT
  mHaveTaskCV.notify_one();
#endif
  return counter++;
}

void cThreadPool::EnqueueMultipleTasks(int count, Task* taskarry)
{
  mTaskQueueMutex.lock();
  for (int i = 0; i < count; ++i)
  {
    mTaskQueue.push_back(taskarry[i]);
  }
#ifndef USE_BUSYWAIT
  mTaskCount += count;
#endif
  mTaskQueueMutex.unlock();

#ifndef USE_BUSYWAIT
  mHaveTaskCV.notify_all();
#endif
}