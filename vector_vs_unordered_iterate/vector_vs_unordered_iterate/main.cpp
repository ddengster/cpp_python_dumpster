
#include <iostream>
#include <vector>
#include <unordered_set>

#include <Windows.h>
#include "Timer.h"

using namespace std;

struct Item
{
  int val = 10;
  int k = 111;
  double a = 6.2;
  char str[64];
  int aval[116];
};

int main()
{
  TIMER_INIT;
  Item* items_arr = new Item[16000];
  for (int i = 0; i < 16000; ++i)
  {
    if (i % 2 == 0)
    {
      strcpy_s(items_arr[i].str, "ff");
      memset(items_arr[i].aval, 4, sizeof(int)*116);
    }
    if (i == 534)
      items_arr[i].val = 443;
  }
  Item* item_to_find = items_arr + 1442;

#define ITEM_COUNT 40 //reduce for smaller item sets, which is what we have in most games
  std::vector<Item*> items;
  for (int i = 0; i < ITEM_COUNT; ++i)
    items.push_back(&items_arr[i]);

#define ITR_COUNT 19000
  float totaltimer = 0.0f;
  for (int l = 0; l < ITR_COUNT; ++l)
  {
    TIMER_START;
    for (int i = 0; i < items.size(); ++i)
    {
      items[i]->val = items[i]->val + items[i]->k;
    }
    float timer = 0.0f;
    TIMER_STOP(timer);
    totaltimer += timer;
  }
  cout << "item vector average timing:" << totaltimer / ITR_COUNT << endl;
  
  std::unordered_set<Item*> items_set;
  for (int i = 0; i < ITEM_COUNT; ++i)
    items_set.insert(&items_arr[i]);

  totaltimer = 0.0f;
  for (int l = 0; l < ITR_COUNT; ++l)
  {
    TIMER_START;
    for (auto itr : items_set)
    {
      itr->val = itr->val + itr->k;
    }
    float timer = 0.0f;
    TIMER_STOP(timer);
    totaltimer += timer;
  }
  cout << "item unorderedset average timing:" << totaltimer / ITR_COUNT << endl;

  //search
  totaltimer = 0.0f;
  for (int l = 0; l < ITR_COUNT; ++l)
  {
    int idx = l;
    if (idx >= (items.size() - 1))
      idx = items.size() - 1;
    items.insert(items.begin() + idx, item_to_find);
    TIMER_START;
    for (int i = 0; i < items.size(); ++i)
    {
      if (items[i] == item_to_find)
      {
        items.erase(items.begin() + i);
        break;
      }
    }
    float timer = 0.0f;
    TIMER_STOP(timer);
    totaltimer += timer;
  }
  cout << "item vector search to erase average timing:" << totaltimer / ITR_COUNT << endl;

  totaltimer = 0.0f;
  for (int l = 0; l < ITR_COUNT; ++l)
  {
    items_set.insert(items_set.begin(), item_to_find);
    TIMER_START;
    items_set.erase(item_to_find);
    float timer = 0.0f;
    TIMER_STOP(timer);
    totaltimer += timer;
  }
  cout << "item unorderedset search to erase average timing:" << totaltimer / ITR_COUNT << endl;

  system("pause");
  return 0;
}