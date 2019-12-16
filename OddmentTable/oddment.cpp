
/*
http://www.godpatterns.com/2006/01/oddment-table.html
eg. You have 10 cards. 1 of them is the rare item card, the rest(9) are common items.
    This algorithm does a rand() % numberofcards, then adds the number of oddments(cards per type of item),
    and returns the type of item if the random number is less than or equal to the oddment count. if not, 
    it continues on until a suitable item is reached.
    eg. commonitem = 9, rareitem = 1
    randomnumber = 10
    9 >= 10 -> fail, next loop
    (9+1) >= 10 -> pass, return 2nd item
    randomnumber = any number from 1 to 9
    (1 to 9) >= 9 -> return 1st item
    Therefore, only the number 10 from a range of 1 - 10 will work for the rare item, essentially
    giving a 1 in 10 chance(not exact) of getting a rare item.
    
    OR another way to look at this is:
    
    10  | Rare   |
    9   | Common |
    .   | Common |
    .   | Common |
    .   | Common |
    .   | Common |
    1   | Common |
    10 cards, pick 1 at random from the bottom
    
    13  | Unique   |
    12  | Rare   |
    11  | Rare   |
    10  | Rare   |
    9   | Common |
    .   | Common |
    .   | Common |
    .   | Common |
    .   | Common |
    1   | Common |
    13 cards, pick 1 at random from the bottom
    
    Not really a good way of setting an exact probability for a certain item
    due to the rng.
*/

#include <iostream>
#include "oddment.h"

using namespace std;

class myEvent
{
public:
  myEvent() { }
  ~myEvent() { }
  
  int val;
};

int main()
{
#if 0
  OddmentTable<myEvent> *table = new OddmentTable<myEvent>(10);
  
  myEvent dropcommonitem;
  dropcommonitem.val = 1;
  table->AddEntry(dropcommonitem, 9);
  myEvent droprareitem;
  droprareitem.val = 2;
  table->AddEntry(droprareitem, 1);
  
  for (int i=0; i < 10; ++i)
  {
    myEvent evt = table->PickEntry();
    if (evt.val == 1)
      cout << "Common item" << endl;
    else if (evt.val == 2)
      cout << "Rare item" << endl;
  }
  
  delete table;
#endif

#if 1
  OddmentTable<myEvent> *table = new OddmentTable<myEvent>(2);
  
  myEvent dropcommonitem;
  dropcommonitem.val = 1;
  myEvent droprareitem;
  droprareitem.val = 2;
  myEvent dropuniqueitem;
  dropuniqueitem.val = 3;
  
  table->AddEntry(dropcommonitem, 9);
  table->AddEntry(droprareitem, 3);
  
  table->AddEntry(dropuniqueitem, 1);
  
  
  for (int i=0; i < 20; ++i)
  {
    myEvent evt = table->PickEntry();
    if (evt.val == 1)
      cout << "Common item" << endl;
    else if (evt.val == 2)
      cout << "Rare item" << endl;
    else if (evt.val == 3)
      cout << "Unique item" << endl;
  }
  
  delete table;
#endif
  return 0;
}
