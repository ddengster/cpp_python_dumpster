#ifndef _ODDMENT_H
#define _ODDMENT_H

#include <vector>
#include <assert.h>
#include <stdlib.h>

template <class eventtype>
class OddmentTable
{
  private:
   
  public: 
    OddmentTable(int seed)
    {
      srand(seed);
      m_maxoddment = 0;
    }
    ~OddmentTable() { }
    
    eventtype PickEntry()
    {
      int randnum = rand() % m_maxoddment;
      
      /*From 1 to n, not 0 to n - 1*/
      randnum++;

      int oddment_total = 0;
      unsigned int i = 0;
      for (i=0; i<m_entrylist.size(); ++i)
      {
        oddment_total += m_entrylist[i].m_oddment;

        if (oddment_total >= randnum)
          return m_entrylist[i].m_event;
      }
      return m_entrylist[i-1].m_event;
    }
    
    void AddEntry(eventtype event, int oddment)
    {
      assert(oddment > 0);
      Entry ent(event, oddment);
      m_entrylist.push_back(ent);
      m_maxoddment += oddment;
    }
    
  private:

    struct Entry
    {
    public:
      int m_oddment;
      eventtype m_event;
      Entry(eventtype event, int oddment)
      {
        m_oddment = oddment;
        m_event = event;
      }
    };
    
    std::vector<Entry> m_entrylist;
    int m_maxoddment;
};

#endif

