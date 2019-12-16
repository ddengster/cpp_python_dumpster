#pragma once


#include "vector.h"
#include <algorithm>

namespace tn {
//@note: Should be the same as the STL's impl, with a function to grab the container.
//Mostly similar implementation to EASTL
//https://github.com/electronicarts/EASTL/blob/master/include/EASTL/priority_queue.h

template <typename T, typename Container = tn::vector<T>, typename Comparator = std::less<typename Container::value_type>>
class priority_queue
{
public:
  //typedef _Container container_type;
  //typedef _Pr value_compare;
  typedef typename Container::value_type value_type;
  typedef typename Container::size_type size_type;
  typedef typename Container::reference reference;
  typedef typename Container::const_reference const_reference;

public:
  Container c;
  Comparator comp;

public:
  inline priority_queue()
    :c(), comp()
  { }

  inline priority_queue(const Comparator& compare)
    :c(), comp(compare)
  {}

  inline priority_queue(const Comparator& compare, const Container& container)
    :c(container), comp(compare)
  {
    std::make_heap(c.begin(), c.end(), comp);
  }

  inline priority_queue(const Comparator& compare, Container&& container)
    :c(container), comp(compare)
  {
    std::make_heap(c.begin(), c.end(), comp);
  }

  inline priority_queue(std::initializer_list<value_type> ilist, const Comparator& compare)
    : c(), comp(compare)
  {
    c.insert(c.end(), ilist.begin(), ilist.end());
    std::make_heap(c.begin(), c.end(), comp);
  }

  template <typename InputIterator>
  inline priority_queue(InputIterator first, InputIterator last)
    : c(first, last), comp()
  {
    std::make_heap(c.begin(), c.end(), comp);
  }

  template <typename InputIterator>
  inline priority_queue(InputIterator first, InputIterator last, const Comparator& compare)
    : c(first, last), comp(compare)
  {
    std::make_heap(c.begin(), c.end(), comp);
  }


  inline bool empty() const { return c.empty(); }
  inline size_type size() const noexcept { return c.size(); }
  inline const_reference top() const noexcept { return c.front(); }
  tn::vector<T>& get_container() { return c; }
  const tn::vector<T>& get_container() const { return c; }

  inline void push(const value_type& value)
  {
    c.push_back(value);
    std::push_heap(c.begin(), c.end(), comp);
  }
  inline void push(value_type&& value)
  {
    c.push_back(std::move(value));
    std::push_heap(c.begin(), c.end(), comp);
  }
  template <class... Args>
  inline void emplace(Args&&... args)
  {
    push(value_type(std::forward<Args>(args)...)); // The C++11 Standard 23.6.4/1 states that c.emplace is used, but also declares that c doesn't need to have an emplace function.
  }

  inline void pop()
  {
    std::pop_heap(c.begin(), c.end(), comp);
    c.pop_back();
  }
  inline void pop(value_type& value)
  {
    value = std::move(c.front());  // To consider: value = move_if_noexcept_assignable(c.front());
    pop();
  }

  inline void swap(Container& x)
  {
    std::swap(c, x.c);
    std::swap(comp, x.comp);
  }
  
  inline bool validate() const
  {
    return /*c.validate() && */std::is_heap(c.begin(), c.end(), comp);
  }

  void resort() { std::make_heap(c.begin(), c.end(), comp); }
  void resort(const Comparator& compare)
  {
    comp = compare;
    std::make_heap(c.begin(), c.end(), comp);
  }
  void clear() { c.clear(); }
  void reserve(unsigned int size) { c.reserve(size); }
};

template <typename T, typename Container, typename Compare>
bool operator==(const priority_queue<T, Container, Compare>& a, const priority_queue<T, Container, Compare>& b)
{
  return (a.c == b.c);
}

}