#pragma once

#include <assert.h>

namespace tn {

//lightweight DoubleEnded array intended as a replacement to deque. Most of the stl functions are stripped
template<typename T>
class devector {
private:
  //typedef devector<T> V;

public:
  // Typedefs.
  typedef T                                      value_type;
  //typedef Allocator                              allocator_type;
  typedef unsigned int                           size_type;
  typedef T&                                     reference;
  typedef const T&                               const_reference;
  typedef T*                                     iterator;
  typedef const T*                               const_iterator;
  //typedef std::reverse_iterator<iterator>        reverse_iterator;
  //typedef std::reverse_iterator<const_iterator>  const_reverse_iterator;

  devector() { }
  ~devector() noexcept 
  {
    delete[] storage;
  }

  devector(const devector<T>& other) 
  { 
    storage_capacity = other.storage_capacity;
    begin_cursor = other.begin_cursor;
    end_cursor = other.end_cursor;

    if (storage_capacity)
    {
      storage = new T[storage_capacity];
      if (std::is_pointer<T>::value || std::is_fundamental<T>::value ||
        (std::is_trivially_copyable<T>::value))
        memcpy(storage, other.storage, sizeof(T) * storage_capacity);
      else
      {
        for (unsigned int i = 0; i < storage_capacity; ++i)
          storage[i] = other.storage[i];
      }
    }
  }

  devector(devector<T>&& other) {
    if (other.storage)
      storage = std::move(other.storage);
    storage_capacity = other.storage_capacity;
    begin_cursor = other.begin_cursor;
    end_cursor = other.end_cursor;
  }

  devector<T>& operator=(const devector<T>& other) {
    storage_capacity = other.storage_capacity;
    begin_cursor = other.begin_cursor;
    end_cursor = other.end_cursor;

    delete[] storage;
    storage = nullptr;
    if (storage_capacity)
    { 
      storage = new T[storage_capacity];
      if (std::is_pointer<T>::value || std::is_fundamental<T>::value ||
        (std::is_trivially_copyable<T>::value))
        memcpy(storage, other.storage, sizeof(T) * storage_capacity);
      else
      {
        for (unsigned int i = 0; i < storage_capacity; ++i)
          storage[i] = other.storage[i];
      }
    }
    return *this;
  }

  devector<T>& operator=(devector<T>&& other) noexcept(alloc_traits::propagate_on_container_move_assignment::value) {
    if (this != &other) {
      if (other.storage)
        storage = std::move(other.storage);
      storage_capacity = other.storage_capacity;
      begin_cursor = other.begin_cursor;
      end_cursor = other.end_cursor;
    }

    return *this;
  }

  template<class InputIterator>
  devector(InputIterator first, InputIterator last) {
    size_type sz = (size_type)(last - first);

    storage_capacity = sz;
    begin_cursor = 0;
    end_cursor = sz;

    if (storage_capacity)
    {
      storage = new T[storage_capacity];
      int c = 0;
      for (InputIterator it = first; it != last; ++it)
        storage[c++] = *it;
    }
  }

  // Iterators.
  iterator               begin()         noexcept { return &storage[begin_cursor]; }
  const_iterator         begin()   const noexcept { return &storage[begin_cursor]; }
  iterator               end()           noexcept { return &storage[end_cursor]; }
  const_iterator         end()     const noexcept { return &storage[end_cursor]; }

  //reverse_iterator       rbegin()        noexcept { return reverse_iterator(end()); }
  //const_reverse_iterator rbegin()  const noexcept { return const_reverse_iterator(end()); }
  //reverse_iterator       rend()          noexcept { return reverse_iterator(begin()); }
  //const_reverse_iterator rend()    const noexcept { return const_reverse_iterator(begin()); }

  const_iterator         cbegin()  const noexcept { return begin(); }
  const_iterator         cend()    const noexcept { return end(); }
  //const_reverse_iterator crbegin() const noexcept { return rbegin(); }
  //const_reverse_iterator crend()   const noexcept { return rend(); }

  // Capacity.
  size_type max_size()       const noexcept { return UINT_MAX; }
  size_type size()           const noexcept { return end_cursor - begin_cursor; }
  size_type capacity()       const noexcept { return storage_capacity; }
  void reserve(size_type n) { reserve_back(n); }

  void reserve(size_type new_front, size_type new_back) {
    size_type capac_front = end_cursor;
    size_type capac_back = storage_capacity >= begin_cursor ? storage_capacity - begin_cursor : 0;
    if (capac_front >= new_front && capac_back >= new_back)
      return;

    reallocate(new_front - size(), new_back - size());
  }

  void reserve_front(size_type n) {
    size_type capac_front = end_cursor;
    if (capac_front >= n)
      return;

    reallocate(n - size(), storage_capacity >= end_cursor ? storage_capacity - end_cursor : 0);
  }

  void reserve_back(size_type n) {
    size_type capac_back = storage_capacity >= begin_cursor ? storage_capacity - begin_cursor : 0;
    if (capac_back >= n)
      return;

    reallocate(begin_cursor, n - size());
  }

  bool empty() const noexcept { return begin_cursor == end_cursor; }

  // Indexing.
  reference       operator[](size_type i)       noexcept { assert(begin_cursor != end_cursor && "operator[] error: Size is 0."); return storage[begin_cursor + i]; }
  const_reference operator[](size_type i) const noexcept { assert(begin_cursor != end_cursor && "operator[] error: Size is 0."); return storage[begin_cursor + i]; }

  reference         front()       noexcept { assert(begin_cursor != end_cursor && "front() error: Size is 0."); return storage[begin_cursor]; }
  const_reference   front() const noexcept { assert(begin_cursor != end_cursor && "front() error: Size is 0."); return storage[begin_cursor]; }
  reference         back()        noexcept { assert(begin_cursor != end_cursor && "back() error: Size is 0."); return storage[end_cursor - 1]; }
  const_reference   back()  const noexcept { assert(begin_cursor != end_cursor && "back() error: Size is 0."); return storage[end_cursor - 1]; }
  T*                data()        noexcept { assert(begin_cursor != end_cursor && "data() error: Size is 0."); return storage[begin_cursor]; }
  const T*          data()  const noexcept { assert(begin_cursor != end_cursor && "data() error: Size is 0."); return storage[begin_cursor]; }

  // Modifiers.
  void push_front(const T& x) { emplace_front(x); }
  void push_front(T&& x)      { emplace_front(std::move(x)); }
  void push_back(const T& x)  { emplace_back(x); }
  void push_back(T&& x)       { emplace_back(std::move(x)); }

  void pop_front() noexcept 
  { 
    if (begin_cursor < end_cursor)
    {
      //begin_cursor->~T(); //let delete[] handle the recursive destructor calls
      ++begin_cursor;
    }
    else
      assert(0 && "Cannot pop empty devector!");
  }
  void pop_back()  noexcept 
  {
    if (end_cursor > begin_cursor)
    {
      //end_cursor->~T(); //let delete[] handle the recursive destructor calls
      --end_cursor;
    }
    else
      assert(0 && "Cannot pop empty devector!");
  }

  template<class... Args>
  void emplace_front(Args&&... args) {
    assure_space_front(1);
    
    --begin_cursor;
    new (&storage[begin_cursor]) T(std::forward<Args>(args) ...);
  }

  template<class... Args>
  void emplace_back(Args&&... args) {
    assure_space_back(1);

    new (&storage[end_cursor]) T(std::forward<Args>(args) ...);
    ++end_cursor;
  }

  void swap(devector<T>& other)
  {
    using std::swap;

    swap(storage, other.storage);
    swap(storage_capacity, other.storage_capacity);
    swap(begin_cursor, other.begin_cursor);
    swap(end_cursor, other.end_cursor);
  }

  void clear() {
    /*for (int i = begin_cursor; i < end_cursor; ++i)
    {
      storage[i]->~T();
    }*/
    end_cursor = begin_cursor;
  }

  //inline void shrink_to_fit();
private:
  T* storage = nullptr; // storage[0]
  size_type storage_capacity = 0;
  size_type begin_cursor = 0;
  size_type end_cursor = 0; //one past the end unless 0 size
  
  // Reallocate with exactly space_front free space in the front, and space_back in the back.
  void reallocate(size_type space_front, size_type space_back) 
  {
    //assumed parameters are valid
    size_type cur_size = size();
    size_type new_capacity = space_front + cur_size + space_back;

    T* new_storage = new T[new_capacity];
    if (storage)
    {
      if (std::is_pointer<T>::value || std::is_fundamental<T>::value || 
        (std::is_trivially_copyable<T>::value))
        memcpy(new_storage + space_front, storage + begin_cursor, cur_size * sizeof(T));
      else
      {
        for (unsigned int i = 0; i < cur_size; ++i)
          new_storage[space_front + i] = storage[begin_cursor + i];
      }
      delete[] storage;
    }

    storage = new_storage;
    storage_capacity = new_capacity;
    begin_cursor  = space_front;
    end_cursor = storage_capacity - space_back;
  }

  // Make sure there is space for at least n elements at the front of the devector. This may steal
    // space from the back.
  void assure_space_front(size_type n) 
  {
    if (begin_cursor >= n)
      return;

    size_type sz = size();

    size_type space_back = storage_capacity >= end_cursor ? (storage_capacity - end_cursor) / 2 : 0;
    size_type sz_req = sz + n;
    size_type space_front_req = sz_req >= 16 ? sz_req / 3 : sz_req; 
    size_type mem_req = sz_req + space_front_req + space_back; 

    if (mem_req > storage_capacity) {
      // Use exponential growth with factor 1.5 (2 for small sizes) if possible.
      size_type alloc_size = storage_capacity * (3 + (storage_capacity < 16)) / 2;
      if (mem_req > alloc_size) 
        reallocate(space_front_req, space_back);
      else
        reallocate(alloc_size - sz - space_back, space_back);
    }
    else {
      // We have enough space already, we just have to move elements around.
      size_type new_end_cursor = storage_capacity - space_back;//32-14=18
      size_type shift = new_end_cursor - end_cursor;//18-4=14
      
      // We now have to move the elements into their new location. Some of the new
      // locations are in uninitialized memory. This has to be handled seperately. 
      if (sz)
        memmove(storage + (begin_cursor + shift), storage + begin_cursor, sizeof(T) * sz);

      begin_cursor = begin_cursor + shift;
      end_cursor = new_end_cursor;
     }
  }

  void assure_space_back(size_type n) 
  {
    size_type space_back = storage_capacity >= end_cursor ? storage_capacity - end_cursor : 0;
    if (space_back >= n)
      return;

    // Don't compute this multiple times.
    size_type sz = size();

    size_type sz_req = sz + n;
    size_type space_front = (begin_cursor) / 2;
    size_type space_back_req = sz_req >= 16 ? sz_req / 3 : sz_req;
    size_type mem_req = sz_req + space_front + space_back_req;

    if (mem_req > storage_capacity) {
      // Use exponential growth with factor 1.5 (2 for small sizes) if possible.
      size_type alloc_size = storage_capacity * (3 + (storage_capacity < 16)) / 2;
      if (mem_req > alloc_size)
        reallocate(space_front, space_back_req);
      else
        reallocate(space_front, alloc_size - sz - space_front);
    }
    else {
      // We have enough space already, we just have to move elements around.
      size_type new_begin_cursor = space_front;
      //size_type shift = begin_cursor - new_begin_cursor;//18-4=14

      if (sz)
        memmove(storage + new_begin_cursor, storage + begin_cursor, sizeof(T) * sz);
      
      begin_cursor = new_begin_cursor;
      end_cursor = new_begin_cursor + sz;
     }
  }
};


// Comparison operators.
template<class T, class Allocator>
inline bool operator==(const devector<T>& lhs, const devector<T>& rhs) {
  return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), rhs.begin());
}

template<class T, class Allocator>
inline bool operator< (const devector<T>& lhs, const devector<T>& rhs) {
  return std::lexicographical_compare(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Allocator>
inline bool operator!=(const devector<T>& lhs, const devector<T>& rhs) {
  return !(lhs == rhs);
}

template<class T, class Allocator>
inline bool operator> (const devector<T>& lhs, const devector<T>& rhs) {
  return rhs < lhs;
}

template<class T, class Allocator>
inline bool operator<=(const devector<T>& lhs, const devector<T>& rhs) {
  return !(rhs < lhs);
}

template<class T, class Allocator>
inline bool operator>=(const devector<T>& lhs, const devector<T>& rhs) {
  return !(lhs < rhs);
}

template<class T, class Allocator>
inline void swap(devector<T>& lhs, devector<T>& rhs)
noexcept(noexcept(lhs.swap(rhs))) {
  lhs.swap(rhs);
}


}