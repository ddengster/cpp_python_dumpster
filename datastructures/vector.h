#pragma once

#include <assert.h>

namespace tn {

template<typename T>
void vector_cpy(T* __restrict dest, T* __restrict src, unsigned int ele_count)
{
  if (std::is_pointer<T>::value || std::is_fundamental<T>::value ||
    (std::is_trivially_copyable<T>::value))
    memcpy(dest, src, ele_count * sizeof(T));
  else
  {
    for (unsigned int i = 0; i < ele_count; ++i)
      dest[i] = src[i];
  }
}

template<typename T>
void vector_move(T* dest, T* src, unsigned int ele_count)
{
  if (std::is_pointer<T>::value || std::is_fundamental<T>::value ||
    (std::is_trivially_copyable<T>::value))
    memmove(dest, src, ele_count * sizeof(T));
  else
  {
    if (dest > src)
    {
      for (int i = (int)ele_count - 1; i >= 0; --i)
        dest[i] = src[i];
    }
    else
    {
      for (unsigned int i = 0; i < ele_count; ++i)
        dest[i] = src[i];
    }
  }
}

template <typename T>
class vector
{
public:
  // types:
  typedef T                                    value_type;
  typedef T&                                   reference;
  typedef const T&                             const_reference;
  typedef T*                                   pointer;
  typedef const T*                             const_pointer;
  typedef T*                                   iterator;
  typedef const T*                             const_iterator;
  typedef ptrdiff_t                            difference_type;
  typedef unsigned int                         size_type;

  inline vector() { }
  inline vector(size_type n, const T& value) 
  {
    storage_capacity = n << 2;
    arr = new T[storage_capacity];
    for (size_type i = 0; i < n; ++i)
      arr[i] = value;
    vec_size = n;
  }
  inline vector(typename vector<T>::iterator first, typename vector<T>::iterator last)
  {
    size_type count = (size_type)(last - first);
    if (count > storage_capacity) {
      reserve(count << 2);
    }
    for (size_type i = 0; i < count; ++i, ++first)
      arr[i] = *first;
    vec_size = count;
  }
  inline vector(std::initializer_list<T> lst)
  {
    storage_capacity = lst.size() << 2;
    arr = new T[storage_capacity];
    for (auto &item : lst)
      arr[vec_size++] = item;
  }

  inline vector(const vector<T>& other)
  {
    storage_capacity = other.storage_capacity;
    vec_size = other.vec_size;
    arr = new T[storage_capacity];
    vector_cpy(arr, other.arr, vec_size);
  }
  inline vector(vector<T>&& other)
  {
    /**/
    *this = std::move(other);
  }
  inline ~vector() { delete[] arr; }

  inline vector<T>& operator=(const vector<T>& other)
  {
    reserve(other.storage_capacity);
    vec_size = other.vec_size;

    vector_cpy(arr, other.arr, vec_size);
    return *this;
  }

  inline vector<T>& operator=(vector<T>&& other)
  {
    storage_capacity = other.storage_capacity;
    vec_size = other.vec_size;
    arr = other.arr;

    other.storage_capacity = 0;
    other.vec_size = 0;
    other.arr = nullptr;
    return *this;
  }

  inline vector<T>& operator=(std::initializer_list<T> lst)
  {
    reserve(lst.size() << 2);
    vec_size = 0;
    for (auto &item : lst)
      arr[vec_size++] = item;
  }

  // iterators:
  inline iterator begin() const { return arr; }
  inline iterator end()   const { return arr + vec_size; }
  inline const_iterator cbegin() const { return arr; }
  inline const_iterator cend() const   { return arr + vec_size; }

  // 23.3.11.3, capacity:
  inline bool empty() const                  { return vec_size == 0; }
  inline size_type size() const noexcept     { return vec_size; }
  inline size_type capacity() const noexcept { return storage_capacity; }
  inline void resize(size_type sz)
  {
    reserve(sz);
    vec_size = sz;
  }
  inline void reserve(size_type sz)
  {
    if (sz <= storage_capacity)
      return;
    T* new_arr = new T[sz];
    vector_cpy(new_arr, arr, vec_size);

    delete[] arr;
    storage_capacity = sz;
    arr = new_arr;
  }

  inline void shrink_to_fit() {
    T* new_arr = new T[vec_size];
    vector_cpy(new_arr, arr, vec_size);

    delete[] arr;
    storage_capacity = vec_size;
    arr = new_arr;
  }

  // element access
  inline reference operator [](size_type idx)             { assert(idx < vec_size && "operator[]() fail: Index out of range!");  return arr[idx]; }
  inline const_reference operator [](size_type idx) const { assert(idx < vec_size && "operator[]() fail: Index out of range!");  return arr[idx]; }
  inline reference front()                            { assert(vec_size > 0 && "front() fail: Size is 0!"); return arr[0]; }
  inline const_reference front() const                { assert(vec_size > 0 && "front() fail: Size is 0!"); return arr[0]; }
  inline reference back()                             { assert(vec_size > 0 && "back() fail:Size is 0!"); return arr[vec_size - 1]; }
  inline const_reference back() const                 { assert(vec_size > 0 && "back() fail:Size is 0!"); return arr[vec_size - 1]; }

  // 23.3.11.4, data access:
  T* data()  { return arr; }
  const T* data() const { return arr; }

  // 23.3.11.5, modifiers:
  template <class ... Args> void emplace_back(Args && ... args)
  {
    if (vec_size == storage_capacity)
      reserve(storage_capacity == 0 ? 8 : storage_capacity << 2);

    //arr[vec_size] = std::move(T(std::forward<Args>(args) ...));
    new (&arr[vec_size]) T(std::forward<Args>(args) ...);
    ++vec_size;
  }

  inline void push_back(const T& val)
  {
    if (vec_size == storage_capacity)
      reserve(storage_capacity == 0 ? 8 : storage_capacity << 2);
    arr[vec_size] = val;
    ++vec_size;
  }
  inline void push_back(T&& val)
  {
    if (vec_size == storage_capacity)
      reserve(storage_capacity == 0 ? 8 : storage_capacity << 2);
    arr[vec_size] = std::move(val);
    ++vec_size;
  }
  inline void pop_back()
  {
    assert(vec_size > 0 && "pop_back() fail: Size is 0!");
    if (vec_size)
      --vec_size;
  }

  template <class ... Args> iterator emplace(const_iterator pos, Args && ... args)
  { 
    size_type idx = pos ? (size_type)(pos - arr) : 0;
    if (vec_size == storage_capacity) {
      reserve(storage_capacity == 0 ? 8 : storage_capacity << 2);
    }

    size_type ele_count = (size_type)(vec_size - idx);
    vector_move(&arr[idx + 1], &arr[idx], ele_count);
    iterator it = &arr[idx];
    new (&arr[idx]) T(std::forward<Args>(args) ...);
    ++vec_size;
    return it;
  }

  iterator insert(const_iterator pos, const T& val) {
    size_type idx = pos ? (size_type)(pos - arr) : 0;
    if (vec_size == storage_capacity) {
      reserve(storage_capacity == 0 ? 8 : storage_capacity << 2);
    }

    size_type ele_count = (size_type)(vec_size - idx);
    vector_move(&arr[idx + 1], &arr[idx], ele_count);
    arr[idx] = val;
    ++vec_size;
    return &arr[idx];
  }
  iterator insert(const_iterator pos, T&& val)
  {
    size_type idx = pos ? (size_type)(pos - arr) : 0;
    if (vec_size == storage_capacity) {
      reserve(storage_capacity == 0 ? 8 : storage_capacity << 2);
    }

    size_type ele_count = (size_type)(vec_size - idx);
    vector_move(&arr[idx + 1], &arr[idx], ele_count);
    arr[idx] = std::move(val);
    ++vec_size;
    return &arr[idx];
  }

  template <class InputIt> const_iterator insert(const_iterator pos, InputIt first, InputIt last)
  {
    size_type cnt = (size_type)(last - first);
    if (!cnt) 
      return pos;
    
    size_type idx = pos ? (size_type)(pos - arr) : 0;
    if (vec_size + cnt > storage_capacity) {
      reserve((vec_size + cnt) << 2);
    }

    size_type ele_to_move = vec_size - idx;
    vector_move(&arr[idx + cnt], &arr[idx], ele_to_move);
    for (unsigned i = idx; first != last; ++i, ++first)
      arr[i] = *first;
    vec_size += cnt;
    return &arr[idx];
  }

  iterator insert(const_iterator pos, std::initializer_list<T> lst) {
    size_type cnt = (size_type)lst.size();
    if (!cnt)
      return end();
    size_type idx = pos ? (size_type)(pos - arr) : 0;
    if ((vec_size + cnt) > storage_capacity) {
      reserve((vec_size + cnt) << 2);
    }

    size_type ele_to_move = vec_size - idx;
    vector_move(&arr[idx + cnt], &arr[idx], ele_to_move);

    unsigned i = idx;
    for (auto &item : lst) {
      arr[i++] = item;
    }
    vec_size += cnt;
    return &arr[idx];
  }

  iterator erase(const_iterator it)
  {
    if (!arr || !it)
      return end();
    
    size_type offset = (size_type)(it - arr);
    size_type ele_count = vec_size - offset - 1;
    vector_move(&arr[offset], &arr[offset + 1], ele_count);
    --vec_size;
    return &arr[offset];
  }
  iterator erase(iterator first, iterator last)
  {
    if (!arr || !first || !last)
      return nullptr;
    if (first == last)
      return first;

    size_type offset = (size_type)(first - arr);
    size_type last_offset = (size_type)(last - arr);
    size_type ele_count = (size_type)(vec_size - last_offset);
    vector_move(&arr[offset], last, ele_count);
    vec_size -= (unsigned int)(last - first);
    return &arr[offset];
  }
  void swap(vector<T>& rhs) 
  {
    unsigned int tvec_size = vec_size, tstorage_capacity = storage_capacity;
    T *tarr = arr;

    vec_size = rhs.vec_size;
    storage_capacity = rhs.storage_capacity;
    arr = rhs.arr;

    rhs.vec_size = tvec_size;
    rhs.storage_capacity = tstorage_capacity;
    rhs.arr = tarr;
  }
  void clear() 
  {
    vec_size = 0;
  }
  void clear_reconstruct()
  {
    for (unsigned i = 0; i < vec_size; ++i)
      arr[i] = T();
    vec_size = 0;
  }

  bool operator==(const vector<T>& rhs) const 
  {
    if (vec_size != rhs.vec_size) return false;
    size_type i;
    for (i = 0; i < vec_size; ++i)
      if (arr[i] != rhs.arr[i])
        return false;
    return true;
  }
  bool operator!=(const vector<T>& rhs) const
  {
    if (vec_size != rhs.vec_size) return true;
    size_type i;
    for (i = 0; i < vec_size; ++i)
      if (arr[i] != rhs.arr[i])
        return true;
    return false;
  }
  bool operator<(const vector<T>& rhs) const 
  {
    size_type i, j, ub = vec_size < rhs.vec_size ? vec_size : rhs.vec_size;
    for (i = 0; i < ub; ++i)
      if (arr[i] != rhs.arr[i])
        return arr[i] < rhs.arr[i];
    return vec_size < rhs.vec_size;
  }
  bool operator<=(const vector<T>& rhs) const {
    size_type i, j, ub = vec_size < rhs.vec_size ? vec_size : rhs.vec_size;
    for (i = 0; i < ub; ++i)
      if (arr[i] != rhs.arr[i])
        return arr[i] < rhs.arr[i];
    return vec_size <= rhs.vec_size;
  }
  bool operator>(const vector<T>& rhs) const {
    size_type i, j, ub = vec_size < rhs.vec_size ? vec_size : rhs.vec_size;
    for (i = 0; i < ub; ++i)
      if (arr[i] != rhs.arr[i])
        return arr[i] > rhs.arr[i];
    return vec_size > rhs.vec_size;
  }
  bool operator>=(const vector<T>& rhs) const {
    size_type i, j, ub = vec_size < rhs.vec_size ? vec_size : rhs.vec_size;
    for (i = 0; i < ub; ++i)
      if (arr[i] != rhs.arr[i])
        return arr[i] > rhs.arr[i];
    return vec_size >= rhs.vec_size;
  }


public:
  size_type storage_capacity = 0;
  size_type vec_size = 0;
  T *arr = nullptr;
};


}