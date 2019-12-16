
#ifndef _ALLOCATOR_H
#define _ALLOCATOR_H

#if 0
#include <limits>
#include <cstdlib>

template <class T>
class Allocator 
{
public:
  // Type Definitions
  typedef T value_type;
  typedef T* pointer;
  typedef const T* const_pointer;
  typedef T& reference;
  typedef const T& const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  // Rebind Allocator to Type U
  template <class U> struct rebind 
  {
    typedef Mallocator<U> other;
  };

  // Return Address of a Value
  inline pointer address(reference value) const             { return &value; }
  inline const_pointer address(const_reference value) const { return &value; }

  inline Allocator()  { }
  inline ~Allocator() { }

  inline Allocator(const Allocator&) { }
  inline Allocator& operator=(const Mallocator&) { return *this; }
  template <class U> Mallocator(const Mallocator<U>&) { }

  // This returns the maximum number of ELEMENTS that can be allocated
  inline size_type max_size() const { /*return std::numeric_limits<std::size_t>::max() / sizeof(T);*/return (size_t)(UINT_MAX / sizeof(T)); }

  inline pointer allocate(size_type num)
  {
    if (num == 0) 
      return nullptr;

    if (num > max_size()) 
    {
      // Too many elements
      //throw std::length_error("Mallocator Too Many Elements");
      //LOG("Too many elements!");
    }

    void* ptr = malloc(num * sizeof(T));

    if (!ptr) {
      // malloc failed
      //throw std::bad_alloc();
    }

    return static_cast<pointer>(ptr);
  }

  pointer allocate(const size_t n, const void* /* unused */) 
  {
    return allocate(n);
  }

  void deallocate(pointer p, size_type /* num */) 
  {
    free(p);
  }

  void construct(pointer p, const T& value) 
  {
    new(static_cast<void*>(p)) T(value);
  }

  void destroy(pointer p) 
  {
    p->~T();
  }
};
#endif

#include <memory>

template <class T>
struct Allocator 
{
  typedef T value_type;
  Allocator() { }
    
  template <class U> 
  Allocator(const Allocator<U>&) { }
  
  T* allocate(std::size_t n) 
  {
    char* p = new char[n * sizeof(T)];
    return reinterpret_cast<T*>(p);
  }

  void deallocate(T* p, std::size_t ) 
  {
    ::delete(p); 
  }
};

#define ALLOCATOR_BLOCKSIZE 16
#define NUM_BLOCKS 4096

static int mCounter = 0;
static void* mStringPool = new char[ALLOCATOR_BLOCKSIZE * NUM_BLOCKS];

template <class T>
struct PoolAllocator
{
  // Type Definitions
  typedef T value_type;
  typedef T* pointer;
  typedef const T* const_pointer;
  typedef T& reference;
  typedef const T& const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  PoolAllocator() { }

  template <class U>
  PoolAllocator(const PoolAllocator<U>&) { }

  T* allocate(std::size_t n)
  {
    size_t totalsize = n * sizeof(T);

    size_t remainder = totalsize % ALLOCATOR_BLOCKSIZE;
    size_t extra = (remainder == 0) ? 0 : 1;
    size_t blocks = (totalsize / ALLOCATOR_BLOCKSIZE) + extra;

    char* p = (char*)mStringPool;
    return reinterpret_cast<T*>(p);
  }

  void deallocate(T* p, std::size_t)
  {
    
  }

  void construct(pointer p, const T& value)
  {
    new(static_cast<void*>(p)) T(value); //placement new, no allocations here
  }

  void destroy(pointer p)
  {
    p->~T();
  }

  // Rebind Allocator to Type U
  template <class U> struct rebind { typedef PoolAllocator<U> other; };
};

#endif
