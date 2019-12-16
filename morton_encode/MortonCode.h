#pragma once


#if _MSC_VER
/*
BMI2 (Bit Manipulation Instruction Set 2) is a special set of instructions available for intel core i5, i7 (since Haswell architecture) and Xeon E3 processors.
Some instructions are not available for Microsoft Visual Studio older than 2013.
*/

//#include <immintrin.h>
//#define USE_BMI2
#endif

#if __GNUC__
//  #include <x86intrin.h>
#endif

typedef unsigned int uint;
typedef unsigned long long uint64;

//@reference: Real Time Collision Detection Chapter 7
inline uint Part1By1(uint n)
{
  // Separates low 16 bits of input by one bit

  // n = ----------------fedcba9876543210 : Bits initially
  // n = --------fedcba98--------76543210 : After (1)
  // n = ----fedc----ba98----7654----3210 : After (2)
  // n = --fe--dc--ba--98--76--54--32--10 : After (3)
  // n = -f-e-d-c-b-a-9-8-7-6-5-4-3-2-1-0 : After (4)
  n = (n ^ (n << 8)) & 0x00ff00ff; // (1)
  n = (n ^ (n << 4)) & 0x0f0f0f0f; // (2)
  n = (n ^ (n << 2)) & 0x33333333; // (3)
  n = (n ^ (n << 1)) & 0x55555555; // (4)
  return n;
}

inline uint Part1By2(uint n)
{
  // Separates low 16 bits of input by two bit

  // n = ----------------------9876543210 : Bits initially
  // n = ------98----------------76543210 : After (1)
  // n = ------98--------7654--------3210 : After (2)
  // n = ------98----76----54----32----10 : After (3)
  // n = ----9--8--7--6--5--4--3--2--1--0 : After (4)
  n = (n ^ (n << 16)) & 0xff0000ff; // (1)
  n = (n ^ (n << 8)) & 0x0300f00f; // (2)
  n = (n ^ (n << 4)) & 0x030c30c3; // (3)
  n = (n ^ (n << 2)) & 0x09249249; // (4)
  return n;
}

inline uint morton_encode_2d(uint x, uint y)
{
  // Takes two 16-bit numbers and bit-interleaves them into one number
#ifdef USE_BMI2
  const uint64 x2_mask = 0xAAAAAAAAAAAAAAAA; //0b...10101010
  const uint64 y2_mask = 0x5555555555555555; //0b...01010101
  //https://www.felixcloutier.com/x86/pdep
  //_pdep_u64();
  uint xbits = _pdep_u32(x, x2_mask);
  uint ybits = _pdep_u32(y, y2_mask);
  return xbits | ybits;
#else
  return (Part1By1(y) << 1) + Part1By1(x);
#endif
}

inline uint morton_encode_3d(uint x, uint y, uint z)
{
  // Takes two 16-bit numbers and bit-interleaves them into one number
#ifdef USE_BMI2
  const uint64 x_mask = 0x4924924924924924; //0b...10101010
  const uint64 y_mask = 0x2492492492492492; //0b...01010101
  const uint64 z_mask = 0x9249249249249249;
  uint xbits = _pdep_u32(x, x_mask);
  uint ybits = _pdep_u32(y, y_mask);
  uint zbits = _pdep_u32(z, z_mask);
  return xbits | ybits | zbits;
#else
  return (Part1By2(z) << 2) + (Part1By2(y) << 1) + Part1By2(x);
#endif
}

//@reference: https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
// Inverse of Part1By1 - "delete" all odd-indexed bits
inline uint Compact1By1(uint x)
{
  x &= 0x55555555;                  // x = -f-e -d-c -b-a -9-8 -7-6 -5-4 -3-2 -1-0
  x = (x ^ (x >> 1)) & 0x33333333; // x = --fe --dc --ba --98 --76 --54 --32 --10
  x = (x ^ (x >> 2)) & 0x0f0f0f0f; // x = ---- fedc ---- ba98 ---- 7654 ---- 3210
  x = (x ^ (x >> 4)) & 0x00ff00ff; // x = ---- ---- fedc ba98 ---- ---- 7654 3210
  x = (x ^ (x >> 8)) & 0x0000ffff; // x = ---- ---- ---- ---- fedc ba98 7654 3210
  return x;
}

// Inverse of Part1By2 - "delete" all bits not at positions divisible by 3
uint Compact1By2(uint x)
{
  x &= 0x09249249;                  // x = ---- 9--8 --7- -6-- 5--4 --3- -2-- 1--0
  x = (x ^ (x >> 2)) & 0x030c30c3; // x = ---- --98 ---- 76-- --54 ---- 32-- --10
  x = (x ^ (x >> 4)) & 0x0300f00f; // x = ---- --98 ---- ---- 7654 ---- ---- 3210
  x = (x ^ (x >> 8)) & 0xff0000ff; // x = ---- --98 ---- ---- ---- ---- 7654 3210
  x = (x ^ (x >> 16)) & 0x000003ff; // x = ---- ---- ---- ---- ---- --98 7654 3210
  return x;
}

inline void morton_decode_2d(uint encoded, uint& x, uint& y)
{
#ifdef USE_BMI2
  const uint64 x2_mask = 0xAAAAAAAAAAAAAAAA; //0b...10101010
  const uint64 y2_mask = 0x5555555555555555; //0b...01010101
  x = _pext_u32(x, x2_mask);
  y = _pext_u32(y, y2_mask);
#else
  x = Compact1By1(encoded);
  y = Compact1By1(encoded >> 1);
#endif
}

inline void morton_decode_3d(uint encoded, uint& x, uint& y, uint& z)
{
#ifdef USE_BMI2
  const uint64 x_mask = 0x4924924924924924; //0b...10101010
  const uint64 y_mask = 0x2492492492492492; //0b...01010101
  const uint64 z_mask = 0x9249249249249249;
  //https://www.felixcloutier.com/x86/pdep
  //_pdep_u64();
  x = _pext_u32(x, x_mask);
  y = _pext_u32(y, y_mask);
  z = _pext_u32(z, z_mask);
#else
  x = Compact1By2(encoded);
  y = Compact1By2(encoded >> 1);
  z = Compact1By2(encoded >> 2);
#endif
}
