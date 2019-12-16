
#include "BitHackLib.h"

#define CHAR_BIT 8

bool BitHackLib::Detect2IntOppositeSigns(int x, int y)
{
	/*bool f = ((x^y) < 0); */
	return ((x^y) < 0);
}

unsigned int BitHackLib::ComputeIntegerAbs(int i)
{
	const int mask = i >> (sizeof(int) * 8 - 1);
	return (i + mask) ^ mask;

	//Patented version
	//return (i ^ mask) - mask;
}

int BitHackLib::ComputeMin2Ints(int x, int y)
{
	return (y ^ ((x ^ y) & -(x < y)));

	//Quick and Dirty version; Use only if INT_MIN <= x-y <= INT_MAX
	//return (y + ((x - y) & ((x - y) >> (sizeof(int) * CHAR_BIT - 1)))); // min(x, y)
}

int BitHackLib::ComputeMax2Ints(int x, int y)
{
	return (x ^ ((x ^ y) & -(x < y)));

	//Quick and Dirty version; Use only if INT_MIN <= x-y <= INT_MAX
	//return (x - ((x - y) & ((x - y) >> (sizeof(int) * CHAR_BIT - 1)))); // max(x, y)
}

bool BitHackLib::IsPowerOfTwo(int i)
{
	//0 is not considered a power of 2
	return i && (!(i & i - 1));
}

unsigned int BitHackLib::SetOrClearBit(unsigned int val, unsigned int mask, bool yesno)
{
	//val ^= (-yesno ^ val) & mask;

	//for superscalar cpus (aka modern cpus)
	val = (val & ~mask) | (-yesno & mask);
	return val;
}

int BitHackLib::NegateValueIfFlagFalse(int val, bool fDontNegate)
{
	return (fDontNegate ^ (fDontNegate - 1)) * val;
}

int BitHackLib::NegateValueIfFlagTrue(int val, bool fNegate)
{
	return (val ^ -fNegate) + fNegate;
}

unsigned int BitHackLib::MergeBits(unsigned int val1, unsigned int val2, unsigned int mask)
{
	return (val1 ^ ((val1 ^ val2) & mask));
}

unsigned int BitHackLib::Mod(unsigned int num, unsigned int s)
{
	//can optimize abit more
	unsigned int d = 1U << s;
	return (num & (d-1));
}

unsigned int BitHackLib::RoundToNextHighestPower2FloatCast(unsigned int num)
{
	if (num > 1) 
	{
	  float f = (float)num;
	  unsigned int const t = 1U << ((*(unsigned int *)&f >> 23) - 0x7f);
	  return (t << (t < num));
	}
	else 
	{
	  return 1;
	}
	
	/* Dirty version for domain of 1 < num < (1<<25) */
	/*
	float f = (float)(num - 1);  
	return (1U << ((*(unsigned int*)(&f) >> 23) - 126));
	*/
}

unsigned int BitHackLib::RoundToNextHighestPower2(unsigned int num)
{
	num--;
	num |= num >> 1;
	num |= num >> 2;
	num |= num >> 4;
	num |= num >> 8;
	num |= num >> 16;
	num++;
	return num;
}

int BitHackLib::ComputeIntegerSign(int num)
{
	return ((num != 0) | -(int)((unsigned int)((int)num) >> (sizeof(int) * CHAR_BIT - 1)));

	// Or, for more speed but less portability:
	//return ((num != 0) | (num >> (sizeof(int) * CHAR_BIT - 1)));  // -1, 0, or +1

	// Or, for portability, brevity, and (perhaps) speed:
	//return ((num > 0) - (num < 0)); // -1, 0, or +1
}
