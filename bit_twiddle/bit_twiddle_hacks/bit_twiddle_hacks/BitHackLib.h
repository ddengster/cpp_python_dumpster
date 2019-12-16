
#ifndef _BITHACKLIB_H
#define _BITHACKLIB_H

/*
  Of course, taking time to initialize objects for these is costly.
  If you want the best speed, copy paste the implementations.

  Hacks from:
	http://graphics.stanford.edu/~seander/bithacks.html

*/
class BitHackLib
{
public:
	/* Detects if 2 integers have opposite signs */
	bool Detect2IntOppositeSigns(int x, int y);

	/* Computes the integer absoute value without branching.
	   For machines where branching is expensive
	*/
	unsigned int ComputeIntegerAbs(int i);

	/* Computes minimum of 2 integers without branching */
	int ComputeMin2Ints(int x, int y);

	/* Computes maximum of 2 integers without branching */
	int ComputeMax2Ints(int x, int y);

	/* Determines if an integer is a power of 2 */
	bool IsPowerOfTwo(int i);

	/* Conditionally set or clear bits without branching*/
	unsigned int SetOrClearBit(unsigned int val, unsigned int mask, bool yesno);

	/* Conditionally negate a value without branching 
	   Aka. Can optimize:
	   if (!flag)
		NegateValue(val);
	*/
	int NegateValueIfFlagFalse(int val, bool fDontNegate);
	int NegateValueIfFlagTrue(int val, bool fNegate);

	/* Merge bits from 2 values according to mask 
	   Does: (a & ~mask) | (b & mask)
	   *Dunno how to use this
	*/
	unsigned int MergeBits(unsigned int val1, unsigned int val2, unsigned int mask);

	/*Swap values subtraction and addition*/
#define SwapAddSub(a, b) ((&(a) == &(b)) || \
                    (((a) -= (b)), ((b) += (a)), ((a) = (b) - (a))))

	/*Swap values with XOR*/
#define SwapXOR(a, b) (((a) ^= (b)), ((b) ^= (a)), ((a) ^= (b)))

	/* Compute modulus division by 1 << s without a division operator
	   Aka num % (number that is power of 2)
	*/
	unsigned int Mod(unsigned int num, unsigned int s);

	/* Round up to next highest power of 2 by float casting */
	unsigned int RoundToNextHighestPower2FloatCast(unsigned int num);

	/* Round up to next highest power of 2 */
	unsigned int RoundToNextHighestPower2(unsigned int num);

	/**************************************************************************/

	/* Architecture-specific/Compiler-specific/(Stuff with portability issues)

	/**************************************************************************/
	/* Computes sign of an integer. Return -1 for -ve, 0 for zero, +1 for +ve*/
	int ComputeIntegerSign(int num);
};

/* Sign extension function: 
   Suppose you want to convert a +ve or -ve integer with a smaller number of bits to 
   a integer with a larger number of bits, while retaining -veness and value. Use
   this function
*/
template <typename T, unsigned B>
inline T signextend(const T x)
{
  struct {T x:B;} s;
  return s.x = x;
}



#endif
