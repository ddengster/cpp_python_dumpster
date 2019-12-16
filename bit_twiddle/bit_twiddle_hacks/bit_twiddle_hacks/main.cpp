
#include "BitHackLib.h"
#include <iostream>

int main()
{
	using namespace std;
	BitHackLib lib;

	//Detect2IntOppositeSigns
	/*
	cout << lib.Detect2IntOppositeSigns(-1, -3) << endl;
	cout << lib.Detect2IntOppositeSigns(-1, 3) << endl;
	cout << lib.Detect2IntOppositeSigns(1, 3) << endl;
	*/

	//ComputeIntegerAbs
	/*
	cout << lib.ComputeIntegerAbs(-3) << endl;
	int a = (2 << 31) - 1;
	cout << a << endl;
	cout << lib.ComputeIntegerAbs(a) << endl;

	a = (2 << 31) + (2 << 30); // largest possible -ve number
	cout << a << endl;
	cout << lib.ComputeIntegerAbs(a) << endl; //becomes largest +ve number
	*/

	//Compute Min
	/*
	cout << lib.ComputeMin2Ints(1, 3) << endl;
	cout << lib.ComputeMin2Ints(-1, -3) << endl;

	cout << lib.ComputeMin2Ints(-1, 3) << endl;
	cout << lib.ComputeMin2Ints(1, -3) << endl;
	*/
	
	//Compute Max
	/*
	cout << lib.ComputeMax2Ints(1, 3) << endl;
	cout << lib.ComputeMax2Ints(-1, -3) << endl;

	cout << lib.ComputeMax2Ints(-1, 3) << endl;
	cout << lib.ComputeMax2Ints(1, -3) << endl;
	*/
	
	//Is Power of two
	/*
	for (int i=0; i<=(2<<10); ++i)
	{
		if (lib.IsPowerOfTwo(i))
			cout << "Power of 2 found: " << i << endl;
	}
	*/

	/* SetOrClearBit */
	/*
	int a = 3; // 0x00..011
	int mask = 2; //0x00..010 ; we want to set/clear 2nd bit
	bool clear = false;// to 0

	cout << lib.SetOrClearBit(a, mask, clear) << endl; //should produce 1(0x0..001)

	a = 3;
	mask = 4; //0x00..100
	clear = true; //set 3rd bit to 1

	cout << lib.SetOrClearBit(a, mask, clear) << endl; //should produce 7 (0x0..111)
	*/

	/* Conditional Negates */
	/*
	int val = 10;
	cout << lib.NegateValueIfFlagFalse(val, false) << endl;
	cout << lib.NegateValueIfFlagFalse(val, true) << endl;

	cout << lib.NegateValueIfFlagTrue(val, false) << endl;
	cout << lib.NegateValueIfFlagTrue(val, true) << endl;
	*/

	/* Merge bits */
	/*
	int a = 0;
	cout << lib.MergeBits(2, 1, a) << endl;
	*/

	/*Swap Bits*/
	/*
	int a=10, b=2;
	SwapAddSub(a, b);
	cout << a << " " << b << endl;
	SwapXOR(a, b);
	cout << a << " " << b << endl;
	*/

	/*Mod*/
	/*
	cout << lib.Mod(9,3) << endl; // 9 % (2^3) = 9 % 8
	*/

	/* Round to next highest power of 2 */
	/*
	cout << lib.RoundToNextHighestPower2FloatCast(5) << endl;
	cout << lib.RoundToNextHighestPower2(33) << endl;
	*/

	/* Computes sign of integers */
	/*
	cout << lib.ComputeIntegerSign(-11) << endl;
	cout << lib.ComputeIntegerSign(0) << endl;
	cout << lib.ComputeIntegerSign(21) << endl;
	*/
	return 0;
}
