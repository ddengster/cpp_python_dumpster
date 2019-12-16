
#ifndef _TRICK2_H
#define _TRICK2_H

/******************
Compile-Time Constants from Binary Representations
******************/

#define cat(a,b) a##b

void Trick2()
{
	int a = cat(1,2);
	std::cout << a << std::endl; // 1 and 2 make 12!
}

//More

#define HEX_DIGIT_0000 0
#define HEX_DIGIT_0001 1
#define HEX_DIGIT_0010 2
#define HEX_DIGIT_0011 3
#define HEX_DIGIT_0100 4
#define HEX_DIGIT_0101 5
#define HEX_DIGIT_0110 6
#define HEX_DIGIT_0111 7
#define HEX_DIGIT_1000 8
#define HEX_DIGIT_1001 9
#define HEX_DIGIT_1010 a
#define HEX_DIGIT_1011 b
#define HEX_DIGIT_1100 c
#define HEX_DIGIT_1101 d
#define HEX_DIGIT_1110 e
#define HEX_DIGIT_1111 f

#define HEX_DIGIT(a) HEX_DIGIT_ ## a // thus, HEX_DIGIT(1010) becomes HEX_DIGIT_1010, then becomes a.

#define BINARY1H(a) (0x ## a) //BINARY1H(10) becomes (0x10)
#define BINARY1I(a) BINARY1H(a) //BINARY1I(10) becomes BINARYH(10) becomes 0x10
#define BINARY1(a) BINARY1I(HEX_DIGIT(a)) //BINARY1(0110) -> BINARYI(HEX_DIGIT(0110)) -> BINARYI(a) -> 0xa


#define BINARY2H(a,b) (0x ## a ## b)
#define BINARY2I(a,b) BINARYH(a,b)
#define BINARY2(a,b) BINARY1I(HEX_DIGIT(a), HEX_DIGIT(b))

#define BINARY8H(a,b,c,d,e,f,g,h) (0x##a##b##c##d##e##f##g##h)
#define BINARY8I(a,b,c,d,e,f,g,h) BINARY8H(a,b,c,d,e,f,g,h)
#define BINARY8(a,b,c,d,e,f,g,h) BINARY8I(HEX_DIGIT(a), HEX_DIGIT(b), HEX_DIGIT(c), HEX_DIGIT(d), HEX_DIGIT(e), \
								HEX_DIGIT(f), HEX_DIGIT(g), HEX_DIGIT(h))

const int nibble = BINARY1(0101); //0x5
const int byte   = BINARY2(1010, 0101); //0xa5


#endif
