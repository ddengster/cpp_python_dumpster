

#ifndef _CRC32_H
#define _CRC32_H


typedef unsigned char uchar;
typedef unsigned long long uint64;

//for updating a crc as it's passed through a stream
void CRC32_Init(uint64 &crcvalue);
void CRC32_UpdateChecksum(uint64 &crcvalue, const void *data, int length);
void CRC32_FinishChecksum(uint64 &crcvalue);

//for quickly computing crc for a block of data
uint64 CRC32_BlockChecksum(const void *data, int length);


#endif


