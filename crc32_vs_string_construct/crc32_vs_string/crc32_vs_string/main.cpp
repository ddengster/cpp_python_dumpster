
#include <iostream>
#include <string>
#include "CRC32.h"

typedef std::string String;

String asd[100];
uint64 crc[100];
void test1(const String& s)
{
  static int i = 0;
  
  asd[i] = s;
  i++;
  i = i % 100;
}

void test2(const char* s)
{
  static int i = 0;
  crc[i] = CRC32_BlockChecksum(s, strlen(s) + 1);

  i++;
  i = i % 100;
}

void test3(const char* s)
{
  static int i = 0;

  int hash = 0;
  for (int k = 0; *s != '\0'; k++) {
    hash += (*s++) * (k + 119);
  }

  crc[i] = hash;

  i++;
  i = i % 100;
}

void test4(const char* s, int length)
{
  static int i = 0;

  int hash = 0;
  for (int k = 0; k < length; k++) {
    hash += (*s++) * (k + 119);
  }

  crc[i] = hash;

  i++;
  i = i % 100;
}

int main(int argc, char *argv[])
//int main()
{
  /*
  using namespace std;
  char s[32] = { 'A', 'S', 0, 's', 't' };
  char s2[32] = { 'A', 'S', 0, 'a', 'b' };
  char* t = "AS";
  cout << CRC32_BlockChecksum(s, strlen(s)) << endl;
  cout << CRC32_BlockChecksum(s2, strlen(s2)) << endl;
  cout << CRC32_BlockChecksum(s, 32) << endl;
  cout << CRC32_BlockChecksum(s2, 32) << endl;
  cout << CRC32_BlockChecksum(t, strlen(t)) << endl;
  cout << CRC32_BlockChecksum(t, 32) << endl;
  */
  //for 1,000,000
  //Timing
  //test2: ~0.15s to test1:~0.20s test3: ~0.1s, test4: 0.075
  const int lcount = atoi(argv[1]);
  //int lcount = 10000000;
  //int lcount = 1;
  
  for (int i = 0; i < lcount; i++)
  {
    //test3("qweqweqweqweqweqweqweqweqweqweqw");
    test4("qweqweqweqweqweqweqweqweqweqweqw", strlen("qweqweqweqweqweqweqweqweqweqweqw"));
  }
  return 0;
}