

#include <iostream>
#include "MortonCode.h"
using namespace std;

int main()
{
  //2d cartesian coordinate system
  uint rows = 16, cols = 16;
  uint sz_per_ele = 1;
  cout << "size per element: " << sz_per_ele << endl;

  uint arraycount = rows * cols;
  cout << "arrycount: " << arraycount << endl;
  cout << "final: " << morton_encode_2d(15, 15) << endl;

#if 0
  cout << "left to right, bottom to top iteration\n";
  for (uint y = 0; y < rows; ++y)
  {
    for (uint x = 0; x < cols; ++x)
    {
      uint arryid = morton_encode_2d(x, y);
      uint memoryoffset = sz_per_ele * arryid;
      cout << x << "," << y << " : [" << arryid << "], memoryoffset: " << memoryoffset << endl;
    }
  }
#endif
  cout << "left to right, iteration\n";
  uint y2 = 0;
  for (uint x = 0; x < cols; ++x)
  {
    uint arryid = morton_encode_2d(x, y2);
    uint memoryoffset = sz_per_ele * arryid;
    cout << x << "," << y2 << " : [" << arryid << "], memoryoffset: " << memoryoffset << endl;
  }

  cout << "top to bot, iteration \n";
  uint x2 = 0;
  for (uint y = 0; y < rows; ++y)
  {
    uint arryid = morton_encode_2d(x2, y);
    uint memoryoffset = sz_per_ele * arryid;
    cout << x2 << "," << y << " : [" << arryid << "], memoryoffset: " << memoryoffset << endl;
  }

  uint linear_mem_descent = sz_per_ele * cols;
  uint cachel = 64;
  cout << "\nif we descend down y coordinate of the array, linear_memory_descent = " << linear_mem_descent << endl;
  cout << "given cache line of " << cachel << ", cache miss every " << (float)cachel / (float)linear_mem_descent << endl;
  cout << "" << endl;

  cout << "3d tests..\n";
  {
    uint x = 0, y = 0;
    morton_decode_2d(morton_encode_2d(4, 26), x, y);
    cout << "(" << x << ", " << y << ")\n";
  }

  {
    uint x = 0, y = 0, z = 0;
    uint arryidx = morton_encode_3d(4, 26, 1);
    cout << arryidx << endl;
    morton_decode_3d(arryidx, x, y, z);
    cout << "(" << x << ", " << y << ", " << z << ")\n";
  }
  system("pause");
  return 0;
}