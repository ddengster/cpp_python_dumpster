// regextest.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include <string>
#include <regex>

int main()
{
    std::cout << "Hello World!\n"; 
    FILE* f = fopen("./doc.txt", "rb+");
    if (f)
    {
      fseek(f, 0, SEEK_END);
      int length = ftell(f);
      fseek(f, 0, SEEK_SET);

      char* buf = new char[length];
      
      fread(buf, 1, length, f);


      std::string str = buf;
      std::smatch m;
      std::regex regex("(?:<a href=\")(.*)(\">)");
      while (std::regex_search(str, m, regex)) {
        std::cout << m.size() << std::endl;
        std::string s = m.str(0);
        std::cout << s << std::endl;
        for (int i=0; i<m.size(); ++i) 
          std::cout << m[i].str() << " ";
        std::cout << std::endl;
        str = m.suffix().str();
      }

      fclose(f);
    }

}
