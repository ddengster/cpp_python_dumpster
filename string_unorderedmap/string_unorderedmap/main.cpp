
#include <unordered_map>
#include <string>

using namespace std;

struct object
{
  int a;
};

struct char_cmp 
{
  bool operator()(const char  *a, const char  *b)
  {
    return strcmp(a, b) < 0;
  }
};
/*
struct Hash_Func{
  //BKDR hash algorithm
  int operator()(const char * str)const
  {
    int seed = 131;//31  131 1313 13131131313 etc//
    int hash = 0;
    while (*str)
    {
      hash = (hash * seed) + (*str);
      str++;
    }

    return hash & (0x7FFFFFFF);
  }
};
*/

typedef unordered_map<const char*, object, std::hash<const char*>, char_cmp> Map;
//typedef unordered_map<string, object> Map;
Map m;

bool test(const char *s)
{
  Map::iterator it = m.find(s);
  return it != m.end();
}

int main(int argc, char *argv[])
{
  //10,000,000 iterations
  //all const char (including t): 0.35
  //strings: 0.4
  m.insert(std::make_pair("hello", object()));

  bool a = false;
  const char* t = "hello";
  //string t("hello");

  const int lcount = atoi(argv[1]);
  for (int i = 0; i < lcount; i++)
  {
    Map::iterator it = m.find(t);
    a = (it != m.end());
  }



  return a;
}