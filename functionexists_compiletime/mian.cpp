
#include <iostream>

using namespace std;

template<typename T>
struct has_size_method
{
private:
  typedef std::true_type yes;
  typedef std::false_type no;

  template<typename U> static auto test(int) -> decltype(std::declval<U>().GetObjDesc() == 1, yes());

  template<typename> static no test(...);

public:

  static const bool value = std::is_same<decltype(test<T>(0)), yes>::value;
};

template <typename T> class Has_GetObjectDesc_Func
{
  template <typename U> static std::true_type test(decltype(std::declval<U>().GetObjDesc())*);
  template <typename U> static std::false_type test(...);

public:
  /*! \brief True if the type has a GetObjectDesc() method, false otherwise. */
  static const bool value = std::is_same<decltype(test<T>(nullptr)), std::true_type>::value;
};

class AB
{
public:
  int GetObjDesc() { return 0; }
};
int main()
{
  AB v;
  if (v.GetObjDesc())
    cout << "ASDSA";
  cout << Has_GetObjectDesc_Func<AB>::value << endl; //doesnot work in msvc2012!!!!
  return 0;
}