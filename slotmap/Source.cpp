
#include <iostream>
#include <vector>
#include <unordered_map>

using namespace std;

#define SLOTMAP_CHUNKSIZE 256
typedef unsigned char      Byte;
typedef unsigned char      uchar;
typedef unsigned short     ushort;
typedef unsigned int       uint;
typedef unsigned long      ulong;
typedef unsigned long long uint64;
typedef long long          int64;

class Object;

typedef uint ObjectHandle;
ushort GetHandleID(ObjectHandle handle)
{
  return handle >> 16;
}
ushort GetRecycleVersion(ObjectHandle handle)
{
  return (handle & 0x00ff);
}

ObjectHandle Make_ObjectHandle(ushort id, ushort recycle_ver)
{
  ObjectHandle r = ((uint)id << 16) | ((uint)recycle_ver);
  return r;
}

template <typename Object>
class SlotMap
{
public:
  SlotMap()
  { }

  ObjectHandle GrantHandle(Object* obj)
  {
    if (!mFreeIds.empty())
    {
      ObjectHandle handle = mFreeIds.back();
      mFreeIds.pop_back();

      ObjectHandle newhandle = Make_ObjectHandle(GetHandleID(handle), GetRecycleVersion(handle) + 1);
      mObjectIdHash.insert(std::make_pair(newhandle, obj));
      return newhandle;
    }
    else
    {
      ++mHighestId;
      ObjectHandle newhandle = Make_ObjectHandle(mHighestId, 0);
      mObjectIdHash.insert(std::make_pair(newhandle, obj));
      return newhandle;
    }
  }

  void ReserveHandle(ObjectHandle handle)
  {
    ReserveId(GetHandleID(handle));
  }

  void ReserveId(ushort id)
  {
    if (mHighestId <= id)
      mHighestId = id + 1;
  }

  void RecycleHandle(ObjectHandle id)
  {
    mObjectIdHash.erase(id);
    mFreeIds.emplace_back(id);
  }

  Object* GetObject(ObjectHandle id)
  {
    auto it = mObjectIdHash.find(id);
    if (it != mObjectIdHash.end())
      return it->second;
    return nullptr;
  }


  ushort mHighestId = 0;
  std::vector<ObjectHandle> mFreeIds;
  std::unordered_map<ObjectHandle, Object*> mObjectIdHash;
};


class ObjectT
{
public:
  static SlotMap<ObjectT>* mMap;

  ObjectHandle mObjectHandle;

  void Initialize()
  {
    mObjectHandle = mMap->GrantHandle(this);
  }

  void Destroy()
  {
    mMap->RecycleHandle(mObjectHandle);
  }
};

SlotMap<ObjectT>* ObjectT::mMap = nullptr;

int main()
{
  ObjectT::mMap = new SlotMap<ObjectT>();
  auto obj = new ObjectT;
  obj->Initialize();

  //ObjectT::mMap->RecycleHandle(obj->mObjectHandle);
  //deserialize(obj);
  //ObjectT::mMap->ReserveHandle(obj->mObjectHandle);

  uint s = Make_ObjectHandle(199, 15);
  
  auto v1 = GetHandleID(s);
  auto v2 = GetRecycleVersion(s);

  s = Make_ObjectHandle(v1, v2);
  return 0;
}