#pragma once

#include <string>
#include <vector>
using namespace std;

struct EnumVal
{
  int mVal = -1;
  string mStringVal;
};

struct ArgData
{
  int mIndex = -1;
  string mName;
  string mType;
  bool isPtr = false, isRef = false;
  //string mData;
};

struct MemberData
{
  string mName;
  string mType;
  bool isFunction = false;
  bool isPtr = false;
  bool isArray = false;
  int mArraySize = 0;
  std::vector<ArgData> mFuncArgs;

  //custom
  int mMinVersion = -1, mMaxVersion = -1;
  bool mMustReflect = false;
};

struct ReflectData
{
  string mName;
  string mBaseClassName;
  std::vector<MemberData> mMembers;

  //custom reflection data
  string mCategory;
  int mVersion = -1;
  string mFactory;
  bool mReflectAllFunctions = false;

  //enum
  bool mIsEnum = false;
  string mEnumBaseType;
  int mEnumMinVal = 0, mEnumMaxVal = INT_MAX;
  int mEnumMinValIdx = -1, mEnumMaxValIdx = -1;
  std::vector<EnumVal> mEnum;
};

struct RF
{
  string output;
  string fileToReflect;
  std::vector<ReflectData> mReflectedData;
};

struct Outputs
{
  string output;
  FILE* mOutputIncludes = nullptr;
  FILE* mRFData = nullptr;
  FILE* mRFEnums = nullptr;
};

extern void OutputReflectionCode(Outputs* out, RF* rfdat);