

#include "ReflectData.h"

void OutputReflectionCode(Outputs* out, RF* rfdat)
{
  FILE* inc = out->mOutputIncludes;
  FILE* rf = out->mRFData;
  FILE* rfenum = out->mRFEnums;
  
  fprintf(inc, "#include \"%s\"\r\n", rfdat->fileToReflect.c_str());
  
  /*
  {
    ObjectMember m[] = {
        { "x", "float", offsetof(Vector2, x), sizeof(float), 1, false, UNDEFINED_VERSION, UNDEFINED_VERSION },
      };
    auto c = reflectionmgr->GetObjectDescriptor("Vector2");
    c->BuildMembers(m, 1);
  }
  */
  
  for (int i = 0; i < rfdat->mReflectedData.size(); ++i)
  {
    ReflectData* r = &rfdat->mReflectedData[i]; 
    if (!r->mIsEnum)
    {
      fprintf(rf, "{\r\n");
      fprintf(rf, "  ObjectMember m[] = {\r\n");
      for (int j = 0; j < r->mMembers.size(); ++j)
      {
        MemberData* mem = &r->mMembers[j];
        if (mem->isFunction)
          continue; //@todo: functions

        char minver_buf[128] = { 0 };
        _itoa(mem->mMinVersion, minver_buf, 10);
        char maxver_buf[128] = { 0 };
        _itoa(mem->mMaxVersion, maxver_buf, 10);

        int arrsize = mem->mArraySize;
        if (arrsize == 0)
          arrsize = 1;

        fprintf(rf, "      { \"%s\", \"%s\", offsetof(%s, %s), sizeof(%s), %d, %s, %s, %s },\r\n",
          mem->mName.c_str(), mem->mType.c_str(), r->mName.c_str(), mem->mName.c_str(), mem->mType.c_str(), arrsize,
          mem->isPtr ? "true" : "false", mem->mMinVersion == -1 ? "UNDEFINED_VERSION" : minver_buf, mem->mMaxVersion == -1 ? "UNDEFINED_VERSION" : maxver_buf);
      }
      fprintf(rf, "    };\r\n");
      fprintf(rf, "  auto c = reflectionmgr->GetObjectDescriptor(\"%s\");\r\n", r->mName.c_str());
      fprintf(rf, "  c->BuildMembers(m, %d);\r\n", (int)r->mMembers.size());
      fprintf(rf, "}\r\n");
      fprintf(rf, "\r\n");
    }
    else
    {
      fprintf(rf, "{\r\n");

      string basetype = r->mEnumBaseType;
      if (basetype == "")
        basetype = "int";

      fprintf(rf, "  auto c = new ObjectDescriptor(\"%s\", sizeof(%s), false);\r\n", r->mName.c_str(), basetype.c_str());
      fprintf(rf, "  c->mIsEnumType = true;\r\n");
      fprintf(rf, "  c->mEnumToStr = %s_To_String;\r\n", r->mName.c_str());
      fprintf(rf, "  c->mStrToEnum = String_To_%s;\r\n", r->mName.c_str());
      fprintf(rf, "  c->mMinEnumVal = %d;\r\n", r->mEnumMinVal);
      fprintf(rf, "  c->mMaxEnumVal = %d;\r\n", r->mEnumMaxVal);
      fprintf(rf, "  reflectionmgr->RegisterObjectDescriptor(c);\r\n");
      fprintf(rf, "}\r\n");
      fprintf(rf, "\r\n");
    }
  }

  /*
  const char* ENUMTYPE_To_String(ENUMTYPE t)
  {
    if (t == VAL)
      return "VAL";
      ...
    return "ENUMTYPE_FIRSTVAL";
  }

  ENUMTYPE String_To_ENUMTYPE(const char* str)
  {
    if (strcmp(str, "VAL") == 0)
      return VAL;
    return ENUMTYPE_FIRSTVAL;
  }
  */
  for (int i = 0; i < rfdat->mReflectedData.size(); ++i)
  {
    ReflectData* r = &rfdat->mReflectedData[i];
    if (!r->mIsEnum)
      continue;

    fprintf(rfenum, "const char* %s_To_String(%s t)\r\n", r->mName.c_str(), r->mName.c_str());
    fprintf(rfenum, "{\r\n");
    for (int j = 0; j < r->mEnum.size(); ++j)
    {
      EnumVal enumval = r->mEnum[j];
      fprintf(rfenum, "  if (t == %s)\r\n", enumval.mStringVal.c_str());
      fprintf(rfenum, "    return \"%s\";\r\n", enumval.mStringVal.c_str());
    }
    if (r->mEnum.size())
      fprintf(rfenum, "  return \"%s\";\r\n", r->mEnum[0].mStringVal.c_str());
    fprintf(rfenum, "}\r\n");
    fprintf(rfenum, "\r\n");

    fprintf(rfenum, "%s String_To_%s(const char* str)\r\n", r->mName.c_str(), r->mName.c_str());
    fprintf(rfenum, "{\r\n");
    for (int j = 0; j < r->mEnum.size(); ++j)
    {
      EnumVal enumval = r->mEnum[j];
      fprintf(rfenum, "  if (strcmp(str, \"%s\") == 0)\r\n", enumval.mStringVal.c_str());
      fprintf(rfenum, "    return %s;\r\n", enumval.mStringVal.c_str());
    }
    if (r->mEnum.size())
      fprintf(rfenum, "  return %s;\r\n", r->mEnum[0].mStringVal.c_str());
    fprintf(rfenum, "}\r\n");
    fprintf(rfenum, "\r\n");
  }
  
}
