
#define LEXER_USE_FIXED_TYPES
#define LEXER_USE_ASSERT
#define LEXER_IMPLEMENTATION
#include "lexer.h"

#include "ReflectData.h"

#include <stdio.h>
#include <stdarg.h>
#include <limits>



static void test_log(void *pArg, enum lexer_log_level type, lexer_size line, const char *fmt, ...)
{
  //char buffer[1024];
  va_list arglist;
  (void)pArg;
  va_start(arglist, fmt);
  printf("%s(%lu):  ", (type == LEXER_WARNING) ? "Warning" : "Error", line);
  vprintf(fmt, arglist);
  va_end(arglist);
}


static void ParseFunctionArgs(MemberData* funcdata, struct lexer* lx, struct lexer_token* first_after_paren_tok)
{
  lexer_token* tok = first_after_paren_tok;
  for (;;)
  {
    //@todo: read arguements
    const char* ignored_keywords[] = { "const" };
    int ignorecount = sizeof(ignored_keywords) / sizeof(const char*);
    for (int i = 0; i < ignorecount; ++i)
    {
      if (!lexer_token_cmp(tok, ignored_keywords[i]))
        lexer_read(lx, tok);
    }

    if (tok->type == LEXER_TOKEN_NAME)
    {
      ArgData argdata;
      argdata.mIndex = (int)funcdata->mFuncArgs.size();
      argdata.mType = string(tok->str, tok->len);

      //@todo: double pointers
      int r = lexer_read(lx, tok);
      if (r && tok->type == LEXER_TOKEN_PUNCTUATION && tok->subtype == LEXER_PUNCT_MUL)
      {
        argdata.isPtr = true;
        r = lexer_read(lx, tok);
      }
      if (r && tok->type == LEXER_TOKEN_PUNCTUATION && tok->subtype == LEXER_PUNCT_BIN_AND)
      {
        argdata.isRef = true;
        r = lexer_read(lx, tok);
      }

      if (r && tok->type == LEXER_TOKEN_NAME)
      {
        argdata.mName = string(tok->str, tok->len);
        funcdata->mFuncArgs.push_back(argdata);
        printf(" args: type %s, name %s\n", argdata.mType.c_str(), argdata.mName.c_str());
      }
    }
    
    if (tok->type == LEXER_TOKEN_PUNCTUATION && tok->subtype == LEXER_PUNCT_PARENTHESE_CLOSE)
      break;
    int r = lexer_read(lx, tok);
    if (r == 0)
      break;
  }
}

static void ParseStructReflectionArgs(ReflectData* rf, struct lexer* lx, struct lexer_token* first_after_paren_tok)
{
  lexer_token* tok = first_after_paren_tok;
  for (;;)
  {
    if (tok->type == LEXER_TOKEN_NAME && !lexer_token_cmp(tok, "category"))
    {
      lexer_token t2;
      int r = lexer_read(lx, &t2);
      if (r && t2.type == LEXER_TOKEN_PUNCTUATION && t2.subtype == LEXER_PUNCT_COLON)
      {
        r = lexer_read(lx, &t2);
        /*if (r && t2.type == LEXER_TOKEN_STRING)
          rf->mCategory = string(t2.str, t2.len);*/
        if (r && t2.type == LEXER_TOKEN_NAME)
          rf->mCategory = string(t2.str, t2.len);
      }
    }

    if (tok->type == LEXER_TOKEN_NAME && !lexer_token_cmp(tok, "version"))
    {
      lexer_token t2;
      int r = lexer_read(lx, &t2);
      if (r && t2.type == LEXER_TOKEN_PUNCTUATION && t2.subtype == LEXER_PUNCT_COLON)
      {
        r = lexer_read(lx, &t2);
        if (r && t2.type == LEXER_TOKEN_NUMBER)
          rf->mVersion = lexer_token_to_int(&t2);
      }
    }

    if (tok->type == LEXER_TOKEN_NAME && !lexer_token_cmp(tok, "factory"))
    {
      lexer_token t2;
      int r = lexer_read(lx, &t2);
      if (r && t2.type == LEXER_TOKEN_PUNCTUATION && t2.subtype == LEXER_PUNCT_COLON)
      {
        r = lexer_read(lx, &t2);
        if (r && t2.type == LEXER_TOKEN_NAME)
        {
          string factorytype = string(t2.str, t2.len);
          if (factorytype == "trackless" || factorytype == "trackful" || factorytype == "handle")
            rf->mFactory = factorytype;
        }
      }
    }

    if (tok->type == LEXER_TOKEN_NAME && !lexer_token_cmp(tok, "reflectallfunctions"))
    {
      rf->mReflectAllFunctions = true;
    }

    if (tok->type == LEXER_TOKEN_PUNCTUATION && tok->subtype == LEXER_PUNCT_PARENTHESE_CLOSE)
      break;

    int r = lexer_read(lx, tok);
    if (r == 0)
      break;
  }
}

static void ParseMemberReflectionArgs(MemberData* memdata, struct lexer* lx, struct lexer_token* first_after_paren_tok)
{
  memdata->mMustReflect = true;
  lexer_token* tok = first_after_paren_tok;
  for (;;)
  {
    if (tok->type == LEXER_TOKEN_NAME)
    {
      if (!lexer_token_cmp(tok, "minver"))
      {
        lexer_token t2;
        int r = lexer_read(lx, &t2);
        if (r && t2.type == LEXER_TOKEN_PUNCTUATION && t2.subtype == LEXER_PUNCT_COLON)
        {
          r = lexer_read(lx, &t2);
          if (r && t2.type == LEXER_TOKEN_NUMBER)
            memdata->mMinVersion = lexer_token_to_int(&t2);
        }
      }
      else if (!lexer_token_cmp(tok, "maxver"))
      {
        lexer_token t2;
        int r = lexer_read(lx, &t2);
        if (r && t2.type == LEXER_TOKEN_PUNCTUATION && t2.subtype == LEXER_PUNCT_COLON)
        {
          r = lexer_read(lx, &t2);
          if (r && t2.type == LEXER_TOKEN_NUMBER)
            memdata->mMaxVersion = lexer_token_to_int(&t2);
        }
      }
    }
    if (tok->type == LEXER_TOKEN_PUNCTUATION && tok->subtype == LEXER_PUNCT_PARENTHESE_CLOSE)
      break;
    int r = lexer_read(lx, tok);
    if (r == 0)
      break;
  }
}

static void ParseMembers(ReflectData* rf, struct lexer* lx, struct lexer_token* firsttok)
{
  struct lexer_token* first_meaningful_tok = firsttok;
  MemberData dat;
  //Special define for inputting per-member metadata, place it at the front
  if (first_meaningful_tok->type == LEXER_TOKEN_NAME && !lexer_token_cmp(first_meaningful_tok, "META"))
  {
    lexer_read(lx, first_meaningful_tok);
    ParseMemberReflectionArgs(&dat, lx, first_meaningful_tok);
    lexer_read(lx, first_meaningful_tok); //get token after the close parenthesis
    //do the ignore keywords consumption routine
  }

  const char* ignored_keywords[] = { "public", "private", "inline", "explicit", "const" };
  int ignorecount = sizeof(ignored_keywords) / sizeof(const char*);
  const char* skipped_keywords[] = { "static", "friend" };
  int skipcount = sizeof(skipped_keywords) / sizeof(const char*);

  bool checkagain = true, skip = false;
  do
  {
    checkagain = false;
    for (int i = 0; i < ignorecount; ++i)
    {
      if (!lexer_token_cmp(first_meaningful_tok, ignored_keywords[i]))
      {
        checkagain = true;
        lexer_read(lx, first_meaningful_tok);
        if (i == 0 || i == 1)
          lexer_read(lx, first_meaningful_tok); //consume an extra colon
      }
    }

    for (int i = 0; i < skipcount; ++i)
    {
      if (!lexer_token_cmp(first_meaningful_tok, skipped_keywords[i]))
      {
        checkagain = true;
        skip = true;
        lexer_read(lx, first_meaningful_tok);
      }
    }
  } while (checkagain);

  if (skip)
  {
    do
    {
      struct lexer_token tok;
      int r =lexer_read(lx, &tok);
      if (r)
      {
        if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_PARENTHESE_OPEN)
          lexer_skip_until(lx, ")");

        if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_BRACE_OPEN)
        {
          lexer_skip_until(lx, "}");
          break;
        }
        if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_SEMICOLON)
          break;
      }
    } while (1);
    return;
  }

  //by here, the first token of any meaning to our reflection data is settled, find the name
  struct lexer_token tok;
  int r = lexer_read(lx, &tok);

  //pointer/reference accounting
  if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_MUL)
  {
    dat.isPtr = true;
    /*else if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_BIN_AND)
    dat.isref = true;*/
    //@todo: double or more pointers
    r = lexer_read(lx, &tok);
  }
  if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_BIN_AND)
  {
    /*dat.isref = true;*/;
    r = lexer_read(lx, &tok);
  }

  if (r)
  {
    if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_PARENTHESE_OPEN) 
    {
      //( encountered, some kind of macro OR a constructor
      for (;;)
      {
        //@todo: define/constructor params
        struct lexer_token tok2;
        if (lexer_read(lx, &tok2) && tok2.type == LEXER_TOKEN_PUNCTUATION && tok2.subtype == LEXER_PUNCT_PARENTHESE_CLOSE)
          break;
      }

      struct lexer_token tok2;
      r = lexer_read(lx, &tok2);

      //for constructors, we may get the initialization list, and we can expect the function body too, so skip until we have a brace
      if (r && tok2.type == LEXER_TOKEN_PUNCTUATION && tok2.subtype == LEXER_PUNCT_COLON)
      {
        lexer_skip_until(lx, "}");
      }

      if (r && tok2.type == LEXER_TOKEN_PUNCTUATION && tok2.subtype == LEXER_PUNCT_BRACE_OPEN)
        lexer_skip_until(lx, "}");
    }
    else if (tok.type == LEXER_TOKEN_NAME)
    {
      dat.mType = string(first_meaningful_tok->str, first_meaningful_tok->len);
      dat.mName = string(tok.str, tok.len);
      int a;
      if (rf->mMembers.size() == 18)
        a = 0;
      
      r = lexer_read(lx, &tok);
      if (dat.mName == "operator")
      {
        do
        {
          if (r && tok.type == LEXER_TOKEN_PUNCTUATION)
          {
            string punc = string(tok.str, tok.len);
            dat.mName.append(punc);
            r = lexer_read(lx, &tok);
            if (r && tok.subtype == LEXER_PUNCT_PARENTHESE_OPEN)
              break;
          }
        } while (1);
        
      }
      //printf("type: %s, name: %s\n", dat.mType.c_str(), dat.mName.c_str());

      if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_PARENTHESE_OPEN) //a function
      {
        dat.isFunction = true;
        bool reflect = rf->mReflectAllFunctions || dat.mMustReflect;
        if (reflect)
        {
          lexer_token tk;
          lexer_read(lx, &tk);
          ParseFunctionArgs(&dat, lx, &tk);
        }
        else
        {
          for (;;)
          {
            lexer_token tk;
            if (lexer_read(lx, &tk) && tk.type == LEXER_TOKEN_PUNCTUATION && tk.subtype == LEXER_PUNCT_PARENTHESE_CLOSE)
              break;
          }
        }
        
        r = lexer_read(lx, &tok);
        if (r)
        {
          if (tok.type == LEXER_TOKEN_NAME && !lexer_token_cmp(&tok, "const")) //const function
            r = lexer_read(lx, &tok);
          if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_BRACE_OPEN)
            lexer_skip_until(lx, "}");
          if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_SEMICOLON)
            ; //end
          if (reflect)
            rf->mMembers.push_back(dat);
        }
      }
      //below is member data, check for commas or end with semicolons
      else if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_SEMICOLON)
      {
        rf->mMembers.push_back(dat);
      }
      else if (r && tok.type == LEXER_TOKEN_PUNCTUATION)
      {
        bool parsingarraymember = false;
        MemberData s = dat;
        struct lexer_token tok2 = tok;
        for (;;)
        {
          if (tok2.subtype == LEXER_PUNCT_ASSIGN)
            ;

          if (tok2.subtype == LEXER_PUNCT_BRACKET_OPEN)
          {
            parsingarraymember = true;
            s.isArray = true;
          }
          if (tok2.subtype == LEXER_PUNCT_BRACKET_CLOSE)
            parsingarraymember = false;
          if (parsingarraymember == true && tok2.type == LEXER_TOKEN_NUMBER && (tok2.subtype & LEXER_TOKEN_INT))
          {
            s.mArraySize = lexer_token_to_int(&tok2);
          }

          if (tok2.type == LEXER_TOKEN_NAME)
          {
            s.mName = string(tok2.str, tok2.len);
            /*if (s.mName == "cz")
              a = 0;*/
          }

          if (tok2.subtype == LEXER_PUNCT_COMMA)
          {
            rf->mMembers.push_back(s);
            s.isArray = false;
          }
          if (tok2.subtype == LEXER_PUNCT_SEMICOLON)
          {
            rf->mMembers.push_back(s);
            break;
          }

          r = lexer_read(lx, &tok2);
          if (r == 0)
            break;
        }
      }
    }
  }

}


static void ParseIntrospectable(ReflectData* rf, struct lexer* lx)
{
  struct lexer_token tok;
  if (lexer_read(lx, &tok) && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_PARENTHESE_OPEN)
  {
    //parse introspec params
    {
      struct lexer_token tok2;
      if (lexer_read(lx, &tok2))
        ParseStructReflectionArgs(rf, lx, &tok2);
    }
    
    int r = lexer_read(lx, &tok);
    if (r && (!lexer_token_cmp(&tok, "struct") || !lexer_token_cmp(&tok, "class")))
    {
      if (lexer_read(lx, &tok))
      {
        if (tok.type == LEXER_TOKEN_NAME)
          rf->mName = string(tok.str, tok.len);

        //single inheritance, multi inheritance not yet supported
        r = lexer_read(lx, &tok);
        if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_COLON)
        {
          if (lexer_read(lx, &tok) && tok.type == LEXER_TOKEN_NAME && (!lexer_token_cmp(&tok, "public") || !lexer_token_cmp(&tok, "private")))
          {
            if (lexer_read(lx, &tok) && tok.type == LEXER_TOKEN_NAME)
              rf->mBaseClassName = string(tok.str, tok.len);
          }
        }

        if (lexer_skip_until(lx, "{"))
        {
          //start reading members
          for (;;)
          {
            if (lexer_read(lx, &tok))
            {
              if (tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_BRACE_CLOSE)
                break;
              else
                ParseMembers(rf, lx, &tok);
            }
          }
        }
      }
      printf("done!");
      printf("members for %s:\n", rf->mName.c_str());
      for (auto member : rf->mMembers)
      {
        printf(" type: %s, name: %s, isfunc: %d\n", 
          member.mType.c_str(), member.mName.c_str(), member.isFunction ? 1 : 0);
      }
    }
    else if (r && !lexer_token_cmp(&tok, "enum"))
    {
      rf->mIsEnum = true;
      int r = lexer_read(lx, &tok);
      if (r)
        rf->mName = string(tok.str, tok.len);

      r = lexer_read(lx, &tok);
      if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_COLON)
      {
        r = lexer_read(lx, &tok); //inheritance
        if (r && tok.type == LEXER_TOKEN_NAME)
        {
          rf->mEnumBaseType = string(tok.str, tok.len);
          r = lexer_read(lx, &tok);
        }
      }

      if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_BRACE_OPEN)
      {
        int e = -1;
        r = lexer_read(lx, &tok);
        for (;;)
        {
          string ename;
          if (r && tok.type == LEXER_TOKEN_NAME)
          {
            ename = string(tok.str, tok.len);
            r = lexer_read(lx, &tok);

            if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_ASSIGN)
            {
              r = lexer_read(lx, &tok);
              if (r && tok.type == LEXER_TOKEN_NUMBER && tok.subtype & LEXER_TOKEN_INT)
              {
                e = lexer_token_to_int(&tok);
                r = lexer_read(lx, &tok);
              }
            }
            else
              e += 1;

            if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_COMMA)
            {
              EnumVal new_ev;
              new_ev.mVal = e;
              new_ev.mStringVal = ename;
              rf->mEnum.push_back(new_ev);
            }
          }

          if (r && tok.type == LEXER_TOKEN_PUNCTUATION && tok.subtype == LEXER_PUNCT_BRACE_CLOSE)
            break;
          r = lexer_read(lx, &tok);
          if (r == 0)
            break;
        } 
      }

      rf->mEnumMinVal = INT_MAX;
      rf->mEnumMaxVal = -INT_MAX;
      printf("done!\n");
      printf("enum name: %s, basetype: %s\n", rf->mName.c_str(), rf->mEnumBaseType.c_str());
      for (int i = 0; i < rf->mEnum.size(); ++i)
      {
        printf("enumval name: %s, intval: %d\n", rf->mEnum[i].mStringVal.c_str(), rf->mEnum[i].mVal);
        if (rf->mEnumMinVal > rf->mEnum[i].mVal)
        {
          rf->mEnumMinVal = rf->mEnum[i].mVal;
          rf->mEnumMinValIdx = i;
        }
        if (rf->mEnumMaxVal < rf->mEnum[i].mVal)
        {
          rf->mEnumMaxVal = rf->mEnum[i].mVal;
          rf->mEnumMaxValIdx = i;
        }
      }
      
      printf("minval: %d at idx %d, maxval: %d at idx %d\n", 
        rf->mEnumMinVal, rf->mEnumMinValIdx, rf->mEnumMaxVal, rf->mEnumMaxValIdx);
    }
  }
  else
    printf("error; introspect must have parentheses");
}

struct A
{
  float iz;
  const char* a = nullptr;
  float zz;
};
void MyFunc(A az) { /*printf("ASDSD\n");*/ }

int main()
{
  A a[] = { 
    {1, "as", 2}, 
    {1, "as", 2} 
  };
  //printf("%d", offsetof(A, a));
  
  A z = { 1, "a", 2 };
  //A z2 = { 1 }; //compile err
  MyFunc({ 1,"ss", 1 });

  string currentoutput;
  std::vector<RF> files;
  std::vector<Outputs> outputs;
  FILE* f = fopen("reflection.txt", "rb+");
  if (f)
  {
    char line[512] = { 0 };
    while (1)
    {
      if (fgets(line, 512, f) == nullptr)
        break;
      if (strcmp(line, "\r\n") == 0)
        continue;

      char d[512] = { 0 };
      int args = sscanf_s(line, "reflectoutput:%s\r\n", d, 512);
      if (args)
      {
        currentoutput = d;

        Outputs o;
        o.output = d;

        string includesfilename = d;
        includesfilename.append("_includes.gen");
        o.mOutputIncludes = fopen(includesfilename.c_str(), "wb+");

        string rfcodefilename = d;
        rfcodefilename.append("_reflected.gen");
        o.mRFData = fopen(rfcodefilename.c_str(), "wb+");

        string rfenumfilename = d;
        rfenumfilename.append("_enums.gen");
        o.mRFEnums = fopen(rfenumfilename.c_str(), "wb+");
        outputs.push_back(o);
      }
      else
      {
        RF s;
        s.output = currentoutput;
        s.fileToReflect = line;
        if (s.fileToReflect[s.fileToReflect.length() - 1] == '\n')
        {
          s.fileToReflect.pop_back();
          s.fileToReflect.pop_back();
        }
        files.push_back(s);
        printf("%s: %s\n", s.output.c_str(), s.fileToReflect.c_str());
      }
    }
    fclose(f);
  }

  for (int i = 0; i < files.size(); ++i)
  {
    char* contents = nullptr;
    size_t size = 0;
    FILE *File = fopen(files[i].fileToReflect.c_str(), "r");
    if (File)
    {
      fseek(File, 0, SEEK_END);
      size = ftell(File);
      fseek(File, 0, SEEK_SET);

      contents = new char[size + 1];
      fread(contents, size, 1, File);
      contents[size] = 0;

      fclose(File);
    }
    else
      continue;
    if (size == 0)
      continue;

    struct lexer_token tok;
    struct lexer lexer;
    lexer_init(&lexer, contents, size, NULL, test_log, NULL);

    while (true)
    {
      //lexer_check_type(lexer, LEXER_TOKEN_NAME, 0, tok);
      //int res = lexer_check_string(&lexer, "introspect");
      if (lexer_read(&lexer, &tok) && !lexer_token_cmp(&tok, "introspect"))
      {
        ReflectData dat;
        files[i].mReflectedData.push_back(dat);
        ParseIntrospectable(&files[i].mReflectedData[files[i].mReflectedData.size() - 1], &lexer);
      }

      if (lexer.current == lexer.end)
        break;
    }

    for (int i = 0; i < outputs.size(); ++i)
    {
      if (files[i].output == outputs[i].output)
      {
        OutputReflectionCode(&outputs[i], &files[i]);
        break;
      }
    }
  }

  for (int i = 0; i < outputs.size(); ++i)
  {
    fclose(outputs[i].mOutputIncludes);
    fclose(outputs[i].mRFData);
    fclose(outputs[i].mRFEnums);
  }
  return 0;
}