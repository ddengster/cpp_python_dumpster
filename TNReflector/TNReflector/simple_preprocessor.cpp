/* ========================================================================
   $File: $
   $Date: $
   $Revision: $
   $Creator: Casey Muratori $
   $Notice: (C) Copyright 2015 by Molly Rocket, Inc. All Rights Reserved. $
   ======================================================================== */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include <vector>
#include <string>
using namespace std;

//@note: As a rule of thumb, make sure your struct is compilable before you slap a introspect keyword on it


struct member_struct
{
  string mType;
  string mName;
  bool mIsFunction = false, mIsPointer = false;
};
struct meta_struct
{
    string Name;
    string mBaseClass;
    std::vector<member_struct> mMembers;
};
static std::vector<meta_struct> gIntrospectionData;
meta_struct* gCurrentEditing = nullptr;

static char* ReadEntireFileIntoMemoryAndNullTerminate(char *FileName)
{
    char *Result = 0;
    
    FILE *File = fopen(FileName, "r");
    if(File)
    {
        fseek(File, 0, SEEK_END);
        size_t FileSize = ftell(File);
        fseek(File, 0, SEEK_SET);

        Result = (char *)malloc(FileSize + 1);
        fread(Result, FileSize, 1, File);
        Result[FileSize] = 0;
        
        fclose(File);
    }

    return(Result);
}

enum token_type
{
    Token_Unknown,
    
    Token_OpenParen,
    Token_CloseParen,
    Token_Colon,
    Token_Semicolon,
    Token_Asterisk,
    Token_Ampersand,
    Token_OpenBracket,
    Token_CloseBracket,
    Token_OpenBrace,
    Token_CloseBrace,
    Token_Equals,
    Token_Comma,

    Token_String,
    Token_Identifier,

    Token_EndOfStream,
};
struct token
{
    token_type Type;
    
    size_t TextLength;
    char *Text;
};

struct tokenizer
{
    char *At;
};

inline bool IsEndOfLine(char C)
{
    bool Result = ((C == '\n') ||
                   (C == '\r'));

    return(Result);
}

inline bool IsWhitespace(char C)
{
    bool Result = ((C == ' ') ||
                   (C == '\t') ||
                   (C == '\v') ||
                   (C == '\f') ||
                   IsEndOfLine(C));

    return(Result);
}

inline bool IsAlpha(char C)
{
    bool Result = (((C >= 'a') && (C <= 'z')) ||
                   ((C >= 'A') && (C <= 'Z')));

    return(Result);
}

inline bool IsNumber(char C)
{
    bool Result = ((C >= '0') && (C <= '9'));
    
    return(Result);
}

inline bool TokenEquals(token Token, char *Match)
{
    char *At = Match;
    for(int Index = 0;
        Index < Token.TextLength;
        ++Index, ++At)
    {
        if((*At == 0) ||
           (Token.Text[Index] != *At))
        {
            return(false);
        }
        
    }

    bool Result = (*At == 0);
    return(Result);
}

static void EatAllWhitespace(tokenizer *Tokenizer)
{
    for(;;)
    {
        if(IsWhitespace(Tokenizer->At[0]))
        {
            ++Tokenizer->At;
        }
        else if((Tokenizer->At[0] == '/') &&
                (Tokenizer->At[1] == '/'))
        {
            Tokenizer->At += 2;
            while(Tokenizer->At[0] && !IsEndOfLine(Tokenizer->At[0]))
            {
                ++Tokenizer->At;
            }
        }
        else if((Tokenizer->At[0] == '/') &&
                (Tokenizer->At[1] == '*'))
        {
            Tokenizer->At += 2;
            while(Tokenizer->At[0] &&
                  !((Tokenizer->At[0] == '*') &&
                    (Tokenizer->At[1] == '/')))
            {
                ++Tokenizer->At;
            }
            
            if(Tokenizer->At[0] == '*')
            {
                Tokenizer->At += 2;
            }
        }
        else
        {
            break;
        }
    }
}

static token GetToken(tokenizer *Tokenizer)
{
    EatAllWhitespace(Tokenizer);

    token Token = {};
    Token.TextLength = 1;
    Token.Text = Tokenizer->At;
    char C = Tokenizer->At[0];
    ++Tokenizer->At;
    switch(C)
    {
        case '\0': {Token.Type = Token_EndOfStream;} break;
           
        case '(': {Token.Type = Token_OpenParen;} break;
        case ')': {Token.Type = Token_CloseParen;} break;
        case ':': {Token.Type = Token_Colon;} break;
        case ';': {Token.Type = Token_Semicolon;} break;
        case '*': {Token.Type = Token_Asterisk;} break;
        case '&': {Token.Type = Token_Ampersand; } break;
        case '[': {Token.Type = Token_OpenBracket;} break;
        case ']': {Token.Type = Token_CloseBracket;} break;
        case '{': {Token.Type = Token_OpenBrace;} break;
        case '}': {Token.Type = Token_CloseBrace;} break;
        case '=': {Token.Type = Token_Equals; } break;
        case ',': {Token.Type = Token_Comma; } break;
          
            
        case '"':
        {
            Token.Type = Token_String;

            Token.Text = Tokenizer->At;
            
            while(Tokenizer->At[0] &&
                  Tokenizer->At[0] != '"')
            {
                if((Tokenizer->At[0] == '\\') &&
                   Tokenizer->At[1])
                {
                    ++Tokenizer->At;
                }
                ++Tokenizer->At;
            }

            Token.TextLength = Tokenizer->At - Token.Text;
            if(Tokenizer->At[0] == '"')
            {
                ++Tokenizer->At;
            }
        } break;

        default:
        {
            if(IsAlpha(C))
            {
                Token.Type = Token_Identifier;
                
                while(IsAlpha(Tokenizer->At[0]) ||
                      IsNumber(Tokenizer->At[0]) ||
                      (Tokenizer->At[0] == '_'))
                {
                    ++Tokenizer->At;
                }
                
                Token.TextLength = Tokenizer->At - Token.Text;

            }
#if 0
            else if(IsNumeric(C))
            {
                ParseNumber();
            }
#endif
            else
            {
                Token.Type = Token_Unknown;
            }
        } break;
    }

    return(Token);
}

static bool RequireToken(tokenizer *Tokenizer, token_type DesiredType)
{
    token Token = GetToken(Tokenizer);
    bool Result = (Token.Type == DesiredType);
    return(Result);
}

static bool ParseIntrospectionParams(tokenizer *Tokenizer)
{
    bool Valid = true;
    
    for(;;)
    {
        token Token = GetToken(Tokenizer);
        if(TokenEquals(Token, "IGNORED"))
        {
            Valid = false;
            break;
        }
        
        if((Token.Type == Token_CloseParen) ||
           (Token.Type == Token_EndOfStream))
        {
            break;
        }
    }

    return(Valid);
}

static void ParseMember(tokenizer *Tokenizer, token structNameToken, token MemberTypeToken)
{
#if 1
    bool Parsing = true;
    bool IsPointer = false;
    bool foundmembername = false;
    bool isConstructorOrDefine = false;
    bool memberIsFunction = false;
    while(Parsing)
    {
        token Token = GetToken(Tokenizer);
        if (!foundmembername)
        {
          if (Token.Type == Token_Asterisk)
            IsPointer = true;
          else if (Token.Type == Token_Ampersand)
          { }
          else if (Token.Type == Token_OpenParen)
          {
            //input arguments; very likely a #define with parameters OR a constructor, skip
            isConstructorOrDefine = true;
            for (;;)
            {
              token t = GetToken(Tokenizer);
              if (t.Type == Token_CloseParen)
                break;
              if (t.Type == Token_EndOfStream)
                break;
            }
          }
          else if (Token.Type == Token_OpenBrace)
          {
            //an open brace; should be some function for inline code
            bool endparsemember = false;
            for (;;)
            {
              token t = GetToken(Tokenizer);
              if (t.Type == Token_CloseBrace)
              {
                endparsemember = true;
                break;
              }
              if (t.Type == Token_EndOfStream)
                break;
            }
            if (endparsemember)
              break;
          }
          else if (Token.Type == Token_Identifier)
          {
            if (!isConstructorOrDefine)
            {
              foundmembername = true;
              member_struct s;
              s.mType = string(MemberTypeToken.Text, MemberTypeToken.TextLength);
              s.mName = string(Token.Text, Token.TextLength);
              s.mIsPointer = IsPointer;
              gCurrentEditing->mMembers.push_back(s);
            }
          }
          else if (Token.Type == Token_Semicolon)
          {
            break;
          }
        }
        else if (foundmembername)
        {
          member_struct* member = &gCurrentEditing->mMembers[gCurrentEditing->mMembers.size() - 1];
          string type = member->mType;
          if (Token.Type == Token_OpenParen)
          {
            member->mIsFunction = true;
            //function input arguments; todo
            for (;;)
            {
              token t = GetToken(Tokenizer);
              if (t.Type == Token_CloseParen)
                break;
              if (t.Type == Token_EndOfStream)
                break;
            }
          }
          else if (Token.Type == Token_OpenBrace)
          {
            //an open brace; should be some function for inline code
            bool endparsemember = false;
            for (;;)
            {
              token t = GetToken(Tokenizer);
              if (t.Type == Token_CloseBrace)
              {
                endparsemember = true;
                break;
              }
              if (t.Type == Token_EndOfStream)
                break;
            }
            if (endparsemember)
              break;
          }
          else if (Token.Type == Token_Comma)
          {
            token subtok = GetToken(Tokenizer);

            member_struct s;
            s.mType = type;
            s.mName = string(subtok.Text, subtok.TextLength);
            gCurrentEditing->mMembers.push_back(s);
          }
          else if (Token.Type == Token_Equals)
          { 
            //eg.
            //int a = 0, b = 0, c = 0;
            bool endparsemember = false;
            for (;;)
            {
              token t = GetToken(Tokenizer);
              if (t.Type == Token_Comma)
              {
                token subtok = GetToken(Tokenizer);

                member_struct s;
                s.mType = type;
                s.mName = string(subtok.Text, subtok.TextLength);
                gCurrentEditing->mMembers.push_back(s);
              }
              if (t.Type == Token_Semicolon)
              {
                foundmembername = false;
                endparsemember = true;
                break;
              }
              if (t.Type == Token_EndOfStream)
                break;
            }
            if (endparsemember)
              break;
          }
          else if (Token.Type == Token_Semicolon)
          {
            foundmembername = false;
            break;
          }
        }
    }
#else
    token Token = GetToken(Tokenizer);
    switch(Token.Type)
    {
        case Token_Asterisk:
        {
            ParseMember(Tokenizer, Token);
        } break;

        case Token_Identifier:
        {
            printf("DEBUG_VALUE(%.*s);\n", (int)Token.TextLength, Token.Text);
        } break;
    }
#endif
}

static void ParseStruct(tokenizer *Tokenizer)
{
    token structNameToken = GetToken(Tokenizer);
    meta_struct s;
    gIntrospectionData.push_back(s);
    gCurrentEditing = &gIntrospectionData[gIntrospectionData.size() - 1];
    gCurrentEditing->Name = string(structNameToken.Text, structNameToken.TextLength);

    if (RequireToken(Tokenizer, Token_Colon))
    {
      //printf("inheritance!");
      token t = GetToken(Tokenizer);
      if (TokenEquals(t, "public") || TokenEquals(t, "private"))
      {
        //printf("dont care\n");
        t = GetToken(Tokenizer);
      }
      printf("base class: %.*s\n", (int)t.TextLength, t.Text); //@note: multiple inheritance is banned from my code, dont bother with it
      gCurrentEditing->mBaseClass = string(t.Text, t.TextLength);
    }
    
    if(RequireToken(Tokenizer, Token_OpenBrace))
    {
        printf("member_definition MembersOf_%.*s[] = \n", (int)structNameToken.TextLength, structNameToken.Text);
        printf("{\n");
        for(;;)
        {
            token bodytoken = GetToken(Tokenizer);
            printf("%.*s\n", (int)bodytoken.TextLength, bodytoken.Text);
            if(bodytoken.Type == Token_CloseBrace)
                break;
            if (bodytoken.Type == Token_Identifier)
            {
              const char* ignored_keywords[] = { "public", "private", "inline", "explicit", "const" };
              int ignorecount = sizeof(ignored_keywords) / sizeof(const char*);
              bool ignore = false;
              for (int i = 0; i < ignorecount; ++i)
              {
                if (strncmp(bodytoken.Text, ignored_keywords[i], bodytoken.TextLength) == 0)
                {
                  ignore = true;
                  break;
                }
              }

              if (!ignore)
                ParseMember(Tokenizer, structNameToken, bodytoken);
            }
        }
        printf("};\n");

        
        
        /*meta_struct *Meta = (meta_struct *)malloc(sizeof(meta_struct));
        Meta->Name = (char *)malloc(structNameToken.TextLength + 1);
        memcpy(Meta->Name, structNameToken.Text, structNameToken.TextLength);
        Meta->Name[structNameToken.TextLength] = 0;
        Meta->Next = FirstMetaStruct;
        FirstMetaStruct = Meta;*/
    }

    gCurrentEditing = nullptr;
}

static void ParseIntrospectable(tokenizer *Tokenizer)
{
    if(RequireToken(Tokenizer, Token_OpenParen))
    {
        if(ParseIntrospectionParams(Tokenizer))
        {
            token TypeToken = GetToken(Tokenizer);
            if (TokenEquals(TypeToken, "struct") || TokenEquals(TypeToken, "class"))
            {
                ParseStruct(Tokenizer);
            }
            else
            {
                fprintf(stderr, "ERROR: Introspection is only supported for structs and classes right now :(\n");
            }
        }
    }
    else
    {
        fprintf(stderr, "ERROR: Missing parentheses.\n");
    }
}

int main(int ArgCount, char **Args)
{
    char *FileNames[] =
    {
        "Vector2.h",
    };
    for(int FileIndex = 0;
        FileIndex < (sizeof(FileNames)/sizeof(FileNames[0]));
        ++FileIndex)
    {
        char *FileContents = ReadEntireFileIntoMemoryAndNullTerminate(FileNames[FileIndex]);

        tokenizer Tokenizer = {};
        Tokenizer.At = FileContents;

        bool Parsing = true;
        while(Parsing)
        {
            token Token = GetToken(&Tokenizer);
            switch(Token.Type)
            {
                case Token_EndOfStream:
                {
                    Parsing = false;
                } break;

                case Token_Unknown:
                {
                } break;

                case Token_Identifier:
                {
                    if(TokenEquals(Token, "introspect"))
                    {
                        ParseIntrospectable(&Tokenizer);
                    }
                } break;
                        
                default:
                {
                  //printf("%d: %.*s\n", Token.Type, (int)Token.TextLength, Token.Text);
                } break;
            }
        }
    }

    printf("#define META_HANDLE_TYPE_DUMP(MemberPtr, NextIndentLevel) \\\n");
    /*for(meta_struct *Meta = FirstMetaStruct;
        Meta;
        Meta = Meta->Next)
    {
        printf("    case MetaType_%s: {DEBUGTextLine(Member->Name); DEBUGDumpStruct(ArrayCount(MembersOf_%s), MembersOf_%s, MemberPtr, (NextIndentLevel));} break; %s\n",
               Meta->Name, Meta->Name, Meta->Name,
               Meta->Next ? "\\" : "");
    }*/
    system("pause");
}
