
#ifndef _CUIWIDGETS_H
#define _CUIWIDGETS_H

#include "EnginePrereqs.h"
#include "cUIElementBase.h"
#include "GraphicsManaged/ManagedGfxDefines.h"

/* 
!!! File contains widgets used in the game. Intentionally compressed since the number of gui widgets we can make is a lot
*/ 
ENGINE_NSP

class cSpriteGfxCmp;
class cTextGfxCmp;

/******************************************************************************/
/*!    UIImage widget. A simple image.  */
/******************************************************************************/
struct sUIImageParams
{
  OBJDESC_DEFINE_METADATA(sUIImageParams)
  {
    c->mCurrentVersion = 2;
    c->AddMember("RectTransform", &sUIImageParams::mTransform, 1);
    c->AddMember("MaterialName", &sUIImageParams::mMaterialName, 1);
    c->AddMember("TextureName", &sUIImageParams::mTextureName, 1);
    c->AddMember("AnimRangeSetName", &sUIImageParams::mAnimRangeSetName, 1);
    c->AddMember("Layer", &sUIImageParams::mLayer, 1);
    c->AddMember("Visible", &sUIImageParams::mVisible, 1);
    c->AddMember("ResponseInset", &sUIImageParams::mResponseInset, 3);
    c->AddMember("ResponseOffset", &sUIImageParams::mResponseOffset, 3);
    c->AddMember("NoInputDetect", &sUIImageParams::mNoInputDetection, 2);
  }

  sUIRectTransform mTransform;
  String mTextureName;
  String mMaterialName;
  String mAnimRangeSetName;
  Vector2 mResponseInset = Vector2(0, 0);
  Vector2 mResponseOffset = Vector2(0, 0);
  uint   mLayer = 5;
  bool   mVisible = true;
  bool   mNoInputDetection = false;
};

class cUIImage : public cUIElementBase
{
  OBJDESC_DEFINE_METADATA_VIRTUAL(cUIImage)
  {
    c->AddBase<cUIElementBase>();
    c->SetFactory(new TracklessFactory<cUIImage>(c, nullptr, MEMORY_SIZE_1));
    c->AddMethod(FACTORYINITFUNCNAME0, &cUIImage::Initialize);
  }
public:
  cUIImage();
  virtual ~cUIImage();

  virtual bool Initialize(const String& name, const sUIImageParams& params);
  virtual void Destroy();

  void Update(float dt);
  void UpdateMtxTransform();

  virtual float GetWidth();
  virtual float GetHeight();

  cSpriteGfxCmp* GetSprite() { return mSpriteComponent; }
  void SetImage(const String& imagename);
  void ResizeForTooltip(float width, float height, bool center = false);

  void SetVisible(bool visible);
  bool IsVisible();

  bool GetResponsive() { return IsVisible() && cUIElementBase::GetResponsive(); }

  void MouseOvered() { }
  void MouseNotOver() { }
  void MouseDown() { }

  cSpriteGfxCmp* mSpriteComponent;
};


/******************************************************************************/
/*!    cUITextbox widget. A simple textbox */
/******************************************************************************/
struct sUITextParams
{
  OBJDESC_DEFINE_METADATA(sUITextParams)
  {
    c->mCurrentVersion = 5;
    c->AddMember("RectTransform", &sUITextParams::mTransform, 1);
    c->AddMember("Text", &sUITextParams::mText, 1);
    
    c->AddMemberStaticArray("TextID", &sUITextParams::mTextID, 64, 5);
    c->AddMember("Language", &sUITextParams::mLanguage, 5);

    c->AddMember("MaxWidthPixels", &sUITextParams::mMaxWidthPixels, 1);
    c->AddMember("Alignment", &sUITextParams::mAlignment, 1);
    c->AddMember("HorizontalOverflow", &sUITextParams::mHorizontalOverflow, 1);
    c->AddMember("FontSize", &sUITextParams::mFontSize, 1);
    c->AddMember("Layer", &sUITextParams::mLayer, 1);
    c->AddMember("Visible", &sUITextParams::mVisible, 1);
    c->AddMember("CanModify", &sUITextParams::mCanModifyText, 1);
    c->AddMember("NoInputDetect", &sUITextParams::mNoInputDetection, 2);
    c->AddMember("WorldUI", &sUITextParams::mWorldUI, 3);
    c->AddMember("MaxCharacters", &sUITextParams::mMaxCharacters, 2);
    c->AddMember("MinHeightPixels", &sUITextParams::mMinHeightPixels, 4);
  }

  sUIRectTransform mTransform;
  String mText;
  char mTextID[64] = { 0 };
  int mLanguage = 0; //@note: See cFont ->enum LOCALIZATION

  float mMaxWidthPixels = 1200.0f;
  TEXTALIGNMENT mAlignment = CENTER_ALIGNMENT;
  TEXTOVERFLOW mHorizontalOverflow = OV_WRAP;
  uint mFontSize = 36;
  uint mLayer = 5;
  bool mVisible = true;
  bool mCanModifyText = false;
  bool mNoInputDetection = false;
  bool mWorldUI = false;
  uint mMaxCharacters = UINT_MAX;
  float mMinHeightPixels = 36.f;
};

typedef void(*TextboxFocusCallback)(cUIElementBase*, void*);
typedef void(*TextboxDefocusCallback)(cUIElementBase*, void*);
typedef void(*TextboxSubmissionCallback)(cUIElementBase*, void*);

class cUITextbox : public cUIElementBase
{
  OBJDESC_DEFINE_METADATA_VIRTUAL(cUITextbox)
  {
    c->AddBase<cUIElementBase>();
    c->SetFactory(new TracklessFactory<cUITextbox>(c, nullptr, MEMORY_SIZE_2));
    c->AddMethod(FACTORYINITFUNCNAME0, &cUITextbox::Initialize);
  }
public:
  cUITextbox();
  virtual ~cUITextbox();

  bool Initialize(const String& name, const sUITextParams& params);
  void Destroy();

  void UpdateMtxTransform();
  void UpdateLocalizedText();

  virtual float GetWidth();
  virtual float GetHeight();
  Vector2 GetSpecificAABBOffset();

  void SetTextLocalized(uint hash);
  void SetTextLocalized(const char* ident);
  void SetText(const String& text);
  void SetVisible(bool visible);

  cTextGfxCmp* GetTextCmp() { return mTextCmp; }

  String GetText();
  bool IsVisible();
  bool GetResponsive() { return IsVisible() && cUIElementBase::GetResponsive(); }

  void AppendText(const String& str, bool appendnewline = true);

  static void TextboxEditMode(cUIElementBase*, void*);
  void MouseOvered() { }
  void MouseNotOver() { }
  void MouseDown() { }

  void SetFocusCallbackFunc(TextboxFocusCallback cb, void* userdata)
  { mFocusCB = cb; mFocusUserdata = userdata; }
  void SetDefocusCallbackFunc(TextboxDefocusCallback cb, void* userdata)
  { mDefocusCB = cb; mDefocusUserdata = userdata; }
  void SetSubmissionCallbackFunc(TextboxSubmissionCallback cb, void* userdata)
  { mSubmissionCB = cb; mSubmissionUserdata = userdata;}
  
protected:
  cTextGfxCmp* mTextCmp;
  bool mCanModifyText; 
  uint mMaxCharacters = UINT_MAX;
  float mMinHeightPixels = 36.f;
  uint mTextIDHash = 0;
protected:
  friend class cGUISystem;
  TextboxFocusCallback mFocusCB = nullptr;
  TextboxDefocusCallback mDefocusCB = nullptr;
  TextboxSubmissionCallback mSubmissionCB = nullptr;
  void* mDefocusUserdata = nullptr, *mSubmissionUserdata = nullptr, *mFocusUserdata = nullptr;
};

/******************************************************************************/
/*!    cUIButton widget. A simple button */
/******************************************************************************/
struct sUIButtonParams
{
  OBJDESC_DEFINE_METADATA(sUIButtonParams)
  {
    c->mCurrentVersion = 6;
    c->AddMember("RectTransform", &sUIButtonParams::mUITransform);
    c->AddMember("ResponseAreaScale", &sUIButtonParams::mResponseAreaScale);
    c->AddMember("MaterialName", &sUIButtonParams::mMaterialName);
    c->AddMember("TextureName", &sUIButtonParams::mTextureName);
    c->AddMember("AnimRangeSetName", &sUIButtonParams::mAnimRangeSetName);
    c->AddMember("Layer", &sUIButtonParams::mLayer);
    c->AddMember("Visible", &sUIButtonParams::mVisible);

    c->AddMember("Text", &sUIButtonParams::mText);

    c->AddMemberStaticArray("TextID", &sUIButtonParams::mTextID, 64, 6);
    c->AddMember("Language", &sUIButtonParams::mLanguage, 6);

    c->AddMember("TextAlignment", &sUIButtonParams::mAlignment);
    c->AddMember("FontSize", &sUIButtonParams::mFontSize);
    c->AddMember("TextInsetWidth", &sUIButtonParams::mTextInsetWidth, 2);
    c->AddMember("TextInsetHeight", &sUIButtonParams::mTextInsetHeight, 2);
    c->AddMember("ResponseInset", &sUIButtonParams::mResponseInset, 3);
    c->AddMember("ResponseOffset", &sUIButtonParams::mResponseOffset, 3);
    c->AddMember("WorldUI", &sUIButtonParams::mWorldUI, 4);
    c->AddMember("NoInputDetect", &sUIButtonParams::mNoInputDetection, 2);
    c->AddMember("Outline", &sUIButtonParams::mOutline, 5);
  }

  sUIRectTransform mUITransform;
  Vector2 mResponseAreaScale = Vector2::UNIT_SCALE;
  String mTextureName;
  String mMaterialName;
  String mAnimRangeSetName;
  uint   mLayer = 5;
  bool   mVisible = true;

  String mText;
  char mTextID[64] = { 0 };
  int mLanguage = 0; //@note: See cFont ->enum LOCALIZATION

  TEXTALIGNMENT mAlignment = CENTER_ALIGNMENT;
  uint mFontSize = 36;
  Vector2 mResponseInset = Vector2(0, 0);
  Vector2 mResponseOffset = Vector2(0, 0);
  uint mTextInsetWidth = 0, mTextInsetHeight = 0;
  bool mWorldUI = false;
  bool mNoInputDetection = false;
  bool mOutline = false;
};

class cUIButton : public cUIElementBase
{
  OBJDESC_DEFINE_METADATA_VIRTUAL(cUIButton)
  {
    c->AddBase<cUIElementBase>();
    c->SetFactory(new TracklessFactory<cUIButton>(c, nullptr, MEMORY_SIZE_2));
    c->AddMethod(FACTORYINITFUNCNAME0, &cUIButton::Initialize);
  }
public:
  cUIButton();
  virtual ~cUIButton();

  bool Initialize(const String& name, const sUIButtonParams& params);
  void Destroy();

  virtual float GetWidth();
  virtual float GetHeight();
  
  void Update(float dt);
  void UpdateMtxTransform();
  void UpdateLocalizedText();

  void SetLMBClickedCallbackFunc(InteractCallbackFunc func, void* userdata = nullptr)   { mLMBInteractCallbackFunc = func; mUserData[LMBINTERACT_USERDATA] = userdata; }
  void SetRMBClickedCallbackFunc(InteractCallbackFunc func, void* userdata = nullptr)   { mRMBInteractCallbackFunc = func; mUserData[RMBINTERACT_USERDATA] = userdata; }
  void SetMouseOverCallbackFunc(MouseOverCallbackFunc func, void* userdata = nullptr)   { mMouseOverCallbackFunc = func; mUserData[MOUSEOVER_USERDATA] = userdata; }
  void SetMouseEnterCallbackFunc(MouseEnterCallbackFunc func, void* userdata = nullptr) { mMouseEnterCallbackFunc = func; mUserData[MOUSEENTER_USERDATA] = userdata; }
  void SetMouseExitCallbackFunc(MouseExitCallbackFunc func, void* userdata = nullptr)   { mMouseExitCallbackFunc = func; mUserData[MOUSEEXIT_USERDATA] = userdata; }

  void SetTextLocalized(uint hash);
  void SetTextLocalized(const char* ident);
  void SetText(const String& text);
  String GetText();

  void SetVisible(bool visible);
  bool IsVisible();

  void SetImage(const String& imagename);

  cTextGfxCmp* GetTextCmp()  { return mTextCmp; }
  cSpriteGfxCmp* GetSprite() { return mSpriteComponent; }

protected:
  void MouseOvered();
  void MouseNotOver();
  void MouseDown();

protected:
  cTextGfxCmp*   mTextCmp;
  cSpriteGfxCmp* mSpriteComponent;
  uint mTextIDHash = 0;
};

/******************************************************************************/
/*!    cUISlider widget. A simple slider with a box shape as its body and mouseclick as input */
/******************************************************************************/
struct sUISliderParams
{
  OBJDESC_DEFINE_METADATA(sUISliderParams)
  {
    c->mCurrentVersion = 1;
    c->AddMember("RectTransform", &sUISliderParams::mTransform);
    c->AddMember("ResponseAreaScale", &sUISliderParams::mResponseAreaScale);
    c->AddMember("MaterialName", &sUISliderParams::mMaterialName);
    c->AddMember("TextureName", &sUISliderParams::mTextureName);
    c->AddMember("AnimRangeSetName", &sUISliderParams::mAnimRangeSetName);
    c->AddMember("Layer", &sUISliderParams::mLayer);
    c->AddMember("Visible", &sUISliderParams::mVisible);
    c->AddMember("MinVal", &sUISliderParams::mMinValue);
    c->AddMember("MaxVal", &sUISliderParams::mMaxValue);
  }

  sUIRectTransform mTransform;
  Vector2 mResponseAreaScale = Vector2::UNIT_SCALE;
  String mTextureName;
  String mMaterialName;
  String mAnimRangeSetName;
  uint   mLayer = 5;
  bool   mVisible = true;
  float mMinValue = 0.0f;
  float mMaxValue = 1.0f;
};

class cUISlider : public cUIElementBase
{
  OBJDESC_DEFINE_METADATA_VIRTUAL(cUISlider)
  {
    c->AddBase<cUIElementBase>();
    c->SetFactory(new TracklessFactory<cUISlider>(c, nullptr, MEMORY_SIZE_2));
    c->AddMethod(FACTORYINITFUNCNAME0, &cUISlider::Initialize);
  }
public:
  cUISlider();
  virtual ~cUISlider();

  bool Initialize(const String& name, const sUISliderParams& params);
  void Destroy();

  virtual float GetWidth();
  virtual float GetHeight();

  void Update(float dt);
  void UpdateMtxTransform();

  void SetLMBDownCallbackFunc(InteractCallbackFunc func, void* userdata = nullptr)      { mLMBDownCallbackFunc = func; mUserData[LMBDOWN_USERDATA] = userdata; }
  void SetLMBClickedCallbackFunc(InteractCallbackFunc func, void* userdata = nullptr)   { mLMBInteractCallbackFunc = func; mUserData[LMBINTERACT_USERDATA] = userdata; }
  void SetMouseEnterCallbackFunc(MouseEnterCallbackFunc func, void* userdata = nullptr) { mMouseEnterCallbackFunc = func; mUserData[MOUSEENTER_USERDATA] = userdata; }
  void SetMouseExitCallbackFunc(MouseExitCallbackFunc func, void* userdata = nullptr)   { mMouseExitCallbackFunc = func; mUserData[MOUSEEXIT_USERDATA] = userdata; }

  void SetVisible(bool visible);
  bool IsVisible();

  //cFontGfxCmp* GetFontObj()  { return mTextCmp; }
  cSpriteGfxCmp* GetSprite() { return mSpriteComponent; }

  float mMinValue = 0.0f;
  float mMaxValue = 1.0f;
protected:
  void MouseOvered();
  void MouseNotOver();
  void MouseDown();

protected:
  cSpriteGfxCmp* mSpriteComponent;
};

/******************************************************************************/
/*!    cUIEditableTextbox widget. A simple button */
/******************************************************************************/

END_NSP

#endif
