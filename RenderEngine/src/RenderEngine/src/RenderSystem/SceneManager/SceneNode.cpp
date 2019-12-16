
#include "RenderSystem/SceneManager/SceneNode.h"
#include "RenderSystem/SceneManager/SceneManager.h"
//temp
#include "RenderSystem/RenderingComponents/HardwareBufferManager.h"
#include "RenderSystem/RenderEngine.h"
#include "RenderSystem/RenderingComponents/IRenderer.h"

DECLARE_ENGINE_NAMESPACE

SceneNode::SceneNode()
	: mName(""), mParent(0), sceneMan(0),
	  firstChild(0), lastChild(0), nextSibling(0), prevSibling(0),
	  mPosition(0, 0, 0), mScale(1 ,1, 1)
{

}

SceneNode::~SceneNode()
{
	Destroy();
}

void SceneNode::Create(const String& _name, SceneNode* _parent, SceneManager* _sceneMan)
{
	mName = _name;
	mParent = _parent;
	sceneMan = _sceneMan;
	firstChild = 0;
	lastChild = 0;
	nextSibling = 0;
	prevSibling = 0;
	mPosition = mDerivedPos = Vector3::ZERO;
	mScale = mDerivedScale = Vector3::UNIT_SCALE;
	mOrientation = mDerivedOrient = Quaternion::IDENTITY;
	mDerivedTransform = Matrix4::IDENTITY;

	if (mParent)
	{
		mParent->childrenList.insert(std::make_pair(mName, this));
		if (!mParent->lastChild)
		{
			mParent->firstChild = this;
			mParent->lastChild = mParent->firstChild;
		}
		else
		{
			this->prevSibling = mParent->lastChild;
			mParent->lastChild->nextSibling = this;
			mParent->lastChild = this;
		}
	}

	UpdateFromParent();

	//Temporary
	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* obj1_vert_dec = hwbmgr->GenerateVertexDeclaration();
	obj1_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT3, 0);
	obj1_vert_dec->AddVertexElement(ELE_USG_COLOR, ELE_TYP_FLOAT4, 0);

	obj1_v_buf = hwbmgr->GenerateVertexBuffer(obj1_vert_dec, 3, HDW_BUF_USG_STATIC);
	Real obj1_data[][7] = {
		{ -1.0, 0, 0, 1.0, 0.0, 0.0, 1.0 },
		{ 0.0, 1.0, 0, 0.0, 1.0, 0.0, 1.0 },
		{ 1.0, 0, 0.0, 0.0, 0.0, 1.0, 1.0 }
	};
	obj1_v_buf->UpdateData(obj1_data, 0, 3);

}

void SceneNode::Destroy()
{
	if (prevSibling)
		prevSibling->nextSibling = nextSibling;
	if (nextSibling)
		nextSibling->prevSibling = prevSibling;
	if (mParent)
	{
		if (mParent->firstChild == this)
			mParent->firstChild = nextSibling;
		if (mParent->lastChild == this)
			mParent->lastChild = prevSibling;

		NODEMAP::iterator iter = mParent->childrenList.find(GetName());
		if (iter != mParent->childrenList.end())
		{
			mParent->childrenList.erase(iter);
		}
	}

	firstChild = 0;
	lastChild = 0;
	nextSibling = 0;
	prevSibling = 0;
	mParent = 0;

	//Destroy all children nodes
	while (childrenList.empty() == false)
	{
		sceneMan->DestroySceneNode(childrenList.begin()->second->GetName());
	}
}

SceneNode* SceneNode::CreateChildSceneNode(const String& name)
{
	assert(sceneMan);

	return sceneMan->CreateSceneNode(name, this);
}

void SceneNode::DestroyChildSceneNode(const String name)
{
	NODEMAP::iterator iter = childrenList.find(name);
	if (iter == childrenList.end())	//cannot find!
	{
		throw std::exception(": Child SceneNode with name ? cannot be found.");
	}

	sceneMan->DestroySceneNode(iter->second->GetName());
}


void SceneNode::SetPosition(const Vector3& pos)
{
	mPosition = pos;
	UpdateFromParent();
}

void SceneNode::SetOrientation(const Quaternion& orient)
{
	mOrientation = orient;
	UpdateFromParent();
}

void SceneNode::SetScaleFactor(const Vector3& factor)
{
	mScale = factor;
	UpdateFromParent();
}

void SceneNode::UpdateFromParent()
{
	mDerivedTransform = Matrix4::IDENTITY;
	mDerivedTransform.MakeTransform(GetPosition(),
								   GetScaleFactor(),
								   GetOrientation());
	if (mParent)
	{
		mDerivedTransform = mParent->GetDerivedTransformation() * mDerivedTransform;

		//derivedPos = parent->GetDerivedPosition() + position;
		mDerivedPos = mDerivedTransform.GetTrans();
		mDerivedScale = mParent->GetDerivedScaleFactor() * mScale;
		mDerivedOrient = mParent->GetDerivedOrientation() * mOrientation;
	}
	else
	{
		mDerivedPos = mPosition;
		mDerivedScale = mScale;
		mDerivedOrient = mOrientation;
	}

	for (NODEMAP::iterator iter=childrenList.begin(); iter!=childrenList.end(); ++iter)
		iter->second->UpdateFromParent();
}

void SceneNode::Render()
{
	IRenderer *renderer = RenderEngine::GetInstance()->GetRenderer();

	renderer->SetMatrix(WORLD_MATRIX, mDerivedTransform);
	obj1_v_buf->SetBuffer();

	renderer->DrawPrimitive(PRIMITIVE_TRI_LIST, 0, (obj1_v_buf->GetNumberOfVertices()/3));

	obj1_v_buf->UnsetBuffer();
}


END_ENGINE_NAMESPACE