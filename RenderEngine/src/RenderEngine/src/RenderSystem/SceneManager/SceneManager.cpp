
#include "RenderSystem/SceneManager/SceneManager.h"
#include "RenderSystem/SceneManager/SceneNode.h"
#include "RenderSystem/RenderingComponents/IRenderer.h"
#include "RenderSystem/RenderEngine.h"
#include <vector>
#include <exception>

DECLARE_ENGINE_NAMESPACE

SceneManager::SceneManager()
:mRoot(0)
{
	//Todo: Replace with objectmanager?
	mSceneNodeMemPool = new FreeListPool<SceneNode>(TABLE_LARGE_SIZE);
	mRenderer = RenderEngine::GetInstance()->GetRenderer();

	mRoot = CreateSceneNode("RootNode", 0);
}

SceneManager::~SceneManager()
{
	DestroyAllSceneNodes();
	SAFE_DELETE(mSceneNodeMemPool);
}

SceneNode* SceneManager::CreateSceneNode(const String& name, SceneNode* parent)
{
	SceneNode::NODEMAP::iterator iter = mSceneNodeList.find(name);
	if (iter != mSceneNodeList.end())	//already added!
		throw std::exception(": SceneNode with name \"\" already created.");

	SceneNode* node = GetSceneNodeMemPool()->NewInstance();
	mSceneNodeList.insert(std::make_pair(name, node));

	node->Create(name, parent, this);

	return node;
}

void SceneManager::DestroySceneNode(const String& name)
{
	SceneNode::NODEMAP::iterator iter = mSceneNodeList.find(name);
	if (iter == mSceneNodeList.end())
		throw std::exception(": No SceneNode with name \"" "\".");

	SceneNode* node = iter->second;
	node->Destroy();
	GetSceneNodeMemPool()->FreeInstance(node);

	mSceneNodeList.erase(iter);
}

SceneNode* SceneManager::GetSceneNodeByName(const String& name)
{
	SceneNode::NODEMAP::iterator iter = mSceneNodeList.find(name);
	if (iter == mSceneNodeList.end())	//cannot find
		throw std::exception(": SceneNode with name \"" "\" not found.");

	return iter->second;
}

void SceneManager::DestroyAllSceneNodes()
{
	while (mSceneNodeList.empty() == false)
		DestroySceneNode(mSceneNodeList.begin()->second->GetName());
}

void SceneManager::ClearSceneGraph()
{
	DestroySceneNode(mRoot->GetName());
	mRoot = CreateSceneNode("RootNode", 0);
}

void SceneManager::Render()
{
	mRenderer->ClearFrame(CLEAR_TARGET | CLEAR_ZBUFFER | CLEAR_STENCIL ,
								  0.0, 0.2, 0.8, 1.0, 1.0, 0);

	mRenderer->StartRendering();
	//mRenderer->LookAt(Vector3(), cam->GetDerivedLookAt(), cam->GetDerivedUpDirection());

	//!NOTE: Renderers require that lights be set up after setting view and
	//       before any other change to the modelview matrix!!!
	//SetLights();

	SceneNode *node = GetRootSceneNode();
	std::vector<SceneNode*> nodeList;

	while (node)
	{
		node->Render();
		nodeList.push_back(node);

		node = node->firstChild;

		if (node == 0)
		{
			node = nodeList.back();
			nodeList.pop_back();

			node = node->nextSibling;
			while (node == 0 && nodeList.empty() == false)
			{
				node = nodeList.back();
				nodeList.pop_back();

				node = node->nextSibling;
			}
		}
	}

	mRenderer->EndRendering();
}

END_ENGINE_NAMESPACE
