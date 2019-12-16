
#ifndef _SCENEMANAGER_H
#define _SCENEMANAGER_H

#include "Prerequisites.h"
#include "DataManagement/FreeListPool.h"
#include "RenderSystem/SceneManager/SceneNode.h"

DECLARE_ENGINE_NAMESPACE

class IRenderer;
class RenderEngine;

class SceneManager
{
protected:
	friend class RenderEngine;
	SceneManager();
	virtual ~SceneManager();

public:
	/**
	 * Create a SceneNode
	 *
	 * \param name					Name of the new SceneNode
	 * \param parent				Parent of the new SceneNode (defaults to 0)
	 * \return						The newly created SceneNode
	 */
	SceneNode* CreateSceneNode(const String& name, SceneNode* parent = 0);
	/**
	 * Destroy a SceneNode
	 *
	 * \param name					Name of the SceneNode to be destroyed
	 */
	virtual void DestroySceneNode(const String& name);
	/**
	 * Get Root SceneNode pointer
	 *
	 * \return						The root node
	 */
	SceneNode* GetRootSceneNode() { return mRoot; }

	void DestroyAllSceneNodes();
	/**
	 * Destroy all nodes connected to root
	 */
	virtual void ClearSceneGraph();
	/**
	 * Get SceneNode pointer
	 *
	 * \param name					Name of the created SceneNode.
	 * \return						A SceneNode that is managed by this SceneManager
	 */
	virtual SceneNode* GetSceneNodeByName(const String& name);

	/**
	 * Rendering of Scenegraph
	 */
	virtual void Render();

	//Mempools
	FreeListPool<SceneNode>* GetSceneNodeMemPool() { return mSceneNodeMemPool; }
protected:
	SceneNode* mRoot;	//root node

	FreeListPool<SceneNode>				*mSceneNodeMemPool;
	SceneNode::NODEMAP			mSceneNodeList;	//List of all nodes in the scenegraph

	IRenderer* mRenderer; // pointer to renderer
};

END_ENGINE_NAMESPACE

#endif
