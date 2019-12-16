
#ifndef _SCENENODE_H
#define _SCENENODE_H

#include "Prerequisites.h"
#include "Math/Vector3.h"
#include "Math/Quaternion.h"
#include "Math/Matrix4.h"

#include <map>

DECLARE_ENGINE_NAMESPACE

class Vector3;
class Quaternion;
class Matrix4;
class SceneManager;
class VertexBuffer;

class SceneNode
{
public:
	SceneNode();
		/**
	 * Constructor for SceneNode
	 * \param _name						Unique name for scene node
	 * \param _parent					SceneNode pointer that is the parent of this scene node
	 * \param _sceneMan					Pointer to the SceneManager managing the scene
	 */
	virtual void Create(const String& _name, SceneNode* _parent, SceneManager* _sceneMan);
	virtual void Destroy();

	virtual ~SceneNode();
	/**
	 * Creating INodes and adding to childrenList
	 *
	 * @param name					Unique name for SceneNode
	 * @return						Created SceneNode
	 */
	virtual SceneNode* CreateChildSceneNode(const String& name);
	/**
	 * Deleting INodes from childrenList
	 * @param name					Name for SceneNode for deletion
	 */
	virtual void DestroyChildSceneNode(const String name);

	typedef std::map<String, SceneNode*> NODEMAP;

	/**************************/
	/** Transformation stuff **/
	/**************************/
	/**
	 * Set local position of node
	 * @param pos					Pointer of Object to detach
	 */
	void SetPosition(const Vector3& pos);
	/**
	 * Get local position of node
	 * @return 							Position of SceneNode
	 */
	inline const Vector3& GetPosition() const { return mPosition; }
	/**
	 * Set local orientation of node
	 * @param orient					Quaternion to set local orientation
	 */
	void SetOrientation(const Quaternion& orient);
	/**
	 * Get local orientation of node
	 * @return							Local orientation
	 */
	inline const Quaternion& GetOrientation() const { return mOrientation; }
	/**
	 * Set local scale factor of node
	 * @param factor					Vector to set scaling along different axes
	 */
	void SetScaleFactor(const Vector3& factor);
	/**
	 * Getting local scale factor of node
	 * @return							Local Scale
	 */
	inline const Vector3& GetScaleFactor() const { return mScale; }

	/* Get world position */
	inline const Vector3& GetDerivedPosition() const { return mDerivedPos; }
	/* Get world orientation */
	inline const Quaternion& GetDerivedOrientation() const { return mDerivedOrient; }
	/* Get world scale factor */
	inline const Vector3& GetDerivedScaleFactor() const { return mDerivedScale; }
	/* Get world transform */
	inline const Matrix4& GetDerivedTransformation() const { return mDerivedTransform; }

	/* Update derived transformation matrix */
	virtual void UpdateFromParent();

	void Render();

	inline const String& GetName() const { return mName; }
protected:
	
	Vector3 mPosition, mDerivedPos;
	Quaternion mOrientation, mDerivedOrient;
	Vector3 mScale, mDerivedScale;
	Matrix4 mDerivedTransform;

	SceneManager* sceneMan;	//pointer to SceneGraph instance managing current node

	String mName;	//unique name for node
protected:
	friend class SceneManager;
	SceneNode* mParent;
	SceneNode *firstChild, *lastChild, *nextSibling, *prevSibling;	//for tree traversal
	NODEMAP childrenList;

	//temp
	VertexBuffer *obj1_v_buf;
};

END_ENGINE_NAMESPACE

#endif
