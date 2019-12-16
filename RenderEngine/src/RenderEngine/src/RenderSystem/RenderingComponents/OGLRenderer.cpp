
#include "RenderSystem/RenderingComponents/OGLRenderer.h"
#include "RenderSystem/RenderEngine.h"
#include "RenderSystem/RenderingComponents/HardwareBufferManager.h"

#include "LogManager/LogManager.h"
#include "Math/Vector3.h"

DECLARE_ENGINE_NAMESPACE

OGLRenderer::OGLRenderer()
:mWindowHandle(NULL), mhDC(NULL), mhRC(NULL), mModelMatrix(Matrix4::IDENTITY), mViewMatrix(Matrix4::IDENTITY)
{
}

OGLRenderer::~OGLRenderer()
{
	Shutdown();
}

bool OGLRenderer::Init()
{
	mWindowHandle = (HWND)RenderEngine::GetInstance()->GetRenderSetup().window_handle;
	
	static PIXELFORMATDESCRIPTOR pfd =				// pfd Tells Windows How We Want Things To Be
	{
		sizeof(PIXELFORMATDESCRIPTOR),				// Size Of This Pixel Format Descriptor
		1,											// Version Number
		PFD_DRAW_TO_WINDOW |						// Format Must Support Window
		PFD_SUPPORT_OPENGL |						// Format Must Support OpenGL
		PFD_DOUBLEBUFFER,							// Must Support Double Buffering
		PFD_TYPE_RGBA,								// Request An RGBA Format
		32,											// Select Our Color Depth
		0, 0, 0, 0, 0, 0,							// Color Bits Ignored
		0,											// No Alpha Buffer
		0,											// Shift Bit Ignored
		0,											// No Accumulation Buffer
		0, 0, 0, 0,									// Accumulation Bits Ignored
		16,											// 16Bit Z-Buffer (Depth Buffer)  
		4,											// No Stencil Buffer
		0,											// No Auxiliary Buffer
		PFD_MAIN_PLANE,								// Main Drawing Layer
		0,											// Reserved
		0, 0, 0										// Layer Masks Ignored
	};
	
	if (!( mhDC = GetDC(mWindowHandle) ))							// Did We Get A Device Context?
	{
		throw std::exception("Can't Create A GL Device Context.");
		return false;
	}

	int	PixelFormat;
	PixelFormat = ChoosePixelFormat(mhDC, &pfd);	// Did Windows Find A Matching Pixel Format?
	SetPixelFormat(mhDC, PixelFormat, &pfd);		// Are We Able To Set The Pixel Format?
	mhRC = wglCreateContext(mhDC);				// Are We Able To Get A Rendering Context?
	wglMakeCurrent(mhDC, mhRC);		

	glDisable(GL_CULL_FACE);
	glCullFace(GL_BACK);
	glFrontFace(GL_CCW);
	GLfloat ambientLight[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambientLight);

	if (CheckCompatibility())
	{
		LogManager::GetInstance()->AppendToLog("[OGLRenderer] Initialised successfully");
		return true;
	}
	return false;
}

bool OGLRenderer::CheckCompatibility()
{
	//Unused at the moment
	/*if (GL_ARB_vertex_buffer_object)
	{
		m_VBOSupported = true;
	}
	else
	{
		LogManager::GetInstance()->AppendToLog("[OGLRenderer] Vertex Buffer Object not supported.");
	}

	if (GL_ARB_occlusion_query)
	{
		m_OcclusionSupported = true;
		glGenQueries(1, &m_queryID);
	}
	else
	{
		LogManager::GetInstance()->AppendToLog("[OGLRenderer] Occlusion query not supported");
	}*/
	return true;
}

void OGLRenderer::Shutdown()
{
	wglMakeCurrent(mhDC,NULL);
	wglDeleteContext(mhRC);
}

void OGLRenderer::StartRendering()
{
}

void OGLRenderer::EndRendering()
{
	SwapBuffers(mhDC);
}

void OGLRenderer::ClearFrame(uint32_t target, Real r, Real g, Real b, Real a, Real z, uint32_t s)
{
	GLbitfield flags = 0;

	if (target|CLEAR_TARGET)
	{
		flags |= GL_COLOR_BUFFER_BIT;
		glClearColor(r, g, b, a);
	}

	if (target|CLEAR_ZBUFFER)
	{
		flags |= GL_DEPTH_BUFFER_BIT;
		glClearDepth(z);
	}
	if (target|CLEAR_STENCIL)
	{
		flags |= GL_STENCIL_BUFFER_BIT;
		glClearStencil(s);
	}

	glClear(flags);
}

bool OGLRenderer::SetRenderState(RENDERSTATE renderstate, uint32_t value)
{
	switch(renderstate)
	{
		case RS_SHADING:
			{
				if (value == SHADEMODE_FLAT)
					glShadeModel(GL_FLAT);
				else if (value == SHADEMODE_SMOOTH)
					glShadeModel(GL_SMOOTH);
				else
					return false;
				break;
			}
		case RS_DEPTH_TESTING:
			{
				if (value == DEPTHMODE_DISABLE)
					glDisable(GL_DEPTH_TEST);
				else if (value == DEPTHMODE_ENABLE)
					glEnable(GL_DEPTH_TEST);
				else 
					return false;
				break;
			}
		case RS_CULLING:
			{
				switch(value)
				{
					case CULLMODE_DISABLE:
						{
							glDisable(GL_CULL_FACE);
							break;
						}
					case CULLMODE_CW:
						{
							glEnable(GL_CULL_FACE);
							glFrontFace(GL_CCW);
							break;
						}
					case CULLMODE_CCW:
						{
							glEnable(GL_CULL_FACE);
							glFrontFace(GL_CW);
							break;
						}
					default:
						return false;
						break;
				}
				break;
			}
		case RS_FILLMODE:
			{
				switch(value)
				{
					case FILLMODE_POINT:
						glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
						break;
					case FILLMODE_WIREFRAME:
						glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
						break;
					case FILLMODE_SOLID:
						glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
						break;
					default:
						return false;
				}
				break;
			}
		case RS_LIGHTING:
			{
				if (value == LIGHTING_DISABLE)
					glDisable(GL_LIGHTING);
				else if (value == LIGHTING_ENABLE)
					glEnable(GL_LIGHTING);
				else
					return false;
				break;
			}
		case RS_BLENDING:
			{
				if (value == BLENDING_DISABLE)
					glDisable(GL_BLEND);
				else if (value == BLENDING_ENABLE)
					glEnable(GL_BLEND);
				else
					return false;
				break;
			}
		case RS_ALPHA_TESTING:
			{
				if (value == ALPHA_TESTING_ENABLE)
				{
					glEnable(GL_ALPHA_TEST);
					glAlphaFunc(GL_GREATER, 0.4);
				}
				else if (value == ALPHA_TESTING_DISABLE)
				{
					glDisable(GL_ALPHA_TEST);
				}
				break;
			}
		case RS_TEXTURE_2D:
			{
				if (value == TEXTURE_2D_DISABLE)
					glDisable(GL_TEXTURE_2D);
				else if (value == TEXTURE_2D_ENABLE)
					glEnable(GL_TEXTURE_2D);
				else
					return false;
				break;
			}
		default:
			return false;
	}
	return true;
}

void OGLRenderer::SetViewPort(Real x, Real y, Real width, Real height)
{
	glViewport(x, y, width, height);
}

void OGLRenderer::SetPerspective(Real fov, Real nearDist, Real farDist, int32_t width, int32_t height)
{
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	gluPerspective(fov, (GLfloat) width/(GLfloat) height,(Real) nearDist, (Real) farDist);
	
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
}

void OGLRenderer::SetOrtho(int32_t left, int32_t right, int32_t bottom, int32_t top, Real nearDist, Real farDist)
{
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	
	glOrtho(left, right, bottom, top, nearDist, farDist);
	
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
}

//Naming needs to be changed such that lookat is NOT the point the camera looks at, but rather the direction the camera
//is facing
void OGLRenderer::LookAt(const Vector3& pos, const Vector3& lookat, const Vector3& up)
{
	//gluLookAt no longer used as we need to get the view matrix.
	//update view matrix
	//Uncertainties: glLoadMatrixf resets the current matrix type; so screw ups might be possible when
	//called in the middle of the loop.
	//http://www.gamedev.net/community/forums/topic.asp?topic_id=421529 for more info.
	Vector3 dirN;
	Vector3 upN;
	Vector3 rightN;

	dirN = lookat;
	dirN.Normalise();

	upN = up;
	upN.Normalise();

	rightN = dirN.CrossProduct(upN);
	rightN.Normalise();

	upN = rightN.CrossProduct(dirN);
	upN.Normalise();

	mViewMatrix[0][0] = rightN.x;	mViewMatrix[0][1] = upN.x;	mViewMatrix[0][2] = -dirN.x;	mViewMatrix[0][3] = 0.0;
	mViewMatrix[1][0] = rightN.y;	mViewMatrix[1][1] = upN.y;	mViewMatrix[1][2] = -dirN.y;	mViewMatrix[1][3] = 0.0;
	mViewMatrix[2][0] = rightN.z;	mViewMatrix[2][1] = upN.z;	mViewMatrix[2][2] = -dirN.z;	mViewMatrix[2][3] = 0.0;

	mViewMatrix[3][0] = -(rightN.DotProduct(pos));
	mViewMatrix[3][1] = -(upN.DotProduct(pos));
	mViewMatrix[3][2] = (dirN.DotProduct(pos));
	mViewMatrix[3][3] = 1.0;


	glMatrixMode(GL_MODELVIEW);
	glLoadMatrixf(mViewMatrix[0]);

	mViewMatrix = mViewMatrix.Transpose();
}

void OGLRenderer::GetMatrix(MATRIX_TYPE type, Matrix4 *matrix)
{
	if (type >= NUM_MATRICES || type < 0)
	{
		LogManager::GetInstance()->AppendToLog("[OGLRenderer] Invalid matrix mode");
		return;
	}

	switch(type)
	{
	case WORLD_MATRIX:
		{
			*matrix = mModelMatrix;
			return;
		}
	case VIEW_MATRIX:
		{
			*matrix = mViewMatrix;
			return;
		}
	case PROJECTION_MATRIX:
		{
			Matrix4	mat;
			glGetFloatv(GL_PROJECTION_MATRIX, mat[0]);
			*matrix = mat.Transpose();
			return;
		}
	}
}

void OGLRenderer::SetMatrix(MATRIX_TYPE type, const Matrix4& matrix)
{
	if (type >= NUM_MATRICES || type < 0)
	{
		LogManager::GetInstance()->AppendToLog("[OGLRenderer] Invalid matrix mode");
		return;
	}
	/*glMatrixMode(GL_MODELVIEW);
	glMultMatrixf(matrix.Transpose()[0]);
	return
	glMatrixMode(GL_MODELVIEW);
	glLoadMatrixf(matrix.Transpose()[0]);
	return;*/

	switch(type)
	{
	case WORLD_MATRIX:
		{
			mModelMatrix = matrix;
			Matrix4	mdl_view;
			mdl_view = mViewMatrix*mModelMatrix;

			glMatrixMode(GL_MODELVIEW);
			glLoadMatrixf(mdl_view.Transpose()[0]);

			break;
		}
	case VIEW_MATRIX:
		{
			mViewMatrix = matrix;
			Matrix4	mdl_view;
			mdl_view = mViewMatrix*mModelMatrix;

			glMatrixMode(GL_MODELVIEW);
			glLoadMatrixf(mdl_view.Transpose()[0]);

			break;
		}
	case PROJECTION_MATRIX:
		{
			glMatrixMode(GL_PROJECTION);
			glLoadMatrixf(matrix.Transpose()[0]);
			break;
		}
	}
}

void OGLRenderer::DrawPrimitive(PRIMITIVE_TYPE type, uint32_t start, uint32_t polycount)
{
	//glDrawArrays takes in the number of vertices as its last parameter, so we need to convert it
	glDrawArrays(_ConvertToGLPrimitiveType(type), start, _CalculatePrimitiveIndexCount(type, polycount));
}

void OGLRenderer::DrawIndexedPrimitive(PRIMITIVE_TYPE type, uint32_t start, uint32_t polycount)
{
	//IndexBuffer	*cur_index_buf = RenderEngine::GetInstance()->GetHardwareBufferManager()->GetCurrentIndexBuffer();
	IndexBuffer	*cur_index_buf = HardwareBufferManager::GetInstance()->GetCurrentIndexBuffer();
	uint32_t	idx_s = cur_index_buf->GetIndexSize();

	//glDrawElements takes in the number of indices as its 2nd parameter, so we need to convert it
	glDrawElements(	_ConvertToGLPrimitiveType(type), _CalculatePrimitiveIndexCount(type, polycount),
					(idx_s == 2? GL_UNSIGNED_SHORT : GL_UNSIGNED_INT), (void*)(start*idx_s));
}

/*
TODO
void OGLRenderer::SetLight(uint32_t index, const SceneLight* light)
{
	
	uint32_t lightIndex = GL_LIGHT0 + index;

	if (light)
	{
		glLightfv(lightIndex, GL_AMBIENT, &light->GetAmbientRGBA().x);
		glLightfv(lightIndex, GL_DIFFUSE, &light->GetDiffuseRGBA().x);
		glLightfv(lightIndex, GL_SPECULAR, &light->GetSpecularRGBA().x);

		// OpenGL determines light type based on the w-coord in position
		switch (light->GetType())
		{
			case SceneLight::LIGHT_DIRECTIONAL:
				{
					float dir[4] = { light->GetDerivedDirection().x, light->GetDerivedDirection().y, light->GetDerivedDirection().z, 0.0f };
					glLightfv(lightIndex, GL_POSITION, dir);
				}
				break;
			case SceneLight::LIGHT_POSITIONAL:
			case SceneLight::LIGHT_SPOTLIGHT:
				{
					float pos[4] = { light->GetDerivedPosition().x, light->GetDerivedPosition().y, light->GetDerivedPosition().z, 1.0f };
					glLightfv(lightIndex, GL_POSITION, pos);

					glLightfv(lightIndex, GL_SPOT_DIRECTION, &light->GetDerivedDirection().x);
				}
				break;
				//TODO: What about LIGHT_NONE?
		}

		glLightf(lightIndex, GL_SPOT_EXPONENT, light->GetSpotExponent());
		glLightf(lightIndex, GL_SPOT_CUTOFF, light->GetCutoffAngle().ValueDegrees());
		glLightf(lightIndex, GL_CONSTANT_ATTENUATION, light->GetConstantAttenuation());
		glLightf(lightIndex, GL_LINEAR_ATTENUATION, light->GetLinearAttenuation());
		glLightf(lightIndex, GL_QUADRATIC_ATTENUATION, light->GetQuadraticAttenuation());

		glEnable(lightIndex);
	}
	else
	{
		glDisable(lightIndex);
	}
	
}*/
/*
TODO
void OGLRenderer::SetBasicMaterial(const Material& mat)
{
	
	// OpenGL is RGBA
	GLfloat glColorValue[4];
	glColorValue[0] = mat.ambient.r;
	glColorValue[1] = mat.ambient.g;
	glColorValue[2] = mat.ambient.b;
	glColorValue[3] = mat.ambient.a;

	glMaterialfv(GL_FRONT, GL_AMBIENT, glColorValue);

	glColorValue[0] = mat.diffuse.r;
	glColorValue[1] = mat.diffuse.g;
	glColorValue[2] = mat.diffuse.b;
	glColorValue[3] = mat.diffuse.a;

	glMaterialfv(GL_FRONT, GL_DIFFUSE, glColorValue);

	glColorValue[0] = mat.specular.r;
	glColorValue[1] = mat.specular.g;
	glColorValue[2] = mat.specular.b;
	glColorValue[3] = mat.specular.a;

	glMaterialfv(GL_FRONT, GL_SPECULAR, glColorValue);

	glColorValue[0] = mat.emissive.r;
	glColorValue[1] = mat.emissive.g;
	glColorValue[2] = mat.emissive.b;
	glColorValue[3] = mat.emissive.a;

	glMaterialfv(GL_FRONT, GL_EMISSION, glColorValue);

	glMaterialf(GL_FRONT, GL_SHININESS, mat.shininess);
	
}
*/
GLenum OGLRenderer::_ConvertToGLPrimitiveType(PRIMITIVE_TYPE type)
{
	static GLenum	table[NUM_PRIMITIVES] = {	GL_POINTS,
												GL_TRIANGLES,
												GL_TRIANGLE_STRIP,
												GL_TRIANGLE_FAN,
												GL_LINES,
												GL_LINE_STRIP
												//TODO: QUAD and POLY LISTS
	};

	return table[type];
}


uint32_t OGLRenderer::_CalculatePrimitiveIndexCount(PRIMITIVE_TYPE type, uint32_t count)
{
	uint32_t index_count = 0;
	// OpenGL uses the number of the indices instead of number of primitives
	// need to convert to index number;
	switch(type)
	{
	case PRIMITIVE_POINT_LIST:
		index_count = count;
		break;
	case PRIMITIVE_TRI_LIST:
		index_count = count*3;
		break;
	case PRIMITIVE_TRI_STRIP:
		index_count = count + 2;
		break;
	case PRIMITIVE_LINE_LIST:
		index_count = count*2;
		break;
	case PRIMITIVE_LINE_STRIP:
		index_count = count + 1;
		break;
	}

	return index_count;
}

END_ENGINE_NAMESPACE