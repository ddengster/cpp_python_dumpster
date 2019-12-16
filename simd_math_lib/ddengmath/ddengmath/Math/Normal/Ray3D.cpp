
#include "Ray3D.h"
#include "Matrix4.h"

void Ray3D::GenerateWorldRayFromProjectionMatrix(const Matrix4& proj, const Matrix4& view, float viewportwidth, float viewportheight, float clickpointx, float clickpointy)
{
  //not tested with opengl
  //the windows api uses 0 to viewportwidth inclusive, so it's a slightly different form of truncation
  //use number line technique to see whether they are correct
  float ndc_x = ( 2.0f * clickpointx / viewportwidth) - 1.0f;
  float ndc_y = (-2.0f * clickpointy / viewportheight) + 1.0f;
  float ndc_z = 1.0f; //assumes near plane at 1.0f

  //scale by aspect ratio to get coordinates of click point on view frame in view frame
  ndc_x /=  proj.m_2d[0][0];
  ndc_y /=  proj.m_2d[1][1];
  
  Matrix4 viewinverse(view.InverseCopy());

  mOrigin = Vector3(viewinverse.m_2d[0][3], viewinverse.m_2d[1][3], viewinverse.m_2d[2][3]);
  mDirection = Vector3(ndc_x * viewinverse.m_2d[0][0] + ndc_y * viewinverse.m_2d[0][1] + ndc_z * viewinverse.m_2d[0][2],
                       ndc_x * viewinverse.m_2d[1][0] + ndc_y * viewinverse.m_2d[1][1] + ndc_z * viewinverse.m_2d[1][2],
                       ndc_x * viewinverse.m_2d[2][0] + ndc_y * viewinverse.m_2d[2][1] + ndc_z * viewinverse.m_2d[2][2]);
}

#if 0 //pseudocode for camera implementations
void Ray3D::GenerateFromCamera(Camera* cam, float clickpointx, float clickpointy)
{
  //get ndc
  float ndc_x = ( 2.0f * clickpointx / viewportwidth) - 1.0f;
  float ndc_y = (-2.0f * clickpointy / viewportheight) + 1.0f;
  
  //do aspect ratio adjustment
  float radians = cam->GetFOV() * PI / 180;
  float tangent = std::tan(radians * 0.5f); 
  ndc_x *= cam->GetNearDist() * tangent * cam->GetAspect();
  ndc_y *= cam->GetNearDist() * tangent;

  pick.z = -cam->GetNearDist();
  pick.w = 1.0f;

  //construct view to world frame matrix
  Matrix4 g(ConstructWorldFrameMatrix(cam));
  Vector4 worldframepick = g * pick;

  mOrigin = gfxVector3(worldframepick.x, worldframepick.y, worldframepick.z);
  mDirection = raystart - cam->GetPosition();
  mDirection.Normalize();
}

#endif
