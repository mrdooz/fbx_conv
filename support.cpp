#include "stdafx.h"
#include "support.hpp"

using namespace std;

void get_aspect_and_fov(FbxCamera *camera, double *fov_x, double *fov_y, double *aspect) {
  // crazy amount of code to get the aspect ratio and fov
  FbxCamera::EAspectRatioMode lCamAspectRatioMode = camera->GetAspectRatioMode();
  double lAspectX = camera->AspectWidth.Get();
  double lAspectY = camera->AspectHeight.Get();
  double lAspectRatio = 1.333333;
  switch( lCamAspectRatioMode)
  {
  case FbxCamera::eWindowSize:
    lAspectRatio = lAspectX / lAspectY;
    break;
  case FbxCamera::eFixedRatio:
    lAspectRatio = lAspectX;

    break;
  case FbxCamera::eFixedResolution:
    lAspectRatio = lAspectX / lAspectY * camera->GetPixelRatio();
    break;
  case FbxCamera::eFixedWidth:
    lAspectRatio = camera->GetPixelRatio() / lAspectY;
    break;
  case FbxCamera::eFixedHeight:
    lAspectRatio = camera->GetPixelRatio() * lAspectX;
    break;
  default:
    break;
  }

  //get the aperture ratio
  double lFilmHeight = camera->GetApertureHeight();
  double lFilmWidth = camera->GetApertureWidth() * camera->GetSqueezeRatio();
  //here we use Height : Width
  double lApertureRatio = lFilmHeight / lFilmWidth;


  //change the aspect ratio to Height : Width
  lAspectRatio = 1 / lAspectRatio;
  //revise the aspect ratio and aperture ratio
  FbxCamera::EGateFit cameraGateFit = camera->GateFit.Get();
  switch( cameraGateFit )
  {

  case FbxCamera::eFitFill:
    if( lApertureRatio > lAspectRatio)  // the same as eHORIZONTAL_FIT
    {
      lFilmHeight = lFilmWidth * lAspectRatio;
      camera->SetApertureHeight( lFilmHeight);
      lApertureRatio = lFilmHeight / lFilmWidth;
    }
    else if( lApertureRatio < lAspectRatio) //the same as eVERTICAL_FIT
    {
      lFilmWidth = lFilmHeight / lAspectRatio;
      camera->SetApertureWidth( lFilmWidth);
      lApertureRatio = lFilmHeight / lFilmWidth;
    }
    break;
  case FbxCamera::eFitVertical:
    lFilmWidth = lFilmHeight / lAspectRatio;
    camera->SetApertureWidth( lFilmWidth);
    lApertureRatio = lFilmHeight / lFilmWidth;
    break;
  case FbxCamera::eFitHorizontal:
    lFilmHeight = lFilmWidth * lAspectRatio;
    camera->SetApertureHeight( lFilmHeight);
    lApertureRatio = lFilmHeight / lFilmWidth;
    break;
  case FbxCamera::eFitStretch:
    lAspectRatio = lApertureRatio;
    break;
  case FbxCamera::eFitOverscan:
    if( lFilmWidth > lFilmHeight)
    {
      lFilmHeight = lFilmWidth * lAspectRatio;
    }
    else
    {
      lFilmWidth = lFilmHeight / lAspectRatio;
    }
    lApertureRatio = lFilmHeight / lFilmWidth;
    break;
  case FbxCamera::eFitNone:
  default:
    break;
  }
  //change the aspect ratio to Width : Height
  lAspectRatio = 1 / lAspectRatio;

#define HFOV2VFOV(h, ar) (2.0 * atan((ar) * tan( (h * FBXSDK_PI_DIV_180) * 0.5)) * FBXSDK_180_DIV_PI) //ar : aspectY / aspectX
#define VFOV2HFOV(v, ar) (2.0 * atan((ar) * tan( (v * FBXSDK_PI_DIV_180) * 0.5)) * FBXSDK_180_DIV_PI) //ar : aspectX / aspectY


  double lFieldOfViewX = 0.0;
  double lFieldOfViewY = 0.0;
  if ( camera->GetApertureMode() == FbxCamera::eVertical)
  {
    lFieldOfViewY = camera->FieldOfView.Get();
    lFieldOfViewX = VFOV2HFOV( lFieldOfViewY, 1 / lApertureRatio);
  }
  else if (camera->GetApertureMode() == FbxCamera::eHorizontal)
  {
    lFieldOfViewX = camera->FieldOfView.Get(); //get HFOV
    lFieldOfViewY = HFOV2VFOV( lFieldOfViewX, lApertureRatio);
  }
  else if (camera->GetApertureMode() == FbxCamera::eFocalLength)
  {
    lFieldOfViewX = camera->ComputeFieldOfView(camera->FocalLength.Get());    //get HFOV
    lFieldOfViewY = HFOV2VFOV( lFieldOfViewX, lApertureRatio);
  }
  else if (camera->GetApertureMode() == FbxCamera::eHorizAndVert) {
    lFieldOfViewX = camera->FieldOfViewX.Get();
    lFieldOfViewY = camera->FieldOfViewY.Get();
  }

  *aspect = lAspectRatio;
  *fov_x = lFieldOfViewX;
  *fov_y = lFieldOfViewY;
}

/*
void optimize_spline_fit(int dimension, int num_samples, AnimType *sample_data, AnimType err) {
  using namespace Wm5;
  int lo = 5;
  int hi = num_samples;
  int cur = 10;
  while (lo < hi) {
    BSplineCurveFit<AnimType> fitter(dimension, num_samples, sample_data, 3, cur);

    // compute RMSD
    AnimType d = 0;
    for (int i = 0; i < num_samples; ++i) {
      AnimType v[4];
      fitter.GetPosition((AnimType)i/(num_samples-1), v);
      AnimType e = 0;
      for (int j = 0; j < dimension; ++j) {
        AnimType c = sample_data[i*dimension+j];
        e += (c - v[j]) * (c - v[j]);
      }
      d += e;
    }
    AnimType cur_err = sqrt(d/num_samples);
    if (cur_err < err) {
      hi = cur;
    } else {
      if (lo == cur) {
        cur = hi;
        break;
      }
      lo = cur;
    }
    cur = (lo + hi) / 2;
  }
}
*/
FbxDouble3 GetMaterialProperty(const FbxSurfaceMaterial *pMaterial, const char *pPropertyName, const char *pFactorPropertyName, string *filename)
{
  FbxDouble3 lResult(0, 0, 0);
  const FbxProperty lProperty = pMaterial->FindProperty(pPropertyName);
  const FbxProperty lFactorProperty = pMaterial->FindProperty(pFactorPropertyName);
  if (lProperty.IsValid() && lFactorProperty.IsValid()) {
    lResult = lProperty.Get<FbxDouble3>();
    double lFactor = lFactorProperty.Get<FbxDouble>();
    if (lFactor != 1) {
      lResult[0] *= lFactor;
      lResult[1] *= lFactor;
      lResult[2] *= lFactor;
    }
  }

  // get the filename
  if (filename && lProperty.IsValid()) {
    if (int lTextureCount = lProperty.GetSrcObjectCount(FbxFileTexture::ClassId)) {
      if (const FbxFileTexture* lTexture = lProperty.GetSrcObject(FBX_TYPE(FbxFileTexture), 0)) {
        *filename = lTexture->GetFileName();
      }
    }
  }

  return lResult;
}


// Get the geometry offset to a node. It is never inherited by the children.
FbxAMatrix GetGeometry(FbxNode* pNode)
{
  const FbxVector4 lT = pNode->GetGeometricTranslation(FbxNode::eSourcePivot);
  const FbxVector4 lR = pNode->GetGeometricRotation(FbxNode::eSourcePivot);
  const FbxVector4 lS = pNode->GetGeometricScaling(FbxNode::eSourcePivot);

  return FbxAMatrix(lT, lR, lS);
}

void print_matrix(const FbxAMatrix &mtx) {
  char buf[512];
  sprintf(buf, "[ %.5f %.5f %.5f %.5f ]\n[ %.5f %.5f %.5f %.5f ]\n[ %.5f %.5f %.5f %.5f ]\n[ %.5f %.5f %.5f %.5f ]\n", 
    mtx.Get(0, 0), mtx.Get(0, 1), mtx.Get(0, 2), mtx.Get(0, 3),
    mtx.Get(1, 0), mtx.Get(1, 1), mtx.Get(1, 2), mtx.Get(1, 3),
    mtx.Get(2, 0), mtx.Get(2, 1), mtx.Get(2, 2), mtx.Get(2, 3),
    mtx.Get(3, 0), mtx.Get(3, 1), mtx.Get(3, 2), mtx.Get(3, 3));
  OutputDebugStringA(buf);
}


FbxAMatrix GetPoseMatrix(FbxPose* pPose, int pNodeIndex);
FbxAMatrix GetGlobalPosition(FbxNode* pNode, const FbxTime& pTime, FbxPose* pPose, FbxAMatrix* pParentGlobalPosition)
{
  FbxAMatrix lGlobalPosition;
  bool        lPositionFound = false;

  if (pPose)
  {
    int lNodeIndex = pPose->Find(pNode);

    if (lNodeIndex > -1)
    {
      // The bind pose is always a global matrix.
      // If we have a rest pose, we need to check if it is
      // stored in global or local space.
      if (pPose->IsBindPose() || !pPose->IsLocalMatrix(lNodeIndex))
      {
        lGlobalPosition = GetPoseMatrix(pPose, lNodeIndex);
      }
      else
      {
        // We have a local matrix, we need to convert it to
        // a global space matrix.
        FbxAMatrix lParentGlobalPosition;

        if (pParentGlobalPosition)
        {
          lParentGlobalPosition = *pParentGlobalPosition;
        }
        else
        {
          if (pNode->GetParent())
          {
            lParentGlobalPosition = GetGlobalPosition(pNode->GetParent(), pTime, pPose);
          }
        }

        FbxAMatrix lLocalPosition = GetPoseMatrix(pPose, lNodeIndex);
        lGlobalPosition = lParentGlobalPosition * lLocalPosition;
      }

      lPositionFound = true;
    }
  }

  if (!lPositionFound)
  {
    // There is no pose entry for that node, get the current global position instead.

    // Ideally this would use parent global position and local position to compute the global position.
    // Unfortunately the equation 
    //    lGlobalPosition = pParentGlobalPosition * lLocalPosition
    // does not hold when inheritance type is other than "Parent" (RSrs).
    // To compute the parent rotation and scaling is tricky in the RrSs and Rrs cases.
    lGlobalPosition = pNode->EvaluateGlobalTransform(pTime);
  }

  return lGlobalPosition;
}

// Get the matrix of the given pose
FbxAMatrix GetPoseMatrix(FbxPose* pPose, int pNodeIndex)
{
  FbxAMatrix lPoseMatrix;
  FbxMatrix lMatrix = pPose->GetMatrix(pNodeIndex);

  memcpy((double*)lPoseMatrix, (double*)lMatrix, sizeof(lMatrix.mData));

  return lPoseMatrix;
}
