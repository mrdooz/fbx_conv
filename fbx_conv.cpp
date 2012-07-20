// converter between .fbx and .kumi format

/*

Original
** INFO **
Total size: 5048642
Material size: 1188 (0.00 Mb)
Mesh size: 33212 (0.03 Mb)
Light size: 176 (0.00 Mb)
Camera size: 76 (0.00 Mb)
Animation size: 1794168 (1.71 Mb)
Binary size: 3219762 (3.07 Mb)

No unnecessary texture coordinates
** INFO **
Total size: 4736938
Material size: 1188 (0.00 Mb)
Mesh size: 33212 (0.03 Mb)
Light size: 176 (0.00 Mb)
Camera size: 76 (0.00 Mb)
Animation size: 1794168 (1.71 Mb)
Binary size: 2908058 (2.77 Mb)

32-bit normals
** INFO **
Total size: 4025042
Material size: 1188 (0.00 Mb)
Mesh size: 33212 (0.03 Mb)
Light size: 176 (0.00 Mb)
Camera size: 76 (0.00 Mb)
Animation size: 1794168 (1.71 Mb)
Binary size: 2196162 (2.09 Mb)


*/

#include "stdafx.h"
#include "fbx_conv.hpp"
#include "utils.hpp"
#include <functional>
#include <mmsystem.h>

using namespace std;
using namespace stdext;

class FbxConverter;

bool g_verbose;
bool g_file_watch;
FbxConverter *g_converter;
string g_src_path;
string g_src, g_dst;
HWND g_hwnd;
HANDLE g_dir;
HANDLE g_quit_event;

const int WM_USER_CONVERT_FILE = WM_USER + 1;
const int FILE_CHANGED_TIMER_ID = 1;


#if _DEBUG
#pragma comment(lib, "fbxsdk-2013.1-mdd.lib")
#else
#pragma comment(lib, "fbxsdk-2013.1-md.lib")
#endif

#pragma comment(lib, "d3dx10.lib")
#pragma comment(lib, "winmm.lib")

template <class T, class U>
U drop(const T &t) {
  return U(t[0], t[1], t[2]);
}


D3DXVECTOR4 max_to_dx(const FbxDouble4 &m, bool neg_w) {
  return D3DXVECTOR4((float)m[0], (float)m[2], (float)m[1], (neg_w ? -1 : 1) * (float)m[3]);
}

D3DXVECTOR4 max_to_dx(const FbxDouble4 &m) {
  return D3DXVECTOR4((float)m[0], (float)m[2], (float)m[1], (float)m[3]);
}

D3DXVECTOR3 max_to_dx(const FbxDouble3 &m) {
  return D3DXVECTOR3((float)m[0], (float)m[2], (float)m[1]);
}

D3DXMATRIX max_to_dx(const FbxAMatrix &mtx) {
  D3DXMATRIX mtx_s, mtx_r, mtx_t;

  auto s = max_to_dx(mtx.GetS());
  auto q = max_to_dx(mtx.GetQ(), true);
  auto t = max_to_dx(mtx.GetT());

  return 
    *D3DXMatrixScaling(&mtx_s, (float)s[0], (float)s[1], (float)s[2]) * 
    *D3DXMatrixRotationQuaternion(&mtx_r, &D3DXQUATERNION((float)q[0], (float)q[1], (float)q[2], (float)q[3])) *
    *D3DXMatrixTranslation(&mtx_t, (float)t[0], (float)t[1], (float)t[2]);
}


FbxConverter::FbxConverter() 
  : _mgr(FbxManager::Create())
  , _settings(FbxIOSettings::Create(_mgr, IOSROOT))
  , _converter(new FbxGeometryConverter(_mgr))
  , _importer(FbxImporter::Create(_mgr, ""))
  , _indent_level(0)
  , _evaluator(nullptr)
  , _writer(new Writer)
  , _log_file(nullptr)
{
  memset(_indent_buffer, ' ', cMaxIndent);
  _indent_buffer[cMaxIndent] = '\0';
}

FbxConverter::~FbxConverter() {

}

string to_string(const char *format, ...)
{
  string res;
  va_list arg;
  va_start(arg, format);
  int len = _vscprintf(format, arg);
  char *buf = (char *)_alloca(len + 1);
  vsprintf_s(buf, len + 1, format, arg);
  va_end(arg);
  return string(buf);
}


#define CHECK_FATAL(x, msg, ...) if (!(x)) {_errors.push_back(string("!! ") + __FUNCTION__ + string(": ") + to_string(msg, __VA_ARGS__)); return false; }

bool FbxConverter::convert(const char *src, const char *dst) {

  DWORD start_time = timeGetTime();
  DWORD end_time;

  // tihi, my c++ looks like JS :)
  bool res = [&]() -> bool {

    DEFER([&] { end_time = timeGetTime(); } );
    add_info("===================================================================");
    _log_file = fopen((string(dst) + ".log").c_str(), "wt");
    if (!_log_file) {
      add_error("unable to open log file!");
      return false;
    }


    bool res = _importer->Initialize(src, -1, _settings);
    if (!res) {
      add_error(to_string("Error calling FbxImporter::Initialize: %s", _importer->GetLastErrorString()).c_str());
      return false;
    }

    _fbx_scene = FbxScene::Create(_mgr, "my_scene");
    _evaluator = _fbx_scene->GetEvaluator();
    _importer->Import(_fbx_scene);
    const FbxGlobalSettings &global_settings = _fbx_scene->GetGlobalSettings();
    auto time_mode = global_settings.GetTimeMode();
    FbxTimeSpan time_span;
    global_settings.GetTimelineDefaultTimeSpan(time_span);
    _start_time = time_span.GetStart();
    _stop_time = time_span.GetStop();
    _duration = time_span.GetDuration().GetSecondDouble();
    _duration_ms = time_span.GetDuration().GetMilliSeconds();
    _fps = time_span.GetDuration().GetFrameRate(time_mode);
    _scene.ambient = global_settings.GetAmbientColor();

    auto axis = _fbx_scene->GetGlobalSettings().GetAxisSystem();

    // check if we need to convert the scene to a DirectX coordinate system
#if USE_DIRECTX_SYSTEM
    if (axis != FbxAxisSystem::eDirectX) {
      FbxAxisSystem dx_system(FbxAxisSystem::eDirectX);
      dx_system.ConvertScene(_fbx_scene);
    }
#else
    // require a 3ds-max system
    if (axis != FbxAxisSystem::eMax) {
      add_error("We require a 3ds-max coordinate system");
      return false;
    }
#endif

    process_scene(_fbx_scene);
    save_scene(dst);

    _fbx_scene->Destroy();
    _fbx_scene = NULL;
    return true;
  }();

  add_verbose("Elapsed time: %.2f", (end_time - start_time) / 1000.0f);

  if (!_errors.empty()) {
    if (_log_file) {
      fprintf(_log_file, "****************** ERRORS ******************\n");
      for (size_t i = 0; i < _errors.size(); ++i) {
        fprintf(_log_file, "%s\n", _errors[i].c_str());
      }
    }
    for (size_t i = 0; i < _errors.size(); ++i)
      puts(_errors[i].c_str());
  }

  if (_log_file) {
    fclose(_log_file);
    _log_file = nullptr;
  }


  return res;
}

bool FbxConverter::process_light(FbxNode *node, FbxLight *fbx_light) {

  INFO_SCOPE;

  if (!fbx_light->CastLight.Get())
    return true;

  FbxAnimEvaluator *evaluator = _fbx_scene->GetEvaluator();

  Light *light = new Light;
  light->name = node->GetName();
  FbxMatrix mtx = evaluator->GetNodeGlobalTransform(node);
  light->pos = max_to_dx(FbxDouble3(mtx.Get(3,0), mtx.Get(3,1), mtx.Get(3,2)));
  light->color = fbx_light->Color.Get();
  light->intensity = fbx_light->Intensity.Get();

  light->use_near_attenuation = fbx_light->EnableNearAttenuation.Get();
  light->near_attenuation_start = fbx_light->NearAttenuationStart.Get();
  light->near_attenuation_end = fbx_light->NearAttenuationEnd.Get();

  light->use_far_attenuation = fbx_light->EnableFarAttenuation.Get();
  light->far_attenuation_start = fbx_light->FarAttenuationStart.Get();
  light->far_attenuation_end = fbx_light->FarAttenuationEnd.Get();

  _scene.lights.emplace_back(unique_ptr<Light>(light));

  add_verbose("found light: %s", light->name.c_str());

  if (!process_animation(node, true))
    return false;

  return true;
}

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

bool FbxConverter::process_camera(FbxNode *node, FbxCamera *camera) {

  INFO_SCOPE;

  FbxCamera::EApertureMode mode = camera->GetApertureMode();
  double w = camera->GetApertureWidth();
  double h = camera->GetApertureHeight();

  FbxCamera::EAspectRatioMode ar_mode = camera->GetAspectRatioMode();

  double fov_x, fov_y, aspect;
  get_aspect_and_fov(camera, &fov_x, &fov_y, &aspect);

  auto c = unique_ptr<Camera>(new Camera);
  c->name = node->GetName();
  c->pos = max_to_dx(camera->Position.Get());
  c->target = max_to_dx(camera->InterestPosition.Get());
  c->up = max_to_dx(camera->UpVector.Get());
  c->roll = camera->Roll.Get();
  c->aspect_ratio = (float)aspect;
  c->fov_x = (float)fov_x;
  c->fov_y = (float)fov_y;
  c->near_plane = camera->NearPlane.Get();
  c->far_plane = camera->FarPlane.Get();

  add_verbose("found camera: %s", c->name.c_str());

  if (!process_animation(node, true))
    return false;

  FbxNode *target = node->GetTarget();
  if (!target) {
    add_error("No target found for camera: %s", node->GetName());
    return false;
  }

  if (!process_animation(target, true))
    return false;

  _scene.cameras.push_back(move(c));
  return true;
}

static FbxDouble3 GetMaterialProperty(const FbxSurfaceMaterial *pMaterial, const char *pPropertyName, const char *pFactorPropertyName, string *filename)
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

void FbxConverter::copy_texture(string src, string *dst) {

  if (src.empty())
    return;

  string drive, dir, fname, ext;

  if (_texture_dst.empty()) {
    split_path(g_dst.c_str(), &drive, &dir, &fname, &ext);
    _texture_dst = drive + dir + "textures/";
    CreateDirectory(_texture_dst.c_str(), NULL);
  }


  split_path(src.c_str(), &drive, &dir, &fname, &ext);
  string dst_path = _texture_dst + fname + ext;
  CopyFile(src.c_str(), dst_path.c_str(), FALSE);

  // make the dst-path relative the .fbx file
  *dst = "textures/" + fname + ext;
}

bool FbxConverter::process_material(FbxNode *fbx_node, int *material_count) {
  INFO_SCOPE;
  *material_count = fbx_node->GetMaterialCount();

  for (int i = 0; i < *material_count; ++i) {
    FbxSurfaceMaterial *node_material = fbx_node->GetMaterial(i);
    const char *name = node_material->GetName();

    // Skip if we've already seen this material
    if (_scene.materials_by_name.find(name) != _scene.materials_by_name.end())
      continue;

    add_verbose("found material: %s", name);

    Material *material = new Material(name);
    _scene.materials_by_name.insert(make_pair(name, material));
    _scene.materials.push_back(unique_ptr<Material>(material));

#define ADD_PROP(name, mat) { string filename; \
  MaterialProperty prop(#name, GetMaterialProperty(mat, FbxSurfaceMaterial::s##name, FbxSurfaceMaterial::s##name##Factor, &filename), true); \
  if (!filename.empty()) { copy_texture(filename, &prop.filename); } \
  material->properties.push_back(prop); }

#define ADD_PROP_FACTOR(name, factor, mat) { string filename; \
  MaterialProperty prop(#name, GetMaterialProperty(mat, FbxSurfaceMaterial::s##name, FbxSurfaceMaterial::s##factor, &filename), true); \
  if (!filename.empty()) { copy_texture(filename, &prop.filename); } \
  material->properties.push_back(prop); }

#define ADD_PROP_FLOAT(name, mat) { \
  const FbxProperty lProperty = mat->FindProperty(FbxSurfaceMaterial::s##name); \
  if (lProperty.IsValid()) { \
    MaterialProperty prop(#name, lProperty.Get<FbxDouble>()); \
    material->properties.push_back(prop); \
  } \
}

#define ADD_PROP_WITHOUT_FACTOR(name, mat) { string filename; \
  MaterialProperty prop(#name, GetMaterialProperty(mat, FbxSurfaceMaterial::s##name, NULL, &filename), true); \
  if (!filename.empty()) { copy_texture(filename, &prop.filename); } \
  material->properties.push_back(prop); }

    bool is_phong = node_material->GetClassId().Is(FbxSurfacePhong::ClassId);
    bool is_lambert = node_material->GetClassId().Is(FbxSurfaceLambert::ClassId);

    if (is_lambert || is_phong) {
      FbxSurfaceLambert *mat = (FbxSurfaceLambert *)node_material;

      ADD_PROP(Ambient, mat);
      ADD_PROP(Diffuse, mat);
      ADD_PROP(Emissive, mat);
      ADD_PROP_FACTOR(TransparentColor, TransparencyFactor, mat);

      if (is_phong) {
        FbxSurfacePhong *mat = (FbxSurfacePhong *)node_material;
        ADD_PROP(Specular, mat);
        // grab the shininess, and convert it back to the 3dsmax value
        // (parameters obtained via logarithmic regression)
        double a0 = 0.0048;
        double a1 = 14.42737962;
        const FbxProperty shininess_property = mat->FindProperty(FbxSurfaceMaterial::sShininess);
        if (shininess_property.IsValid()) {
          double x = shininess_property.Get<FbxDouble>();
          double shininess = a0 + a1 * log(x);
          MaterialProperty prop("Shininess", shininess);
          material->properties.emplace_back(prop);
          //ADD_PROP_FLOAT(Shininess, mat);
        }

        ADD_PROP(Reflection, mat);
      }
    } else {
      // unknown material
      _errors.push_back(to_string("Unknown material type: %s", node_material->GetTypeName()));
    }

    // If the diffuse property has a texturemap, use the texturemap technique
    auto it = find_if(begin(material->properties), end(material->properties), [&](const MaterialProperty &prop) {return prop.name == "Diffuse"; });
    if (it != end(material->properties) && !it->filename.empty()) {
      material->technique = "texture";
    } else {
      material->technique = "diffuse";
    }

#undef ADD_PROP
#undef ADD_PROP_FACTOR
#undef ADD_PROP_WITHOUT_FACTOR
  }

  return true;
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
FbxAMatrix GetGlobalPosition(FbxNode* pNode, const FbxTime& pTime, FbxPose* pPose = NULL, FbxAMatrix* pParentGlobalPosition = NULL);
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

template <class T>
vector<int> remove_keyframes(const vector<T> &org, const function<float(const T& a, const T& b)> &distance) {

  auto tmp(org);

  // calc diff between adjacent keyframes
  vector<float> diff;
  set<int> to_remove;
  diff.resize(tmp.size()-2);
  for (size_t i = 1; i < tmp.size()-1; ++i) {
    float d = distance(tmp[i-1], tmp[i+1]);
    diff[i-1] = d;
    if (d < 0.00001f)
      to_remove.insert(i);
  }

  vector<int> res;
  res.reserve(org.size());

  for (size_t i = 0; i < org.size(); ++i) {
    if (to_remove.find(i) == to_remove.end())
      res.push_back(i);
  }

  // hack to simplify to 1 frame
  if (res.size() == 2) {
    if (org[res[0]] == org[res[1]])
      res.pop_back();
  }

  return res;
}

bool FbxConverter::process_animation(FbxNode *node, bool translation_only) {
  INFO_SCOPE;

  FbxAMatrix a = GetGeometry(node);

  auto &anim = _animation[node->GetName()];

  vector<D3DXVECTOR3> pos;
  vector<D3DXVECTOR4> rot;
  vector<D3DXVECTOR3> scale;

  int num_frames = (int)(_duration_ms * _fps / 1000 + 0.5f);
  for (int i = 0; i <= num_frames; ++i) {
    FbxTime cur;
    double cur_time = i*_duration/num_frames;
    cur.SetSecondDouble(cur_time);
    FbxAMatrix m;
    m = GetGlobalPosition(node, cur) * a;
    pos.emplace_back(drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetT())));
    rot.emplace_back(max_to_dx(m.GetQ(), true));
    scale.emplace_back(drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetS())));
  }

  auto vec3_dist = [&](const D3DXVECTOR3 &a, const D3DXVECTOR3 &b) -> float { 
    D3DXVECTOR3 d; D3DXVec3Subtract(&d, &a, &b); return D3DXVec3Length(&d);
  };

  auto vec4_dist = [&](const D3DXVECTOR4 &a, const D3DXVECTOR4 &b) -> float { 
    D3DXVECTOR4 d; D3DXVec4Subtract(&d, &a, &b); return D3DXVec4Length(&d);
  };

  // simplify the keyframes
  auto pos_frames = remove_keyframes<D3DXVECTOR3>(pos, vec3_dist);
  auto rot_frames = remove_keyframes<D3DXVECTOR4>(rot, vec4_dist);
  auto scale_frames = remove_keyframes<D3DXVECTOR3>(scale, vec3_dist);

  FbxTime cur;
  FbxAMatrix m;
  for (size_t i = 0; i < pos_frames.size(); ++i) {
    double cur_time = pos_frames[i]*_duration/num_frames;
    cur.SetSecondDouble(cur_time);
    m = GetGlobalPosition(node, cur) * a;
    anim.pos.emplace_back(KeyFrameVec3(cur_time, drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetT()))));
  }

  if (!translation_only) {
    for (size_t i = 0; i < rot_frames.size(); ++i) {
      double cur_time = rot_frames[i]*_duration/num_frames;
      cur.SetSecondDouble(cur_time);
      m = GetGlobalPosition(node, cur) * a;
      anim.rot.emplace_back(KeyFrameVec4(cur_time, max_to_dx(m.GetQ(), true)));
    }

    for (size_t i = 0; i < scale_frames.size(); ++i) {
      double cur_time = scale_frames[i]*_duration/num_frames;
      cur.SetSecondDouble(cur_time);
      m = GetGlobalPosition(node, cur) * a;
      anim.scale.emplace_back(KeyFrameVec3(cur_time, drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetS()))));
    }
  }

  return true;
}

bool FbxConverter::process_mesh(FbxNode *fbx_node, FbxMesh *fbx_mesh) {

  INFO_SCOPE;

  add_info("found mesh: %s", fbx_node->GetName());

  int material_count;
  if (!process_material(fbx_node, &material_count)) {
    return false;
  }

  // just use layer 0.
  FbxLayer *layer = fbx_mesh->GetLayer(0);
  CHECK_FATAL(layer, "no layer 0 found");

  // group the polygons by material
  vector<vector<int>> polys_by_material;
  polys_by_material.resize(max(1, material_count));

  bool use_default_material = false;
  const FbxLayerElementMaterial *materials = layer->GetMaterials();
  if (materials) {
    FbxLayerElement::EMappingMode mm = materials->GetMappingMode();
    static const char *mm_str[] = { "eNONE", "eBY_CONTROL_POINT", "eBY_POLYGON_VERTEX", "eBY_POLYGON", "eBY_EDGE", "eALL_SAME" };
    CHECK_FATAL(mm == FbxLayerElement::eByPolygon || mm == FbxLayerElement::eAllSame, 
                "Unsupported mapping mode: %s", mm <= FbxLayerElement::eAllSame ? mm_str[mm] : "unknown");

    const FbxLayerElementArrayTemplate<int> &arr = materials->GetIndexArray();
    if (mm == FbxLayerElement::eByPolygon) {
      // each poly has its own material index
      for (int i = 0; i < arr.GetCount(); ++i) {
        polys_by_material[arr[i]].push_back(i);
      }
    } else if (mm == FbxLayerElement::eAllSame) {
      // all the polys share the same material
      for (int i = 0; i < fbx_mesh->GetPolygonCount(); ++i)
        polys_by_material[0].push_back(i);
    }
  } else {
    // no materials found, so use default material
    use_default_material = true;
    for (int i = 0; i < fbx_mesh->GetPolygonCount(); ++i)
      polys_by_material[0].push_back(i);
  }

  // get the uv sets
  vector<string> uv_sets;
  for (int i = 0; i < layer->GetUVSetCount(); ++i)
    uv_sets.push_back(layer->GetUVSets()[i]->GetName());

  auto mesh = unique_ptr<Mesh>(new Mesh(fbx_node->GetName()));

  if (polys_by_material.size() > 1)
    add_verbose("%Iu submesh%s", polys_by_material.size(), polys_by_material.size() == 1 ? "" : "es");

  mesh->obj_to_world = max_to_dx(fbx_node->EvaluateGlobalTransform());

  // each material used for the mesh creates a sub mesh
  for (size_t i = 0; i < polys_by_material.size(); ++i) {

    uint32 vertex_flags = SubMesh::kPos | SubMesh::kNormal;

    if (!use_default_material) {
      // check if the current material uses a diffuse map
      FbxSurfaceMaterial *material = fbx_node->GetMaterial(i);
      const char *name = material->GetName();
      string filename;
      GetMaterialProperty(material, FbxSurfaceMaterial::sDiffuse, FbxSurfaceMaterial::sDiffuseFactor, &filename);
      if (!filename.empty())
        vertex_flags |= SubMesh::kTex0;
    }

    // keep track of the unique vertices
    set<SuperVertex> super_verts;

    const string material_name = use_default_material ? "default-material" : fbx_node->GetMaterial(i)->GetName();
    string submesh_name = polys_by_material.size() > 1 ? to_string("%s_%d", fbx_node->GetName(), i) : fbx_node->GetName();
    SubMesh *sub_mesh = new SubMesh(submesh_name, material_name, vertex_flags);
    mesh->sub_meshes.push_back(std::unique_ptr<SubMesh>(sub_mesh));

    auto material_it = _scene.materials_by_name.find(material_name);
    if (material_it != end(_scene.materials_by_name)) {
      MaterialInfo info;
      info.submesh = sub_mesh;
      info.material = material_it->second;
      _material_info.push_back(info);
    }

    auto &indices = polys_by_material[i];
    for (size_t j = 0; j < indices.size(); ++j) {
      const int poly_idx = indices[j];
      CHECK_FATAL(fbx_mesh->GetPolygonSize(j) == 3, "Only polygons of size 3 supported");

      // we reverse the winding order to convert between the right and left-handed systems
      for (int k = 2; k >= 0; --k) {
        auto pos = max_to_dx(fbx_mesh->GetControlPointAt(fbx_mesh->GetPolygonVertex(poly_idx, k)));
        FbxVector4 normal2;
        fbx_mesh->GetPolygonVertexNormal(poly_idx, k, normal2);
        auto normal = max_to_dx(normal2);

        SuperVertex cand(drop<D3DXVECTOR4, D3DXVECTOR3>(pos), drop<D3DXVECTOR4, D3DXVECTOR3>(normal));
        for (size_t uv_idx = 0; uv_idx  < uv_sets.size(); ++uv_idx) {
          FbxVector2 uv;
          fbx_mesh->GetPolygonVertexUV(poly_idx, k, uv_sets[uv_idx].c_str(), uv);
          // flip the y-coordinate to match DirectX
          uv[1] = 1 - uv[1];
          cand.uv.push_back(D3DXVECTOR2((float)uv[0], (float)uv[1]));
        }

        // create a supervertex for the current vertex, and check if it already exists to
        // determine what vertex index to give it
        int idx = -1;
        set<SuperVertex>::iterator it = super_verts.find(cand);
        if (it == super_verts.end()) {
          idx = cand.idx = sub_mesh->pos.size();
          sub_mesh->pos.push_back(drop<D3DXVECTOR4, D3DXVECTOR3>(pos));
          sub_mesh->normal.push_back(drop<D3DXVECTOR4, D3DXVECTOR3>(normal));
          if (vertex_flags & SubMesh::kTex0) sub_mesh->tex0.push_back(cand.uv[0]);
          if (vertex_flags & SubMesh::kTex1) sub_mesh->tex1.push_back(cand.uv[1]);
          super_verts.insert(cand);
        } else {
          // vertex already exists, so reuse the vertex index
          idx = it->idx;
        }
        sub_mesh->indices.push_back(idx);
      }
    }
  }

  _scene.meshes.push_back(move(mesh));

  if (!process_animation(fbx_node, false))
    return false;

  return true;
}

bool FbxConverter::process_scene(FbxScene *scene) {

  if (FbxNode *root = scene->GetRootNode()) {
    for (int i = 0; i < root->GetChildCount(); ++i) {
      FbxNode *child = root->GetChild(i);
      FbxNodeAttribute *node_attr = child->GetNodeAttribute();
      if (!node_attr)
        continue;

      switch (node_attr->GetAttributeType()) {
      case FbxNodeAttribute::eMarker:
        break;

      case FbxNodeAttribute::eSkeleton:
        break;

      case FbxNodeAttribute::eMesh:
        {
          FbxMesh *triangulated = _converter->TriangulateMesh((FbxMesh *)node_attr);
          bool res = process_mesh(child, triangulated);
          triangulated->Destroy();
          if (!res)
            return false;
        }
        break;

      case FbxNodeAttribute::eCamera:
        if (!process_camera(child, (FbxCamera *)node_attr))
          return false;
        break;

      case FbxNodeAttribute::eLight:
        if (!process_light(child, (FbxLight *)node_attr))
          return false;
        break;

      }   
    }
  }

  return true;
}

bool FbxConverter::save_scene(const char *dst) {
  // the scene is saved so it can be read in a single sequential read,
  // and then a simple fixup applied to the pointers

  if (!_writer->open(dst))
    return false;

  MainHeader header;
  header.version = FILE_VERSION;
  _writer->push_pos();
  _writer->write(header);

  header.global_ofs = _writer->pos();
  if (!save_globals())
    return false;

  header.material_ofs = _writer->pos();
  if (!save_materials())
    return false;

  if (!save_material_info())
    return false;

  header.mesh_ofs = _writer->pos();
  if (!save_meshes())
    return false;

  header.light_ofs = _writer->pos();
  if (!save_lights())
    return false;

  header.camera_ofs = _writer->pos();
  if (!save_cameras())
    return false;

  header.animation_ofs = _writer->pos();
  if (!save_animations())
    return false;

  header.binary_ofs = _writer->pos();
  _writer->save_binary();
  header.total_size = _writer->pos();

  _writer->push_exch();
  _writer->write(header);
  _writer->pop_pos();

  add_verbose("** INFO **");
  add_verbose("Total size: %d", header.total_size);
#define ADD_INFO(name, o1, o2) add_verbose(#name ## " size: %d (%.2f Mb)", header.o1 - header.o2, (header.o1 - header.o2) / (float)(1024 * 1024));
  ADD_INFO(Material, mesh_ofs, material_ofs);
  ADD_INFO(Mesh, light_ofs, mesh_ofs);
  ADD_INFO(Light, camera_ofs, light_ofs);
  ADD_INFO(Camera, animation_ofs, camera_ofs);
  ADD_INFO(Animation, binary_ofs, animation_ofs);
  ADD_INFO(Binary, total_size, binary_ofs);
#undef ADD_INFO

  return true;
}

void write_vector(const Writer &writer, const D3DXVECTOR3 &v) {
  writer.write(v[0]);
  writer.write(v[1]);
  writer.write(v[2]);
}

void write_vector(const Writer &writer, const FbxDouble3 &v) {
  writer.write((float)v[0]);
  writer.write((float)v[1]);
  writer.write((float)v[2]);
}

void write_vector(const Writer &writer, const FbxDouble4 &v) {
  writer.write((float)v[0]);
  writer.write((float)v[1]);
  writer.write((float)v[2]);
}

template<class T> 
void prune_animations(map<string, vector<KeyFrame<T>>> *anims) {
  for (auto it = begin(*anims); it != end(*anims); ) {
    if (it->second.empty()) {
      it = anims->erase(it);
    } else {
      ++it;
    }
  }
}

template<class T> 
bool FbxConverter::save_animations(const map<string, vector<KeyFrame<T>>> &anims) {

  _writer->write((int)anims.size());

  for (auto it = begin(anims); it != end(anims); ++it) {
    const string &node_name = it->first;
    auto &frames = it->second;
    _writer->add_deferred_string(node_name);
    _writer->write((int)frames.size());
    _writer->write_raw(&frames[0], sizeof(frames[0]) * frames.size());
  }

  return true;
}

template<class T> 
bool FbxConverter::save_animations(const vector<KeyFrame<T>> &frames) {

  _writer->write((int)frames.size());
  if (frames.size() > 0)
    _writer->write_raw(&frames[0], sizeof(frames[0]) * frames.size());

  return true;
}

bool FbxConverter::save_animations() {
  BlockHeader header;
  header.id = BlockId::kAnimation;
  ScopedBlock scoped_block(header, *_writer);

  for (auto it = begin(_animation); it != end(_animation); ++it) {
    auto &name = it->first;
    auto &anim = it->second;
    if (!anim.pos.empty() || !anim.rot.empty() || !anim.scale.empty()) {
      _writer->add_deferred_string(name);
      save_animations<D3DXVECTOR3>(anim.pos);
      save_animations<D3DXVECTOR4>(anim.rot);
      save_animations<D3DXVECTOR3>(anim.scale);
    }
  }

/*
  prune_animations(&_animation_float);
  prune_animations(&_animation_vec3);
  prune_animations(&_animation_mtx);

  save_animations<float>(_animation_float);
  save_animations<D3DXVECTOR3>(_animation_vec3);
  save_animations<D3DXMATRIX>(_animation_mtx);
*/
  return true;
}

bool FbxConverter::save_cameras() {

  BlockHeader header;
  header.id = BlockId::kCameras;
  ScopedBlock scoped_block(header, *_writer);

  auto &cameras = _scene.cameras;

  _writer->write((int)cameras.size());
  for (size_t i = 0; i < cameras.size(); ++i) {
    auto &camera = cameras[i];
    _writer->add_deferred_string(camera->name);
    write_vector(*_writer, camera->pos);
    write_vector(*_writer, camera->target);
    write_vector(*_writer, camera->up);
    _writer->write((float)camera->roll);
    _writer->write((float)camera->aspect_ratio);
    _writer->write((float)camera->fov_x);
    _writer->write((float)camera->fov_y);
    _writer->write((float)camera->near_plane);
    _writer->write((float)camera->far_plane);
  };

  return true;
}

bool FbxConverter::save_lights() {

  BlockHeader header;
  header.id = BlockId::kLights;
  ScopedBlock scoped_block(header, *_writer);

  auto &lights = _scene.lights;

  _writer->write((int)lights.size());
  for (size_t i = 0; i < lights.size(); ++i) {
    auto &light = lights[i];
    _writer->add_deferred_string(light->name);
    write_vector(*_writer, light->pos);
    write_vector(*_writer, light->color);
    _writer->write((float)light->intensity);
    _writer->write(light->use_far_attenuation);
    _writer->write((float)light->far_attenuation_start);
    _writer->write((float)light->far_attenuation_end);
  }

  return true;
}

bool FbxConverter::save_meshes() {

  BlockHeader header;
  header.id = BlockId::kMeshes;
  ScopedBlock scoped_block(header, *_writer);

  auto &meshes = _scene.meshes;

  _writer->write((int)meshes.size());
  for (size_t i = 0; i < meshes.size(); ++i) {

    auto &mesh = meshes[i];
    _writer->add_deferred_string(mesh->name);
    _writer->write(mesh->obj_to_world);

    _writer->write((int)mesh->sub_meshes.size());
    for (size_t j = 0; j < mesh->sub_meshes.size(); ++j) {
      SubMesh *sub = mesh->sub_meshes[j].get();
      _writer->add_deferred_string(sub->name);
      _writer->add_deferred_string(sub->material);

      int index_size;
      vector<char> vb, ib;
      compact_vertex_data(*sub, &vb);
      compact_index_data(*sub, &ib, &index_size);

      _writer->write(sub->vertex_flags);
      _writer->write(sub->element_size);
      _writer->write(index_size);
      _writer->add_deferred_binary(vb.data(), vb.size());
      _writer->add_deferred_binary(ib.data(), ib.size());
    }
  }

  return true;
}

bool FbxConverter::save_globals() {
  BlockHeader header;
  header.id = BlockId::kGlobals;
  ScopedBlock scoped_block(header, *_writer);
  _writer->write((float)_scene.ambient.mRed);
  _writer->write((float)_scene.ambient.mGreen);
  _writer->write((float)_scene.ambient.mBlue);
  _writer->write((float)_scene.ambient.mAlpha);
  return true;
}

bool FbxConverter::save_materials() {

  BlockHeader header;
  header.id = BlockId::kMaterials;
  ScopedBlock scoped_block(header, *_writer);

  auto &materials = _scene.materials;

  _writer->write((int)_scene.materials_by_name.size());
  for (auto it = begin(_scene.materials_by_name); it != end(_scene.materials_by_name); ++it) {
    const Material *mat = it->second;
    _writer->add_deferred_string(mat->name);
    _writer->add_deferred_string(mat->technique);
    _writer->write((int)mat->properties.size());

    // write the properties for the material
    for (size_t i = 0; i < mat->properties.size(); ++i) {
      const MaterialProperty &prop = mat->properties[i];
      _writer->add_deferred_string(prop.name);
      _writer->add_deferred_string(prop.filename);
      _writer->write(prop.type);
      switch (prop.type) {
        case Property::kInt:
          _writer->write(prop._int);
          break;
        case Property::kFloat:
          _writer->write_raw(&prop._float[0], sizeof(float));
          break;
        case Property::kFloat3:
          _writer->write_raw(&prop._float[0], 3 * sizeof(float));
          break;
        case Property::kFloat4:
        case Property::kColor:
          _writer->write_raw(&prop._float[0], 4 * sizeof(float));
          break;
        default:
          add_error("Unknown property type exporting materials");
          break;
        }
    }
  }
  return true;
}

static string type_to_string(Property::Type type) {
  switch (type) {
    case Property::kInt: return "int";
    case Property::kFloat: return "float";
    case Property::kColor: return "color";
    case Property::kFloat3: return "float3";
    case Property::kFloat4: return "float4";
  }
  return "unknown";
}

bool FbxConverter::save_material_info() {

  string drive, dir, fname, ext;
  split_path(g_dst.c_str(), &drive, &dir, &fname, &ext);
  FILE *f = fopen((drive + dir + fname + "_materials_default.json").c_str(), "wt");

  fprintf(f, "{\n\t\"material_connections\" : [");

  for (size_t i = 0; i < _material_info.size(); ++i) {
    auto &cur = _material_info[i];
    fprintf(f, ",\n\t\t{ \"submesh\" : \"%s\", \"technique\" : \"%s\", \"material\" : \"%s\" }" + (i == 0 ? 1 : 0), 
      cur.submesh->name.c_str(),
      cur.material->technique.c_str(),
      cur.material->name.c_str());
  }

  for (auto it = begin(_material_info); it != end(_material_info); ++it) {
    SubMesh *sm = it->submesh;
    Material *material = it->material;
  }

  fprintf(f, "\n\t],\n\n");

  fprintf(f, "\t\"materials\" : [\n");

  auto &materials = _scene.materials;
  size_t num_mats = materials.size();
  for (size_t i = 0; i < num_mats; ++i) {
    auto &mat = materials[i];
    size_t num_props = mat->properties.size();
    fprintf(f, "\t\t{\n\t\t\t\"name\" : \"%s\"%s\n", mat->name.c_str(), num_props ? ", " : "");

    if (num_props) {
      fprintf(f, "\t\t\t\"properties\" : [\n");

      // write the properties for the material
      for (size_t j = 0; j < num_props; ++j) {
        const MaterialProperty &prop = mat->properties[j];
        fprintf(f, "\t\t\t\t{\n");
        fprintf(f, "\t\t\t\t\t\"name\" : \"%s\",\n", prop.name.c_str());
        fprintf(f, "\t\t\t\t\t\"type\" : \"%s\",\n", type_to_string(prop.type).c_str());
        fprintf(f, "\t\t\t\t\t\"value\" : ");

        switch (prop.type) {
          case Property::kFloat: {
            fprintf(f, "{ \"x\" : %f }\n", prop._float[0]);
            break;
          }

          case Property::kColor: {
            fprintf(f, "{\n\t\t\t\t\t\t\"r\" : %f, \n", prop._float[0]);
            fprintf(f, "\t\t\t\t\t\t\"g\" : %f, \n", prop._float[1]);
            fprintf(f, "\t\t\t\t\t\t\"b\" : %f, \n", prop._float[2]);
            fprintf(f, "\t\t\t\t\t\t\"a\" : %f\n\t\t\t\t\t}\n", prop._float[3]);
            break;
          }
        }
        fprintf(f, "\t\t\t\t}%s\n", (j != num_props - 1) ? "," : "\n\t\t\t]");
      }
    }
    fprintf(f, "\t\t}%s\n", (i != num_mats - 1) ? "," : "");
  }

  fprintf(f, "\t]");
    
  fprintf(f, "\n}\n");
  fclose(f);

  return true;
}


void FbxConverter::add_error(const char *fmt, ...) {

  va_list arg;
  va_start(arg, fmt);
  int len = _vscprintf(fmt, arg);
  char *buf = (char *)_alloca(len + 1);
  vsprintf_s(buf, len + 1, fmt, arg);
  va_end(arg);
  _errors.push_back(buf);
}

void FbxConverter::add_info(const char *fmt, ...) {

  va_list arg;
  va_start(arg, fmt);
  int len = _vscprintf(fmt, arg);
  char *buf = (char *)_alloca(len + 1 + _indent_level);
  vsprintf_s(buf + _indent_level, len + 1, fmt, arg);
  memset(buf, ' ', _indent_level);
  va_end(arg);
  puts(buf);

  if (_log_file)
    fprintf(_log_file, "%s\n", buf);
}

void FbxConverter::add_verbose(const char *fmt, ...) {

  va_list arg;
  va_start(arg, fmt);
  int len = _vscprintf(fmt, arg);
  char *buf = (char *)_alloca(len + 1 + _indent_level);
  vsprintf_s(buf + _indent_level, len + 1, fmt, arg);
  memset(buf, ' ', _indent_level);
  va_end(arg);

  if (g_verbose)
    puts(buf);

  if (_log_file)
    fprintf(_log_file, "%s\n", buf);
}

string wide_char_to_utf8(LPCWSTR str, int len_in_bytes) {
  char *buf = (char *)_alloca(len_in_bytes) + 1;
  int len = WideCharToMultiByte(CP_UTF8, 0, str, len_in_bytes / 2, buf, len_in_bytes + 1, NULL, NULL);
  if (len)
    buf[len] = '\0';
  return string(buf);
}

int parse_cmd_line(LPSTR lpCmdLine) {
  vector<string> tokens;
  boost::split(tokens, lpCmdLine, boost::is_any_of("\t "));

  char module_name[MAX_PATH];
  GetModuleFileName(NULL, module_name, sizeof(module_name));

  int argc = (int)tokens.size();

  if (argc < 2) {
    printf("syntax: %s [--verbose] [--watch] src dst", module_name);
    return 1;
  }

  for (int i = 0; i < argc - 2; ++i) {
    g_verbose     |= tokens[i] == "--verbose";
    g_file_watch  |= tokens[i] == "--watch";
  }

  g_src = tokens[argc-2].c_str();
  g_dst = tokens[argc-1].c_str();

  replace(RANGE(g_src), '\\', '/');
  replace(RANGE(g_dst), '\\', '/');

  boost::erase_all(g_src, "\"");
  boost::erase_all(g_dst, "\"");

  return 0;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);

int create_msg_window(HINSTANCE instance) {

  static const char* class_name = "DUMMY_CLASS";
  WNDCLASSEX wx = {};
  wx.cbSize = sizeof(WNDCLASSEX);
  wx.lpfnWndProc = WndProc;
  wx.hInstance = instance;
  wx.lpszClassName = class_name;
  if ( RegisterClassEx(&wx) ) {
    g_hwnd = CreateWindowEx( 0, class_name, "dummy_name", 0, 0, 0, 0, 0, HWND_MESSAGE, NULL, NULL, NULL );
    return 0;
  }
  return 1;
}

int create_dir_watch() {

  char drive[_MAX_DRIVE];
  char dir[_MAX_DIR];
  char fname[_MAX_FNAME];
  char ext[_MAX_EXT];
  _splitpath(g_src.c_str(), drive, dir, fname, ext);

  g_src_path = string(drive) + dir;

  g_dir = CreateFileA(g_src_path.c_str(), FILE_LIST_DIRECTORY, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
    NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED, NULL);

  return 0;
}

char g_io_buf[4096];

VOID CALLBACK FileIOCompletionRoutine(DWORD dwErrorCode, DWORD dwNumberOfBytesTransfered, LPOVERLAPPED lpOverlapped) {
  if (dwNumberOfBytesTransfered) {
    FILE_NOTIFY_INFORMATION *info = (FILE_NOTIFY_INFORMATION*)&g_io_buf[0];
    string filename = wide_char_to_utf8(info->FileName, info->FileNameLength);
    if (g_src_path + filename == g_src) {
      // file is modified, so set a timer to convert in 5s
      SetTimer(g_hwnd, FILE_CHANGED_TIMER_ID, 2000, NULL);
    }
  }
}

DWORD WINAPI WatcherThread(LPVOID param) {

  while (true) {
    OVERLAPPED overlapped;
    ZeroMemory(&overlapped, sizeof(overlapped));
    BOOL res = ReadDirectoryChangesW(g_dir, g_io_buf, sizeof(g_io_buf), FALSE, FILE_NOTIFY_CHANGE_LAST_WRITE, NULL, &overlapped, FileIOCompletionRoutine);
    if (WAIT_OBJECT_0 == WaitForSingleObjectEx(g_quit_event, INFINITE, TRUE)) {
      break;
    }
  }

  return 0;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam) {

  switch (msg) {

    case WM_TIMER:
      if (wparam == FILE_CHANGED_TIMER_ID) {
        PostMessage(hwnd, WM_USER_CONVERT_FILE, 0, 0);
        return 0;
      }
      break;

    case WM_DESTROY:
      PostQuitMessage(0);
      return 0;

    case WM_USER_CONVERT_FILE: {

      KillTimer(g_hwnd, FILE_CHANGED_TIMER_ID);
      g_converter = new FbxConverter();
      if (!g_converter->convert(g_src.c_str(), g_dst.c_str())) {
        PostQuitMessage(1);
      }
      SAFE_DELETE(g_converter);
      return 0;
    }

  }

  return DefWindowProc(hwnd, msg, wparam, lparam);
}


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd) {

  AllocConsole();
  freopen("CONOUT$","wb",stdout);
  freopen("CONOUT$","wb",stderr);

  HANDLE console_handle = GetStdHandle(STD_INPUT_HANDLE);

  if (parse_cmd_line(lpCmdLine))
    return 1;

  MSG msg;

  if (g_file_watch) {
    if (create_msg_window(hInstance))
      return 1;

    if (create_dir_watch())
      return 1;

    g_quit_event = CreateEvent(NULL, FALSE, FALSE, NULL);
    DWORD thread_id;
    HANDLE thread = CreateThread(NULL, 0, WatcherThread, NULL, 0, &thread_id);

    SendMessage(g_hwnd, WM_USER_CONVERT_FILE, 0, 0);

    bool done = false;
    while (!done) {
      if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
      }
      INPUT_RECORD inputs[128];
      DWORD events_read;
      ReadConsoleInput(console_handle, inputs, 128, &events_read);
      for (DWORD i = 0; i < events_read; ++i) {
        if (inputs[i].EventType == KEY_EVENT && !inputs[i].Event.KeyEvent.bKeyDown) {

          // keyup
          switch (inputs[i].Event.KeyEvent.wVirtualKeyCode) {

            case VK_ESCAPE:
              msg.wParam = 0;
              done = true;
              break;

            case 'F':
              // force reparse
              SendMessage(g_hwnd, WM_USER_CONVERT_FILE, 0, 0);
              break;

            case 'V':
              g_verbose = !g_verbose;
              printf("** VERBOSE: %s\n", g_verbose ? "on" : "off");
              break;
          }
        }
      }
    }
      
    SetEvent(g_quit_event);
    WaitForSingleObject(thread, INFINITE);

    CloseHandle(g_dir);
  } else {
    g_converter = new FbxConverter();
    msg.wParam = g_converter->convert(g_src.c_str(), g_dst.c_str()) ? 0 : 1;
    SAFE_DELETE(g_converter);
  }

  FreeConsole();

  return msg.wParam;
}

void FbxConverter::compact_vertex_data(const SubMesh &submesh, vector<char> *data) {
  int len = submesh.element_size * submesh.pos.size();
  data->resize(len);
  char *buf = data->data();

  //add_verbose("*** COMPACT ***");

#define SET_INC(type, src) { *(type *)buf = src; buf += sizeof(type); }
  for (size_t i = 0; i < submesh.pos.size(); ++i) {
    SET_INC(D3DXVECTOR3, submesh.pos[i]);
    // save normal as 1+14 bit x, 1+15 bit y, and 1 sign bit for z
    D3DXVECTOR3 n = submesh.normal[i];
    D3DXVec3Normalize(&n, &n);
    int neg_x = n.x < 0 ? 1 : 0;
    int neg_y = n.y < 0 ? 1 : 0;
    int neg_z = n.z < 0 ? 1 : 0;
    int nx = (int)fabs(16383 * n.x);
    int ny = (int)fabs(32767 * n.y);
    uint32 normal = 
      (neg_x << 31) | (nx << 17) |
      (neg_y << 16) | (ny <<  1) |
      (neg_z);
    SET_INC(uint32, normal);
/*
    {
      D3DXVECTOR3 n2;
      bool neg_x = !!(normal & (1 << 31));
      bool neg_y = !!(normal & (1 << 16));
      bool neg_z = !!(normal & 1);

      n2.x = (neg_x ? -1 : 1) * (((normal >> 17) & 0x3fff) / 16383.0f);
      n2.y = (neg_y ? -1 : 1) * (((normal >>  1) & 0x7fff) / 32767.0f);
      n2.z = (neg_z ? -1 : 1) * (sqrtf(1 - n2.x * n2.x - n2.y * n2.y));
      D3DXVec3Normalize(&n2, &n2);

      D3DXVECTOR3 d;
      D3DXVec3Subtract(&d, &n, &n2);
      if (D3DXVec3Length(&d) > 0.1f) {
        int a = 10;
      }
    }
*/
    //add_verbose("%.8x, %f, %f, %f", normal, n.x, n.y, n.z);

    if (submesh.vertex_flags & SubMesh::kTex0) {
      SET_INC(D3DXVECTOR2, submesh.tex0[i]);
    }
    if (submesh.vertex_flags & SubMesh::kTex1) {
      SET_INC(D3DXVECTOR2, submesh.tex1[i]);
    }
  }
  data->resize(buf - data->data());
}

void FbxConverter::compact_index_data(const SubMesh &submesh, vector<char> *data, int *index_size) {
  // check if we need 16 or 32 bit indices
  const size_t num_verts = max4(submesh.pos.size(), submesh.normal.size(), submesh.tex0.size(), submesh.tex1.size());
  const size_t elems = submesh.indices.size();
  const bool need_32_bit = num_verts >= (1 << 16);
  *index_size = need_32_bit ? 4 : 2;
  int len = elems * (*index_size);
  data->resize(len);
  char *buf = data->data();
  if (need_32_bit)
    copy(RANGE(submesh.indices), checked_array_iterator<uint32 *>((uint32 *)buf, elems));
  else
    copy(RANGE(submesh.indices), checked_array_iterator<uint16 *>((uint16 *)buf, elems));
}
