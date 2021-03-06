// converter between .fbx and .kumi format


#include "stdafx.h"
#include "fbx_conv.hpp"
#include "utils.hpp"
#include "bitutils.hpp"
#include "optimize.hpp"
#include "support.hpp"
#include <direct.h>

using namespace std;
using namespace stdext;

class FbxConverter;

bool g_verbose;
bool gExportAnimation = true;
bool gOptimizeMesh = true;
bool g_optimize_spline = false;

int gPositionBits = 14;
int gNormalBits = 11;
int gTexCoordBits = 10;
int gAnimationBits = 16;
int g_num_control_points = 100;

bool gCompressVertices = false;

bool g_file_watch;
FbxConverter *g_converter;
string g_src_path;
string g_src, g_dst;
string g_inputDir, g_outputDir;
HWND g_hwnd;
HANDLE g_dir;
HANDLE g_quit_event;
string gAppRoot;

const int WM_USER_CONVERT_FILE = WM_USER + 1;
const int FILE_CHANGED_TIMER_ID = 1;


#if _DEBUG
#pragma comment(lib, "fbxsdk-2013.2-mdd.lib")
#else
#pragma comment(lib, "fbxsdk-2013.2-md.lib")
#endif

#pragma comment(lib, "d3dx10.lib")
#pragma comment(lib, "winmm.lib")

template <class T, class U>
U drop(const T &t) {
  return U(t[0], t[1], t[2]);
}


static int num_bits(int a) {
  int res = 1;
  while (a > (1 << res) - 1)
    ++res;
  return res;
}

static uint32 quantize(float value, int num_bits) {
  // assume value is in the [-1..1] range
  uint32 scale = (1 << (num_bits-1)) - 1;
  uint32 v = (uint32)(scale * fabs(value));
  if (value < 0)
    v = set_bit(v, num_bits - 1);
  return v;
}

static void compute_extents(const std::vector<D3DXVECTOR3> &verts, D3DXVECTOR3 *center, D3DXVECTOR3 *extents) {
  D3DXVECTOR3 min_value, max_value;
  min_value = max_value = verts[0];
  for (size_t i = 1; i < verts.size(); ++i) {
    auto &cur = verts[i];
    D3DXVec3Minimize(&min_value, &min_value, &cur);
    D3DXVec3Maximize(&max_value, &max_value, &cur);
  }

  *center = 0.5f * (min_value + max_value);
  *extents = max_value - *center;
}

static void compute_extents(const std::vector<D3DXVECTOR4> &verts, D3DXVECTOR4 *center, D3DXVECTOR4 *extents) {
  D3DXVECTOR4 min_value, max_value;
  min_value = max_value = verts[0];
  for (size_t i = 1; i < verts.size(); ++i) {
    auto &cur = verts[i];
    D3DXVec4Minimize(&min_value, &min_value, &cur);
    D3DXVec4Maximize(&max_value, &max_value, &cur);
  }

  *center = 0.5f * (min_value + max_value);
  *extents = max_value - *center;
}


static D3DXVECTOR4 max_to_dx(const FbxDouble4 &m, bool neg_w) {
  return D3DXVECTOR4((float)m[0], (float)m[2], (float)m[1], (neg_w ? -1 : 1) * (float)m[3]);
}

static D3DXVECTOR4 max_to_dx(const FbxDouble4 &m) {
  return D3DXVECTOR4((float)m[0], (float)m[2], (float)m[1], (float)m[3]);
}

static D3DXVECTOR3 max_to_dx(const FbxDouble3 &m) {
  return D3DXVECTOR3((float)m[0], (float)m[2], (float)m[1]);
}

static D3DXMATRIX max_to_dx(const FbxAMatrix &mtx) {
  D3DXMATRIX mtx_s, mtx_r, mtx_t;

  auto s = max_to_dx(mtx.GetS());
  auto q = max_to_dx(mtx.GetQ(), true);
  auto t = max_to_dx(mtx.GetT());

  return 
    *D3DXMatrixScaling(&mtx_s, (float)s[0], (float)s[1], (float)s[2]) * 
    *D3DXMatrixRotationQuaternion(&mtx_r, &D3DXQUATERNION((float)q[0], (float)q[1], (float)q[2], (float)q[3])) *
    *D3DXMatrixTranslation(&mtx_t, (float)t[0], (float)t[1], (float)t[2]);
}

static void extract_prs(const FbxAMatrix &mtx, D3DXVECTOR3 *pos, D3DXVECTOR4 *rot, D3DXVECTOR3 *scale) {
  *pos = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(mtx.GetT()));
  *rot = max_to_dx(mtx.GetQ(), true);
  *scale = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(mtx.GetS()));
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
#pragma warning(suppress: 4355)
  , _stats(this)
{
  memset(_indent_buffer, ' ', cMaxIndent);
  _indent_buffer[cMaxIndent] = '\0';
}

#define CHECK_FATAL(x, msg, ...) if (!(x)) {_errors.push_back(string("!! ") + __FUNCTION__ + string(": ") + to_string(msg, __VA_ARGS__)); return false; }

bool FbxConverter::convert(const char *src, const char *dst) {

  DWORD start_time = timeGetTime();
  DWORD end_time;
  _stats.reset();

  // tihi, my c++ looks like JS :)
  bool res = [&]() -> bool {

    DEFER([&] { end_time = timeGetTime(); } );
    add_info("===================================================================");
    _log_file = fopen((string(dst) + ".log").c_str(), "wt");
    if (!_log_file) {
      add_error("unable to open log file!");
      return false;
    }
    add_info("Input file: %s\nOutput file: %s", g_src.c_str(), g_dst.c_str());

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

  if (!process_animation(node, true, &light->is_static))
    return false;

  return true;
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

  bool static_camera;
  if (!process_animation(node, true, &static_camera)) {
    add_error("error processing animation: %s", c->name.c_str());
    return false;
  }

  FbxNode *target = node->GetTarget();
  if (!target) {
    add_error("No target found for camera: %s", node->GetName());
    return false;
  }

  bool static_target;
  if (!process_animation(target, true, &static_target)) {
    add_error("error processing animation: %s", target->GetName());
    return false;
  }

  c->is_static = static_camera | static_target;

  _scene.cameras.push_back(move(c));
  return true;
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


template<class T>
bool is_static_animation(const vector<T> &org, const function<float(const T& a, const T& b)> &distance) {
  for (size_t i = 0; i < org.size() - 1; ++i) {
    if (distance(org[i], org[i+1]) > 0.001f)
      return false;
  }
  return true;
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

bool FbxConverter::process_animation(FbxNode *node, bool translation_only, bool *is_static) {
  INFO_SCOPE;

  if (!gExportAnimation) {
    *is_static = true;
    return true;
  }

  FbxAMatrix a = GetGeometry(node);

  vector<D3DXVECTOR3> pos;
  vector<D3DXVECTOR4> rot;
  vector<D3DXVECTOR3> scale;

  vector<SamplePoint<D3DXVECTOR3>> pos_samples;
  vector<SamplePoint<D3DXVECTOR4>> rot_samples;
  vector<SamplePoint<D3DXVECTOR3>> scale_samples;

  int num_frames = (int)(_duration_ms * _fps / 1000 + 0.5f);
  D3DXQUATERNION prev_rotation;
  for (int i = 0; i <= num_frames; ++i) {
    FbxTime cur;
    double cur_time = i*_duration/num_frames;
    cur.SetSecondDouble(cur_time);
    FbxAMatrix m = GetGlobalPosition(node, cur) * a;
    D3DXVECTOR3 p = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetT()));
    float t= (float)cur_time;
    pos_samples.push_back(SamplePoint<D3DXVECTOR3>(t, p));
    pos.emplace_back(p);
    D3DXVECTOR4 r = max_to_dx(m.GetQ(), true);
    D3DXQUATERNION q(r.x, r.y, r.z, r.w);
    D3DXQuaternionNormalize(&q, &q);
    if (i > 0) {
      // flip the quat if it isn't in the same quadrant as the previous one
      if (D3DXQuaternionDot(&q, &prev_rotation) < 0) {
        q = -q;
      }
    }
    prev_rotation = q;
    r = D3DXVECTOR4(q.x, q.y, q.z, q.w);
    rot_samples.push_back(SamplePoint<D3DXVECTOR4>(t, r));
    rot.emplace_back(r);

    D3DXVECTOR3 s = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetS()));
    scale_samples.push_back(SamplePoint<D3DXVECTOR3>(t,s));
    scale.emplace_back(s);
  }

  auto vec3_dist = [&](const D3DXVECTOR3 &a, const D3DXVECTOR3 &b) -> float { 
    return D3DXVec3Length(&(a-b));
  };

  auto vec4_dist = [&](const D3DXVECTOR4 &a, const D3DXVECTOR4 &b) -> float { 
    return D3DXVec4Length(&(a-b));
  };

  D3DXVECTOR3 vel[2];
  vel[0] = pos[1] - pos[0];
  vel[1] = pos[2] - pos[1];

  bool vel_change = false;
  bool acc_change = false;
  D3DXVECTOR3 prev_acc = vel[1] - vel[0];
  D3DXVECTOR3 prev_vel = pos[1] - pos[0];
  for (size_t i = 2; i < pos.size(); ++i) {
    D3DXVECTOR3 cur_vel = pos[i] - pos[i-1];
    D3DXVECTOR3 cur_acc = cur_vel - vel[1];

    for (int j = 0; j < 3; ++j) {
      int s0 = sign(cur_vel[j]);
      int s1 = sign(prev_vel[j]);
      if (s0 > 0 && s1 < 0 || s0 < 0 && s1 > 0)
        vel_change = true;
    }

    prev_acc = cur_acc;
    prev_vel = cur_vel;

    vel[0] = vel[1];
    vel[1] = cur_vel;
  }
  if (vel_change)
    add_verbose("change in vel for %s", node->GetName());

  bool static_pos = is_static_animation<D3DXVECTOR3>(pos, vec3_dist);
  bool static_rot = is_static_animation<D3DXVECTOR4>(rot, vec4_dist);
  bool static_scale = is_static_animation<D3DXVECTOR3>(scale, vec3_dist);

  if (static_pos && (translation_only || (static_rot && static_scale))) {
    *is_static = true;
    return true;
  }

  auto pos_segments = linear_fit(pos_samples, 1);
  auto scale_segments = linear_fit(scale_samples, 0.1);
  auto rot_segments = linear_fit(rot_samples, 0.01);

  *is_static = false;

  Animation &anim = _animation[node->GetName()];

  if (g_optimize_spline) {

  } else {

/*
    if (!static_pos) {
      Wm5::BSplineCurveFit<float> fitter(3, pos.size(), (float *)pos.data(), 3, g_num_control_points);
      anim.pos_control_points.resize(g_num_control_points);
      memcpy(anim.pos_control_points.data(), fitter.GetControlData(), g_num_control_points * sizeof(D3DXVECTOR3));
    }

    if (!static_rot) {
      Wm5::BSplineCurveFit<float> fitter(4, rot.size(), (float *)rot.data(), 3, g_num_control_points);
      anim.rot_control_points.resize(g_num_control_points);
      memcpy(anim.rot_control_points.data(), fitter.GetControlData(), g_num_control_points * sizeof(D3DXVECTOR4));
    }

    if (!static_scale) {
      Wm5::BSplineCurveFit<float> fitter(3, scale.size(), (float *)scale.data(), 3, g_num_control_points);
      anim.scale_control_points.resize(g_num_control_points);
      memcpy(anim.scale_control_points.data(), fitter.GetControlData(), g_num_control_points * sizeof(D3DXVECTOR3));
    }
*/
    if (!static_pos) {
      for (size_t i = 0; i < pos_segments.size(); ++i) {
        anim.pos.push_back(KeyFrameVec3(pos_segments[i].p0.t, pos_segments[i].p0.value));
      }
    }

    if (!static_rot) {
      for (size_t i = 0; i < rot_segments.size(); ++i) {
        anim.rot.push_back(KeyFrameVec4(rot_segments[i].p0.t, rot_segments[i].p0.value));
      }
    }

    if (!static_scale) {
      for (size_t i = 0; i < scale_segments.size(); ++i) {
        anim.scale.push_back(KeyFrameVec3(scale_segments[i].p0.t, scale_segments[i].p0.value));
      }
    }


  }
/*
  add_verbose("%s - %d pos frames, %d rot frames, %d scale frames", node->GetName(), 
    static_pos ? 0 : pos.size(),
    static_rot ? 0 : rot.size(),
    static_scale ? 0 : scale.size());
*/
/*
  FbxTime cur;
  FbxAMatrix m;
  for (size_t i = 0; i < pos.size(); ++i) {
    double cur_time = i*_duration/num_frames;
    m = anim_cache[cur_time];
    if (!static_pos)
      anim.pos.emplace_back(KeyFrameVec3(cur_time, drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetT()))));

    if (!translation_only) {
      if (!static_rot)
        anim.rot.emplace_back(KeyFrameVec4(cur_time, max_to_dx(m.GetQ(), true)));

      if (!static_scale)
        anim.scale.emplace_back(KeyFrameVec3(cur_time, drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(m.GetS()))));
    }
  }
*/
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

  int tangentRefMode = layer->GetTangents() ? layer->GetTangents()->GetReferenceMode() : -1;
  int binormalRefMode = layer->GetBinormals() ? layer->GetBinormals()->GetReferenceMode() : -1;

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

  Mesh *mesh = new Mesh(fbx_node->GetName());

  bool isStatic;
  if (!process_animation(fbx_node, false, &isStatic))
    return false;

  mesh->is_static = isStatic;

  FbxAMatrix mtxTransform = fbx_node->EvaluateGlobalTransform();
  FbxAMatrix mtxTransformNormal(mtxTransform);
  mtxTransformNormal.SetT(FbxVector4(0,0,0,1));

  extract_prs(mtxTransform, &mesh->pos, &mesh->rot, &mesh->scale);

  // If the object is static, then apply the rotational part of its transform
  // but leave the translation alone
  if (isStatic) {
    mesh->rot = D3DXVECTOR4(0,0,0,1);
  }

  if (polys_by_material.size() > 1)
    add_verbose("%Iu submesh%s", polys_by_material.size(), polys_by_material.size() == 1 ? "" : "es");

  vector<D3DXVECTOR3> verts;
  verts.reserve(fbx_mesh->GetPolygonVertexCount());

  // each material used for the mesh creates a sub mesh
  for (size_t i = 0; i < polys_by_material.size(); ++i) {

    uint32 vertex_flags = SubMesh::kPos | SubMesh::kNormal;

    if (tangentRefMode != -1) {
      // Add tangent space if the mesh has a bump mapped material
      FbxSurfaceMaterial *material = fbx_node->GetMaterial(i);
      const char *name = material->GetName();
      string filename;
      GetMaterialProperty(material, FbxSurfaceMaterial::sBump, FbxSurfaceMaterial::sBumpFactor, &filename);
      if (!filename.empty())
        vertex_flags |= SubMesh::kTex0 | SubMesh::kTangentSpace;
    }

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
    set<VertexElement> super_verts;

    const string material_name = use_default_material ? "default-material" : fbx_node->GetMaterial(i)->GetName();
    string submesh_name = polys_by_material.size() > 1 ? to_string("%s_%d", fbx_node->GetName(), i) : fbx_node->GetName();
    SubMesh *sub_mesh = new SubMesh(mesh, submesh_name, material_name, vertex_flags);
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

        int polyVtx = fbx_mesh->GetPolygonVertex(poly_idx, k);
        auto fbxPos = fbx_mesh->GetControlPointAt(polyVtx);
        FbxVector4 fbxNormal;
        fbx_mesh->GetPolygonVertexNormal(poly_idx, k, fbxNormal);

        // if the mesh is static, rotate it
        if (isStatic) {
          fbxPos = mtxTransformNormal.MultT(fbxPos);
          fbxNormal = mtxTransformNormal.MultT(fbxNormal);
        }

        auto pos3 = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(fbxPos));
        verts.push_back(pos3);
        auto normal3 = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(fbxNormal));
        D3DXVec3Normalize(&normal3, &normal3);

        // Create the super vertex
        VertexElement cand(pos3, normal3);
        for (size_t uv_idx = 0; uv_idx  < uv_sets.size(); ++uv_idx) {
          FbxVector2 uv;
          fbx_mesh->GetPolygonVertexUV(poly_idx, k, uv_sets[uv_idx].c_str(), uv);
          // flip the y-coordinate to match DirectX
          uv[1] = 1 - uv[1];
          if (uv_idx == 0) {
            cand.tex0 = D3DXVECTOR2((float)uv[0], (float)uv[1]);
          } else if (uv_idx == 1) {
            cand.tex1 = D3DXVECTOR2((float)uv[0], (float)uv[1]);
          } else {
            add_error("Too many uv-sets!");
          }
        }

        if (vertex_flags & SubMesh::kTangentSpace) {
          FbxVector4 fbxTangent, fbxBinormal;

          if (tangentRefMode == FbxLayerElement::eDirect) {
            fbxTangent = layer->GetTangents()->GetDirectArray()[polyVtx + k];
          } else {
            int idx = layer->GetTangents()->GetIndexArray()[polyVtx + k];
            fbxTangent = layer->GetTangents()->GetDirectArray()[idx];
          }

          if (binormalRefMode == FbxLayerElement::eDirect) {
            fbxBinormal = layer->GetBinormals()->GetDirectArray()[polyVtx + k];
          } else {
            int idx = layer->GetBinormals()->GetIndexArray()[polyVtx + k];
            fbxBinormal = layer->GetBinormals()->GetDirectArray()[idx];
          }

          if (isStatic) {
            fbxTangent = mtxTransformNormal.MultT(fbxTangent);
            fbxBinormal = mtxTransformNormal.MultT(fbxBinormal);
          }

          cand.tangent = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(fbxTangent));
          cand.binormal = drop<D3DXVECTOR4, D3DXVECTOR3>(max_to_dx(fbxBinormal));
          D3DXVec3Normalize(&cand.tangent, &cand.tangent);
          D3DXVec3Normalize(&cand.binormal, &cand.binormal);
        }

        int idx;
        // Check if the super vertex already exists
        auto it = super_verts.find(cand);
        if (it == super_verts.end()) {
          idx = cand.idx = sub_mesh->vertices.size();
          sub_mesh->vertices.push_back(cand);
          super_verts.insert(cand);
        } else {
          // vertex already exists, so reuse the vertex index
          idx = it->idx;
        }
        sub_mesh->indices.push_back(idx);
      }
    }

    if (gOptimizeMesh) {
      vector<int> optimzedIndices;
      vector<int> vertex_reorder;
      VertexOptimizer opt(sub_mesh->vertices.size());
      opt.AddTriangles(sub_mesh->indices.data(), sub_mesh->indices.size(), &optimzedIndices, &vertex_reorder);
      assert(sub_mesh->indices.size() == optimzedIndices.size());

      // we need to reorder the pos/normal etc now to match the new optimized order
      vector<VertexElement> optimzedVerts(sub_mesh->vertices.size());

      for (size_t i = 0; i < sub_mesh->vertices.size(); ++i) {
        int idx = vertex_reorder[i];
        optimzedVerts[idx] = sub_mesh->vertices[i];
      }

      sub_mesh->vertices = optimzedVerts;
      sub_mesh->indices = optimzedIndices;
    }
  }

  // calc center and extents
  compute_extents(verts, &mesh->center, &mesh->extents);

  // If the mesh is static, move the vertices so the center of the AABB is at (0,0,0), and
  // apply that translation to the obj->world matrix
  if (isStatic) {
    D3DXVECTOR3 ofs(mesh->center);
    for (size_t i = 0; i < mesh->sub_meshes.size(); ++i) {
      SubMesh *submesh = mesh->sub_meshes[i].get();
      auto &verts = submesh->vertices;
      size_t numVerts = verts.size();
      for (size_t j = 0; j < numVerts; ++j) {
        verts[j].pos -= ofs;
      }
    }
    mesh->pos += mesh->center;
    mesh->center = D3DXVECTOR3(0,0,0);
  }

  _scene.meshes.push_back(unique_ptr<Mesh>(mesh));

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

#define CHECKED_CALL(x) if (!(x)) { add_error("Error in " #x); return false; }

  CHECKED_CALL(_writer->open(dst));

  MainHeader header;
  header.version = FILE_VERSION;
  _writer->push_pos();
  _writer->write(header);
  header.compressedVertices = gCompressVertices;
  header.position_bits = gPositionBits;
  header.normal_bits = gNormalBits;
  header.texcoord_bits = gTexCoordBits;
  header.animation_bits = gAnimationBits;

  header.global_ofs = _writer->pos();
  CHECKED_CALL(save_globals());

  header.material_ofs = _writer->pos();
  CHECKED_CALL(save_materials());

  CHECKED_CALL(save_material_info());

  header.mesh_ofs = _writer->pos();
  CHECKED_CALL(save_meshes());

  header.light_ofs = _writer->pos();
  CHECKED_CALL(save_lights());

  header.camera_ofs = _writer->pos();
  CHECKED_CALL(save_cameras());

  header.animation_ofs = _writer->pos();
  CHECKED_CALL(save_animations());

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

  _stats.print();

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

bool FbxConverter::save_animations2(const std::vector<KeyFrameVec3> &frames) {
  _writer->write((int)frames.size());
  for (size_t i = 0; i < frames.size(); ++i) {
    _writer->write((float)frames[i].time);
    _writer->write(frames[i].value);
  }
  return true;
}

bool FbxConverter::save_animations2(const std::vector<KeyFrameVec4> &frames) {
  _writer->write((int)frames.size());
  for (size_t i = 0; i < frames.size(); ++i) {
    _writer->write((float)frames[i].time);
    _writer->write(frames[i].value);
  }
  return true;
}

bool FbxConverter::save_animations(const vector<KeyFrameVec3> &frames) {

  BitWriter writer;

  writer.write_varint(frames.size());
  if (frames.size() > 0) {

    vector<D3DXVECTOR3> values(frames.size());
    for (size_t i = 0; i < frames.size(); ++i)
      values[i] = frames[i].value;

    D3DXVECTOR3 center, extents;
    compute_extents(values, &center, &extents);

    writer.write(*(uint32 *)&center.x, 32);
    writer.write(*(uint32 *)&center.y, 32);
    writer.write(*(uint32 *)&center.z, 32);

    writer.write(*(uint32 *)&extents.x, 32);
    writer.write(*(uint32 *)&extents.y, 32);
    writer.write(*(uint32 *)&extents.z, 32);

    for (size_t i = 0; i < frames.size(); ++i) {
      float t = (float)frames[i].time;
      writer.write(*(uint32 *)&t, 32);
      const D3DXVECTOR3 &cur = frames[i].value;
      writer.write(quantize((cur.x - center.x) / extents.x, gAnimationBits), gAnimationBits);
      writer.write(quantize((cur.y - center.y) / extents.y, gAnimationBits), gAnimationBits);
      writer.write(quantize((cur.z - center.z) / extents.z, gAnimationBits), gAnimationBits);
    }
  }

  uint32 bit_len;
  uint8 *buf;
  writer.get_stream(&buf, &bit_len);
  _writer->add_deferred_binary(buf, (bit_len + 7) / 8);

  return true;
}

bool FbxConverter::save_animations(const vector<KeyFrameVec4> &frames) {

  BitWriter writer;

  writer.write_varint(frames.size());
  if (frames.size() > 0) {

    vector<D3DXVECTOR4> values(frames.size());
    for (size_t i = 0; i < frames.size(); ++i)
      values[i] = frames[i].value;

    D3DXVECTOR4 center, extents;
    compute_extents(values, &center, &extents);

    writer.write(*(uint32 *)&center.x, 32);
    writer.write(*(uint32 *)&center.y, 32);
    writer.write(*(uint32 *)&center.z, 32);
    writer.write(*(uint32 *)&center.w, 32);

    writer.write(*(uint32 *)&extents.x, 32);
    writer.write(*(uint32 *)&extents.y, 32);
    writer.write(*(uint32 *)&extents.z, 32);
    writer.write(*(uint32 *)&extents.w, 32);

    for (size_t i = 0; i < frames.size(); ++i) {
      float t = (float)frames[i].time;
      writer.write(*(uint32 *)&t, 32);
      const D3DXVECTOR4 &cur = frames[i].value;
      writer.write(quantize((cur.x - center.x) / extents.x, gAnimationBits), gAnimationBits);
      writer.write(quantize((cur.y - center.y) / extents.y, gAnimationBits), gAnimationBits);
      writer.write(quantize((cur.z - center.z) / extents.z, gAnimationBits), gAnimationBits);
      writer.write(quantize((cur.w - center.w) / extents.w, gAnimationBits), gAnimationBits);
    }
  }

  uint32 bit_len;
  uint8 *buf;
  writer.get_stream(&buf, &bit_len);
  _writer->add_deferred_binary(buf, (bit_len + 7) / 8);

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
/*
      // pos
      int num_pts = (int)anim.pos_control_points.size();
      _writer->write(num_pts);
      if (num_pts)
        _writer->write_raw(anim.pos_control_points.data(), num_pts * sizeof(D3DXVECTOR3));

      // rot
      num_pts = (int)anim.rot_control_points.size();
      _writer->write(num_pts);
      if (num_pts > 0)
        _writer->write_raw(anim.rot_control_points.data(), num_pts * sizeof(D3DXVECTOR4));

      // scale
      num_pts = (int)anim.scale_control_points.size();
      _writer->write(num_pts);
      if (num_pts > 0)
        _writer->write_raw(anim.scale_control_points.data(), num_pts * sizeof(D3DXVECTOR3));
*/

      save_animations2(anim.pos);
      save_animations2(anim.rot);
      save_animations2(anim.scale);

    }
  }

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
    _writer->write(camera->is_static);
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
    _writer->write(light->is_static);
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
    _writer->write(mesh->pos);
    _writer->write(mesh->rot);
    _writer->write(mesh->scale);
    _writer->write(mesh->center);
    _writer->write(mesh->extents);
    _writer->write(mesh->is_static);

    _writer->write((int)mesh->sub_meshes.size());
    for (size_t j = 0; j < mesh->sub_meshes.size(); ++j) {
      SubMesh *sub = mesh->sub_meshes[j].get();
      _writer->add_deferred_string(sub->name);
      _writer->add_deferred_string(sub->material);

      int index_size;
      BitWriter vb_writer;
      BitWriter ib_writer;

      compact_vertex_data(sub, &vb_writer);
      compact_index_data(sub, &ib_writer, &index_size);

      _writer->write(sub->vertex_flags);
      _writer->write(sub->element_size);
      _writer->write(index_size);

      _writer->write((int)sub->vertices.size());
      _writer->write((int)sub->indices.size());

      uint8 *vb, *ib;
      uint32 vb_len, ib_len;
      vb_writer.get_stream(&vb, &vb_len);
      ib_writer.get_stream(&ib, &ib_len);
      _writer->add_deferred_binary(vb, (vb_len + 7) / 8);
      _writer->add_deferred_binary(ib, (ib_len + 7) / 8);

      _stats.num_indices += sub->indices.size();
      _stats.num_verts += sub->vertices.size();
      _stats.vert_data_size += (vb_len + 7) / 8;
      _stats.index_data_size += (ib_len + 7) / 8;

      if (ib_len > sub->indices.size() * 10)
        int a = 10;

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

int parseCommandLine(LPSTR lpCmdLine) {
  vector<string> tokens;
  boost::split(tokens, lpCmdLine, boost::is_any_of("\t "));

  char module_name[MAX_PATH];
  GetModuleFileName(NULL, module_name, sizeof(module_name));

  int argc = (int)tokens.size();

  int argCount = 0;
  int num_args = argc - 1;
  for (int i = 0; i < num_args; ++i) {

    if (tokens[i] == "--verbose") {
      argCount++;
      g_verbose = true;
    }

    if (tokens[i] == "--watch") {
      argCount++;
      g_file_watch = true;
    }

    if (tokens[i] == "--compress-verts") {
      argCount++;
      gCompressVertices = true;
    }

    if (tokens[i] == "--no-anim") {
      argCount++;
      gExportAnimation = false;
    }

    if (tokens[i] == "--no-opt") {
      argCount++;
      gOptimizeMesh = false;
    }

    if (tokens[i] == "--pos-bits" && i != num_args - 1) {
      argCount += 2;
      gPositionBits = atoi(tokens[++i].c_str());
    }

    if (tokens[i] == "--normal-bits" && i != num_args - 1) {
      argCount += 2;
      gNormalBits = atoi(tokens[++i].c_str());
    }

    if (tokens[i] == "--tex-bits" && i != num_args - 1) {
      argCount += 2;
      gTexCoordBits = atoi(tokens[++i].c_str());
    }

    if (tokens[i] == "--anim-bits" && i != num_args - 1) {
      argCount += 2;
      gAnimationBits = atoi(tokens[++i].c_str());
    }
  }

  int numFiles = argc - argCount;
  if (numFiles < 1) {
    printf("syntax: %s [--verbose] [--watch] [--no-anim] [--no-opt] [--pos-bits NN] [--normal-bits NN] [--tex-bits NN] [--anim-bits NN] src [dst]", module_name);
    return 1;
  }

  if (numFiles == 1) {
    g_src = g_inputDir + tokens[argc-1] + ".fbx";
    g_dst = g_outputDir + tokens[argc-1] + ".kumi";
  } else {
    g_src = tokens[argc-2];
    g_dst = tokens[argc-1];
  }

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


void FbxConverter::compact_vertex_data(const SubMesh *submesh, BitWriter *writer) {
  int len = submesh->element_size * submesh->vertices.size();

  int vertexFlags = submesh->vertex_flags;

  Mesh *mesh = submesh->mesh;

  if (gCompressVertices) {

    for (size_t i = 0; i < submesh->vertices.size(); ++i) {

      auto &vtx = submesh->vertices[i];
      // save vertices as 14 bit
      D3DXVECTOR3 ofs = vtx.pos - mesh->center;
      ofs.x /= (fabs(mesh->extents.x) > 0.001f ? mesh->extents.x : 1);
      ofs.y /= (fabs(mesh->extents.y) > 0.001f ? mesh->extents.y : 1);
      ofs.z /= (fabs(mesh->extents.z) > 0.001f ? mesh->extents.z : 1);

      assert(ofs.x >= -1 && ofs.x <= 1);
      assert(ofs.y >= -1 && ofs.y <= 1);
      assert(ofs.z >= -1 && ofs.z <= 1);

      writer->write(quantize(ofs.x, gPositionBits), gPositionBits);
      writer->write(quantize(ofs.y, gPositionBits), gPositionBits);
      writer->write(quantize(ofs.z, gPositionBits), gPositionBits);

      auto &writeNormal = [&](const D3DXVECTOR3 &n) {
        // save normal as 11 bit x/y, and 1 sign bit for z
        writer->write(quantize(n.x, gNormalBits), gNormalBits);
        writer->write(quantize(n.y, gNormalBits), gNormalBits);
        writer->write(n.z < 0 ? 1 : 0, 1);
      };
      writeNormal(vtx.normal);
      if (vertexFlags & SubMesh::kTangentSpace) {
        writeNormal(vtx.tangent);
        writeNormal(vtx.binormal);
      }

      if (vertexFlags & SubMesh::kTex0) {
        // 11 bit u/v
        writer->write(quantize(vtx.tex0.x, gTexCoordBits), gTexCoordBits);
        writer->write(quantize(vtx.tex0.y, gTexCoordBits), gTexCoordBits);
      }

      if (vertexFlags & SubMesh::kTex1) {
        writer->write(quantize(vtx.tex1.x, gTexCoordBits), gTexCoordBits);
        writer->write(quantize(vtx.tex1.y, gTexCoordBits), gTexCoordBits);
      }
    }

  } else {

    auto &writeFloat = [&](float a) { writer->write(*(uint32 *)&a, 32); };
    auto &writeVector2 = [&](const D3DXVECTOR2 &v) { writeFloat(v.x); writeFloat(v.y); };
    auto &writeVector3 = [&](const D3DXVECTOR3 &v) { writeFloat(v.x); writeFloat(v.y); writeFloat(v.z); };
    for (size_t i = 0; i < submesh->vertices.size(); ++i) {

      auto &vtx = submesh->vertices[i];

      writeVector3(vtx.pos);
      writeVector3(vtx.normal);

      if (vertexFlags & SubMesh::kTangentSpace) {
        writeVector3(vtx.tangent);
        writeVector3(vtx.binormal);
      }

      if (vertexFlags & SubMesh::kTex0) {
        writeVector2(vtx.tex0);
      }

      if (vertexFlags & SubMesh::kTex1) {
        writeVector2(vtx.tex1);
      }

    }


  }

}


void FbxConverter::compact_index_data(const SubMesh *submesh, BitWriter *writer, int *index_size) {

  // check if we need 16 or 32 bit indices
  const size_t num_verts = submesh->vertices.size();
  const bool need_32_bit = num_verts >= (1 << 16);
  *index_size = need_32_bit ? 4 : 2;

  // compute deltas for indices, zig-zag encode, and save as varints
  auto &indices = submesh->indices;
  writer->write_varint(zigzag_encode(indices[0]));
  int prev = indices[0];
  for (size_t i = 1; i < indices.size(); ++i) {
    int cur = indices[i];
    int delta = cur - prev;
    int zz = zigzag_encode(delta);
    writer->write_varint(zz);
    prev = cur;
  }
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

bool fileExists(const char *filename)
{
  struct _stat status;
  return _access(filename, 0) == 0 && _stat(filename, &status) == 0 && (status.st_mode & _S_IFREG);
}

void findAppRoot()
{
  char startingDir[MAX_PATH];
  _getcwd(startingDir, MAX_PATH);

  // keep going up directory levels until we find "app.json", or we hit the bottom..
  char prevDir[MAX_PATH], curDir[MAX_PATH];
  ZeroMemory(prevDir, sizeof(prevDir));

  while (true) {
    _getcwd(curDir, MAX_PATH);
    // check if we haven't moved
    if (!strcmp(curDir, prevDir))
      break;

    memcpy(prevDir, curDir, MAX_PATH);

    if (fileExists("fbx_conv.ini")) {
      gAppRoot = curDir;
      return;
    }

    if (_chdir("..") == -1)
      break;
  }
  gAppRoot = startingDir;
}




int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd) {

  AllocConsole();
  freopen("CONOUT$","wb",stdout);
  freopen("CONOUT$","wb",stderr);

  findAppRoot();

  char iniFile[MAX_PATH];
  _getcwd(iniFile, sizeof(iniFile));
  strcat(iniFile, "\\fbx_conv.ini");

  char inputDir[MAX_PATH], outputDir[MAX_PATH];
  int inputLen = GetPrivateProfileString("Settings", "InputDir", "", inputDir, sizeof(inputDir), iniFile);
  int outputLen = GetPrivateProfileString("Settings", "OutputDir", "", outputDir, sizeof(outputDir), iniFile);

  if (inputLen && outputLen) {
    g_inputDir = inputDir;
    g_outputDir = outputDir;
    auto &normalize = [&](string *str) {
      if (str->back() != '\\' && str->back() != '/')
        (*str) += "\\";
    };
    normalize(&g_inputDir);
    normalize(&g_outputDir);
  }

  HANDLE console_handle = GetStdHandle(STD_INPUT_HANDLE);

  if (parseCommandLine(lpCmdLine))
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

  printf("Done.\n");
  while (!is_bit_set(GetAsyncKeyState(VK_ESCAPE), 15))
    ;

  FreeConsole();

  return msg.wParam;
}
