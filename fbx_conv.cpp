// converter between .fbx and .kumi format

#include "stdafx.h"

#define FILE_VERSION 4

typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;

using namespace std;
using namespace stdext;

class FbxConverter;

#define USE_DIRECTX_SYSTEM 0

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

#define SAFE_DELETE(x) if( (x) != 0 ) { delete (x); (x) = 0; }

#define GEN_NAME2(prefix, line) prefix##line
#define GEN_NAME(prefix, line) GEN_NAME2(prefix, line)
#define MAKE_SCOPED(type) type GEN_NAME(ANON, __COUNTER__)

#define RANGE(x) begin(x), end(x)

#if _DEBUG
#pragma comment(lib, "fbxsdk-2013.1-mdd.lib")
#else
#pragma comment(lib, "fbxsdk-2013.1-md.lib")
#endif

#pragma comment(lib, "d3dx10.lib")


#pragma pack(push, 1)
struct MainHeader {
  int version;
  int material_ofs;
  int mesh_ofs;
  int light_ofs;
  int camera_ofs;
  int animation_ofs;
  int binary_ofs;
  int total_size;
};

namespace BlockId {
  enum Enum {
    kMaterials,
    kMeshes,
    kCameras,
    kLights,
    kAnimation,
  };
}

struct BlockHeader {
  BlockId::Enum id;
  int size;	// size excl header
};
#pragma pack(pop)

template<class T> 
void seq_delete(T* t) {
  for (auto it = t->begin(); it != t->end(); ++it)
    delete *it;
  t->clear();
}

template<class T> 
void assoc_delete(T* t) {
  for (T::iterator it = t->begin(); it != t->end(); ++it)
    delete it->second;
  t->clear();
}

class Writer {
public:
  Writer() : _f(nullptr) {}
  ~Writer() {
    if (_f)
      fclose(_f);
  }

  struct DeferredBinary {
    DeferredBinary(int ofs, const void *d, int len, bool save_len)
      : ofs(ofs)
      , save_len(save_len) {
        data.resize(len);
        memcpy(&data[0], d, len);
    }
    int ofs;
    bool save_len;
    vector<uint8> data;
  };

  bool open(const char *filename) {
    if (!(_f = fopen(filename, "wb")))
      return false;
    return true;
  }

  void close() {
    fclose(_f);
  }

  void add_deferred_string(const string &str) {
    int p = pos();
    _deferred_binary.push_back(DeferredBinary(p, str.data(), str.size() + 1, false));
    write(p);
  }

  void add_deferred_binary(void *data, int len) {
    int p = pos();
    _deferred_binary.push_back(DeferredBinary(p, data, len, true));
    write(p);
  }

  void save_binary() {
    // we assume that the binary data is going to be transient,
    // so all the offsets are relative the binary block
    int binary_start = pos();
    write((int)_deferred_binary.size());
    for (size_t i = 0; i < _deferred_binary.size(); ++i)
      write(_deferred_binary[i].ofs);

    for (size_t i = 0; i < _deferred_binary.size(); ++i) {
      // the offset is relative the start of the binary block
      auto &cur = _deferred_binary[i];
      int ofs = pos() - binary_start;
      int len = (int)cur.data.size();
      if (cur.save_len)
        write(len);
      write_raw(&cur.data[0], len);
      push_pos();
      set_pos(cur.ofs);
      write(ofs);
      pop_pos();
    }
  }

  template <class T>
  void write(const T& data) const {
    fwrite(&data, 1, sizeof(T), _f);
  }

  void write_raw(const void *data, int len) {
    fwrite(data, 1, len, _f);
  }

  void push_pos() {
    _file_pos_stack.push_back(ftell(_f));
  }

  void pop_pos() {
    assert(!_file_pos_stack.empty());
    int p = _file_pos_stack.back();
    _file_pos_stack.pop_back();
    fseek(_f, p, SEEK_SET);
  }

  int pos() {
    return ftell(_f);
  }

  void set_pos(int p) {
    fseek(_f, p, SEEK_SET);
  }

  void push_exch() {
    // push the current position, and move to the last position on the stack
    assert(!_file_pos_stack.empty());

    int p = ftell(_f);
    pop_pos();
    _file_pos_stack.push_back(p);
  }

private:
  vector<DeferredBinary> _deferred_binary;

  deque<int> _file_pos_stack;
  FILE *_f;
};

struct ScopedBlock {
  ScopedBlock(BlockHeader &header, Writer &writer) : header(header), writer(writer) {
    writer.push_pos();
    writer.write(header);
    block_start = writer.pos();
  }

  ~ScopedBlock() {
    header.size = writer.pos() - block_start;
    writer.push_exch();
    writer.write(header);
    writer.pop_pos();
  }

  BlockHeader &header;
  Writer &writer;
  int block_start;
};

namespace Property {
  enum Type {
    kUnknown,
    kFloat,
    kFloat2,
    kFloat3,
    kFloat4,
    kFloat4x4,
    kInt,
  };
}

struct MaterialProperty {
  MaterialProperty(const string &name, int value) : name(name), type(Property::kInt), _int(value) {}
  MaterialProperty(const string &name, FbxDouble value) : name(name), type(Property::kFloat) { _float[0] = (float)value; }
  MaterialProperty(const string &name, FbxDouble3 value) : name(name), type(Property::kFloat3) { for (int i = 0; i < 3; ++i) _float[i] = (float)value[i]; _float[3] = 0; }
  MaterialProperty(const string &name, FbxDouble4 value) : name(name), type(Property::kFloat4) { for (int i = 0; i < 4; ++i) _float[i] = (float)value[i]; }
  string name;
  Property::Type type;
  union {
    int _int;
    float _float[4];
  };
};

struct Material {
  Material(const string &name) : name(name) {}
  string name;
  string technique;
  vector<MaterialProperty> properties;
};

struct SuperVertex {
  SuperVertex(const FbxVector4 &pos, const FbxVector4 &normal) : pos(pos), normal(normal), idx(-1) {}
  FbxVector4 pos;
  FbxVector4 normal;
  vector<FbxVector2> uv;
  int idx;

  friend bool operator<(const SuperVertex &a, const SuperVertex &b) {
    // strict weak ordering
    for (int i = 0; i < 3; ++i) {
      if (a.pos[i] < b.pos[i]) 
        return true;
      if (b.pos[i] < a.pos[i])
        return false;
    }

    for (int i = 0; i < 3; ++i) {
      if (a.normal[i] < b.normal[i])
        return true;
      if (b.normal[i] < a.normal[i])
        return false;
    }

    for (size_t i = 0; i < a.uv.size(); ++i) {
      for (int j = 0; j < 2; ++j) {
        if (a.uv[i][j] < b.uv[i][j])
          return true;
        if (b.uv[i][j] < a.uv[i][j])
          return false;
      }
    }
    return false;
  }
};


#if USE_DIRECTX_SYSTEM
FbxDouble4 max_to_dx(const FbxDouble4 &m, bool neg_w = false) {
  return FbxDouble4(m[0], m[1], m[2], (neg_w ? -1 : 1) * m[3]);
}

FbxDouble3 max_to_dx(const FbxDouble3 &m) {
  return m;
}
#else
FbxDouble4 max_to_dx(const FbxDouble4 &m, bool neg_w = false) {
  return FbxDouble4(m[0], m[2], m[1], (neg_w ? -1 : 1) * m[3]);
}

FbxDouble3 max_to_dx(const FbxDouble3 &m) {
  return FbxDouble3(m[0], m[2], m[1]);
}

D3DXVECTOR3 max_to_dx2(const FbxDouble3 &m) {
  return D3DXVECTOR3((float)m[0], (float)m[2], (float)m[1]);
}

#endif
D3DXMATRIX max_to_dx(const FbxAMatrix &mtx) {
  D3DXMATRIX mtx_s, mtx_r, mtx_t;

  FbxDouble4 s = max_to_dx(mtx.GetS());
  FbxDouble4 q = max_to_dx(mtx.GetQ(), true);
  FbxDouble4 t = max_to_dx(mtx.GetT());

  return 
    *D3DXMatrixScaling(&mtx_s, (float)s[0], (float)s[1], (float)s[2]) * 
    *D3DXMatrixRotationQuaternion(&mtx_r, &D3DXQUATERNION((float)q[0], (float)q[1], (float)q[2], (float)q[3])) *
    *D3DXMatrixTranslation(&mtx_t, (float)t[0], (float)t[1], (float)t[2]);
}

struct Vector2 {
  Vector2() {}
  Vector2(const FbxVector2 &v) : x((float)v[0]), y((float)v[1]) {}
  float x, y;
};

struct Vector3 {
  Vector3() {}
  Vector3(const FbxVector4 &v) : x((float)v[0]), y((float)v[1]), z((float)v[2]) {}
  float x, y, z;
};

struct SubMesh {

  enum VertexFlags {
    kPos     = 1 << 0,
    kNormal  = 1 << 1,
    kTex0    = 1 << 2,
    kTex1    = 1 << 3,
  };

  SubMesh(const string &material, uint32 vertex_flags) 
    : material(material), vertex_flags(vertex_flags)
  {
    element_size = vertex_flags & kPos ? sizeof(Vector3) : 0;
    element_size += vertex_flags & kNormal ? sizeof(Vector3) : 0;
    element_size += vertex_flags & kTex0 ? sizeof(Vector2) : 0;
    element_size += vertex_flags & kTex1 ? sizeof(Vector2) : 0;
  }
  string material;
  uint32 vertex_flags;
  uint32 element_size;
  vector<Vector3> pos;
  vector<Vector3> normal;
  vector<Vector2> tex0;
  vector<Vector2> tex1;
  vector<uint32> indices;
};

template<typename T>
T max3(const T &a, const T &b, const T &c) {
  return max(a, max(b, c));
}

template<typename T>
T max4(const T &a, const T &b, const T &c, const T &d) {
  return max(a, max3(b, c, d));
}

void compact_vertex_data(const SubMesh &submesh, void **data, int *len) {
  *len = submesh.element_size * submesh.pos.size();
  char *buf = new char[*len];
  *data = buf;
  for (size_t i = 0; i < submesh.pos.size(); ++i) {
    *(Vector3 *)buf = submesh.pos[i]; buf += sizeof(submesh.pos[0]);
    *(Vector3 *)buf = submesh.normal[i]; buf += sizeof(submesh.normal[0]);
    if (submesh.vertex_flags & SubMesh::kTex0) {
      *(Vector2 *)buf = submesh.tex0[i]; buf += sizeof(submesh.tex0[0]);
    }
    if (submesh.vertex_flags & SubMesh::kTex1) {
      *(Vector2 *)buf = submesh.tex1[i]; buf += sizeof(submesh.tex1[0]);
    }
  }
}

void compact_index_data(const SubMesh &submesh, void **data, int *len, int *index_size) {
  // check if we need 16 or 32 bit indices
  const size_t num_verts = max4(submesh.pos.size(), submesh.normal.size(), submesh.tex0.size(), submesh.tex1.size());
  const size_t elems = submesh.indices.size();
  const bool need_32_bit = num_verts >= (1 << 16);
  *index_size = need_32_bit ? 4 : 2;
  *len = elems * (*index_size);
  void *buf = new char[*len];
  *data = buf;
  if (need_32_bit)
    copy(RANGE(submesh.indices), checked_array_iterator<uint32 *>((uint32 *)buf, elems));
  else
    copy(RANGE(submesh.indices), checked_array_iterator<uint16 *>((uint16 *)buf, elems));
}

struct Mesh {
  Mesh(const string &name) : name(name) {}
  ~Mesh() {
    seq_delete(&sub_meshes);
  }
  string name;
  D3DXMATRIX obj_to_world;
  vector<SubMesh *> sub_meshes;
};


struct Camera {
  string name;
  FbxDouble3 pos, target, up;
  FbxDouble roll;
  FbxDouble aspect_ratio;
  FbxDouble fov;
  FbxDouble near_plane, far_plane;
};

struct Light {
  enum Type {
    Omni,
    Directional,
    Spot
  };

  enum Decay {
    None,
    Linear,
    Quadratic,
    Cubic,
  };

  string name;
  Type type;
  Decay decay;
  FbxDouble3 pos;
  FbxDouble3 color;
  FbxDouble intensity;
};

struct Hierarchy {

};

#pragma pack(push, 1)
template<typename T>
struct KeyFrame {
  KeyFrame(double time, const T& value) : time(time), value(value) {}
  double time;
  T value;
};
/*
struct KeyFrameMtx {
  KeyFrameMtx(double time, const D3DXMATRIX &mtx) : time(time), mtx(mtx) {}
  double time;
  D3DXMATRIX mtx;
};

struct KeyFrameFloat {
  KeyFrameFloat(double time, float value) : time(time), value(value) {}
  double time;
  float value;
};

struct KeyFrameVec3 {
  KeyFrameVec3(double time, const D3DXVECTOR3 &value) : time(time), value(value) {}
  double time;
  D3DXVECTOR3 value;
};
*/


typedef KeyFrame<float> KeyFrameFloat;
typedef KeyFrame<D3DXVECTOR3> KeyFrameVec3;
typedef KeyFrame<D3DXMATRIX> KeyFrameMtx;

#pragma pack(pop)

struct Scene {
  ~Scene() {
    assoc_delete(&materials);
    seq_delete(&lights);
    seq_delete(&cameras);
  }

  typedef map<string, Material *> Materials;
  typedef vector<Mesh *> Meshes;
  typedef vector<Camera *> Cameras;
  typedef vector<Light *> Lights;

  Cameras cameras;
  Lights lights;
  Materials materials;
  vector<shared_ptr<Mesh>> meshes;
};

class FbxConverter {
public:
  FbxConverter();
  ~FbxConverter();
  bool convert(const char *src, const char *dst);
private:

  bool convert_inner(const char *src, const char *dst);

  // like a CSI!
  bool process_scene(FbxScene *scene);

  bool process_mesh(FbxNode *node, FbxMesh *mesh);
  bool process_material(FbxNode *node, int *material_count);
  bool process_camera(FbxNode *node, FbxCamera *camera);
  bool process_light(FbxNode *node, FbxLight *light);

  bool process_animation(FbxNode *node, bool translation_only);
  bool process_position_animation(FbxNode *node);

  bool save_scene(const char *dst);
  bool save_meshes();
  bool save_cameras();

  template<class T> bool save_animations(const map<string, vector<KeyFrame<T>>> &anims);
  bool save_animations();
  bool save_lights();
  bool save_materials();

  void add_error(const char *fmt, ...);
  void add_info(const char *fmt, ...);

  static const int cIndentLevel = 4;
  static const int cMaxIndent = 512;
  void enter_scope() { _indent_level += cIndentLevel; }
  void leave_scope() { _indent_level -= cIndentLevel; }

  struct InfoScope {
    InfoScope() { g_converter->enter_scope(); }
    ~InfoScope() { g_converter->leave_scope(); }
  };

#define INFO_SCOPE MAKE_SCOPED(InfoScope);

  int _indent_level;
  char _indent_buffer[cMaxIndent+1];

  vector<string> _errors;
  map<string, vector<KeyFrameFloat>> _animation_float;
  map<string, vector<KeyFrameVec3>> _animation_vec3;
  map<string, vector<KeyFrameMtx>> _animation_mtx;
  Scene _scene;
  Writer _writer;

  FbxTime _start_time;
  FbxTime _stop_time;

  FbxLongLong _duration_ms;
  double _duration;
  double _fps;

  FbxAnimEvaluator *_evaluator;
  FbxScene *_fbx_scene;
  FbxManager *_mgr;
  FbxIOSettings *_settings;
  FbxGeometryConverter *_converter;
  FbxImporter *_importer;
};

FbxConverter::FbxConverter() 
  : _mgr(FbxManager::Create())
  , _settings(FbxIOSettings::Create(_mgr, IOSROOT))
  , _converter(new FbxGeometryConverter(_mgr))
  , _importer(FbxImporter::Create(_mgr, ""))
  , _indent_level(0)
  , _evaluator(nullptr)
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

bool FbxConverter::convert_inner(const char *src, const char *dst) {

  bool res = _importer->Initialize(src, -1, _settings);
  if (!res) {
    _errors.push_back(to_string("Error calling FbxImporter::Initialize: %s", _importer->GetLastErrorString()));
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
}

bool FbxConverter::convert(const char *src, const char *dst) {

  add_info("===================================================================");
  bool res = convert_inner(src, dst);

  copy(RANGE(_errors), ostream_iterator<string>(cout));

  return res;
}

bool FbxConverter::process_light(FbxNode *node, FbxLight *light) {

  INFO_SCOPE;

  if (!light->CastLight.Get())
    return true;

  FbxAnimEvaluator *evaluator = _fbx_scene->GetEvaluator();

  Light *l = new Light;
  l->name = node->GetName();
  FbxMatrix mtx = evaluator->GetNodeGlobalTransform(node);
  l->pos = max_to_dx(FbxDouble3(mtx.Get(3,0), mtx.Get(3,1), mtx.Get(3,2)));
  l->color = light->Color.Get();

  _scene.lights.push_back(l);

  if (g_verbose) {
    add_info("found light: %s", l->name.c_str());
  }

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

  Camera *c = new Camera;
  c->name = node->GetName();
  c->pos = max_to_dx(camera->Position.Get());
  c->target = max_to_dx(camera->InterestPosition.Get());
  c->up = max_to_dx(camera->UpVector.Get());
  c->roll = camera->Roll.Get();
  c->aspect_ratio = (float)aspect;
  c->fov = (float)fov_y;
  c->near_plane = camera->NearPlane.Get();
  c->far_plane = camera->FarPlane.Get();

  _scene.cameras.push_back(c);

  if (g_verbose) {
    add_info("found camera: %s", c->name.c_str());
  }

  if (!process_animation(node, true))
    return false;

  FbxNode *target = node->GetTarget();
  if (!target) {
    add_error("No target found for camera: %s", node->GetName());
    return false;
  }

  if (!process_animation(target, true))
    return false;

  return true;
}

bool FbxConverter::process_material(FbxNode *fbx_node, int *material_count) {
  INFO_SCOPE;
  *material_count = fbx_node->GetMaterialCount();
  for (int i = 0; i < *material_count; ++i) {
    FbxSurfaceMaterial *node_material = fbx_node->GetMaterial(i);
    const char *name = node_material->GetName();

    // Skip if we've already seen this material
    if (_scene.materials.find(name) != _scene.materials.end())
      continue;

    if (g_verbose) {
      add_info("found material: %s", name);
    }

    Material *material = new Material(name);
    _scene.materials.insert(make_pair(name, material));

    material->technique = "diffuse";

#define ADD_PROP(name, mat) material->properties.push_back(MaterialProperty(#name, mat->name))

    bool is_phong = node_material->GetClassId().Is(FbxSurfacePhong::ClassId);
    bool is_lambert = node_material->GetClassId().Is(FbxSurfaceLambert::ClassId);

    if (is_lambert || is_phong) {
      FbxSurfaceLambert *mat = (FbxSurfaceLambert *)node_material;
      ADD_PROP(Ambient, mat);
      ADD_PROP(AmbientFactor, mat);
      ADD_PROP(Diffuse, mat);
      ADD_PROP(DiffuseFactor, mat);
      ADD_PROP(Emissive, mat);
      ADD_PROP(EmissiveFactor, mat);
      ADD_PROP(TransparentColor, mat);
      ADD_PROP(TransparencyFactor, mat);

      if (is_phong) {
        FbxSurfacePhong *mat = (FbxSurfacePhong *)node_material;
        ADD_PROP(Specular, mat);
        ADD_PROP(SpecularFactor, mat);
        ADD_PROP(Shininess, mat);
        ADD_PROP(Reflection, mat);
        ADD_PROP(ReflectionFactor, mat);
      }
    } else {
      // unknown material
      _errors.push_back(to_string("Unknown material type: %s", node_material->GetTypeName()));
    }

#undef ADD_PROP
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

bool FbxConverter::process_animation(FbxNode *node, bool translation_only) {
  INFO_SCOPE;

  FbxAMatrix a = GetGeometry(node);

  vector<KeyFrameMtx> &mtx_anims = _animation_mtx[node->GetName()];
  vector<KeyFrameVec3> &vec3_anims = _animation_vec3[node->GetName()];

  int num_frames = (int)(_duration_ms * _fps / 1000 + 0.5f);
  for (int i = 0; i <= num_frames; ++i) {
    FbxTime cur;
    double cur_time = i*_duration/num_frames;
    cur.SetSecondDouble(cur_time);
    FbxAMatrix m;
    m = GetGlobalPosition(node, cur) * a;
    if (translation_only) {
      vec3_anims.push_back(KeyFrameVec3(cur_time, max_to_dx2(m.GetT())));
    } else {
      mtx_anims.push_back(KeyFrameMtx(cur_time, max_to_dx(m)));
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
  typedef vector<int> PolyIndices;
  vector<PolyIndices > polys_by_material;
  for (int i = 0; i < material_count; ++i)
    polys_by_material.push_back(vector<int>());

  bool use_default_material = false;
  if (const FbxLayerElementMaterial *materials = layer->GetMaterials()) {
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
    polys_by_material.push_back(PolyIndices());
    for (int i = 0; i < fbx_mesh->GetPolygonCount(); ++i)
      polys_by_material[0].push_back(i);
  }

  // get the uv sets
  vector<string> uv_sets;
  for (int i = 0; i < layer->GetUVSetCount(); ++i)
    uv_sets.push_back(layer->GetUVSets()[i]->GetName());

  shared_ptr<Mesh> mesh(new Mesh(fbx_node->GetName()));
  _scene.meshes.push_back(mesh);

  if (g_verbose) {
    add_info("%Iu submesh%s", polys_by_material.size(), polys_by_material.size() == 1 ? "" : "es");
  }

  mesh->obj_to_world = max_to_dx(fbx_node->EvaluateGlobalTransform());

  uint32 vertex_flags = SubMesh::kPos | SubMesh::kNormal;
  vertex_flags |= uv_sets.size() == 2 ? SubMesh::kTex1 | SubMesh::kTex0 : uv_sets.size() == 1 ? SubMesh::kTex0 : 0;

  // each material used for the mesh creates a sub mesh
  for (size_t i = 0; i < polys_by_material.size(); ++i) {

    // keep track of the unique vertices
    set<SuperVertex> super_verts;

    SubMesh *sub_mesh = new SubMesh(
      use_default_material ? "default-material" : fbx_node->GetMaterial(i)->GetName(), vertex_flags);
    mesh->sub_meshes.push_back(sub_mesh);

    const PolyIndices &indices = polys_by_material[i];
    for (size_t j = 0; j < indices.size(); ++j) {
      const int poly_idx = indices[j];
      CHECK_FATAL(fbx_mesh->GetPolygonSize(j) == 3, "Only polygons of size 3 supported");

      FbxVector4 pos, normal;
#if USE_DIRECTX_SYSTEM
      for (int k = 0; k <= 2; ++k) {
#else
      // we reverse the winding order to convert between the right and left-handed systems
      for (int k = 2; k >= 0; --k) {
#endif
        pos = max_to_dx(fbx_mesh->GetControlPointAt(fbx_mesh->GetPolygonVertex(poly_idx, k)));
        fbx_mesh->GetPolygonVertexNormal(poly_idx, k, normal);
        normal = max_to_dx(normal);

        SuperVertex cand(pos, normal);
        for (size_t uv_idx = 0; uv_idx  < uv_sets.size(); ++uv_idx) {
          FbxVector2 uv;
          fbx_mesh->GetPolygonVertexUV(poly_idx, k, uv_sets[uv_idx].c_str(), uv);
          cand.uv.push_back(uv);
        }

        // create a supervertex for the current vertex, and check if it already exists to
        // determine what vertex index to give it
        int idx = -1;
        set<SuperVertex>::iterator it = super_verts.find(cand);
        if (it == super_verts.end()) {
          idx = cand.idx = sub_mesh->pos.size();
          sub_mesh->pos.push_back(pos);
          sub_mesh->normal.push_back(normal);
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

  if (!_writer.open(dst))
    return false;

  MainHeader header;
  header.version = FILE_VERSION;
  _writer.push_pos();
  _writer.write(header);

  header.material_ofs = _writer.pos();
  if (!save_materials())
    return false;

  header.mesh_ofs = _writer.pos();
  if (!save_meshes())
    return false;

  header.light_ofs = _writer.pos();
  if (!save_lights())
    return false;

  header.camera_ofs = _writer.pos();
  if (!save_cameras())
    return false;

  header.animation_ofs = _writer.pos();
  if (!save_animations())
    return false;

  header.binary_ofs = _writer.pos();
  _writer.save_binary();
  header.total_size = _writer.pos();

  _writer.push_exch();
  _writer.write(header);
  _writer.pop_pos();

  return true;
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

template<class T> void prune_animations(map<string, vector<KeyFrame<T>>> *anims) {
  for (auto it = begin(*anims); it != end(*anims); ) {
    if (it->second.empty()) {
      it = anims->erase(it);
    } else {
      ++it;
    }
  }
}

template<class T> bool FbxConverter::save_animations(const map<string, vector<KeyFrame<T>>> &anims) {

  _writer.write((int)anims.size());

  for (auto it = begin(anims); it != end(anims); ++it) {
    const string &node_name = it->first;
    auto &frames = it->second;
    _writer.add_deferred_string(node_name);
    _writer.write((int)frames.size());
    _writer.write_raw(&frames[0], sizeof(frames[0]) * frames.size());
  }

  return true;
}

bool FbxConverter::save_animations() {
  BlockHeader header;
  header.id = BlockId::kAnimation;
  ScopedBlock scoped_block(header, _writer);

  prune_animations(&_animation_float);
  prune_animations(&_animation_vec3);
  prune_animations(&_animation_mtx);

  save_animations<float>(_animation_float);
  save_animations<D3DXVECTOR3>(_animation_vec3);
  save_animations<D3DXMATRIX>(_animation_mtx);

  return true;
}

bool FbxConverter::save_cameras() {

  BlockHeader header;
  header.id = BlockId::kCameras;
  ScopedBlock scoped_block(header, _writer);

  Scene::Cameras &cameras = _scene.cameras;

  _writer.write((int)cameras.size());
  for (size_t i = 0; i < cameras.size(); ++i) {
    Camera *camera = cameras[i];
    _writer.add_deferred_string(camera->name);
    write_vector(_writer, camera->pos);
    write_vector(_writer, camera->target);
    write_vector(_writer, camera->up);
    _writer.write((float)camera->roll);
    _writer.write((float)camera->aspect_ratio);
    _writer.write((float)camera->fov);
    _writer.write((float)camera->near_plane);
    _writer.write((float)camera->far_plane);
  };

  return true;
}

bool FbxConverter::save_lights() {

  BlockHeader header;
  header.id = BlockId::kLights;
  ScopedBlock scoped_block(header, _writer);

  Scene::Lights &lights = _scene.lights;

  _writer.write((int)lights.size());
  for (size_t i = 0; i < lights.size(); ++i) {
    Light *light = lights[i];
    _writer.add_deferred_string(light->name);
    write_vector(_writer, light->pos);
    write_vector(_writer, light->color);
    _writer.write((float)light->intensity);
  }

  return true;
}

bool FbxConverter::save_meshes() {

  BlockHeader header;
  header.id = BlockId::kMeshes;
  ScopedBlock scoped_block(header, _writer);

  auto &meshes = _scene.meshes;

  _writer.write((int)meshes.size());
  for (size_t i = 0; i < meshes.size(); ++i) {

    auto &mesh = meshes[i];
    _writer.add_deferred_string(mesh->name);
    _writer.write(mesh->obj_to_world);

    // we save the vertex data in a deferred segment, because
    // once the buffers have been created we're free to throw
    // the data away
    _writer.write((int)mesh->sub_meshes.size());
    for (size_t j = 0; j < mesh->sub_meshes.size(); ++j) {
      SubMesh *sub = mesh->sub_meshes[j];
      _writer.add_deferred_string(sub->material);

      void *vb, *ib;
      int vb_len, ib_len, index_size;
      compact_vertex_data(*sub, &vb, &vb_len);
      compact_index_data(*sub, &ib, &ib_len, &index_size);

      _writer.write(sub->vertex_flags);
      _writer.write(sub->element_size);
      _writer.write(index_size);
      _writer.add_deferred_binary(vb, vb_len);
      _writer.add_deferred_binary(ib, ib_len);
    }
  }

  return true;
}

bool FbxConverter::save_materials() {

  BlockHeader header;
  header.id = BlockId::kMaterials;
  ScopedBlock scoped_block(header, _writer);

  Scene::Materials &materials = _scene.materials;

  _writer.write((int)materials.size());
  for (Scene::Materials::iterator it = materials.begin(); it != materials.end(); ++it) {
    const Material *mat = it->second;
    _writer.add_deferred_string(mat->name);
    _writer.add_deferred_string(mat->technique);
    _writer.write((int)mat->properties.size());

    // write the properties for the material
    for (size_t i = 0; i < mat->properties.size(); ++i) {
      const MaterialProperty &prop = mat->properties[i];
      _writer.add_deferred_string(prop.name);
      _writer.write(prop.type);
      switch (prop.type) {
      case Property::kInt:
        _writer.write(prop._int);
        break;
      case Property::kFloat:
        _writer.write_raw(&prop._float[0], sizeof(float));
        break;
      case Property::kFloat3:
        _writer.write_raw(&prop._float[0], 3 * sizeof(float));
        break;
      case Property::kFloat4:
        _writer.write_raw(&prop._float[0], 4 * sizeof(float));
        break;
      default:
        _errors.push_back("Unknown property type exporting materials");
        break;
      }
    }
  }
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
