#pragma once

#define FILE_VERSION 10

#pragma pack(push, 1)
struct MainHeader {
  int version;
  int global_ofs;
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
    kGlobals,
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

namespace Property {
  enum Type {
    kUnknown,
    kFloat,
    kFloat2,
    kFloat3,
    kFloat4,
    kColor,
    kFloat4x4,
    kInt,
  };
}

struct MaterialProperty {
  MaterialProperty(const std::string &name, int value) : name(name), type(Property::kInt), _int(value) {}
  MaterialProperty(const std::string &name, FbxDouble value) : name(name), type(Property::kFloat) { _float[0] = (float)value; }
  MaterialProperty(const std::string &name, FbxDouble3 value, bool is_color) 
    : name(name), type(is_color ? Property::kColor : Property::kFloat3) 
  { 
    for (int i = 0; i < 3; ++i) 
      _float[i] = (float)value[i]; 
    _float[3] = 0; 
  }

  MaterialProperty(const std::string &name, FbxDouble4 value, bool is_color) 
    : name(name), type(is_color ? Property::kColor : Property::kFloat4) 
  { 
    for (int i = 0; i < 4; ++i) 
      _float[i] = (float)value[i]; 
  }

  std::string name;
  std::string filename;
  Property::Type type;
  union {
    int _int;
    float _float[4];
  };
};

struct Material {
  Material(const std::string &name) : name(name) {}
  std::string name;
  std::string technique;
  std::vector<MaterialProperty> properties;
};

struct SuperVertex {
  SuperVertex(const D3DXVECTOR3 &pos, const D3DXVECTOR3 &normal) : pos(pos), normal(normal), idx(-1) {}
  D3DXVECTOR3 pos;
  D3DXVECTOR3 normal;
  std::vector<D3DXVECTOR2> uv;
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

struct SubMesh {

  enum VertexFlags {
    kPos     = 1 << 0,
    kNormal  = 1 << 1,
    kTex0    = 1 << 2,
    kTex1    = 1 << 3,
  };

  SubMesh(const std::string &name, const std::string &material, uint32 vertex_flags) 
    : name(name), material(material), vertex_flags(vertex_flags)
  {
    element_size = vertex_flags & kPos ? sizeof(D3DXVECTOR3) : 0;
    element_size += vertex_flags & kNormal ? sizeof(D3DXVECTOR3) : 0;
    element_size += vertex_flags & kTex0 ? sizeof(D3DXVECTOR2) : 0;
    element_size += vertex_flags & kTex1 ? sizeof(D3DXVECTOR2) : 0;
  }
  std::string name;
  std::string material;
  uint32 vertex_flags;
  uint32 element_size;
  std::vector<D3DXVECTOR3> pos;
  std::vector<D3DXVECTOR3> normal;
  std::vector<D3DXVECTOR2> tex0;
  std::vector<D3DXVECTOR2> tex1;
  std::vector<uint32> indices;
};

struct Mesh {
  Mesh(const std::string &name) : name(name) {}
  std::string name;
  D3DXMATRIX obj_to_world;
  std::vector<std::unique_ptr<SubMesh>> sub_meshes;
};


struct Camera {
  std::string name;
  D3DXVECTOR3 pos, target, up;
  FbxDouble roll;
  FbxDouble aspect_ratio;
  FbxDouble fov_x;
  FbxDouble fov_y;
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

  std::string name;
  Type type;
  Decay decay;

  bool use_near_attenuation;
  FbxDouble near_attenuation_start;
  FbxDouble near_attenuation_end;

  bool use_far_attenuation;
  FbxDouble far_attenuation_start;
  FbxDouble far_attenuation_end;

  D3DXVECTOR3 pos;
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

//typedef KeyFrame<float> KeyFrameFloat;
typedef KeyFrame<D3DXVECTOR3> KeyFrameVec3;
//typedef KeyFrame<D3DXQUATERNION> KeyFrameQuat;
typedef KeyFrame<D3DXVECTOR4> KeyFrameVec4;
//typedef KeyFrame<D3DXMATRIX> KeyFrameMtx;

#pragma pack(pop)

struct Scene {
  FbxColor ambient;
  std::vector<std::shared_ptr<Camera>> cameras;
  std::vector<std::shared_ptr<Light>> lights;
  std::map<std::string, Material *> materials_by_name;
  std::vector<std::shared_ptr<Material>> materials;
  std::vector<std::shared_ptr<Mesh>> meshes;
};

struct MaterialInfo {
  SubMesh *submesh;
  Material *material;
};

class Writer;
class FbxConverter;
extern FbxConverter *g_converter;

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
  bool save_globals();
  bool save_meshes();
  bool save_cameras();

  template<class T> bool save_animations(const std::map<std::string, std::vector<KeyFrame<T>>> &anims);
  template<class T> bool save_animations(const std::vector<KeyFrame<T>> &frames);
  bool save_animations();
  bool save_lights();
  bool save_materials();

  bool save_material_info();

  void copy_texture(std::string src, std::string *dst);

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

  std::vector<std::string> _errors;

  struct Animation {
    std::vector<KeyFrameVec3> pos;
    std::vector<KeyFrameVec4> rot;
    std::vector<KeyFrameVec3> scale;
  };

  std::map<std::string, Animation> _animation;
  //std::map<std::string, std::vector<KeyFrameMtx>> _animation_mtx;
  Scene _scene;
  std::unique_ptr<Writer> _writer;

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

  std::vector<MaterialInfo> _material_info;
  std::string _texture_dst;
};


