#include "stdafx.h"

using namespace std;

#define SAFE_DELETE(x) if( (x) != 0 ) { delete (x); (x) = 0; }

#if _DEBUG
#pragma comment(lib, "fbxsdk-2012.1-mdd.lib")
#else
#pragma comment(lib, "fbxsdk-2012.1.lib")
#endif

#pragma pack(push, 1)
struct MainHeader {
	int material_ofs;
	int mesh_ofs;
	int string_ofs;
	int binary_ofs;
	int total_size;
};

namespace BlockId {
	enum Enum {
		kMaterials,
		kMeshes
	};
}

struct BlockHeader {
	BlockId::Enum id;
	int size;	// size excl header
};
#pragma pack(pop)

template<class T> 
void seq_delete(T* t) {
	for (T::iterator it = t->begin(); it != t->end(); ++it)
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

		for (size_t i = 0; i < _deferred_binary.size(); ++i)
			delete [] _deferred_binary[i].data;
		_deferred_binary.clear();
	}

	struct DeferredString {
		DeferredString(int ofs, const string &str) : ofs(ofs), str(str) {}
		int ofs;
		string str;
	};

	struct DeferredBinary {
		DeferredBinary(int ofs, void *data, int len) : ofs(ofs), data(data), len(len) {}
		int ofs;
		void *data;
		int len;
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
		_deferred_strings.push_back(DeferredString(p, str));
		write(p);
	}

	void add_deferred_binary(void *data, int len) {
		int p = pos();
		_deferred_binary.push_back(DeferredBinary(p, data, len));
		write(p);
	}

	void save_strings() {

		// save the # deferred strings and their locations
		write((int)_deferred_strings.size());
		for (size_t i = 0; i<  _deferred_strings.size(); ++i)
			write(_deferred_strings[i].ofs);

		for (size_t i = 0; i < _deferred_strings.size(); ++i) {
			// for each deferred string, save the string, and update the
			// inplace offset to point to it
			int p = pos();
			write_raw(_deferred_strings[i].str.c_str(), _deferred_strings[i].str.size() + 1);
			push_pos();
			set_pos(_deferred_strings[i].ofs);
			write(p);
			pop_pos();
		}
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
			int p = pos() - binary_start;
			write(_deferred_binary[i].len);
			write_raw(_deferred_binary[i].data, _deferred_binary[i].len);
			push_pos();
			set_pos(_deferred_binary[i].ofs);
			write(p);
			pop_pos();
		}
	}

	template <class T>
	void write(const T& data) {
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
	vector<DeferredString> _deferred_strings;
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

enum ParamType {
	kInt = 1,
	kFloat,
	kFloat2,
	kFloat3,
	kFloat4
};

struct MaterialProperty {
	MaterialProperty(const string &name, int value) : name(name), type(kInt), _int(value) {}
	MaterialProperty(const string &name, fbxDouble1 value) : name(name), type(kFloat) { _float[0] = (float)value; }
	MaterialProperty(const string &name, fbxDouble4 value) : name(name), type(kFloat4) { for (int i = 0; i < 4; ++i) _float[i] = (float)value[i]; }
	string name;
	ParamType type;
	union {
		int _int;
		float _float[4];
	};
};

struct Material {
	Material(const string &name) : name(name) {}
	string name;
	vector<MaterialProperty> properties;
};

struct SuperVertex {
	SuperVertex(const KFbxVector4 &pos, const KFbxVector4 &normal) : pos(pos), normal(normal), idx(-1) {}
	KFbxVector4 pos;
	KFbxVector4 normal;
	vector<KFbxVector2> uv;
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

struct Vector2 {
	Vector2() {}
	Vector2(const KFbxVector2 &v) : x((float)v[0]), y((float)v[1]) {}
	float x, y;
};

struct Vector3 {
	Vector3() {}
	Vector3(const KFbxVector4 &v) : x((float)v[0]), y((float)v[1]), z((float)v[2]) {}
	float x, y, z;
};

struct SubMesh {

	enum VertexFlags {
		kPos     = 1 << 0,
		kNormal  = 1 << 1,
		kTex0    = 1 << 2,
		kTex1    = 1 << 3,
	};

	SubMesh(const string &material, uint32_t vertex_flags) 
		: material(material), vertex_flags(vertex_flags)
	{
		element_size = vertex_flags & kPos ? sizeof(Vector3) : 0;
		element_size += vertex_flags & kNormal ? sizeof(Vector3) : 0;
		element_size += vertex_flags & kTex0 ? sizeof(Vector2) : 0;
		element_size += vertex_flags & kTex1 ? sizeof(Vector2) : 0;
	}
	string material;
	uint32_t vertex_flags;
	uint32_t element_size;
	vector<Vector3> pos;
	vector<Vector3> normal;
	vector<Vector2> tex0;
	vector<Vector2> tex1;
	vector<uint32_t> indices;
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
	const size_t elems = max4(submesh.pos.size(), submesh.normal.size(), submesh.tex0.size(), submesh.tex1.size());
	const bool need_32_bit = elems >= (1 << 16);
	*index_size = need_32_bit ? 4 : 2;
	*len = elems * (*index_size);
	void *buf = new char[*len];
	*data = buf;
	if (need_32_bit) {
		uint32_t *p = (uint32_t *)buf;
		for (size_t i = 0; i < elems; ++i) {
			*p++ = submesh.indices[i];
		}
	} else {
		uint16_t *p = (uint16_t *)buf;
		for (size_t i = 0; i < elems; ++i) {
			*p++ = (uint16_t)submesh.indices[i];
		}
	}
}

struct Mesh {
	Mesh(const string &name) : name(name) {}
	~Mesh() {
		seq_delete(&sub_meshes);
	}
	string name;
	vector<SubMesh *> sub_meshes;
};


struct Camera {

};

struct Light {

};

struct Hierarchy {

};

struct Animation {

};

struct Scene {
	~Scene() {
		assoc_delete(&materials);
		seq_delete(&meshes);
	}

	typedef map<string, Material *> Materials;
	typedef vector<Mesh *> Meshes;

	Materials materials;
	Meshes meshes;
};

class FbxConverter {
public:
	FbxConverter();
	~FbxConverter();
	bool convert(const char *src, const char *dst);
private:

	bool export_mesh(KFbxNode *node, KFbxMesh *mesh);
	void traverse_scene(KFbxScene *scene);
	bool save_scene(const char *dst);
	bool save_meshes();
	bool save_materials();

	vector<string> _errors;
	Scene _scene;
	Writer _writer;

	KFbxSdkManager *_mgr;
	KFbxIOSettings *_settings;
	KFbxGeometryConverter *_converter;
	KFbxImporter *_importer;
};

FbxConverter::FbxConverter() 
	: _mgr(KFbxSdkManager::Create())
	, _settings(KFbxIOSettings::Create(_mgr, IOSROOT))
	, _converter(new KFbxGeometryConverter(_mgr))
	, _importer(KFbxImporter::Create(_mgr, ""))
{
}

FbxConverter::~FbxConverter() {

}

#define CHECK_FATAL(x, msg) if (!(x)) {_errors.push_back(__FUNCTION__ + string(": ") + string(msg)); return false; }

bool FbxConverter::convert(const char *src, const char *dst) {

	bool res = _importer->Initialize(src, -1, _settings);
	if (!res)
		return false;

	KFbxScene *scene = KFbxScene::Create(_mgr, "my_scene");
	_importer->Import(scene);
	traverse_scene(scene);
	save_scene(dst);

	scene->Destroy();

	return true;
}

bool FbxConverter::export_mesh(KFbxNode *fbx_node, KFbxMesh *fbx_mesh) {

	int material_count = fbx_node->GetMaterialCount();
	for (int i = 0; i < material_count; ++i) {
		KFbxSurfaceMaterial *node_material = fbx_node->GetMaterial(i);
		const char *name = node_material->GetName();

		// Skip if we've already seen this material
		if (_scene.materials.find(name) != _scene.materials.end())
			continue;

		Material *material = new Material(name);
		_scene.materials.insert(make_pair(name, material));

#define PROP(name, type) KFbxSurfaceMaterial::name, KFbxGet<type>(node_material->FindProperty(KFbxSurfaceMaterial::name))
		material->properties.push_back(MaterialProperty(PROP(sEmissive, fbxDouble4)));
		material->properties.push_back(MaterialProperty(PROP(sEmissiveFactor, fbxDouble1)));

		material->properties.push_back(MaterialProperty(PROP(sAmbient, fbxDouble4)));
		material->properties.push_back(MaterialProperty(PROP(sAmbientFactor, fbxDouble1)));

		material->properties.push_back(MaterialProperty(PROP(sDiffuse, fbxDouble4)));
		material->properties.push_back(MaterialProperty(PROP(sDiffuseFactor, fbxDouble1)));

		material->properties.push_back(MaterialProperty(PROP(sSpecular, fbxDouble4)));
		material->properties.push_back(MaterialProperty(PROP(sSpecularFactor, fbxDouble1)));
		material->properties.push_back(MaterialProperty(PROP(sShininess, fbxDouble1)));

		material->properties.push_back(MaterialProperty(PROP(sTransparentColor, fbxDouble4)));
		material->properties.push_back(MaterialProperty(PROP(sTransparencyFactor, fbxDouble1)));

		material->properties.push_back(MaterialProperty(PROP(sReflection, fbxDouble4)));
		material->properties.push_back(MaterialProperty(PROP(sReflectionFactor, fbxDouble1)));

#undef PROP
	}

	// just use layer 0.
	KFbxLayer *layer = fbx_mesh->GetLayer(0);
	CHECK_FATAL(layer, "no layer 0 found");

	// group the polygons by material
	typedef vector<int> PolyIndices;
	vector<PolyIndices > polys_by_material;
	for (int i = 0; i < material_count; ++i)
		polys_by_material.push_back(vector<int>());

	{
		const KFbxLayerElementMaterial *materials = layer->GetMaterials();
		KFbxLayerElement::EMappingMode mm = materials->GetMappingMode();
		CHECK_FATAL(mm == KFbxLayerElement::eBY_POLYGON, "Unsupported mapping mode");
		const KFbxLayerElementArrayTemplate<int> &arr = materials->GetIndexArray();
		for (int i = 0; i < arr.GetCount(); ++i) {
			polys_by_material[arr[i]].push_back(i);
		}
	}

	// get the uv sets
	vector<string> uv_sets;
	for (int i = 0; i < layer->GetUVSetCount(); ++i)
		uv_sets.push_back(layer->GetUVSets()[i]->GetName());

	Mesh *mesh = new Mesh(fbx_mesh->GetName());
	_scene.meshes.push_back(mesh);

	uint32_t vertex_flags = SubMesh::kPos | SubMesh::kNormal;
	vertex_flags |= uv_sets.size() == 2 ? SubMesh::kTex1 | SubMesh::kTex0 : uv_sets.size() == 1 ? SubMesh::kTex0 : 0;

	// each material used for the mesh creates a sub mesh
	for (size_t i = 0; i < polys_by_material.size(); ++i) {

		// keep track of the unique vertices
		set<SuperVertex> super_verts;

		SubMesh *sub_mesh = new SubMesh(fbx_node->GetMaterial(i)->GetName(), vertex_flags);
		mesh->sub_meshes.push_back(sub_mesh);

		const PolyIndices &indices = polys_by_material[i];
		for (size_t j = 0; j < indices.size(); ++j) {
			const int poly_idx = indices[j];
			CHECK_FATAL(fbx_mesh->GetPolygonSize(j) == 3, "Only polygons of size 3 supported");

			// create a supervertex for the current vertex, and check if it already exists to
			// determine what vertex index to give it
			for (int k = 0; k < 3; ++k) {
				KFbxVector4 normal;
				KFbxVector4 pos = fbx_mesh->GetControlPointAt(fbx_mesh->GetPolygonVertex(poly_idx, k));
				SuperVertex cand(pos, normal);
				fbx_mesh->GetPolygonVertexNormal(poly_idx, k, normal);
				for (size_t uv_idx = 0; uv_idx  < uv_sets.size(); ++uv_idx) {
					KFbxVector2 uv;
					fbx_mesh->GetPolygonVertexUV(poly_idx, k, uv_sets[uv_idx].c_str(), uv);
					cand.uv.push_back(uv);
				}

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

	return true;
}


void FbxConverter::traverse_scene(KFbxScene *scene) {

	if (KFbxNode *root = scene->GetRootNode()) {
		for (int i = 0; i < root->GetChildCount(); ++i) {
			KFbxNode *child = root->GetChild(i);
			KFbxNodeAttribute *cur = child->GetNodeAttribute();
			if (!cur)
				continue;
			switch (cur->GetAttributeType()) {
			case KFbxNodeAttribute::eMARKER:
				break;

			case KFbxNodeAttribute::eSKELETON:
				break;

			case KFbxNodeAttribute::eMESH:
				{
					KFbxMesh *triangulated = _converter->TriangulateMesh((KFbxMesh *)cur);
					if (!export_mesh(child, triangulated)) {
						// report error
					}
					triangulated->Destroy();
				}
				break;

			case KFbxNodeAttribute::eNURB:
				break;

			case KFbxNodeAttribute::ePATCH:
				break;

			case KFbxNodeAttribute::eCAMERA:
				break;

			case KFbxNodeAttribute::eLIGHT:
				break;

			case KFbxNodeAttribute::eLODGROUP:
				break;
			}   
		}
	}
}

bool FbxConverter::save_scene(const char *dst) {
	// the scene is saved so it can be read in a single sequential read,
	// and then a simple fixup applied to the pointers

	if (!_writer.open(dst))
		return false;

	MainHeader header;
	_writer.push_pos();
	_writer.write(header);

	header.material_ofs = _writer.pos();
	if (!save_materials())
		return false;

	header.mesh_ofs = _writer.pos();
	if (!save_meshes())
		return false;

	header.string_ofs = _writer.pos();
	_writer.save_strings();

	header.binary_ofs = _writer.pos();
	_writer.save_binary();
	header.total_size = _writer.pos();

	_writer.push_exch();
	_writer.write(header);
	_writer.pop_pos();

	return true;
}

bool FbxConverter::save_meshes() {

	BlockHeader header;
	header.id = BlockId::kMeshes;
	ScopedBlock scoped_block(header, _writer);

	Scene::Meshes &meshes = _scene.meshes;

	_writer.write((int)meshes.size());
	for (size_t i = 0; i < meshes.size(); ++i) {
		Mesh *mesh = meshes[i];
		_writer.add_deferred_string(mesh->name);

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
		_writer.write((int)mat->properties.size());

		// write the properties for the material
		for (size_t i = 0; i < mat->properties.size(); ++i) {
			const MaterialProperty &prop = mat->properties[i];
			_writer.add_deferred_string(prop.name);
			_writer.write(prop.type);
			switch (prop.type) {
			case kInt:
				_writer.write(prop._int);
				break;
			case kFloat:
				_writer.write_raw(&prop._float[0], sizeof(float));
				break;
			case kFloat4:
				_writer.write_raw(&prop._float[0], 4 * sizeof(float));
				break;
			}
		}

	}


	return true;
}

void test_load(const char *filename) {
	FILE *f = fopen(filename, "rb");
	fseek(f, 0, SEEK_END);
	int len = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *buf = new char[len];
	fread(buf, 1, len, f);
	MainHeader *header = (MainHeader *)buf;

	int *pp = (int *)(buf + header->string_ofs);
	int count = *pp++;
	for (int i = 0; i < count; ++i) {
		int *ofs = (int *)(buf + pp[i]);
		*ofs += (int)buf;
		const char *tmp = (const char *)*ofs;
		int a = 10;
	}

	delete [] buf;

	fclose(f);

}

int _tmain(int argc, _TCHAR* argv[])
{

	const char *dst = "c:\\temp\\torus.kumi";

	FbxConverter converter;
	if (!converter.convert("C:\\Users\\dooz\\Documents\\3dsMax\\export\\torus.fbx", dst)) {
		if (!converter.convert("C:\\Users\\dooz\\Dropbox\\export\\torus.fbx", dst)) {
			return 1;
		}
	}

	test_load(dst);

	return 0;
}
