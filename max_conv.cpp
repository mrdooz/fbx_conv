// max_conv.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <fbxsdk.h>
#include <string>
#include <vector>
#include <stdint.h>
#include <map>
#include <set>

using namespace std;

#define SAFE_DELETE(x) if( (x) != 0 ) { delete (x); (x) = 0; }

#if _DEBUG
#pragma comment(lib, "fbxsdk-2012.1-mdd.lib")
#else
#pragma comment(lib, "fbxsdk-2012.1.lib")
#endif

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

enum SubBufferType {
	Pos,
	Normal,
	Tex,
};

struct SuperVertex {
	SuperVertex(const KFbxVector4 &pos, const KFbxVector4 &normal, const KFbxVector2 &uv0, int idx) : pos(pos), normal(normal), idx(idx) {
		uv.push_back(uv0);
	}
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

struct VertexBuffer {
	VertexBuffer() : data(nullptr) {}
	~VertexBuffer() { delete [] data; }
	struct Sub {
		SubBufferType type;
		int elem_size;
	};
	int elem_count;
	int elem_size;
	vector<Sub> subs;
	void *data;
};

struct IndexBuffer {
	IndexBuffer() : data(nullptr) {}
	~IndexBuffer() { delete [] data; }
	int elem_count;
	int elem_size;
	void *data;
};

struct SubMesh {
	SubMesh(const string &material) : material(material) {}
	string material;
	vector<SuperVertex> verts;
	vector<int> indices;
};

struct Mesh {
	~Mesh() {
		seq_delete(&sub_meshes);
	}
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

	map<string, Material *> materials;
	vector<Mesh *> meshes;
};

/*
struct InPlaceString {
	const char *str;
	size_t len;
};

template< typename T>
struct InPlaceArray {
	T *data;
	size_t len;
};

struct LinearMemory {
	LinearMemory(const char *start, size_t len) : _start(start), _len(len) {}
	const char *_start;
	size_t _len;
};

struct Node {
	Node *parent;
	vector<Node *> children;
	uint32_t id;
};

struct Mesh {
	string name;
};

struct Scene {
	Scene() : root(nullptr) {}
	Node *root;
};
*/
class FbxConverter {
public:
	FbxConverter();
	~FbxConverter();
	bool convert(const char *src, const char *dst);
private:

	bool export_mesh(KFbxNode *node, KFbxMesh *mesh);
	void traverse_scene(KFbxScene *scene);

	vector<string> _errors;
	Scene _scene;

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

	Mesh *mesh = new Mesh;

	// each material used for the mesh creates a sub mesh
	for (size_t i = 0; i < polys_by_material.size(); ++i) {

		// keep track of the unique vertices
		set<SuperVertex> super_verts;

		SubMesh *sub_mesh = new SubMesh(fbx_node->GetMaterial(i)->GetName());
		const PolyIndices &indices = polys_by_material[i];
		for (size_t j = 0; j < indices.size(); ++j) {
			const int poly_idx = indices[j];
			CHECK_FATAL(fbx_mesh->GetPolygonSize(j) == 3, "Only polygons of size 3 supported");

			for (int k = 0; k < 3; ++k) {
				KFbxVector4 normal;
				KFbxVector2 uv0;
				KFbxVector4 pos = fbx_mesh->GetControlPointAt(fbx_mesh->GetPolygonVertex(poly_idx, k));
				fbx_mesh->GetPolygonVertexNormal(poly_idx, k, normal);
				fbx_mesh->GetPolygonVertexUV(poly_idx, k, uv_sets[0].c_str(), uv0);

				int idx = -1;
				SuperVertex cand(pos, normal, uv0, -1);
				set<SuperVertex>::iterator it = super_verts.find(cand);
				if (it == super_verts.end()) {
					idx = cand.idx = sub_mesh->verts.size();
					sub_mesh->verts.push_back(cand);
					super_verts.insert(cand);
				} else {
					idx = it->idx;
				}
				sub_mesh->indices.push_back(idx);
			}
		}
		mesh->sub_meshes.push_back(sub_mesh);
	}

	_scene.meshes.push_back(mesh);


//	mesh->GetNormals()
/*
	KFbxLayerElementNormal *normals = layer->GetNormals();

	const KFbxLayerElementArrayTemplate<KFbxVector4> &arr = normals->GetDirectArray();
	const KFbxLayerElementArrayTemplate<int> &arr2 = normals->GetIndexArray();
	int c = arr.GetCount();
	int c2 = arr2.GetCount();
	int cc = mesh->GetControlPointsCount();
	int b = mesh->GetElementNormalCount();

	for (int i = 0; i < mesh->GetLayerCount(); ++i) {
		KFbxLayer *layer = mesh->GetLayer(i);
		const int uv_set_count = layer->GetUVSetCount();
		KArrayTemplate<const KFbxLayerElementUV *> uvs = layer->GetUVSets();
		for (int j = 0; j < uv_set_count; ++j) {
			const char *uv_set_name = uvs[j]->GetName();
			KFbxLayerElement::EMappingMode mm = uvs[j]->GetMappingMode();
			int a = 10;
			
		}
		int a = 10;
	}

	int max_p = 0;
	int aaa = mesh->GetPolygonCount();
	for (int i = 0; i < mesh->GetPolygonCount(); ++i) {
		const int verts_in_poly = mesh->GetPolygonSize(i);
		for (int j = 0; j < verts_in_poly; ++j) {
			int v = mesh->GetPolygonVertex(i, j);
			max_p = max(v, max_p);
			int a = 0;
		}
	}
*/
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

int _tmain(int argc, _TCHAR* argv[])
{

	FbxConverter converter;
	if (!converter.convert("C:\\Users\\dooz\\Documents\\3dsMax\\export\\torus.fbx", NULL))
		return 1;

	return 0;
}

