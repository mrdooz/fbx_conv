#pragma once

void get_aspect_and_fov(FbxCamera *camera, double *fov_x, double *fov_y, double *aspect);

typedef float AnimType;
void optimize_spline_fit(int dimension, int num_samples, AnimType *sample_data, AnimType err);
FbxDouble3 GetMaterialProperty(const FbxSurfaceMaterial *pMaterial, const char *pPropertyName, const char *pFactorPropertyName, std::string *filename);

FbxAMatrix GetGeometry(FbxNode* pNode);
FbxAMatrix GetGlobalPosition(FbxNode* pNode, const FbxTime& pTime, FbxPose* pPose = NULL, FbxAMatrix* pParentGlobalPosition = NULL);


inline int sign(float a) {
  return a < 0 ? -1 : a > 0 ? 1 : 0;
}

template <typename T>
struct SamplePoint {
  SamplePoint(float t, const T &value) : t(t), value(value) {}
  float t;
  T value;
};

template <typename T>
struct Segment {
  Segment(const SamplePoint<T> &p0, const SamplePoint<T> &p1, int start, int end) : p0(p0), p1(p1), sample_start(start), sample_end(end), err(0) {}
  SamplePoint<T> p0, p1;
  int sample_start, sample_end; // [sample_start, sample_end)
  double err;
};

template <typename T>
T sample_point(float t, const Segment<T> &segment) {
  auto &a = segment.p0;
  auto &b = segment.p1;
  float f = (t - a.t) / (b.t - a.t);
  return a.value + f * (b.value - a.value);
}

inline float length_sq(const D3DXVECTOR3 &v) {
  return D3DXVec3LengthSq(&v);
}

inline float length_sq(const D3DXVECTOR4 &v) {
  return D3DXVec4LengthSq(&v);
}

template <typename T>
double calc_error(const std::vector<SamplePoint<T>> &points, const Segment<T> &segment) {

  double err = 0;
  for (int i = segment.sample_start; i < segment.sample_end; ++i) {
    T pt = sample_point(points[i].t, segment);
    err += length_sq(pt - points[i].value);
  }
  return err / (segment.sample_end - segment.sample_start);
}

template <typename T>
std::deque<Segment<T>> linear_fit(const std::vector<SamplePoint<T>> &points, double threshold) {

  typedef Segment<T> SegT;

  // add a single line segment
  deque<SegT> segments;
  segments.push_back(SegT(points.front(), points.back(), 0, points.size() - 1));
  segments.back().err = calc_error(points, segments.back());

  deque<SegT> done_segments;

  while (!segments.empty()) {
    double total_err = 0;
    for (size_t i = 0; i < segments.size(); ++i)
      total_err += segments[i].err;

    if (total_err < threshold)
      break;

    // sort segments by error
    sort(RANGE(segments), [&](const SegT &a, const SegT &b) { return a.err > b.err; });

    // split the segment with the largest error into 2
    auto largest_err = segments.front();
    segments.pop_front();

    int s = largest_err.sample_start;
    int e = largest_err.sample_end;
    int middle = (s + e) / 2;
    SegT s0(points[s], points[middle], s, middle);
    SegT s1(points[middle], points[e], middle, e);

    s0.err = calc_error(points, s0);
    s1.err = calc_error(points, s1);

    if (middle - s <= 1) {
      done_segments.push_back(s0);
    } else {
      segments.push_back(s0);
    }

    if (e - middle <= 1) {
      done_segments.push_back(s1);
    } else {
      segments.push_back(s1);
    }
  }

  copy(RANGE(done_segments), back_inserter(segments));
  sort(RANGE(segments), [&](const SegT &a, const SegT &b) { return a.sample_start < b.sample_start; });

  return segments;
}
