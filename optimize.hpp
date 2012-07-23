// Copyright 2011 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License. You
// may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied. See the License for the specific language governing
// permissions and limitations under the License.

#pragma once


// Linear-Speed Vertex Cache Optimisation, via:
// http://home.comcast.net/~tom_forsyth/papers/fast_vert_cache_opt.html
class VertexOptimizer {
 public:
  struct TriangleData {
    bool active;  // true iff triangle has not been optimized and emitted.
    // TODO: eliminate some wasted computation by using this cache.
    // float score;
  };

  VertexOptimizer(int num_verts);
  void AddTriangles(const int* indices, size_t length, std::vector<int> *optimized_order, std::vector<int> *vertex_reorder);

 private:
   static const int kUnknownIndex = -1;
  static const size_t kCacheSize = 32;  // Does larger improve compression?

  struct VertexData {
    VertexData() : cache_tag(kCacheSize), output_index(kUnknownIndex) {}

    // Should this also update scores for incident triangles?
    void UpdateScore();
    // TODO: this assumes that "tri" is in the list!
    void RemoveFace(int tri);

    // TODO: since most vertices are part of 6 faces, you can optimize
    // this by using a small inline buffer.
    std::vector<int> faces;
    unsigned int cache_tag;  // kCacheSize means not in cache.
    float score;
    int output_index;
  };

  int FindBestTriangle(const int* indices, const std::vector<TriangleData>& per_tri);

  // TODO: faster to update an entire triangle.
  // This also updates the vertex scores!
  void InsertIndexToCache(int index);

  std::vector<VertexData> per_vertex_;
  int cache_[kCacheSize + 1];
  int next_unused_index_;
};

