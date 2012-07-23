#include "stdafx.h"
#include "optimize.hpp"

VertexOptimizer::VertexOptimizer(int num_verts)
    : per_vertex_(num_verts),
      next_unused_index_(0)
{
  // The cache has an extra slot allocated to simplify the logic in
  // InsertIndexToCache.
  for (unsigned int i = 0; i < kCacheSize + 1; ++i) {
    cache_[i] = kUnknownIndex;
  }
}

void VertexOptimizer::AddTriangles(const int* indices, size_t length, std::vector<int> *optimized_order, std::vector<int> *vertex_reorder) {

  std::vector<TriangleData> per_tri(length / 3);
  optimized_order->clear();
  optimized_order->reserve(length);
  vertex_reorder->clear();
  vertex_reorder->resize(per_vertex_.size());

  // Loop through the triangles, updating vertex->face lists.
  for (size_t i = 0; i < per_tri.size(); ++i) {
    per_tri[i].active = true;
    per_vertex_[indices[3*i + 0]].faces.push_back(i);
    per_vertex_[indices[3*i + 1]].faces.push_back(i);
    per_vertex_[indices[3*i + 2]].faces.push_back(i);
  }

  // TODO: with index bounds, no need to recompute everything.
  // Compute initial vertex scores.
  for (size_t i = 0; i < per_vertex_.size(); ++i) {
    VertexData& vertex_data = per_vertex_[i];
    vertex_data.cache_tag = kCacheSize;
    vertex_data.output_index = kUnknownIndex;
    vertex_data.UpdateScore();
  }

  // Consume indices, one triangle at a time.
  for (size_t c = 0; c < per_tri.size(); ++c) {
    const int best_triangle = FindBestTriangle(indices, per_tri);
    per_tri[best_triangle].active = false;

    // Iterate through triangle indices.
    for (size_t i = 0; i < 3; ++i) {
      const int index = indices[3*best_triangle + i];
      VertexData& vertex_data = per_vertex_[index];
      vertex_data.RemoveFace(best_triangle);

      InsertIndexToCache(index);

      const int cached_output_index = per_vertex_[index].output_index;
      // Have we seen this index before?
      if (cached_output_index != kUnknownIndex) {
        optimized_order->push_back(cached_output_index);
        continue;
      }

      // The first time we see a vertex, we save the remapping between old and new index
      (*vertex_reorder)[index] = next_unused_index_;
      per_vertex_[index].output_index = next_unused_index_;
      optimized_order->push_back(next_unused_index_++);

    }
  }
}

int VertexOptimizer::FindBestTriangle(const int* indices, const std::vector<TriangleData>& per_tri) {
  float best_score = -std::numeric_limits<float>::infinity();
  int best_triangle = -1;

  // The trick to making this algorithm run in linear time (with
  // respect to the vertices) is to only scan the triangles incident
  // on the simulated cache for the next triangle. It is an
  // approximation, but the score is heuristic. Anyway, most of the
  // time the best triangle will be found this way.
  for (size_t i = 0; i < kCacheSize; ++i) {
    if (cache_[i] == kUnknownIndex) {
      break;
    }
    const VertexData& vertex_data = per_vertex_[cache_[i]];
    for (size_t j = 0; j < vertex_data.faces.size(); ++j) {
      const int tri_index = vertex_data.faces[j];
      if (per_tri[tri_index].active) {
        const float score =
          per_vertex_[indices[3*tri_index + 0]].score +
          per_vertex_[indices[3*tri_index + 1]].score +
          per_vertex_[indices[3*tri_index + 2]].score;
        if (score > best_score) {
          best_score = score;
          best_triangle = tri_index;
        }
      }
    }
  }
  // TODO: keep a range of active triangles to make the slow scan a
  // little faster. Does this ever happen?
  if (best_triangle == -1) {
    // If no triangles can be found through the cache (e.g. for the
    // first triangle) go through all the active triangles and find
    // the best one.
    for (size_t i = 0; i < per_tri.size(); ++i) {
      if (per_tri[i].active) {
        const float score =
          per_vertex_[indices[3*i + 0]].score +
          per_vertex_[indices[3*i + 1]].score +
          per_vertex_[indices[3*i + 2]].score;
        if (score > best_score) {
          best_score = score;
          best_triangle = i;
        }
      }
    }
    assert(-1 != best_triangle);
  }
  return best_triangle;
}

// TODO: faster to update an entire triangle.
// This also updates the vertex scores!
void VertexOptimizer::InsertIndexToCache(int index) {
  // Find how recently the vertex was used.
  const unsigned int cache_tag = per_vertex_[index].cache_tag;

  // Don't do anything if the vertex is already at the head of the
  // LRU list.
  if (cache_tag == 0) return;

  // Loop through the cache, inserting the index at the front, and
  // bubbling down to where the index was originally found. If the
  // index was not originally in the cache, then it claims to be at
  // the (kCacheSize + 1)th entry, and we use an extra slot to make
  // that case simpler.
  int to_insert = index;
  for (unsigned int i = 0; i <= cache_tag; ++i) {
    const int current_index = cache_[i];

    // Update cross references between the entry of the cache and
    // the per-vertex data.
    cache_[i] = to_insert;
    per_vertex_[to_insert].cache_tag = i;
    per_vertex_[to_insert].UpdateScore();

    // No need to continue if we find an empty entry.
    if (current_index == kUnknownIndex) {
      break;
    }

    to_insert = current_index;
  }
}

void VertexOptimizer::VertexData::UpdateScore() {
  const size_t active_tris = faces.size();
  if (active_tris <= 0) {
    score = -1.f;
    return;
  }
  // TODO: build initial score table.
  if (cache_tag < 3) {
    // The most recent triangle should has a fixed score to
    // discourage generating nothing but really long strips. If we
    // want strips, we should use a different optimizer.
    const float kLastTriScore = 0.75f;
    score = kLastTriScore;
  } else if (cache_tag < kCacheSize) {
    // Points for being recently used.
    const float kScale = 1.f / (kCacheSize - 3);
    const float kCacheDecayPower = 1.5f;
    score = powf(1.f - kScale * (cache_tag - 3), kCacheDecayPower);
  } else {
    // Not in cache.
    score = 0.f;
  }

  // Bonus points for having a low number of tris still to use the
  // vert, so we get rid of lone verts quickly.
  const float kValenceBoostScale = 2.0f;
  const float kValenceBoostPower = 0.5f;
  // rsqrt?
  const float valence_boost = powf((float)active_tris, -kValenceBoostPower);
  score += valence_boost * kValenceBoostScale;
}

// TODO: this assumes that "tri" is in the list!
void VertexOptimizer::VertexData::RemoveFace(int tri) {
  auto face = faces.begin();
  while (*face != tri) 
    ++face;
  *face = faces.back();
  faces.pop_back();
}
