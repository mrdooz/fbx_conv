#pragma once

#define SAFE_DELETE(x) if( (x) != 0 ) { delete (x); (x) = 0; }

#define GEN_NAME2(prefix, line) prefix##line
#define GEN_NAME(prefix, line) GEN_NAME2(prefix, line)
#define MAKE_SCOPED(type) type GEN_NAME(ANON, __COUNTER__)

#define RANGE(x) begin(x), end(x)

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

void split_path(const char *path, std::string *drive, std::string *dir, std::string *fname, std::string *ext);

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
    std::vector<uint8> data;
  };

  bool open(const char *filename) {
    if (!(_f = fopen(filename, "wb")))
      return false;
    return true;
  }

  void close() {
    fclose(_f);
  }

  void add_deferred_string(const std::string &str) {
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
  std::vector<DeferredBinary> _deferred_binary;

  std::deque<int> _file_pos_stack;
  FILE *_f;
};

struct BlockHeader;

struct ScopedBlock {
  ScopedBlock(BlockHeader &header, Writer &writer);
  ~ScopedBlock();

  BlockHeader &header;
  Writer &writer;
  int block_start;
};


template<typename T>
T max3(const T &a, const T &b, const T &c) {
  return max(a, max(b, c));
}

template<typename T>
T max4(const T &a, const T &b, const T &c, const T &d) {
  return max(a, max3(b, c, d));
}

