#include "stdafx.h"
#include "utils.hpp"
#include "fbx_conv.hpp"

using namespace std;

void split_path(const char *path, std::string *drive, std::string *dir, std::string *fname, std::string *ext) {
  char drive_buf[_MAX_DRIVE];
  char dir_buf[_MAX_DIR];
  char fname_buf[_MAX_FNAME];
  char ext_buf[_MAX_EXT];
  _splitpath(path, drive_buf, dir_buf, fname_buf, ext_buf);
  if (drive) *drive = drive_buf;
  if (dir) *dir = dir_buf;
  if (fname) *fname = fname_buf;
  if (ext) *ext = ext_buf;
}

ScopedBlock::ScopedBlock(BlockHeader &header, Writer &writer) 
  : header(header), writer(writer) {
  writer.push_pos();
  writer.write(header);
  block_start = writer.pos();
}

ScopedBlock::~ScopedBlock() {
  header.size = writer.pos() - block_start;
  writer.push_exch();
  writer.write(header);
  writer.pop_pos();
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

