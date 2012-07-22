#pragma once

class BitReader
{
public:
  BitReader(uint8 *data, uint32 len_in_bits);
  uint32 read(uint32 count);
  uint32 read_varint(); 
  bool eof() const;
private:
  uint32 _length_in_bits;
  uint32 _bit_offset;
  uint32 _byte_offset;
  uint8 *_data;
};

class BitWriter
{
public:
  BitWriter(uint32 buf_size = 1024);
  ~BitWriter();
  void write(uint32 value, uint32 num_bits);
  void write_varint(uint32 value);
  void get_stream(std::vector<uint8> *out, uint32 *bit_length);
  void get_stream(uint8 **out, uint32 *bit_length);
private:
  uint32 _bit_offset;
  uint32 _byte_offset;
  uint32 _buf_size;
  uint8 *_buf;
};
