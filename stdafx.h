// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#undef max
#undef min

#include <tchar.h>
#include <mmsystem.h>

#include <d3d11.h>
#include <D3DX10math.h>

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <stdint.h>
#include <stdio.h>

#include <functional>
#include <iostream>
#include <iterator>
#include <limits>
#include <vector>
#include <algorithm>
#include <map>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include <boost/algorithm/string.hpp>

#define FBXSDK_NEW_API
#include <fbxsdk.h>


typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;

typedef int16_t int16;


#include <direct.h>
#include <io.h>
#include <sys/stat.h>
