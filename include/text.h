#include <Rts.h>


HsInt find_json_string_end(uint32_t* state, const unsigned char* ba, HsInt offset, HsInt len);
HsInt decode_json_string(char *dest, const char *src, HsInt srcoff, HsInt srclen);