
#include <text.h>
#include <stdint.h>
#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const uint8_t utf8d[] = {
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x00 ~ 0x1F
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x20 ~ 0x3F
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x40 ~ 0x5F
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x60 ~ 0x7F
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,   // 0x80 ~ 0x9F
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,   // 0xA0 ~ 0xBF
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,   // 0xC0 ~ 0xDF
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,   // 0xE0 ~ 0xFF

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12, 
};

static uint32_t inline updatestate(uint32_t *state, uint32_t byte) {
    uint32_t type = utf8d[byte];
    *state = utf8d[256 + *state + type];
    return *state;
}

static inline uint32_t decode_hex(uint32_t c) {
    if (c >= '0' && c <= '9')      return c - '0';
    else if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    else if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return 0xFFFFFFFF; // Should not happen
}

HsInt decode_json_string(char *dest, const char *src, HsInt srcoff, HsInt srclen) {
    char *d = dest;
    const char *s      = src + srcoff;
    const char *srcend = s + srclen;

    uint32_t state = UTF8_ACCEPT;
    unsigned char cur_byte;

    uint8_t surrogate = 0;
    uint32_t temp_hex = 0;
    uint32_t unidata;
    // ECMA 404 require codepoints beyond Basic Multilingual Plane encoded as surrogate pair
    uint32_t h_surrogate;
    uint32_t l_surrogate;

// read current byte to cur_byte and guard input end
#define DISPATCH(label) {\
    if (s >= srcend) {\
        return -1;\
    }\
    cur_byte = *s++;\
    goto label;\
}

standard:
    // Test end of stream
    while (s < srcend) {
        cur_byte = *s++;
        if (updatestate(&state, (uint32_t)cur_byte) == UTF8_REJECT) { return -1; }

        if (cur_byte == '\\')
            DISPATCH(backslash)
        else {
            *d++ = cur_byte;
        }
    }
    // Exit point, use sign bit to indicate utf8 validation error
    return (state == UTF8_ACCEPT) ? (d - dest) : (dest - d);

backslash:
    switch (cur_byte) {
        case '\\':
        case '/':
            *d++ = cur_byte;
            goto standard;
            break;
        case '\n': 
            *d++ = cur_byte;
            while (s < srcend && *s == ' ') {
                cur_byte = *s++;
            }
            goto standard;
        case 'b': *d++ = '\b';goto standard;
        case 'f': *d++ = '\f';goto standard;
        case 'n': *d++ = '\n';goto standard;
        case 'r': *d++ = '\r';goto standard;
        case 't': *d++ = '\t';goto standard;
        case 'u': DISPATCH(unicode1);;break;
        default:
            return -1;
    }

unicode1:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata = temp_hex << 12;
    DISPATCH(unicode2);
unicode2:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata |= temp_hex << 8;
    DISPATCH(unicode3);
unicode3:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata |= temp_hex << 4;
    DISPATCH(unicode4);
unicode4:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata |= temp_hex;
    if (surrogate) {
        if (unidata < 0xDC00 || unidata > 0xDFFF) // is not low surrogate
            return -1;
        surrogate = 0;
        // decode surrogate pair
        l_surrogate = unidata;  
        unidata = 0x10000;
        unidata += (h_surrogate & 0x03FF) << 10;
        unidata += (l_surrogate & 0x03FF);
    } else if (unidata >= 0xD800 && unidata <= 0xDBFF ) { // is high surrogate
        surrogate = 1;
        DISPATCH(surrogate1);
    } else if (unidata >= 0xDC00 && unidata <= 0xDFFF) { // is low surrogate
        return -1;
    }
    // encode unidata into UTF8 bytes
    if (unidata <= 0x7F) {
        // plain ASCII
        *d++ = (char) unidata;
    }
    else if (unidata <= 0x07FF) {
        // 2-byte unicode
        *d++ = (char) (((unidata >> 6) & 0x1F) | 0xC0);
        *d++ = (char) (((unidata >> 0) & 0x3F) | 0x80);
    }
    else if (unidata <= 0xFFFF) {
        // 3-byte unicode
        *d++ = (char) (((unidata >> 12) & 0x0F) | 0xE0);
        *d++ = (char) (((unidata >>  6) & 0x3F) | 0x80);
        *d++ = (char) (((unidata >>  0) & 0x3F) | 0x80);
    }
    else if (unidata <= 0x10FFFF) {
        // 4-byte unicode
        *d++ = (char) (((unidata >> 18) & 0x07) | 0xF0);
        *d++ = (char) (((unidata >> 12) & 0x3F) | 0x80);
        *d++ = (char) (((unidata >>  6) & 0x3F) | 0x80);
        *d++ = (char) (((unidata >>  0) & 0x3F) | 0x80);
    }
    else { 
        // error 
        return -1;
    }
    goto standard;
surrogate1:
    if (cur_byte != '\\') { return -1; }
    h_surrogate = unidata;
    DISPATCH(surrogate2)
surrogate2:
    if (cur_byte != 'u') { return -1; }
    DISPATCH(unicode1)
}

HsInt find_json_string_end(uint32_t* state, const unsigned char* ba, HsInt offset, HsInt len){
    const unsigned char *s   = ba + offset;
    const unsigned char *end = s + len;
    uint32_t skip = *state >> 8;
    uint32_t escaped = *state & 0xFF;
    for (; s < end; s++) {
        if (skip == 1){
            skip = 0;       // skip this char
        }
        else if (*s == '\\') {  // backslash
            escaped = 1;
            skip = 1;
        }
        else if (s + 2 < end && *s == '\"' && *(s+1) == '\"' && *(s+2) == '\"') {  // double quote
            *state = (skip << 8) | escaped; // save the state
            return (s - ba - offset + 2);
        } else if (*s <= 0x1F && ((*s != 0x0A) || (s + 1) < end && *s != 0x0D && *(s + 1) != 0x0A)) {  // unescaped control characters
            escaped = 3;          // even if it's skipped, it will be rejected in decode_json_string
        }
    }
    *state = (skip << 8) | escaped; // save the state
    return (-1);
}