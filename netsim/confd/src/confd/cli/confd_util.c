#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "confd_util.h"

/* a version of snprintf that accepts negative n for size */
/* and treats it as zero                                  */
int confd_snprintf(char *str, int size, char *format, ...)
{
    va_list args;
    int ret;

    if (size < 0) {
        size = 0;
    }
    va_start(args, format);
    ret = vsnprintf(str, size, format, args);
    va_end(args);
    return ret;
}

/*  Like a mix of strncpy() and memcpy(). If src_len is less than zero
    it is treated as zero. If src_len is less than dest_len everything
    (src_len) will be copied. If src_len is greater than or equal to
    dest_len the result will be truncated. null is always added to the
    end of dest unless dest_len < 1. The return value of the function
    is the number of bytes copied (excluding null). Note that unlike
    strncpy() this function does not check for null inside src, nor
    does it fill the remaining part of dest with null. */
int confd_strncpy(char *dest, int dest_len, const void *src, int src_len)
{
    if (src_len < 0) {
        src_len = 0;
    }
    if (src_len < dest_len) {
        memcpy(dest, src, src_len);
        dest[src_len] = '\0';
        return src_len;
    }
    if (dest_len > 0) {
        memcpy(dest, src, dest_len-1);
        dest[dest_len-1] = '\0';
        return dest_len - 1;
    }
    return 0;
}

/*  This function is similar to strncpy(), but it copies at most
    size-1 bytes to dest, always adds a terminating null byte (unless
    size is zero) and does not pad the destination with (further) null
    bytes.  This function fixes some of the problems of strcpy() and
    strncpy(), but the caller must still handle the possibility of
    data loss if size is too small.  The return value of the function
    is the length of src, which allows truncation to be easily
    detected: if the return value is greater than or equal to size,
    truncation occurred.  If loss of data matters, the caller must
    either check the arguments before the call, or test the function
    return value.  strlcpy() is not present in glibc and is not
    standardized by POSIX, so we provide an implementation here. A
    difference between the BSD version and this is that size is
    defined as int (size_t on BSD). This allow size to negative which
    is treated as zero. */
int confd_strlcpy(char *dest, const char *src, int size)
{
    const char *ps = src;

    if (size < 0) {
        size = 0;
    }
    if (size) {
        while (--size && *ps) {
            *dest++ = *ps++;
        }
        *dest = 0;
    }
    if (size == 0) {
        while (*ps) {
            ps++;
        }
    }
    return ps - src;
}
