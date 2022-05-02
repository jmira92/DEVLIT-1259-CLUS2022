#ifndef _CONFD_UTIL_H_
#define _CONFD_UTIL_H_

#ifdef __GNUC__
#define PRINTF(F,A) __attribute__ ((format (printf, F, A)))
#else
#define PRINTF(F,A)
#endif

extern int confd_snprintf(char *src, int size, char *format, ...) PRINTF(3,4);
extern int confd_strncpy(char *dst, int dst_len, const void *src, int src_len);
extern int confd_strlcpy(char *dest, const char *src, int size);

#endif
