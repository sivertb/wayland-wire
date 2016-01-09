#include <sys/socket.h>

#ifndef CMSG_H
#define CMSG_H

struct cmsghdr *cmsg_firsthdr(struct msghdr *msgh);
struct cmsghdr *cmsg_nxthdr(struct msghdr *msgh, struct cmsghdr *cmsg);
size_t cmsg_space(size_t length);
size_t cmsg_len(size_t length);
unsigned char* cmsg_data(struct cmsghdr *cmsg);

#endif
