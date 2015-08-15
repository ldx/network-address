#include <stdint.h>

struct network_info4 {
       uint32_t ifindex;
       uint32_t family;
       uint32_t prefixlen;
       uint32_t address;
       uint8_t pad[12];
};

struct network_info6 {
       uint32_t ifindex;
       uint32_t family;
       uint32_t prefixlen;
       uint32_t address[4];
};

struct network_info_storage {
       uint32_t ifindex;
       uint32_t family;
       uint32_t prefixlen;
       uint8_t address[16];
};

struct network_info4 *get_if_addrs4(char *interface, int *n);
struct network_info6 *get_if_addrs6(char *interface, int *n);
struct network_info_storage *get_if_addrs(char *interface, int *n);
char *ifindex2name(uint32_t ifindex, char *name, uint32_t length);
