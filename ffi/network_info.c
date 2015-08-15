#include <arpa/inet.h>

#include <netlink/route/link.h>
#include <netlink/route/addr.h>

#include "network_info.h"

struct network_info_array {
       int n;
       struct network_info_storage *network_info;
};

static void fill_ni(struct network_info_storage *ni, unsigned int ifindex,
                    struct sockaddr *sa, unsigned int plen)
{
       struct sockaddr_in *si;
       struct sockaddr_in6 *si6;

       memset(ni, 0, sizeof(struct network_info_storage));

       ni->ifindex = ifindex;
       ni->prefixlen = plen;
       ni->family = sa->sa_family;

       switch (sa->sa_family) {
              case AF_INET:
                     si = (struct sockaddr_in *)sa;
                     memcpy(ni->address, &si->sin_addr.s_addr, 4);
                     break;
              case AF_INET6:
                     si6 = (struct sockaddr_in6 *)sa;
                     memcpy(ni->address, si6->sin6_addr.s6_addr, 16);
                     break;
              default:
                     /* Unknown address family. */
                     break;
       }
}

static void nl_cache_callback(struct nl_object *obj, void *arg)
{
       unsigned int ifindex;
       struct nl_addr *addr;
       struct sockaddr_storage ss;
       socklen_t slen = sizeof(ss);
       struct network_info_array *nia = arg;
       struct network_info_storage *ni;

       addr = rtnl_addr_get_local((struct rtnl_addr *)obj);
       if (!addr)
              return;

       if (nl_addr_fill_sockaddr(addr, (struct sockaddr *)&ss, &slen) < 0)
              return;

       ifindex = rtnl_addr_get_ifindex((struct rtnl_addr *)obj);
       if (ifindex == RTNL_LINK_NOT_FOUND)
              return;

       nia->n++;
       nia->network_info = realloc(nia->network_info,
                                   nia->n * sizeof(struct network_info_storage));
       ni = nia->network_info + nia->n - 1;
       fill_ni(ni, ifindex, (struct sockaddr *)&ss,
               nl_addr_get_prefixlen(addr));
}

static struct network_info_storage *_get_if_addrs(char *interface, int family, int *n)
{
       int ifindex = RTNL_LINK_NOT_FOUND;
       struct nl_handle *sock = NULL;
       struct nl_cache *addr_cache = NULL;
       struct nl_cache *link_cache = NULL;
       struct rtnl_addr *addr = NULL;
       struct network_info_array nia = {
              .n = 0,
              .network_info = NULL
       };

       *n = -1;

       sock = nl_handle_alloc();
       if (!sock)
              goto out;

       if (nl_connect(sock, NETLINK_ROUTE) < 0)
              goto out;

       addr_cache = rtnl_addr_alloc_cache(sock);
       link_cache = rtnl_link_alloc_cache(sock);
       if (!link_cache || !addr_cache)
              goto out;

       if (interface != NULL && strcmp(interface, "")) {
              ifindex = rtnl_link_name2i(link_cache, interface);
              if (ifindex == RTNL_LINK_NOT_FOUND)
                     goto out;
       }

       addr = rtnl_addr_alloc();
       if (!addr)
              goto out;

       if (family > 0)
              rtnl_addr_set_family(addr, family);
       if (ifindex != RTNL_LINK_NOT_FOUND)
              rtnl_addr_set_ifindex(addr, ifindex);

       nl_cache_foreach_filter(addr_cache,
                               (struct nl_object *)addr,
                               nl_cache_callback,
                               &nia);
       *n = nia.n;

out:
       if (addr)
              rtnl_addr_put(addr);
       if (addr_cache)
              nl_cache_free(addr_cache);
       if (link_cache)
              nl_cache_free(link_cache);
       if (sock)
              nl_handle_destroy(sock);

       return nia.network_info;
}

struct network_info_storage *get_if_addrs(char *interface, int *n)
{
       return _get_if_addrs(interface, 0, n);
}

struct network_info4 *get_if_addrs4(char *interface, int *n)
{
       return (struct network_info4 *)_get_if_addrs(interface, AF_INET, n);
}

struct network_info6 *get_if_addrs6(char *interface, int *n)
{
       return (struct network_info6 *)_get_if_addrs(interface, AF_INET6, n);
}

char *ifindex2name(uint32_t ifindex, char *name, uint32_t length)
{
       char *res = NULL;
       struct nl_handle *sock = NULL;
       struct nl_cache *link_cache = NULL;

       sock = nl_handle_alloc();
       if (!sock)
              goto out;

       if (nl_connect(sock, NETLINK_ROUTE) < 0)
              goto out;

       link_cache = rtnl_link_alloc_cache(sock);
       if (!link_cache)
              goto out;

       res = rtnl_link_i2name(link_cache, ifindex, name, (size_t)length);

out:
       if (link_cache)
              nl_cache_free(link_cache);
       if (sock)
              nl_handle_destroy(sock);
       return res;
}
#ifdef TEST_MAIN

#include <stddef.h>

int main(int argc, char **argv)
{
       if (argc != 2)
              return 1;

       int n;
       char buf[128] = { '\0' };
       struct network_info4 *ni4 = get_if_addrs4(argv[1], &n);
       if (!ni4)
              return 1;
       int i;
       for (i = 0; i < n; i++) {
              struct network_info4 *tmp = ni4 + i;
              inet_ntop(AF_INET, (void *)&tmp->address, buf, sizeof(buf));
              printf("%d -> %s/%d (%d)\n", tmp->ifindex, buf, tmp->prefixlen, tmp->family);
       }
       free(ni4);

       struct network_info6 *ni6 = get_if_addrs6(argv[1], &n);
       if (!ni6)
              return 1;
       for (i = 0; i < n; i++) {
              struct network_info6 *tmp = ni6 + i;
              inet_ntop(AF_INET6, (void *)&tmp->address, buf, sizeof(buf));
              printf("%d -> %s/%d (%d)\n", tmp->ifindex, buf, tmp->prefixlen, tmp->family);
       }
       free(ni6);

       return 0;
}
#endif
