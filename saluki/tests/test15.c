#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>


int init_connection(const char *name) {
    struct addrinfo *addr;
    int fd = -1;
    if (getaddrinfo(name, "80",NULL,&addr) < 0) return -1;
    if (addr == NULL) return -2;
    fd = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (fd < 0) return -3;
    if (connect(fd, addr->ai_addr, addr->ai_addrlen) < 0) return -4;
    freeaddrinfo(addr);
    return fd;
}


int main(void) {
    int fd;
    char buf[BUFSIZ] = {0};
    printf("Enter address: ");
    if (fgets(buf,BUFSIZ - 1, stdin) == NULL) return 1;
    fd = init_connection(buf);
    if (fd < 0) return fd;
    printf("Enter message: ");
    if (fgets(buf, BUFSIZ - 1, stdin) == NULL) return 1;
    if (send(fd, buf, BUFSIZ, 0) < 0) return 2;
    close(fd);
}
