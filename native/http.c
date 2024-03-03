#include <lean/lean.h>
#include <netdb.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/socket.h>

/*********************/

struct addr_info_list {
	struct addrinfo* addrs;
	struct addrinfo* current;
};

static lean_external_class* addr_info_list_class = NULL;

static void addr_info_list_finalize(void * obj) {
	struct addr_info_list* list = (struct addr_info_list*)obj;
	freeaddrinfo(list->addrs);
	free(list);
}

static void addr_info_list_foreach(void *, b_lean_obj_arg) {
	// no nested lean objects
}

static lean_object* addr_info_list_to_lean(struct addr_info_list* list) {
	if (addr_info_list_class == NULL) {
		addr_info_list_class = lean_register_external_class(
			addr_info_list_finalize,
			addr_info_list_foreach
		);
	}
	return lean_alloc_external(addr_info_list_class, list);
}

lean_obj_res get_addr_info(lean_obj_arg lean_host, lean_obj_arg lean_service) {
	const char const* host = lean_string_cstr(lean_host);
	const char const* service = lean_string_cstr(lean_service);

	struct addrinfo hints;
	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;

	struct addr_info_list* list = malloc(sizeof(struct addr_info_list));
	int res = getaddrinfo(host, service, &hints, &list->addrs);
	if (res != 0) {
		free(list);
		return lean_io_result_mk_error(
			lean_mk_io_error_other_error(
				-res,
				lean_mk_string(gai_strerror(res))
			)
		);
	}

	list->current = list->addrs;
	return lean_io_result_mk_ok(addr_info_list_to_lean(list));
}

lean_obj_res addr_info_list_next(lean_obj_arg lean_list) {
	struct addr_info_list* list = lean_get_external_data(lean_list);
	/*
	printf("%i %i %i %i %s\n",
		list->current->ai_family,
		list->current->ai_socktype,
		list->current->ai_flags,
		list->current->ai_protocol,
		list->current->ai_canonname
	);
	*/
	if (list->current->ai_next) {
		list->current = list->current->ai_next;
		return lean_io_result_mk_ok(lean_box(1));
	}
	return lean_io_result_mk_ok(lean_box(0));
}

uint32_t addr_info_list_get_family(lean_obj_arg lean_list) {
	struct addr_info_list* list = lean_get_external_data(lean_list);
	return list->current->ai_family;
}

uint32_t addr_info_list_inet_get_addr(lean_obj_arg lean_list) {
	struct addr_info_list* list = lean_get_external_data(lean_list);
	struct sockaddr_in* in = (struct sockaddr_in*)(list->current->ai_addr);
	return htonl(in->sin_addr.s_addr);
}

uint16_t addr_info_list_inet_get_port(lean_obj_arg lean_list) {
	struct addr_info_list* list = lean_get_external_data(lean_list);
	struct sockaddr_in* in = (struct sockaddr_in*)(list->current->ai_addr);
	return htons(in->sin_port);
}
