#include "node.h"

#include <stdlib.h>
#include <stdio.h>
#include "y.tab.h"

node *new_node(const node_type type){
    node *new_node = (node *)malloc(sizeof(node));
    new_node->type = type;

    int body_count = get_body_count(type);
    new_node->body = calloc(body_count, sizeof(node*));

    return new_node;
}

int get_body_count(const node_type type) {
  switch (type){
  case PROGRAM:
  case WHILE:
    return 2;
  case FOR:
    return 4;
  case ASSIGN:
  case IF:
  case EXPR:
    return 3;
  case STATEMENT:
    return 1;
  default:
    return 0;
  }
}
