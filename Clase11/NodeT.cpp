#include <iostream>
#include "NodeT.h" // tu interfaz
#include "ArrayList.cpp" // impl array
using namespace std;

struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};

typedef NodeT* Tree;

Tree empty() {
    NodeT* node = new NodeT;
    node->elem = 0;
    node->left = NULL;
    node->right = NULL;
    return node; 
}


Tree nodeT(int elem, Tree left, Tree right) {
     NodeT* node = new NodeT;
    node->elem = elem;
    node->left = left;
    node->right = right;
    return node; 
}

bool isEmptyT(Tree t) {
    return t->elem == NULL;
}

int rootT(Tree t) {
      return t->elem;
}
Tree left(Tree t) {
    return t->left;
}
Tree right(Tree t) {
    return t->right;
}




// ========================== USUARIO ===================================

int sumarT(Tree t) {
    if (isEmptyT(t)) {
        return 0;
    } else return (rootT(t) + sumarT(left(t)) + sumarT(right(t)));
}


int sizeT(Tree t) {
        if (isEmptyT(t)) {
        return 0;
    } else return (1 + sizeT(left(t)) + sizeT(right(t)));
}

bool perteneceT(int e, Tree t) {
    return rootT(t) == x o perteneceT(e, left(t)) o perteneceT(e, right(t)); 
}

int aparicionesT(int e, Tree t) {
    if(rootT(t) == e) {
      return  1 + aparicionesT(e, left(t)) + aparicionesT(e,right(t));
    } else return aparicionesT(e, left(t)) + aparicionesT(e,right(t));
}

int heightT(Tree t) {
    if(isEmptyT(t)) {
        return 0;
    } else return (1 + max(heightT(t) , heightT(right(t)))); 

}

ArrayList toList(Tree t) {
    ArrayList newArray = newArrayListWith(sizeT(t) + 1);
    createArray(t, newArray);
    return newArray;
}

void createArray(Tree t, ArrayList a) {
    if(!isEmptyT(t)) {
      createArray(left(t), a);
      add(rootT(t), a);// mi lado recurso 'caso borde'
      createArray(right(t),a);
    }

    // primero izq como para mantener un orden
}

ArrayList leaves(Tree t) {
 ArrayList newArray = newArrayListWith(sizeT(t) + 1);
 createLeavesArray(t, newArray);
 return newArray;
}

void createLeavesArray(Tree t, ArrayList a) {
    if(esHoja(t)) {
        add(rootT(t), a);
    } else {
        createLeavesArray(left(t), a);
        createLeavesArray(right(t), a);
}
    }


bool esHoja(Tree t) {
    return rootT(t) != NULL && left(t) == NULL && right(t) == NULL;  
}

