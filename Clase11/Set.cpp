#include <iostream>
#include "Set.h"
using namespace std;


struct NodoS {
int elem; 
NodoS* siguiente;
};
struct SetSt {
int cantidad; 
NodoS* primero; 
};

typedef SetSt* Set;


Set emptyS(){
    SetSt* set = new SetSt;
    set->cantidad = 0;
    set->primero = NULL;
    return set;
};

bool isEmptyS(Set s) {
   return s->cantidad == 0;
};

void AddS(int x, Set s) {
    NodoS* actual = s->primero;
      while (actual != NULL) {
        if (actual->elem == x) {
            return; 
        }
        actual = actual->siguiente;
    }
    NodoS* newNodo = new NodoS;
    newNodo->elem = x;
    newNodo->siguiente = s->primero;
    s->primero = newNodo;
    s->cantidad++;
};


 void RemoveS(int x, Set s) {
 // Caso especial: el primer nodo tiene el valor a eliminar
 if (s->primero != NULL && s->primero->elem == x) {
     NodoS* aEliminar = s->primero;
     s->primero = s->primero->siguiente;
     delete aEliminar;
     s->cantidad--;
     return;
 }
 // Caso general: buscamos en el resto de la lista
 NodoS* nodo = s->primero;
 while (nodo != NULL && nodo->siguiente != NULL) {
     if (nodo->siguiente->elem == x) {
         NodoS* aEliminar = nodo->siguiente;
         nodo->siguiente = nodo->siguiente->siguiente;
         delete aEliminar;
         s->cantidad--;
         return;
     }
     nodo = nodo->siguiente;
     }
 }

int sizeS(Set s) {
    return s ->cantidad;
}

LinkedList setToList(Set s) {
    LinkedList newList = nil();
    NodoS * actual = s->primero;
    while (actual != NULL) {
        Snoc(actual->elem, newList);
        actual = actual ->siguiente;
    }
    return newList;   
}

void DestroyS(Set s) {
    NodoS* nodo = s->primero;
    while (nodo != NULL){
        NodoS* aEliminar = nodo;
        nodo = nodo ->siguiente;
        delete aEliminar;
        s->cantidad--;
    }
    delete s;
}









