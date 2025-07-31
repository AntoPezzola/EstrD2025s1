#include <iostream>
#include "LinkedListSt.h" // tu interfaz
using namespace std;


struct NodoL {
int elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};
struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
};
typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL

struct IteratorSt {
NodoL* current;
};
typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL

LinkedList nil() {
 LinkedListSt* list = new LinkedListSt;
 list->cantidad = 0;
 list->primero = NULL;
 return list;
};

bool isEmpty(LinkedList xs) {
    return xs->cantidad == 0;
};

int head(LinkedList xs) {
    if (xs->cantidad >= 1) {
       return xs->primero->elem;  
    } return -1;
};

void Cons(int x, LinkedList xs) {
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = xs->primero;
    xs->primero = nodo;
    xs->cantidad++;
 
};

void Tail(LinkedList xs) {
    if(xs->cantidad != 0) {
    NodoL* tmp = xs->primero;
    xs->primero = xs->primero->siguiente;
    delete tmp;
    xs->cantidad--;
    };
};

int length(LinkedList xs) {
    return xs ->cantidad;
};

void Snoc(int x, LinkedList xs) {
    NodoL* newN = new NodoL;
    newN->elem = x;
    newN ->siguiente = NULL;
    if(xs ->primero == NULL) {
        xs ->primero = newN;
    } else {
        NodoL* actual = xs -> primero;
        while (actual-> siguiente != NULL) {
            actual = actual ->siguiente;
        } actual -> siguiente = newN;
    } xs ->cantidad++;

};

ListIterator getIterator(LinkedList xs){
     IteratorSt* it = new IteratorSt;
     it ->current = xs->primero;
     return it; 
}

int current(ListIterator ixs) {
    return ixs ->current->elem; 
};

void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x; 
};

void Next(ListIterator ixs){
   ixs->current = ixs->current->siguiente;
};

bool atEnd(ListIterator ixs){
    return ixs->current->siguiente == NULL; 
};

void DisposeIterator(ListIterator ixs) {
    delete ixs;
};

void DestroyL(LinkedList xs){
    NodoL* actual = xs ->primero;
    while(actual != NULL) {
        NodoL* tmp = actual;
        actual = actual ->siguiente;
        delete tmp;
    }
   delete xs; 
};

void printList(LinkedList xs) {
    ListIterator it = getIterator(xs);
    while (!atEnd(it)) {
        cout << current(it) << " ";
        Next(it);
    }
    // Imprimir el último elemento
    if (!isEmpty(xs)) {
        cout << current(it);
    }
    cout << endl;
    DisposeIterator(it);
}
