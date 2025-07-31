#include <iostream>
#include "Clase11/ArrayList.h"
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
NodoL* ultimo; 
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
    return ixs->current == NULL; 
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

ArrayList ascendente(int* list, int length) {
    ArrayList arrayResult = newArrayListWith(length);

    if(length == 0) {
        return arrayResult;
    }

    int vistoHastaAhora = get(0, arrayResult);
    add(vistoHastaAhora, list);

    for (int i = 1; i < length; i++) {
    int actual = get(i, list);
    if (actual > vistoHastaAhora){
        vistoHastaAhora = actual;
        add(actual, arrayResult);
    } else {
        break;
    }
      return arrayResult;
    }

}

ArrayList toArray(LinkedList l) { 
 ArrayList arrayResult = newArrayListWith(l->cantidad);
 IteratorSt* it = getIterator(l);

 while (!atEnd(it)) {
    int actual = current(it);
     add(actual, arrayResult);
     Next(it);
 }

 DisposeIterator(it)
 DestroyL(l);

 return arrayResult; 
 }

// LinkedList Reverse(Linkedlist l) {

// }

LinkedList from(ArrayList array) {
// convierte los elementos del array en una linkedlist
 LinkedListSt* list = new LinkedListSt;
 list->cantidad = 0;
 list->primero = NULL;

 NodoL* ultimo = NULL;

for (int i = 0; i < lengthAL(array); i++) {
  NodoL* newNodo = new NodoL;
  newNodo->elem = get(i, array);
  newNodo->siguiente = NULL;

  if(list->primero == NULL) {
      list ->primero = newNodo
  } else {
     ultimo->siguiente = ultimo;
  }
  ultimo = newNodo; 
  list->cantidad++; 
}
 return list; 
 }

LinkedList from(int values[], int size)  {
 LinkedListSt* list = new LinkedListSt;
 list->cantidad = size;
 list->primero = NULL; 

 NodoL* ultimo = NULL;

for (int i = 0; i < size ; i++) {
  NodoL* newNodo = new NodoL;
  newNodo->elem = values[i];
  newNodo->siguiente = NULL;

  if(list->primero == NULL) {
      list ->primero = newNodo; 
  } else {
     ultimo->siguiente = newNodo;
  }
  ultimo = newNodo; 
}
 return list; 
 }

// ==========================================================================


LinkedList mapCuadrado(LinkedList xs) {
    LinkedList nueva = new LinkedListSt;
    nueva->cantidad = 0;
    nueva->primero = NULL;

    NodoL* actual = xs->primero;
    NodoL* ultimo = NULL;

    while (actual != NULL) {
        NodoL* nuevo = new NodoL;
        nuevo->elem = actual->elem * actual->elem;
        nuevo->siguiente = NULL;

        if (nueva->primero == NULL) {
            nueva->primero = nuevo;
        } else {
            ultimo->siguiente = nuevo;
        }

        ultimo = nuevo;
        nueva->cantidad++;
        actual = actual->siguiente;
    }

    return nueva;
}

void removeValue(LinkedList xs, int x) {

    NodoL* actual = xs->primero;
    NodoL* anterior = NULL;

    while (actual != NULL){
    if(actual->elem == x) {
        NodoL* temp = actual;
        if (anterior == NULL) {
            xs ->primero = actual -> siguiente;
            actual = xs ->primero;
        } else {
            anterior ->siguiente = actual ->siguiente;
            actual = actual->siguiente; 
        } delete temp;
        xs->cantidad--;
    } anterior = actual;
      actual = actual ->siguiente; 

    }
    

}


void Reverse(Linkedlist list) {
    NodoL* prev = NULL;
    NodoL* current = list-> primero; 

    while (current != NULL)  {
      NodoL* next = current->siguiente;
      current->siguiente = prev;
      prev = current;
      current = next;
    }
    list-> primero = prev; 
    
}