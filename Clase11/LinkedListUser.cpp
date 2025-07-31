#include <iostream>
#include "LinkedListSt.cpp" // tu interfaz implementada
using namespace std;


int sumatoria(LinkedList xs) {
    int sumaHastaAhora = 0;
  ListIterator it = getIterator(xs);
    while(!atEnd(it)) {
        sumaHastaAhora += current(it);
        Next(it);
    }
    DisposeIterator(it);
    return sumaHastaAhora; 
}

void Sucesores(LinkedList xs) {
   ListIterator it = getIterator(xs);
    while(!atEnd(it)) {
      int valor = current(it);
       SetCurrent(valor + 1, it); 
       Next(it);
    }
    DisposeIterator(it);
}

bool pertenece(int x, LinkedList xs) {
     ListIterator it = getIterator(xs);
    while(!atEnd(it) && current(it) != x) {
        Next(it)
    }    
    bool encontrado = !atEnd(it); 
    DisposeIterator(it);
    return encontrado;
}

int apariciones(int x, LinkedList xs) {
     ListIterator it = getIterator(xs);
    int aparicionesHastaAhora = 0;
     while(!atEnd(it)) {
        if(current(it) == x) {
        aparicionesHastaAhora ++;
        }
        Next(it);
    }
    DisposeIterator(it);
    return aparicionesHastaAhora; 
}

int minimo(LinkedList xs) {
    if (isEmpty(xs)) {
        return NULL; // o throw, o INT_MAX, según lo que esperes
    }

 ListIterator it = getIterator(xs);
 int minHastaAhora = current(it);
       while(!atEnd(it)) {
        if(current(it) < minHastaAhora) {
            minHastaAhora = current(it);
        }
        Next(it);
       }
       DisposeIterator(it);
       return minHastaAhora;
}

LinkedList copy(LinkedList xs) {
   LinkedList newXs = nil();
    ListIterator it = getIterator(xs);
    while(!atEnd(it)) {
       Snoc(current(it), newXs);
       Next(it); 
    }    
    DisposeIterator(it);
    return newXs;
}

void Append(LinkedList xs, LinkedList ys) {
    ListIterator it = getIterator(ys);
    while(!atEnd(it)) {
       Snoc(current(it), xs);
       Next(it); 
    } 
    DisposeIterator(it);
    DestroyL(ys);       
}