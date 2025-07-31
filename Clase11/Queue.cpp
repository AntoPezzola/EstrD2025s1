#include <iostream>
#include "Queue.h"
using namespace std;

struct NodoQ {
int elem; // valor del nodo
NodoQ* siguiente; // puntero al siguiente nodo
}
struct QueueSt {
int cantidad; // cantidad de elementos
NodoQ* primero; // puntero al primer nodo
NodoQ* ultimo; // puntero al ultimo nodo
}
typedef QueueSt* Queue;


Queue emptyQ() {
    QueueSt* queue = new QueueSt;
    queue ->cantidad = 0; 
    queue ->primero = NULL;
    queue ->ultimo = NULL;
    return queue;
};

bool isEmptyQ(Queue q) {
    return q ->cantidad == 0;
};

int firstQ(Queue q) {
    return q ->primero;
}
void Enqueue(int x, Queue q) {
    NodoQ* newElem = new NodoQ;
    newElem->elem = x;
    newElem->siguiente = NULL;

    if(q->primero == NULL) {
        q->primero = newElem;
        q->ultimo = newElem;
    } else {
        q->ultimo->siguiente = newElem;
    }

    q->cantidad++;
}

void Dequeue(Queue q) {
    if (q->primero != NULL) {
        NodoQ* primerElem = q->primero;
        q->primero = q->primero->siguiente;
        
        if (q->primero == NULL) {
            q->ultimo = NULL;
        }

        delete primerElem;
        q->cantidad--;
    }
}
int lengthQ(Queue q) {
    return q ->cantidad;
}
void MergeQ(Queue q1, Queue q2) {
    if(q1->primero == NULL) {
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
    } else {
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    q1->cantidad++; 
    delete q2; 
}

void DestroyQ(Queue q) {
    NodoQ* actual = q->primero;
        while (actual != NULL) {
        NodoQ* aEliminar = actual;
        actual = actual->siguiente;
        delete aEliminar;
        q->cantidad--
    }

    delete q;
    
}
