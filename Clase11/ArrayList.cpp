#include <iostream>
using namespace std;


struct ArrayListSt {
int cantidad; // cantidad de elementos
int* elementos; // array de elementos
int capacidad; // tamaño del array
};

typedef ArrayListSt* ArrayList;


ArrayList newArrayList() {
 ArrayListSt* array = new ArrayListSt; 
 array ->cantidad = 0;
 array ->capacidad = 16;
 array ->elementos = 0;
 return array; 

}
ArrayList newArrayListWith(int capacidad) {
 ArrayListSt* array = new ArrayListSt; 
 array ->cantidad = 0;
 array ->capacidad = capacidad;
 array ->elementos = 0;
 return array; 

}
int lengthAL(ArrayList xs) {
    return xs ->cantidad;

}
int get(int i, ArrayList xs) {
    if (i >= 0 && i < xs->cantidad) {
        return xs->elementos[i];
    } else {
        cout << "Índice fuera de rango en get: " << i << endl;
        return -1; 
    }
}
void set(int i, int x, ArrayList xs) {
     if (i >= 0 && i < xs->cantidad) {
         xs->elementos[i] = x;
    }
}
void resize(int capacidad, ArrayList xs) {
    if(capacidad == xs ->capacidad ) return;
    int* nuevoArray = new int[capacidad];
    int nuevosElementos = (xs->cantidad < capacidad) ? xs->cantidad : capacidad; 

    for(int i = 0; i < nuevosElementos; i++) {
        nuevoArray[i] = xs -> elementos[i];
    }
    delete[] xs ->elementos; 

    xs -> elementos = nuevoArray;
    xs -> cantidad = nuevosElementos;
    xs -> capacidad = capacidad; 
}
// Decrementa o aumenta la capacidad del array.
// Nota: en caso de decrementarla, se pierden los elementos del nal de la lista.
void add(int x, ArrayList xs) {
    if (xs->cantidad < xs->capacidad) {
        xs->elementos[xs->cantidad] = x;
        xs->cantidad += 1;
    }
}
void remove(ArrayList xs) {
       if (xs->cantidad > 0) {
        xs->cantidad -= 1;
    }
}



// ----------------------------IMPLEMENTACION------------------------------------


int sumatoria(ArrayList xs) {
    int cantidadLista = lengthAL(xs); 
    int sumaHastaAhora = 0;
    for(int i = 0; i < cantidadLista; i++) {
        sumaHastaAhora += get(i, xs);
    } return sumaHastaAhora; 
}

void sucesores(ArrayList xs) {
    int cantidadLista = lengthAL(xs); 
    int suc = 0;
    for(int i = 0; i < cantidadLista; i++) {
        suc = get(i, xs);
        set(i, suc + 1 , xs);
    } 
}

bool pertenece(int x, ArrayList xs) {
    int cantidadLista = lengthAL(xs); 
    for (int i = 0; i < cantidadLista; i++) {
        int valor = get(i, xs);
        if (valor == x) {
            return true;
        }
    } 
    return false;
}

int apariciones(int x, ArrayList xs) {
    int cantidadLista = lengthAL(xs); 
    int aparicionesHastaAhora = 0;
    for (int i = 0; i < cantidadLista; i++) { 
        int valor = get(i, xs);
        if (valor == x) {
            aparicionesHastaAhora ++;
        }
    } return aparicionesHastaAhora;
}

ArrayList append(ArrayList xs, ArrayList ys) {
    ArrayList nuevo = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    for (int i = 0; i < lengthAL(xs); i++) {
        add(get(i, xs), nuevo);
    }
    for (int i = 0; i < lengthAL(ys); i++) {
        add(get(i, ys), nuevo);
    }
    return nuevo;
}

int minimo(ArrayList xs) {
    int minimoHastaAhora = get(0,xs);
    for(int i = 0; i < lengthAL(xs); i++) {
        int value = get(i, xs);
        if(value < minimoHastaAhora) {
            minimoHastaAhora = value;
        }
    } return minimoHastaAhora; 
}