#include <iostream>
#include "LinkedListSt.h" 

using namespace std;

void imprimirLista(LinkedList xs) {
    ListIterator it = getIterator(xs);
    cout << "[ ";
    while (!atEnd(it)) {
        cout << current(it) << " ";
        Next(it);
    }
    cout << "]";
    DisposeIterator(it);
}

int main() {
    // Crear lista xs
    LinkedList xs = nil();

    // Agregar elementos
    Cons(3, xs);
    Cons(2, xs);
    Cons(1, xs);  // xs = [1, 2, 3]
    Snoc(4, xs);  // xs = [1, 2, 3, 4]

    cout << "Lista xs: ";
    imprimirLista(xs);
    cout << endl;

    // Copiar xs
    LinkedList copia = copy(xs);
    cout << "Copia de xs: ";
    imprimirLista(copia);
    cout << endl;

    // Buscar mínimo
    cout << "Mínimo de xs: " << minimo(xs) << endl;

    // Ver si 3 pertenece
    cout << "¿3 pertenece? " << (pertenece(3, xs) ? "Sí" : "No") << endl;

    // Contar apariciones de 2
    cout << "Apariciones de 2: " << apariciones(2, xs) << endl;

    // Crear ys y hacer Append(xs, ys)
    LinkedList ys = nil();
    Cons(7, ys);
    Cons(6, ys);  // ys = [6, 7]

    Append(xs, ys); // xs ahora es [1, 2, 3, 4, 6, 7], ys se destruyó

    cout << "Después de Append, xs: ";
    imprimirLista(xs);
    cout << endl;

    // Destruir listas
    DestroyL(xs);
    DestroyL(copia);

    return 0;
}
