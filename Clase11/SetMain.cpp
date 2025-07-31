#include <iostream>
#include "Set.h"  // interfaz pública del conjunto
using namespace std;

void printList(LinkedList xs); // definida más abajo, usa interfaz de lista

int main() {
    Set s = emptyS();

    AddS(10, s);
    AddS(5, s);
    AddS(8, s);
    AddS(5, s); // No debe duplicarse

    LinkedList lista = setToList(s);
    cout << "Contenido del conjunto:" << endl;
    printList(lista);

    RemoveS(5, s);

    LinkedList lista2 = setToList(s);
    cout << "Después de eliminar 5:" << endl;
    printList(lista2);

    DestroyS(s);
    DestroyL(lista);
    DestroyL(lista2);
    return 0;
}
