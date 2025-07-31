#include <iostream>
#include "ArrayList.h" // tu interfaz
using namespace std;

int main() {
    // Crear listas
    ArrayList a = newArrayList();
    add(3, a);
    add(5, a);
    add(3, a);
    add(7, a);
    add(2, a);

    ArrayList b = newArrayList();
    add(10, b);
    add(20, b);

    // --- PRUEBA sumatoria ---
    cout << "sumatoria(a) = " << sumatoria(a) << " (esperado: 20)" << endl;

    // --- PRUEBA sucesores ---
    sucesores(a);
    cout << "sucesores(a): ";
    for (int i = 0; i < lengthAL(a); i++) cout << get(i, a) << " ";
    cout << "(esperado: 4 6 4 8 3)" << endl;

    // --- PRUEBA pertenece ---
    cout << "pertenece(6, a) = " << pertenece(6, a) << " (esperado: 1)" << endl;
    cout << "pertenece(99, a) = " << pertenece(99, a) << " (esperado: 0)" << endl;

    // --- PRUEBA apariciones ---
    cout << "apariciones(4, a) = " << apariciones(4, a) << " (esperado: 2)" << endl;

    // --- PRUEBA append ---
    ArrayList c = append(a, b);
    cout << "append(a, b): ";
    for (int i = 0; i < lengthAL(c); i++) cout << get(i, c) << " ";
    cout << "(esperado: 4 6 4 8 3 10 20)" << endl;

    // --- PRUEBA minimo ---
    cout << "minimo(c) = " << minimo(c) << " (esperado: 3)" << endl;

    // --- Liberar memoria ---
    destroyArrayList(a);
    destroyArrayList(b);
    destroyArrayList(c);

    return 0;
}
