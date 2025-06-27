#include <iostream>
#include "Pokemon.h"
using namespace std;

typedef string TipoDePokemon;

struct PokeSt {
TipoDePokemon tipo;
int vida;
};

// ¿Qué es un puntero a una estructura?
// Un puntero a una estructura te permite:

// Alocar memoria dinámicamente con malloc o new (según si usás C o C++).

// Pasar objetos grandes sin copiarlos (solo se pasa la dirección en memoria).

// Modificar el contenido original desde funciones.

Pokemon consPokemon(TipoDePokemon tipo) {
    PokeSt* pok = new PokeSt; 
     pok -> tipo = tipo;
     pok -> vida = 100; 
     return pok; 
}

TipoDePokemon tipoDePokemon(Pokemon p) {
    return p -> tipo; 
}
int energia(Pokemon p) {
    return p -> vida; 
}
void perderEnergia(int energia, Pokemon p) {
    p -> vida = p -> vida - energia;
}
bool superaA(Pokemon p1, Pokemon p2) {
    return energia(p1) > energia(p2);
}
