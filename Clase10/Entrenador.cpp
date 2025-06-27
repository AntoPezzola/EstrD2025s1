#include <iostream>
#include "Pokemon.h"
using namespace std;

struct EntrenadorSt {
string nombre;
Pokemon* pokemon;
int cantPokemon;
};
typedef EntrenadorSt* Entrenador;


Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* e = new EntrenadorSt;
    e ->nombre = nombre;
    e ->pokemon = pokemon;
    e ->cantPokemon = cantidad; 
    return e; 
}
string nombreDeEntrenador(Entrenador e) {
    return e ->nombre; 
}
int cantidadDePokemon(Entrenador e) {
   return e ->cantPokemon; 
}

Pokemon pokemonNro(int i, Entrenador e) {
// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
 if (i <= e -> cantPokemon) {
    return e ->pokemon[i-1];
    } return NULL;
}

bool leGanaATodos(Entrenador e1, Entrenador e2) {
    for (int i = 0; i < e2->cantPokemon; i++) {
        bool existeGanador = false;

        for (int j = 0; j < e1->cantPokemon && !existeGanador; j++) {
            existeGanador = superaA(e1->pokemon[j], e2->pokemon[i]);
        }

        if (!existeGanador) return false;
    }

    return true;
}
