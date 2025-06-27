#include <iostream>
#include "Pokemon.h" // tu header con typedefs y funciones
#include "Entrenador.h" // tu header con typedefs y funciones

using namespace std;


int main() {
    // Crear pokémon
    Pokemon pikachu = consPokemon("Eléctrico");
    perderEnergia(10, pikachu); // energía = 90

    Pokemon charmander = consPokemon("Fuego");
    perderEnergia(50, charmander); // energía = 50

    Pokemon squirtle = consPokemon("Agua");
    perderEnergia(20, squirtle); // energía = 80

    Pokemon bulbasaur = consPokemon("Planta");
    perderEnergia(80, bulbasaur); // energía = 20

    // Crear arrays
    Pokemon* listaAsh = new Pokemon[2]{ pikachu, charmander };
    Pokemon* listaMisty = new Pokemon[2]{ squirtle, bulbasaur };
    Pokemon* listaBrock = new Pokemon[1]{ bulbasaur };

    // Crear entrenadores
    Entrenador ash = consEntrenador("Ash", 2, listaAsh);
    Entrenador misty = consEntrenador("Misty", 2, listaMisty);
    Entrenador brock = consEntrenador("Brock", 1, listaBrock);

    Pokemon p1 = pokemonNro(1, ash);
    Pokemon p2 = pokemonNro(2, ash);
    Pokemon pInvalid = pokemonNro(3, ash); // fuera de rango

    cout << "Pokémon 1 de Ash: " << tipoDePokemon(p1) << " (" << energia(p1) << ")" << endl;
    cout << "Pokémon 2 de Ash: " << tipoDePokemon(p2) << " (" << energia(p2) << ")" << endl;


    cout << "Ash le gana a todos los de Misty? " << (leGanaATodos(ash, misty) ? "Sí" : "No") << endl;
    cout << "Misty le gana a todos los de Ash? " << (leGanaATodos(misty, ash) ? "Sí" : "No") << endl;
    cout << "Ash le gana a todos los de Brock? " << (leGanaATodos(ash, brock) ? "Sí" : "No") << endl;
    cout << "Brock le gana a todos los de Misty? " << (leGanaATodos(brock, misty) ? "Sí" : "No") << endl;


}