#include <iostream>
using namespace std;
#include "Pokemon.h"

int main() {
    Pokemon anto = consPokemon("Fuego");
    Pokemon leon = consPokemon("Aire");
    cout << "Enerias: "  << energia(anto) << " y " << energia(leon) << endl; 
    perderEnergia(5, anto);
    perderEnergia(1, leon);
    cout << "Después de perder energia " << energia(anto) << " y " << energia(leon) << endl;
}
   