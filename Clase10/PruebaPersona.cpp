#include <iostream>
using namespace std;
#include "Persona.h"

int main() {
    Persona anto = consPersona("Anto", 24);
    Persona leon = consPersona("Leon", 31);
    cambioDeNombre("Rocio", anto);
    cout << "Nombres: " << nombre(anto) << " y " << nombre(leon) << ", Edad: " << edad(anto) << " y " << edad(leon) << endl;
    crecer(anto);
    crecer(leon);
    cout << "Después de crecer, Edad: " << edad(anto) << " y " << edad(leon) << " El mayor: " << nombre(laQueEsMayor(anto, leon)) << endl;
    return 0;
}
   