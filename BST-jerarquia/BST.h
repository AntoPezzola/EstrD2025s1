#include <iostream>
using namespace std;

  struct JBSTNodeStr {
    string name;        // Nombre de la persona
    JBSTNodeStr* left;   // Empleados con nombre menor a name
    JBSTNodeStr* right;  // Empleados con nombre mayor a name
    JBSTNodeStr* parent; // Superior jerarquico directo de name
  };

  struct JBSTStr {
  JBSTNodeStr* root; // Root no puede ser null 
  };

typedef JBSTStr* jerarquiaBST;


JBSTNodeStr* find(string nombre, JBSTNodeStr* t) {

JBSTNodeStr* nodoActual = t;
     while (nodoActual != NULL && nodoActual->name != nombre)
    {
    if(nodoActual->name > nombre) {
        nodoActual = nodoActual->right;
    } else {
        nodoActual = nodoActual->left;
    }
    } return nodoActual;
    
}

void insertar (string nuevo, string superior, JBSTNodeStr* t) {
//que modifica la jerarquia t agregando a la persona de nombre nuevo como subordinado
JBSTNodeStr* newNode = new JBSTNodeStr;
newNode->name = nuevo;
newNode->left = NULL;
newNode->right = NULL;
newNode->parent = find(superior, t);

JBSTNodeStr* ultimoVisto = NULL;
JBSTNodeStr* actual = NULL;

    while (actual != NULL)
    {
      ultimoVisto = actual;
      if(actual->name < nuevo) {
        actual = actual->right;
      } else {
        actual->left;
      }
    }

    if(ultimoVisto->name < nuevo) {
        ultimoVisto->right = newNode;
    } 
    else {
        ultimoVisto->left = newNode;
    }
 }

 bool esSubordinadoDe(string nombreE, string nombreS, jerarquiaBST bst) {
//   que indica si la persona con nombre empleado es subordinada (directa o 
//   indirectamente) de la persona con nombre superior.
// PRECONDICIONES:
//   -Existe una persona en la jerarquia con el nombre del empleado dado.
//   -Existe una persona en la jerarquia con el nombre del superior dado.

JBSTNodeStr* actual = find(empleado, bst);

while (actual != NULL && actual->name != nombreS){
      actual = actual ->parent;
}
return actual != NULL;
 }




