# ocaml-genetic-TSP

## Compilation

cd graph/source
ocamlbuild graph2.byte gfile2.byte tools.byte algogen.byte test.byte
./test.byte

## Usage

Importer le graphe : 
    let graph = from_file "path"

Chercher le TSP :
    genetic_algo graph nbPopulation txElitisme txMutation nbGénération

    nbPopulation : Nombre d'individus dans la population. Entier > 0
    txElitisme : Pourcentage des meilleurs individus a conserver à chaque génération. Float [0; 100]
    txMutation : Probabilité qu'une mutation survienne. Entier [0; 100]
    nbGénation : Nombre de génération à faire. Entier > 0
    




