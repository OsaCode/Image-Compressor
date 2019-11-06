# Image Compressor

USAGE : ./imageCompressor n e path

n   =   Nombre de couleurs de l'image désirée
e   =   Limite de convergence (Plus la valeur est faible, plus le résultat sera précis, et plus le processus sera long)
IN  =   Chemin vers le fichier contenant les couleurs des pixels

EXEMPLE :   ./imageCompressor 2 0.8 exemple.in

FORMAT  :   (X,Y) (R,G,B)\n[...]

IN EXEMPLE : cat exemple.in
(0,1) (98,99,233)
(2,0) (88,77,211)
(0,2) (45,12,167)
(1,0) (33,16,94)
(1,1) (78,8,9)
(1,2) (20,27,67)
(2,1) (1,56,37)
(0,0) (66,20,26)
(2,2) (15,89,40)