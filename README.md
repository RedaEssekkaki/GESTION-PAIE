
# Projet de création d'un logiciel de gestion de paie
Ce logiciel permet de gérer les processus liés à la paie des employés d'une entreprise



## Execution du programme
pour compiler le programme avec les machines de la fac utiliser le compilateur OPEN COBOL 1.1
Ajouter cette librairie pour executer: export LD_LIBRARY_PATH=/usr/local/opt/open-cobol/lib

compilation :

-en respectant le code entre les colonnes 8 et 72 : cobc -x nomfic.cob (pour écrire des commentaires - mettre un * col 7)
-sans respecter le code entre les colonnes 8 et 72 : cobc --free -x nomfic.cob (pour écrire des commentaires - mettre *> )

Exécution : ./nomfic

sur les autres PC : veuillez installer OPEN COBOL 1.1
## Utilisation de l'application 

dans ce projet on peut se connecter en tant que directeur ou admin:

- Directeur: 
username: directeur , password : directeur
- Admin:
username : admin , password : admin


## Se connecter
-pour se connecter en tant qu'admin :
![alt tag](https://gitlab.univ-nantes.fr/E20B396W/projetcobol/uploads/85c7b24fc28f4984e662a33c97e600dc/se_connecter_admin.png)

-les diffèrents fonctionnalités qu'on peut avoir une foi connecté en tant qu'admin :
![alt tag](https://gitlab.univ-nantes.fr/E20B396W/projetcobol/uploads/e1bb74bc91b7d1d059f31fe660c56e7c/connecter_tant_qu_admin.png)

-Se connecter en tant que directeur et les différents fonctionnalités qu'on peut avoir une fois connecté en tant que directeur :
![alt tag](https://gitlab.univ-nantes.fr/E20B396W/projetcobol/uploads/afea7b8df87907997cf11f6a11b45e9c/connecter_en_tant_que_directeur.png)



