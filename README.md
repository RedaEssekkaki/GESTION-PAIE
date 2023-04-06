
BIENVENUE cher.e utilisateur/trice !!!!

Ce read me vous permettra d'avoir une prise en main de notre logiciel de gestion paie.

	I - INSTALLATION
		- Veuillez installer le logiciel OpenCobolIDE 4.7.6 : 
		- Ouvrez le fichier "GestionPaie" -> lancer l'application avec le bouton "RUN"
		- L'application est désormais manipulable dans le terminal de votre système, agrandissez la fenêtre du terminal pour la visualiser encore mieux!

		- Avec ligne de commande si vous installez OPEN COBOL 1.1 du CIE : 
		- Ouvrez le fichier "GestionPaie" dans un éditeur -> compiler à l'aide de la commande "cobc -x GestionPaie.cob"
		- Exécutez avec la commande "./GestionPaie"

	II - UTILISATION
Au lancement du logiciel, vous avez le choix entre deux Menus : 
	
	1 - Menu administrateur { Username: admin } uniquement pour la direction des ressources humaines et le CEO
				{ Password: admin }
    
    2 - Menu directeur pour le chef d'entreprise qui veut un acces simplifié aux fonctionnalités {Username : directeur}{Password : directeur}
	3 - Menu employé/utilisateur : pas de username, pas de password

DISCLAIMERS : 

-----ATTENTION le logiciel effacera vos données de test à la fin de programme, ceci pour permettre de repartir du même environnement à chaque fois
-----ATTENTION si vous sortez d'un menu, le logiciel s'arretera et les données que vous avez rentré seront réinitialisés pour permettre à quelqu'un d'autre de tester le logiciel à partir de zéro
-----ATTENTION des enregistrements d'employés et de domaines sont déjà renseignés dans le logiciel, vous pouvez directement utiliser ceux ci pour vos test ou proceder avec vos jeux de test en supprimant les enregistrements avec les fonctionnalités de suppression données

    TESTS conseillés pour un premier contact avec le logiciel:
    - Connectez-vous en tant que admin
    - affichez tous les employés avec la fonctionnalité donnée
    - affichez le metier avec la fonctionnalité donnée
    - essayez de produire des bulletins de paie pour le mois courant -- attention vous pourrez faire ça une seule fois pas session d'execution du logiciel
    - produisez un rapport de paie pour le mois courant en saisissant les valeurs demandés


Si vous choisissez de supprimer les données des employés présents et le metier déjà présent au lanceent du logiciel il faudra suivre une séquence d'opérations à réaliser dans l'ordre pour avoir un workflow idéal du logiciel:
!!! Vous devez d'abord inserer les données avec le rôle "administrateur" selon un ordre de priorité.  !!!
      -->LES SAISIES à réaliser dans l'ordre:
	1 - Saisir les métiers
	2 - Saisir les employés avec les metiers existants
	3 - Production des bulletins de paie
	4 - Production des rapports de paie
      
      -->LES CONSULTATIONS (sans ordre specifique, pour vérifier que les données sont bien chargés):
	5 - Consulter les metiers, les employés, les bulletins de paie, les rapports de paie

      -->LES EDITIONS:
	6 - Editer les informations des employés, les metiers, la mise à jour du taux des cotisations sociales(25%)

      -->LES SUPPRESSIONS:
	7 - Supprimer les metiers, les employés. 

Le Menu employé permet aux employés ou simples utilisateurs d'avoir uniquement la rubrique/
      -->LES CONSULTATIONS:
		Des bulletins de paie et des metiers.


	III - CONTRIBUTIONS
	entreprise : G4-cinématique
	developpeurs : Reda ESSEKKAKI, Christel MVELE, Aubin KAMANDA, Fatoumata DIALLO


	NO COPYRIGHT. ALL RIGHTS RESERVED 2023 ©
