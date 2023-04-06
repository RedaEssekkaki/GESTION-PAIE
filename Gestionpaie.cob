      ******************************************************************
      * Author: Essekkaki - Mvele - Kamanda - Diallo
      * Date: 2022/2023
      * Purpose:   Gestion de paie
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. Gestionpaie.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           select femployes assign to "employes.dat"
           organization indexed
           access mode is dynamic
           record key is fe_numidentification
           file status is cr_femployes.

           Select fmetiers assign to "metiers.dat"
           organization indexed
           access mode is dynamic
           record key is fm_nomMetier
           alternate record key is fm_domaine WITH DUPLICATES
           file status is cr_fmetiers.

           select fbulletinsPaie assign to "bulletinsPaie.dat"
           organization indexed
           access mode is dynamic
           record key is fbp_cleBP
           alternate record key is fbp_mois WITH DUPLICATES
           alternate record key is fbp_annee WITH DUPLICATES
           alternate record key is fbp_identifiant WITH DUPLICATES
           file status is cr_fbulletinsPaie.

           Select frapportsPaie assign to "rapportspaie.dat"
           organization indexed
           access mode is dynamic
           record key is fr_cleRapport
           alternate record key is fr_mois WITH DUPLICATES
           alternate record key is fr_annee WITH DUPLICATES
           file status is cr_frapportsPaie.

       DATA DIVISION.

       FILE SECTION.
       FD femployes.
       01 tamp_femployes.
           02 fe_numIdentification     PIC X(10).
           02 fe_nom                   PIC A(20).
           02 fe_prenom                PIC A(20).
           02 fe_dateEmbauche          PIC X(10).
           02 fe_metier                PIC A(20).

       FD fmetiers.
       01 tamp_fmetiers.
           02 fm_nomMetier          PIC A(20).
           02 fm_domaine            PIC A(20).
           02 fm_salaireBase        PIC 9(10)V99 VALUE ZEROES.
           02 fm_tauxsupplementaire PIC 9(10)V99 VALUE ZEROES.

       FD fbulletinsPaie.
       01 tamp_fbulletinsPaie.
           02 fbp_cleBP.
              03 fbp_mois          PIC 9(2).
              03 fbp_annee         PIC 9(4).
              03 fbp_identifiant   PIC X(10).
           02 fbp_jour             PIC 9(2).
           02 fbp_heuresTravailles PIC 9(3)V99.
           02 fbp_montantAvantages PIC 9(10)V99.
           02 fbp_salaireBrut      PIC 9(10)V99.
           02 fbp_cotisations      PIC 9(10)V99.
           02 fbp_autresDeductions PIC 9(10)V99.
           02 fbp_salaireNet       PIC 9(10)V99.

       FD frapportsPaie.
       01 tamp_frapportsPaie.
           02 fr_cleRapport.
               03 fr_mois          PIC 9(2).
               03 fr_annee         PIC 9(4).
           02 fr_totalSalaires         PIC 9(10)V99.
           02 fr_totalImpots           PIC 9(10)V99.
           02 fr_totalAutresDeductions PIC 9(10)V99.


       WORKING-STORAGE SECTION.
       01 CHOICE PIC 9(2).
       01 SEPARATOR PIC X(80) VALUE "--------------------------------"-
           "---------------------------------------".
       01 MENU-TITLE PIC X(80) VALUE "MENU".
       01 MENU-OPTIONS.
         05 OPTION-0 PIC X(80) VALUE
         "Revenir au menu principal".
         05 OPTION-1 PIC X(80) VALUE
         "Saisie employes".
         05 OPTION-2 PIC X(80) VALUE
         "Editer informations employe".
         05 OPTION-3 PIC X(80) VALUE
         "Retirer un employe de la liste du personnel".
         05 OPTION-4 PIC X(80) VALUE
         "Produire bulletins de paie".
         05 OPTION-5 PIC X(80) VALUE
         "Consulter bulletin de paie".
         05 OPTION-6 PIC X(80) VALUE
         "Produire rapports de paie".
         05 OPTION-7 PIC X(80) VALUE
         "Consulter rapport de paie".
         05 OPTION-8 PIC X(80) VALUE
         "Editer metier".
         05 OPTION-9 PIC X(80) VALUE
         "Saisir metiers".
         05 OPTION-10 PIC X(80) VALUE
         "Mettre a jour le taux des cotisations sociales(25%) ".
         05 OPTION-11 PIC X(80) VALUE
         "Consulter les Metiers".
         05 OPTION-12 PIC X(80) VALUE
         "Afficher details employe".
         05 OPTION-13 PIC X(80) VALUE
         "Afficher tous les employes".
         05 OPTION-14 PIC X(80) VALUE
         "Retirer des metiers".



       01 employe.
           02 temp_numIdentification  PIC X(10).
           02 temp_nom                PIC A(20).
           02 temp_prenom             PIC A(20).
           02 temp_dateEmbauche       PIC X(10).
           02 dateEmbauche.
               05 jourEmbauche PIC 99.
               05 moisEmbauche PIC 99.
               05 anneeEmbauche PIC 9(4).
           02 estDateValide PIC X(1) VALUE 'N'.
           02 temp_metier             PIC A(20).
           02 temp_montantAvantages   PIC 9(20).
           02 temp_heurestravaillees  PIC 9(20).
           02 temp_autresdeductions   PIC 9(20).

       01 bp.
           02 temp_fbp_cleBP.
              03 temp_fbp_mois         PIC 9(2).
              03 temp_fbp_annee        PIC 9(4).
              03 temp_fbp_identifiant  PIC X(10).

           02 temp_fbp_salaireBrut     PIC 9(10)V99.
           02 affichage_salaireBrut    PIC Z,ZZZ,ZZ9.99.

           02 temp_fbp_cotisations     PIC 9(10)V99.

           02 temp_fbp_autresDeductions PIC 9(10)V99.

           02 temp_fbp_salaireNet      PIC 9(10)V99.

           02 saisie_montantAvantages  PIC X(12).
           02 saisie_autresDeductions  PIC X(12).
           02 vrai_montantAvantages    PIC 9(10)V99.
           02 vrai_autresDeductions    PIC 9(10)V99.
           02 affichage_montantAvantages   PIC ZZ,ZZ9.99.


       01 metier.
           02 temp_fm_nomMetier          PIC A(20).
           02 temp_fm_domaine            PIC A(20).

           02 saisie_salaireBase        PIC X(12).
           02 vrai_salaireBase          PIC 9(10)V99.
           02 affichage_salaireBase     PIC ZZ,ZZ9.99.

           02 saisie_tauxsupplementaire PIC X(12).
           02 vrai_tauxSupplementaire   PIC 9(10)V99.
           02 affichage_taux_supplementaire PIC ZZ,ZZ9.99.

       01 rp.
           02 temp_fr_dateRapport  PIC X(10).
           02 temp_fr_mois         PIC 9(2).
           02 temp_fr_annee        PIC 9(4).
           02 temp_fr_totalSalaires        PIC 9(10)V99.
           02 temp_fr_totalImpots      PIC 9(10)V99.
           02 temp_fr_totalAutresDeductions    PIC 9(10)V99.


       01 USERNAME      PIC X(20).
       01 PASSWORD      PIC X(10).
       01 NIV-ACCES     PIC A(1).
       01 TAUX-CS       PIC 9(5)V99 VALUE 0.2.


       01 temp_taux_cs PIC 9(5)V99.
       01 tentative PIC 9(1).

       77 cr_femployes PIC 9(2).
       77 cr_fmetiers PIC 9(2).
       77 cr_fbulletinsPaie PIC 9(2).
       77 cr_frapportsPaie PIC 9(2).
       77 Wfin PIC 9.
       77 Wtrouve PIC 9.
       77 Wtrouve1 PIC 9.
       77 Wtrouve2 PIC 9.

       77 Identifiant PIC X(10).
       77 Datepaie  PIC X(10).
       77 Wheu PIC 9(10)V99.
       77 Wsal PIC 9(10)V99.
       77 salbrut PIC 9(10)V99.
       77 cotisocia PIC 9(10)V99.
       77 Advsocia PIC 9(10)V99.
       77 sumsal PIC 9(10)V99.
       77 salmid PIC 9(10)V99.
       77 salnet PIC 9(10)V99.
       77 Reste    PIC 9(3).




       PROCEDURE DIVISION.

       OPEN I-O femployes
       IF cr_femployes=35 THEN
       OPEN OUTPUT femployes
       END-IF
       CLOSE femployes

       OPEN I-O fmetiers
       IF cr_fmetiers=35 THEN
       OPEN OUTPUT fmetiers
       END-IF
       CLOSE fmetiers

       OPEN I-O fbulletinsPaie
       IF cr_fbulletinsPaie=35 THEN
       OPEN OUTPUT fbulletinsPaie
       END-IF
       CLOSE fbulletinsPaie

       OPEN I-O frapportsPaie
       IF cr_frapportsPaie=35 THEN
       OPEN OUTPUT frapportsPaie
       END-IF
       CLOSE frapportsPaie



       OPEN OUTPUT frapportsPaie
       OPEN OUTPUT fbulletinsPaie
       OPEN OUTPUT fmetiers
       OPEN OUTPUT femployes
       CLOSE frapportsPaie
       CLOSE fbulletinsPaie
       CLOSE fmetiers
       CLOSE femployes


      *Donnees pour le TEST

       OPEN I-O fmetiers
       MOVE "DEVELOPPEUR" TO fm_nomMetier
       MOVE 12.00 TO fm_salaireBase
       MOVE 1.00 TO fm_tauxsupplementaire
       MOVE "DEVELOPPEMENT" TO fm_domaine
       WRITE tamp_fmetiers
       END-WRITE
       CLOSE fmetiers

       OPEN I-O femployes
       MOVE "1" TO fe_numIdentification
       MOVE "Essekkaki" TO fe_nom
       MOVE "Reda" TO fe_prenom
       MOVE "01/01/2023" TO fe_dateEmbauche
       MOVE "DEVELOPPEUR" TO fe_metier
       WRITE tamp_femployes
       END-WRITE
       CLOSE femployes

       OPEN I-O femployes
       MOVE "2" TO fe_numIdentification
       MOVE "Mvele" TO fe_nom
       MOVE "Christel" TO fe_prenom
       MOVE "01/01/2023" TO fe_dateEmbauche
       MOVE "DEVELOPPEUR" TO fe_metier
       WRITE tamp_femployes
       END-WRITE
       CLOSE femployes


       OPEN I-O femployes
       MOVE "3" TO fe_numIdentification
       MOVE "Kamanda" TO fe_nom
       MOVE "Aubin" TO fe_prenom
       MOVE "01/01/2023" TO fe_dateEmbauche
       MOVE "DEVELOPPEUR" TO fe_metier
       WRITE tamp_femployes
       END-WRITE
       CLOSE femployes


       OPEN I-O femployes
       MOVE "4" TO fe_numIdentification
       MOVE "Diallo" TO fe_nom
       MOVE "Fatoumata" TO fe_prenom
       MOVE "01/01/2023" TO fe_dateEmbauche
       MOVE "DEVELOPPEUR" TO fe_metier
       WRITE tamp_femployes
       END-WRITE
       CLOSE femployes



       PERFORM MAIN

       STOP RUN.

       MAIN.
       DISPLAY "BIENVENUE DANS LE LOGICIEL DE GESTION DE PAIE :"
       DISPLAY "Veuillez-vous connecter en tant que admin ou directeur"
       DISPLAY SEPARATOR
       DISPLAY "Username: ".
       ACCEPT USERNAME.
       DISPLAY "Password: ".
       ACCEPT PASSWORD.
       EVALUATE USERNAME
           WHEN "admin"
               EVALUATE PASSWORD
                   WHEN "admin"
                       MOVE "A" TO NIV-ACCES
                       PERFORM MENU-GESTIONNAIRE
               END-EVALUATE

           WHEN "directeur"
               EVALUATE PASSWORD
                   WHEN "directeur"
                   MOVE "B" TO NIV-ACCES
                   PERFORM MENU-DIRECTEUR
               END-EVALUATE

           WHEN OTHER
               MOVE "C" TO NIV-ACCES
               PERFORM MENU-EMPLOYE

       END-EVALUATE.


       MENU-GESTIONNAIRE.
           DISPLAY SEPARATOR
           DISPLAY "-----------------MENU-ADMIN-----------------------"
           DISPLAY SEPARATOR
           DISPLAY "---------------GESTION EMPLOYES-------------------"
           DISPLAY "1. " OPTION-1
           DISPLAY "2. " OPTION-2
           DISPLAY "3. " OPTION-3
           DISPLAY "4. " OPTION-12
           DISPLAY "5. " OPTION-13
           DISPLAY SEPARATOR
           DISPLAY "-------------BULLETINS DE PAIE--------------------"
           DISPLAY "6. " OPTION-4
           DISPLAY "7. " OPTION-5
           DISPLAY "8. " OPTION-10
           DISPLAY SEPARATOR
           DISPLAY "-------------LES RAPPORTS DE PAIE ----------------"
           DISPLAY "9. " OPTION-6
           DISPLAY "10. " OPTION-7
           DISPLAY SEPARATOR
           DISPLAY "---------------LES METIERS------------------------"
           DISPLAY "11. " OPTION-8
           DISPLAY "12. " OPTION-9
           DISPLAY "13. " OPTION-11
           DISPLAY "14. " OPTION-14
           DISPLAY SEPARATOR
           ACCEPT CHOICE

           EVALUATE CHOICE
               WHEN 1 PERFORM SAISIE-EMPLOYE
               WHEN 2 PERFORM EDITER-EMPLOYE
               WHEN 3 PERFORM RETIRER-EMPLOYE
               WHEN 6  PERFORM PRODUIRE-BP
               WHEN 7 PERFORM CONSULTER-BP
               WHEN 9 PERFORM PRODUIRE-RBP
               WHEN 10 PERFORM CONSULTER-RBP
               WHEN 11 PERFORM EDITER-METIER
               WHEN 12 PERFORM SAISIR-METIERS
               WHEN 8 PERFORM MAJ-TAUX-CS
               WHEN 13 PERFORM CONSULTER-METIERS
               WHEN 4 PERFORM AFFICHER-EMPLOYE
               WHEN 5 PERFORM AFFICHER-TOUS-EMPLOYES
               WHEN 14 PERFORM RETIRER-METIER
               WHEN OTHER DISPLAY "choix non valide --> sortie"
           END-EVALUATE.


       MENU-DIRECTEUR.
           DISPLAY SEPARATOR
           DISPLAY "MENU-DIRECTEUR"
           DISPLAY SEPARATOR
           DISPLAY "1. " OPTION-3
           DISPLAY "2. " OPTION-5
           DISPLAY "3. " OPTION-7
           DISPLAY "4. " OPTION-11
           DISPLAY "5. " OPTION-12
           DISPLAY "6. " OPTION-13
           DISPLAY "7. " OPTION-14


           DISPLAY SEPARATOR
           ACCEPT CHOICE
           EVALUATE CHOICE
               WHEN 1 PERFORM RETIRER-EMPLOYE
               WHEN 2 PERFORM CONSULTER-BP
               WHEN 3 PERFORM CONSULTER-RBP
               WHEN 4 PERFORM CONSULTER-METIERS
               WHEN 5 PERFORM AFFICHER-EMPLOYE
               WHEN 6 PERFORM AFFICHER-TOUS-EMPLOYES
               WHEN 7 PERFORM RETIRER-METIER
           END-EVALUATE.


       MENU-EMPLOYE.
           DISPLAY SEPARATOR
           DISPLAY "MENU-EMPLOYE"
           DISPLAY "1. " OPTION-5
           DISPLAY "2. " OPTION-11
           DISPLAY SEPARATOR
           ACCEPT CHOICE
           EVALUATE CHOICE
               WHEN 1 PERFORM CONSULTER-BP
               WHEN 2 PERFORM CONSULTER-METIERS
           END-EVALUATE.


       SAISIE-EMPLOYE.
      * ici le code --------------
           DISPLAY SEPARATOR
           DISPLAY "SAISIE EMPLOYE"
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
               DISPLAY "Numero d'identification: "
               ACCEPT temp_numIdentification
               OPEN INPUT femployes
               MOVE temp_numIdentification TO fe_numIdentification
               READ femployes
                   INVALID KEY MOVE 0 TO Wtrouve
                   NOT INVALID KEY
                       DISPLAY "Employe deja present, sortir et",
                       " faire editer employe plutot"
                       MOVE 1 TO Wtrouve
               END-READ
               CLOSE femployes
           END-PERFORM

           DISPLAY "Nom: "
           ACCEPT temp_nom
           DISPLAY "Prenom: "
           ACCEPT temp_prenom

           PERFORM WITH TEST AFTER UNTIL estDateValide = 'Y'
               DISPLAY "Date d'embauche (format: JJ/MM/AAAA): "
               ACCEPT temp_dateEmbauche
               PERFORM VerifierDate
               IF estDateValide = 'Y'
            DISPLAY "Date d'embauche valide: ",
            jourEmbauche, "/",
            moisEmbauche, "/", anneeEmbauche
           ELSE
               DISPLAY "Date d'embauche invalide"
           END-IF
           END-PERFORM

           DISPLAY "Les metiers deja existants (copier-coller) : "
           OPEN INPUT fmetiers

           PERFORM WITH TEST AFTER UNTIL Wfin = 1
               READ fmetiers NEXT
               AT END DISPLAY "--fin metiers" MOVE 1 TO Wfin
               NOT AT END DISPLAY fm_nomMetier
               END-READ
           END-PERFORM

           CLOSE fmetiers
           DISPLAY SEPARATOR
           MOVE 0 TO Wtrouve
           MOVE 0 TO tentative
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 OR tentative > 3
               DISPLAY "Quel Metier aura le nouveau employe ? : "
               ACCEPT temp_metier
               OPEN INPUT fmetiers
               MOVE temp_metier TO fm_nomMetier
               READ fmetiers
                   INVALID KEY DISPLAY "Le metier est inexistant"-
                   " essayez encore, si n'existe pas dans la liste ",
                   "ajouter un metier quelconque ensuite realiser ",
                   "la saisie du metier, apres vous pourrez editer ",
                   "la fiche de l'employe plus tard"
                   COMPUTE tentative = tentative + 1
                   NOT INVALID KEY DISPLAY "metier ajouté avec success"
                   MOVE 1 TO Wtrouve
               END-READ
               CLOSE fmetiers
           END-PERFORM

           IF tentative > 3 THEN
               DISPLAY "Trop de tentatives infructueuses --> sortie"
               CLOSE femployes
               CLOSE fbulletinsPaie
               CLOSE fmetiers
               PERFORM RETOUR-MENU
               STOP RUN
           END-IF


           OPEN I-O femployes
           MOVE temp_numIdentification TO fe_numIdentification
           MOVE temp_nom TO fe_nom
           MOVE temp_prenom TO fe_prenom
           MOVE temp_dateEmbauche TO fe_dateEmbauche
           MOVE temp_metier TO fe_metier

           WRITE tamp_femployes
           END-WRITE
           CLOSE femployes
           IF cr_femployes = "00" THEN
               DISPLAY "Employe " fe_nom " ajoute avec succes"
           ELSE
               DISPLAY "erreur lors de la saisie"
           END-IF

       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       ACCEPT CHOICE
           PERFORM RETOUR-MENU.

       AFFICHER-EMPLOYE.
           DISPLAY "AFFICHER EMPLOYE"
           DISPLAY "Identifiant employe : "
           ACCEPT temp_numIdentification
           OPEN INPUT femployes
           MOVE temp_numIdentification TO fe_numIdentification
           READ femployes
           INVALID KEY DISPLAY "Employe inexistant"
               PERFORM RETOUR-MENU
           NOT INVALID KEY
               DISPLAY "Numero d'identification: " fe_numIdentification
               DISPLAY "Nom: " fe_nom
               DISPLAY "Prenom: " fe_prenom
               DISPLAY "Date d'embauche: " fe_dateEmbauche
               DISPLAY "Metier: " fe_metier
           END-READ
           CLOSE femployes
           DISPLAY "--fin, toucher envoi pour retourner au menu--"
           ACCEPT CHOICE
           PERFORM RETOUR-MENU.

       AFFICHER-TOUS-EMPLOYES.
           DISPLAY "AFFICHER TOUS LES EMPLOYES"
           OPEN INPUT femployes
           MOVE 0 TO Wfin
           PERFORM WITH TEST AFTER UNTIL Wfin = 1
               READ femployes NEXT
               AT END MOVE 1 TO Wfin
               NOT AT END
                   DISPLAY "Numero d'identification: "
                       fe_numIdentification
                   DISPLAY "Nom: "
                       fe_nom
                   DISPLAY "Prenom: "
                       fe_prenom
                   DISPLAY "Date d'embauche: "
                       fe_dateEmbauche
                   DISPLAY "Metier: "
                       fe_metier
                   DISPLAY SEPARATOR
               END-READ
           END-PERFORM
           CLOSE femployes.
           DISPLAY "--fin, toucher envoi pour retourner au menu--"
           ACCEPT CHOICE
           PERFORM RETOUR-MENU.

       EDITER-EMPLOYE.
           DISPLAY "EDITER EMPLOYE"
           DISPLAY "Identifiant employe : "
           ACCEPT temp_numIdentification
           OPEN I-O femployes
           MOVE temp_numIdentification TO fe_numIdentification
           READ femployes
           INVALID KEY DISPLAY "Employe inexistant"
               PERFORM RETOUR-MENU
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Quel nouveau metier aura l' employe ? : "
               ACCEPT temp_metier
               OPEN INPUT fmetiers
               MOVE temp_metier TO fm_nomMetier
               READ fmetiers
                   INVALID KEY DISPLAY "Le metier est inexistant"-
                   " essayez encore, si n'existe pas dans la liste ",
                   "ajouter un metier quelconque ensuite realiser ",
                   "la saisie du metier, apres vous pourrez editer ",
                   "la fiche de l'employe plus tard"
                   NOT INVALID KEY DISPLAY "metier ajoute avec success"
                   MOVE 1 TO Wtrouve
               END-READ
               END-PERFORM

               MOVE temp_metier TO fe_metier

               REWRITE tamp_femployes
           END-READ
           CLOSE femployes
           IF cr_femployes = "00" THEN
               DISPLAY "Employe " fe_nom "edite avec succes"
           ELSE
               DISPLAY "erreur lors de l'edition" cr_femployes
           END-IF
           DISPLAY "--fin, toucher envoi pour retourner au menu--"
           ACCEPT CHOICE
           PERFORM RETOUR-MENU.

       RETIRER-EMPLOYE.
           DISPLAY "RETIRER EMPLOYE"
           DISPLAY SEPARATOR
           DISPLAY "Identifiant employe : "
           ACCEPT temp_numIdentification
           OPEN I-O femployes
           MOVE temp_numIdentification TO fe_numIdentification
           READ femployes
               INVALID KEY
                   DISPLAY "Employe inexistant"
                   PERFORM RETOUR-MENU
               NOT INVALID KEY DISPLAY "Vous etes sur/e? "
                   DISPLAY "0. - oui"
                   DISPLAY "1. - non"
                   ACCEPT CHOICE
                   EVALUATE CHOICE
                       WHEN 0
                           DELETE femployes RECORD
                           DISPLAY "Employe "
            fe_nom "retire avec succes"
                       WHEN OTHER
                           PERFORM RETOUR-MENU
                   END-EVALUATE
           END-READ
           CLOSE femployes
           DISPLAY "--fin, toucher envoi pour retourner au menu--"
           ACCEPT CHOICE
           PERFORM RETOUR-MENU.



      *-----------------------------------------------------------------
       SAISIR-METIERS.
       DISPLAY "Saisie des informations sur le metiers des employes"
       DISPLAY SEPARATOR
       OPEN I-O fmetiers
       PERFORM UNTIL CHOICE = 0
       DISPLAY "Nom du metier? : "
       ACCEPT temp_fm_nomMetier
       MOVE temp_fm_nomMetier TO fm_nomMetier
       MOVE 0 TO Wtrouve
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 OR Wfin = 1
           READ fmetiers
               INVALID KEY
                   MOVE 1 TO Wfin
                   DISPLAY "Domaine du metier? : "
                   ACCEPT temp_fm_domaine
                   MOVE temp_fm_domaine TO fm_domaine
           PERFORM WITH TEST AFTER UNTIL fm_salaireBase > 0
                       DISPLAY "Salaire de base? XX.XX:  "
                       ACCEPT saisie_salaireBase
                       INSPECT saisie_salaireBase REPLACING ALL ','
                       BY '.'
                       MOVE FUNCTION NUMVAL(saisie_salaireBase) TO
                           vrai_salaireBase
                       MOVE vrai_salaireBase TO fm_salaireBase
                       MOVE vrai_salaireBase TO affichage_salaireBase
                       DISPLAY "saisie : " affichage_salaireBase
                   END-PERFORM
           PERFORM WITH TEST AFTER UNTIL fm_tauxsupplementaire > 0
                       DISPLAY "Taux supplementaire? XX.XX : "
                       ACCEPT saisie_tauxsupplementaire
                       INSPECT saisie_tauxsupplementaire
                       REPLACING ALL ',' BY '.'
                       MOVE FUNCTION
                           NUMVAL(saisie_tauxsupplementaire) TO
                           vrai_tauxSupplementaire
                       MOVE vrai_tauxSupplementaire
                           TO fm_tauxsupplementaire
                       MOVE vrai_tauxSupplementaire
                           TO affichage_taux_supplementaire
                       DISPLAY "saisie : " affichage_taux_supplementaire
                   END-PERFORM
                   MOVE 0 TO Wtrouve
                   WRITE tamp_fmetiers
                   END-WRITE
               NOT INVALID KEY
                   DISPLAY "Metier deja renseigne"
                   MOVE 1 TO Wtrouve
               END-READ
               END-PERFORM
           DISPLAY "Voulez-vous saisir un "-
               "autre metier (Oui : 1/Non : 0)? : "
           ACCEPT CHOICE
       END-PERFORM
       CLOSE fmetiers
       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       PERFORM RETOUR-MENU.


       PRODUIRE-BP.
       DISPLAY "Produire des bulletins de paie"
       DISPLAY SEPARATOR
       OPEN I-O femployes
       OPEN INPUT fmetiers
       OPEN I-O fbulletinsPaie


       MOVE FUNCTION CURRENT-DATE(5:2) TO fbp_mois
       MOVE FUNCTION CURRENT-DATE(1:4) TO fbp_annee
       MOVE FUNCTION CURRENT-DATE(7:2) TO fbp_jour

       START fbulletinsPaie, KEY IS = fbp_annee
       INVALID KEY DISPLAY "Verification annee: OK"
       NOT INVALID KEY
           START fbulletinsPaie, KEY IS = fbp_mois
               INVALID KEY DISPLAY "Verification mois : OK"
               NOT INVALID KEY
                   DISPLAY "Les bulletins de paie pour ce mois ",
                   "existent deja, impossible de les redeclarer ",
                   "choisir l'option --consulter bulletin paie"
                   CLOSE fbulletinsPaie
                   CLOSE fmetiers
                   CLOSE femployes
                   PERFORM RETOUR-MENU
                   STOP RUN
           END-START
       END-START


       DISPLAY "date du bulletin: " fbp_jour,"/" fbp_mois,"/" fbp_annee

       DISPLAY SEPARATOR
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
       READ femployes NEXT
           AT END MOVE 1 TO Wfin
           NOT AT END
               DISPLAY "Nom : " fe_nom
               DISPLAY "Prenom : " fe_prenom
               DISPLAY "Identifiant : " fe_numIdentification
               DISPLAY SEPARATOR

               MOVE 0 TO Wtrouve
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Combien d'heures a travaille " fe_nom,fe_prenom,
               " ? "
               ACCEPT temp_heurestravaillees
               IF temp_heurestravaillees > 0 THEN
                   MOVE 1 TO Wtrouve
               ELSE DISPLAY "Le montant d'heures doit etre doit etre",
                   " positif"
               END-IF
               END-PERFORM

               MOVE 0 TO Wtrouve
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Montant des avantages sociaux pour ce mois ?",
               " format : XXX,XX"
               ACCEPT saisie_montantAvantages
               INSPECT saisie_montantAvantages
                       REPLACING ALL ',' BY '.'
               MOVE FUNCTION
                           NUMVAL(saisie_montantAvantages) TO
                           vrai_montantAvantages
               MOVE vrai_montantAvantages TO affichage_montantAvantages
               DISPLAY "saisie : " affichage_montantAvantages
               IF vrai_montantAvantages < 0 THEN
                   MOVE 0 to Wtrouve
                   DISPLAY "Ce montant doit etre positif"
               ELSE MOVE 1 TO Wtrouve
               END-IF
               END-PERFORM

               MOVE 0 TO Wtrouve
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Montant autres deductions sur salaire ce mois",
               " ? XXX.XX"
               ACCEPT saisie_autresDeductions
               INSPECT saisie_autresDeductions
                       REPLACING ALL ',' BY '.'
               MOVE FUNCTION
                           NUMVAL(saisie_autresDeductions) TO
                           vrai_autresDeductions
               MOVE vrai_autresDeductions TO affichage_montantAvantages
               DISPLAY "saisie : " affichage_montantAvantages
               IF vrai_autresDeductions < 0 THEN
                   MOVE 0 to Wtrouve
                   DISPLAY "Ce montant doit etre positif"
               ELSE MOVE 1 TO Wtrouve MOVE vrai_autresDeductions TO
                   temp_fbp_autresDeductions
               END-IF
               END-PERFORM


               MOVE fe_numIdentification TO fbp_identifiant

               PERFORM CALCULER-SALAIRE

               MOVE temp_fbp_salaireBrut TO fbp_salaireBrut
               MOVE temp_fbp_cotisations TO fbp_cotisations
               MOVE temp_fbp_autresDeductions TO fbp_autresDeductions
               MOVE temp_fbp_salaireNet TO fbp_salaireNet
               DISPLAY SEPARATOR
               DISPLAY "Bulletin de paie genere pour "
               fe_nom " " fe_prenom " (ID: " fe_numIdentification ")"
               DISPLAY "Date de paie : " fbp_jour, "/", fbp_mois,"/"
               fbp_annee
               DISPLAY "Identifiant de l'employe : " fbp_identifiant
               DISPLAY "Nom de l'employe : " fe_nom
               DISPLAY "Prenom de l'employe : " fe_prenom

           MOVE temp_heurestravaillees TO affichage_salaireBrut
               DISPLAY "Heures travaillees : " affichage_salaireBrut

           MOVE temp_fbp_salaireBrut TO affichage_salaireBrut
               DISPLAY "Salaire brut : " affichage_salaireBrut

           DISPLAY "Montant avantages : " affichage_montantAvantages

           MOVE temp_fbp_cotisations TO affichage_salaireBrut
               DISPLAY "Cotisations : " affichage_salaireBrut

           MOVE fbp_autresdeductions TO affichage_salaireBrut
               DISPLAY "Autres deductions : " affichage_salaireBrut

           MOVE fbp_salaireNet TO affichage_salaireBrut
               DISPLAY "Salaire net : " affichage_salaireBrut
               DISPLAY SEPARATOR

               WRITE tamp_fbulletinsPaie
               END-WRITE
       END-READ
       END-PERFORM
       CLOSE femployes fmetiers fbulletinsPaie
       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       ACCEPT CHOICE
       PERFORM RETOUR-MENU.

       CALCULER-SALAIRE.
       MOVE fe_metier TO fm_nomMetier
       OPEN INPUT fmetiers
       READ fmetiers
       INVALID KEY DISPLAY "Le metier de cet employe n'existe pas"
       NOT INVALID KEY
               COMPUTE temp_fbp_salaireBrut =
               temp_heurestravaillees * fm_salaireBase +
               (temp_heurestravaillees * fm_tauxsupplementaire) +
               vrai_montantAvantages

               COMPUTE temp_fbp_cotisations =
                   temp_fbp_salaireBrut * TAUX-CS
               COMPUTE temp_fbp_salaireNet = temp_fbp_salaireBrut -
                   temp_fbp_cotisations - vrai_autresDeductions
       END-READ
       CLOSE fmetiers.


       CONSULTER-BP.
       DISPLAY "Consulter un bulletin de paie"
       DISPLAY SEPARATOR
       OPEN INPUT fbulletinsPaie
       DISPLAY "Entrer Identifiant employe"
       ACCEPT temp_fbp_identifiant
       MOVE 0 TO Wtrouve
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
           DISPLAY "Entrer le mois du bulletin de paie (format: MM): "
           ACCEPT temp_fbp_mois
           IF temp_fbp_mois < 0 OR temp_fbp_mois > 12 THEN
               MOVE 0 TO Wtrouve
               DISPLAY "mois au mauvais format, reessayer "
           ELSE MOVE 1 TO Wtrouve
       END-PERFORM

       MOVE 0 TO Wtrouve
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
       DISPLAY "Entrer l'annee du bulletin de paie (format: AAAA): "
       ACCEPT temp_fbp_annee
       IF temp_fbp_annee < 0 OR temp_fbp_annee >
           FUNCTION CURRENT-DATE(1:4)
           THEN
               MOVE 0 TO Wtrouve
           ELSE MOVE 1 TO Wtrouve
       END-PERFORM

       DISPLAY "Mois saisi : " temp_fbp_mois
       DISPLAY "Annee saisie : " temp_fbp_annee

       MOVE temp_fbp_mois TO fbp_mois
       MOVE temp_fbp_annee TO fbp_annee
       MOVE temp_fbp_identifiant TO fbp_identifiant

       READ fbulletinsPaie KEY IS fbp_identifiant

       INVALID KEY DISPLAY "Bulletin de paie non trouve"

       NOT INVALID KEY
           DISPLAY SEPARATOR
           DISPLAY "Date de paie : "
           fbp_jour, "/", fbp_mois, "/", fbp_annee

           DISPLAY "Identifiant de l'employe : " fbp_identifiant


           MOVE fbp_salaireBrut TO temp_fbp_salaireBrut
           MOVE temp_fbp_salaireBrut TO affichage_salaireBrut
           DISPLAY "Salaire brut : " affichage_salaireBrut

           MOVE fbp_cotisations TO temp_fbp_cotisations
           MOVE temp_fbp_cotisations TO affichage_salaireBrut
           DISPLAY "Cotisations : " affichage_salaireBrut

           MOVE fbp_autresdeductions TO temp_fbp_autresDeductions
           MOVE temp_fbp_autresDeductions TO affichage_salaireBrut
           DISPLAY "Autres deductions : " affichage_salaireBrut

           MOVE fbp_salaireNet TO temp_fbp_salaireNet
           MOVE temp_fbp_salaireNet TO affichage_salaireBrut
           DISPLAY "Salaire net : " affichage_salaireBrut
           DISPLAY SEPARATOR
       END-READ

       CLOSE fbulletinsPaie
       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       PERFORM RETOUR-MENU.



       PRODUIRE-RBP.
       DISPLAY "Production d'un rapport de paie"
       DISPLAY SEPARATOR

       OPEN I-O frapportsPaie
       OPEN INPUT fbulletinsPaie

       MOVE 0 TO Wtrouve
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
           DISPLAY "Pour quel mois vous souhaitez produire le rapport?",
           "format MM, exemple: 03"
           ACCEPT temp_fr_mois
           IF temp_fr_mois > 12 OR temp_fr_mois < 0 THEN
               DISPLAY "Mois invalide, reessayer"
           ELSE MOVE 1 TO Wtrouve
           END-IF
       END-PERFORM

       MOVE 0 TO Wtrouve
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
           DISPLAY "Pour quelle annee vous souhaitez",
           " produire le rapport? format AAAA, exemple: 2023"
               ACCEPT temp_fr_annee
               IF temp_fr_annee > FUNCTION CURRENT-DATE(7:4)
                   OR temp_fr_annee < 0 THEN
                   DISPLAY "Annee < 0 ou dans le futur, reessayer"
               ELSE MOVE 1 TO Wtrouve
               END-IF
       END-PERFORM

       MOVE temp_fr_mois TO fr_mois
       MOVE temp_fr_annee TO fr_annee

       READ frapportsPaie
       INVALID KEY DISPLAY "Verification existance Rapport : OK"
       NOT INVALID KEY
           DISPLAY "Un rapport de paie pour ce mois existe deja",
           " svp, choisissez l'option -consulter rapport paie pour"
           " le consulter"
           CLOSE frapportsPaie
           CLOSE fbulletinsPaie
           PERFORM RETOUR-MENU
           STOP RUN
       END-READ

       MOVE fr_mois TO fbp_mois
       MOVE fr_annee TO fbp_annee


       MOVE 0 TO Wfin
       START fbulletinsPaie, KEY IS = fbp_annee
       INVALID KEY DISPLAY "Pas de bulletins cette annee"
       NOT INVALID KEY
           START fbulletinsPaie, KEY IS = fbp_mois
               INVALID KEY DISPLAY "Pas de bulletins ce mois"
               NOT INVALID KEY
                   PERFORM WITH TEST AFTER UNTIL Wfin = 1
                       READ fbulletinsPaie NEXT
                       AT END MOVE 1 TO Wfin
                       NOT AT END
                           COMPUTE temp_fr_totalSalaires =
                               temp_fr_totalSalaires +fbp_salaireBrut
                           COMPUTE temp_fr_totalImpots =
                               temp_fr_totalImpots + fbp_cotisations

                           COMPUTE temp_fr_totalAutresDeductions =
                               temp_fr_totalAutresDeductions +
                               fbp_autresDeductions
                       END-READ
                    END-PERFORM
               END-START
       END-START

       MOVE temp_fr_totalSalaires TO fr_totalSalaires
       MOVE temp_fr_totalImpots TO fr_totalImpots
       MOVE temp_fr_totalAutresDeductions TO fr_totalAutresDeductions

       WRITE tamp_frapportsPaie
       END-WRITE
       CLOSE frapportsPaie
       CLOSE fbulletinsPaie

       MOVE temp_fr_totalSalaires TO affichage_salaireBrut
       DISPLAY "Masse salariale du mois : "affichage_salaireBrut

       MOVE temp_fr_totalImpots TO affichage_salaireBrut
       DISPLAY "Total impots du mois : "affichage_salaireBrut

       MOVE temp_fr_totalAutresDeductions TO affichage_salaireBrut
       DISPLAY "Total autres deductions : "affichage_salaireBrut

       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       ACCEPT CHOICE
       PERFORM RETOUR-MENU.


       CONSULTER-RBP.
       DISPLAY "Consulter un rapport de paie"
       DISPLAY "Voici la liste des rapports de paie existants par mois:"
       DISPLAY SEPARATOR
       OPEN INPUT frapportsPaie
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ frapportsPaie NEXT
           AT END MOVE 1 TO Wfin
           NOT AT END DISPLAY "--" fr_mois, "/" fr_annee
       END-PERFORM

       DISPLAY SEPARATOR
       DISPLAY "Quel rapport vous voulez consulter? saisir L'ANNEE",
       " uniquement"

       ACCEPT temp_fr_annee

       DISPLAY "Quel rapport vous voulez consulter? saisir LE MOIS",
       " uniquement"

       ACCEPT temp_fr_mois

       MOVE temp_fr_mois TO fr_mois
       MOVE temp_fr_annee TO fr_annee

       READ frapportsPaie
           INVALID KEY DISPLAY "Rapport inexistant ou saisie maladroite"
           NOT INVALID KEY
              DISPLAY "La date du rapport est :",
              " ", fr_mois, "/", fr_annee

              DISPLAY "La masse salariale de l'entreprise est :"

              MOVE fr_totalSalaires TO affichage_salaireBrut
              DISPLAY affichage_salaireBrut

              DISPLAY "Le total des deductions sur salaire: "
              MOVE fr_totalAutresDeductions TO affichage_salaireBrut
              DISPLAY affichage_salaireBrut


              DISPLAY "Le total des cotisations sociales pour ce mois: "
              MOVE fr_totalImpots TO affichage_salaireBrut
              DISPLAY affichage_salaireBrut
       END-READ
       CLOSE frapportsPaie
       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       ACCEPT CHOICE
           PERFORM RETOUR-MENU.


       EDITER-METIER.
       DISPLAY "Edition des informations sur les metiers des employes"
       DISPLAY SEPARATOR
       OPEN I-O fmetiers
       PERFORM UNTIL CHOICE = 0
        DISPLAY "Nom du metier a modifier? : "
        ACCEPT temp_fm_nomMetier
        MOVE temp_fm_nomMetier TO fm_nomMetier
        MOVE 0 TO Wtrouve
        PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 OR Wfin = 1
            READ fmetiers KEY IS fm_nomMetier
                INVALID KEY
                    MOVE 1 TO Wfin
                    DISPLAY "Metier non trouve"
                NOT INVALID KEY
                    DISPLAY "Metier trouve"
                    DISPLAY "Nom metier : " fm_nomMetier
                    DISPLAY "Domaine : " fm_domaine

                    DISPLAY "Nouveau domaine du metier? : "
                    ACCEPT temp_fm_domaine
                    MOVE temp_fm_domaine TO fm_domaine

                    PERFORM WITH TEST AFTER UNTIL fm_salaireBase > 0
                        DISPLAY "Nouveau salaire de base? XX.XX:  "
                        ACCEPT saisie_salaireBase
                        INSPECT saisie_salaireBase REPLACING ALL ','
                        BY '.'
                        MOVE FUNCTION NUMVAL(saisie_salaireBase) TO
                            vrai_salaireBase
                        MOVE vrai_salaireBase TO fm_salaireBase
                        MOVE vrai_salaireBase TO affichage_salaireBase
                        DISPLAY "saisie : " affichage_salaireBase
                    END-PERFORM

                    PERFORM WITH TEST AFTER UNTIL
                       fm_tauxsupplementaire > 0
                        DISPLAY "Nouveau taux supplementaire? XX.XX : "
                        ACCEPT saisie_tauxsupplementaire
                        INSPECT saisie_tauxsupplementaire
                        REPLACING ALL ',' BY '.'
                        MOVE FUNCTION
                            NUMVAL(saisie_tauxsupplementaire) TO
                            vrai_tauxSupplementaire
                        MOVE vrai_tauxSupplementaire
                            TO fm_tauxsupplementaire
                        MOVE vrai_tauxSupplementaire
                            TO affichage_taux_supplementaire
                        DISPLAY "saisie : "
                        affichage_taux_supplementaire
                    END-PERFORM

                    MOVE 1 TO Wtrouve
                    REWRITE tamp_fmetiers
                    DISPLAY "Metier modifie avec succes"

                    OPEN I-O femployes
                    PERFORM WITH TEST AFTER UNTIL Wfin = 1
                        READ femployes NEXT
                        AT END
                            MOVE 1 TO Wfin
                        NOT AT END
                            IF fe_metier = temp_fm_nomMetier
                                MOVE temp_fm_nomMetier TO fe_metier
                                REWRITE tamp_femployes
                                DISPLAY "Employe " fe_nom " mis a jour"
                            END-IF
                        END-READ
                    END-PERFORM
                    CLOSE femployes
            END-READ
        END-PERFORM
        DISPLAY "Voulez-vous modifier "-
        "un autre metier (Oui : 1/Non : 0)? : "
        ACCEPT CHOICE
       END-PERFORM
       CLOSE fmetiers
       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       PERFORM RETOUR-MENU.


       MAJ-TAUX-CS.
       MOVE 0 TO Wtrouve
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
       DISPLAY "Nouveau Taux de cotisations sociales ?: XX.XX"
       ACCEPT temp_taux_cs
       IF temp_taux_cs < 0 OR temp_taux_cs > 1 THEN
           DISPLAY "taux invalide X>0 et X<1"
       ELSE
           MOVE temp_taux_cs TO TAUX-CS
           DISPLAY "taux modifie avec succes"
           MOVE 1 TO Wtrouve
       END-PERFORM

       DISPLAY "--fin--"
       ACCEPT CHOICE
           PERFORM RETOUR-MENU.

       CONSULTER-METIERS.
       DISPLAY "Consulter les metiers : "
       DISPLAY SEPARATOR
       OPEN INPUT fmetiers
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fmetiers NEXT
           AT END MOVE 1 TO Wfin
           NOT AT END
               DISPLAY "Nom metier : " fm_nomMetier
               DISPLAY "Domaine : " fm_domaine

               MOVE FUNCTION NUMVAL(fm_salaireBase)
                   TO vrai_salaireBase
               DIVIDE vrai_salaireBase BY 100 GIVING vrai_salaireBase
               MOVE vrai_salaireBase TO affichage_salaireBase
           DISPLAY "Salaire Base : " affichage_salaireBase

           MOVE FUNCTION NUMVAL(fm_tauxsupplementaire)
                   TO vrai_tauxSupplementaire
               DIVIDE vrai_tauxSupplementaire BY
                   100 GIVING vrai_tauxSupplementaire
               MOVE vrai_tauxSupplementaire
                   TO affichage_taux_supplementaire

           DISPLAY "Taux Supplementaire : "affichage_taux_supplementaire
           DISPLAY SEPARATOR
           END-READ
       END-PERFORM
           CLOSE fmetiers.

       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       ACCEPT CHOICE
           PERFORM RETOUR-MENU.


       RETIRER-METIER.
       DISPLAY "Retirer un metier"
       DISPLAY SEPARATOR
       DISPLAY "ATTENTION, retirer un metier supprimera aussi",
       " les employes rattaches à ce metier "
       OPEN I-O fmetiers
       PERFORM UNTIL CHOICE = 0
        DISPLAY "Nom du metier a retirer? : "
        ACCEPT temp_fm_nomMetier
        MOVE temp_fm_nomMetier TO fm_nomMetier
        MOVE 0 TO Wtrouve
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 OR Wfin = 1
            READ fmetiers
                INVALID KEY
                    MOVE 1 TO Wfin
                    DISPLAY "Metier non trouve"
                NOT INVALID KEY
                   OPEN I-O femployes
                   MOVE 0 TO Wfin
                      PERFORM WITH TEST AFTER UNTIL Wfin = 1
                       READ femployes NEXT
                       AT END MOVE 1 TO Wfin
                       NOT AT END
                           IF fe_metier = fm_nomMetier THEN
                               DISPLAY "Employe " ,fe_nom, " retire"
                               DELETE femployes RECORD
                           END-IF
                       END-READ
                      END-PERFORM
                      CLOSE femployes
                            DELETE fmetiers
                            DISPLAY "Metier retire avec succes"
                            MOVE 1 TO Wtrouve
            END-READ
        END-PERFORM
        DISPLAY "Voulez-vous retirer un "-
           "autre metier (Oui : 1/Non : 0)? : "
        ACCEPT CHOICE
       END-PERFORM
       CLOSE fmetiers
       DISPLAY "--fin, toucher envoi pour retourner au menu--"
       ACCEPT CHOICE
           PERFORM RETOUR-MENU.



       VerifierDate.
       MOVE temp_dateEmbauche(1:2) TO jourEmbauche
       MOVE temp_dateEmbauche(4:2) TO moisEmbauche
       MOVE temp_dateEmbauche(7:4) TO anneeEmbauche
       IF jourEmbauche IS NUMERIC AND moisEmbauche IS NUMERIC AND
           anneeEmbauche IS NUMERIC
        IF moisEmbauche >= 1 AND moisEmbauche <= 12
            IF jourEmbauche >= 1 AND jourEmbauche <= 31
               EVALUATE TRUE
                    WHEN moisEmbauche = 4 OR
                    moisEmbauche = 6 OR moisEmbauche = 9 OR
                    moisEmbauche = 11
                        IF jourEmbauche <= 30
                            MOVE 'Y' TO estDateValide
                        END-IF
                    WHEN moisEmbauche = 2
                   COMPUTE Reste = FUNCTION MOD(anneeEmbauche, 4)
                   IF Reste = 0 AND
                       (FUNCTION MOD(anneeEmbauche, 100) <> 0 OR
                       FUNCTION MOD(anneeEmbauche, 400) = 0)
                            IF jourEmbauche <= 29
                                MOVE 'Y' TO estDateValide
                            END-IF
                        ELSE
                            IF jourEmbauche <= 28
                                MOVE 'Y' TO estDateValide
                            END-IF
                        END-IF
                    WHEN OTHER
                        MOVE 'Y' TO estDateValide
                END-EVALUATE
            END-IF
        END-IF
       END-IF.



       RETOUR-MENU.
           IF NIV-ACCES = "A" THEN
               PERFORM MENU-GESTIONNAIRE
           ELSE IF NIV-ACCES = "B"
               PERFORM MENU-DIRECTEUR
           ELSE
               PERFORM MENU-EMPLOYE
           END-IF.
