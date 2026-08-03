       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG07.
       AUTHOR. JOSE FRANCISCO RODRIGUEZ. GARCIA.
       DATE-WRITTEN. 03-08-2026.
      *REFRESCANDO CONOCIMIENTOS DE COBOL74 UNISYS
       SECURITY. FOREVER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       $IF UNISYS DEFINED
            SELECT LISTA ASSIGN TO DISK.
       $ELSE
            SELECT LISTA ASSIGN TO "/home/paco/cobol-projects/nombres.txt"
                         ORGANIZATION IS LINE SEQUENTIAL.
       $END-IF
       DATA DIVISION.
       FILE SECTION.
       FD  LISTA.
       $IF UNISYS DEFINED
                  VALUE OF DEPENDENTSPECS IS TRUE.
       $END-IF
       01 REG-LISTA.
           02 REG-NUMLISTA     PIC 9(04).
           02 REG-NOMBRE       PIC X(18).
           02 REG-APELLIDOS    PIC X(20).
           02 REG-NUMGPO       PIC X(02).
       WORKING-STORAGE SECTION.
           01 WKS-EOF-LISTA        PIC 9(02) VALUE IS 0.
           01 WKS-TOTAL-REGISTROS  PIC 9(03).
           01 WL-INDICE            PIC 9(03).
           01 WS-LISTA-PARTICIPANTES.
               03 WS-LISTA OCCURS 50 TIMES
                   ASCENDING KEY IS WS-NUMLISTA
                   INDEXED BY WK-INDICE.
                   05 WS-NUMLISTA     PIC 9(04).
                   05 WS-NOMBRE       PIC X(18).
                   05 WS-APELLIDOS    PIC X(20).
                   05 WS-NUMGPO       PIC X(02).
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO
           PERFORM 3000-TERMINA
           STOP RUN.
       1000-INICIO.
           $IF UNISYS DEFINED
                      CHANGE ATTRIBUTE TITLE OF LISTA TO
                      "/home/paco/cobol-projects/nombres/txt.".
                      IF ATTRIBUTE RESIDENT OF LISTA = VALUE TRUE
                          OPEN INPUT LISTA
                          PERFORM 9999-LEE-LISTA
                          SET WK-INDICE TO 0
                      ELSE
                          DISPLAY "NO EXISTE EL ARCHIVO LISTA: "
                          MOVE 1 TO WKS-EOF-LISTA.
           $ELSE
                      OPEN INPUT LISTA
                      PERFORM 9999-LEE-LISTA
                      SET WK-INDICE TO 0
           $END-IF
       2000-PROCESO.
           PERFORM 2500-LLENA-TABLA UNTIL WKS-EOF-LISTA = 1
           MOVE WS-NUMLISTA (WK-INDICE) TO WKS-TOTAL-REGISTROS
           DISPLAY "TOTAL DE REGISTROS: " WKS-TOTAL-REGISTROS
           PERFORM 2500-DESPLIEGA-LISTA
                   VARYING WL-INDICE FROM 1 BY 1 
                   UNTIL WL-INDICE > WKS-TOTAL-REGISTROS
       2500-LLENA-TABLA.
           SET WK-INDICE UP BY 1
           MOVE REG-NUMLISTA TO WS-NUMLISTA (WK-INDICE)
           MOVE REG-NOMBRE   TO WS-NOMBRE   (WK-INDICE)
           MOVE REG-APELLIDOS TO WS-APELLIDOS (WK-INDICE)
           MOVE REG-NUMGPO   TO WS-NUMGPO   (WK-INDICE)
           PERFORM 9999-LEE-LISTA.
       2500-DESPLIEGA-LISTA.
           SET WK-INDICE TO WL-INDICE
           DISPLAY "REGISTRO: "    WS-NUMLISTA (WK-INDICE)
                      ".- : "      WS-APELLIDOS (WK-INDICE)
                      "     "      WS-NOMBRE (WK-INDICE).
       3000-TERMINA.
           CLOSE LISTA.
       9999-LEE-LISTA.
           READ LISTA AT END MOVE 1 TO WKS-EOF-LISTA.
      