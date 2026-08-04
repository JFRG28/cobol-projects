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
            SELECT REPORTE ASSIGN TO PRINTER.
       $ELSE
            SELECT LISTA ASSIGN TO 
            "/home/paco/cobol-projects/nombres.txt"
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT REPORTE ASSIGN TO 
            "/home/paco/cobol-projects/reporte.txt"
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
       FD REPORTE REPORT IS LISTADO.
       WORKING-STORAGE SECTION.
           01 WKS-EOF-LISTA    PIC 9(02) VALUE IS 0.
           01 WS-CUANTOS       PIC 9(03).
           01 WS-HORAS         PIC 9(10).
           01 WS-CURRENT-DATE.
              02 WS-CURRENT-YY          PIC 99.
              02 WS-CURRENT-MM          PIC 99.
              02 WS-CURRENT-DD          PIC 99.
           01 WS-TODAYS-DATE.
              02 WS-TODAYS-YY   PIC 99.
              02 WS-TODAYS-MM   PIC 99.
              02 WS-TODAYS-DD   PIC 99.
       REPORT SECTION.
        RD LISTADO
           PAGE LIMIT      058
           HEADING         001
           FIRST DETAIL    009
           LAST DETAIL     054
           CONTROL IS      REG-NUMGPO.
       01 RD-IMPRE-TITULOS TYPE IS PH.
          10 RD-IMPRE-TITULO-01 LINE 001.
             15 COLUMN 001 PIC X(13) VALUE "S999/P600-001". 
             15 COLUMN 047 PIC X(42) VALUE 
             "GRUPO CAPACITACION STEFANINI - COBOL74".
          10 RD-IMPRE-TITULO-02 LINE 003.
             15 COLUMN 001 PIC X(14) VALUE "FECHA PROCESO:". 
             15 COLUMN 016 PIC 99/99/99 SOURCE WS-TODAYS-DATE.
             15 COLUMN 060 PIC X(25) VALUE 
             "SISTEMA DE CAPACITACIONES".
             15 COLUMN 120 PIC X(7) VALUE "HOJA:".
             15 COLUMN 127 PIC Z,ZZZ SOURCE PAGE-COUNTER.
          10 RD-IMPRE-TITULO-03 LINE 004.
             15 COLUMN 001 PIC X(6) VALUE "HORA:".
             15 COLUMN 016 PIC 9(10) SOURCE WS-HORAS.
             15 COLUMN 047 PIC X(40) VALUE 
             "L I S T A   D E   A S I S T E N C I A".
          10 RD-IMPRE-TITULO-04 LINE 006.
             15 COLUMN 002 PIC X(3) VALUE "GPO".
             15 COLUMN 006 PIC X(4) VALUE "NUM.".
             15 COLUMN 015 PIC X(6) VALUE "NOMBRE".
             15 COLUMN 040 PIC X(9) VALUE "APELLIDOS".
       01 RD-IMPRE-DETALLE TYPE IS DETAIL.
          10 RD-IMPRE-DET01 LINE PLUS 001.
             15 COLUMN 002 GROUP INDICATE PIC Z(2) SOURCE REG-NUMGPO.
             15 COLUMN 006 PIC Z(4) SOURCE REG-NUMLISTA.
             15 COLUMN 015 PIC X(20) SOURCE REG-NOMBRE.
             15 COLUMN 040 PIC X(25) SOURCE REG-APELLIDOS.
       01 RD-IMPRE-CORTE TYPE CF REG-NUMGPO.
          10 RD-IMPRE-PIE-01 LINE PLUS 002.
             15 COLUMN 006 PIC X(27) VALUE
             "---------------------------".
       01 TYPE PAGE FOOTING.
          10 LINE 056.
             15 COLUMN 10 PIC X(42) VALUE
                "REPORTE DE ASISTENCIA CAPACITACION COBOL74".
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO
           PERFORM 3000-TERMINA
           STOP RUN.
       1000-INICIO.
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-CURRENT-MM TO WS-TODAYS-MM
           MOVE WS-CURRENT-DD TO WS-TODAYS-DD
           MOVE WS-CURRENT-YY TO WS-TODAYS-YY
           $IF UNISYS DEFINED
                      CHANGE ATTRIBUTE TITLE OF LISTA TO
                      "JFRG/FILE/INTE/99902E02/01/171127/LST/TXT.".
                      IF ATTRIBUTE RESIDENT OF LISTA = VALUE TRUE
                          OPEN INPUT LISTA
                          PERFORM 9999-LEE-LISTA
                      ELSE
                          DISPLAY "NO EXISTE EL ARCHIVO LISTA: "
                          MOVE 1 TO WKS-EOF-LISTA.
                      CHANGE ATTRIBUTE TITLE OF REPORTE TO
                      "JFRG/LIST/P126/171127.".
           $ELSE
                      OPEN INPUT LISTA
                      PERFORM 9999-LEE-LISTA
           $END-IF           
                      ACCEPT WS-HORAS FROM TIME
                      OPEN OUTPUT REPORTE
                      INITIATE LISTADO.
       2000-PROCESO.
           PERFORM 2500-GENERA-REPORTE UNTIL WKS-EOF-LISTA = 1
           TERMINATE LISTADO.
       2500-GENERA-REPORTE.
           IF WKS-EOF-LISTA = 0
               GENERATE RD-IMPRE-DETALLE
               PERFORM 9999-LEE-LISTA
           END-IF.
       3000-TERMINA.
           CLOSE LISTA.
           CLOSE REPORTE.
       9999-LEE-LISTA.
           READ LISTA AT END 
           MOVE 1 TO WKS-EOF-LISTA.
      