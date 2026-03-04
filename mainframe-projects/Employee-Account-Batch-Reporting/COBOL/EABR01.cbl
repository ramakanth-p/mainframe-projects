       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. EABR01.
      *
       ENVIRONMENT DIVISION.
      *---------------------
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO EABRF01
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
      *
       DATA DIVISION.
      *--------------
       FILE SECTION.
      *
       FD  OUTFILE RECORDING MODE IS F.
       01  OUT-REC.
           05  OF-FIRST-NAME       PIC X(15).
           05  OF-SUR-NAME         PIC X(20).
           05  OF-ADDRESS1         PIC X(25).
           05  OF-ADDRESS2         PIC X(20).
           05  OF-ADDRESS3         PIC X(15).
           05  OF-ACCTNO           PIC X(08).
           05  OF-CR-LIMIT         PIC S9(07)V99.
           05  OF-BALANCE          PIC S9(07)V99.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-FILE-STATUS          PIC XX.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-COUNT                PIC 9(9) VALUE 0.
       01  WS-COUNT-MOD            PIC 9(9) VALUE 0.
       01  WS-SQLCODE-DISPLAY      PIC -9(9).
       01  WS-LIMIT                PIC S9(7)V99 COMP-3.
       01  WS-BALANCE              PIC S9(7)V99 COMP-3.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL
               DECLARE C1 CURSOR FOR
               SELECT ACCTNO, LIMIT, BALANCE,
                      SURNAME, FIRSTN,
                      ADDRESS1, ADDRESS2, ADDRESS3
               FROM Z88436.Z88436T
               WHERE BALANCE > 0
               ORDER BY ACCTNO
           END-EXEC.
      *
       01  EMPLOYEE-REC.
           05  HV-ACCTNO           PIC X(08).
           05  HV-CR-LIMIT         PIC S9(7)V99 COMP-3.
           05  HV-BALANCE          PIC S9(7)V99 COMP-3.
           05  HV-SUR-NAME         PIC X(20).
           05  HV-FIRST-NAME       PIC X(15).
           05  HV-ADDRESS1         PIC X(25).
           05  HV-ADDRESS2         PIC X(20).
           05  HV-ADDRESS3         PIC X(15).
      *
       PROCEDURE DIVISION.
      *-------------------
       MAIN-SECTION.
           PERFORM OPEN-ALL
      *
           PERFORM FETCH-CURSOR
                 UNTIL WS-EOF = 'Y'
      *
           PERFORM CLOSE-ALL
      *
           PERFORM STOP-PROCESS.
      *
       OPEN-ALL.
           OPEN OUTPUT OUTFILE.
           IF  WS-FILE-STATUS NOT = '00'
               DISPLAY 'INPUT FILE OPEN ERROR : ' WS-FILE-STATUS
               MOVE 12             TO RETURN-CODE
               PERFORM STOP-PROCESS
           END-IF
      *
           EXEC SQL OPEN C1 END-EXEC.
           IF  SQLCODE NOT = 0
               MOVE SQLCODE        TO WS-SQLCODE-DISPLAY
               DISPLAY 'OPEN ERROR: ' WS-SQLCODE-DISPLAY
               MOVE 12             TO RETURN-CODE
               PERFORM STOP-PROCESS
           END-IF.
      *
       FETCH-CURSOR.
           EXEC SQL
               FETCH C1 INTO
               :HV-ACCTNO,
               :HV-CR-LIMIT,
               :HV-BALANCE,
               :HV-SUR-NAME,
               :HV-FIRST-NAME,
               :HV-ADDRESS1,
               :HV-ADDRESS2,
               :HV-ADDRESS3
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN 0
                   PERFORM WRITE-OUTPUT
               WHEN 100
                   MOVE 'Y'        TO WS-EOF
               WHEN -805
                   MOVE SQLCODE    TO WS-SQLCODE-DISPLAY
                   DISPLAY 'PACKAGE NOT FOUND - BIND REQUIRED'
                   DISPLAY 'SQLCODE: ' WS-SQLCODE-DISPLAY
                   MOVE 12         TO RETURN-CODE
                   PERFORM CLOSE-ALL
                   PERFORM STOP-PROCESS
               WHEN -911
                   MOVE SQLCODE    TO WS-SQLCODE-DISPLAY
                   DISPLAY 'DEADLOCK OR TIMEOUT OCCURRED'
                   DISPLAY 'SQLCODE: ' WS-SQLCODE-DISPLAY
                   MOVE 12         TO RETURN-CODE
                   PERFORM CLOSE-ALL
                   PERFORM STOP-PROCESS
               WHEN OTHER
                   MOVE SQLCODE    TO WS-SQLCODE-DISPLAY
                   DISPLAY 'DB2 ERROR:  ' WS-SQLCODE-DISPLAY
                   DISPLAY 'SQLSTATE: ' SQLSTATE
                   DISPLAY 'PROGRAM TERMINATED ABNORMALLY'
                   MOVE 12         TO RETURN-CODE
                   PERFORM CLOSE-ALL
                   PERFORM STOP-PROCESS
           END-EVALUATE.
      *
       WRITE-OUTPUT.
           MOVE HV-ACCTNO          TO OF-ACCTNO
           MOVE HV-CR-LIMIT        TO OF-CR-LIMIT
           MOVE HV-BALANCE         TO OF-BALANCE
           MOVE HV-SUR-NAME        TO OF-SUR-NAME
           MOVE HV-FIRST-NAME      TO OF-FIRST-NAME
           MOVE HV-ADDRESS1        TO OF-ADDRESS1
           MOVE HV-ADDRESS2        TO OF-ADDRESS2
           MOVE HV-ADDRESS3        TO OF-ADDRESS3
      *
           WRITE OUT-REC
           IF  WS-FILE-STATUS NOT = '00'
               DISPLAY 'WRITE ERROR: ' WS-FILE-STATUS
               MOVE 12 TO RETURN-CODE
               PERFORM CLOSE-ALL
               PERFORM STOP-PROCESS
           END-IF
      *
           ADD 1                   TO WS-COUNT
           ADD 1                   TO WS-COUNT-MOD
           IF  WS-COUNT-MOD = 1000
               DISPLAY 'PROCESSED RECORDS: ' WS-COUNT
               MOVE 0              TO WS-COUNT-MOD
           END-IF.
      *
       CLOSE-ALL.
           CLOSE OUTFILE
           IF  WS-FILE-STATUS NOT = '00'
               DISPLAY 'FILE CLOSE ERROR: ' WS-FILE-STATUS
           END-IF
      *
           EXEC SQL CLOSE C1 END-EXEC
           IF  SQLCODE NOT = 0
               MOVE SQLCODE        TO WS-SQLCODE-DISPLAY
               DISPLAY 'CLOSE ERROR: ' WS-SQLCODE-DISPLAY
           END-IF
      *
           DISPLAY 'TOTAL RECORDS WRITTEN:  ' WS-COUNT.
      *
       STOP-PROCESS.
           STOP RUN.
