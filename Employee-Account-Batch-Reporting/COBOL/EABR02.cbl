       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. EABR02.
      *
       ENVIRONMENT DIVISION.
      *---------------------
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO EABRF01
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IN-FILE-STATUS.
           SELECT RPTFILE ASSIGN TO EABRF02
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-FILE-STATUS.
      *
       DATA DIVISION.
      *--------------
       FILE SECTION.
      *
       FD  INFILE RECORDING MODE IS F.
       01  IN-REC.
           05  IN-FIRST-NAME       PIC X(15).
           05  IN-SUR-NAME         PIC X(20).
           05  IN-ADDRESS1         PIC X(25).
           05  IN-ADDRESS2         PIC X(20).
           05  IN-ADDRESS3         PIC X(15).
           05  IN-ACCTNO           PIC X(08).
           05  IN-CR-LIMIT         PIC S9(07)V99.
           05  IN-BALANCE          PIC S9(07)V99.
      *
       FD  RPTFILE RECORDING MODE IS V.
       01  RPT-REC                 PIC X(132).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-IN-FILE-STATUS       PIC XX.
       01  WS-OUT-FILE-STATUS      PIC XX.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-PAGE-NUM             PIC 9(03) VALUE 0.
       01  WS-LINE-COUNT           PIC 9(03).
       01  WS-MAX-LINES            PIC 9(02) VALUE 55.
       01  WS-LIMIT-DISP           PIC Z,ZZZ,ZZ9.99.
       01  WS-BALANCE-DISP         PIC Z,ZZZ,ZZ9.99.
       01  WS-TOTAL-BAL-DISP       PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-TOTAL-BAL-NUM        PIC S9(07)V99 COMP-3.
       01  WS-TOTAL-COUNT          PIC 9(9) VALUE 0.
       01  WS-TOTAL-BALANCE        PIC S9(13)V99 COMP-3 VALUE 0.
      *
       01  WS-DATE.
           05  WS-YYYY             PIC 9(04).
           05  WS-MM               PIC 9(02).
           05  WS-DD               PIC 9(02).
      *
       01  WS-DATE-DISP.
           05  WS-DATE-DISP-YYYY   PIC 9(04).
           05  WS-DATE-DISP-SEP1   PIC X(01) VALUE '-'.
           05  WS-DATE-DISP-MM     PIC 9(02).
           05  WS-DATE-DISP-SEP2   PIC X(01) VALUE '-'.
           05  WS-DATE-DISP-DD     PIC 9(02).
      *
       01  WS-TIME.
           05  WS-HR               PIC 9(02).
           05  WS-MIN              PIC 9(02).
           05  WS-SEC              PIC 9(02).
      *
       01  WS-TIME-DISP.
           05  WS-TIME-DISP-HR     PIC 9(02).
           05  WS-TIME-DISP-SEP1   PIC X(01) VALUE '-'.
           05  WS-TIME-DISP-MIN    PIC 9(02).
           05  WS-TIME-DISP-SEP2   PIC X(01) VALUE '-'.
           05  WS-TIME-DISP-SEC    PIC 9(02).
      *
       01  HDG-LINE-1.
           05  FILLER              PIC X(50) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE 'ACCOUNT REPORT'.
           05  FILLER              PIC X(40) VALUE SPACES.
           05  FILLER              PIC X(06) VALUE 'PAGE: '.
           05  HDR-PAGE            PIC ZZ9.
           05  FILLER              PIC X(13) VALUE SPACES.
      *
       01  HDG-LINE-2.
           05  FILLER              PIC X(06) VALUE 'DATE: '.
           05  HDR-DATE            PIC X(10).
           05  FILLER              PIC X(94) VALUE SPACES.
           05  FILLER              PIC X(06) VALUE 'TIME: '.
           05  HDR-TIME            PIC X(08).
           05  FILLER              PIC X(08) VALUE SPACES.
      *
       01  HDG-LINE-3.
           05  FILLER              PIC X(08) VALUE 'ACCTNO'.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(15) VALUE ' FIRST NAME'.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE ' SUR NAME'.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(13) VALUE 'LIMIT'.
           05  FILLER              PIC X(13) VALUE 'BALANCE'.
           05  FILLER              PIC X(25) VALUE 'ADDRESS1'.
           05  FILLER              PIC X(20) VALUE 'ADDRESS2'.
           05  FILLER              PIC X(15) VALUE 'ADDRESS3'.
      *
       01  DTL-LINE.
           05  DTL-ACCTNO          PIC X(08).
           05  DTL-SEP1            PIC X(01) VALUE SPACES.
           05  DTL-FIRST-NAME      PIC X(15).
           05  DTL-SUR-NAME        PIC X(20).
           05  DTL-LIMIT           PIC X(13).
           05  DTL-BALANCE         PIC X(13).
           05  DTL-ADDRESS1        PIC X(25).
           05  DTL-ADDRESS2        PIC X(20).
           05  DTL-ADDRESS3        PIC X(15).
      *
       01  TRLR-LINE.
           05 FILLER               PIC X(15) VALUE 'TOTAL RECORDS:'.
           05 TOT-COUNT            PIC Z,ZZZ,ZZ9.
           05 FILLER               PIC X(10) VALUE SPACES.
           05 FILLER               PIC X(14) VALUE 'TOTAL BALANCE:'.
           05 TOT-BALANCE          PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(60) VALUE SPACES.
      *
       PROCEDURE DIVISION.
      *-------------------
       MAIN-SECTION.
           PERFORM INIT-HDR
           PERFORM OPEN-ALL
           PERFORM PRINT-HEADER
      *
           PERFORM UNTIL WS-EOF = 'Y'
               PERFORM READ-FILE
           END-PERFORM
      *
           PERFORM PRINT-TOTALS
           PERFORM CLOSE-ALL
      *
           PERFORM STOP-PROCESS.
      *
       INIT-HDR.
           INITIALIZE RPT-REC
      *
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TIME FROM TIME
      *
           MOVE WS-YYYY            TO WS-DATE-DISP-YYYY
           MOVE WS-MM              TO WS-DATE-DISP-MM
           MOVE WS-DD              TO WS-DATE-DISP-DD
           MOVE WS-DATE-DISP       TO HDR-DATE
      *
           MOVE WS-HR              TO WS-TIME-DISP-HR
           MOVE WS-MIN             TO WS-TIME-DISP-MIN
           MOVE WS-SEC             TO WS-TIME-DISP-SEC
           MOVE WS-TIME-DISP       TO HDR-TIME.
      *
       OPEN-ALL.
           OPEN INPUT INFILE
           IF  WS-IN-FILE-STATUS NOT = '00'
               DISPLAY 'INPUT FILE OPEN ERROR : ' WS-IN-FILE-STATUS
               MOVE 12 TO RETURN-CODE
               PERFORM STOP-PROCESS
           END-IF
      *
           OPEN OUTPUT RPTFILE
           IF  WS-OUT-FILE-STATUS NOT = '00'
               DISPLAY 'OUTPUT FILE OPEN ERROR : ' WS-OUT-FILE-STATUS
               MOVE 12 TO RETURN-CODE
               PERFORM STOP-PROCESS
           END-IF.
      *
       PRINT-HEADER.
           MOVE ALL '-'            TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING PAGE
           IF  WS-OUT-FILE-STATUS NOT = '00'
               DISPLAY 'WRITE ERROR: ' WS-OUT-FILE-STATUS
               MOVE 12 TO RETURN-CODE
               PERFORM CLOSE-ALL
               PERFORM STOP-PROCESS
           END-IF
           ADD 1                   TO WS-LINE-COUNT
      *
           ADD 1 TO WS-PAGE-NUM
           MOVE WS-PAGE-NUM        TO HDR-PAGE
           MOVE HDG-LINE-1         TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE
           ADD 1                   TO WS-LINE-COUNT
      *
           MOVE HDG-LINE-2         TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE
           ADD 1                   TO WS-LINE-COUNT
      *
           MOVE ALL '-'            TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE
           ADD 1                   TO WS-LINE-COUNT
      *
           MOVE HDG-LINE-3         TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE
           ADD 1                   TO WS-LINE-COUNT.
      *
           MOVE ALL '-'            TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE
           ADD 1                   TO WS-LINE-COUNT.
      *
       READ-FILE.
           READ INFILE
           END-READ
      *
           EVALUATE WS-IN-FILE-STATUS
               WHEN '00'
                   PERFORM PROCESS-DATA
               WHEN '10'
                   MOVE 'Y'        TO WS-EOF
               WHEN OTHER
                   DISPLAY 'READ ERROR: ' WS-IN-FILE-STATUS
                   MOVE 12 TO RETURN-CODE
                   PERFORM CLOSE-ALL
                   PERFORM STOP-PROCESS
           END-EVALUATE.
      *
       PROCESS-DATA.
           MOVE  IN-FIRST-NAME     TO DTL-FIRST-NAME
           MOVE  IN-SUR-NAME       TO DTL-SUR-NAME
           MOVE  IN-ACCTNO         TO DTL-ACCTNO
           MOVE  IN-ADDRESS1       TO DTL-ADDRESS1
           MOVE  IN-ADDRESS2       TO DTL-ADDRESS2
           MOVE  IN-ADDRESS3       TO DTL-ADDRESS3
           MOVE  IN-CR-LIMIT       TO WS-LIMIT-DISP
           MOVE  WS-LIMIT-DISP     TO DTL-LIMIT
           MOVE  IN-BALANCE        TO WS-BALANCE-DISP
           MOVE  WS-BALANCE-DISP   TO DTL-BALANCE
      *
           MOVE DTL-LINE           TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE
           ADD 1                   TO WS-LINE-COUNT
           ADD 1                   TO WS-TOTAL-COUNT
           MOVE IN-BALANCE         TO WS-TOTAL-BAL-NUM
           ADD WS-TOTAL-BAL-NUM    TO WS-TOTAL-BALANCE
      *
           IF  WS-LINE-COUNT > WS-MAX-LINES
               MOVE 0              TO WS-LINE-COUNT
               PERFORM PRINT-HEADER
           END-IF.
      *
       PRINT-TOTALS.
           MOVE WS-TOTAL-COUNT     TO TOT-COUNT
           MOVE WS-TOTAL-BALANCE   TO WS-TOTAL-BAL-DISP
           MOVE WS-TOTAL-BAL-DISP  TO TOT-BALANCE
      *
           MOVE ALL '-'            TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE
      *
           MOVE TRLR-LINE          TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 2 LINES
           MOVE ALL '-'            TO RPT-REC
           WRITE RPT-REC AFTER ADVANCING 1 LINE.
      *
       CLOSE-ALL.
           CLOSE INFILE
           CLOSE RPTFILE.
      *
       STOP-PROCESS.
           STOP RUN.
