      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CAIQ01.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
      *-------------
       DATA DIVISION.
      *-------------
       WORKING-STORAGE SECTION.
      *
       COPY DFHAID.
      *
       COPY CAIQM01.
      *
       01  WS-RESP              PIC S9(08)  COMP.
       01  WS-RESP2             PIC S9(08)  COMP.
      *
       01  WS-TRANSID           PIC X(4).
      *
       01  WS-PF3-MESSAGE       PIC X(17) VALUE 'Exiting program..'.
      *
       01 WS-BAL-DISP           PIC ZZZ,ZZZ,ZZ9.99.
      *
       01  WS-CUST-REC.
           05  WS-CUST-ID        PIC X(10).
           05  WS-CUST-NAME      PIC X(30).
           05  WS-ACCT-TYPE      PIC X(10).
           05  WS-BRANCH-CODE    PIC X(06).
           05  WS-ACCT-BALANCE   PIC 9(09)V99.
           05  WS-ACCT-STATUS    PIC X(10).
     *
       EXEC SQL INCLUDE SQLCA END-EXEC.
      *
       01  HV-ACCTNO             PIC X(8).
       01  HV-ADDR1              PIC X(25).
       01  HV-ADDR2              PIC X(20).
       01  HV-ADDR3              PIC X(15).
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  LK-CUST-REC.
               10 LK-CUST-ID        PIC X(10).
               10 LK-CUST-NAME      PIC X(30).
               10 LK-ACCT-TYPE      PIC X(10).
               10 LK-BRANCH-CODE    PIC X(06).
               10 LK-ACCT-BALANCE   PIC 9(09)V99.
               10 LK-ACCT-STATUS    PIC X(10).
      *
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *
       MAIN-PARA.
      *
           MOVE EIBTRNID TO WS-TRANSID.
      *
           IF EIBCALEN = 0
               PERFORM SEND-INITIAL-MAP
               GO TO END-PROG
           ELSE
               MOVE DFHCOMMAREA TO WS-CUST-REC
               PERFORM RECEIVE-MAP
               PERFORM VALIDATE-INPUT
               PERFORM READ-VSAM
               PERFORM SEND-OUTPUT
               GO TO END-PROG
           END-IF.
      *
       SEND-INITIAL-MAP.
           EXEC CICS SEND
               MAP('CAIQM01')
               MAPSET('CAIQM01')
               MAPONLY
               ERASE
           END-EXEC.
      *
           MOVE SPACES     TO WS-CUST-REC.
      *
       RECEIVE-MAP.
           EXEC CICS RECEIVE
               MAP('CAIQM01')
               MAPSET('CAIQM01')
               RESP(WS-RESP)
           END-EXEC.
      *
           IF EIBAID = DFHPF3 OR EIBAID = DFHCLEAR
              EXEC CICS SEND
                  CONTROL ERASE
              END-EXEC
      *
              EXEC CICS RETURN
              END-EXEC
           END-IF.
      *
           IF WS-RESP = DFHRESP(MAPFAIL)
              PERFORM INIT-ALL-OUTPUT-FIELDS
              MOVE 'ENTER CUSTOMER ID'    TO MSGLO
              PERFORM SEND-OUTPUT
              GO TO END-PROG
           END-IF.
      *
           IF WS-RESP NOT = DFHRESP(NORMAL)
              PERFORM INIT-ALL-OUTPUT-FIELDS
              MOVE 'TECHNICAL ERROR'      TO MSGLO
              PERFORM SEND-OUTPUT
              GO TO END-PROG
           END-IF.
      *
       VALIDATE-INPUT.
           IF CUSTIDI = SPACES
              PERFORM INIT-OUTPUT-FIELDS
              MOVE 'CUSTOMER ID CANNOT BE BLANK'   TO  MSGLO
              MOVE CUSTIDI                         TO CUSTIDO
              PERFORM SEND-OUTPUT
              GO TO END-PROG
            END-IF.
      *
           IF CUSTIDI NOT NUMERIC
               PERFORM INIT-OUTPUT-FIELDS
               MOVE 'CUSTOMER ID MUST BE NUMERIC'  TO MSGLO
               MOVE CUSTIDI                        TO CUSTIDO
               PERFORM SEND-OUTPUT
               GO TO END-PROG
           END-IF.
      *
       READ-VSAM.
           MOVE SPACES TO HV-ADDR1 HV-ADDR2 HV-ADDR3
           MOVE CUSTIDI  TO  WS-CUST-ID
      *
           EXEC CICS READ
               FILE('CAIQF01')
               INTO(WS-CUST-REC)
               RIDFLD(WS-CUST-ID)
               RESP(WS-RESP)
               RESP2(WS-RESP2)
           END-EXEC.
      *
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                  PERFORM GET-VALUES-FROM-TABLE
               WHEN DFHRESP(NOTFND)
                  PERFORM INIT-OUTPUT-FIELDS
                  MOVE 'CUSTOMER NOT FOUND'             TO MSGLO
               WHEN OTHER
                  PERFORM INIT-OUTPUT-FIELDS
                  MOVE 'FILE ERROR CONTACT ADMIN'       TO MSGLO
           END-EVALUATE.
      *
       GET-VALUES-FROM-TABLE.
           EXEC CICS HANDLE ABEND
              LABEL(DB2-NOT-AVAILABLE)
           END-EXEC
      *
           PERFORM DO-DB2-SELECT
      *
           EXEC CICS HANDLE ABEND CANCEL
           END-EXEC
      *
           PERFORM CHECK-SQLCODE
      *
           PERFORM POP-OUTPUT-FIELDS.
      *
       DO-DB2-SELECT.
           MOVE CUSTIDI(1:8) TO HV-ACCTNO.
      *
           EXEC SQL
              SELECT ADDRESS1,
                     ADDRESS2,
                     ADDRESS3
                INTO :HV-ADDR1,
                     :HV-ADDR2,
                     :HV-ADDR3
                FROM Z88436.Z88436T
               WHERE ACCTNO = :HV-ACCTNO
           END-EXEC.
      *
       CHECK-SQLCODE.
           EVALUATE SQLCODE
                   WHEN 0
                       CONTINUE
                   WHEN 100
                       MOVE SPACES TO HV-ADDR1 HV-ADDR2 HV-ADDR3
                       MOVE 'CUSTOMER ADDRESS NOT FOUND'      TO MSGLO
                   WHEN OTHER
                       MOVE SPACES TO HV-ADDR1 HV-ADDR2 HV-ADDR3
                       MOVE 'DATABASE ERROR CONTACT ADMIN'    TO MSGLO
           END-EVALUATE.
      *
       POP-OUTPUT-FIELDS.
           MOVE CUSTIDI            TO CUSTIDO
           MOVE WS-CUST-NAME       TO NAMEOO
           MOVE WS-ACCT-TYPE       TO TYPEOO
           MOVE WS-BRANCH-CODE     TO BRCDOO
           MOVE WS-ACCT-BALANCE    TO WS-BAL-DISP
           MOVE WS-BAL-DISP        TO BALOO
           MOVE WS-ACCT-STATUS     TO STATOO
      *
           MOVE HV-ADDR1           TO ADDR1OO
           MOVE HV-ADDR2           TO ADDR2OO
           MOVE HV-ADDR3           TO ADDR3OO.
      *
       INIT-OUTPUT-FIELDS.
           INITIALIZE NAMEOO
           INITIALIZE TYPEOO
           INITIALIZE BRCDOO
           INITIALIZE BALOO
           INITIALIZE STATOO
           INITIALIZE ADDR1OO
           INITIALIZE ADDR2OO
           INITIALIZE ADDR3OO.
      *
       INIT-ALL-OUTPUT-FIELDS.
           INITIALIZE CUSTIDO
           INITIALIZE NAMEOO
           INITIALIZE TYPEOO
           INITIALIZE BRCDOO
           INITIALIZE BALOO
           INITIALIZE STATOO
           INITIALIZE ADDR1OO
           INITIALIZE ADDR2OO
           INITIALIZE ADDR3OO.
      *
       SEND-OUTPUT.
           EXEC CICS SEND
               MAP('CAIQM01')
               MAPSET('CAIQM01')
               FROM(CAIQM01O)
               ERASE
           END-EXEC.
      *
       END-PROG.
           EXEC CICS RETURN
              TRANSID(WS-TRANSID)
              COMMAREA(WS-CUST-REC)
              LENGTH(LENGTH OF WS-CUST-REC)
           END-EXEC.
      *
       DB2-NOT-AVAILABLE.
           EXEC CICS HANDLE ABEND CANCEL
           END-EXEC
      *
           MOVE SPACES TO HV-ADDR1 HV-ADDR2 HV-ADDR3
           MOVE 'DB2 NOT AVAILABLE IN THIS REGION'  TO MSGLO
      *
           PERFORM POP-OUTPUT-FIELDS
           PERFORM SEND-OUTPUT
           GO TO END-PROG.
