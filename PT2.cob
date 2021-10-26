       IDENTIFICATION DIVISION.
       PROGRAM-ID. PT2.
       AUTHOR. JNGUYE02.
      *PROJECT 2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE 
                   ASSIGN TO 'NEWEMP'.
           SELECT PRNT-FILE 
                   ASSIGN TO 'UR-S-PRNT'.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE
           DATA RECORD IS INPUT-REC.
       01 INPUT-REC.
               03 E-ID                  PIC X(7).
               03 E-LNAME               PIC X(15).
               03 E-FNAME               PIC X(15).
               03 E-TYPE                PIC X(2).
               03 E-TITLE               PIC X(17).
               03 E-SSN                 PIC X(9).
               03 FILLER                PIC X(1).
               03 E-STARTDATE           PIC X(8).
               03 FILLER                PIC X(1).
               03 E-RATE                PIC X(6).
               03 E-STATUS              PIC X(1).
               03 FILLER                PIC X(2).
       FD PRNT-FILE
           DATA RECORD IS PRINTZ.
       01 PRINTZ.
               03 SSN-PR                PIC 999B99B9999.
               03 FILLER                PIC X(3) VALUE SPACES.
               03 LNAME-PR              PIC X(15).
               03 FILLER                PIC X(2) VALUE SPACES.
               03 FNAME-PR              PIC X(15).
               03 FILLER                PIC X(3) VALUE SPACES.
               03 ID-PR                 PIC X(7).
               03 FILLER                PIC X(3) VALUE SPACES.
               03 TITLE-PR              PIC X(17).
               03 FILLER                PIC X(1) VALUE SPACES.
               03 TYPE-PR               PIC X(2).
               03 FILLER                PIC X(5) VALUE SPACES.
               03 STARTDATE-PR          PIC 99/99/9999.
               03 FILLER                PIC X(3) VALUE SPACES.
               03 RATE-PR               PIC $$$9.99.
               03 FILLER                PIC X(6) VALUE SPACES.
               03 STATUS-PR             PIC X(1).
       WORKING-STORAGE SECTION.
       01 INDICATORS.
               03 EOF-I                 PIC 9  VALUE 0.
       01 CURRENT-DATE-HDR.
               03 CUR-MON               PIC 9(2).
               03 FILLER                PIC X(1) VALUE "/".
               03 CUR-DAY               PIC 99.
               03 FILLER                PIC X(1) VALUE "/".
               03 CUR-YR                PIC 9(4).
       01 PAGE-HDR.
               03 FILLER                PIC X(36) VALUE SPACES.
               03 FILLER                PIC X(11) VALUE "THE BEST IS".
               03 FILLER                PIC X(5) VALUE " YET ".
               03 FILLER                PIC X(13) VALUE "TO COME, INC.".
       01 PAGE-HDR2.
               03 FILLER                PIC X(34) VALUE SPACES.
               03 FILLER                PIC X(9) VALUE "EMPLOYEE ".
               03 FILLER                PIC X(15) VALUE"CLASSIFICATION".
               03 FILLER                PIC X(8) VALUE " AND PAY".
       01 COLUMN-HDR.
               03 FILLER                PIC X(3) VALUE "SSN".
               03 FILLER                PIC X(11) VALUE SPACES.
               03 FILLER                PIC X(4) VALUE "LAST".
               03 FILLER                PIC X(13) VALUE SPACES.
               03 FILLER                PIC X(5) VALUE "FIRST".
               03 FILLER                PIC X(13) VALUE SPACES.
               03 FILLER                PIC X(6) VALUE "EMP ID".
               03 FILLER                PIC X(4) VALUE SPACES.
               03 FILLER                PIC X(5) VALUE "TITLE".
               03 FILLER                PIC X(13) VALUE SPACES.
               03 FILLER                PIC X(4) VALUE "TYPE".
               03 FILLER                PIC X(3) VALUE SPACES.
               03 FILLER                PIC X(4) VALUE "DATE".
               03 FILLER                PIC X(9) VALUE SPACES.
               03 FILLER                PIC X(4) VALUE "RATE".
               03 FILLER                PIC X(4) VALUE SPACES.
               03 FILLER                PIC X(6) VALUE "STATUS".
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM A-100-INITIALIZATION.
           PERFORM B-100-PROCESS-FILE.
           PERFORM C-100-WRAP-UP.
           STOP RUN.
       A-100-INITIALIZATION.
           OPEN INPUT INPUT-FILE
           OUTPUT PRNT-FILE.
           UNSTRING FUNCTION CURRENT-DATE
           INTO CUR-YR CUR-MON CUR-DAY
           END-UNSTRING.
           DISPLAY CURRENT-DATE-HDR.
           WRITE PRINTZ FROM PAGE-HDR
                AFTER ADVANCING PAGE.
           WRITE PRINTZ FROM PAGE-HDR2
                AFTER ADVANCING 1 LINE.
           WRITE PRINTZ FROM COLUMN-HDR
                AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINTZ.
           WRITE PRINTZ
                AFTER ADVANCING 1 LINE.
       B-100-PROCESS-FILE.
           READ INPUT-FILE
                   AT END
                   MOVE 1 TO EOF-I.
           PERFORM B-200-PROCESS-RECORD
                UNTIL EOF-I = 1.
       B-200-PROCESS-RECORD.
           MOVE SPACES TO PRINTZ.
           MOVE E-ID TO ID-PR.
           MOVE E-LNAME TO LNAME-PR.
           MOVE E-FNAME TO FNAME-PR.
           MOVE E-TYPE TO TYPE-PR.
           MOVE E-TITLE TO TITLE-PR.
           MOVE E-SSN TO SSN-PR.
           INSPECT SSN-PR REPLACING ALL ' ' BY '-'.
           MOVE E-STARTDATE TO STARTDATE-PR.
           MOVE E-RATE TO RATE-PR.
           MOVE E-STATUS TO STATUS-PR.
           WRITE PRINTZ
                AFTER ADVANCING 1 LINE.
           READ INPUT-FILE
                   AT END
                   MOVE 1 TO EOF-I.
       C-100-WRAP-UP.
           CLOSE INPUT-FILE
                PRNT-FILE.
