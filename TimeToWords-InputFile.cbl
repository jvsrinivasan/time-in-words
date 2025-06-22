      ******************************************************************
      * Author: Srinivasan JV
      * Date: 04-Jun-2025
      * Purpose: Inspired by the QLOCKTWO W Beigert & Funk Watch <3
      *          This COBOL program converts current system time into
      *          a readable textual format like "IT IS FIVE PAST ONE"
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIME-TO-WORDS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD             PIC 9(4).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD            PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-HOUR                  PIC 99.
       01  WS-MINUTE                PIC 99.
       01  WS-EOF                   PIC X VALUE 'N'.
           88 END-OF-FILE           VALUE 'Y'.
           88 NOT-END-OF-FILE       VALUE 'N'.

       01  WS-PART1                 PIC X(6)  VALUE 'IT IS '.
       01  WS-MINUTE-TEXT           PIC X(20).
       01  WS-MINUTE-TEXT-REV       PIC X(20).
       01  WS-MINUTE-TEXT-COUNT     PIC 99.
       01  WS-MINUTE-TEXT-LEN       PIC 99.
       01  WS-HOUR-TEXT             PIC X(20).
       01  WS-HOUR-TEXT-REV         PIC X(20).
       01  WS-HOUR-TEXT-COUNT       PIC 99.
       01  WS-HOUR-TEXT-LEN         PIC 99.
       01  WS-MINUTE-TO             PIC 99.
       01  WS-TIME-PERIOD           PIC X(20).
       01  WS-TIME-PERIOD-REV       PIC X(20).
       01  WS-TIME-PERIOD-COUNT     PIC 99.
       01  WS-TIME-PERIOD-LEN       PIC 99.
       01  WS-HOUR-1                PIC 99.
       01  WS-LINE                  PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      ******************************************************************
      * Open the input file containing 1440 lines - each line having the
      * time in HHMM format (from 0000 to 2359)
      ******************************************************************

           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM UNTIL END-OF-FILE
               READ INPUT-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PROCESS-TIME
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PROCESS-TIME.
           MOVE INPUT-RECORD(1:2) TO WS-HOUR
           MOVE INPUT-RECORD(3:2) TO WS-MINUTE

           PERFORM TIME-TO-WORDS-PARA-S THRU TIME-TO-WORDS-PARA-E.

      ******************************************************************
      * Write the time in words for each time value read from the i/p
      * file
      ******************************************************************

           INITIALIZE WS-MINUTE-TEXT-REV
                      WS-MINUTE-TEXT-COUNT
                      WS-MINUTE-TEXT-LEN
                      WS-HOUR-TEXT-REV
                      WS-HOUR-TEXT-COUNT
                      WS-HOUR-TEXT-LEN
                      WS-TIME-PERIOD-REV
                      WS-TIME-PERIOD-COUNT
                      WS-TIME-PERIOD-LEN
                      WS-LINE.
      ******************************************************************
      * The time in words are built piece by piece. The first part is
      * "IT IS " which is a string of fixed length. The second part
      * contains the text for Minutes, the third part contains the
      * text for hours and fourth part - text for time period. The
      * exact length of the 2nd, 3rd and 4th parts may vary.

      * The following block of code finds the exact
      * length of the text for the minute, hour and the time
      * period parts. For readability purpose, it is essential to not
      * display more no. of spaces between the texts when building
      * these parts with STRING verb
      ******************************************************************
           MOVE FUNCTION REVERSE(WS-HOUR-TEXT) TO WS-HOUR-TEXT-REV
           INSPECT WS-HOUR-TEXT-REV TALLYING WS-HOUR-TEXT-COUNT FOR
           LEADING SPACE
           COMPUTE WS-HOUR-TEXT-LEN = FUNCTION LENGTH(WS-HOUR-TEXT) -
           WS-HOUR-TEXT-COUNT

           MOVE FUNCTION REVERSE(WS-MINUTE-TEXT) TO WS-MINUTE-TEXT-REV
           INSPECT WS-MINUTE-TEXT-REV TALLYING WS-MINUTE-TEXT-COUNT FOR
           LEADING SPACE
           COMPUTE WS-MINUTE-TEXT-LEN = FUNCTION LENGTH(WS-MINUTE-TEXT)
           -            WS-MINUTE-TEXT-COUNT

           MOVE FUNCTION REVERSE(WS-TIME-PERIOD) TO WS-TIME-PERIOD-REV
           INSPECT WS-TIME-PERIOD-REV TALLYING WS-TIME-PERIOD-COUNT FOR
           LEADING SPACE
           COMPUTE WS-TIME-PERIOD-LEN = FUNCTION LENGTH(WS-TIME-PERIOD)
           -            WS-TIME-PERIOD-COUNT

      ******************************************************************
      * There is only one instance when the hour text appears before
      * the minute text and that is when the minute = 00. For ex, IT IS
      * ONE O'CLOCK. When the minute is > 00, the hour text always
      * appear after the minute text. For ex, IT IS TWELVE PAST ONE.
      * The following block of code handles this unique case
      ******************************************************************
           IF WS-MINUTE = 00
               STRING INPUT-RECORD DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      WS-PART1 DELIMITED BY SIZE
                      WS-HOUR-TEXT(1:WS-HOUR-TEXT-LEN) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      WS-MINUTE-TEXT(1:WS-MINUTE-TEXT-LEN) DELIMITED
                      BY SIZE
                      " " DELIMITED BY SIZE
                      WS-TIME-PERIOD(1:WS-TIME-PERIOD-LEN) DELIMITED
                      BY SIZE
                      INTO WS-LINE
           ELSE
               STRING INPUT-RECORD DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      WS-PART1 DELIMITED BY SIZE
                      WS-MINUTE-TEXT(1:WS-MINUTE-TEXT-LEN) DELIMITED
                      BY SIZE
                      " " DELIMITED BY SIZE
                      WS-HOUR-TEXT(1:WS-HOUR-TEXT-LEN) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      WS-TIME-PERIOD(1:WS-TIME-PERIOD-LEN) DELIMITED
                      BY SIZE
                      INTO WS-LINE
           END-IF

           MOVE WS-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.

       TIME-TO-WORDS-PARA-S.

      ******************************************************************
      * Determine how to express the minute part in words
      ******************************************************************
           EVALUATE TRUE
               WHEN WS-MINUTE = 0
                   MOVE "O' CLOCK" TO WS-MINUTE-TEXT

               WHEN WS-MINUTE = 15
                   MOVE "QUARTER PAST" TO WS-MINUTE-TEXT

               WHEN WS-MINUTE = 30
                   MOVE "HALF PAST" TO WS-MINUTE-TEXT

               WHEN WS-MINUTE < 30
                   EVALUATE WS-MINUTE
                       WHEN 1   MOVE "ONE PAST"        TO WS-MINUTE-TEXT
                       WHEN 2   MOVE "TWO PAST"        TO WS-MINUTE-TEXT
                       WHEN 3   MOVE "THREE PAST"      TO WS-MINUTE-TEXT
                       WHEN 4   MOVE "FOUR PAST"       TO WS-MINUTE-TEXT
                       WHEN 5   MOVE "FIVE PAST"       TO WS-MINUTE-TEXT
                       WHEN 6   MOVE "SIX PAST"        TO WS-MINUTE-TEXT
                       WHEN 7   MOVE "SEVEN PAST"      TO WS-MINUTE-TEXT
                       WHEN 8   MOVE "EIGHT PAST"      TO WS-MINUTE-TEXT
                       WHEN 9   MOVE "NINE PAST"       TO WS-MINUTE-TEXT
                       WHEN 10  MOVE "TEN PAST"        TO WS-MINUTE-TEXT
                       WHEN 11  MOVE "ELEVEN PAST"     TO WS-MINUTE-TEXT
                       WHEN 12  MOVE "TWELVE PAST"     TO WS-MINUTE-TEXT
                       WHEN 13  MOVE "THIRTEEN PAST"   TO WS-MINUTE-TEXT
                       WHEN 14  MOVE "FOURTEEN PAST"   TO WS-MINUTE-TEXT
                       WHEN 15  MOVE "FIFTEEN PAST"    TO WS-MINUTE-TEXT
                       WHEN 16  MOVE "SIXTEEN PAST"    TO WS-MINUTE-TEXT
                       WHEN 17  MOVE "SEVENTEEN PAST"  TO WS-MINUTE-TEXT
                       WHEN 18  MOVE "EIGHTEEN PAST"   TO WS-MINUTE-TEXT
                       WHEN 19  MOVE "NINETEEN PAST"   TO WS-MINUTE-TEXT
                       WHEN 20  MOVE "TWENTY PAST"     TO WS-MINUTE-TEXT
                       WHEN 21  MOVE "TWENTY ONE PAST" TO WS-MINUTE-TEXT
                       WHEN 22  MOVE "TWENTY TWO PAST" TO WS-MINUTE-TEXT
                       WHEN 23  MOVE "TWENTY THREE PAST"
                       TO WS-MINUTE-TEXT
                       WHEN 24  MOVE "TWENTY FOUR PAST"
                       TO WS-MINUTE-TEXT
                       WHEN 25  MOVE "TWENTY FIVE PAST"
                       TO WS-MINUTE-TEXT
                       WHEN 26  MOVE "TWENTY SIX PAST" TO WS-MINUTE-TEXT
                       WHEN 27  MOVE "TWENTY SEVEN PAST"
                       TO WS-MINUTE-TEXT
                       WHEN 28  MOVE "TWENTY EIGHT PAST"
                       TO WS-MINUTE-TEXT
                       WHEN 29  MOVE "TWENTY NINE PAST"
                       TO WS-MINUTE-TEXT
                   END-EVALUATE

               WHEN WS-MINUTE > 30
                   COMPUTE WS-MINUTE-TO = 60 - WS-MINUTE
                   EVALUATE WS-MINUTE-TO
                       WHEN 1  MOVE "ONE TO"           TO WS-MINUTE-TEXT
                       WHEN 2  MOVE "TWO TO"           TO WS-MINUTE-TEXT
                       WHEN 3  MOVE "THREE TO"         TO WS-MINUTE-TEXT
                       WHEN 4  MOVE "FOUR TO"          TO WS-MINUTE-TEXT
                       WHEN 5  MOVE "FIVE TO"          TO WS-MINUTE-TEXT
                       WHEN 6   MOVE "SIX TO"          TO WS-MINUTE-TEXT
                       WHEN 7   MOVE "SEVEN TO"        TO WS-MINUTE-TEXT
                       WHEN 8   MOVE "EIGHT TO"        TO WS-MINUTE-TEXT
                       WHEN 9   MOVE "NINE TO"         TO WS-MINUTE-TEXT
                       WHEN 10  MOVE "TEN TO"          TO WS-MINUTE-TEXT
                       WHEN 11  MOVE "ELEVEN TO"       TO WS-MINUTE-TEXT
                       WHEN 12  MOVE "TWELVE TO"       TO WS-MINUTE-TEXT
                       WHEN 13  MOVE "THIRTEEN TO"     TO WS-MINUTE-TEXT
                       WHEN 14  MOVE "FOURTEEN TO"     TO WS-MINUTE-TEXT
                       WHEN 15  MOVE "QUARTER TO"      TO WS-MINUTE-TEXT
                       WHEN 16  MOVE "SIXTEEN TO"      TO WS-MINUTE-TEXT
                       WHEN 17  MOVE "SEVENTEEN TO"    TO WS-MINUTE-TEXT
                       WHEN 18  MOVE "EIGHTEEN TO"     TO WS-MINUTE-TEXT
                       WHEN 19  MOVE "NINETEEN TO"     TO WS-MINUTE-TEXT
                       WHEN 20  MOVE "TWENTY TO"       TO WS-MINUTE-TEXT
                       WHEN 21  MOVE "TWENTY ONE TO"   TO WS-MINUTE-TEXT
                       WHEN 22  MOVE "TWENTY TWO TO"   TO WS-MINUTE-TEXT
                       WHEN 23  MOVE "TWENTY THREE TO"
                       TO WS-MINUTE-TEXT
                       WHEN 24  MOVE "TWENTY FOUR TO"
                       TO WS-MINUTE-TEXT
                       WHEN 25  MOVE "TWENTY FIVE TO"
                       TO WS-MINUTE-TEXT
                       WHEN 26  MOVE "TWENTY SIX TO"   TO WS-MINUTE-TEXT
                       WHEN 27  MOVE "TWENTY SEVEN TO"
                       TO WS-MINUTE-TEXT
                       WHEN 28  MOVE "TWENTY EIGHT TO"
                       TO WS-MINUTE-TEXT
                       WHEN 29  MOVE "TWENTY NINE TO"
                       TO WS-MINUTE-TEXT
                   END-EVALUATE
           END-EVALUATE

           MOVE WS-HOUR TO WS-HOUR-1

      ******************************************************************
      * Determine how to express the hour part in words
      ******************************************************************
           IF WS-HOUR-1 >= 12
               IF WS-MINUTE <= 30
                   SUBTRACT 12 FROM WS-HOUR-1
               ELSE IF WS-MINUTE > 30 AND WS-MINUTE <= 59
                   ADD 1 TO WS-HOUR-1
                   SUBTRACT 12 FROM WS-HOUR-1
               END-IF
               END-IF
           ELSE
               IF WS-MINUTE > 30 AND WS-MINUTE <= 59
                   ADD 1 TO WS-HOUR-1
               END-IF
           END-IF

           IF WS-HOUR-1 = 00
               MOVE 12 TO WS-HOUR-1
           END-IF

           EVALUATE WS-HOUR-1
           WHEN 1 MOVE "ONE" TO WS-HOUR-TEXT
           WHEN 2 MOVE "TWO" TO WS-HOUR-TEXT
           WHEN 3 MOVE "THREE" TO WS-HOUR-TEXT
           WHEN 4 MOVE "FOUR" TO WS-HOUR-TEXT
           WHEN 5 MOVE "FIVE" TO WS-HOUR-TEXT
           WHEN 6 MOVE "SIX" TO WS-HOUR-TEXT
           WHEN 7 MOVE "SEVEN" TO WS-HOUR-TEXT
           WHEN 8 MOVE "EIGHT" TO WS-HOUR-TEXT
           WHEN 9 MOVE "NINE" TO WS-HOUR-TEXT
           WHEN 10 MOVE "TEN" TO WS-HOUR-TEXT
           WHEN 11 MOVE "ELEVEN" TO WS-HOUR-TEXT
           WHEN 12 MOVE "TWELVE" TO WS-HOUR-TEXT
           END-EVALUATE

      ******************************************************************
      * Determine how to express the AM/PM in words
      ******************************************************************
           IF WS-HOUR = 12 AND WS-MINUTE = 0
               MOVE "IN THE NOON" TO WS-TIME-PERIOD
           ELSE
               EVALUATE TRUE
                   WHEN WS-HOUR < 12
                       MOVE "IN THE MORNING" TO WS-TIME-PERIOD
                   WHEN WS-HOUR = 12
                       MOVE "IN THE NOON" TO WS-TIME-PERIOD
                   WHEN WS-HOUR > 12 AND WS-HOUR < 17
                       MOVE "IN THE AFTERNOON" TO WS-TIME-PERIOD
                   WHEN WS-HOUR >= 17 AND WS-HOUR < 21
                       MOVE "IN THE EVENING" TO WS-TIME-PERIOD
                   WHEN OTHER
                       MOVE "IN THE NIGHT" TO WS-TIME-PERIOD
               END-EVALUATE
           END-IF.

       TIME-TO-WORDS-PARA-E. EXIT.

       END PROGRAM TIME-TO-WORDS.
