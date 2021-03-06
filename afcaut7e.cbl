?CONSULT $SYSTEM.SYSTEM.COBOLEX0
?SEARCH  $SYSTEM.SYSTEM.COBOLLIB
?SEARCH  =TALLIB
?SEARCH  =ASC2EBC
?SEARCH  =EBC2ASC
?SEARCH  =IUMSW07
?SEARCH  =SDBCDU5
?SEARCH  =SYSABL1
?SEARCH  =SYSMK7I
?SEARCH  =WCAPM92
?SEARCH  =WISO107
?SEARCH  =WISO207
*kl20170503 - G.02.00 - Fuer EMV-Tag-Gefummel
?SEARCH  =WISO300
?SEARCH  =WISO310
?SEARCH  =WISO400
*kl20170503 - G.02.00 - Ende
?SEARCH  =WSYS020
?SEARCH  =WSYS022
?SEARCH  =WSY7065
?SEARCH  =WSY7066
?SEARCH  =WSYS971
?SEARCH  =WSYS980
?SEARCH  =WSYS990
?SEARCH  =WCSI060
?SEARCH  =WEUR055
?SEARCH  =WEUR056

?NOLMAP, SYMBOLS, INSPECT
?SAVE ALL
?SAVEABEND
?LINES 66
?CHECK 3
?SQL

 IDENTIFICATION DIVISION.

 PROGRAM-ID. AFCAUT7 .

 DATE-COMPILED.


*******************************************************************************
* Letzte Aenderung :: 2019-04-05
* Letzte Version   :: G.03.04
* Kurzbeschreibung :: Dieses Programm setzt Flottenkarten-
* Kurzbeschreibung :: Autorisierungsanantworten vom AS-IFSF-Protokoll
* Kurzbeschreibung :: auf WEAT-TERMINAL-Protokoll um. Bearbeitet werden
* Kurzbeschreibung :: nur AS-Nachrichten von Typ 1210, die
* Kurzbeschreibung :: auf Termial-Nachrichten vom Typ 210 umgesetzt
* Kurzbeschreibung :: werden.
* Package          :: ICC
*
* Aenderungen
*
*--------------------------------------------------------------------*
* Vers. | Datum    | von | Kommentar                                 *
*-------|----------|-----|-------------------------------------------*
*G.03.04|2019-04-05| kus | R7-496:
*       |          |     | - ASID richtig
*-------|----------|-----|-------------------------------------------*
*G.03.03|2019-03-12| kus | E100-3:
*       |          |     | - Umsetzung E100
*-------|----------|-----|-------------------------------------------*
*G.03.02|2018-11-19| kus | DKVCHIP-25:
*       |          |     | - BMP 55 ggf. vom AS an TS weiterleiten
*-------|----------|-----|-------------------------------------------*
*G.03.01|2018-09-11| kus | R7-376:
*       |          |     | - Umstellung von festem ROUTKZ auf AS-Verf
*-------|----------|-----|-------------------------------------------*
*G.02.10|2018-05-30| kl  | RRIFSF-2:
*       |          |     | - Neues FK-AS: Road Runner (RK=25)
*       |          |     |   (Vereinbarung: Identisch WEAT AS 2)
*-------|----------|-----|-------------------------------------------*
*G.02.10|2018-04-04| kl  | R7-272:
*       |          |     | Optimierung Zugriff / Laden FCPARAM
*       |          |     | (CARDID = 0 oder X - Vorrang bei X)
*-------|----------|-----|----------------------------------------------------*
*G.02.09|2018-02-13| hkn | BMP32 in Nachricht 210 mit L�ngenangabe
*-------|----------|-----|----------------------------------------------------*
*G.02.08|2018-02-01| hkn | R7-279 POST70 entfernt, da nicht benutzt
*-------|----------|-----|----------------------------------------------------*
*G.02.07|2018-01-22| hkn | IQ-3  Neu: Stiglechner mit Routkz = 24
*-------|----------|-----|--- ------------------------------------------------*
*G.02.06|2018-01-09| kl  | Initialierung T-MAX in B000 optimiert              *
*-------|----------|-----|----------------------------------------------------*
*G.02.05|2018-01-05| kl  | Speichertabelle fuer FCPARAM vergroessert          *
*-------|----------|-----|----------------------------------------------------*
*G.02.04|2018-01-04| hkn | R7-Version aus X-Version kopiert                   *
*-------|----------|-----|----------------------------------------------------*
*G.02.03|2017-12-12| kl  | Keine Pr�fung auf BMP 32 bei WAS2                  *
*       |          |     | wg. ausl�ndischer Karten (wird vom ausl. AS        *
*       |          |     | neu gesetzt) - F1ICC-69                            *
*-------|----------|-----|----------------------------------------------------*
*G.02.02|2017-08-03| hkn |TXILOG70.AC-TERM aus TABL-ABL,                      *
*       |          |     |wenn TABL-ABL gesetzt                               *
*-------|----------|-----|----------------------------------------------------*
*G.02.01|2017-05-19| kl  | - Stack-Adressierung Belegnr korrigiert
*       |          |     |   (in C050-GET-MEMLOG)
*       |          |     | - VUNR nur ans Terminal zur�ck,
*       |          |     |   wenn nicht in Terminal-Anfrage
*-------|----------|-----|----------------------------------------------------*
*G.02.00|2017-05-03| kl  | - Chipkartenverarbeitung integriert
*       |          |     | - MEMLOG-Key jetzt wieder komplettes BMP 59 (statt
*       |          |     |   Teilstring aus BMP 59)
*       |          |     | - Daraus resultierende Aufr�umarbeiten in C050
*-------|----------|-----|----------------------------------------------------*
*G.01.18|2017-06.20| hkn | BMP39: Add - Nach dem speziellen Teil
*       |          |     | TERMABL: Logik angepasst, da fehlerhaft
*-------|----------|-----|----------------------------------------------------*
*G.01.17|2017-05-17| hkn | Falsche Belegnr. bei Manueller Autorisierung
*       |          |     | Wurde mit G.02.01 angepasst
*-------|----------|-----|----------------------------------------------------*
*G.01.15|2017-05-03| hkn | �berarbeitung AC-Werte
*-------|----------|-----|----------------------------------------------------*
*G.01.14|2017-03-23| hkn | MAC-Pr�fung am Ende der ANTWORT-Pr�fung
*-------|----------|-----|----------------------------------------------------*
*G.01.13|2017-03-21| hkn | Logging: Nach MAC-Pr�fung mit Fehler vom AS
*-------|----------|-----|----------------------------------------------------*
*G.01.12|2017-03-15| hkn | �bersetzung wg. �nderung DUKPT
*-------|----------|-----|----------------------------------------------------*
*G.01.11|2017-02-02| hkn | FCPARM ersetzt durch FCPARAM
*-------|----------|-----|----------------------------------------------------*
*G.01.10|2016-11-08| hkn | �bersetzung wg. �nderung DUKPT
*-------|----------|-----|----------------------------------------------------*
*G.01.09|2016-11-07| hkn | �bersetzung wg. �nderung DUKPT
*-------|----------|-----|----------------------------------------------------*
*G.01.08|2016-11-01| hkn | Fehlende Unterprograme eingef�gt
*-------|----------|-----|----------------------------------------------------*
*G.01.07|2016-11-01| hkn | Nur �bersetzt, wg. �nderung DUKPT
*-------|----------|-----|----------------------------------------------------*
*G.01.06|2016-10-26| hkn | Shell: Lokale Variablen nach Memlogzugriff
*       |          |     | erneut zuweisen
*-------|----------|-----|----------------------------------------------------*
*G.01.05|2016-10-25| hkn | Neu: LogPay mit Routkz = 23
*-------|----------|-----|----------------------------------------------------*
*G.01.04|2016-10-12| hkn | EUROWAG: PAC Umschl�ssen und Bilden
*       |          |     | mit DUKPT-Verfahren
*-------|----------|-----|----------------------------------------------------*
*G.01.03|2016-09-14| sk  | Wenn eine EMV-Konfig vorliegt, steht im
*       |          |     | M120-CALL-SYSABL1 ein nicht num. Wert in W-ACX
*       |          |     | (A0). Dieser darf nicht zum SDBCDUx-Modul gesendet
*       |          |     | werden, sondern eine 0;(=crdusedn.AC ist numerisch)
*-------|----------|-----|----------------------------------------------------*
*G.01.01|2016-08-09| hkn | Neu: Eurowag mit Routkz = 22
*-------|----------|-----|----------------------------------------------------*
*G.01.00|2016-06-14| hkn |Fall1: AS-AC 185 und AS-BMP 62 vorhanden, Mapping von
*       |          |     |mind. einem Produkt m�glich.
*       |          |     |Korrekt.: 210er AC 87, BMP 63 mit mind. einem Produkt
*       |          |     |
*       |          |     |Fall2: AS-AC 185 und AS-BMP 62 vorhanden,
*       |          |     |jedoch kein Mapping m�glich.
*       |          |     |Korrekt.: 210er AC 87, BMP 63 mit 0 Produkten
*       |          |     |
*       |          |     |Fall3: AS-AC 185 und AS-BMP 62 nicht vorhanden
*       |          |     |Korrekt.: 210er AC 87, BMP 63 mit 0 Produkten
*       |          |     |
*-------|----------|-----|----------------------------------------------------*
*G.00.06|2016-05-19| hkn | C500-LOGGING: Umsatzscheibung gepr�ft
*-------|----------|-----|----------------------------------------------------*
*G.00.05|2016-05-17| kl  | BMP 59 nochmals korrigiert
*-------|----------|-----|----------------------------------------------------*
*G.00.04|2016-05-13| kl  | BMP 59 Anpassungen:
*       |          |     |
*       |          |     | - jetzt Release+Applikation+ASTRACENR
*       |          |     | - Belegnr. fuer UMSWEAT muss dann aus
*       |          |     |   BMP 3 der TSANFRAGE kommen
*       |          |     |   (funktioniert, da ohne TSANFRAGE keine
*       |          |     |    Verarbeitung sic kein UMSWEAT)
*-------|----------|-----|----------------------------------------------------*
*G.00.03|2016-04-28| cb  | Neucompile da �nderung im Modul SYSABL1M
*-------|----------|-----|----------------------------------------------------*
*G.00.02|2015-12-03| kl  | Steuerung Artikelmapping der Host-
*       |          |     | antwort etwas optimiert (TS.BMP63 nur,
*       |          |     | wenn auch in TS-Anfrage gesetzt wg.
*       |          |     | Crash-Gefahr)
*-------|----------|-----|----------------------------------------------------*
*G.00.01|2015-10-19| HKN |Aufruf Artikelmapper pro AS'sen
*-------|----------|-----|----------------------------------------------------*
*G.00.00|2015-03-10| BAH |Neuerstellung
*-----------------------------------------------------------------------------*
*
* Programmbeschreibung
* --------------------
*
* Das Programm setzt Flottenkarten-IFSF-Autorisierungs-Antworten
* auf WEAT-Terminalformat um. Dabei muss die Umsetzung
* f�r jedes AS programmiert werden. Umsetzungsregeln k�nnen in der
* Tabelle =FCPARAM hinterlegt werden. Vorgesehen sind zun�chst die
* folgenden Flottenkartenautorisierungssysteme:
*
*    -   Avia    (erste Ausbaustufe)
*    -   Shell
*    -   Total
*    -   DKV
*    -   EUROWAG
*    -   STIGLECHNER
*    -   LOGPAY
*    -   BP
*    -   ENI
*    -   Orlen
*    -   UTA
*    -   TND
*
* F�r jedes AS muss eine eigene Serverklasse mit diesem Programm
* definiert werden (z.B. AFCAUT7S-05 f�r Avia). Die Keys f�r das
* jeweilige AS werden in der Tabelle =KEYNAMEN erwartet.
*
* Das Programm erwartet die folgenden Parameter:
*
*    -   AS-ROUTKZ   Routkennzeichen f�r das relevante AS
*    -   MACKEYT     MAC-Schl�ssel f�r Terminaltransaktionen
*    -   MACKEYA     MAC-Schl�ssel f�r AS-Transaktionen
*    -   BOXMON      Festlegung des Boxenservers
*    -   ARTMAP      Festlegung des zust�ndigen Artikel-Mappers
*
*
*
*
*
******************************************************************
******************************************************************
*
* Uebersicht der SECTIONs:
*
* CONFIGURATION
* INPUT-OUTPUT
* FILE
* WORKING-STORAGE
* EXTENDED-STORAGE
*
* A000-WHENEVER
* A100-STEUERUNG
*
* B000-VORLAUF
* B090-ENDE
* B100-VERARBEITUNG
*
* C000-INIT
* C100-ANTWORT-CHECK
* C200-AS-GENERELL
* C300-AS-SPEZIELL
* C400-BUILD-TS-NACHRICHT
* C500-LOGGING
*
* D305-AVIA
* D307-SHELL
* D310-TOTAL
* D312-DKV
* D322-EUROWAG
* D326-STIGLECHNER
* D324-LOGPAY
* D314-BP
* D315-ENI
* D316-ORLEN
* D317-UTA
* D318-TND
*
* E300-ARTIKELDATEN
* E900-PUT-ERRLOG
*
* F910-MAC-PRUEFEN
* F920-MAC-BILDEN
* F915-ASMAC-DUKPT
*
* G100-PUT-TXILOG70
* G110-PUT-TXNLOG70-TS
* G120-PUT-TXNLOG70-AS
* G130-PUT-UMSWEAT
* G140-PUT-CRDUSEDN
*
* L100-ADD-BMP
* L110-COB2ISO
* L120-ISO2COB
*
* M100-CALL-SDBCDU5
* M110-CALL-SYSMK7I
* M120-CALL-SYSABL1
* M130-CALL-WISO207
* M140-CALL-WSY7066
* M160-CALL-WSYS971
* M180-CALL-IUMSW07
*
* P100-PATHSEND
* P900-WTHEX
* P910-WTUNHEX
* P950-GETPARAMTEXT
* P960-GET-PROC-ANCNAME
*
* R000-LESEN-MESSAGE
*
* S000-SCHREIBEN-REPLY
* S100-SELECT-KEYNAMEN
* S180-INSERT-TXILOG70
* S190-INSERT-TXNLOG70-TS
* S200-INSERT-TXNLOG70-AS
* S900-OPEN-FCPARAM-CURSOR
* S910-FETCH-FCPARAM-CURSOR
* S920-CLOSE-FCPARAM-CURSOR
* S930-OPEN-KEYNAMEN-CURSOR
* S940-FETCH-KEYNAMEN-CURSOR
* S950-CLOSE-KEYNAMEN-CURSOR
* S960-OPEN-IFSFAC-CURSOR
* S970-FETCH-IFSFAC-CURSOR
* S980-CLOSE-IFSFAC-CURSOR
*
* U100-BEGIN
* U101-BEGINTRANSACTION
* U110-COMMIT
* U111-ENDTRANSACTION
* U120-ROLLBACK
* U121-ABORTTRANSACTION
* U200-TIMESTAMP
* U300-SEARCH-TAB
* U400-INTERPRET-ABWEICHUNG
*
* Z001-SQLERROR
* Z002-PROGERR
* Z999-ERRLOG
*
******************************************************************


 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SPECIAL-NAMES.
     SWITCH-1 IS TRACE-FLAG
         ON  STATUS IS TRACE-ON
         OFF STATUS IS TRACE-OFF
     SWITCH-15 IS ANZEIGE-VERSION
         ON STATUS IS SHOW-VERSION
     CLASS ALPHNUM IS "0123456789"
                      "abcdefghijklmnopqrstuvwxyz"
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      " .,;-_!�$%&/=*+"
     DECIMAL-POINT IS COMMA.

 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     SELECT MESSAGE-DATEI     ASSIGN TO $RECEIVE
                              FILE STATUS IS FILE-STATUS.
     SELECT REPLY-DATEI       ASSIGN TO $RECEIVE
                              FILE STATUS IS FILE-STATUS.

 RECEIVE-CONTROL.
     TABLE OCCURS 20 TIMES
     REPLY CONTAINS MESSAGE-DATEI RECORD.

 DATA DIVISION.
 FILE SECTION.

 FD  MESSAGE-DATEI IS EXTERNAL
     RECORD IS VARYING IN SIZE.
**          ---> FREGAT-Satz + interner Zusatz (Laenge = 4096)
 01          INTERN-MESSAGE.
             COPY    INT-SCHNITTSTELLE-C OF  "=MSGLIB"
                     REPLACING =="*"== BY ==IMSG==.

 FD  REPLY-DATEI
     RECORD IS VARYING IN SIZE
               FROM 12 TO 4096 CHARACTERS
               DEPENDING ON REPLY-LAENGE.

 01          REPLY-SATZ.
     05      REPLY-DUMMY         PIC S9(04) COMP.
     05      REPLY-FLAG          PIC X(10).
     05      FILLER              PIC X(4084).

 WORKING-STORAGE SECTION.
*--------------------------------------------------------------------*
* Comp-Felder: Pr�fix Cn mit n = Anzahl Digits
*--------------------------------------------------------------------*
 01          COMP-FELDER.
     05      C4-ANZ              PIC S9(04) COMP.
     05      C4-COUNT            PIC S9(04) COMP.
     05      C4-I1               PIC S9(04) COMP.
     05      C4-I2               PIC S9(04) COMP.
     05      C4-I3               PIC S9(04) COMP.
     05      C4-LEN              PIC S9(04) COMP.
     05      C4-PTR              PIC S9(04) COMP.

     05      C4-X.
      10                         PIC X value low-value.
      10     C4-X2               PIC X.
     05      C4-NUM redefines C4-X
                                 PIC S9(04) COMP.

     05      C9-ANZ              PIC S9(09) COMP.
     05      C9-COUNT            PIC S9(09) COMP.

     05      C18-VAL             PIC S9(18) COMP.

     05      REPLY-LAENGE        PIC  9(04) COMP.

*--------------------------------------------------------------------*
* Display-Felder: Pr�fix D
*--------------------------------------------------------------------*
 01          DISPLAY-FELDER.
     05      D-NUM1              PIC  9.
     05      D-NUM2              PIC  9(02).
     05      D-NUM20             PIC  9(02).
     05      D-NUM21             PIC  9(02).
     05      D-NUM3              PIC  9(03).
     05      D-NUM4              PIC -9(04).
     05      D-NUM4M             PIC  9(04).
     05      D-NUM4N             PIC  9(04).
     05      D-NUM4OV            PIC  9(04).
     05      D-NUM6              PIC  9(06).
     05      D-NUM61             PIC  9(06).
     05      D-NUM9              PIC  9(09).
     05      D-NUM12             PIC  9(12).
     05      D-NUM18             PIC  9(18).

*--------------------------------------------------------------------*
* Felder mit konstantem Inhalt: Pr�fix K
*--------------------------------------------------------------------*
 01          KONSTANTE-FELDER.
     05      K-MODUL             PIC X(08)          VALUE "AFCAUT7S".

**          ---> Pflichtfelder einer 210-TS-Nachricht
     05      K-BYTEMAP-A210      PIC X(64) VALUE
     "0011000000111000000000000000000010000010100000001000000000000000".
**             1         2         3         4         5         6
**    1234567890123456789012345678901234567890123456789012345678901234


*----------------------------------------------------------------*
* Conditional-Felder
*----------------------------------------------------------------*
 01          SCHALTER.
     05      FILE-STATUS         PIC X(02).
          88 FILE-OK                         VALUE "00".
          88 FILE-NOK                        VALUE "01" THRU "99".
          88 FILE-TIME-OUT                   VALUE "30".
     05      REC-STAT REDEFINES  FILE-STATUS.
        10   FILE-STATUS1        PIC X.
          88 FILE-EOF                        VALUE "1".
          88 FILE-INVALID                    VALUE "2".
          88 FILE-PERMERR                    VALUE "3".
          88 FILE-LOGICERR                   VALUE "4".
          88 FILE-NONAME                     VALUE "5" THRU "8".
          88 FILE-IMPLERR                    VALUE "9".
        10                       PIC X.

     05      MSG-STATUS          PIC 9       VALUE ZERO.
          88 MSG-OK                          VALUE ZERO.
          88 MSG-EOF                         VALUE 1.

     05      PRG-STATUS          PIC 9.
          88 PRG-OK                          VALUE ZERO.
          88 PRG-NOK                         VALUE 1 THRU 9.
          88 PRG-ENDE                        VALUE 1.
          88 PRG-ABBRUCH                     VALUE 2.

     05      ENDE-FLAG           PIC 9       VALUE ZERO.
          88 ENDE-OFF                        VALUE ZERO.
          88 ENDE                            VALUE 1.

     05      FCPARAM-FLAG         PIC 9       VALUE ZERO.
          88 FCPARAM-OK                       VALUE ZERO.
          88 FCPARAM-EOD                      VALUE 1.
          88 FCPARAM-NOK                      VALUE 9.

     05      KEYNAMEN-FLAG       PIC 9       VALUE ZERO.
          88 KEYNAMEN-OK                     VALUE ZERO.
          88 KEYNAMEN-EOD                    VALUE 1.
          88 KEYNAMEN-NOK                    VALUE 9.

     05      IFSFAC-FLAG         PIC 9       VALUE ZERO.
          88 IFSFAC-OK                       VALUE ZERO.
          88 IFSFAC-NOK                      VALUE 1.

     05      TXILOG70-FLAG       PIC 9       VALUE ZERO.
          88 TXILOG70-OK                     VALUE ZERO.
          88 TXILOG70-NOK                    VALUE 1.

     05      TXNLOG70-FLAG       PIC 9       VALUE ZERO.
          88 TXNLOG70-OK                     VALUE ZERO.
          88 TXNLOG70-NOK                    VALUE 1.

     05      UMS-FLAG            PIC X     VALUE SPACE.
          88 UMS-ZAHLUNG                   VALUE SPACE.
          88 UMS-GUTSCHRIFT                VALUE HIGH-VALUE.

     05      PRM-FLAG            PIC X     VALUE SPACE.
          88 PRM-NOT-FOUND                 VALUE SPACE.
          88 PRM-FOUND                     VALUE HIGH-VALUE.

     05      ERF-FLAG            PIC X     VALUE SPACE.
          88 ERF-ERROR                     VALUE SPACE.
          88 ERF-SPUR2                     VALUE LOW-VALUE.
          88 ERF-MANUELL                   VALUE HIGH-VALUE.

     05      PAC-FLAG            PIC X     VALUE LOW-VALUE.
          88 PAC-YES                       VALUE LOW-VALUE.
          88 PAC-NO                        VALUE HIGH-VALUE.

     05      MAC-FLAG            PIC X     VALUE LOW-VALUE.
          88 MAC-YES                       VALUE LOW-VALUE.
          88 MAC-NO                        VALUE HIGH-VALUE.

     05      GEODATA-FLAG        PIC X     VALUE LOW-VALUE.
          88 GEODATA-NO                    VALUE LOW-VALUE.
          88 GEODATA-YES                   VALUE HIGH-VALUE.

     05      FEP-ANTWORT-FLAG    PIC X     VALUE LOW-VALUE.
          88 NO-FEP-ANTWORT                VALUE LOW-VALUE.
          88 FEP-ANTWORT                   VALUE HIGH-VALUE.


*--------------------------------------------------------------------*
* weitere Arbeitsfelder
*--------------------------------------------------------------------*
**          ---> unver�ndert
 01          WORK-FELDER.
     05      W-ROUTKZ            PIC S9(04) comp.
     05      W-KEYNAME           PIC  X(08).

**          ---> werden bei jeder Tx initiert
 01          WORK-INIT.
     05      W-CARDID            PIC S9(04) comp.
     05      W-KANR-LEN          PIC S9(04) comp.
     05      W18-BETRAG          PIC S9(16)V99 comp.
     05      W-ZP-VERKAUF        PIC S9(18) COMP.

     05      W-ACX.
      10     W-AC                PIC 9(02).
     05      W-AC-AS             PIC 9(03).
     05      W-ACQUIRER-ID       PIC X(06).
     05      W-GENNR             PIC X(06).
     05      W-AS-TRACENR        PIC 9(06).
     05      W-ABL               PIC 9(04).
     05      W-MDNR              PIC 9(08).
     05      W-TSNR              PIC 9(08).

     05      W-MEMLOG-KEY.
      10     W-TERMNR            PIC 9(08).
      10     W-TRACENR           PIC 9(06).
      10     W-NTYPE             PIC 9(04).
      10     W-BMP03             PIC 9(06).

     05      W-ABWKZ             PIC 9(02).
     05      W-BELEGNR           PIC 9(04).
     05      W-LTGIND            PIC 9(04).
     05      W-ASID              PIC 9(18).
     05      W-BETRAG            PIC 9(18).
     05      W-ERFASSUNGS-ART    PIC 9(02).
          88 W-ERF-MANUELL                   value 01.
          88 W-ERF-MAGNET                    value 02.
          88 W-ERF-CHIP                      value 05.
          88 W-ERF-KONTAKTLOS                value 07 91.

     05      W-TRANS-ART         PIC X(02).
     05      W-KANR              PIC X(19).
     05      W-WKZ               PIC 9(03).
     05      W-SPUR2             PIC X(40).
     05      W-BUFFER            PIC X(128).
     05      W-BUFFER-LEN        PIC S9(04) comp.

**          ---> Schl�sselfelder
 01          W-MACKEYA           PIC X(04) VALUE SPACES.
 01          W-PACKEYA           PIC X(08) VALUE LOW-VALUE.
 01          W-MACKEYT           PIC X(04) VALUE SPACES.
 01          W-PACKEYT           PIC X(04) VALUE SPACES.
 01          W-HERSTID           PIC X(02) VALUE SPACES.
 01          W-VERSION           PIC X(02) VALUE SPACES.
 01          W-PADDING           PIC X(08) VALUE LOW-VALUE.

**  ---> Liste der Inlineservices
 01          INLINE-SERVICES.
     05      W-ARTMAP            PIC X(16) VALUE "WXAMP07S-05".
     05      INLINE-SERVICE      PIC S9(04) COMP VALUE ZERO.
          88 USE-WXAMP                           VALUE 20.

 01          GEO-BUFFER.
     05      GEO-KZ-BREITE       PIC X.
     05      GEO-BREITE-8        PIC 9(08).
     05                          PIC X  VALUE SPACE.
     05      GEO-KZ-LAENGE       PIC X.
     05      GEO-LAENGE-9        PIC 9(09).
 01          GEO-FAKTOR          PIC S9(18) COMP VALUE 1000000.

**          ---> Bereiche f�r IFSF-BMP48
 01          W-BYTEMAP-48        PIC X(64) VALUE
     "0000000000000000000000000000000000000000000000000000000000000000".
 01          W-BITMAP            PIC X(08)  VALUE LOW-VALUES.
 01          W-BMP48-VAL         PIC X(128) VALUE SPACES.
 01          W-48-LEN            PIC 9(03)  VALUE ZERO.

**  ---> Zwischenfelder fuer Fregat
 01          W-FRE.
     05      W-FRE-TERMID        PIC X(16).
     05      W-FRE-MONNAME       PIC X(16).
     05      W-FRE-DATLEN        PIC S9(04) COMP.


*--------------------------------------------------------------------*
* Datm-Uhrzeitfelder (f�r TAL-Routine)
*--------------------------------------------------------------------*
 01          TAL-TIME.
     05      TAL-JHJJMMTT.
      10     TAL-JHJJ            PIC S9(04) COMP.
      10     TAL-MM              PIC S9(04) COMP.
      10     TAL-TT              PIC S9(04) COMP.
     05      TAL-HHMI.
      10     TAL-HH              PIC S9(04) COMP.
      10     TAL-MI              PIC S9(04) COMP.
     05      TAL-SS              PIC S9(04) COMP.
     05      TAL-HS              PIC S9(04) COMP.
     05      TAL-MS              PIC S9(04) COMP.

 01          TAL-TIME-D.
     05      TAL-JHJJMMTT.
        10   TAL-JHJJ            PIC  9(04).
        10   TAL-MM              PIC  9(02).
        10   TAL-TT              PIC  9(02).
     05      TAL-HHMI.
        10   TAL-HH              PIC  9(02).
        10   TAL-MI              PIC  9(02).
     05      TAL-SS              PIC  9(02).
     05      TAL-HS              PIC  9(02).
     05      TAL-MS              PIC  9(02).
 01          TAL-TIME-N REDEFINES TAL-TIME-D.
     05      TAL-TIME-N16        PIC  9(16).
     05      TAL-TIME-REST       PIC  9(02).

 01          TAL-JUL-DAY         PIC S9(09) COMP.

**          ---> Datum im DATETIME-Format
 01          TAGESDATUM.
     05      TD-JHJJ             PIC X(04).
     05                          PIC X   VALUE "-".
     05      TD-MM               PIC XX.
     05                          PIC X   VALUE "-".
     05      TD-TT               PIC XX.
     05                          PIC X   VALUE ":".
     05      TD-HH               PIC XX.
     05                          PIC X   VALUE ":".
     05      TD-MI               PIC XX.
     05                          PIC X   VALUE ":".
     05      TD-SS               PIC XX.
     05                          PIC X   VALUE ".".
     05      TD-HS               PIC XX.


*--------------------------------------------------------------------*
* Parameter f�r Untermodulaufrufe
*--------------------------------------------------------------------*
 01          PARAMETER-FELDER.
     05      P-DUMMY             PIC X(02).

**          ---> ChangeCode mit ASC2EBC und EBC2ASC
 01          P-CC-LEN            PIC S9(04) COMP.
 01          P-CC-IN             PIC X(64).
 01          P-CC-OUT            PIC X(64).

**          ---> f�r WT^HEX und WT^UNHEX Routinen
 01          P-HEX8              PIC X(08).
 01          P-HEX16             PIC X(16).

**          ---> f�r COBOL-Utilities GET-/PUT-STARTUPTEXT
**          --->                     GET-/PUT-PARAMTEXT
 01          STUP-PARAMETER.
     05      STUP-RESULT         PIC S9(04) COMP VALUE ZERO.
     05      STUP-CPLIST         PIC  9(09) COMP VALUE ZERO.
     05      STUP-PORTION        PIC  X(30) VALUE "STRING".
     05      STUP-TEXT           PIC X(128).

**          ---> Holen des eigenen Pathwaysystems
 01          PAIRINFO            PIC S9(04) COMP VALUE ZERO.
 01          FEHL                PIC S9(04) COMP VALUE ZERO.

**          ---> Holen des Prozessnamens
 01          PNAME               PIC  X(47).
 01          PNAMELEN            PIC S9(04) COMP.
 01          PROC-INFO           PIC  X(18) VALUE SPACE.


 EXTENDED-STORAGE SECTION.

*--------------------------------------------------------------------*
* weitere Buffer
*--------------------------------------------------------------------*
**          ---> Buffer f�r Log-Prozess
 01          MEMLOG-SATZ.
     05      MEM-TXILOG70        PIC X(3300).
     05      MEM-TXNLOG70-TS     PIC X(3300).
     05      MEM-TXNLOG70-AS     PIC X(3300).

**          ---> Parametertabelle f�r Autorisierungssystem
**          ---> wird im Programmvorlauf geladen, d.h. bei �nderungen
**          ---> muss das Programm (Serverklasse) neu gestartet werden

*kl20180404 - G.02.10 - Sieht gut aus, ist aber hier nicht angebracht
* 01          T-FCPARAM.
*     05      T-FCPARAM-TAB    occurs 200.
*     05      T-FCPARAM-TAB    occurs 1 to 200
*     05      T-FCPARAM-TAB    occurs 1 to 500
*                             depending on     T-MAX
*                             ascending key is T-KEY
*                             indexed by       IND-TAB.
*      10     T-KEY.
*       15    T-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
*       15    T-CARDID            PIC S9(04) COMP VALUE ZEROS.
*       15    T-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
*       15    T-KZ-MSG            PIC  X(02).
*       15    T-BMP               PIC S9(04) COMP VALUE ZEROS.
*       15    T-LFDNR             PIC S9(04) COMP VALUE ZEROS.
*      10     T-KZ-ABWEICHUNG     PIC  X(64).*
*
* 01          T-MAX               PIC S9(04) COMP VALUE ZEROS.
*
* 01          T-TAB-MAX           PIC S9(04) COMP VALUE 200.
* 01          T-TAB-MAX           PIC S9(04) COMP VALUE 500.
* 01          T-AKT-IND           PIC S9(04) COMP VALUE ZEROS.

*==> Zur�ck zur Standardtabellendefinition
 01          T-FCPARAM.
     05      T-FCPARAM-TAB    OCCURS 500.
       10    T-KEY.
         15  T-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
         15  T-CARDID            PIC S9(04) COMP VALUE ZEROS.
         15  T-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
         15  T-KZ-MSG            PIC X(02).
         15  T-BMP               PIC S9(04) COMP VALUE ZEROS.
         15  T-LFDNR             PIC S9(04) COMP VALUE ZEROS.
        10   T-KZ-ABWEICHUNG     PIC  X(64).

 01          T-MAX               PIC S9(04) COMP VALUE ZEROS.
 01          T-TAB-MAX           PIC S9(04) COMP VALUE 500.
 01          T-AKT-IND           PIC S9(04) COMP VALUE ZEROS.

**          ---> zu suchende Werte
 01          S-SEARCH-KEY.
     05      S-ROUTKZ            PIC S9(04) comp.
     05      S-CARDID            PIC S9(04) comp.
     05      S-ISONTYP           PIC S9(04) comp.
     05      S-KZ-MSG            PIC  X(02).
     05      S-BMP               PIC S9(04) comp.
     05      S-LFDNR             PIC S9(04) comp.

*kl20180404 - G.02.10 - Zus�tzlicher Searchkey f�r CARDID = 0
*                       (wird aus S-SEARCH-KEY gef�llt; dann wird
*                        ledglich S2-CARDID mit ZERO �berschrieben)
 01          S2-SEARCH-KEY.
     05      S2-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
     05      S2-CARDID            PIC S9(04) COMP VALUE ZEROS.
     05      S2-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
     05      S2-KZ-MSG            PIC  X(02).
     05      S2-BMP               PIC S9(04) COMP VALUE ZEROS.
     05      S2-LFDNR             PIC S9(04) COMP VALUE ZEROS.
*kl20180404 - G.02.10 - Ende

*G.03.01 - Tabelle auf 150 vergroessert
**          ---> AS-Keytabelle
 01          TK-KEYNAMEN.
     05      TK-KEYNAMEN-TABELLE occurs 150.
      10     TK-ROUTKZ           PIC S9(04).
      10     TK-CARDID           PIC S9(04).
      10     TK-KEYNAME          PIC X(08).
      10     TK-ISOGEN           PIC X(02).
      10     TK-ISOVERS          PIC X(02).
      10     TK-HEXKEY           PIC X(04).

 01          TK-MAX              PIC S9(04) COMP.
 01          TK-TAB-MAX          PIC S9(04) COMP VALUE 150.
*G.03.01 - Ende

**          ---> Mapping-Tabelle f�r 3-stellige/2-stellige AC's
 01          TAC-MAP-TABELLE.
     05      TAC-MAP                         OCCURS 1000.
*nicht n�tig, da = Index      10     TAC-IFSF            PIC 9(03).
      10     TAC-TERM            PIC S9(04) comp.

**          ---> Mapping ROUTKZ <-> APPL_KZ.IFSFAC
**          --->
**          ---> hier muss ggf. bei weiteren AS'sen erweitert werden
*G.03.01 - Refactoring fuer AS-Verfahren
 01          VERF-AS            PIC 9(02) VALUE ZEROS.
*G.03.01 - Ende
          88 VERF-AG                         VALUE 15.
          88 VERF-AV                         VALUE 05.
          88 VERF-BP                         VALUE 14.
          88 VERF-DK                         VALUE 12.
          88 VERF-EU                         VALUE 22.
          88 VERF-LO                         VALUE 23.
          88 VERF-NF                         VALUE 99.
          88 VERF-OR                         VALUE 16.
          88 VERF-SH                         VALUE 07.
          88 VERF-TN                         VALUE 18.
          88 VERF-TO                         VALUE 10.
          88 VERF-UT                         VALUE 17.
          88 VERF-IQ                         VALUE 24.
          88 VERF-RR                         VALUE 25.
*G.03.03 -  E100 neu
          88 VERF-E1                         VALUE 26.
*G.03.03 - Ende

          
**          ---> Verfahrensfestlegung AS-MAC-Berechnung
 01          AS-VERF             PIC X(02).
          88 AS-VERF-DK                      VALUE "DK".
          88 AS-VERF-EU                      VALUE "EU".
          88 AS-VERF-IQ                      VALUE "IQ".
          88 AS-VERF-LO                      VALUE "LO".
          88 AS-VERF-TO                      VALUE "TO".
          88 AS-VERF-UT                      VALUE "UT".
          88 AS-VERF-TN                      VALUE "TN".
          88 AS-VERF-RR                      VALUE "RR".
*G.03.03 -  E100 neu
          88 AS-VERF-E1                      VALUE "E1".
*G.03.03 - Ende
          88 AS-VERF-DEFAULT                 VALUE "SL".

**          --->
 01          W-TEILSTRING-TABELLE.
     05      W-TEILSTRING-TAB    occurs 10.
      10     W-TEILSTRING        PIC X(64).

 01          W-DELIM-TABELLE.
     05      W-DELIM-TAB         occurs 10.
      10     W-DELIM             PIC X(01).

 01          W-COUNT-TABELLE.
     05      W-COUNT-TAB         occurs 10.
      10     W-COUNT             PIC S9(04) comp.

**          ---> zum Metadaten dieses Prozesses
 01          MY-META.
   05        MYPROG.
    10       SYSTEM              PIC X(08).
    10       VOL                 PIC X(08).
    10       SUBVOL              PIC X(08).
    10       MODUL               PIC X(08).
   05        PROC-INFO           PIC X(18).
   05        ANCNAME             PIC X(10).
   05        MY-SRV-ID           PIC S9(04) COMP.
   05        MY-SRV-CLASS        PIC X(16)   VALUE " ".

**          ---> fuer Pathsend (serverclass_send_)
 01          PS-PATHNAME         PIC X(15).
 01          PS-PATHNAMELEN      PIC S9(04) COMP VALUE 15.
 01          PS-SRVCLASS         PIC X(15).
 01          PS-SRVCLASSLEN      PIC S9(04) COMP VALUE 15.
 01          PS-REQLEN           NATIVE-2.
 01          PS-MAXREPLEN        NATIVE-2.
 01          PS-AKTREPLEN        NATIVE-2.
 01          PS-TIMEOUT          PIC S9(04) COMP.
 01          PS-ERROR            PIC S9(04) COMP.

*           ---> fuer Pathsend (serverclass_send_info)
 01          SSI-ERROR           PIC S9(04) COMP.
 01          SSI-PSERROR         PIC S9(04) COMP.
 01          SSI-FSERROR         PIC S9(04) COMP.

*--------------------------------------------------------------------*
* Parameter f�r Untermodulaufrufe - COPY-Module
*--------------------------------------------------------------------*
 01          AS-INTERN-MESSAGE.
     COPY    INT-SCHNITTSTELLE-C OF  "=MSGLIB"
             REPLACING =="*"== BY ==AS==.

**          ---> fuer Artikeldatenmapper WXAMP011
 01          AMP-SCHNITTSTELLE  IS EXTERNAL.
     COPY    WXAMP01C OF "=MSGLIB"
             REPLACING =="*"== BY ==AMP==.

**          ---> fuer Fehlerbeh.
     COPY    WSYS022C OF "=MSGLIB".

**          ---> Schnittstelle zu SDBCDU5
     COPY    SDBCDU0C OF "=MSGLIB"
             REPLACING =="*"== BY ==SDB==.

**          ---> Schnittstelle zu Modul SYSABL1
     COPY    SYSABL1C OF "=MSGLIB"
             REPLACING =="*"== BY ==TABL==.

**          ---> Schnittstelle zu SYSMK7I
     COPY    SYSML7IC    OF "=MSGLIB"
             REPLACING =="*"== BY ==MEM==.

**          ---> Schnittstelle zu WCAPM92
     COPY    PCAPM01C    OF "=MSGLIB"
             REPLACING =="*"== BY ==PCAP==.

**          ---> Schnittstelle zu WISO207
     COPY    WISO207C OF "=MSGLIB"
             REPLACING =="*"== BY ==W207==.

**          ---> Schnittstelle zu WISO207 f�r TS-Nachricht
     COPY    WISO207C OF "=MSGLIB"
             REPLACING =="*"== BY ==W2TS==.

**          ---> Schnittstelle zu WISO207 f�r AS-Nachricht
     COPY    WISO207C OF "=MSGLIB"
             REPLACING =="*"== BY ==W2AS==.

*kl20170503 - G.02.00 - Fuer EMV-Verarbeitung
**          ---> fuer Zwischenschichtmodul WISO400 zum BER-TLV / KAAI-LTV
     COPY    WISO400C    OF "=MSGLIB"
             REPLACING =="*"== BY ==W400==.
*kl20170503 - G.02.00 - Ende

**          ---> Schnittstelle zu WSY7066
     COPY    WSY7066C OF "=MSGLIB"
             REPLACING =="*"== BY ==W66==.

**          ---> Schnittstelle zu WSYS971
     COPY    WSYS971C OF "=MSGLIB"
             REPLACING =="*"== BY ==CHK==.

**          ---> Schnittstelle zum Identifikationsmodul WSYS980
     COPY    WSYS980C   OF "=MSGLIB"
             REPLACING =="*"== BY ==ID==.

**          ---> Fuer Umsatz
     COPY    WUMSO07C OF "=MSGLIB"
             REPLACING =="*"== BY ==WUMS==.

**          ---> F�r Boxen-interface
     COPY    WEUR056C OF  "=MSGLIB"
             REPLACING =="*"== BY ==Z==.

******************************************************************
* Im Folgenden Vorkehrungen f�r SQL
******************************************************************

 EXEC SQL
     INCLUDE STRUCTURES ALL VERSION 315
 END-EXEC

 EXEC SQL
     INCLUDE SQLCA
 END-EXEC

 EXEC SQL
     BEGIN DECLARE SECTION
 END-EXEC

******************************************************************
* Im Folgenden zun�chst Host-Variable, die Bestandteil von
* SQL - Tabellen sind
******************************************************************
 01          HOST-VARIABLEN.
     05      H-DUMMY             PIC X(02).
     05      H-SYSKEY            PIC S9(18) COMP.
     05      H-ZP-IN             PIC X(22).
     05      H-ZP-OUT            PIC X(22).

******************************************************************
* Im Folgenden mit dem INVOKE-Befehl die Tabellenstruktur-
* definitonen der ben�tigten Tabellen einf�gen
******************************************************************
**  ---> Struktur der Tabelle FCPARAM
 EXEC SQL
    INVOKE =FCPARAM   AS FCPARAM
 END-EXEC

**  ---> Struktur der Tabelle IFSFAC
 EXEC SQL
    INVOKE =IFSFAC   AS IFSFAC
 END-EXEC

**  ---> Struktur der Tabelle KEYNAMEN
 EXEC SQL
    INVOKE =KEYNAMEN AS KEYNAMEN
 END-EXEC

**  ---> Struktur der Tabelle TXILOG70
 EXEC SQL
    INVOKE =TXILOG70 AS TXILOG70
 END-EXEC

**  --->  Transaktionslog Nachrichten der Station
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-TS
 END-EXEC

**  --->  Transaktionslog Nachrichten zum/vom AS
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-AS
 END-EXEC

**  --->  Umsatzliste
 EXEC SQL
    INVOKE =UMSWEAT  AS UMSWEAT
 END-EXEC

******************************************************************

 EXEC SQL
     END DECLARE SECTION
 END-EXEC

 EXEC SQL
     CONTROL TABLE * WAIT IF LOCKED
 END-EXEC.

 EXEC SQL
     CONTROL TABLE * OPEN ACCESSED PARTITIONS
 END-EXEC.


******************************************************************
* Im Folgenden werden die ben�etigten CURSOR auf die
* verschiedenen SQL - Tabellen definiert
******************************************************************
**  ---> Cursor auf Tabelle FCPARAM
 EXEC SQL
     DECLARE FCPARAM_CURS CURSOR FOR
         SELECT   ROUTKZ, CARDID, ISONTYP, KZ_MSG, BMP, LFDNR
                 ,KZ_ABWEICHUNG
           FROM  =FCPARAM
          WHERE   ROUTKZ = :ROUTKZ  of FCPARAM
            AND   APPKZ = "R7"
*kl20180404 - G.03.14 - wg. Prioriseirung CARDID=X vor CARDID=0
*          ORDER  BY ROUTKZ, CARDID, ISONTYP, BMP, LFDNR
          ORDER  BY ROUTKZ          ASC,
                    CARDID          DESC,
                    ISONTYP         ASC,
                    BMP             ASC,
                    LFDNR           ASC
*kl20180404 - G.03.14 - Ende
         BROWSE  ACCESS
 END-EXEC

**  ---> Cursor auf Tabelle KEYNAMEN
 EXEC SQL
     DECLARE KEYNAMEN_CURS CURSOR FOR
         SELECT   ROUTKZ, CARDID, KEYNAME, ISOGEN, ISOVERS
           FROM  =KEYNAMEN
*G.03.01 - alle laden
*          WHERE   ROUTKZ = :ROUTKZ of KEYNAMEN
         ORDER  BY ROUTKZ, CARDID
*G.03.01 - Ende
         BROWSE  ACCESS
 END-EXEC

**  ---> Cursor auf Tabelle IFSFAC
 EXEC SQL
     DECLARE IFSFAC_CURS CURSOR FOR
         SELECT   IFSF_AC, WEAT_AC
           FROM  =IFSFAC
          WHERE   APPL_KZ = :APPL-KZ of IFSFAC
         BROWSE   ACCESS
 END-EXEC

******************************************************************
* Ende der SQL - Definitionen
******************************************************************

 PROCEDURE DIVISION.

******************************************************************
* Die folgenden WHENEVER-Anweisungen legen Fehlerbehandlungen fest
******************************************************************
 A000-WHENEVER SECTION.
 A000-00.
     EXEC SQL
         WHENEVER SQLERROR       PERFORM :Z001-SQLERROR
     END-EXEC

     EXEC SQL
         WHENEVER SQLWARNING     PERFORM :Z001-SQLERROR
     END-EXEC
     .
 A000-99.
     EXIT.

******************************************************************
* Steuerungs-Section
******************************************************************
 A100-STEUERUNG SECTION.
 A100-00.
**  ---> wenn SWICH-15 gesetzt ist
**  ---> nur Umwandlungszeitpunkt zeigen und dann beenden
     IF  SHOW-VERSION
         DISPLAY K-MODUL " vom: " FUNCTION WHEN-COMPILED
         CALL "WSYS022" USING GEN-ERROR SQLCA
         STOP RUN
     END-IF

**  ---> Vorlauf: oeffnen Dateien etc.
     PERFORM B000-VORLAUF

**  ---> solange verarbeiten bis
**  ---> Datei $RECEIVE auf AT END kommt
     PERFORM UNTIL PRG-NOK
             OR    MSG-EOF

**      ---> holen momentanen Zeitpunkt
         PERFORM U200-TIMESTAMP
         MOVE TAGESDATUM TO H-ZP-IN

**      ---> Bearbeiten Nachricht
         EVALUATE IMSG-MONNAME (1:2)
             WHEN "AS"
**                      ---> Transaktion beginnen
                         PERFORM U100-BEGIN
**                      ---> Anfrage bearbeiten
                         PERFORM B100-VERARBEITUNG
**                      ---> Transaktion beenden
                         PERFORM U110-COMMIT
             WHEN OTHER
                         MOVE 1004 TO ERROR-NR of GEN-ERROR
                         STRING  IMSG-MONNAME (1:2)
                                 "@"
                                     delimited by size
                           INTO  DATEN-BUFFER1
                         END-STRING
                         PERFORM Z002-PROGERR
                         SET ENDE TO TRUE
         END-EVALUATE

**      ---> Schreiben Reply pathsend
         PERFORM S000-SCHREIBEN-REPLY
         IF  NOT MSG-EOF
**          ---> Initialisierung Felder
             PERFORM C000-INIT
**          ---> lesen Message-Datei
             PERFORM R000-LESEN-MESSAGE
         END-IF

     END-PERFORM

**  ---> Nachlauf: Dateien schiessen
     PERFORM B090-ENDE
     STOP RUN
     .
 A100-99.
     EXIT.

******************************************************************
* Vorlauf
******************************************************************
 B000-VORLAUF SECTION.
 B000-00.
**  ---> Initialisierung Felder
     PERFORM C000-INIT

*G.03.01 - neuen Parameter AS-VERF laden und nicht mehr AS-ROUTKZ
***  ---> holen Parameter AS-ROUTKZ
*     MOVE "AS-ROUTKZ" TO STUP-PORTION
*     PERFORM P950-GETPARAMTEXT
*     IF  PRG-ABBRUCH
*         EXIT SECTION
*     END-IF
     
**  ---> holen Parameter AS-VERF
     MOVE "AS-VERF" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF


**  ---> holen Parameter f�r zust�ndiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO VERF-AS
                                       ROUTKZ of FCPARAM
*                                       W-ROUTKZ
                                       S-ROUTKZ
***                                    ---> f�r Artikelmapper
*                                       VERF-ROUTKZ     
*G.03.01 - Ende        

**  ---> Anwendung setzen f�r MAC-Berechnung
     EVALUATE TRUE
         WHEN VERF-DK    SET AS-VERF-DK TO TRUE
         WHEN VERF-EU    SET AS-VERF-EU TO TRUE
         WHEN VERF-IQ    SET AS-VERF-DK TO TRUE
         WHEN VERF-LO    SET AS-VERF-LO TO TRUE
         WHEN VERF-TO    SET AS-VERF-TO TO TRUE
         WHEN VERF-TN    SET AS-VERF-TN TO TRUE
         WHEN VERF-RR    SET AS-VERF-RR TO TRUE
*G.03.03 -  E100 neu
         WHEN VERF-E1    SET AS-VERF-E1 TO TRUE
*G.03.03 - Ende
         WHEN OTHER      SET AS-VERF-DEFAULT TO TRUE

     END-EVALUATE

**  ---> interne Tabelle initialisieren
*kl20180109 - G.02.06 - Initialisieren mit T-TAB-MAX
*                       statt Fixwert 200/500
     MOVE   T-TAB-MAX       TO T-MAX
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   C4-I1 > T-TAB-MAX
         INITIALIZE T-KEY (C4-I1)
     END-PERFORM

**  ---> ersten Eintrag holen
     PERFORM S900-OPEN-FCPARAM-CURSOR
     PERFORM S910-FETCH-FCPARAM-CURSOR

**  ---> Schleife �ber alle Eintr�ge f�r das ROUTKZ
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   FCPARAM-EOD
             OR      FCPARAM-NOK
             OR      C9-COUNT > T-TAB-MAX

         MOVE ROUTKZ        of FCPARAM TO T-ROUTKZ        (C4-I1)
         MOVE CARDID        of FCPARAM TO T-CARDID        (C4-I1)
         MOVE ISONTYP       of FCPARAM TO T-ISONTYP       (C4-I1)
         MOVE KZ-MSG        of FCPARAM TO T-KZ-MSG        (C4-I1)
         MOVE BMP           of FCPARAM TO T-BMP           (C4-I1)
         MOVE LFDNR         of FCPARAM TO T-LFDNR         (C4-I1)
         MOVE KZ-ABWEICHUNG of FCPARAM TO T-KZ-ABWEICHUNG (C4-I1)

**      ---> n�chsten Eintrag holen
         PERFORM S910-FETCH-FCPARAM-CURSOR

     END-PERFORM

**  ---> schliessen Cursor
     PERFORM S920-CLOSE-FCPARAM-CURSOR
     MOVE C9-COUNT TO T-MAX

**  ---> Fehlerkonstellationen
     IF  FCPARAM-NOK
         MOVE "Fehler beim Laden der Tabelle =FCPARAM" TO
         DATEN-BUFFER1 MOVE "Programm-Abbruch" TO  DATEN-BUFFER2
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF
*kl20180105 - G.02.05 - T-TAB-MAX enthaelt maximal zulaessige Anzahl
*     IF  C9-COUNT > T-MAX
     IF  C9-COUNT > T-TAB-MAX
*kl20180105 - G.02.05 - Ende
         MOVE "Programm interne Tabelle zu klein" TO  DATEN-BUFFER1
         MOVE "mehr als 500 Eintr�ge in =FCPARAM " TO  DATEN-BUFFER2
         MOVE ROUTKZ OF FCPARAM TO D-NUM4
         STRING  "f�r ROUTKZ = "     delimited by size
                 D-NUM4              delimited by size
           INTO  DATEN-BUFFER3
         END-STRING
         MOVE "Programm-Abbruch" TO  DATEN-BUFFER4
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

*G.03.01 - zus�tzlich AIID mit in diese Tabelle laden + alle Eintraege aus KEYNAMEN
**  ---> AS Schl�ssel MACKEYA und PACKEYA aus Tabelle =KEYNAMEN einlesen
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> !!!! zun�chstmal wird nur der erste eingelesen !!!!
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> ersten Eintrag holen
*     MOVE W-ROUTKZ TO ROUTKZ of KEYNAMEN
     PERFORM S930-OPEN-KEYNAMEN-CURSOR
     PERFORM S940-FETCH-KEYNAMEN-CURSOR

**  ---> Schleife �ber alle Eintr�ge f�r das ROUTKZ
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   KEYNAMEN-EOD
             or      KEYNAMEN-NOK
             or      C9-COUNT > TK-TAB-MAX
*             or      C4-I1 > 1

         MOVE ROUTKZ  of KEYNAMEN TO TK-ROUTKZ  (C4-I1)
         MOVE CARDID  of KEYNAMEN TO TK-CARDID  (C4-I1)
         MOVE KEYNAME of KEYNAMEN TO TK-KEYNAME (C4-I1)
         MOVE ISOGEN  of KEYNAMEN TO TK-ISOGEN  (C4-I1)
         MOVE ISOVERS of KEYNAMEN TO TK-ISOVERS (C4-I1)
**      ---> Keynamen hexen
         MOVE TK-KEYNAME (C4-I1) TO P-HEX16
         PERFORM P900-WTHEX
         MOVE P-HEX8 TO TK-HEXKEY (C4-I1)

**      ---> n�chsten Eintrag holen
         PERFORM S940-FETCH-KEYNAMEN-CURSOR

     END-PERFORM

**  ---> schliessen Cursor
     PERFORM S950-CLOSE-KEYNAMEN-CURSOR
     MOVE C9-COUNT TO TK-MAX
     IF  TK-MAX = 1
         MOVE TK-HEXKEY (1) TO W-MACKEYA
         MOVE TK-HEXKEY (1) TO W-PACKEYA (1:4)
     END-IF
*G.03.01 - Ende

**  ---> bestimmen Verfahren f�r AC-Mapping
     EVALUATE TRUE

         WHEN VERF-AG    MOVE "AG" TO APPL-KZ of IFSFAC
         WHEN VERF-AV    MOVE "AV" TO APPL-KZ of IFSFAC
         WHEN VERF-BP    MOVE "BP" TO APPL-KZ of IFSFAC
         WHEN VERF-DK    MOVE "DK" TO APPL-KZ of IFSFAC
         WHEN VERF-EU    MOVE "EU" TO APPL-KZ of IFSFAC
         WHEN VERF-IQ    MOVE "IQ" TO APPL-KZ of IFSFAC
         WHEN VERF-LO    MOVE "LO" TO APPL-KZ of IFSFAC
         WHEN VERF-OR    MOVE "OR" TO APPL-KZ of IFSFAC
         WHEN VERF-SH    MOVE "SH" TO APPL-KZ of IFSFAC
         WHEN VERF-TN    MOVE "TN" TO APPL-KZ of IFSFAC
         WHEN VERF-TO    MOVE "TO" TO APPL-KZ of IFSFAC
         WHEN VERF-UT    MOVE "UT" TO APPL-KZ of IFSFAC
         WHEN VERF-RR    MOVE "RR" TO APPL-KZ of IFSFAC         
*G.03.03 -  E100 neu
         WHEN VERF-E1    MOVE "E1" TO APPL-KZ of IFSFAC
*G.03.03 - Ende
         WHEN OTHER      MOVE "Verfahren f�r AC-Mapping kann nicht bestimmt werden"
                             TO DATEN-BUFFER1
                         MOVE "Programm wird beendet" TO DATEN-BUFFER2
                         SET ENDE TO TRUE
                         PERFORM Z002-PROGERR
                         EXIT SECTION

     END-EVALUATE

**  ---> Terminal AC's initialisieren (alle negativ)
     MOVE HIGH-VALUE TO TAC-MAP-TABELLE

**  ---> laden der AC-Mapping-Tabelle
     PERFORM S960-OPEN-IFSFAC-CURSOR
     PERFORM S970-FETCH-IFSFAC-CURSOR

**  ---> Schleife �ber alle Eintr�ge des Verfahrens mit APPL_KZ
**  ---> gesetzt werden AC's mit Index IFSF-AC alle anderen sind negativ
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   C4-I1 > 100
             OR      IFSFAC-NOK

         IF  IFSF-AC of IFSFAC not = ZERO
             MOVE WEAT-AC of IFSFAC TO TAC-TERM (IFSF-AC of IFSFAC)
         END-IF

**      ---> lesen n�chsten Eintrag
         PERFORM S970-FETCH-IFSFAC-CURSOR

     END-PERFORM

     PERFORM S980-CLOSE-IFSFAC-CURSOR

**  ---> Holen Terminal-Schl�sselnamen
**     > Ungepackten MAC(Terminal)-Schl�sselID holen
     MOVE "MACKEYT" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     MOVE STUP-TEXT(1:STUP-RESULT) TO P-HEX16
     PERFORM P900-WTHEX
     MOVE P-HEX8 TO W-MACKEYT

**  ---> holen Boxen-Monitor
     MOVE "BOXMON" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     MOVE STUP-TEXT(1:STUP-RESULT) TO W66-BOXMON-TEXT

**  --->  Artikelmapper bestimmen
     MOVE "ARTMAP" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     MOVE STUP-TEXT(1:STUP-RESULT) TO W-ARTMAP

**  ---> holen Prozess- und ANCNAME
     PERFORM P960-GET-PROC-ANCNAME

**  ---> wenn bis hier alles OK war, dann lesen MSG-Datei
     IF  PRG-OK
**      ---> oeffnen $RECEIVE
         OPEN INPUT  MESSAGE-DATEI
              OUTPUT REPLY-DATEI

**      ---> 1. Lesen Message-Datei
         PERFORM R000-LESEN-MESSAGE
     END-IF
     .
 B000-99.
     EXIT.

******************************************************************
* Ende
******************************************************************
 B090-ENDE SECTION.
 B090-00.
     continue
*** =>
*** => weitere Verarbeitung hier einf�gen
*** =>

**  ---> schliessen $RECEIVE
     IF  PRG-OK
         CLOSE MESSAGE-DATEI
     END-IF
     .
 B090-99.
     EXIT.


******************************************************************
* Verarbeitung
******************************************************************
 B100-VERARBEITUNG SECTION.
 B100-00.
**  --> Fregat-Angaben merken
     MOVE IMSG-TERMID  TO W-FRE-TERMID
     MOVE IMSG-MONNAME TO W-FRE-MONNAME
     MOVE IMSG-DATLEN  TO W-FRE-DATLEN

**  ---> erstmal die Nachrichten aus dem Hinweg aus dem MEMLOG holen
     PERFORM C050-GET-MEMLOG
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> Kontrolle der Antwort <---
**  ---> ist die Antwort evtl. OK?
     PERFORM C100-ANTWORT-CHECK

     IF ENDE
        CONTINUE
     ELSE
**   ---> f�r alle AS'sen g�ltige Transaktions Regeln
      PERFORM C200-AS-GENERELL
     END-IF

     IF ENDE
        CONTINUE
     ELSE
**   ---> spezielle Regeln f�r AS'sen
      PERFORM C300-AS-SPEZIELL
     END-IF

     IF ENDE
        CONTINUE
     ELSE

**  ---> BMP 39 - Antwortcode
       MOVE 39    TO W207-XBMP
       MOVE 02    TO W207-XCOBLEN
       MOVE W-AC  TO W207-XCOBVAL

       IF  W-AC = ZEROS
**      ---> zun�chst pr�fen, ob ein spez. AC gesendet werden soll
           PERFORM M120-CALL-SYSABL1
           IF TABL-ABL
              MOVE TABL-AC TO W207-XCOBVAL
           END-IF
       END-IF

     PERFORM L100-ADD-BMP
     IF ENDE
        CONTINUE
     ELSE
**   ---> und hier die TS-Nachricht zusammenbauen
      PERFORM C400-BUILD-TS-NACHRICHT
     END-IF

**  ---> Schreiben der Log-Daten
     PERFORM C500-LOGGING

**  ---> FREGAT-Parameter setzen
     SET  IMSG-WRITE-SL TO TRUE
     MOVE IMSG-MONNAME  TO IMSG-NEXTSERV
     .
 B100-99.
     EXIT.

******************************************************************
* Initialisierung von Feldern und Strukturen
******************************************************************
 C000-INIT SECTION.
 C000-00.
     INITIALIZE SCHALTER
                GEN-ERROR
                TXILOG70
                TXNLOG70-TS
                TXNLOG70-AS
                UMSWEAT
                WORK-INIT

     MOVE ZERO TO W-AC

*** =>
*** => weitere Verarbeitung hier einf�gen
*** =>
     .
 C000-99.
     EXIT.
*kl20170505 - G.02.00 - Das muss mal schoen gemacht werden
******************************************************************
* Holen Daten aus MEMLOG
******************************************************************
 C050-GET-MEMLOG SECTION.
 C050-00.

**  ---> Vorbereiten Modulaufruf
     SET MEM-READ TO TRUE
*    timeout Parameter setzen (bei -1 Verfahrensdefault "C")
     MOVE -1      TO MEM-APP-TIMEOUT
*    Verfahren mit langen Buffern
     MOVE "B"     TO MEM-VERFKZ
*    Key intialisieren
     MOVE SPACES  TO MEM-LOGKEY

**  ---> Arbeitsfelder intialisieren, die hier bef�llt werden sollen
     MOVE ZEROES TO W-MDNR,
                    W-TSNR,
                    W-TERMNR,
                    W-TRACENR,
                    W-NTYPE,
                    W-ABWKZ,
                    W-BELEGNR,
                    W-CARDID

**  ---> Pruefen, ob BMP 59 voehanden (enthaelt MEMLOG-KEY)
     IF  IMSG-TBMP(59) = 1
     AND IMSG-TPTR(59) > 0
     AND IMSG-TLEN(59) > 0
*        BMP 59 gueltig, Alles ok
         CONTINUE
     ELSE
*       BMP 59 fehlt - Lesen MEMLOG nicht moeglich
         INITIALIZE GEN-ERROR
         STRING "Kein BMP 59 von AS: ",
                IMSG-MONNAME
         DELIMITED BY SIZE INTO DATEN-BUFFER1
         MOVE "Lesen MEMLOG und Weiterverarbeitung nicht moeglich"
              TO DATEN-BUFFER2
         PERFORM Z002-PROGERR
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF

**  ---> MEMLOG Key aus BMP 59 �bernehmen
     MOVE IMSG-CF(IMSG-TPTR(59):IMSG-TLEN(59))   TO MEM-LOGKEY
     MOVE IMSG-MONNAME                           TO MEM-ASNAME

     if  trace-on
         move "Test # - C050-GET-MEMLOG - MEMLOG-Logkey:" to daten-buffer1
         move mem-logkey                                  to daten-buffer2
         string  "AS-Name: "
                 mem-asname
                     delimited by size
           into  daten-buffer3
         end-string
         perform z002-progerr
     end-if

**  ---> MEMLOG Aufruf
     PERFORM M110-CALL-SYSMK7I
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> Daten aus MEMLOG in Tabellenstruktur verteilen
     MOVE MEM-TXILOG70    TO TXILOG70
     MOVE MEM-TXNLOG70-TS TO TXNLOG70-TS
     MOVE MEM-TXNLOG70-AS TO TXNLOG70-AS

**  ---> Daten f�r Weiterverwendung bereitstellen
     MOVE MDNR      of TXILOG70    TO W-MDNR
     MOVE TSNR      of TXILOG70    TO W-TSNR
     MOVE CARDID    of TXILOG70    TO W-CARDID
     
*G.03.01 - ROUTKZ aus TXILOG70
     MOVE ROUTKZ    of TXILOG70    TO W-ROUTKZ   
*G.03.01 - Ende

*    Terminal-Nr. bereitstellen ...
     MOVE TERMNR    of TXILOG70    TO W-TERMNR,
                                      P-HEX16
*    ... und packen fuer ERRLOG
     PERFORM P900-WTHEX
     MOVE P-HEX8(1:4) TO TERMID of APPL-SPEC-BUF

*    Tracenr und Nachrichtentyp
     MOVE TRACENR OF TXILOG70       TO W-TRACENR
     MOVE ISONTYP OF TXILOG70       TO W-NTYPE

*    TS-Anfrage aufbereiten f�r Spiegelfelder
     MOVE VAL of ANFRAGE of TXNLOG70-TS(1:LEN of ANFRAGE of TXNLOG70-TS)
                                        TO W207-ISOSTRING
     MOVE LEN of ANFRAGE of TXNLOG70-TS TO W207-ISOLEN
     SET  W207-EC TO TRUE
     PERFORM L120-ISO2COB
     IF  ENDE
         EXIT SECTION
     END-IF

**  +++> und Daten in TS-Struktur bereitstellen
     MOVE W207-WISO207C TO W2TS-WISO207C

**  ---> Abwicklungskennzeichen und Belegnr bereitstellen (u.a. fuer UMSWEAT)
*    Terminalnachricht wird mit offset / AS-Antwort adressiert. Das geht dann
*    schief, wenn Handeingabe, da BMP 2 VOR BMP 3 kommt, in der TS-Anfrage aber
*    BMP 3 immer an erster Stelle (Handeingabe durch verk�rztes BMP 35!)
*    MOVE W2TS-CF(IMSG-TPTR(03):2)        TO W-ABWKZ
*    MOVE W2TS-CF(IMSG-TPTR(03) + 2:4)    TO W-BELEGNR
*    Adressierung mit "passendem" Offset!
     MOVE W2TS-CF(W2TS-TPTR(03):2)        TO W-ABWKZ
     MOVE W2TS-CF(W2TS-TPTR(03) + 2:4)    TO W-BELEGNR

**  ---> AS-Anfrage aufbereiten f�r Pr�fungen
     MOVE VAL of ANFRAGE of TXNLOG70-AS(1:LEN of ANFRAGE of TXNLOG70-AS)
                                        TO W207-ISOSTRING
     MOVE LEN of ANFRAGE of TXNLOG70-AS TO W207-ISOLEN
     SET  W207-IFSF TO TRUE
     PERFORM L120-ISO2COB
     IF  ENDE
         EXIT SECTION
     END-IF
**  +++> und Daten in AS-Struktur bereitstellen
     MOVE W207-WISO207C TO W2AS-WISO207C
     .
 C050-99.
     EXIT.

******************************************************************
* Kontrolle Eingangsnachricht
******************************************************************
 C100-ANTWORT-CHECK SECTION.
 C100-00.
**  ---> interne Schnittstelle sichern
     MOVE INTERN-MESSAGE TO AS-INTERN-MESSAGE

**  ---> erstmal TERMID f�r ERRLOG aufbereiten
     MOVE W-TERMNR TO P-HEX16
     PERFORM P900-WTHEX
     MOVE P-HEX8(1:4) TO TERMID of APPL-SPEC-BUF

**  ---> formale Pr�fung der Nachricht
     MOVE ZERO TO W-AC
     SET  CHK-CHECK-ALL TO TRUE
     MOVE IMSG-NTYPE    TO CHK-NTYPE
                           W-NTYPE
     MOVE "A7"          TO CHK-ABWKZ(1:2)
     MOVE K-MODUL (2:4) TO CHK-ABWKZ(3:4)
**  ---> soll dann "1210" "A7FCAU" sein

**  ---> formale Pr�fung durch Modul WSYS971
     PERFORM M160-CALL-WSYS971
     IF  ENDE or W-AC > ZERO
         EXIT SECTION
     END-IF

**  ---> weitere Pr�fungen
**  ---> diverse Felder m�ssen gespiegelt werden
**       es wird immer AC=30 gesetzt, aber die Transaktion beantwortet
**  ---> BMP 3 - Abwicklungskennzeichen
     IF  IMSG-CF(IMSG-TPTR(3):IMSG-TLEN(3))
         not = W2AS-CF(W2AS-TPTR(3):W2AS-TLEN(3))
         MOVE "BMP 3 in AS-Antwort <> AS-Anfrage"  TO DATEN-BUFFER1
         MOVE "Transaktion wird mit AC 81 beantwortet" TO DATEN-BUFFER2
         MOVE 81 TO W-AC
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF

**  ---> hier schon mal den AC holen
     MOVE IMSG-CF(IMSG-TPTR(39):IMSG-TLEN(39)) TO W-AC-AS

**  ---> BMP 4 - Betrag
     IF  IMSG-TBMP(4) = 1
         MOVE IMSG-CF(IMSG-TPTR(04):IMSG-TLEN(04)) TO W-BETRAG
         COMPUTE W18-BETRAG = W-BETRAG / 100
         EVALUATE W-AC-AS
**          ---> wenn AC=0, muss gleich sein
             WHEN ZERO
               IF    IMSG-CF(IMSG-TPTR(4):IMSG-TLEN(4))
               NOT = W2AS-CF(W2AS-TPTR(4):W2AS-TLEN(4))
                   MOVE "BMP 4 in AS-Antwort <> AS-Anfrage"
                     TO DATEN-BUFFER1
                   MOVE "Transaktion mit AC 81 beantwortet"
                     TO DATEN-BUFFER2
                   MOVE 81 TO W-AC
                   PERFORM Z002-PROGERR
                   EXIT SECTION
                END-IF
**          ---> bei Teilgenehmigung, muss kleiner gleich oder null sein
             WHEN 2
               CONTINUE
**          ---> bei Ablehnung, muss 0 sein
             WHEN OTHER
               IF  IMSG-CF(IMSG-TPTR(4):IMSG-TLEN(4)) not = ZERO
                   MOVE "BMP 4 bei Ablehnung gr��er Null"
                     TO DATEN-BUFFER1
                   MOVE "Transaktion wird mit AC 81 beantwortet"
                     TO DATEN-BUFFER2
                   MOVE 81 TO W-AC
                   PERFORM Z002-PROGERR
                   EXIT SECTION
               END-IF
         END-EVALUATE
     END-IF

**  ---> BMP 11 - Tracenummer  muss gleich sein , sonst Problem mit MEMLOG
**  ---> BMP 12 - Lokalzeit / Datum
     IF  IMSG-CF(IMSG-TPTR(12):IMSG-TLEN(12))
         not = W2AS-CF(W2AS-TPTR(12):W2AS-TLEN(12))
         MOVE "BMP 12 in AS-Antwort <> AS-Anfrage"
           TO DATEN-BUFFER1
         MOVE "Transaktion wird mit AC 81 beantwortet"
           TO DATEN-BUFFER2
         MOVE 81 TO W-AC
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF

**  ---> BMP 30 - Ersatzbetr�ge
     EVALUATE W-AC-AS
**      ---> wenn AC=0, darf nicht da sein
         WHEN ZERO
                 IF  IMSG-TBMP(30) = 1
                     MOVE "Genehmigung mit BMP 30            "
                       TO DATEN-BUFFER1
                     MOVE "Transaktion wird mit AC 81 beantwortet"
                       TO DATEN-BUFFER2
                     MOVE 81 TO W-AC
                     PERFORM Z002-PROGERR
                     EXIT SECTION
                 END-IF
**      ---> bei Teilgenehmigung, muss vorhanden sein
         WHEN 2
                 IF  IMSG-TBMP(30) = 0
                     MOVE "Teilgenehmigung/Ablehnung ohne BMP 30"
                       TO DATEN-BUFFER1
                     MOVE "Transaktion wird mit AC 81 beantwortet"
                       TO DATEN-BUFFER2
                     MOVE 81 TO W-AC
                     PERFORM Z002-PROGERR
                     EXIT SECTION
                 END-IF
**      ---> bei Ablehnung, muss = bmp4 aus Anfrage sein
         WHEN OTHER
           IF IMSG-TBMP(30) = 0
             IF IMSG-TBMP(4) = 1
                MOVE "Ablehnung vom AS ohne BMP 30"
                  TO DATEN-BUFFER1
                MOVE "Transaktion wird mit AC=81 beantwortet"
                  TO DATEN-BUFFER2
                MOVE 81 TO W-AC
                PERFORM Z002-PROGERR
                EXIT SECTION
             ELSE
                MOVE "Ablehnung ohne BMP 04 / 30"
                  TO DATEN-BUFFER1
                MOVE 81 TO W-AC
                PERFORM Z002-PROGERR
                EXIT SECTION
             END-IF
           ELSE
**---> einige AS'sen, stellen den Wert links-andere rechtsb�ndig ein
             IF IMSG-TBMP(4) = 1
                IF IMSG-CF(IMSG-TPTR(30) + 12:12)
                 = W2AS-CF(W2AS-TPTR(4):W2AS-TLEN(4))
                OR  IMSG-CF(IMSG-TPTR(30)     :12)
                 = W2AS-CF(W2AS-TPTR(4):W2AS-TLEN(4))
                   CONTINUE
                ELSE
                  MOVE "BMP30 in AS-Antwort <> BMP4 AS-Anfrage"
                    TO DATEN-BUFFER1
                  MOVE "Transaktion wird mit AC=81 beantwortet"
                    TO DATEN-BUFFER2
                  MOVE 81 TO W-AC
                  PERFORM Z002-PROGERR
                  EXIT SECTION
                END-IF
             END-IF
           END-IF
     END-EVALUATE


*kl20171212 - G.02.03 - Pr�fung nicht bei WAS2 wg. ausl. Karten
     IF W-ROUTKZ = 5
        CONTINUE
     ELSE
**  ---> BMP 32 - Kennung Netzbetreiber (AIID)
        IF  IMSG-CF(IMSG-TPTR(32):IMSG-TLEN(32))
            not = W2AS-CF(W2AS-TPTR(32):W2AS-TLEN(32))
            MOVE "BMP 32 in AS-Antwort <> AS-Anfrage" TO DATEN-BUFFER1
            MOVE "Transaktion wird mit AC = 81 beantwortet" TO DATEN-BUFFER2
            MOVE 81 TO W-AC
            PERFORM Z002-PROGERR
            EXIT SECTION
        END-IF
     END-IF

**  ---> BMP 38 - Autorisierungsmerkmal merken
     IF  IMSG-TBMP(38) = 1
         MOVE IMSG-CF(IMSG-TPTR(38):IMSG-TLEN(38)) TO W-GENNR
     END-IF

**  ---> BMP 41 - Terminal-Nummer, wenn in Anfrage vorhanden
     IF  W2AS-TBMP(41) = 1
         IF  IMSG-CF(IMSG-TPTR(41):IMSG-TLEN(41))
             not = W2AS-CF(W2AS-TPTR(41):W2AS-TLEN(41))
             MOVE "BMP 41 in AS-Antwort <> AS-Anfrage" TO DATEN-BUFFER1
             MOVE "Transaktion wird mit AC 81 beantwortet" TO DATEN-BUFFER2
             MOVE 81 TO W-AC
             PERFORM Z002-PROGERR
             EXIT SECTION
         END-IF
     END-IF

**  ---> BMP 49 - W�hrungscode
     IF  IMSG-CF(IMSG-TPTR(49):IMSG-TLEN(49))
         not = W2AS-CF(W2AS-TPTR(49):W2AS-TLEN(49))
         MOVE "BMP 49 in AS-Antwort <> AS-Anfrage" TO DATEN-BUFFER1
         MOVE "Transaktion wird AC 81 beantwortet" TO DATEN-BUFFER2
         MOVE 81 TO W-AC
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF

*kl20170505 - G.02.00 - Gueltiger EMV-Nachrichtenfluss
*        TX genehmigt vom AS
     IF  (W-AC-AS = ZERO OR W-AC-AS = 2)
*        Chipdaten vom TS (EMV-TX)
     AND W2TS-TBMP(55) = 1
*        Chipdaten vom AS gekommen?
         IF  W2AS-TBMP(55) = 1
         AND W2AS-TPTR(55) > ZERO
         AND W2AS-TLEN(55) > ZERO
*            Alles gut
             CONTINUE
         ELSE
*            Genehmigte EMV-TRansaktion, aber KEINE Chipdaten vom AS
             INITIALIZE GEN-ERROR
             MOVE "Keine Chipdaten vom AS bei genehmigter EMV-TX"
                   TO DATEN-BUFFER1
*                Ausgabe Transaktionsschl�ssel nur, wenn vorhanden
             IF  W2TS-TBMP(41) = 1
             AND W2TS-TPTR(41) > 0
             AND W2TS-TLEN(41) > 0
             AND W2TS-TBMP(11) = 1
             AND W2TS-TPTR(11) > 0
             AND W2TS-TLEN(11) > 0
                 STRING "Bei TERMNR ",
                        W2TS-CF(W2TS-TPTR(41):W2TS-TLEN(41)),
                        " / TRACENR ",
                        W2TS-CF(W2TS-TPTR(11):W2TS-TLEN(11))
                  DELIMITED BY SIZE INTO DATEN-BUFFER2
              ELSE
*                Sonst Fragezeichen
                  STRING "Bei TERMNR ",
                         "???",
                         " / TRACENR ",
                         "???",
                         " (nicht in TS-Anfrage)"
                  DELIMITED BY SIZE INTO DATEN-BUFFER2
              END-IF
              MOVE "TS-Antwort mit AC 81"   TO DATEN-BUFFER3
              PERFORM Z002-PROGERR
              MOVE 81 TO W-AC
              EXIT SECTION
         END-IF
     END-IF

**  ---> BMP 59 - Tracenummer Transportdaten muss gleich, sonst Problem mit MEMLOG
**  ---> BMP 62 - Produktdaten enthalten? (nur bei AC=185)

**  ---> Antwortcode von IFSF auf WEAT umsetzen
     IF  W-AC-AS = ZERO
         MOVE ZERO TO W-AC
     ELSE
         IF  TAC-TERM (W-AC-AS) < ZERO
             MOVE "Fehler bei Antwortcode-Mapping" TO DATEN-BUFFER1
             STRING  "f�r IFSF-AC: "
                     W-AC-AS
                     " kein WEAT-AC gefunden"
                         delimited by size
               INTO  DATEN-BUFFER2
             END-STRING
             MOVE "WEAT-AC 96 gesetzt" TO DATEN-BUFFER3
             MOVE 96 TO W-AC
             PERFORM Z002-PROGERR
         ELSE
             MOVE TAC-TERM (W-AC-AS) TO W-AC
         END-IF
     END-IF

     IF W-AC > ZEROS
        EXIT SECTION
     ELSE

**  ---> MAC pr�fen, sofern vorhanden
      SET MAC-NO TO TRUE

      IF  IMSG-TBMP(64) = 1
*G.03.01 - AS-VERF hier verwenden und Key bestimmen
         PERFORM D200-FIX-KEY
*        EVALUATE W-ROUTKZ
         EVALUATE VERF-AS
*G.03.01 - Ende
            WHEN 22
                 PERFORM F915-ASMAC-DUKPT
         WHEN OTHER
                 SET MAC-YES  TO TRUE
                 MOVE AS-VERF TO W66-ANWENDUNG
                 SET W66-IFSF TO TRUE
                 PERFORM F910-MAC-PRUEFEN
         END-EVALUATE

         IF TRACE-ON
            MOVE "Test # - Ergebnis AS-MAC pr�fen - AC:"
              TO DATEN-BUFFER1
            MOVE W-AC
              TO DATEN-BUFFER2
            PERFORM Z002-PROGERR
          END-IF

          IF  W-AC > ZERO
              MOVE "AS-MAC-Pr�fung nicht ok"
                TO DATEN-BUFFER1
              MOVE "Transaktion wird AC 96 beantwortet"
                TO DATEN-BUFFER2
              MOVE 96 TO W-AC
              PERFORM Z002-PROGERR
              EXIT SECTION
          END-IF
      END-IF
     END-IF

     .
 C100-99.
      EXIT.

******************************************************************
* TS-Antwortteile, die bei Antworten von alle AS'sen gleich sind
******************************************************************
 C200-AS-GENERELL SECTION.
 C200-00.
**  ---> im Folgenden die Terminalanfrage als Basis nehmen
     MOVE W2TS-COBDATEN TO W207-COBDATEN

**  ---> zun�chst die BitMap auf die Pflichtfelder setzen
     MOVE LOW-VALUE      TO W207-TBMP-O
     MOVE K-BYTEMAP-A210 TO W207-TBMP-O (1:64)

     SET W207-EC TO TRUE
**  ---> Nachrichtentyp f�r Nachricht setzen
     MOVE 210 TO W207-NTYPE

**  ---> BMP  3 - AbWkz bleibt
**  ---> BMP  4 - Betrag bleibt 
**  ---> BMP 11 - Tracenummer �bernehmen
**  ---> BMP 12 - Lokalzeit   �bernehmen
**  ---> BMP 13 - Lokaldatum  �bernehmen

**BMP 33 - ASID aus AS-Antw. BMP32, maximal 6 Stellen

     MOVE 33 TO W207-XBMP

*G.02.09 - Anfang
*    MOVE 06 TO W207-XCOBLEN
*    MOVE IMSG-CF(IMSG-TPTR(32):6) TO W207-XCOBVAL
*                                     W-ASID
*G.03.04 - muss nicht auf 6 begrenzt werden
*     IF IMSG-TLEN(32) > 6
*        MOVE 06 TO W207-XCOBLEN
*        MOVE IMSG-CF(IMSG-TPTR(32):6) TO W207-XCOBVAL
*        MOVE IMSG-CF(IMSG-TPTR(32):6) TO W-ASID
*     ELSE
        MOVE IMSG-TLEN(32)
          TO W207-XCOBLEN
        MOVE IMSG-CF(IMSG-TPTR(32):IMSG-TLEN(32))
          TO W207-XCOBVAL
        MOVE IMSG-CF(IMSG-TPTR(32):IMSG-TLEN(32))
          TO W-ASID
*     END-IF
*G.03.04 - Ende
*G.02.09 - Ende

     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 41 - Terminalnummer �bernehmen

**  ---> BMP 42 - VUNR
*kl20170519 - G.02.01 - Nur dann, wenn nicht schon in Terminalanfrage
*                       gesendet
     IF  W2TS-TBMP(42) = 1
     AND W2TS-TPTR(42) > 0
     AND W2TS-TLEN(42) > 0
*        Sicherheitshalber abschalten (wg. K-BYTEMAP)
         MOVE  ZERO   TO W207-TBMP(42)
     ELSE
*        Falls nicht in TS-Anfrage, aus TXILOG70 hinzufuegen
         MOVE 42                TO W207-XBMP
         MOVE VUNR OF TXILOG70  TO W207-XCOBVAL
         MOVE 15                TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

**  ---> BMP 49 - WKZ �bernehmen
**  ---> BMP 53 - Sicherheitsverfahren ggf. �bernehmen
     IF  W2TS-TBMP(53) = 1
         MOVE 53 TO W207-XBMP
         MOVE 16 TO W207-XCOBLEN
         MOVE "0100000002000000" TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     
*G.03.02 - Chipdaten vom AS? Dann ans Terminal 
**  ---> BMP 55 - Chipdaten ggf. �bernehmen
     IF  IMSG-TBMP(55) = 1
         MOVE 55            TO W207-XBMP
         MOVE IMSG-TLEN(55) TO W207-XCOBLEN
         MOVE IMSG-CF(IMSG-TPTR(55):IMSG-TLEN(55)) TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
*G.03.02 - Ende

**  ---> BMP 57 - Verschl�sselungsparameter ggf. �bernehmen
     IF  W2TS-TBMP(53) = 1
         MOVE 57 TO W207-XBMP
         MOVE 18 TO W207-XCOBLEN
         MOVE W2TS-CF(W2TS-TPTR(57):W2TS-TLEN(57)) TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

**  ---> BMP 59 - Autorisierungsmerkmal aus AS-Antw. BMP38
     IF  IMSG-TBMP(38) = 1
         MOVE 59 TO W207-XBMP
         MOVE 08 TO W207-XCOBLEN
         MOVE W-GENNR TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

** BMP 60 - ???
** BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
** Pro Mandant im speziellen Teil

**  ---> Dummy-MAC vorbereiten
     IF  W2TS-TBMP(64) = 1
         SET MAC-YES TO TRUE
         MOVE 64 TO W207-XBMP
         MOVE 08 TO W207-XCOBLEN
         MOVE ALL LOW-VALUES TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
         SET MAC-NO TO TRUE
     END-IF

**  ---> damit sollte die Nachricht fertig sein
**  ---> nur f�rs testen
     IF  TRACE-ON
         move "Test #1 - C200-AS-GENERELL - w207-tbmp:" to daten-buffer1
         move w207-tbmp-o (1:64) to daten-buffer2

         perform z002-progerr
     END-IF
     .
 C200-99.
     EXIT.

******************************************************************
* TS-Antwortteile, die bei Antworten bei AS'sen unterschiedlich sind
******************************************************************
 C300-AS-SPEZIELL SECTION.
 C300-00.
**  ---> verzweigen je nach ROUTKZ
*G.03.01 - VERF-AS verwenden hier
*     EVALUATE W-ROUTKZ
     EVALUATE VERF-AS
*G.03.01 - Ende
         WHEN 05     PERFORM D305-AVIA
         WHEN 07     PERFORM D307-SHELL
         WHEN 10     PERFORM D310-TOTAL
         WHEN 12     PERFORM D312-DKV
         WHEN 14     PERFORM D314-BP
         WHEN 15     PERFORM D315-ENI
         WHEN 16     PERFORM D316-ORLEN
         WHEN 17     PERFORM D317-UTA
         WHEN 18     PERFORM D318-TND
         WHEN 22     PERFORM D322-EUROWAG
         WHEN 23     PERFORM D323-LOGPAY
         WHEN 24     PERFORM D324-STIGLECHNER
         WHEN 25     PERFORM D325-ROADRUNNER
*G.03.03 -  E100 neu
         WHEN 26     PERFORM D326-E100
*G.03.03 - Ende

         WHEN OTHER
                 SET ENDE TO TRUE
*G.03.01 - jetzt VERF-AS verwenden
*                 MOVE W-ROUTKZ TO D-NUM4
                 MOVE VERF-AS TO D-NUM4
*G.03.01 - Ende
                 STRING  "Keine speziellen Verarbeitungsregeln "
                         "f�r Rout-KZ = "
                         D-NUM4
                             delimited by size
                   INTO  DATEN-BUFFER1
                 END-STRING
                 PERFORM Z002-PROGERR
                 EXIT SECTION

     END-EVALUATE
     .
 C300-99.
     EXIT.

******************************************************************
* TS-Nachricht erstellen
******************************************************************
 C400-BUILD-TS-NACHRICHT SECTION.
 C400-00.
     SET W207-EC TO TRUE
     PERFORM L110-COB2ISO
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und echten MAC bilden und einstellen
     IF  MAC-YES
         MOVE ALL LOW-VALUES    TO W66-TKEY-NAME
         MOVE W-MACKEYT         TO W66-TKEY-NAME (1:4)
         SET  W66-MAC-BILDEN-TS TO TRUE
         SET  W66-EC            TO TRUE
         MOVE W-TERMNR          TO IMSG-TERMNR
         PERFORM F920-MAC-BILDEN
     END-IF

**  ---> und Empf�nderinfos einstellen
     MOVE FREHEADER of TXNLOG70-TS  TO IMSG-HEADER

**  ---> nur f�rs testen
     if  trace-on
         move "Test #2 - C400-BUILD-AS-NACHRICHT - w207-tbmp:" to daten-buffer1
         move w207-tbmp-o (1:64) to daten-buffer2

         perform z002-progerr
     end-if

     .
 C400-99.
     EXIT.

******************************************************************
* Loggen der Transaktionsdaten in die Tabellen
******************************************************************
 C500-LOGGING SECTION.
 C500-00.
**  ---> F�hrungstabelle =TXILOG70 beschicken
     PERFORM G100-PUT-TXILOG70

     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> Anfragenachricht f�r Tabelle =TXNLOG70 TS-Nachrichten
     PERFORM G110-PUT-TXNLOG70-TS

     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> Anfragenachricht f�r Tabelle =TXNLOG70 AS-Nachrichten
     PERFORM G120-PUT-TXNLOG70-AS

     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und schliesslich Eintrag in CRDUSEDN erzeugen
     PERFORM G140-PUT-CRDUSEDN
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> nun noch UMSWEAT bedienen
     IF W-AC = ZEROS
        PERFORM G130-PUT-UMSWEAT
        IF  ENDE
            EXIT SECTION
        END-IF
     END-IF

     .
 C500-99.
     EXIT.

******************************************************************
* Key bestimmen
*G.03.01 - muss bestimmt werden
******************************************************************
 D200-FIX-KEY SECTION.
 D200-00.
**  ---> wenn Schl�sseltabelle nur mit einem belegt ist, wieder zur�ck
     IF  TK-MAX = 1
         MOVE 1 TO C4-I1
         EXIT SECTION
     END-IF

**  ---> nun muss doch gesucht werden
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   C4-I1 > TK-MAX

         IF  TK-CARDID (C4-I1) NOT = W-CARDID
         OR  TK-ROUTKZ (C4-I1) NOT = W-ROUTKZ
**          ---> n�chsten suchen
             EXIT PERFORM CYCLE
         END-IF

**      ---> Schl�ssel zur Verf�gung stellen
         MOVE TK-HEXKEY (C4-I1) TO W-MACKEYA
         MOVE TK-HEXKEY (C4-I1) TO W-PACKEYA (1:4)
         EXIT SECTION
     END-PERFORM

**  ---> hier sind keine Schl�ssen gefunden worden
     SET ENDE TO TRUE
     MOVE W-ROUTKZ TO D-NUM4
     MOVE W-CARDID TO D-NUM4OV
     MOVE "Keine AS-Schl�ssel in =KEYNAMEN gefunden f�r:" TO DATEN-BUFFER1
     STRING  "ROUTKZ / CARDID = "
             D-NUM4
             " / "
             D-NUM4OV
                 delimited by size
       INTO  DATEN-BUFFER2
     END-STRING
     PERFORM Z002-PROGERR
     .
 D200-99.
     EXIT.
     
     
******************************************************************
* spezielle Behandlung f�r das Avia-AS
******************************************************************
 D305-AVIA SECTION.
 D305-00.
**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "AV" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .
 D305-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das Shell-AS
******************************************************************
 D307-SHELL SECTION.
 D307-00.

**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "SH" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .
 D307-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das Total-AS
******************************************************************
 D310-TOTAL SECTION.
 D310-00.

**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "TO" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .

 D310-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das DKV-AS
******************************************************************
 D312-DKV SECTION.
 D312-00.

**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "DK" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .

 D312-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das BP-AS
******************************************************************
 D314-BP SECTION.
 D314-00.

**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "BP" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .

 D314-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das ENI-AS fr�her AGYP
******************************************************************
 D315-ENI SECTION.
 D315-00.

**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "AG" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .

 D315-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das ORLEN-AS
******************************************************************
 D316-ORLEN SECTION.
 D316-00.

**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "OR" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

     .
 D316-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das UTA-AS
******************************************************************
 D317-UTA SECTION.
 D317-00.

**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "UT" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .

 D317-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das TND-AS
******************************************************************
 D318-TND SECTION.
 D318-00.
**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1

** TND wird im Arktikelmapper nicht extra behandelt
**       MOVE "??" TO AMP-FORMAT

         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .

 D318-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das EUROWAG-AS
******************************************************************
 D322-EUROWAG SECTION.
 D322-00.
**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "EU" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .
 D322-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das LOGPAY-AS
******************************************************************
 D323-LOGPAY SECTION.
 D323-00.
**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "LO" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .
 D323-99.
     EXIT.

******************************************************************
* spezielle Behandlung f�r das STIGLECHNER-AS
******************************************************************
 D324-STIGLECHNER SECTION.
 D324-00.
**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "IQ" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .
 D324-99.
     EXIT.

*kl20180530 - G.02.100 - Neues AS Road Runner
******************************************************************
* spezielle Behandlung f�r das RoadRunner-AS
******************************************************************
 D325-ROADRUNNER SECTION.
 D325-00.
**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "RR" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .
 D325-99.
     EXIT.
*kl20180530 - G.02.100 - Ende


******************************************************************
* spezielle Behandlung f�r das E100-AS
*G.03.03 - neu
******************************************************************
 D326-E100 SECTION.
 D326-00.
**  ---> BMP 63 - Artikeldaten (in AS-Tx zugelassen Prod. aus BMP62)
**  ---> Produktdaten aufbereiten

     IF  W-AC = 87 and IMSG-TBMP(62) = 1
         MOVE "E1" TO AMP-FORMAT
         PERFORM E300-ARTIKELDATEN
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF
     .
 D326-99.
     EXIT.

******************************************************************
 E300-ARTIKELDATEN SECTION.
 E300-00.

*Fall3: AS-AC 185 und AS-BMP 62 nicht vorhanden, dann mit der Nachricht 210
*AC 87 und BMP 63 mit 0 Produkten ans TS zur�ck schicken

     IF  W-AC-AS = 185
     AND IMSG-TBMP(62) = 0
         MOVE 63                         TO W207-XBMP
         MOVE 1                          TO W207-XCOBLEN
         MOVE LOW-VALUE                  TO AMP-POS-VAL(1:AMP-POS-LEN)
         MOVE AMP-POS-VAL(1:AMP-POS-LEN) TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         MOVE 87 TO W-AC

         EXIT SECTION

     END-IF

**  ---> Artikel mappen �ber Mappingserver
     SET AMP-S2P TO TRUE
     MOVE AS-CF(IMSG-TPTR(62):AS-TLEN(62)) TO AMP-HOST-VAL
     MOVE AS-TLEN(62)                      TO AMP-HOST-LEN
     MOVE W-MDNR                           TO AMP-MDNR
     MOVE W-TSNR                           TO AMP-TSNR
     MOVE W-CARDID                         TO AMP-CARDID
     MOVE ALL SPACES  TO AMP-POS-VAL
     MOVE ZERO        TO AMP-RC
                         AMP-HOST-LEN

     MOVE ALL SPACES  TO AMP-TS63

*kl20151203 - G.xx.xx - Nur dann, wenn auch besetzt - sonst schmiert
*                       das sp�testens im Artikel-Mapper ab ...
*    MOVE W2TS-CF(W2TS-TPTR(63):W2TS-TLEN(63)) TO AMP-TS63

     IF  W2TS-TBMP(63) = 1
     AND W2TS-TPTR(63) > ZERO
     AND W2TS-TLEN(63) > ZERO
         MOVE W2TS-CF(W2TS-TPTR(63):W2TS-TLEN(63)) TO AMP-TS63
     END-IF
*kl20151203 - G.xx.xx - Ende

**  ---> Pathsend zum Mappingserver
     MOVE 20 TO INLINE-SERVICE
     PERFORM P100-PATHSEND
     
     IF AMP-OK
        MOVE 63          TO W207-XBMP
        MOVE AMP-POS-LEN TO W207-XCOBLEN
        MOVE AMP-POS-VAL(1:AMP-POS-LEN) TO W207-XCOBVAL
        PERFORM L100-ADD-BMP

*Fall1: AS-AC 185 und AS-BMP 62 vorhanden und Mapping von mindestens
*       einem Produkt m�glich, dann 210er mit AC 87 und BMP 63 mit mindestens
*       einem Produkt zur�ck ans TS
        IF  W-AC-AS = 185
        AND IMSG-TBMP(62) = 1
            MOVE 87 TO W-AC
        END-IF

        EXIT SECTION
     END-IF


**  ---> Return-Code aus Artikelmapper
     MOVE AMP-RC TO D-NUM4
     STRING    "Return-Code aus Artikelmapping: "
               D-NUM4
     delimited by size
       INTO    DATEN-BUFFER1
     END-STRING
     PERFORM Z002-PROGERR

*Fall2: AS-AC 185 und AS-BMP 62 vorhanden,jedoch kein Mapping m�glich,
*dann 210er AC 87, BMP 63 mit 0 Produkten zur�ck ans TS

     IF  W-AC-AS = 185
     AND IMSG-TBMP(62) = 1
         MOVE 87 TO W-AC
     ELSE
**Keine Artikel zur�ck zum Terminal und AC=96
       MOVE 96 TO W-AC
     END-IF

     .

 E300-99.
     EXIT.

******************************************************************
* erzeugen Fehlermeldung
******************************************************************
 E900-PUT-ERRLOG SECTION.
 E900-00.
     MOVE 1105 TO ERROR-NR of GEN-ERROR
     MOVE "=FCPARAM f�r: @" TO DATEN-BUFFER1
*G.03.01 - VERF-AS jetzt
*     MOVE W-ROUTKZ  TO D-NUM4
     MOVE VERF-AS  TO D-NUM4
*G.03.01 - Ende
     MOVE S-ISONTYP TO D-NUM4M
     MOVE S-BMP     TO D-NUM4N
     MOVE W-CARDID  TO D-NUM4OV
     STRING  "ROUTKZ/ISONTYP/KZ-MSG/BMP/CARDID = "
             D-NUM4 "/" D-NUM4M "/" S-KZ-MSG "/" D-NUM4N "/" D-NUM4OV
                 delimited by size
       INTO  DATEN-BUFFER2
     END-STRING
     PERFORM Z002-PROGERR
     .
 E900-99.
     EXIT.

******************************************************************
* MAC pr�fen (ggf. ist W-AC gesetzt)
******************************************************************
 F910-MAC-PRUEFEN SECTION.
 F910-00.
     SET W66-MAC-PRUEFEN-AS TO TRUE
     MOVE 88                TO W66-RCODE of W66-WSY7066C
     MOVE ALL LOW-VALUES    TO W66-AKEY-NAME
     MOVE W-MACKEYA         TO W66-AKEY-NAME (1:4)
     PERFORM M140-CALL-WSY7066
     .
 F910-99.
     EXIT.


 F915-ASMAC-DUKPT SECTION.
 F915-00.
*
      SET Z-MAC-PRUEFEN-AS      TO TRUE
      MOVE 88              TO Z-RCODE OF Z-WEUR056C
      MOVE ALL LOW-VALUES  TO Z-AKEY-NAME
      MOVE W-MACKEYA       TO Z-AKEY-NAME (1:4)

*     ASTRACENR wird nur bei PAC-Umschl�sselung gesetzt und
*     muss bei diesem Aufruf bereits in BMP 53 eingestellt sein
*     (R�ckgabe aus PAC-UMSCHL)

      CALL "WEUR056"  USING Z-WEUR056C,
                            INTERN-MESSAGE

      IF Z-ERR
         MOVE   Z-RCODE     TO D-NUM4
         STRING "Returncode aus WEUR056: ", D-NUM4
         DELIMITED BY SIZE INTO DATEN-BUFFER1
         PERFORM Z002-PROGERR
         MOVE 96 TO W-AC
      END-IF
*
   .
 F915-99.
   EXIT.

******************************************************************
* Erstellen Antwort-MAC
******************************************************************
 F920-MAC-BILDEN SECTION.
 F920-00.
**  ---> MAC-Bildung und Schl�sselwechsel �ber WSY7066
**  ---> Datenbereich Message-datei external �bergibt Daten f�r WSY7066
     SET W66-EC    TO TRUE
     MOVE 88       TO W66-RCODE of W66-WSY7066C
     MOVE ALL LOW-VALUES    TO W66-TKEY-NAME
     MOVE W-MACKEYT         TO W66-TKEY-NAME (1:4)
     PERFORM M140-CALL-WSY7066
     IF  W66-ERR
         MOVE 96   TO W-AC
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         MOVE W66-RCODE TO D-NUM4
         STRING  "WSY7066@"          delimited by size
                 D-NUM4              delimited by size
                 "@@"                delimited by size
           INTO  DATEN-BUFFER1
         END-STRING
         MOVE "Bei F920-MAC-BILDEN" TO DATEN-BUFFER2
         MOVE "Transaktions-Ende"   TO DATEN-BUFFER3
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF

**  --> Datenteil wird in WSY7066 mit MAC versehen
     .
 F920-99.
      EXIT.

******************************************************************
* Einstellen Daten in TXILOG70 Buffer
******************************************************************
 G100-PUT-TXILOG70 SECTION.
 G100-00.
**  ---> autorisierter Betrag
     MOVE W18-BETRAG     TO BETRAG-AUTOR   of TXILOG70
     MOVE W-ASID         TO ASID           of TXILOG70
     MOVE W-AC-AS        TO AC-AS          of TXILOG70

     IF TABL-ABL
        MOVE TABL-AC     TO AC-TERM        of TXILOG70
     ELSE
        MOVE W-AC        TO AC-TERM        of TXILOG70
     END-IF

     MOVE W-GENNR        TO GENNR          of TXILOG70
     IF  not ENDE
         MOVE "R"        TO KZ-BEARB       of TXILOG70
     END-IF

**  ---> holen momentanen Zeitpunkt
     PERFORM U200-TIMESTAMP
     MOVE TAGESDATUM TO ZP-TOUT of TXILOG70
     MOVE H-ZP-IN    TO ZP-AIN  of TXILOG70

**  ---> und schreiben
     PERFORM S180-INSERT-TXILOG70
     .
 G100-99.
     EXIT.

******************************************************************
* Einstellen Daten in TXNLOG70-TS Buffer
******************************************************************
 G110-PUT-TXNLOG70-TS SECTION.
 G110-00.
     MOVE K-MODUL        TO LOG-SRV        of TXNLOG70-TS
     MOVE IMSG-HEADER    TO FREHEADER      of TXNLOG70-TS
     MOVE IMSG-NDATEN    TO VAL of ANTWORT of TXNLOG70-TS
     MOVE IMSG-DATLEN    TO LEN of ANTWORT of TXNLOG70-TS

**  ---> und nun schreiben
     PERFORM S190-INSERT-TXNLOG70-TS
     .
 G110-99.
     EXIT.

******************************************************************
* Einstellen Daten in TXNLOG70-AS Buffer
******************************************************************
 G120-PUT-TXNLOG70-AS SECTION.
 G120-00.
     MOVE K-MODUL        TO LOG-SRV        of TXNLOG70-AS
     MOVE AS-HEADER      TO FREHEADER      of TXNLOG70-AS
     MOVE AS-NDATEN      TO VAL of ANTWORT of TXNLOG70-AS
     MOVE AS-DATLEN      TO LEN of ANTWORT of TXNLOG70-AS

**  ---> und nun schreiben
     PERFORM S200-INSERT-TXNLOG70-AS
     .
 G120-99.
     EXIT.

******************************************************************
* Einstellen Daten in UMSWEAT Buffer
******************************************************************
 G130-PUT-UMSWEAT SECTION.
 G130-00.
     MOVE PNR       of TXILOG70  TO PNR       of UMSWEAT
     MOVE TERMNR    of TXILOG70  TO TERMNR    of UMSWEAT
     MOVE TRACENR   of TXILOG70  TO TRACENR   of UMSWEAT
     MOVE MDNR      of TXILOG70  TO MDNR      of UMSWEAT
     MOVE TSNR      of TXILOG70  TO TSNR      of UMSWEAT
     MOVE TRACENR-S of TXILOG70  TO TRACENR-S of UMSWEAT
     MOVE CARDID    of TXILOG70  TO CARDID    of UMSWEAT
     MOVE BETRAG    of TXILOG70  TO BETRAG    of UMSWEAT
     MOVE WKZ       of TXILOG70  TO WKZ       of UMSWEAT
     MOVE KZ-VERF   of TXILOG70  TO KZ-VERF   of UMSWEAT
     MOVE "R"                    TO KZ-BEARB  of UMSWEAT
     MOVE W-BELEGNR              TO BELEGNR   of UMSWEAT
     MOVE W-ABWKZ                TO ABWKZ     of UMSWEAT

**  ---> Aufruf Modul IUMSw07 (Zugriff zum UMSIFSF-Server)
     MOVE UMSWEAT    TO WUMS-UMSATZ
     MOVE K-MODUL    TO WUMS-ABSENDER
     SET WUMS-TAB-UW TO TRUE
     SET WUMS-CMD-I  TO TRUE
     PERFORM M180-CALL-IUMSW07
     .
 G130-99.
     EXIT.

******************************************************************
* Einstellen Daten in CRDUSEDN-Buffer
******************************************************************
 G140-PUT-CRDUSEDN SECTION.
 G140-00.
**  ---> zun�chst L�nge der Kartennummer feststellen
     MOVE ZERO TO C4-ANZ
     MOVE 1    TO C4-PTR
     MOVE SPACES     TO W-TEILSTRING-TABELLE
     MOVE SPACES     TO W-DELIM-TABELLE
     MOVE LOW-VALUES TO W-COUNT-TABELLE

     UNSTRING KANR of TXILOG70
                                delimited by ALL SPACE
         INTO W-TEILSTRING (1)  delimiter in W-DELIM (1)
                                count     in W-COUNT (1)
              W-TEILSTRING (2)  delimiter in W-DELIM (2)
                                count     in W-COUNT (2)
         WITH     POINTER C4-PTR
         TALLYING IN      C4-ANZ
     END-UNSTRING

**  ---> L�nge ist jetzt in W-COUNT(1)

     MOVE KANR of TXILOG70 (W-COUNT (1):1) TO SDB-PNR
     MOVE KANR      of TXILOG70 TO SDB-KANR
     MOVE "T"                   TO SDB-AKZ
     MOVE TERMNR    of TXILOG70 TO SDB-TERMNR
     MOVE TRACENR   of TXILOG70 TO SDB-TRACENR
     MOVE W-AC                  TO SDB-AC
     MOVE BETRAG    of TXILOG70 TO SDB-BETRAG
     MOVE W-MDNR                TO SDB-MDNR
     MOVE W-TSNR                TO SDB-TSNR

**  ---> und nun schreiben
     PERFORM M100-CALL-SDBCDU5
     .
 G140-99.
     EXIT.


******************************************************************
* Felder zusaetzlich in Nachricht einbauen
******************************************************************
 L100-ADD-BMP SECTION.
 L100-00.
     SET W207-ADD-BMP TO TRUE
     PERFORM M130-CALL-WISO207

     IF  W207-RCODE NOT = ZERO
         MOVE W207-RCODE TO D-NUM4
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         STRING "WISO207 (ADD-BMP)/@"
                "RC: " D-NUM4
                "@"
                 delimited by size
           INTO DATEN-BUFFER1
         END-STRING
         SET ENDE TO TRUE
         PERFORM Z002-PROGERR
     END-IF
     .
 L100-99.
     EXIT.

******************************************************************
* ISO-Nachricht bereitstellen
******************************************************************
 L110-COB2ISO SECTION.
 L110-00.
     SET W207-COB2ISO TO TRUE
     PERFORM M130-CALL-WISO207

     IF  W207-RCODE NOT = ZERO
         MOVE W207-RCODE TO D-NUM4
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         STRING "WISO207 (COB2ISO)/@"
                "RC: " D-NUM4
                "@"
                 delimited by size
           INTO DATEN-BUFFER1
         END-STRING
         PERFORM Z002-PROGERR
     END-IF

**  ---> Nachricht �bertragen
     MOVE W207-ISOSTRING TO IMSG-NDATEN
     MOVE W207-COBDATEN  TO IMSG-COBDATEN
     MOVE W207-ISOLEN    TO IMSG-SENDLEN
                            IMSG-DATLEN

**  ---> nur f�rs testen
     IF  TRACE-ON
         move "Test #4 - L110-COB2ISO - w207-tbmp:" to daten-buffer1
         move w207-tbmp-o (1:64) to daten-buffer2
         move W207-ISOLEN to d-num4
         move "isolen: " to daten-buffer3
         move d-num4     to daten-buffer3 (9:)
         perform z002-progerr
     END-IF

     .
 L110-99.
     EXIT.

******************************************************************
* ISO-Nachricht bereitstellen
******************************************************************
 L120-ISO2COB SECTION.
 L120-00.
     SET W207-ISO2COB TO TRUE
     PERFORM M130-CALL-WISO207

     IF  W207-RCODE NOT = ZERO
         SET ENDE TO TRUE
         MOVE W207-RCODE TO D-NUM4
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         STRING  "WISO207 (ISO2COB)/@"
                 "RC: " D-NUM4
                 "@"
                     delimited by size
           INTO  DATEN-BUFFER1
         END-STRING
         MOVE W207-ISOTYP TO D-NUM4
         STRING  "bei Verfahren: "
                 D-NUM4
                     delimited by size
           INTO DATEN-BUFFER2
         END-STRING
         PERFORM Z002-PROGERR
     END-IF
     .
 L120-99.
     EXIT.

******************************************************************
* Aufruf Modul SDBCDU5
******************************************************************
 M100-CALL-SDBCDU5 SECTION.
 M100-00.
     MOVE 9999 TO SDB-RCODE
     SET SDB-CMD-INS-OTA TO TRUE
     CALL "SDBCDU5" USING SDB-SDBCDU0C
     IF  SDB-ERR
**      ---> Ausgabe Fehlermeldung
         MOVE 1201 TO ERROR-NR OF GEN-ERROR
         MOVE SDB-RCODE TO D-NUM4
         STRING  "SDBCDU5@"
                 "RCODE@"
                 D-NUM4
                 "@"
                     delimited by size
           INTO  DATEN-BUFFER1
         END-STRING
         PERFORM Z002-PROGERR
**      ---> Programm soll trotzdem weiterarbeiten
         SET ENDE TO TRUE
     END-IF
     .
 M100-99.
     EXIT.

******************************************************************
* Aufruf Modul SYSMK7I (MEMLOG)
******************************************************************
 M110-CALL-SYSMK7I SECTION.
 M110-00.
     CALL "SYSMK7I" USING MEM-SYSML7IC
                          MEMLOG-SATZ
     IF  MEM-RETCODE NOT = ZERO
         MOVE MEM-RETCODE TO D-NUM4
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         STRING "SYSMK7I (MEMLOG)/@"
                "Retcode: " D-NUM4
                "@"
                     delimited by size
           INTO DATEN-BUFFER1
         END-STRING
         SET ENDE TO TRUE
         PERFORM Z002-PROGERR
     END-IF
     .
 M110-99.
     EXIT.

******************************************************************
* Aufruf Modul SYSABL1
******************************************************************
 M120-CALL-SYSABL1 SECTION.
 M120-00.
     MOVE W-TERMNR      TO TABL-TERMNR
     MOVE K-MODUL       TO TABL-SERVER
     MOVE 88            TO TABL-RCODE
     SET TABL-CMD-NOTMF TO TRUE
     CALL "SYSABL1" USING TABL-SYSABL1C

     EVALUATE TRUE
         WHEN TABL-OK    continue
         WHEN TABL-ABL
              STRING  ">>> "
                      TABL-AC
                      " zum Terminal gesendet (TERMABL) <<<"
               delimited by size
                INTO  DATEN-BUFFER1
              END-STRING
              PERFORM Z002-PROGERR
         WHEN OTHER
              MOVE 1201 TO ERROR-NR of GEN-ERROR
              MOVE TABL-RCODE TO D-NUM4
              STRING  "SYSABL1@"
                       D-NUM4
                      "@"
                       W-TERMNR
                      "@"
                delimited by size
                INTO  DATEN-BUFFER1
              END-STRING
              PERFORM Z002-PROGERR
          END-EVALUATE
     .
 M120-99.
     EXIT.

******************************************************************
* Aufruf Modul WISO207
******************************************************************
 M130-CALL-WISO207 SECTION.
 M130-00.
     CALL "WISO207" USING W207-WISO207C
     .
 M130-99.
     EXIT.

******************************************************************
* Aufruf Modul WSY7066
******************************************************************
 M140-CALL-WSY7066 SECTION.
 M140-00.
     CALL "WSY7066" USING W66-WSY7066C
                          INTERN-MESSAGE
     IF  W66-RCODE NOT = ZERO
         MOVE 96 TO W-AC
     END-IF
     .
 M140-99.
     EXIT.

******************************************************************
* Aufruf Modul WSYS971
******************************************************************
 M160-CALL-WSYS971 SECTION.
 M160-00.
     CALL "WSYS971" USING CHK-WSYS971C

**  ---> nun das Ergebnis �berpr�fen
     EVALUATE CHK-RCODE
**      ---> OK
         WHEN ZERO       CONTINUE

**      ---> Fehler bei BMP  ==> ENDE
         WHEN 0001 THRU 0128
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR of GEN-ERROR
                         STRING "BMP/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         SET ENDE TO TRUE

**      ---> andere Fehler (z.B. CMD-Error)  ==> ENDE
         WHEN 0129 THRU 0999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 1201 TO ERROR-NR of GEN-ERROR
                         STRING "WSYS971 (Feldcheck)/"
                                "@"
                                D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         STRING "NTYPE, ABWKZ = "
                                CHK-NTYPE ", "
                                CHK-ABWKZ
                                     delimited by size
                           INTO DATEN-BUFFER2
                         END-STRING
                         SET ENDE TO TRUE

**      ---> BMP nicht numerisch  ==> AC 30
         WHEN 1000 THRU 1999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR of GEN-ERROR
                         STRING "BMP nicht num./Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 81 TO W-AC

**      ---> BMP fehlt  ==> AC 30
         WHEN 4000 THRU 4999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR of GEN-ERROR
                         STRING "BMP fehlt/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 81 TO W-AC

**      ---> BMP zuviel  ==> AC 30
         WHEN 5000 THRU 5999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR of GEN-ERROR
                         STRING "BMP zuviel/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 81 TO W-AC

**      ---> sonstige ?  ==> ENDE
         WHEN OTHER
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 1201 TO ERROR-NR of GEN-ERROR
                         STRING "WSYS971 (Feldcheck)/"
                                "@"
                                D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         SET ENDE TO TRUE
     END-EVALUATE

**  ---> Fehlermeldung vervollst�ndigen
     IF  W-AC > ZERO or ENDE
         STRING  "WEAT-Term-Nr./Trace-Nr.: "
                 IMSG-CF (IMSG-TPTR(41):IMSG-TLEN(41))
                 "/"
                 IMSG-CF (IMSG-TPTR(11):IMSG-TLEN(11))
                     delimited by size
           INTO  DATEN-BUFFER3
         END-STRING
         PERFORM Z002-PROGERR
     END-IF
     .
 M160-99.
     EXIT.

******************************************************************
* Aufruf Modul IUMSW07
******************************************************************
 M180-CALL-IUMSW07 SECTION.
 M180-00.
     CALL "IUMSW07" USING WUMS-WUMSO07C
     IF  WUMS-ERR
         MOVE 1201           TO ERROR-NR OF GEN-ERROR
         MOVE WUMS-RCODE     TO D-NUM4
         STRING  "IUMSW07@"
                 D-NUM4
                 "@@"
                     delimited by size
           INTO  DATEN-BUFFER1
         END-STRING
         SET ENDE TO TRUE
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF
     .
 M180-99.
     EXIT.

******************************************************************
* Aufruf Artikel-Mapper
******************************************************************
 P100-PATHSEND SECTION.
 P100-00.
**  ---> Nachrichtenparameter (Server, L�nge, Timeout)
     MOVE W-ARTMAP TO PS-SRVCLASS
     MOVE 2048     TO PS-REQLEN
     MOVE 1000     TO PS-TIMEOUT

**  ---> �brige Parameter
     MOVE PS-REQLEN TO PS-MAXREPLEN
     MOVE ZERO      TO PS-AKTREPLEN
                       PS-ERROR

**  ---> ggf. Pathwaysystem
     IF  PS-PATHNAME = SPACE
         MOVE ANCNAME TO PS-PATHNAME
     END-IF

     ENTER "SERVERCLASS_SEND_" USING  PS-PATHNAME
                                      PS-PATHNAMELEN
                                      PS-SRVCLASS
                                      PS-SRVCLASSLEN
                                      AMP-SCHNITTSTELLE
                                      PS-REQLEN
                                      PS-MAXREPLEN
                                      PS-AKTREPLEN
                                      PS-TIMEOUT
                               GIVING PS-ERROR

     IF  PS-ERROR NOT = ZERO
         ENTER "SERVERCLASS_SEND_INFO_" USING SSI-PSERROR
                                              SSI-FSERROR
                                       GIVING SSI-ERROR
         IF  SSI-ERROR = ZERO
             SET ENDE  TO TRUE
             MOVE 1210 TO ERROR-NR of GEN-ERROR
             MOVE PS-ERROR    TO D-NUM4
             MOVE SSI-PSERROR TO D-NUM4N
             MOVE SSI-FSERROR TO D-NUM4M
             STRING  D-NUM4
                     "/"
                     D-NUM4N
                     "/"
                     D-NUM4M
                     "@"
                         delimited by size
               INTO DATEN-BUFFER1
             END-STRING
         ELSE
             SET ENDE TO TRUE
             MOVE 1210 TO ERROR-NR of GEN-ERROR
             MOVE SSI-ERROR TO D-NUM4
             STRING  "INFO-Fehler: "
                     D-NUM4
                     "@"
                         delimited by size
               INTO DATEN-BUFFER1
             END-STRING
         END-IF
         PERFORM Z002-PROGERR
     END-IF
     .
 P100-99.
     EXIT.


******************************************************************
* hexen eines Strings (max. 16 Bytes)
*        Eingabe:    P-HEX16
*        Ausgabe:    P-HEX8
******************************************************************
 P900-WTHEX SECTION.
 P900-00.
     ENTER TAL "WT^HEX" USING P-HEX8 P-HEX16
     .
 P900-99.
     EXIT.

******************************************************************
* unhexen eines Strings (max. 16 Bytes)
*        Eingabe:    P-HEX8
*        Ausgabe:    P-HEX16
*        Eingabe:    P-HEX8
******************************************************************
 P910-WTUNHEX SECTION.
 P910-00.
     ENTER TAL "WT^UNHEX" USING P-HEX8 P-HEX16
     .
 P910-99.
     EXIT.

******************************************************************
* Aufruf COBOL-Utility: GETPARAMTEXT
*
*              Eingabe: stup-portion (parametername)
*              Ausgabe: stup-result  (-1:NOK, >=0:OK l�nge von text)
*                       stup-text    (value von ..-portion)
*
******************************************************************
 P950-GETPARAMTEXT SECTION.
 P950-00.
     MOVE SPACE TO STUP-TEXT
     ENTER "GETPARAMTEXT"    USING   STUP-PORTION
                                     STUP-TEXT
                             GIVING  STUP-RESULT
     EVALUATE STUP-RESULT
         WHEN -9999 THRU ZERO
**                  ---> Fehler aus GetParamText
**                  ---> oder Parameter nicht vorhanden
                     MOVE 1101 TO ERROR-NR OF GEN-ERROR
                     MOVE STUP-RESULT TO D-NUM4
                     STRING  STUP-PORTION    delimited by space
                             "@"             delimited by size
                             D-NUM4          delimited by size
                             " oder fehlt@"  delimited by size
                       INTO  DATEN-BUFFER1
                     END-STRING
                     PERFORM Z002-PROGERR
                     SET PRG-ABBRUCH TO TRUE
                     EXIT SECTION

         WHEN OTHER
**                  ---> ParamText ist vorhanden in STUP-TEXT
**                  ---> L�nge     ist vorhanden in STUP-RESULT

                     continue
     END-EVALUATE
     .
 P950-99.
     EXIT.

******************************************************************
* holen ancestor
******************************************************************
 P960-GET-PROC-ANCNAME SECTION.
 P960-00.
     ENTER TAL "WT^GETPROCNAME" USING FEHL
                                      PNAME
                                      PNAMELEN
     IF  PNAMELEN > 1
         continue
     ELSE
         MOVE "Prozessname nicht bestimmbar!" TO DATEN-BUFFER1
         MOVE "Programm-Abbruch (AFCAUT7S)"   TO DATEN-BUFFER2
         PERFORM Z002-PROGERR
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     ENTER TAL "WT^ANCNAME" USING FEHL
                                  ANCNAME
                                  PAIRINFO
     IF  FEHL not = ZERO
         MOVE "Ancestor Pathway nicht ermittelbar" TO DATEN-BUFFER1
         MOVE "Programm-Abbruch (AFCAUT7S)"        TO DATEN-BUFFER2
         STRING  "Prozessname: "
                 PNAME (1:PNAMELEN)
                     delimited by size
           INTO  DATEN-BUFFER3
         END-STRING
         PERFORM Z002-PROGERR
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF
     .
 P960-99.
     EXIT.


******************************************************************
* Lesen Message-Datei $RECEIVE
******************************************************************
 R000-LESEN-MESSAGE SECTION.
 R000-00.
     READ MESSAGE-DATEI
     IF  FILE-NOK
         SET MSG-EOF TO TRUE
     ELSE
         MOVE IMSG-DATLEN TO IMSG-SENDLEN
     END-IF

*** =>
*** => weitere Verarbeitung hier einf�gen
*** =>
     .
 R000-99.
     EXIT.

******************************************************************
* Schreiben Reply-Datei $RECEIVE
******************************************************************
 S000-SCHREIBEN-REPLY SECTION.
 S000-00.
**  ---> wenn Verarbeitung nicht OK:
     IF  ENDE
         SET IMSG-READ-IL TO TRUE
         MOVE SPACE TO IMSG-NEXTSERV
         MOVE "*"   TO IMSG-TRACETERMID
     END-IF

     MOVE IMSG-DATLEN TO IMSG-SENDLEN
     COMPUTE REPLY-LAENGE = IMSG-SENDLEN + 88

     MOVE K-MODUL        TO IMSG-MONNAME
     MOVE INTERN-MESSAGE TO REPLY-SATZ

**  ---> Appl. Monitoring
     SET PCAP-WRITE-TRACE TO TRUE
     CALL "WCAPM92" USING PCAP-PCAPM01C

     WRITE REPLY-SATZ
     .
 S000-99.
     EXIT.

******************************************************************
* Select auf Tabelle KEYNAMEN
******************************************************************
 S100-SELECT-KEYNAMEN SECTION.
 S100-00.
     EXEC SQL
         SELECT    ROUTKZ, CARDID, ASNAME, KEYNAME, ISOGEN
                 , ISOVERS, OPTIONEN
           INTO   :ROUTKZ of KEYNAMEN
                 ,:CARDID of KEYNAMEN
                 ,:ASNAME of KEYNAMEN
                 ,:KEYNAME of KEYNAMEN
                 ,:ISOGEN of KEYNAMEN
                 ,:ISOVERS of KEYNAMEN
                 ,:OPTIONEN of KEYNAMEN
           FROM  =KEYNAMEN
          WHERE  ROUTKZ, CARDID
                 =    :ROUTKZ of KEYNAMEN
                     ,:CARDID of KEYNAMEN
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET KEYNAMEN-OK  TO TRUE
         WHEN OTHER      SET ENDE         TO TRUE
                         SET KEYNAMEN-NOK TO TRUE
                         MOVE 91   TO W-AC
                         MOVE 2001 TO ERROR-NR of GEN-ERROR
                         MOVE ROUTKZ of KEYNAMEN TO D-NUM20
                         MOVE CARDID of KEYNAMEN TO D-NUM21
                         STRING  "KEYNAMEN@"
                                 "ROUTID,CARDID="
                                 D-NUM20 "/" D-NUM21 "@"
                                     DELIMITED BY SIZE
                           INTO DATEN-BUFFER1
                         END-STRING
                         PERFORM Z002-PROGERR
                         EXIT SECTION
     END-EVALUATE
     .
 S100-99.
     EXIT.

******************************************************************
* Insert auf Tabelle TXILOG70
******************************************************************
 S180-INSERT-TXILOG70 SECTION.
 S180-00.
     EXEC SQL
         INSERT
           INTO  =TXILOG70
                 ( PNR, TERMNR, TRACENR, ISONTYP, MDNR
                 , TSNR, TRACENR_AS, TRACENR_S, BATCHNR, KANR
                 , KZ_E2EE, KEYNAME, BETRAG, BETRAG_AUTOR
                 , BETRAG_CASHBACK, BETRAG_ART, CARDID, ROUTKZ
                 , LTGIND, ASID, AC_AS, AC_TERM, GENNR, WKZ
                 , LOGPROT, KZ_BEARB, KZ_VERF, KZ_UMSATZ, ABL_JJMM
                 , ACQUIRER_ID, ERFASSUNGS_ART, KARTEN_ART
                 , KARTENFOLGE, POS_DATEN, TRANS_ART, TRANS_TYP
                 , CVM_RESULT, BRANCHEN_KZ, HAENDLERNAME, PROJEKT_ABH_DATEN
                 , VUNR, ZP_VERKAUF, ZP_TIN, ZP_AOUT, ZP_AIN
                 , ZP_TOUT, AA_BMP38, AF_BMP07, ARTIKEL, EMV_DATEN
                 )
         VALUES  (
                  :PNR of TXILOG70
                 ,:TERMNR of TXILOG70
                 ,:TRACENR of TXILOG70
                 ,:ISONTYP of TXILOG70
                 ,:MDNR of TXILOG70
                 ,:TSNR of TXILOG70
                 ,:TRACENR-AS of TXILOG70
                 ,:TRACENR-S of TXILOG70
                 ,:BATCHNR of TXILOG70
                 ,:KANR of TXILOG70
                 ,:KZ-E2EE of TXILOG70
                 ,:KEYNAME of TXILOG70
                 ,:BETRAG of TXILOG70
                 ,:BETRAG-AUTOR of TXILOG70
                 ,:BETRAG-CASHBACK of TXILOG70
                 ,:BETRAG-ART of TXILOG70
                 ,:CARDID of TXILOG70
                 ,:ROUTKZ of TXILOG70
                 ,:LTGIND of TXILOG70
                 ,:ASID of TXILOG70
                 ,:AC-AS of TXILOG70
                 ,:AC-TERM of TXILOG70
                 ,:GENNR of TXILOG70
                 ,:WKZ of TXILOG70
                 ,:LOGPROT of TXILOG70
                 ,:KZ-BEARB of TXILOG70
                 ,:KZ-VERF of TXILOG70
                 ,:KZ-UMSATZ of TXILOG70
                 ,:ABL-JJMM of TXILOG70
                 ,:ACQUIRER-ID of TXILOG70
                 ,:ERFASSUNGS-ART of TXILOG70
                 ,:KARTEN-ART of TXILOG70
                 ,:KARTENFOLGE of TXILOG70
                 ,:POS-DATEN of TXILOG70
                 ,:TRANS-ART of TXILOG70
                 ,:TRANS-TYP of TXILOG70
                 ,:CVM-RESULT of TXILOG70
                 ,:BRANCHEN-KZ of TXILOG70
                 ,:HAENDLERNAME of TXILOG70
                 ,:PROJEKT-ABH-DATEN of TXILOG70
                 ,:VUNR of TXILOG70
                 ,:ZP-VERKAUF of TXILOG70
                 ,:ZP-TIN of TXILOG70
                     TYPE AS DATETIME YEAR TO FRACTION(2)
                 ,:ZP-AOUT of TXILOG70
                     TYPE AS DATETIME YEAR TO FRACTION(2)
                 ,:ZP-AIN of TXILOG70
                     TYPE AS DATETIME YEAR TO FRACTION(2)
                 ,:ZP-TOUT of TXILOG70
                     TYPE AS DATETIME YEAR TO FRACTION(2)
                 ,:AA-BMP38 of TXILOG70
                 ,:AF-BMP07 of TXILOG70
                 ,:ARTIKEL of TXILOG70
                 ,:EMV-DATEN of TXILOG70
                 )
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET TXILOG70-OK  TO TRUE
         WHEN OTHER  SET TXILOG70-NOK TO TRUE
                     SET ENDE TO TRUE
                     MOVE 2005 TO ERROR-NR of GEN-ERROR
                     STRING  "TXILOG70@"
                             PNR     of TXILOG70 "/"
                             TERMNR  of TXILOG70 "/"
                             TRACENR of TXILOG70 "/"
                             ISONTYP of TXILOG70
                                 DELIMITED BY SIZE
                       INTO DATEN-BUFFER1
                     END-STRING
                     PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S180-99.
     EXIT.

******************************************************************
* Insert auf Tabelle TXNLOG70
******************************************************************
 S190-INSERT-TXNLOG70-TS SECTION.
 S190-00.
     EXEC SQL
         INSERT
           INTO  =TXNLOG70
                 ( PNR, TERMNR, TRACENR, ISONTYP, KZ_MSG, ISO_VERF
                 , MDNR, TSNR, LOG_SRV, FREHEADER, ANFRAGE, ANTWORT
                 )
         VALUES  (
                  :PNR of TXNLOG70-TS
                 ,:TERMNR of TXNLOG70-TS
                 ,:TRACENR of TXNLOG70-TS
                 ,:ISONTYP of TXNLOG70-TS
                 ,:KZ-MSG of TXNLOG70-TS
                 ,:ISO-VERF of TXNLOG70-TS
                 ,:MDNR of TXNLOG70-TS
                 ,:TSNR of TXNLOG70-TS
                 ,:LOG-SRV of TXNLOG70-TS
                 ,:FREHEADER of TXNLOG70-TS
                 ,:ANFRAGE of TXNLOG70-TS
                 ,:ANTWORT of TXNLOG70-TS
                 )
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET TXNLOG70-OK  TO TRUE
         WHEN OTHER  SET TXNLOG70-NOK TO TRUE
                     SET ENDE TO TRUE
     END-EVALUATE
     .
 S190-99.
     EXIT.

******************************************************************
* Insert auf Tabelle TXNLOG70
******************************************************************
 S200-INSERT-TXNLOG70-AS SECTION.
 S200-00.
     EXEC SQL
         INSERT
           INTO  =TXNLOG70
                 ( PNR, TERMNR, TRACENR, ISONTYP, KZ_MSG, ISO_VERF
                 , MDNR, TSNR, LOG_SRV, FREHEADER, ANFRAGE, ANTWORT
                 )
         VALUES  (
                  :PNR of TXNLOG70-AS
                 ,:TERMNR of TXNLOG70-AS
                 ,:TRACENR of TXNLOG70-AS
                 ,:ISONTYP of TXNLOG70-AS
                 ,:KZ-MSG of TXNLOG70-AS
                 ,:ISO-VERF of TXNLOG70-AS
                 ,:MDNR of TXNLOG70-AS
                 ,:TSNR of TXNLOG70-AS
                 ,:LOG-SRV of TXNLOG70-AS
                 ,:FREHEADER of TXNLOG70-AS
                 ,:ANFRAGE of TXNLOG70-AS
                 ,:ANTWORT of TXNLOG70-AS
                 )
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET TXNLOG70-OK  TO TRUE
         WHEN OTHER  SET TXNLOG70-NOK TO TRUE
                     SET ENDE TO TRUE
     END-EVALUATE
     .
 S200-99.
     EXIT.

******************************************************************
* OPEN Cursor
******************************************************************
 S900-OPEN-FCPARAM-CURSOR SECTION.
 S900-00.
     MOVE ZERO TO C9-COUNT
     EXEC SQL
         OPEN FCPARAM_CURS
     END-EXEC
     .
 S900-99.
     EXIT.

******************************************************************
* Fetch aus Tabelle FCPARAM
******************************************************************
 S910-FETCH-FCPARAM-CURSOR SECTION.
 S910-00.
     EXEC SQL
         FETCH FCPARAM_CURS
          INTO  :ROUTKZ of FCPARAM
               ,:CARDID of FCPARAM
               ,:ISONTYP of FCPARAM
               ,:KZ-MSG of FCPARAM
               ,:BMP of FCPARAM
               ,:LFDNR of FCPARAM
               ,:KZ-ABWEICHUNG of FCPARAM
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET FCPARAM-OK  TO TRUE
                     ADD 1 TO C9-COUNT
         WHEN 100    SET FCPARAM-EOD TO TRUE
         WHEN OTHER  SET FCPARAM-NOK TO TRUE
     END-EVALUATE
     .
 S910-99.
     EXIT.

******************************************************************
* CLOSE Cursor
******************************************************************
 S920-CLOSE-FCPARAM-CURSOR SECTION.
 S920-00.
     EXEC SQL
         CLOSE FCPARAM_CURS
     END-EXEC
     .
 S920-99.
     EXIT.

******************************************************************
* OPEN Cursor
******************************************************************
 S930-OPEN-KEYNAMEN-CURSOR SECTION.
 S930-00.
     MOVE ZERO TO C9-COUNT
     EXEC SQL
         OPEN KEYNAMEN_CURS
     END-EXEC
     .
 S930-99.
     EXIT.

******************************************************************
* Fetch aus Tabelle KEYNAMEN
******************************************************************
 S940-FETCH-KEYNAMEN-CURSOR SECTION.
 S940-00.
     EXEC SQL
         FETCH KEYNAMEN_CURS
          INTO  :ROUTKZ of KEYNAMEN
               ,:CARDID of KEYNAMEN
               ,:KEYNAME of KEYNAMEN
               ,:ISOGEN of KEYNAMEN
               ,:ISOVERS of KEYNAMEN
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN 0      SET KEYNAMEN-OK  TO TRUE
                     ADD 1 TO C9-COUNT
         WHEN OTHER  SET KEYNAMEN-NOK TO TRUE
     END-EVALUATE
     .
 S940-99.
     EXIT.

******************************************************************
* CLOSE Cursor
******************************************************************
 S950-CLOSE-KEYNAMEN-CURSOR SECTION.
 S950-00.
     EXEC SQL
         CLOSE KEYNAMEN_CURS
     END-EXEC
     .
 S950-99.
     EXIT.

******************************************************************
* OPEN Cursor
******************************************************************
 S960-OPEN-IFSFAC-CURSOR SECTION.
 S960-00.
     MOVE ZERO TO C9-COUNT
     EXEC SQL
         OPEN IFSFAC_CURS
     END-EXEC
     .
 S960-99.
     EXIT.

******************************************************************
* Fetch aus Tabelle IFSFAC
******************************************************************
 S970-FETCH-IFSFAC-CURSOR SECTION.
 S970-00.
     EXEC SQL
         FETCH IFSFAC_CURS
          INTO  :IFSF-AC of IFSFAC
               ,:WEAT-AC of IFSFAC
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN 0      SET IFSFAC-OK  TO TRUE
                     ADD 1 TO C9-COUNT
         WHEN OTHER  SET IFSFAC-NOK TO TRUE
     END-EVALUATE
     .
 S970-99.
     EXIT.

******************************************************************
* CLOSE Cursor
******************************************************************
 S980-CLOSE-IFSFAC-CURSOR SECTION.
 S980-00.
     EXEC SQL
         CLOSE IFSFAC_CURS
     END-EXEC
     .
 S980-99.
     EXIT.

******************************************************************
* Transaktionsbegrenzungen
******************************************************************
 U100-BEGIN SECTION.
 U100-00.
     EXEC SQL
         BEGIN WORK
     END-EXEC
     .
 U100-99.
     EXIT.

 U101-BEGINTRANSACTION SECTION.
 U101-00.
     ENTER TAL BEGINTRANSACTION
     .
 U101-99.
     EXIT.

 U110-COMMIT SECTION.
 U110-00.
     EXEC SQL
         COMMIT WORK
     END-EXEC
     .
 U110-99.
     EXIT.

 U111-ENDTRANSACTION SECTION.
 U111-00.
     ENTER TAL ENDTRANSACTION
        .
 U111-99.
     EXIT.

 U120-ROLLBACK SECTION.
 U120-00.
     EXEC SQL
         ROLLBACK WORK
     END-EXEC
     .
 U120-99.
     EXIT.

 U121-ABORTTRANSACTION SECTION.
 U121-00.
     ENTER TAL ABORTTRANSACTION
     .
 U121-99.
     EXIT.

******************************************************************
* TIMESTAMP erstellen
******************************************************************
 U200-TIMESTAMP SECTION.
 U200-00.
     ENTER TAL "TIME" USING TAL-TIME
     MOVE CORR TAL-TIME TO TAL-TIME-D

     MOVE TAL-JHJJ OF TAL-TIME-D TO TD-JHJJ
     MOVE TAL-MM   OF TAL-TIME-D TO TD-MM
     MOVE TAL-TT   OF TAL-TIME-D TO TD-TT
     MOVE TAL-HH   OF TAL-TIME-D TO TD-HH
     MOVE TAL-MI   OF TAL-TIME-D TO TD-MI
     MOVE TAL-SS   OF TAL-TIME-D TO TD-SS
     MOVE TAL-HS   OF TAL-TIME-D TO TD-HS
     .
 U200-99.
     EXIT.

******************************************************************
* suchen Eintrag aus Parametertabelle
*        Eingabe:    S-ISONTYP - Nachrichtentyp nach dem gesucht wird
*                    S-KZ-MSG  - Kennzeichen nach dem gesucht wird
*                    S-BMP     - BMP nach dem gesucht wird
*                    S-LFDNR   - LFDNR nach der gesucht wird (Start=0)
*        Ausgabe:    T-AKT-IND - Index der Fundstelle
*                    PRM-FOUND     - true or false
*                    PRM-NOT-FOUND - false of true
******************************************************************
 U300-SEARCH-TAB SECTION.
 U300-00.
*kl20170404 - G.03.14 - wg. Cardid ZERO zurueck zur klassischen
*                       Verarbeitung
*     MOVE ZERO TO T-AKT-IND
*
*     SEARCH ALL T-FCPARAM-TAB
*
*         AT END  SET PRM-NOT-FOUND TO TRUE
*                 EXIT SECTION
*
*         WHEN    T-KEY (IND-TAB) = S-SEARCH-KEY
*
*                 SET PRM-FOUND TO TRUE
*                 SET T-AKT-IND TO IND-TAB
*                 continue
*
*     END-SEARCH

*-->  SEARCH-KEY 2 besetzen
*     �bernehmen kompletten 1. Key
     MOVE    S-SEARCH-KEY       TO S2-SEARCH-KEY
*     CARDID ersezten (ggf. auch f�r andere Werte m�glich)
     MOVE    ZERO               TO S2-CARDID

*--> Suchschleife

*    Default: Nicht gefunden
     SET PRM-NOT-FOUND TO TRUE

     PERFORM VARYING T-AKT-IND from 1 BY 1  UNTIL T-AKT-IND > T-MAX
                                               OR PRM-FOUND

        IF T-KEY(T-AKT-IND) = S-SEARCH-KEY
        OR T-KEY(T-AKT-IND) = S2-SEARCH-KEY
           SET PRM-FOUND    TO TRUE
           EXIT PERFORM
        END-IF

     END-PERFORM
*kl20180404 - G.02.10 - Ende
     .
 U300-99.
     EXIT.

******************************************************************
* Interpretieren KZ-ABWEICHUNG
******************************************************************
 U400-INTERPRET-ABWEICHUNG SECTION.
 U400-00.
**  ---> Vorschrift separieren
     MOVE ZERO TO C4-ANZ
     MOVE 1    TO C4-PTR
     MOVE SPACES     TO W-TEILSTRING-TABELLE
     MOVE SPACES     TO W-DELIM-TABELLE
     MOVE LOW-VALUES TO W-COUNT-TABELLE

     UNSTRING T-KZ-ABWEICHUNG (T-AKT-IND)
                                delimited by ALL SPACE or "F" or "@" or "T"
         INTO W-TEILSTRING (1)  delimiter in W-DELIM (1)
                                count     in W-COUNT (1)
              W-TEILSTRING (2)  delimiter in W-DELIM (2)
                                count     in W-COUNT (2)
              W-TEILSTRING (3)  delimiter in W-DELIM (3)
                                count     in W-COUNT (3)
              W-TEILSTRING (4)  delimiter in W-DELIM (4)
                                count     in W-COUNT (4)
              W-TEILSTRING (5)  delimiter in W-DELIM (5)
                                count     in W-COUNT (5)
              W-TEILSTRING (6)  delimiter in W-DELIM (6)
                                count     in W-COUNT (6)
              W-TEILSTRING (7)  delimiter in W-DELIM (7)
                                count     in W-COUNT (7)
              W-TEILSTRING (8)  delimiter in W-DELIM (8)
                                count     in W-COUNT (8)
              W-TEILSTRING (9)  delimiter in W-DELIM (9)
                                count     in W-COUNT (9)
              W-TEILSTRING (10) delimiter in W-DELIM (10)
                                count     in W-COUNT (10)
         WITH     POINTER C4-PTR
         TALLYING IN      C4-ANZ
     END-UNSTRING

**  ---> Trennzeichen in W-DELIM      von Index 1
**  ---> zugeh. Wert  in W-TEILSTRING von Index 2

* Beispiel:
*    Source-String   : T2F=T14  (Terminal-BMP2 / fix "=" / Terminal-BMP14
*
*     ---> Teil-String  1 :                  Laenge: 0000 Trenner: T
*     ---> Teil-String  2 : 2                Laenge: 0001 Trenner: F
*     ---> Teil-String  3 : =                Laenge: 0001 Trenner: T
*     ---> Teil-String  4 : 14               Laenge: 0002 Trenner:
*     ---> Teil-String  5 :                  Laenge: 0000 Trenner:
*
*    C4-ANZ: Anzahl belegter Teilstrings

     IF  C4-ANZ = ZERO
**      ---> kein Inhalt
         EXIT SECTION
     END-IF

     MOVE SPACES TO W-BUFFER
     MOVE 1 TO C4-I2
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   C4-I1 > C4-ANZ

         EVALUATE W-DELIM (C4-I1)

             WHEN "F"    MOVE W-TEILSTRING(C4-I1 + 1) (1:W-COUNT(C4-I1 + 1))
                             TO W-BUFFER (C4-I2:W-COUNT(C4-I1 + 1))
                         ADD W-COUNT(C4-I1 + 1) TO C4-I2

             WHEN "T"    MOVE W-TEILSTRING(C4-I1 + 1) (1:W-COUNT(C4-I1 + 1))
                             TO D-NUM4
                         MOVE D-NUM4 TO C4-I3
                         MOVE IMSG-CF(IMSG-TPTR(C4-I3):IMSG-TLEN(C4-I3))
                             TO W-BUFFER (C4-I2:IMSG-TLEN(C4-I3))
*                             TO W-BUFFER (C4-I2:W-COUNT(C4-I1 + 1))
                         COMPUTE C4-I2 = C4-I2 + IMSG-TLEN(C4-I3)
*                         ADD W-COUNT(C4-I1 + 1) TO C4-I2

             WHEN "@"    continue
             WHEN space  continue

             WHEN OTHER  SET ENDE TO TRUE
*G.03.01 - VERF-AS jetzt
*                         MOVE W-ROUTKZ TO D-NUM4
                         MOVE VERF-AS TO D-NUM4
*G.03.01 - Ende
                         STRING  "Unbekannte Verarbeitungsregeln "
                                 "f�r Rout-KZ = "
                                 D-NUM4
                                     delimited by size
                           INTO  DATEN-BUFFER1
                         END-STRING
                         PERFORM Z002-PROGERR
                         EXIT SECTION

         END-EVALUATE

     END-PERFORM

     COMPUTE W-BUFFER-LEN = C4-I2 - 1

     continue
     .
 U400-99.
     EXIT.

******************************************************************
* SQL-Fehlerbehandlung
******************************************************************
 Z001-SQLERROR SECTION.
 Z001-00.

**  ---> holen Daten f�r Fehlertabelle
     MOVE 1 TO ERR-STAT OF GEN-ERROR

     IF  IMSG-MDNR NUMERIC
         MOVE IMSG-MDNR TO MDNR OF GEN-ERROR
     ELSE
         MOVE ZERO      TO MDNR OF GEN-ERROR
     END-IF
     IF  IMSG-TSNR NUMERIC
         MOVE IMSG-TSNR TO TSNR OF GEN-ERROR
     ELSE
         MOVE ZERO      TO TSNR OF GEN-ERROR
     END-IF

     MOVE K-MODUL TO MODUL-NAME OF GEN-ERROR
     MOVE "SE"    TO ERROR-KZ   OF GEN-ERROR

**  ---> Einstellen in Fehlertabelle
     PERFORM Z999-ERRLOG
     SET ENDE TO TRUE
     .
 Z001-99.
     EXIT.

******************************************************************
* Programm-Fehlerbehandlung
******************************************************************
 Z002-PROGERR SECTION.
 Z002-00.
**  ---> Angaben f�r Terminal- und Trace-Nummer in BUFFER5 einstellen
*G.03.01 - Fehlermeldung erweitert
     MOVE W-CARDID TO D-NUM2
     MOVE W-ROUTKZ TO D-NUM4
     STRING  "TermNr/TraceNr/RoutKZ/Verf/Cardid: "
             W-TERMNR "/" W-TRACENR "/" D-NUM4 "/"
             VERF-AS  "/" D-NUM2
                 delimited by size
       INTO  DATEN-BUFFER5
     END-STRING
*G.03.01 - Ende

**  ---> holen Daten f�r Fehlertabelle
     MOVE 1 TO ERR-STAT OF GEN-ERROR

     IF  IMSG-MDNR NUMERIC
         MOVE IMSG-MDNR TO MDNR OF GEN-ERROR
     ELSE
         MOVE ZERO      TO MDNR OF GEN-ERROR
     END-IF
     IF  IMSG-TSNR NUMERIC
         MOVE IMSG-TSNR TO TSNR OF GEN-ERROR
     ELSE
         MOVE ZERO      TO TSNR OF GEN-ERROR
     END-IF

     MOVE K-MODUL TO MODUL-NAME OF GEN-ERROR
     MOVE "PE"    TO ERROR-KZ   OF GEN-ERROR

**  ---> Einstellen in Fehlertabelle
     PERFORM Z999-ERRLOG
     .
 Z002-99.
     EXIT.

******************************************************************
* Fehler in Tabelle ERRLOG schreiben
******************************************************************
 Z999-ERRLOG SECTION.
 Z999-00.
**  ---> Einstellen in Fehlertabelle
     CALL "WSYS022" USING GEN-ERROR
                          SQLCA
     INITIALIZE GEN-ERROR
     .
 Z999-99.
      EXIT.

******************************************************************
* ENDE Source-Programm
******************************************************************
