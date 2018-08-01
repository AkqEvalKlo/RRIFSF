?CONSULT $SYSTEM.SYSTEM.COBOLEX0
?SEARCH  $SYSTEM.SYSTEM.COBOLLIB
?SEARCH  =TALLIB
?SEARCH  =ASC2EBC
?SEARCH  =EBC2ASC
*G.02.00 - Anfang
?SEARCH  =IUMSW07
*G.02.00 - Ende
*G.02.26 - Anfang
?SEARCH  =SYSABL1
*G.02.26 - Ende
?SEARCH  =SDBCDU5
?SEARCH  =SYSAWKZ
?SEARCH  =SYSMK7I
?SEARCH  =WCAPM92
?SEARCH  =WISO107
?SEARCH  =WISO207
?SEARCH  =WSYS020
?SEARCH  =WSYS022
?SEARCH  =WSY7065
?SEARCH  =WSY7066
?SEARCH  =WSYS930
?SEARCH  =WSYS971
?SEARCH  =WSYS980
?SEARCH  =WSYS990

*G.02.14 - Anfang
?SEARCH  =WCSI060
?SEARCH  =WEUR055
?SEARCH  =WEUR056
*G.02.14 - Ende
*G.02.44 - Modul für Zusammenstellung ZP-VERKAUF
?SEARCH =ZPVERK
*G.02.44 - ende


?NOLMAP, SYMBOLS, INSPECT
?SAVE ALL
?SAVEABEND
?LINES 66
?CHECK 3
?SQL


 IDENTIFICATION DIVISION.

 PROGRAM-ID. PFCFAD7S.

 DATE-COMPILED.


******************************************************************
* Letzte Aenderung :: 2018-07-27
* Letzte Version   :: G.02.50
* Kurzbeschreibung :: Umsetzung Flottenkarten-Teil-
* Kurzbeschreibung :: Stornierungsanfragen vom Trm-Protokoll
* Kurzbeschreibung :: auf AS0IFSF-Protokoll um. Bearbeitet
* Kurzbeschreibung :: werden nur Terminalnachrichten von Typ
* Kurzbeschreibung :: 400/AbWkz=95, die auf AS-Nachrichten
* Kurzbeschreibung :: vom Typ 1220 umgesetzt werden.
* Package          :: ICC
* Auftrag          :: RRIFSF-3


*
* Aenderungen
*
*----------------------------------------------------------------*
* Vers. | Datum    | von | Kommentar                             *
*-------|----------|-----|---------------------------------------*
*G.02.50|2018-07-27| kus | R7-364:
*       |          |     | - Antworten bei 400er mit fehlendem 
*       |          |     |   UMSWEAT Eintrag
*-------|----------|-----|---------------------------------------*
*G.02.49|2018-05-28| kus | RRIFSF-5:
*       |          |     | - Umsetzung Roadrunner (Routkz = 25)
*-------|----------|-----|---------------------------------------*
*G.02.48|2018-05-25| kus | F1ICC-114:
*       |          |     | - BMP 56 mit BMP12+13 (ZP-VERKAUF) aus
*       |          |     |   Anfrage fuellen
*-------|----------|-----|---------------------------------------*
*G.02.47|20180518  | kl  | Neukompilierung wg. Korrektur ZPVERKM
*       |          |     | (ZP-VERKAUF Modul)
*-------|----------|-----|---------------------------------------*
*G.02.46|2018-04-30| kus | F1ICC-106:
*       |          |     | - AS BMP 12 fuellen mit ZP vom Terminal
*       |          |     |   dafuer ZP-VERKAUF verwenden (ist
*       |          |     |   Kombi aus Terminal BMP 12/13)
*-------|----------|-----|---------------------------------------*
*G.02.45|2018-04-11| kus | IQ-16:
*       |          |     | - IQ: BMP 39 mit AC aus Voraut fuellen
*       |          |     | IQ-17:
*       |          |     | - IQ: ANZ-REP ASYNC70 fuellen ueber
*       |          |     |   Serverklassen Parameter wie
*       |          |     |   im PFCSTO7S
*-------|----------|-----|---------------------------------------*
*G.02.44|2018-04-10| sk  | Jira R7-302: Modulaufruf f. zp_verkauf
*       |          |     | Berechnung
*-------|----------|-----|---------------------------------------*
*G.02.43|2018-04-05| kl  | R7-272:
*       |          |     | Optimierung Zugriff / Laden FCPARAM
*       |          |     | (CARDID = 0 oder X - Vorrang bei X)
*-------|----------|-----|---------------------------------------*
*G.02.42|2018-03-27| kus | R7-328:
*       |          |     | - bei fehlendem UMSWEAT Eintrag auch
*       |          |     |   neue Wiederholer Logik ermoeglichen
*-------|----------|-----|---------------------------------------*
*G.02.41|2018-03-22| kus | IQ-13:
*       |          |     | - Feld 48-39 mit Belegnr fuellen
*-------|----------|-----|---------------------------------------*
*G.02.40|2018-03-16| kl  | Bei Chip-Transaktionen Kartenart = 211
*       |          |     | setzen; BASIS: G.02.38
*       |          |     | (DKVCHIP-21)
*-------|----------|-----|---------------------------------------*
*G.02.39|2018-03-16| kl  | VERWORFEN: Falsche Implementierung
*       |          |     |            R7-269
*-------|----------|-----|---------------------------------------*
*G.02.38|2018-02-01| hkn | POST70 entfernt, da nicht benutzt     *
*-------|----------|-----|---------------------------------------*
*G.02.37|2018-01-23| hkn | IQ-6 Neu: Stiglechner mit Routkz = 24 *
*-------|----------|-----|--- -----------------------------------*
*G.02.36|2018-01-09| kl  | Initialierung T-MAX in B000 optimiert *
*-------|----------|-----|--- -----------------------------------*
*G.02.35|2018-01-05| kl  | Speichertabelle fuer FCPARAM          *
*       |          |     | vergroessert                          *
*-------|----------|-----|---------------------------------------*
*G.02.34|2018-01-04| hkn | R7-Version aus X-Version kopiert      *
*-------|----------|-----|---------------------------------------*
*G.02.33|2017-12-27| kus | R7-235:                               *
*       |          |     | - neue Logik fuer Wiederholungen      *
*-------|----------|-----|---------------------------------------*
*G.02.32|2017-09-29| kl  | STATION.mdnr/tsnr besetzen für        *
*       |          |     | SELECT STATION                        *
*-------|----------|-----|---------------------------------------*
*G.02.31|2017-09-11| hkn | Erkennung INDOOR/OUTDOOR: Mit         *
*       |          |     | TXILOG70.Betrag_Art = "M" zusätzlich  *
*       |          |     | zur Prüfung auf BMP 25                *
*-------|----------|-----|---------------------------------------*
*G.02.30|2017-08-09| hkn | TXILOG70.AC-TERM aus TABL-ABL,        *
*       |          |     | wenn TABL-ABL gesetzt                 *
*-------|----------|-----|---------------------------------------*
*G.02.29|2017-07-25| hkn | Nachrichtenprüfung (WSYS971):         *
*       |          |     | Im Fehlerfall AC 30 und Antwort an TS.*
*       |          |     | Achtung nach der Nachrichtenprüfung   *
*       |          |     | mit Rücksprung muss Existenzprüfung   *
*       |          |     | auf BMP erfolgen, sonst Abruch        *
*-------|----------|-----|---------------------------------------*
*G.02.28|2017-07-05| hkn | Ermitteln der Kartenlänge in          *
*       |          |     | D900-ROUTING-ETC mit Ermitteln der    *
*       |          |     | Karte zusammengelegt.                 *
*       |          |     | D900-ROUTING-ETC vor dem ersten       *
*       |          |     | AC ungleich Null.                     *
*       |          |     | CARDUSEDN-Eintrag nur für KANR-LEN    *
*       |          |     | ungleich Null                         *
*-------|----------|-----|---------------------------------------*
*G.02.27|2017-07-03| hkn | AC der Vorautorisierung nicht 0 oder  *
*       |          |     | 2, dann AC 21 zurück an das TS        *
*-------|----------|-----|---------------------------------------*
*G.02.26|2017-06-15| hkn |Ausstehende Aktionen aus TERMABL:      *
*       |          |     |Nicht mehr mit der Vorautorisierung,   *
*       |          |     |da die TRX erst mit der Teilstornierung*
*       |          |     |abgeschlossen ist.                     *
*-------|----------|-----|---------------------------------------*
*G.02.25|2017-06-14| kus | BMP 43 fuer SHELL aus =STATION        *
*-------|----------|-----|---------------------------------------*
*G.02.24|2017-06-12| hkn | BMP38: Senden an alle AS-Systeme      *
*-------|----------|-----|---------------------------------------*
*G.02.23|2017-06-02| das | Verkürztes BMP59 erst mal nur Shell   *
*-------|----------|-----|---------------------------------------*
*G.02.22|2017-05-24| kus |- Anpassung TXILOG70 TRANS-ART aus Aut
*-------|----------|-----|---------------------------------------*
*G.02.21|2017-05-05| kus |Konditionscode (BMP25) pruefen, ob 27  *
*-------|----------|-----|---------------------------------------*
*G.02.20|2017-02-21| hkn |Total: BMP 38 schicken                 *
*       |          |     |AC - 64 an TS                          *
*-------|----------|-----|---------------------------------------*
*G.02.19|2017-02-17| hkn |Teilstorno > autorisierter Betrag:     *
*       |          |     |AC - 64 an TS                          *
*-------|----------|-----|---------------------------------------*
*G.02.18|2017-02-16| hkn | D318-TND: BMP48-Default ergänzt
*-------|----------|-----|---------------------------------------*
*G.02.17|2017-02-02| hkn | FCPARM ersetzt durch FCPARAM
*-------|----------|-----|---------------------------------------*
*G.02.16|2017-01-05| HJO | Trotz AS-Sperre AC 00 ans Terminal und
*       |          |     | Async70 TX für Nachbucher erzeugen
*-------|----------|-----|---------------------------------------*
*G.02.15|2016-12-05| hkn | Nachricht 1220 mit BMP 38
*-------|----------|-----|---------------------------------------*
*G.02.14|2016-11-09| hkn | EUROWAG: PAC Umschlüssen und Bilden
*       |          |     | mit DUKPT-Verfahren
*-------|----------|-----|---------------------------------------*
*G.02.13|2016-09-21| hkn | Neu: LogPay mit Routkz = 23
*-------|----------|-----|---------------------------------------*
*G.02.12|2016-09-16| hkn |Wenn eine EMV-Konfig vorliegt, dann
*       |          |     |Rückgabewert nicht numerisch.
*       |          |     |Absturz der Authorisierung -
*       |          |     |Fehler tritt seit G.01.02 auf.
*       |          |     |Korrektur: C100-ANFRAGE-CHECK:
*       |          |     |Prüfung AS-AC geändert
*       |          |     |
*-------|----------|-----|---------------------------------------*
*G.02.11|2016-09-13| hkn | D310-TOTAL: Keine spezielle
*       |          |     | Bearbeitung BMP 42 - VUNR
*-------|----------|-----|---------------------------------------*
*G.02.10|2016-08-17| hkn | D314-BP: Macbildung - W66-DEFAULT
*-------|----------|-----|---------------------------------------*
*G.02.09|2016-08-10| hkn | Neu: Eurowag mit Routkz = 22
*-------|----------|-----|---------------------------------------*
*G.02.08|2016-07-27| hkn | D314-BP- BMP42: Positionierung korrig.
*-------|----------|-----|---------------------------------------*
*G.02.07|2016-07-05| hkn | AS-Kürzel nach AMP-FORMAT
*-------|----------|-----|---------------------------------------*
*G.02.06|2016-06-29| hkn | TXILOG70.ARTIKEL.LEN aus IMSG,
*       |          |     | TXILOG70.TRACENR.VAL aus IMSG
*       |          |     | füllen und speichern
*       |          |     | TXILOG70.KZ-BEARB  = "R"
*       |          |     | TXILOG70.KZ-UMSATZ = "Z"
*-------|----------|-----|---------------------------------------*
*G.02.05|2016-06-24| hkn | TXILOG70.TRACENR-S der Stornierung aus
*       |          |     | TXILOG70.TRACENR der Autorisieung
*       |          |     | füllen und update
*-------|----------|-----|---------------------------------------*
*G.02.04|2016-06-14| hkn | Teilstorno gegen Null, Löschen Umsweat
*-------|----------|-----|---------------------------------------*
*G.02.03|2016-06-03| das | Update auf UMSWEAT muss mit Vorauto.
*       |          |     | Belegnr aus BMP37 gemacht werden
*-------|----------|-----|---------------------------------------*
*G.02.02|2016-05-11| hkn |Nrcht: 410 - BMP37 gelöscht
*-------|----------|-----|---------------------------------------*
*G.02.01|2016-04-26| hkn |Umsatzverarbeitung: aktiv gesetzt
*-------|----------|-----|---------------------------------------*
*G.02.00|2016-04-26| hkn |Eigenantwort: UmTXNLOG70-TS.ANTWORT
*       |          |     |erweitert
*       |          |     |BMP 42 in Eigenantwort gelöscht
*       |          |     |Umsweatverarbeitung: Eingefügt
*       |          |     |TXNLOG70-AS gelöscht
*       |          |     |Eigenantwort-OK - und NOK
*       |          |     |Memlog schreiben gelöscht
*-------|----------|-----|---------------------------------------*
*G.00.02|2016-04-15| hkn | Nachricht 1220: BMP 41 nicht an UTA
*       |          |     |               : BMP 39 an UTA
*-------|----------|-----|---------------------------------------*
*G.00.01|2015-11-15| hkn | a) TRX-Anfrage in ASYNC70 eintragen
*       |          |     | b) Eigenantwort an TS
*       |          |     | c) Keine Nachricht an AS
*       |          |     |
*-------|----------|-----|---------------------------------------*
*G.00.00|2015-06-01| BAH | Neuerstellung
*----------------------------------------------------------------*
*
* Programmbeschreibung
* --------------------
*
* Das Programm setzt Flottenkarten-Terminal-Teilstornierungs-Anfragen
* auf IFSF-Anfragen für ein spezielles AS um. Dabei muss die Umsetzung
* für jedes AS programmiert werden. Umsetzungsregeln können in der
* Tabelle =FCPARAM hinterlegt werden. Vorgesehen sind zunächst die
* folgenden Flottenkartenautorisierungssysteme:
*
*    -   Avia    (erste Ausbaustufe)
*    -   Shell
*    -   Total
*    -   DKV
*    -   BP
*    -   ENI
*    -   Orlen
*    -   UTA
*    -   TND
*    -   EUROWAG
*    -   LOGPAY
*    -   STIGLECHNER
*
* Für jedes AS muss eine eigene Serverklasse mit diesem Programm
* definiert werden (z.B. PFCFAD7S-05 für Avia). Die Keys für das
* jeweilige AS werden in der Tabelle =KEYNAMEN erwartet.
*
* Es wird keine Nachricht an den AS geschickt. Die TS-Anfrage wird
* hingegen in die Tabelle =ASYNC70 eingetragen, die zeitversetzt
* durch NABUI abgearbeitet wird, und somit wird das Storni dann
* an den AS geschickt.
*
* Das Programm erwartet die folgenden Parameter:
*
*    -   AS-ROUTKZ   Routkennzeichen für das relevante AS
*    -   MACKEYT     MAC-Schlüssel für Terminaltransaktionen
*    -   PACKEYT     PAC-Schlüssel für Terminaltransaktionen
*    -   BOXMON      Festlegung des Boxenservers
*    -   ARTMAP      Festlegung des zuständigen Artikel-Mappers
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
* C100-ANFRAGE-CHECK
* C200-AS-GENERELL
* C300-AS-SPEZIELL
* C400-BUILD-AS-NACHRICHT
*
* D200-FIX-KEY
* D305-AVIA
* D307-SHELL
* D310-TOTAL
* D312-DKV
* D314-BP
* D315-ENI
* D316-ORLEN
* D317-UTA
* D318-TND
* D322-EUROWAG
* D323-LOGPAY
* D324-STIGLECHNER
* D325-ROADRUNNER
* D900-ROUTING-ETC
* D910-GET-ASTRACENR
*
* E100-FEP-ANTWORT
* E305-01-ARTIKELDATEN
* E310-BMP48-DEFAULT
* E900-PUT-ERRLOG
*
*G02-00 - Anfang
* F100-LOGDATEN-EIGEN-OK
* F110-LOGDATEN-EIGEN-NOK
*G02-00 - Ende

* G090-PUT-ASNYC70
* F920-MAC-BILDEN
*
*G.02.14 - Anfang
* F950-ASMAC-DUKPT
*G.02.14 - Ende
*
*G.02.05 - Anfang
* G102-PUT-TXILOG70-AUT
*G.02.05 - Ende

* G100-PUT-TXILOG70
* G110-PUT-TXNLOG70-TS
* G105-PUT-TXNLOG70-TS-ANTWORT
*G.02.00 - Anfang
* G125-PUT-UMSWEAT
*G.02.00 - Ende
* G130-PUT-CRDUSEDN
*
* L100-ADD-BMP
* L110-COB2ISO
*
* M100-CALL-SDBCDU5
* M120-CALL-WCAPM92
* M130-CALL-WISO207-W207
* M135-CALL-WISO207-WATS
* M140-CALL-WSY7066
* M150-CALL-WSYS930
* M160-CALL-WSYS971
*G.02.00 - Anfang
* M180-CALL-IUMSW07
*G.02.00 - Ende
* M170-CALL-SYSAWKZ*
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
* S110-SELECT-MDNR2AS
* S120-UPDATE-MDNR2AS
* S150-SELECT-TSKART40
* S160-SELECT-STATION
* S170-ASYNC70-INSERT
* S180-INSERT-TXILOG70
* S190-INSERT-TXNLOG70-TS
* S200-SELECT-TXILOG70-AUT

*G.02.05 - Anfang
* S185-UPDATE-TXILOG70-AUT
*G.02.05 - Ende

* S210-SELECT-TXNLOG70-ATS
* S900-OPEN-FCPARAM-CURSOR
* S910-FETCH-FCPARAM-CURSOR
* S920-CLOSE-FCPARAM-CURSOR
* S930-OPEN-KEYNAMEN-CURSOR
* S940-FETCH-KEYNAMEN-CURSOR
* S950-CLOSE-KEYNAMEN-CURSOR
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
* V400-WT-HEX-STRING
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
                      " .,;-_!§$%&/=*+"
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
* Comp-Felder: Präfix Cn mit n = Anzahl Digits
*--------------------------------------------------------------------*
 01          COMP-FELDER.
     05      C4-ANZ              PIC S9(04) COMP.
     05      C4-COUNT            PIC S9(04) COMP.
     05      C4-I1               PIC S9(04) COMP.
     05      C4-I2               PIC S9(04) COMP.
     05      C4-I3               PIC S9(04) COMP.
     05      C4-I4               PIC S9(04) COMP.
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
     05      C18-BETRAG          PIC S9(18) COMP.

     05      REPLY-LAENGE        PIC  9(04) COMP.

*--------------------------------------------------------------------*
* Display-Felder: Präfix D
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
     05      D-NUM10             PIC  9(10).
     05      D-NUM12             PIC  9(12).
     05      D-NUM18             PIC  9(18).
     05      D-NUM18X redefines D-NUM18.
         10  RD-NUM18-6          PIC 9(06).
         10  RD-NUM18-12         PIC 9(12).

*--------------------------------------------------------------------*
* Felder mit konstantem Inhalt: Präfix K
*--------------------------------------------------------------------*
 01          KONSTANTE-FELDER.
     05      K-MODUL             PIC X(08)          VALUE "PFCFAD7S".

*G.02.24 - Anfang
**          ---> Pflichtfelder einer 1220-AS-Nachricht
*    05      K-BYTEMAP-A1220      PIC X(64) VALUE
*    "0011001000110000000001011100000100000000110000011000000000100010".
**             1         2         3         4         5         6
**    1234567890123456789012345678901234567890123456789012345678901234
***
**          ---> Pflichtfelder einer 1220-AS-Nachricht
     05      K-BYTEMAP-A1220      PIC X(64) VALUE
     "0011001000110000000001011100000100000100110000011000000000100010".
**             1         2         3         4         5         6
**    1234567890123456789012345678901234567890123456789012345678901234
*G.02.24 - Anfang


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

     05      ASYNC70-FLAG        PIC 9       VALUE ZERO.
          88 ASYNC70-OK                      VALUE ZERO.
          88 ASYNC70-NOK                     VALUE 1.

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

     05      MDNR2AS-FLAG        PIC 9       VALUE ZERO.
          88 MDNR2AS-OK                      VALUE ZERO.
          88 MDNR2AS-NOK                     VALUE 1.

     05      TSKART40-FLAG       PIC 9       VALUE ZERO.
          88 TSKART40-OK                     VALUE ZERO.
          88 TSKART40-NOK                    VALUE 1.

     05      STATION-FLAG        PIC 9       VALUE ZERO.
          88 STATION-OK                      VALUE ZERO.
          88 STATION-NOK                     VALUE 1.

     05      STATIONA-FLAG       PIC 9       VALUE ZERO.
          88 STATIONA-OK                     VALUE ZERO.
          88 STATIONA-NOK                    VALUE 1.

     05      TXILOG70-FLAG       PIC 9       VALUE ZERO.
          88 TXILOG70-OK                     VALUE ZERO.
          88 TXILOG70-NOK                    VALUE 1.

*G.02.05 - Anfang
     05      TXILOG70-FLAG-AUT   PIC 9       VALUE ZERO.
          88 TXILOG70-OK-AUT                 VALUE ZERO.
          88 TXILOG70-NOK-AUT                VALUE 1.
*G.02.05 - Ende

     05      TXNLOG70-FLAG       PIC 9       VALUE ZERO.
          88 TXNLOG70-OK                     VALUE ZERO.
          88 TXNLOG70-NOK                    VALUE 1.

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

     05      STORNO-WFLAG        PIC X     VALUE LOW-VALUE.
          88 STORNO-WERSTE                 VALUE LOW-VALUE.
          88 STORNO-WIEDERHOLUNG           VALUE HIGH-VALUE.

     05      FEP-ANTWORT-FLAG    PIC X     VALUE LOW-VALUE.
          88 NO-FEP-ANTWORT                VALUE LOW-VALUE.
          88 FEP-ANTWORT                   VALUE HIGH-VALUE.

     05      KFZ-FLAG            PIC X     VALUE LOW-VALUE.
          88 KFZ-NO                        VALUE LOW-VALUE.
          88 KFZ-YES                       VALUE HIGH-VALUE.

*G.02.33 - neue Schalter fuer Wdh. Steuerung
     05      WDH-FLAG            PIC X     VALUE LOW-VALUE.
          88 WDH-FIRST                     VALUE LOW-VALUE.
          88 WDH-VORHANDEN                 VALUE HIGH-VALUE.

     05      UMSATZ-FLAG         PIC X     VALUE LOW-VALUE.
          88 UMS-NEU                       VALUE LOW-VALUE.
          88 UMS-DONE                      VALUE HIGH-VALUE.
*G.02.33 - Ende

*--------------------------------------------------------------------*
* weitere Arbeitsfelder
*--------------------------------------------------------------------*
**          ---> unverändert
 01          WORK-FELDER.
     05      W-ROUTKZ            PIC S9(04) COMP.
     05      W-KEYNAME           PIC  X(08).
     05      W-ISOGEN-VERS       PIC  X(02).
*G.02.45 - eingelesene Parameter fuer ANZ-REP
     05      W-ANZREPA           PIC S9(04) COMP.
     05      W-ANZREPW           PIC S9(04) COMP.
*G.02.45 - Ende

**          ---> werden bei jeder Tx initiert
 01          WORK-INIT.
     05      W-CARDID            PIC S9(04) COMP.
     05      W-KANR-LEN          PIC S9(04) COMP.
     05      W18-BETRAG          PIC S9(16)V99 COMP.
     05      W-ZP-VERKAUF        PIC S9(18) COMP.

     05      W-ACX.
      10     W-AC                PIC 9(02).

*G.02.12 - Anfang
     05 PRF-AC               PIC X(03).
        88 PRF-AC-OK         VALUE "00 ", "10 ", "A0 ",
                                   "A1 ", "A2 ", "A3 ",
                                   "A4 ", "A5 ", "A6 ",
                                   "A7 ", "A8 ", "A9 ".
*G.02.12 - Ende

     05      W-ACQUIRER-ID       PIC X(06).
     05      W-AS-TRACENR        PIC 9(06).
     05      W-ABL               PIC 9(04).
     05      W-MDNR              PIC 9(08).
     05      W-TSNR              PIC 9(08).
     05      W-TRACENR-37        PIC 9(06).

     05      W-MEMLOG-KEY.
      10     W-TERMNR            PIC 9(08).
      10     W-TRACENR           PIC 9(06).
      10     W-NTYPE             PIC 9(04).
      10     W-ABWKZ             PIC 9(06).
*G.02.00 - Anfang
      10     W-BELEGNR           PIC 9(04).
*G.02.00 - Ende
     05      W-LTGIND            PIC 9(04).
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
     05      W-BUFFER-AKT        PIC S9(04) comp.


*---------------------> Fuer Konvertierung der TSNR fuer VUNR
*                       TSNR bekommen wir aus DB mit fuehrenden Nullen
 01          W-KONV-TSNR         PIC Z(7)9    VALUE ZEROES.
 01          W-KONV-TSNR-STR     PIC X(08)    REDEFINES W-KONV-TSNR.

**          ---> Schlüsselfelder
 01          W-MACKEYA           PIC X(04) VALUE SPACES.
 01          W-PACKEYA           PIC X(08) VALUE LOW-VALUE.
 01          W-MACKEYT           PIC X(04) VALUE SPACES.
 01          W-PACKEYT           PIC X(04) VALUE SPACES.
 01          W-HERSTID           PIC X(02) VALUE SPACES.
 01          W-VERSION           PIC X(02) VALUE SPACES.
 01          W-PADDING           PIC X(08) VALUE LOW-VALUE.

**  ---> Liste der Inlineservices
 01          INLINE-SERVICES.
     05      W-ARTMAP            PIC X(16) VALUE "WXAMP06S-TEMP".
     05      INLINE-SERVICE      PIC S9(04) COMP VALUE ZERO.
          88 USE-WXAMP                           VALUE 20.

 01          GEO-BUFFER.
     05      GEO-KZ-BREITE       PIC X.
     05      GEO-BREITE-8        PIC 9(08).
     05                          PIC X  VALUE SPACE.
     05      GEO-KZ-LAENGE       PIC X.
     05      GEO-LAENGE-9        PIC 9(09).
 01          GEO-FAKTOR          PIC S9(18) COMP VALUE 1000000.

**          ---> Bereiche für IFSF-BMP48
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
* Datm-Uhrzeitfelder (für TAL-Routine)
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
* Parameter für Untermodulaufrufe
*--------------------------------------------------------------------*
 01          PARAMETER-FELDER.
     05      P-DUMMY             PIC X(02).

**          ---> ChangeCode mit ASC2EBC und EBC2ASC
 01          P-CC-LEN            PIC S9(04) COMP.
 01          P-CC-IN             PIC X(64).
 01          P-CC-OUT            PIC X(64).

**          ---> für WT^HEX und WT^UNHEX Routinen
 01          P-HEX8              PIC X(08).
 01          P-HEX16             PIC X(16).

**          ---> für COBOL-Utilities GET-/PUT-STARTUPTEXT
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
**          ---> Mapping ROUTKZ <-> APPL_KZ.IFSFAC
**          --->
**          ---> hier muss ggf. bei weiteren AS'sen erweitert werden
 01          VERF-ROUTKZ         PIC 9(02).
          88 VERF-AG                         VALUE 15.
          88 VERF-AV                         VALUE 05.
          88 VERF-BP                         VALUE 14.
          88 VERF-DK                         VALUE 12.
*G.02.09 - Anfang
          88 VERF-EU                         VALUE 22.
*G.02.09 - Ende

*G.02.37 - Anfang
          88 VERF-IQ                         VALUE 24.
*G.02.37 - Ende

*G.02.13 - Anfang
          88 VERF-LO                         VALUE 23.
*G.02.13 - Ende
          88 VERF-NF                         VALUE 99.
          88 VERF-OR                         VALUE 16.
          88 VERF-SH                         VALUE 07.
          88 VERF-TN                         VALUE 18.
          88 VERF-TO                         VALUE 10.
          88 VERF-UT                         VALUE 17.
*G.02.49 - Roadrunner neu
          88 VERF-RR                         VALUE 25.
*G.02.49 - Ende

**          ---> Verfahrensfestlegung für Artikelmapper
**          ---> AG, AV und TN sind gleich (werden wie AG behandelt)
 01          AS-VERF             PIC X(02).
          88 AS-VERF-AG                      VALUE "AG".
          88 AS-VERF-AV                      VALUE "AV".
          88 AS-VERF-BP                      VALUE "BP".
          88 AS-VERF-DK                      VALUE "DK".
*G.02.09 - Anfang
          88 AS-VERF-EU                      VALUE "EU".
*G.02.09 - Anfang

*G.02.37 - Anfang
          88 AS-VERF-IQ                      VALUE "IQ".
*G.02.37 - Anfang

*G.02.13 - Anfang
          88 AS-VERF-LO                      VALUE "LO".
*G.02.13 - Anfang
          88 AS-VERF-OR                      VALUE "OR".
          88 AS-VERF-SH                      VALUE "SH".
          88 AS-VERF-TN                      VALUE "TN".
          88 AS-VERF-TO                      VALUE "TO".
          88 AS-VERF-UT                      VALUE "UT".
*G.02.49 - Roadrunner neu
          88 AS-VERF-RR                      VALUE "RR".
*G.02.49 - Ende

          88 AS-VERF-DEFAULT                 VALUE "AG".

**          ---> Parametertabelle für Autorisierungssystem
**          ---> wird im Programmvorlauf geladen, d.h. bei Änderungen
**          ---> muss das Programm (Serverklasse) neu gestartet werden

*kl20180405 - G.02.43 - Sieht gut aus, ist aber hier nicht angebracht
* 01          T-FCPARAM.
*kl20180105 - G.03.08 - Tabelle auf 500 Einträge vergroessert
*     05      T-FCPARAM-TAB    occurs 200.
*     05      T-FCPARAM-TAB    occurs 1 to 200
*     05      T-FCPARAM-TAB    occurs 1 to 500
*kl20180105 - G.03.08 - Ende
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
*kl20180105 - G.03.08 - Tabelle auf 500 Einträge vergroessert
* 01          T-TAB-MAX           PIC S9(04) COMP VALUE 200.
* 01          T-TAB-MAX           PIC S9(04) COMP VALUE 500.
*kl20180105 - G.03.08 - Ende
* 01          T-AKT-IND           PIC S9(04) COMP VALUE ZEROS.

*==> Zurück zur Standardtabellendefinition
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
*kl20180405 - G.02.43. - Ende

**          ---> zu suchende Werte (Bei Aenderung auch
*                                   S2-SEARCH-KEY anpassen!)
 01          S-SEARCH-KEY.
     05      S-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
     05      S-CARDID            PIC S9(04) COMP VALUE ZEROS.
     05      S-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
     05      S-KZ-MSG            PIC  X(02).
     05      S-BMP               PIC S9(04) COMP VALUE ZEROS.
     05      S-LFDNR             PIC S9(04) COMP VALUE ZEROS.

*kl20180405 - G.02.43 - Zusätzlicher Searchkey für CARDID = 0
*                       (wird aus S-SEARCH-KEY gefüllt; dann wird
*                        ledglich S2-CARDID mit ZERO überschrieben)
 01          S2-SEARCH-KEY.
     05      S2-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
     05      S2-CARDID            PIC S9(04) COMP VALUE ZEROS.
     05      S2-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
     05      S2-KZ-MSG            PIC  X(02).
     05      S2-BMP               PIC S9(04) COMP VALUE ZEROS.
     05      S2-LFDNR             PIC S9(04) COMP VALUE ZEROS.
*kl20180405 - G.02.43 - Ende

**          ---> AS-Keytabelle
 01          TK-KEYNAMEN.
     05      TK-KEYNAMEN-TABELLE occurs 10.
      10     TK-ROUTKZ           PIC S9(04).
      10     TK-CARDID           PIC S9(04).
      10     TK-KEYNAME          PIC X(08).
      10     TK-ISOGEN           PIC X(02).
      10     TK-ISOVERS          PIC X(02).
      10     TK-HEXKEY           PIC X(04).
      10     TK-HEXISO           PIC X(02).

 01          TK-MAX              PIC S9(04) COMP.
 01          TK-TAB-MAX          PIC S9(04) COMP VALUE 10.


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

**          ---> für TAL-Modul WT^HEX^STRING
 01          WTHEXS.
     05      WTHEXS-SRC          PIC X(4096).
     05      WTHEXS-SRC-LEN      PIC S9(04) COMP.
     05      WTHEXS-DST          PIC X(4096).
     05      WTHEXS-DST-LEN      PIC S9(04) COMP.

*--------------------------------------------------------------------*
* Parameter für Untermodulaufrufe - COPY-Module
*--------------------------------------------------------------------*
 01          TS-INTERN-MESSAGE.
     COPY    INT-SCHNITTSTELLE-C OF  "=MSGLIB"
             REPLACING =="*"== BY ==TS==.

**          ---> fuer Artikeldatenmapper WXAMP011
 01          AMP-SCHNITTSTELLE  IS EXTERNAL.
     COPY    WXAMP01C OF "=MSGLIB"
             REPLACING =="*"== BY ==AMP==.

**          ---> fuer Fehlerbeh.
     COPY    WSYS022C OF "=MSGLIB".

**          ---> Schnittstelle zu SDBCDU5
     COPY    SDBCDU0C OF "=MSGLIB"
             REPLACING =="*"== BY ==SDB==.

*G.02.26 - Anfang
**          ---> Schnittstelle zu Modul SYSABL1
     COPY    SYSABL1C OF "=MSGLIB"
             REPLACING =="*"== BY ==TABL==.
*G.02.26 - Ende

**          ---> Schnittstelle zu SYSAWKZ
     COPY    SYSWKZ0C OF "=MSGLIB"
             REPLACING =="*"== BY ==WKZ==.

**          ---> Schnittstelle zu SYSMK7I
     COPY    SYSML7IC    OF "=MSGLIB"
             REPLACING =="*"== BY ==MEM==.

**          ---> Schnittstelle zu WCAPM92
     COPY    PCAPM01C    OF "=MSGLIB"
             REPLACING =="*"== BY ==PCAP==.

**          ---> Schnittstelle zu WISO207
     COPY    WISO207C OF "=MSGLIB"
             REPLACING =="*"== BY ==W207==.

**          ---> Schnittstelle zu WISO207
     COPY    WISO207C OF "=MSGLIB"
             REPLACING =="*"== BY ==WATS==.

**          ---> Schnittstelle zu WSY7066
     COPY    WSY7066C OF "=MSGLIB"
             REPLACING =="*"== BY ==W66==.

**          ---> Schnittstelle zu WSYS930
     COPY    WSYS930C OF "=MSGLIB"
             REPLACING =="*"== BY ==ROUT==.

**          ---> Schnittstelle zu WSYS971
     COPY    WSYS971C OF "=MSGLIB"
             REPLACING =="*"== BY ==CHK==.

*G.02.00 - Anfang
**          ---> Fuer Umsatz
      COPY   WUMSO07C OF "=MSGLIB"
             REPLACING =="*"== BY ==WUMS==.
*G.02.00 - Ende

*G.02.14 - Anfang
**          ---> Für Boxen-interface
     COPY    WEUR056C OF  "=MSGLIB"
             REPLACING =="*"== BY ==Z==.
*G.02.14 - Ende

*G.02.44
      COPY    ZPVERKAUF-IFC OF "=MSGLIB".
*G.01.15 - ende

******************************************************************
* Im Folgenden Vorkehrungen für SQL
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
* Im Folgenden zunächst Host-Variable, die Bestandteil von
* SQL - Tabellen sind
******************************************************************
 01          HOST-VARIABLEN.
     05      H-DUMMY             PIC X(02).
     05      H-SYSKEY            PIC S9(18) COMP.
     05      H-ZP-IN             PIC X(22).
     05      H-ZP-OUT            PIC X(22).

******************************************************************
* Im Folgenden mit dem INVOKE-Befehl die Tabellenstruktur-
* definitonen der benötigten Tabellen einfügen
******************************************************************

 EXEC SQL
    INVOKE =ASYNC70 AS ASYNC70
 END-EXEC

**  ---> Struktur der Tabelle FCPARAM
 EXEC SQL
    INVOKE =FCPARAM   AS FCPARAM
 END-EXEC

**  ---> Struktur der Tabelle KEYNAMEN
 EXEC SQL
    INVOKE =KEYNAMEN AS KEYNAMEN
 END-EXEC

**  ---> Struktur der Tabelle MDNR2AS
 EXEC SQL
    INVOKE =MDNR2AS  AS MDNR2AS
 END-EXEC

**  ---> Struktur der Tabelle TSKART40
 EXEC SQL
    INVOKE =TSKART40 AS TSKART40
 END-EXEC

**  ---> Struktur der Tabelle STATION
 EXEC SQL
    INVOKE =STATION  AS STATION
 END-EXEC

**  ---> Struktur der Tabelle STATIONA
 EXEC SQL
    INVOKE =STATIONA AS STATIONA
 END-EXEC

**  ---> Struktur der Tabelle TXILOG70
 EXEC SQL
    INVOKE =TXILOG70 AS TXILOG70
 END-EXEC

**  ---> Struktur der Tabelle TXILOG70 für Autorisierung
 EXEC SQL
    INVOKE =TXILOG70 AS TXILOG70-AUT
 END-EXEC

**  ---> Transaktionslog Autor.Nachrichten der Station
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-ATS
 END-EXEC

**Transaktionslog Nachrichten der Station
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-TS
 END-EXEC

*G.02.00 - Anfang
**Umsatzliste
 EXEC SQL
    INVOKE =UMSWEAT  AS UMSWEAT
 END-EXEC
*G.02.00 - Ende

*G.02.33 - fuer bereits vorhandene Wdh.
**  ---> Struktur der Tabelle TXILOG70 für Autorisierung
 EXEC SQL
    INVOKE =TXILOG70 AS TXILOG70-WDH
 END-EXEC

**  ---> Transaktionslog Autor.Nachrichten der Station
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-WDH
 END-EXEC
*G.02.33 - Ende
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
* Im Folgenden werden die benöetigten CURSOR auf die
* verschiedenen SQL - Tabellen definiert
******************************************************************
**  ---> Cursor auf Tabelle FCPARAM
 EXEC SQL
     DECLARE FCPARAM_CURS CURSOR FOR
         SELECT   ROUTKZ, CARDID, ISONTYP, KZ_MSG, BMP, LFDNR
                 ,KZ_ABWEICHUNG
           FROM  =FCPARAM
          WHERE   ROUTKZ = :ROUTKZ  of FCPARAM
*G.02.17 - Anfang
            AND   APPKZ  = "R7"
*G.02.17 - Ende

*kl20180405 - G.02.43 - wg. Prioriseirung CARDID=X vor CARDID=0
*          ORDER  BY ROUTKZ, CARDID, ISONTYP, BMP, LFDNR
          ORDER  BY ROUTKZ          ASC,
                    CARDID          DESC,
                    ISONTYP         ASC,
                    BMP             ASC,
                    LFDNR           ASC
*kl20180405 - G.02.43 - Ende

         BROWSE  ACCESS
 END-EXEC

**  ---> Cursor auf Tabelle KEYNAMEN
 EXEC SQL
     DECLARE KEYNAMEN_CURS CURSOR FOR
         SELECT   ROUTKZ, CARDID, KEYNAME, ISOGEN, ISOVERS
           FROM  =KEYNAMEN
          WHERE   ROUTKZ = :ROUTKZ of KEYNAMEN
         ORDER  BY CARDID
         BROWSE  ACCESS
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
             WHEN "TS"
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

**  ---> holen Parameter AS-ROUTKZ
     MOVE "AS-ROUTKZ" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF

**  ---> holen Parameter für zuständiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO ROUTKZ of FCPARAM
                                       W-ROUTKZ
                                       S-ROUTKZ
**                                    ---> für Artikelmapper
                                       VERF-ROUTKZ

**  ---> Anwendung setzen für Artikelmapper
**  ---> (die auf Kommentar gesetzten sind default (AG))
     EVALUATE TRUE

*        WHEN VERF-AG    SET AS-VERF-AG TO TRUE
*        WHEN VERF-AV    SET AS-VERF-AV TO TRUE
         WHEN VERF-BP    SET AS-VERF-BP TO TRUE
         WHEN VERF-DK    SET AS-VERF-DK TO TRUE
*G.02.09 - Anfang
         WHEN VERF-EU    SET AS-VERF-EU TO TRUE
*G.02.09 - Ende

*G.02.37 - Anfang
         WHEN VERF-IQ    SET AS-VERF-IQ TO TRUE
*G.02.37 - Ende

*G.02.13 - Anfang
         WHEN VERF-LO    SET AS-VERF-LO TO TRUE
*G.02.13 - Ende
         WHEN VERF-OR    SET AS-VERF-OR TO TRUE
         WHEN VERF-SH    SET AS-VERF-SH TO TRUE
*        WHEN VERF-TN    SET AS-VERF-TN TO TRUE
         WHEN VERF-TO    SET AS-VERF-TO TO TRUE
         WHEN VERF-UT    SET AS-VERF-UT TO TRUE

         WHEN OTHER      SET AS-VERF-DEFAULT TO TRUE

     END-EVALUATE

**  ---> interne Tabelle initialisieren

*kl20180109 - G.02.36 - Initialisieren mit T-TAB-MAX
*                       statt Fixwert 200/500
     MOVE   T-TAB-MAX       TO T-MAX
*kl20180109 - G.02.36 - Ende
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   C4-I1 > T-TAB-MAX
         INITIALIZE T-KEY (C4-I1)
     END-PERFORM

**  ---> ersten Eintrag holen
     PERFORM S900-OPEN-FCPARAM-CURSOR
     PERFORM S910-FETCH-FCPARAM-CURSOR

**  ---> Schleife über alle Einträge für das ROUTKZ
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

**      ---> nächsten Eintrag holen
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
*kl20180105 - G.02.35 - Max. Anzahl in T-TAB-MAX
*     IF  C9-COUNT > T-MAX
     IF  C9-COUNT > T-TAB-MAX
*kl20180105 - G.02.35 - Ende
         MOVE "Programm interne Tabelle zu klein" TO  DATEN-BUFFER1
         MOVE "mehr als 500 Einträge in =FCPARAM " TO  DATEN-BUFFER2
         MOVE ROUTKZ OF FCPARAM TO D-NUM4
         STRING  "für ROUTKZ = "     delimited by size
                 D-NUM4              delimited by size
           INTO  DATEN-BUFFER3
         END-STRING
         MOVE "Programm-Abbruch" TO  DATEN-BUFFER4
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

**  ---> AS Schlüssel MACKEYA und PACKEYA aus Tabelle =KEYNAMEN einlesen
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> !!!! zunächstmal wird nur der erste eingelesen !!!!
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> ersten Eintrag holen

     MOVE W-ROUTKZ TO ROUTKZ of KEYNAMEN
     PERFORM S930-OPEN-KEYNAMEN-CURSOR
     PERFORM S940-FETCH-KEYNAMEN-CURSOR

**  ---> Schleife über alle Einträge für das ROUTKZ
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   KEYNAMEN-EOD
             or      KEYNAMEN-NOK
             or      C9-COUNT > TK-TAB-MAX
*             or      c4-i1 > 1

         MOVE ROUTKZ  of KEYNAMEN TO TK-ROUTKZ  (C4-I1)
         MOVE CARDID  of KEYNAMEN TO TK-CARDID  (C4-I1)
         MOVE KEYNAME of KEYNAMEN TO TK-KEYNAME (C4-I1)
         MOVE ISOGEN  of KEYNAMEN TO TK-ISOGEN  (C4-I1)
         MOVE ISOVERS of KEYNAMEN TO TK-ISOVERS (C4-I1)
**      ---> Keynamen hexen
         MOVE TK-KEYNAME (C4-I1) TO P-HEX16
         PERFORM P900-WTHEX
         MOVE P-HEX8 TO TK-HEXKEY (C4-I1)
**      ---> Generation und Version hexe
         MOVE TK-ISOGEN  (C4-I1) TO P-HEX16
         MOVE TK-ISOVERS (C4-I1) TO P-HEX16 (3:2)
         PERFORM P900-WTHEX
         MOVE P-HEX8 (1:1) TO TK-HEXISO (C4-I1)
         MOVE P-HEX8 (2:1) TO TK-HEXISO (C4-I1) (2:1)

**      ---> nächsten Eintrag holen
         PERFORM S940-FETCH-KEYNAMEN-CURSOR

     END-PERFORM

**  ---> schliessen Cursor
     PERFORM S950-CLOSE-KEYNAMEN-CURSOR
     MOVE C9-COUNT TO TK-MAX
     IF  TK-MAX = 1
         MOVE TK-HEXKEY (1) TO W-MACKEYA
         MOVE TK-HEXKEY (1) TO W-PACKEYA (1:4)
     END-IF

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

*G.02.45 - ANZ-REP Parameter lesen
**  ---> holen Parameter ANZREPA
     MOVE "ANZREPA" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF

**  ---> holen Parameter für zuständiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO W-ANZREPA

**  ---> holen Parameter ANZREPM
     MOVE "ANZREPW" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF

**  ---> holen Parameter für zuständiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO W-ANZREPW
*G.02.45 - Ende

**  --->
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
*** => weitere Verarbeitung hier einfügen
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

**  ---> Kontrolle der Anfrage <---
**  ---> ist die Anfrage evtl. OK?
     PERFORM C100-ANFRAGE-CHECK

*G.02.33 - weitere Wdh. direkt Antwort senden, von 1. Wdh.
     IF WDH-VORHANDEN
        PERFORM E200-WDH-ANTWORT
        EXIT SECTION
     END-IF
*G.02.33 - Ende

*G.02.29 - Anfang
*     IF  ENDE
*         EXIT SECTION
*     END-IF
*G.02.29 - Anfang

*G.02.33 - wenn Teilstorno bereits bearbeitet, hier schon antwortet senden
     IF  W-AC not = ZERO OR UMS-DONE
**      ---> Ablehnung senden: Formatfehler
**      ---> oder Antwort auf erste Wdh.
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF
*G.02.33 - Ende

**  ---> für alle AS'sen gültige Transaktions Regeln
     PERFORM C200-AS-GENERELL
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.19 - Anfang

     IF  W-AC NOT = ZERO
**      ---> Ablehnung senden: Formatfehler
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

*G.02.19 - Ende

**  ---> spezielle Regeln für AS'sen
     PERFORM C300-AS-SPEZIELL
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und hier die AS-Nachricht zusammenbauen
     PERFORM C400-BUILD-AS-NACHRICHT

**  ---> Check AC
     IF  ENDE
         EXIT SECTION
     END-IF
     IF  W-AC > 0
**      ---> Ablehnung senden: 12/96 (s. C200-AS-NACHRICHT)
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

     PERFORM G090-PUT-ASNYC70

     PERFORM E100-FEP-ANTWORT

*G.02.00 - Anfang
**UMSWEAT Korrektur Authrisierter Betrag zu tatsächlichem Betrag
     IF W-AC = ZEROS
        PERFORM G125-PUT-UMSWEAT
     END-IF
*G.02.00 - Ende
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
*G.02.00 - Anfang
                UMSWEAT
*G.02.00 - Ende
                WORK-INIT
                ASYNC70

     MOVE ZERO TO W-AC

*** =>
*** => weitere Verarbeitung hier einfügen
*** =>
     .
 C000-99.
     EXIT.

******************************************************************
* Kontrolle Eingangsnachricht
******************************************************************
 C100-ANFRAGE-CHECK SECTION.
 C100-00.
**  ---> interne Schnittstelle sichern
     MOVE INTERN-MESSAGE TO TS-INTERN-MESSAGE

**  ---> erstmal Termid für ERRLOG aufbereiten
     IF  IMSG-TBMP(41) = 1 AND IMSG-TPTR(41) > 0 AND IMSG-TLEN(41) > 0
         MOVE IMSG-CF(IMSG-TPTR(41):IMSG-TLEN(41)) TO W-TERMNR
         MOVE W-TERMNR TO P-HEX16
         PERFORM P900-WTHEX
         MOVE P-HEX8(1:4) TO TERMID of APPL-SPEC-BUF
     END-IF

**  ---> Trace-Nr. schon malmerken
     IF  IMSG-TBMP(11) = 1 AND IMSG-TPTR(11) > 0 AND IMSG-TLEN(11) > 0
         MOVE IMSG-CF(IMSG-TPTR(11):IMSG-TLEN(11)) TO W-TRACENR
     END-IF

*G.02.25 - =STATION Felder setzen
**  ---> weitere Felder merken
     MOVE IMSG-MDNR TO MDNR of TXILOG70
                       MDNR of TXNLOG70-TS
*kl2010929 - G.02.32 - Wird auch für Select STATION gebraucht
                       MDNR of STATION
*kl2010929 - G.02.32 - Ende
                       W-MDNR

     MOVE IMSG-TSNR TO TSNR of TXILOG70
                       TSNR of TXNLOG70-TS
*kl2010929 - G.02.32 - Wird auch für Select STATION gebraucht
                       TSNR of STATION
*kl2010929 - G.02.32 - Ende
                       W-TSNR
*G.02.25 - Ende

     MOVE W-TERMNR  TO TERMNR of TXILOG70
                       TERMNR of TXNLOG70-TS

*G.02.28 - Anfang
*G.02.29 - Anfang
     IF IMSG-TBMP(63) = 1 AND IMSG-TPTR(63) > 0 AND IMSG-TLEN(63) > 0
        MOVE IMSG-TLEN(63)
          TO LEN OF ARTIKEL OF TXILOG70
        MOVE IMSG-CF(IMSG-TPTR(63):IMSG-TLEN(63))
          TO VAL OF ARTIKEL OF TXILOG70
     END-IF
*G.02.29 - Ende

**  ---> Tracenr aus BMP37 muss < als BMP11 sein
     MOVE IMSG-CF(IMSG-TPTR(37) + 6:6) TO W-TRACENR-37
     IF  W-TRACENR-37 not < W-TRACENR
         MOVE "TraceNr. des Teilstornos nicht aufsteigend"
           TO DATEN-BUFFER1
         MOVE "Transaktion wird nicht beantwortet"
           TO DATEN-BUFFER2
         SET ENDE TO TRUE
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF
*G.02.28 - Ende

*G.02.25 - =STATION lesen
**  ---> Station zum Terminal lesen
     PERFORM S160-SELECT-STATION
     IF  ENDE
         EXIT SECTION
     END-IF
*G.02.25 - Ende

*G.02.33 - Belegnr hier schon gebraucht
     MOVE IMSG-CF(IMSG-TPTR(37) + 2:4)         TO W-BELEGNR
*G.02.33 - Ende

*G.02.28 - Anfang
**  ---> holen Routing, Autorisierungs-Eintrag auf TXILOG70, etc.
     PERFORM D900-ROUTING-ETC
*G.02.33
     IF  W-AC not = ZERO OR WDH-VORHANDEN
         EXIT SECTION
     END-IF
*G.02.33 - Ende
*G.02.28 - Ende

**  ---> Nachrichten enthalten keinen MAC

**  ---> formale Prüfung der Nachricht
     MOVE ZERO TO W-AC
     SET  CHK-CHECK-ALL TO TRUE
     MOVE IMSG-NTYPE    TO CHK-NTYPE
                           W-NTYPE
     MOVE "X"           TO CHK-NTYPE (4:1)
     MOVE "R7"          TO CHK-ABWKZ(1:2)
     MOVE K-MODUL (2:4) TO CHK-ABWKZ(3:4)
**  ---> soll dann "040X" "R7FCFA" sein

**  ---> formale Prüfung durch Modul WSYS971
     PERFORM M160-CALL-WSYS971

*G.02.29 - Anfang
*    IF  ENDE or W-AC > ZERO
*         EXIT SECTION
*     END-IF
**
    IF W-AC > ZEROS
       EXIT SECTION
     END-IF

*G.02.29 - Ende

**  ---> korrektes AbWkz?
     IF  IMSG-CF(IMSG-TPTR(3):2) not = "95"
         MOVE 30  TO W-AC
         MOVE 1004 TO ERROR-NR of GEN-ERROR
         MOVE "AbWkz=95@" TO DATEN-BUFFER1
         MOVE "Switchparameter überprüfen" TO DATEN-BUFFER2
         MOVE "Transaktion wird NICHT beantwortet" TO DATEN-BUFFER3
         PERFORM Z002-PROGERR
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF

**  ---> Abwicklungs-KZ merken
     MOVE IMSG-CF(IMSG-TPTR(03):IMSG-TLEN(03)) TO W-ABWKZ

*G.02.00 - Ende
*G.02.03   W-Belegnr für Update Umsatz muss mit Belegnr aus BMP37 geamcht werden
*     MOVE IMSG-CF(IMSG-TPTR(03) + 2:4)         TO W-BELEGNR
*G.02.33 nach oben geschoben, vor D900-ROUTING-ETC
*     MOVE IMSG-CF(IMSG-TPTR(37) + 2:4)         TO W-BELEGNR
*G.02.33 - Ende
*G.02.00 - Anfang

**  ---> Betrag aufbereiten und merken (nur für Eigenantwort)
     MOVE IMSG-CF(IMSG-TPTR(04):IMSG-TLEN(04)) TO W-BETRAG
     COMPUTE W18-BETRAG = W-BETRAG / 100


*G.02.44
*--> ZP-VERKAUF - Berechnung jetzt über Modul ZPVERK
*
***  ---> Verkaufszeitpunkt aufbereiten (20 + BMP13 + BMP12)
*     MOVE IMSG-CF(IMSG-TPTR(12):IMSG-TLEN(12)) TO D-NUM6
*     MOVE IMSG-CF(IMSG-TPTR(13):IMSG-TLEN(13)) TO D-NUM12
*     COMPUTE D-NUM12      = D-NUM12 * 1000000
*     COMPUTE W-ZP-VERKAUF = (10000000000 * TAL-JHJJ of TAL-TIME-D)
*                          + D-NUM12 + D-NUM6


*--> ZP-VERKAUF - Berechnung jetzt über Modul ZPVERK
*--> Werte fuer ZP_VERKAUF,  Format:JJJJMMTThhmmss
*--> Z-ZPVERKAUF-RC = 0 ==> alles ok, ZPVERKAUF-TXILOG70 berechnet
*                   > 0 ==> Fehler,   ZPVERKAUF-TXILOG70 = 0
*                   = 1 ==> BMP 12 oder 13 nicht numerisch
*                   = 2 ==> Fehler bei der Berechnung mit HCTIME

      MOVE 1                                    TO ZPVERKAUF-RC
      MOVE TAL-TIME-N                           TO ZPVERKAUF-FEPTALZEIT
      MOVE IMSG-CF(IMSG-TPTR(12):IMSG-TLEN(12)) TO ZPVERKAUF-BMP12
      MOVE IMSG-CF(IMSG-TPTR(13):IMSG-TLEN(13)) TO ZPVERKAUF-BMP13
      MOVE 0                                    TO ZPVERKAUF-TXILOG70
      CALL "ZPVERK" USING ZPVERKAUF-IFC
      MOVE ZPVERKAUF-TXILOG70                   TO ZP-VERKAUF OF TXILOG70

      EVALUATE ZPVERKAUF-RC
**      ---> OK
          WHEN ZERO   CONTINUE

**      ---> Numerik Fehler
          WHEN 1      MOVE "Info: Fehler bei der Berechnung ZP-VERKAUF"
                           TO DATEN-BUFFER1
                      MOVE "BMP 12 oder 13 nicht numerisch"
                           TO DATEN-BUFFER2
                      MOVE "In TXILOG70.ZP-VERKAUF wird Null eingetragen."
                           TO DATEN-BUFFER3
                      PERFORM Z002-PROGERR

**      ---> HCTIME Fehler
          WHEN 2      MOVE "Info: Fehler bei der Berechnung ZP-VERKAUF"
                           TO DATEN-BUFFER1
                      MOVE "Zeitberechnung über das Modul HCTIME fehlerhaft "
                           TO DATEN-BUFFER2
                      MOVE "In TXILOG70.ZP-VERKAUF wird Null eingetragen."
                           TO DATEN-BUFFER3
                      PERFORM Z002-PROGERR
      END-EVALUATE
*G.02.44 - Ende



*G.02.28 - Anfang
**  ---> Tracenr aus BMP37 muss < als BMP11 sein
*    MOVE IMSG-CF(IMSG-TPTR(37) + 6:6) TO W-TRACENR-37
*    IF  W-TRACENR-37 not < W-TRACENR
*        MOVE "TraceNr. des Teilstornos nicht aufsteigend" TO DATEN-BUFFER1
*        MOVE "Transaktion wird nicht beantwortet"         TO DATEN-BUFFER2
*        SET ENDE TO TRUE
*        PERFORM Z002-PROGERR
*        EXIT SECTION
*    END-IF
*G.02.28 - Ende

**  ---> prüfen, ob Wiederholungsnachricht
     IF  W-NTYPE > 400
         SET STORNO-WIEDERHOLUNG TO TRUE
     END-IF

*G.02.21 - Pruefung BMP 25, bei Teilsto muss Wert 27 sein
     IF IMSG-TBMP(25) = 1
     AND NOT IMSG-CF(IMSG-TPTR(25) : IMSG-TLEN(25)) = "27"
        MOVE 30 TO W-AC
        MOVE 2201 TO ERROR-NR OF GEN-ERROR
        MOVE "BMP25: für Teil-Storno nicht ok   @" TO DATEN-BUFFER1
        PERFORM Z002-PROGERR
        EXIT SECTION
     END-IF
*G.02.21 - Ende

*G.02.28 - Anfang
**  ---> holen Routing, Autorisierungs-Eintrag auf TXILOG70, etc.
*    PERFORM D900-ROUTING-ETC
*    IF  W-AC not = ZERO
*        EXIT SECTION
*    END-IF
*G.02.28 - Ende

**  ---> holen zuständigen Keynamen
     PERFORM D200-FIX-KEY
**  ---> setzen Keyname für diese Transaktion
     MOVE TK-KEYNAME (C4-I1) TO W-KEYNAME
     MOVE TK-HEXISO  (C4-I1)  TO W-ISOGEN-VERS

**  ---> Währungs-KZ könnte hier gegen Tabelle WKZKURS geprüft werden
     MOVE IMSG-CF (IMSG-TPTR (49) + 1:3) TO W-WKZ
     MOVE W-WKZ TO WKZ-WKZ
     SET WKZ-CMD-WKZ-A  TO TRUE
     PERFORM M170-CALL-SYSAWKZ
     IF  WKZ-ERR
         EXIT SECTION
     END-IF

**  ---> der AC der Vor-Autorisierung muss 0 oder 2 sein

*G.02.12 - Anfang
*    IF  (AC-AS   of TXILOG70-AUT = ZERO or = 2)
*    and (AC-TERM of TXILOG70-AUT = "00 " or = "10 " or = ZERO)
**
     MOVE SPACES                  TO PRF-AC
     MOVE AC-TERM of TXILOG70-AUT TO PRF-AC

     IF  (AC-AS   of TXILOG70-AUT = ZERO or = 2)
     AND (PRF-AC-OK
      OR  AC-TERM of TXILOG70-AUT = "000")
*G.02.12 - Ende
         continue
     ELSE

*G.02.27 - Anfang
*        MOVE 30 TO W-AC
**
         MOVE 21 TO W-AC

*G.02.27 - Anfang

         STRING  "AC der Vorautorisierung = "
                 AC-TERM of TXILOG70-AUT
                     delimited by size
           INTO  DATEN-BUFFER1
         END-STRING
         MOVE "Teilstorno ungültig" TO DATEN-BUFFER2
* wenn nicht beantwortet werden sollte:        SET ENDE TO TRUE
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF

**  ---> Der zu stornierende Betrag darf nicht > sein als der autorisierte
**  ---> beim WEAT-Teilstorno ist der nicht verbrauchte Teil des Vorautori-
**  ---> ten Betrags zu melden

*G.02.19 - Anfang
*
*    IF  W18-BETRAG > BETRAG-AUTOR of TXILOG70-AUT
*        MOVE "Teilstorno-Betrag > autorisierter Betrag" TO DATEN-BUFFER1
*        MOVE "Transaktion wird nicht beantwortet"       TO DATEN-BUFFER2
*        SET ENDE TO TRUE
*        PERFORM Z002-PROGERR
*        EXIT SECTION
*    END-IF
**

*G.02.19 - Ende

     .
 C100-99.
      EXIT.

******************************************************************
* AS-Anfrageteile, die für alle gleich sind
******************************************************************
 C200-AS-GENERELL SECTION.
 C200-00.
**  ---> im Folgenden Terminalnachricht als Grundlage nehmen
     MOVE IMSG-COBDATEN TO W207-COBDATEN

**  ---> zunächst die BitMap auf die Pflichtfelder setzen
     MOVE LOW-VALUE       TO W207-TBMP-O
     MOVE K-BYTEMAP-A1220 TO W207-TBMP-O (1:64)

**  ---> Werte für Tabelle =FCPARAM
     MOVE 1220 TO S-ISONTYP
     MOVE "AS" TO S-KZ-MSG
     MOVE 1    TO S-LFDNR

     SET W207-IFSF TO TRUE
**  ---> Nachrichtentyp für Nachricht setzen
**  ---> prüfen, ob Wiederholung
     IF  STORNO-WIEDERHOLUNG
         MOVE 1221 TO W207-NTYPE
     ELSE
         MOVE 1220 TO W207-NTYPE
     END-IF

**  ---> schon mal einen Dummy-MAC vorbereiten
     MOVE 1220 TO S-ISONTYP
     MOVE "AS" TO S-KZ-MSG
     MOVE 64   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     SET MAC-NO TO TRUE
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "1"
             SET MAC-YES TO TRUE
             MOVE 64 TO W207-XBMP
             MOVE 08 TO W207-XCOBLEN
             MOVE ALL LOW-VALUES TO W207-XCOBVAL
             PERFORM L100-ADD-BMP
             IF  ENDE
                 EXIT SECTION
             END-IF
         ELSE
             MOVE ZERO TO W207-TBMP(64)
         END-IF
     END-IF

*G.02.28 - Anfang
**  ---> BMP  2 - PAN
**  ---> zunächst mal Länge der KANR bestimmen
*    MOVE ZERO TO W-KANR-LEN
*    PERFORM VARYING C4-I1 FROM 19 BY -1
*            UNTIL   C4-I1 = ZERO
*
*        IF  KANR OF TXILOG70-AUT (C4-I1:1) not = SPACE
*            MOVE C4-I1 TO W-KANR-LEN
*            EXIT PERFORM
*        END-IF
*
*    END-PERFORM
*G.02.28 - Ende

**  ---> und nun ggf. auch in NAchricht einbauen
     IF  ERFASSUNGS-ART of TXILOG70-AUT = 1
         MOVE 02                   TO W207-XBMP
         MOVE KANR of TXILOG70-AUT TO W207-XCOBVAL

*G.02.28 - Anfang
**      ---> wenn Länge = 0: Fehlermeldung und ENDE
*        IF  W-KANR-LEN = ZERO
*            SET ENDE TO TRUE
*            MOVE "Keine KANR in =TXNLOG70" TO DATEN-BUFFER1
*            STRING  "TermNr./TraceNr. = "
*                    W-TERMNR
*                    "/"
*                    W-TRACENR
*                        delimited by size
*              INTO  DATEN-BUFFER2
*            END-STRING
*
*        END-IF
*G.02.28 - Ende

         MOVE W-KANR-LEN           TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

**  ---> BMP  3 - Zahlung/Gutschrift (nur Buffer manipulieren)
     MOVE "000000" TO W207-CF(W207-TPTR(3):W207-TLEN(3))

**  ---> BMP  4 - Betrag - Differenz Vorautorisierter - gesendeter Betrag
     MOVE 04 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN
     COMPUTE C18-BETRAG = (BETRAG-AUTOR of TXILOG70-AUT * 100) - W-BETRAG
     MOVE C18-BETRAG TO D-NUM12
     MOVE D-NUM12 TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP  7 - Übertragungszeit
     MOVE 07 TO W207-XBMP
     MOVE 10 TO W207-XCOBLEN
     STRING  TAL-MM   of TAL-TIME-D
             TAL-TT   of TAL-TIME-D
             TAL-HHMI of TAL-TIME-D
             TAL-SS   of TAL-TIME-D
                 delimited by size
       INTO  W207-XCOBVAL
     END-STRING
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 11 - Trace-Nr.
**  +++>          AS-Trace-Nr. holen (steht dann in W-AS-TRACENR)
     PERFORM D910-GET-ASTRACENR
     IF  W-AC NOT = ZERO
         EXIT SECTION
     END-IF
**  +++> hier wird nur der Buffer manipuliert
     MOVE W-AS-TRACENR TO W207-CF(W207-TPTR(11):W207-TLEN(11))

**  ---> BMP 12 - Lokalzeit (JJJJMMTThhmmss)
     MOVE 12 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN
*G.02.46 - Anhand ZP-VERKAUF (Kombination TS BMP 12/13) AS BMP 12 fuellen
     COMPUTE D-NUM12 = ZP-VERKAUF OF TXILOG70 - 20000000000000
     MOVE D-NUM12 TO W207-XCOBVAL
*     STRING  TAL-JHJJMMTT of TAL-TIME-D (3:6)
*             TAL-HHMI     of TAL-TIME-D
*             TAL-SS       of TAL-TIME-D
*                 delimited by size
*       INTO W207-XCOBVAL
*     END-STRING
*G.02.46 - Ende
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 14 - Ablaufdatum
     IF  ERFASSUNGS-ART of TXILOG70-AUT = 1
         MOVE 14     TO W207-XBMP
         MOVE 04     TO W207-XCOBLEN
         MOVE ABL-JJMM of TXILOG70-AUT TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

**  ---> BMP 22 - Eingabeart
     MOVE ERFASSUNGS-ART of TXILOG70-AUT TO W-ERFASSUNGS-ART
     MOVE 22 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN
     MOVE "210201" TO W207-XCOBVAL
     EVALUATE TRUE
         WHEN W-ERF-MANUELL      MOVE "6" TO W207-XCOBVAL (7:1)
         WHEN W-ERF-MAGNET       MOVE "2" TO W207-XCOBVAL (7:1)
         WHEN W-ERF-CHIP         MOVE "5" TO W207-XCOBVAL (7:1)
         WHEN W-ERF-KONTAKTLOS   MOVE "A" TO W207-XCOBVAL (7:1)
         WHEN OTHER              MOVE "3" TO W207-XCOBVAL (7:1)
     END-EVALUATE
*     EVALUATE TRUE
*         WHEN PAC-YES            MOVE "1" TO W207-XCOBVAL (8:1)
*         WHEN PAC-NO             MOVE "1" TO W207-XCOBVAL (8:1)
*         WHEN OTHER              MOVE "6" TO W207-XCOBVAL (8:1)
*     END-EVALUATE
     MOVE "14144" TO W207-XCOBVAL (8:5)
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 24 - Funktionscode
     MOVE 24     TO W207-XBMP
     MOVE 03     TO W207-XCOBLEN
     IF  W18-BETRAG = BETRAG-AUTOR of TXILOG70-AUT
         MOVE "201"  TO W207-XCOBVAL
     ELSE
         MOVE "202"  TO W207-XCOBVAL
     END-IF
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 25 - Message Reason
     MOVE 25 TO S-BMP
     MOVE 1  TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-NOT-FOUND
         PERFORM E900-PUT-ERRLOG
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF
     PERFORM U400-INTERPRET-ABWEICHUNG
     MOVE 25           TO W207-XBMP
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-BUFFER     TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 26 - Branchencode
     MOVE 26     TO W207-XBMP
     MOVE 04     TO W207-XCOBLEN
     MOVE "5541" TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 32 - Netzbetreiber Kennung AIID
     MOVE 32 TO S-BMP
     MOVE 1  TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-NOT-FOUND
         PERFORM E900-PUT-ERRLOG
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF
     PERFORM U400-INTERPRET-ABWEICHUNG
     MOVE 32           TO W207-XBMP
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-BUFFER     TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 35 - Spur 2
     IF  WATS-TLEN(35) > 0
         MOVE 35            TO W207-XBMP
         MOVE WATS-TLEN(35) TO W207-XCOBLEN
         INSPECT WATS-CF(WATS-TPTR(35):WATS-TLEN(35)) CONVERTING "D"
                                                              TO "="
         MOVE WATS-CF(WATS-TPTR(35):WATS-TLEN(35)) TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

*G.02.24 - Anfang
**  ---> BMP 38 - Autorisierungs-KZ (wird hier zunächst gesetzt, muss
**  --->          ggf. in den spez. Section's wieder ausschalten)
     IF GENNR of TXILOG70-AUT > SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR of TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
        MOVE ZERO TO W207-TBMP(38)
     END-IF
*G.02.24 - Ende

**  ---> BMP 41 - TERMNR - wird übernommen
     MOVE 41   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "0"
             MOVE ZERO TO W207-TBMP-O (41:1)
         END-IF
     END-IF

**  ---> BMP 42 - VUNR
     MOVE 42               TO W207-XBMP
     MOVE VUNR of TSKART40 TO W207-XCOBVAL
     MOVE 15               TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 48 - in den spez. Sections
**  ---> BMP 49 - WKZ - wird übernommen
     MOVE 49               TO W207-XBMP
     MOVE IMSG-CF(IMSG-TPTR(49) + 1:3) TO W207-XCOBVAL
     MOVE 3                TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 53 - wenn AS-MAC
     IF  MAC-YES
         MOVE W-ISOGEN-VERS TO W207-XCOBVAL
         MOVE 2 TO W207-XCOBLEN
         MOVE LOW-VALUE TO W207-XCOBVAL (W207-XCOBLEN + 1:)
         ADD 32 TO W207-XCOBLEN
**      --->  nachsehen, ob noch Suffix gesendet werden soll
         MOVE 53 TO S-BMP
         MOVE 1  TO S-LFDNR
         PERFORM U300-SEARCH-TAB
         IF  PRM-FOUND
             PERFORM U400-INTERPRET-ABWEICHUNG
             MOVE W-BUFFER TO W207-XCOBVAL (W207-XCOBLEN + 1:)
             ADD W-BUFFER-LEN TO W207-XCOBLEN
         END-IF
**      --->  und nun in Nachricht
         MOVE 53 TO W207-XBMP
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF


**  ---> BMP 56 - Daten der originalen Transaktion
     MOVE 56     TO W207-XBMP
     MOVE SPACES TO W207-XCOBVAL
     MOVE 1 TO C4-PTR
*G.02.48 - BMP 12+13(ZP-VERKAUF) aus Autorisierung verwenden
*     MOVE TAL-JHJJ of TAL-TIME-D (3:2) TO D-NUM2
*     MOVE AF-BMP07 OF TXILOG70-AUT     TO D-NUM10
     COMPUTE D-NUM12 = ZP-VERKAUF OF TXILOG70-AUT - 20000000000000
     STRING  "1100"
             TRACENR-AS of TXILOG70-AUT
*             D-NUM2
*             D-NUM10
             D-NUM12
                 delimited by size
       INTO  W207-XCOBVAL
       WITH  POINTER C4-PTR
     END-STRING
*G.02.48 - Ende
     COMPUTE W207-XCOBLEN = C4-PTR - 1
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 59 - Transportdaten
     MOVE 59 TO S-BMP
     MOVE 1  TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-NOT-FOUND
         PERFORM E900-PUT-ERRLOG
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF
     PERFORM U400-INTERPRET-ABWEICHUNG
     MOVE 59           TO W207-XBMP
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-BUFFER     TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 63 - Artikel über Artikel-Mapper
     IF  W207-TBMP(63) = 1
         PERFORM E305-01-ARTIKELDATEN
         IF  not AMP-OK
**          ---> Fehler vom Artikel-Mapper
             MOVE AMP-RC TO D-NUM4
             STRING  "RC aus Artikelmapping: "
                     D-NUM4
                         delimited by size
               INTO  DATEN-BUFFER1
             END-STRING
             PERFORM Z002-PROGERR
             IF  AMP-RC = 100
**              ---> Ablehnung senden: Ungültige Transaktion
                 MOVE 45 TO W-AC
             ELSE
**              ---> Ablehnung senden: Verarbeitung z.Zt. nicht möglich
                 MOVE 96 TO W-AC
             END-IF
             EXIT SECTION
         END-IF

**      ---> ab hier Artikel-Mapper OK
**      ---> der Artikelmapper bringt ggf. zunächst Fahrer-/Fahrzeug-
**      ---> daten (AMP-BMP48-FLAG = 1), die müssen für BMP48 aufbereitet
**      ---> werden
**      ---> (Kfz-Daten für BMP48 werden für Avia nicht gebraucht)
         MOVE ZERO TO W-48-LEN
         SET KFZ-NO TO TRUE
         IF  AMP-BMP48-FLAG = 1
             SET KFZ-YES TO TRUE
             MOVE AMP-HOST-VAL(1:3)            TO W-48-LEN
             MOVE AMP-HOST-VAL(1:W-48-LEN + 3) TO W-BMP48-VAL
             ADD  3                            TO W-48-LEN
         END-IF
**      ---> hier Artikel einstellen
         MOVE 63           TO W207-XBMP
         COMPUTE W-BUFFER-LEN = AMP-HOST-LEN - W-48-LEN
         MOVE W-BUFFER-LEN TO W207-XCOBLEN
         MOVE AMP-HOST-VAL(W-48-LEN + 1:W-BUFFER-LEN) TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

*G.02.19 - Anfang

     IF  W18-BETRAG > BETRAG-AUTOR of TXILOG70-AUT
         MOVE "Teilstorno-Betrag > autorisierter Betrag"
         TO DATEN-BUFFER1
         MOVE "Transaktion wird nicht beantwortet"
         TO DATEN-BUFFER2
         STRING  "TermNr./TraceNr. = "
                  W-TERMNR
                 "/"
                  W-TRACENR
         delimited by size
           INTO  DATEN-BUFFER2
         PERFORM Z002-PROGERR
         MOVE 64 TO W-AC
         EXIT SECTION
     END-IF

*G.02.19 - Ende

**  --->E305-01-ARTIKELDATEN nur fürs testen
     IF  TRACE-ON
         move "Test #1 - C200-AS-GENERELL - w207-tbmp:" to daten-buffer1
         move w207-tbmp-o (1:64) to daten-buffer2

         perform z002-progerr
     END-IF
     .
 C200-99.
     EXIT.

******************************************************************
* AS-Anfrageteile, die für alle gleich sind
******************************************************************
 C300-AS-SPEZIELL SECTION.
 C300-00.
**  ---> verzweigen je nach ROUTKZ
     EVALUATE W-ROUTKZ

         WHEN 05     PERFORM D305-AVIA
         WHEN 07     PERFORM D307-SHELL
         WHEN 10     PERFORM D310-TOTAL
         WHEN 12     PERFORM D312-DKV
         WHEN 14     PERFORM D314-BP
         WHEN 15     PERFORM D315-ENI
         WHEN 16     PERFORM D316-ORLEN
         WHEN 17     PERFORM D317-UTA
         WHEN 18     PERFORM D318-TND

*G.02.09 - Anfang
         WHEN 22     PERFORM D322-EUROWAG
*G.02.09 - Ende

*G.02.13 - Anfang
         WHEN 23     PERFORM D323-LOGPAY
*G.02.13 - Ende

*G.02.37 - Anfang
         WHEN 24     PERFORM D324-STIGLECHNER
*G.02.37 - Ende

*G.02.49 - Roadrunner neu
         WHEN 25     PERFORM D325-ROADRUNNER
*G.02.49 - Ende

         WHEN OTHER
                 SET ENDE TO TRUE
                 MOVE W-ROUTKZ TO D-NUM4
                 STRING  "Keine speziellen Verarbeitungsregeln "
                         "für Rout-KZ = "
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
* AS-Nachricht erstellen
******************************************************************
 C400-BUILD-AS-NACHRICHT SECTION.
 C400-00.
     SET W207-IFSF TO TRUE
     PERFORM L110-COB2ISO
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und echten MAC bilden und einstellen
     IF  MAC-YES
         MOVE ALL LOW-VALUES    TO W66-AKEY-NAME
         MOVE W-MACKEYA         TO W66-AKEY-NAME (1:4)
         SET  W66-MAC-BILDEN-AS TO TRUE
         SET  W66-IFSF          TO TRUE
         MOVE W-TERMNR          TO IMSG-TERMNR
         PERFORM F920-MAC-BILDEN
     END-IF

**  ---> nur fürs testen
     IF  TRACE-ON
         move "Test #2 - C400-BUILD-AS-NACHRICHT - w207-tbmp:" to daten-buffer1
         move w207-tbmp-o (1:64) to daten-buffer2

         perform z002-progerr
     END-IF

     .
 C400-99.
     EXIT.


******************************************************************
* Key bestimmen
******************************************************************
 D200-FIX-KEY SECTION.
 D200-00.
**  ---> wenn Schlüsseltabelle nur mit einem belegt ist, wieder zurück
     IF  TK-MAX = 1
         MOVE 1 TO C4-I1
         EXIT SECTION
     END-IF

**  ---> nun muss doch gesucht werden
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   C4-I1 > TK-MAX

         IF  TK-CARDID (C4-I1) NOT = W-CARDID
**          ---> nächsten suchen
             EXIT PERFORM CYCLE
         END-IF

**      ---> Schlüssel zur Verfügung stellen
         MOVE TK-HEXKEY (C4-I1) TO W-MACKEYA
         MOVE TK-HEXKEY (C4-I1) TO W-PACKEYA (1:4)
**      ---> Schlüsselgeneration und -version bereitstellen
         MOVE TK-HEXISO (C4-I1) TO W-ISOGEN-VERS
         EXIT SECTION

     END-PERFORM

**  ---> hier sind keine Schlüssen gefunden worden
     SET ENDE TO TRUE
     MOVE W-ROUTKZ TO D-NUM4
     MOVE W-CARDID TO D-NUM4OV
     MOVE "Keine AS-Schlüssel in =KEYNAMEN gefunden für:" TO DATEN-BUFFER1
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
* spezielle Behandlung für das Avia-AS
*    ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
******************************************************************
 D305-AVIA SECTION.
 D305-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

**  ---> BMP 14 - Ablaufdatum
     MOVE 14   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "0"
             MOVE ZERO TO W207-TBMP-O (14:1)
         END-IF
     END-IF


**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D305-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das Shell-AS
******************************************************************
 D307-SHELL SECTION.
 D307-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

*G.02.25 - BMP 43 senden, wie beim PFCPRE7S aus ORT of STATION
**  ---> BMP 43 - Ort
     MOVE 43 TO W207-XBMP

     MOVE 1  TO C4-PTR
     MOVE SPACES TO W207-XCOBVAL
     STRING  ORT of STATION
                 delimited by space
       INTO  W207-XCOBVAL
       WITH  POINTER C4-PTR
     END-STRING

     COMPUTE W207-XCOBLEN = C4-PTR - 1

     IF  W207-XCOBLEN = ZERO
         MOVE 4      TO W207-XCOBLEN
         MOVE "WEAT" TO W207-XCOBVAL
     END-IF
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
*G.02.25 - Ende

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT

*G.02.23 - Sonderbehandlung verkürztes BMP 59 für Shell2

*    Laenge aus allgemeinem Teil wieder reduzieren (nur noch 10 Byte:
*    Release + Applikation + AS-Tracenr)
     MOVE   10     TO W207-TLEN(59)

*    W207-CF Terminal-ID / Tracenr / Abwkz mit AS-TRACENR ueberschreiben
     MOVE W-AS-TRACENR    TO W207-CF(W207-TPTR(59) + 4:6)
*G.02.23 - Ende
     .
 D307-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das Total-AS
******************************************************************
 D310-TOTAL SECTION.
 D310-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-TOTAL TO TRUE

**  ---> BMP 22 - Eingabeart wird hier überschrieben
     MOVE 22 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN
     IF  ERFASSUNGS-ART OF TXILOG70-AUT = 01
**      ---> handeingabe
         MOVE "210101613001" TO W207-XCOBVAL
     ELSE
         MOVE "210101B13001" TO W207-XCOBVAL
     END-IF
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.20 - Anfang

**  ---> BMP39 einschalten
     MOVE 1 TO W207-TBMP(39)

**  ---> BMP39  ACODE
     MOVE 39    TO W207-XBMP
     MOVE ZEROS TO W207-XCOBVAL
     MOVE 2     TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.20 - Ende

*G.02.11 - Anfang

**  ---> BMP42 - VUNR wird hier überschrieben
*    MOVE 42     TO S-BMP
*    MOVE W-MDNR TO S-LFDNR
*    PERFORM U300-SEARCH-TAB
*    IF  PRM-NOT-FOUND
*        PERFORM E900-PUT-ERRLOG
*        MOVE "keine Info für BMP42 in =FCPARAM" TO DATEN-BUFFER3
*        MOVE "Mandant " TO DATEN-BUFFER4
*        MOVE W-MDNR TO DATEN-BUFFER4 (9:)
*        SET ENDE TO TRUE
*        EXIT SECTION
*    END-IF
*    MOVE T-KZ-ABWEICHUNG (T-AKT-IND) (1:2) TO W207-XCOBVAL
*    MOVE W-TSNR TO D-NUM6
*    MOVE D-NUM6 TO W207-XCOBVAL (3:6)
*    MOVE 42     TO W207-XBMP
*    MOVE  8     TO W207-XCOBLEN
*    PERFORM L100-ADD-BMP
*    IF  ENDE
*        EXIT SECTION
*    END-IF
*
*G.02.11 - Ende

**  ---> und BMP48 neu bilden
**  ---> zunächst die BitMap erstellen
     MOVE ALL ZEROES TO W-BYTEMAP-48
     MOVE LOW-VALUE  TO W-BITMAP
     MOVE SPACES     TO W207-XCOBVAL
     MOVE 8          TO W207-XCOBLEN

**  +++> jetzt das Subfeld 4 (Fixwert)
     MOVE "1"        TO W-BYTEMAP-48(4:1)
     MOVE "1"        TO W-BYTEMAP-48(38:1)
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE W-BITMAP   TO W207-XCOBVAL (1:8)

     MOVE "00000000010" TO W207-XCOBVAL (W207-XCOBLEN + 1:)
     ADD 11             TO W207-XCOBLEN
     MOVE 48            TO W207-XBMP
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
     .
 D310-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das DKV-AS
******************************************************************
 D312-DKV SECTION.
 D312-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DKV TO TRUE

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D312-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das BP-AS
******************************************************************
 D314-BP SECTION.
 D314-00.
**  ---> Anwendung für MAC-Bildung setzen
*G.02.10 - Anfang
*    SET W66-TOTAL TO TRUE
***
     SET W66-DEFAULT TO TRUE
*G.02.10 - Ende

**  ---> BMP 22 - Eingabeart wird hier überschrieben
     MOVE 22 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN
     MOVE "C10201214144" TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF


*  ---> BMP 42 - VUNR wird hier überschrieben
     MOVE 42 TO S-BMP
     MOVE 1  TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-NOT-FOUND
         PERFORM E900-PUT-ERRLOG
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF

     PERFORM U400-INTERPRET-ABWEICHUNG
     MOVE 42           TO W207-XBMP
     MOVE W-BUFFER     TO W207-XCOBVAL
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-MDNR (7:2) TO W207-XCOBVAL (W207-XCOBLEN + 1:2)

*G.02.08 - Anfang
*    MOVE W-TSNR       TO W207-XCOBVAL (W207-XCOBLEN + 9:8)
**
     MOVE W-TSNR       TO W207-XCOBVAL (W207-XCOBLEN + 3:8)

*G.02.08 - Ende

    ADD 10 TO W207-XCOBLEN
    PERFORM L100-ADD-BMP
    IF  ENDE
        EXIT SECTION
    END-IF

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D314-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das ENI-AS
******************************************************************
 D315-ENI SECTION.
 D315-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

**  ---> BMP 42 muss hier nochmal überarbeitet werden
     MOVE "3280"  TO W-BUFFER
     MOVE 4      TO W-BUFFER-LEN

**  ---> und nun der helle Wahnsinn !!!  lt. Kay für erfolgreiche Tests
     IF  W-MDNR = 99 and W-TSNR = 1
         MOVE 1154 TO W-KONV-TSNR
     ELSE
         MOVE W-TSNR TO W-KONV-TSNR
     END-IF

**  ---> führende Nullen entfernen
     MOVE ZERO TO C4-PTR
     INSPECT W-KONV-TSNR-STR TALLYING C4-PTR for LEADING SPACES
     MOVE W-KONV-TSNR-STR (C4-PTR + 1:8 - C4-PTR)
         TO W-BUFFER (W-BUFFER-LEN + 1:8 - C4-PTR)
     COMPUTE W-BUFFER-LEN = W-BUFFER-LEN + 8 - C4-PTR

     MOVE 42           TO W207-XBMP
     MOVE W-BUFFER     TO W207-XCOBVAL
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D315-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das ORLEN-AS
******************************************************************
 D316-ORLEN SECTION.
 D316-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

**  ---> BMP 22 - Eingabeart wird hier überschrieben
     MOVE 22 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN

     IF  ERFASSUNGS-ART OF TXILOG70-AUT = 01
**      ---> handeingabe
         MOVE "C10101654144" TO W207-XCOBVAL
     ELSE
         MOVE "C10201214144" TO W207-XCOBVAL
     END-IF
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF


**  ---> BMP 42 muss hier nochmal überarbeitet werden
     MOVE "276"  TO W-BUFFER
     MOVE 3      TO W-BUFFER-LEN
     MOVE W-TSNR TO W-KONV-TSNR
**  ---> führende Nullen entfernen
     MOVE ZERO TO C4-PTR
     INSPECT W-KONV-TSNR-STR TALLYING C4-PTR for LEADING SPACES
     MOVE W-KONV-TSNR-STR (C4-PTR + 1:8 - C4-PTR)
         TO W-BUFFER (W-BUFFER-LEN + 1:8 - C4-PTR)
     COMPUTE W-BUFFER-LEN = W-BUFFER-LEN + 8 - C4-PTR

     MOVE 42           TO W207-XBMP
     MOVE W-BUFFER     TO W207-XCOBVAL
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D316-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das UTA-AS
******************************************************************
 D317-UTA SECTION.
 D317-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-UTA TO TRUE

** G.00.02 - Anfang
**  ---> BMP41 ausschalten UTA-Spezifikation - Not required
     MOVE ZERO TO W207-TBMP(41)


**  ---> BMP39 einschalten
     MOVE 1 TO W207-TBMP(39)

**  ---> BMP39  ACODE
     MOVE 39    TO W207-XBMP
     MOVE ZEROS TO W207-XCOBVAL
     MOVE 2     TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

** G.00.02 - Ende

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D317-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das TND-AS
******************************************************************
 D318-TND SECTION.
 D318-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-TND TO TRUE

*G.02.18 - Anfang
**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
*G.02.18 - Ende

     continue
     .
 D318-99.
     EXIT.

******************************************************************
* spezielle Behandlung für das Avia-AS
* ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
******************************************************************
 D322-EUROWAG SECTION.
 D322-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

**  ---> BMP 14 - Ablaufdatum
     MOVE 14   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "0"
             MOVE ZERO TO W207-TBMP-O (14:1)
         END-IF
     END-IF

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D322-99.
     EXIT.

*G.02.09 - Ende

*G.02.13 - Anfang

******************************************************************
* spezielle Behandlung für das LogPay-AS
* ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
******************************************************************
 D323-LOGPAY SECTION.
 D323-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

**  ---> BMP 14 - Ablaufdatum
     MOVE 14   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "0"
             MOVE ZERO TO W207-TBMP-O (14:1)
         END-IF
     END-IF

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D323-99.
     EXIT.

*G.02.13 - Ende

*G.02.37 - Anfang
******************************************************************
* Spezielle Behandlung für das Stiglechner-AS
******************************************************************
 D324-STIGLECHNER SECTION.
 D324-00.
**  ---> Anwendung für MAC-Bildung setzen
**   SET W66-DEFAULT TO TRUE
     SET W66-DKV TO TRUE

**  ---> BMP 14 - Ablaufdatum
     MOVE 14   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "0"
             MOVE ZERO TO W207-TBMP-O (14:1)
         END-IF
     END-IF

**  ---> BMP39 einschalten
     MOVE 1 TO W207-TBMP(39)

**  ---> BMP39  ACODE
     MOVE 39    TO W207-XBMP
*G.02.45 - AC aus Anfrage senden
*     MOVE ZEROS TO W207-XCOBVAL
     MOVE AC-AS OF TXILOG70-AUT TO D-NUM3
     MOVE D-NUM3                TO W207-XCOBVAL
     MOVE 3                     TO W207-XCOBLEN
*G.02.45 - Ende
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.41 - nicht BMP 48 Default
**  ---> BMP 48
**  ---> zunächst die BitMap erstellen
     MOVE ALL ZEROES TO W-BYTEMAP-48
     MOVE LOW-VALUE TO W-BITMAP
     MOVE SPACES     TO W207-XCOBVAL
     MOVE 8          TO W207-XCOBLEN

     MOVE "1"        TO W-BYTEMAP-48(4:1)
     MOVE "1"        TO W-BYTEMAP-48(39:1)

**  +++>  nun die Bitmap einfügen
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE W-BITMAP TO W207-XCOBVAL (1:8)

**  +++> jetzt das Subfeld 4 (Fixwert)
     MOVE "0000000001" TO W207-XCOBVAL (W207-XCOBLEN + 1:)
     ADD 10 TO W207-XCOBLEN

**  ---> Fuehrende Nullen fuer 10 stelliges Feld 39 moven
     MOVE "000000"     TO W207-XCOBVAL (W207-XCOBLEN + 1:)
     ADD 6 TO W207-XCOBLEN
     MOVE W-ABWKZ(3:4)    TO W207-XCOBVAL (W207-XCOBLEN + 1:)
     ADD 4 TO W207-XCOBLEN

     MOVE 48 TO W207-XBMP
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
**  ---> und BMP48 aufbereiten
*     PERFORM E310-BMP48-DEFAULT
*G.02.41 - Ende
     .
 D324-99.
     EXIT.

*G.02.37 - Ende

******************************************************************
* spezielle Behandlung für das Roadrunner-AS
*G.02.49 - neu
******************************************************************
 D325-ROADRUNNER SECTION.
 D325-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

**  ---> BMP 14 - Ablaufdatum
     MOVE 14   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "0"
             MOVE ZERO TO W207-TBMP-O (14:1)
         END-IF
     END-IF


**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D325-99.
     EXIT.

******************************************************************
* bestimmen Routing etc
******************************************************************
 D900-ROUTING-ETC SECTION.
 D900-00.

*G.02.33 - Pruefung ob Wdh. vorhanden/bereits zu Umsatz geworden
     MOVE W-TERMNR       TO TERMNR  OF TXILOG70-WDH
     MOVE W-TERMNR (7:2) TO PNR     OF TXILOG70-WDH
     MOVE W-TRACENR      TO TRACENR OF TXILOG70-WDH
     MOVE 401            TO ISONTYP OF TXILOG70-WDH
     PERFORM S300-SELECT-TXILOG70-WDH
     IF WDH-VORHANDEN
        EXIT SECTION
     END-IF

*G.02.42 - brauchen wir hier schon
     MOVE IMSG-NTYPE     TO ISONTYP        of TXILOG70
*G.02.42 - Ende
     PERFORM G126-SELECT-UMSWEAT
     IF KZ-BEARB OF UMSWEAT = "V"
        SET UMS-NEU  TO TRUE
     ELSE
        SET UMS-DONE TO TRUE
     END-IF
*G.02.33 - Ende

**  ---> Werte für Zugriff auf Autorisierung belegen
     MOVE W-TERMNR       TO TERMNR  of TXILOG70-AUT
     MOVE W-TERMNR (7:2) TO PNR     of TXILOG70-AUT
     MOVE W-TRACENR-37   TO TRACENR of TXILOG70-AUT
     MOVE 200            TO ISONTYP of TXILOG70-AUT
     PERFORM S200-SELECT-TXILOG70-AUT
     IF  W-AC not = ZERO
         EXIT SECTION
     END-IF

     MOVE CARDID of TXILOG70-AUT TO ROUT-CARDID
                                    W-CARDID
                                    S-CARDID
     MOVE KANR of TXILOG70-AUT   TO W-KANR

*G.02.27 - Anfang
**  ---> BMP2 (PAN) zunächst mal Länge der KANR bestimmen

     MOVE ZERO TO W-KANR-LEN
     PERFORM VARYING C4-I1 FROM 19 BY -1
             UNTIL   C4-I1 = ZERO

         IF  KANR OF TXILOG70-AUT (C4-I1:1) not = SPACE
             MOVE C4-I1 TO W-KANR-LEN
             EXIT PERFORM
         END-IF

     END-PERFORM

**      ---> wenn Länge = 0: Fehlermeldung und ENDE
     IF  W-KANR-LEN = ZEROS
         SET ENDE TO TRUE
         MOVE "Keine KANR in =TXILOG70-AUT" TO DATEN-BUFFER1
         STRING  "TermNr./TraceNr. = "
                  W-TERMNR
                 "/"
                  W-TRACENR
          delimited by size
           INTO  DATEN-BUFFER2
         END-STRING
     END-IF
*G.02.27 - Ende

**  ---> nun auch die Terminal Autorisierungstransaktion holen
     MOVE W-TERMNR       TO TERMNR  of TXNLOG70-ATS
     MOVE W-TERMNR (7:2) TO PNR     of TXNLOG70-ATS
     MOVE W-TRACENR-37   TO TRACENR of TXNLOG70-ATS
     MOVE 200            TO ISONTYP of TXNLOG70-ATS
     MOVE "T"            TO KZ-MSG  of TXNLOG70-ATS
     PERFORM S210-SELECT-TXNLOG70-ATS
     IF  W-AC not = ZERO
         EXIT SECTION
     END-IF

**  ---> und nun die TS-Nachricht aufbereiten
     MOVE LEN of ANFRAGE of TXNLOG70-ATS TO WATS-ISOLEN
     MOVE VAL of ANFRAGE of TXNLOG70-ATS TO WATS-ISOSTRING
     SET WATS-EC      TO TRUE
     SET WATS-ISO2COB TO TRUE

     PERFORM M135-CALL-WISO207-WATS
**  ---> nun die Anfrage aufbereiten
     IF  WATS-RCODE NOT = ZERO
         MOVE WATS-RCODE TO D-NUM4
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         STRING "WISO207 (ISO2COB)/@"
                "RC: " D-NUM4
                "@"
                 delimited by size
           INTO DATEN-BUFFER1
         END-STRING
         PERFORM Z002-PROGERR
         SET ENDE TO TRUE
     END-IF

**  ---> VUNR / Routkz über TSKART40 holen
     MOVE IMSG-MDNR TO MDNR    of TSKART40
     MOVE IMSG-TSNR TO TSNR    of TSKART40

*G.02.31 - Anfang
*    IF  IMSG-TBMP(25) = 1
*        MOVE 2     TO CARDSYS OF TSKART40
*    ELSE
*        MOVE 1     TO CARDSYS OF TSKART40
*    END-IF
**
     IF IMSG-TBMP(25) = 1
     OR BETRAG-ART OF TXILOG70-AUT = "M"
         MOVE 2 TO CARDSYS OF TSKART40
     ELSE
         MOVE 1 TO CARDSYS OF TSKART40
     END-IF

*G.02.31 - Anfang

     MOVE W-CARDID  TO CARDID  of TSKART40

     PERFORM S150-SELECT-TSKART40
     IF  W-AC not = ZERO
         EXIT SECTION
     END-IF

**  ---> Routinginformationen über Modul WSYS930 holen
     MOVE W-ROUTKZ TO ROUT-KZ
     SET ROUT-CMD-AS TO TRUE
     PERFORM M150-CALL-WSYS930
*G.02.16
     IF  W-AC NOT = ZERO
         EXIT SECTION
     END-IF

**  ---> FREGAT-Daten setzen
     MOVE ROUT-FREGATTE TO IMSG-NEXTSERV
     MOVE ROUT-DTX      TO IMSG-DTXNR
     IF  ROUT-KZSYNC = "S"
         SET IMSG-SEND-SYNC  TO TRUE
     ELSE
         SET IMSG-SEND-ASYNC TO TRUE
     END-IF
     MOVE ROUT-KZSYNC TO W-LTGIND
     .
 D900-99.
     EXIT.

******************************************************************
* holen AS-Tracenummer
******************************************************************
 D910-GET-ASTRACENR SECTION.
 D910-00.
     MOVE IMSG-MDNR          TO MDNR   of MDNR2AS
     MOVE CARDID of TSKART40 TO CARDID of MDNR2AS
     MOVE W-ROUTKZ           TO ROUTKZ of MDNR2AS
     PERFORM S110-SELECT-MDNR2AS
     IF  ENDE
         EXIT SECTION
     END-IF
     MOVE AIID of MDNR2AS TO W-ACQUIRER-ID
     IF  W-AC NOT = ZERO
**      ---> Ablehnung senden: AS nicht verfügbar
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

**  ---> prüfen auf Überschlag
     IF  TRACENR OF MDNR2AS = 999999
         MOVE 1 TO TRACENR OF MDNR2AS
     ELSE
         ADD  1 TO TRACENR OF MDNR2AS
     END-IF

**  ---> sichern AS-Trace-Nr.
     MOVE TRACENR of MDNR2AS TO W-AS-TRACENR

**  ---> TraceNr. und Datum updaten
     PERFORM S120-UPDATE-MDNR2AS
     IF  W-AC NOT = ZERO
**      ---> Ablehnung senden: AS nicht verfügbar
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF
     .
 D910-99.
     EXIT.

******************************************************************
* generieren FEP-Antwort
******************************************************************
 E100-FEP-ANTWORT SECTION.
 E100-00.

*G.02.00 - Anfang
*   SET FEP-ANTWORT TO TRUE
*G.02.00 - Ende

     MOVE TS-INTERN-MESSAGE TO INTERN-MESSAGE

**  ---> zunaechst Felder ausschalten, die nicht gesendet werden sollen
     MOVE ZERO TO IMSG-TBMP (02)
     MOVE ZERO TO IMSG-TBMP (14)
     MOVE ZERO TO IMSG-TBMP (22)
     MOVE ZERO TO IMSG-TBMP (25)
     MOVE ZERO TO IMSG-TBMP (26)
     MOVE ZERO TO IMSG-TBMP (35)

*G.02.00 - Anfang
     MOVE ZERO TO IMSG-TBMP (37)
*G.02.00 - Ende

*G.02.00 - Anfang
     MOVE ZERO TO IMSG-TBMP (42)
*G.02.00 - Ende

     MOVE ZERO TO IMSG-TBMP (52)
     MOVE ZERO TO IMSG-TBMP (56)
     MOVE ZERO TO IMSG-TBMP (60)
     MOVE ZERO TO IMSG-TBMP (63)

**  ---> im Folgenden noch die Felder BMP 33, 39, 42 hinzufuegen
     MOVE IMSG-COBDATEN TO W207-COBDATEN
     SET W207-EC        TO TRUE

**  ---> Nachrichtentype setzen
     MOVE 0410 TO W207-NTYPE

**  ---> BMP 33: WEAT als AS-ID
     MOVE 33       TO W207-XBMP
     MOVE "740000" TO W207-XCOBVAL
     MOVE 6        TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.26 - Anfang
**  ---> BMP39  ACODE
     MOVE 39   TO W207-XBMP
     MOVE W-AC TO W207-XCOBVAL

     IF  W-AC = ZERO
**      ---> zunächst prüfen, ob ein spez. AC gesendet werden soll
         PERFORM M120-CALL-SYSABL1
         IF TABL-ABL
            MOVE TABL-AC TO W207-XCOBVAL
         END-IF
     END-IF

**   MOVE W-AC  TO W207-XCOBVAL
     MOVE 2     TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
*G.02.26 - Ende

*G.02.00 - Anfang
**  ---> BMP42  VUNR
*    MOVE 42               TO W207-XBMP
*    MOVE VUNR of TSKART40 TO W207-XCOBVAL
*    MOVE 15               TO W207-XCOBLEN
*    PERFORM L100-ADD-BMP
*    IF  ENDE
*        EXIT SECTION
*    END-IF
*G.02.00 - Ende

**  ---> BMP53  Sicherheitsverfahren
**  --->        wird so zurück gegeben wie's gekommen ist

**  ---> Nachricht zusammenstellen
     SET W207-EC TO TRUE
     PERFORM L110-COB2ISO
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.00 - Anfang
**  ---> Logdaten für Eigenantwort bereitstellen
*     PERFORM F100-LOGDATEN-EIGEN
**

*G.02.33 - nur ok Logen alles wenn Umsatz noch unbearbeitet
     IF W-AC = ZEROS AND UMS-NEU
        PERFORM F100-LOGDATEN-EIGEN-OK
     ELSE
        PERFORM F110-LOGDATEN-EIGEN-NOK
     END-IF
*G.02.33 - Ende

*G.02.00 - Ende

**  --> FREGAT-Parameter setzen
     SET  IMSG-WRITE-SL    TO TRUE
     MOVE W-FRE-MONNAME    TO IMSG-NEXTSERV
     MOVE MODUL OF MYPROG  TO IMSG-MONNAME
     .
 E100-99.
     EXIT.

******************************************************************
* bereits erstellte Antwort von erster Wdh. erneut senden
*G.01.33 - neu
******************************************************************
 E200-WDH-ANTWORT SECTION.
 E200-00.
    MOVE W-TERMNR       TO TERMNR  of TXNLOG70-WDH
    MOVE W-TERMNR (7:2) TO PNR     of TXNLOG70-WDH
    MOVE W-TRACENR      TO TRACENR of TXNLOG70-WDH
    MOVE 401            TO ISONTYP of TXNLOG70-WDH
    MOVE "T"            TO KZ-MSG  of TXNLOG70-WDH
    PERFORM S310-SELECT-TXNLOG70-WDH
    IF TXNLOG70-NOK
        EXIT SECTION
    END-IF

    MOVE VAL OF ANTWORT OF TXNLOG70-WDH TO IMSG-NDATEN
    MOVE LEN OF ANTWORT OF TXNLOG70-WDH TO IMSG-DATLEN

    MOVE W-TERMNR TO P-HEX16
    PERFORM P900-WTHEX
    MOVE P-HEX8(1:4) TO TERMID OF GEN-ERROR
    MOVE "TEILSTORNO MEHRFACHWIEDERHOLUNG Nachricht wird mit Antwort"
        TO DATEN-BUFFER1
    STRING "aus Teilstorno Wiederholung "
           W-TERMNR
           "/"
           W-TRACENR
           " beantwortet."
         delimited by size
    INTO DATEN-BUFFER2
    END-STRING
    MOVE "Terminalanfrage und Antwort werden nicht geloggt !"
        TO DATEN-BUFFER3
    PERFORM Z002-PROGERR

**  --> FREGAT-Parameter setzen
     SET  IMSG-WRITE-SL    TO TRUE
     MOVE W-FRE-MONNAME    TO IMSG-NEXTSERV
     MOVE MODUL OF MYPROG  TO IMSG-MONNAME
     .
 E200-99.
     EXIT.



******************************************************************
* Aufruf Artikelmapper
******************************************************************
 E305-01-ARTIKELDATEN SECTION.
 E305-01-00.
**  ---> Artikel mappen über Mappingserver WXAMP06
     SET AMP-P2S TO TRUE
     MOVE IMSG-CF(IMSG-TPTR(63):IMSG-TLEN(63))
                             TO AMP-POS-VAL
     MOVE IMSG-TLEN(63)      TO AMP-POS-LEN
     MOVE IMSG-MDNR          TO AMP-MDNR
     MOVE W-TSNR             TO AMP-TSNR
     MOVE CARDID of TSKART40 TO AMP-CARDID

*G.02.49 - Evaluate ist ueberfluessig, AS-VERF wird verwendet
**G.02.07 - Anfang
*     EVALUATE W-ROUTKZ
*         WHEN 05     MOVE "AV" TO AMP-FORMAT
*         WHEN 07     MOVE "SH" TO AMP-FORMAT
*         WHEN 10     MOVE "TO" TO AMP-FORMAT
*         WHEN 12     MOVE "DK" TO AMP-FORMAT
*         WHEN 14     MOVE "BP" TO AMP-FORMAT
*         WHEN 15     MOVE "AG" TO AMP-FORMAT
*         WHEN 16     MOVE "OR" TO AMP-FORMAT
*         WHEN 17     MOVE "UT" TO AMP-FORMAT
*         WHEN 18     MOVE "TN" TO AMP-FORMAT
*
**G.02.09 - Anfang
*         WHEN 22     MOVE "EU" TO AMP-FORMAT
**G.02.09 - Ende
*
**G.02.13 - Anfang
*         WHEN 23     MOVE "LO" TO AMP-FORMAT
**G.02.13 - Ende
*
**G.02.37 - Anfang
*         WHEN 24     MOVE "IQ" TO AMP-FORMAT
**G.02.37 - Ende
*
**G.02.49 - Roadrunner neu
*         WHEN 25     MOVE "RR" TO AMP-FORMAT
**G.02.49 - Ende
*         WHEN OTHER
*              CONTINUE
*     END-EVALUATE
**G.02.07 - Ende
*G.02.49 - Ende

**  ---> sollte Mapping für AGIP verändert werden, wirkt sich das auch hier aus
*     MOVE MODUL OF MYPROG(2:2)        TO AMP-FORMAT
     MOVE AS-VERF    TO AMP-FORMAT
     MOVE ALL SPACES TO AMP-HOST-VAL
     MOVE ZERO       TO AMP-RC
                        AMP-HOST-LEN

**  ---> Pathsend zu WXAMP06S
     MOVE 20 TO INLINE-SERVICE
     PERFORM P100-PATHSEND
     .
 E305-01-99.
     EXIT.

******************************************************************
* Aufbereitung BMP48 default
******************************************************************
 E310-BMP48-DEFAULT SECTION.
 E310-00.
**  ---> BMP 48 - geht erst nach Artikel-Mapper (fummelt aus BMP63 ggf.
**  --->          noch Fahrerdaten, die einzustellen sind (48.8))
**  ---> zunächst die BitMap erstellen
     MOVE ALL ZEROES TO W-BYTEMAP-48
     MOVE  LOW-VALUE TO W-BITMAP
     MOVE SPACES     TO W207-XCOBVAL
     MOVE 8          TO W207-XCOBLEN

     MOVE "1"        TO W-BYTEMAP-48(4:1)

*G.02.41 - nicht mehr benoetigt
**G.02.38 - Anfang
***Subfeld 39, ggf. aus =FCPARAM
*     MOVE 48 TO S-BMP
*     MOVE 39 TO S-LFDNR
*     PERFORM U300-SEARCH-TAB
*     IF PRM-FOUND
*        PERFORM U400-INTERPRET-ABWEICHUNG
*        MOVE W-BUFFER (1:W-BUFFER-LEN)
*          TO W207-XCOBVAL (W207-XCOBLEN + 1:)
*        ADD W-BUFFER-LEN TO W207-XCOBLEN
*        MOVE "1"         TO W-BYTEMAP-48(39:1)
*     END-IF
**G.02.38 - Ende
*G.02.41 - Ende

**  +++>  nun die Bitmap einfügen
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE W-BITMAP TO W207-XCOBVAL (1:8)

**  +++> jetzt das Subfeld 4 (Fixwert)
     MOVE "0000000001" TO W207-XCOBVAL (W207-XCOBLEN + 1:)
     ADD 10 TO W207-XCOBLEN

     MOVE 48 TO W207-XBMP
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
     .
 E310-99.
     EXIT.


******************************************************************
* erzeugen Fehlermeldung
******************************************************************
 E900-PUT-ERRLOG SECTION.
 E900-00.
     MOVE 1105 TO ERROR-NR of GEN-ERROR
     MOVE "=FCPARAM für: @" TO DATEN-BUFFER1
     MOVE W-ROUTKZ  TO D-NUM4
     MOVE W-CARDID  TO D-NUM4OV
     MOVE S-ISONTYP TO D-NUM4M
     MOVE S-BMP     TO D-NUM4N
     STRING  "ROUTKZ/CARDID/ISONTYP/KZ-MSG/BMP = "
             D-NUM4 "/" D-NUM4OV "/" D-NUM4M "/" S-KZ-MSG "/" D-NUM4N
                 delimited by size
       INTO  DATEN-BUFFER2
     END-STRING
     PERFORM Z002-PROGERR
     .
 E900-99.
     EXIT.

*G.02-00 - Anfang
******************************************************************
* Logdaten bei Eigenantworten bereitstellen
******************************************************************
 F100-LOGDATEN-EIGEN-OK SECTION.
 F100-00.
**  ---> TS-Anfragewerte in TS-INTERN-MESSAGE (TS-..)
**  ---> Eigenantwort oder AS-Anfrage in INTERN-MESSAGE (IMSG-..)

**  ---> Führungstabelle =TXILOG70
     PERFORM G100-PUT-TXILOG70
**  ---> und schreiben
     PERFORM S180-INSERT-TXILOG70
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.05 - Anfang

**  ---> Eintrag TRACENR_S in TXILOG70 der Autrisierung
     PERFORM G102-PUT-TXILOG70-AUT
     PERFORM S185-UPDATE-TXILOG70-AUT
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.05 - Ende

**  ---> Anfragenachricht für Tabelle =TXNLOG70
*G.02.00 - Anfang
     PERFORM G105-PUT-TXNLOG70-TS-ANTWORT
*G.02.00 - Ende
    PERFORM G110-PUT-TXNLOG70-TS
**  ---> und nun schreiben
     PERFORM S190-INSERT-TXNLOG70-TS
     IF ENDE
        EXIT SECTION
     END-IF

     PERFORM S170-ASYNC70-INSERT

     IF ASYNC70-NOK
        MOVE 91 TO W-AC
        EXIT SECTION
     END-IF

**  ---> und schliesslich Eintrag in CRDUSEDN erzeugen
     PERFORM G130-PUT-CRDUSEDN
**  ---> und nun schreiben

*G.02.28 - Anfang
*    PERFORM M100-CALL-SDBCDU5
*G.02.28 - Ende

     .
 F100-99.
     EXIT.

******************************************************************
* Logdaten bei Eigenantworten bereitstellen
******************************************************************
 F110-LOGDATEN-EIGEN-NOK SECTION.
 F110-00.
**  ---> TS-Anfragewerte in TS-INTERN-MESSAGE (TS-..)
**  ---> Eigenantwort oder AS-Anfrage in INTERN-MESSAGE (IMSG-..)

**  ---> Führungstabelle =TXILOG70
    PERFORM G100-PUT-TXILOG70
**  ---> und schreiben
    PERFORM S180-INSERT-TXILOG70
    IF  ENDE
        EXIT SECTION
    END-IF

**  ---> Anfragenachricht für Tabelle =TXNLOG70
*G.02.00 - Anfang
        PERFORM G105-PUT-TXNLOG70-TS-ANTWORT
*G.02.00 - Ende
        PERFORM G110-PUT-TXNLOG70-TS
**  ---> und nun schreiben
     PERFORM S190-INSERT-TXNLOG70-TS
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und schliesslich Eintrag in CRDUSEDN erzeugen
     PERFORM G130-PUT-CRDUSEDN
**  ---> und nun schreiben

*G.02.28 - Anfang
*    PERFORM M100-CALL-SDBCDU5
*G.02.28 - Ende

     .
 F110-99.
     EXIT.

*G.02.00 - Ende

******************************************************************
* Einstellen Daten in ASYN70 Buffer
******************************************************************
 G090-PUT-ASNYC70 SECTION.
 G090-00.
      MOVE W-TERMNR (7:2)  TO        PNR        OF ASYNC70
      MOVE W-TERMNR        TO        TERMNR     OF ASYNC70
      MOVE W-TRACENR       TO        TRACENR    OF ASYNC70
      MOVE W-NTYPE         TO        ISONTYP    OF ASYNC70
      MOVE W-MDNR          TO        MDNR       OF ASYNC70
      MOVE W-TSNR          TO        TSNR       OF ASYNC70
      MOVE "FK"            TO        VKZ        OF ASYNC70
      MOVE W-AS-TRACENR    TO        TRACENR-AS OF ASYNC70
      MOVE W-CARDID        TO        CARDID     OF ASYNC70
      MOVE W-ROUTKZ        TO        ROUTKZ     OF ASYNC70
      MOVE "A"             TO        KZ-BEARB   OF ASYNC70
*G.02.45 - Jetzt ueber Serverparameter fuellen
      IF STORNO-WIEDERHOLUNG
         MOVE W-ANZREPW    TO        ANZ-REP    OF ASYNC70
*         MOVE 2            TO        ANZ-REP    OF ASYNC70
      ELSE
         MOVE W-ANZREPA    TO        ANZ-REP    OF ASYNC70
*         MOVE 1            TO        ANZ-REP    OF ASYNC70
      END-IF
*G.02.45 - Ende
      MOVE IMSG-HEADER     TO        FREHEADER  OF ASYNC70
      MOVE IMSG-NDATEN     TO VAL OF ANFRAGE    OF ASYNC70
      MOVE IMSG-SENDLEN    TO LEN OF ANFRAGE    OF ASYNC70
      MOVE SPACES          TO VAL OF ANTWORT    OF ASYNC70
      MOVE ZEROS           TO LEN OF ANTWORT    OF ASYNC70
      MOVE W-KEYNAME       TO        KEY-NAME   OF ASYNC70
     .

 G090-99.
     EXIT.

******************************************************************
* Erstellen Antwort-MAC
******************************************************************
 F920-MAC-BILDEN SECTION.
 F920-00.
**  ---> MAC-Bildung und Schlüsselwechsel über WSY7066
**  ---> Datenbereich Message-datei external übergibt Daten für WSY7066
*G.02.14 - Anfang
     EVALUATE W-ROUTKZ
        WHEN 22
             PERFORM F950-ASMAC-DUKPT
        WHEN OTHER
         MOVE 88 TO W66-RCODE of W66-WSY7066C
         PERFORM M140-CALL-WSY7066
         IF  W66-ERR
             MOVE 30   TO W-AC
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
             SET ENDE TO TRUE
             EXIT SECTION
     END-IF
   END-EVALUATE
*G.02.14 - Ende

**  --> Datenteil wird in WSY7066 mit MAC versehen
     .
 F920-99.
      EXIT.

*G.02.14 - Anfang
******************************************************************
* MAC bilden
******************************************************************
 F950-ASMAC-DUKPT SECTION.
 F950-00.
*
      SET Z-MAC-BILDEN-AS  TO TRUE

      MOVE 88              TO Z-RCODE OF Z-WEUR056C
      MOVE ALL LOW-VALUES  TO Z-AKEY-NAME
      MOVE W-MACKEYA       TO Z-AKEY-NAME (1:4)

      MOVE W-AS-TRACENR    TO Z-AS-TRACENR

      CALL "WEUR056"  USING Z-WEUR056C,
                            INTERN-MESSAGE

      IF Z-ERR
         MOVE Z-RCODE TO D-NUM4
         MOVE 30      TO W-AC
         MOVE 1201    TO ERROR-NR of GEN-ERROR
         MOVE Z-RCODE TO D-NUM4
         STRING "Returncode aus WEUR056: " delimited by size
                 D-NUM4                    delimited by size
                "@@"                       delimited by size
           INTO DATEN-BUFFER1
         MOVE "Bei F950-ASMAC-DUKPT" TO DATEN-BUFFER2
         MOVE "Transaktions-Ende"   TO DATEN-BUFFER3
         PERFORM Z002-PROGERR
         SET ENDE TO TRUE
         EXIT SECTION
      END-IF
   .
 F950-99.
   EXIT.
*G.02.14 - Ende

******************************************************************
* Einstellen Daten in TXILOG70 Buffer
******************************************************************
 G100-PUT-TXILOG70 SECTION.
 G100-00.
     MOVE W-TERMNR (7:2) TO PNR            of TXILOG70
     MOVE W-TERMNR       TO TERMNR         of TXILOG70
     MOVE W-TRACENR      TO TRACENR        of TXILOG70
     MOVE W-NTYPE        TO ISONTYP        of TXILOG70
     MOVE W-MDNR         TO MDNR           of TXILOG70
     MOVE W-TSNR         TO TSNR           of TXILOG70
     MOVE W-AS-TRACENR   TO TRACENR-AS     of TXILOG70
     MOVE W-TRACENR-37   TO TRACENR-S      OF TXILOG70
     MOVE W-KANR         TO KANR           of TXILOG70
     MOVE SPACES         TO KZ-E2EE        of TXILOG70
     MOVE SPACES         TO KEYNAME        of TXILOG70
     MOVE W18-BETRAG     TO BETRAG         of TXILOG70
     MOVE "F"            TO BETRAG-ART     of TXILOG70

*G.02.00 - Anfang
     MOVE BETRAG-AUTOR of TXILOG70-AUT
       TO BETRAG-AUTOR of TXILOG70
*G.02.00 - Ende

     MOVE W-CARDID       TO CARDID         of TXILOG70
     MOVE W-ROUTKZ       TO ROUTKZ         of TXILOG70
     MOVE W-LTGIND       TO LTGIND         of TXILOG70
     MOVE 740000         TO ASID           of TXILOG70
     MOVE 9999           TO AC-AS          of TXILOG70
*G.02.30 - Anfang
*    MOVE W-AC           TO AC-TERM        of TXILOG70
**

     IF TABL-ABL
        MOVE TABL-AC     TO AC-TERM        of TXILOG70
     ELSE
        MOVE W-AC        TO AC-TERM        of TXILOG70
     END-IF
*G.02.30 - Ende
     MOVE WKZ-WKZ-A      TO WKZ            of TXILOG70
     MOVE 70             TO LOGPROT        of TXILOG70
*G.02.06 - Anfang
*    MOVE "T"            TO KZ-BEARB       of TXILOG70
*    MOVE "Z"            TO KZ-UMSATZ      of TXILOG70
**

     MOVE "R"            TO KZ-BEARB       of TXILOG70
     MOVE "T"            TO KZ-UMSATZ      of TXILOG70

*G.02.06 - Ende

     MOVE "f"            TO KZ-VERF        of TXILOG70
     MOVE W-ABL          TO ABL-JJMM       of TXILOG70
     MOVE W-ACQUIRER-ID  TO ACQUIRER-ID    of TXILOG70
     MOVE W-ERFASSUNGS-ART TO ERFASSUNGS-ART of TXILOG70

*kl20180316 - G.02.40 - Unterscheidung zwischen Chip und Spur2
*     MOVE 221            TO KARTEN-ART     of TXILOG70
     IF W-ERF-CHIP
*       Kartenart = Chip ohne Cashback
        MOVE   211       TO KARTEN-ART     of TXILOG70
     ELSE
*       Kartenart = Spur2 Magnet
        MOVE   221       TO KARTEN-ART     of TXILOG70
     END-IF
*kl20180316 - G.02.40 - Ende

*G.02.22 - Trans-Art aus Anfrage uebernehmen
*     MOVE "2E"        TO TRANS-ART      of TXILOG70
     MOVE TRANS-ART OF TXILOG70-AUT TO TRANS-ART OF TXILOG70-AUT
*G.02.22 - Ende
     MOVE SPACES         TO TRANS-TYP      of TXILOG70
     MOVE 5541           TO BRANCHEN-KZ    of TXILOG70
     MOVE VUNR of TSKART40 TO VUNR         of TXILOG70
     MOVE W-ZP-VERKAUF   TO ZP-VERKAUF     of TXILOG70

*G.02.29 - Anfang
**G.02.06 - Anfang
*     MOVE IMSG-TLEN(63)
*       TO LEN OF ARTIKEL                   of TXILOG70
*     MOVE IMSG-CF(IMSG-TPTR(63):IMSG-TLEN(63))
*       TO VAL OF ARTIKEL                   of TXILOG70
*
**G.02.06 - Ende
*G.02.29 - Ende
**  ---> holen momentanen Zeitpunkt
     PERFORM U200-TIMESTAMP

*G.02.00 - Anfang
*     IF  FEP-ANTWORT
*         MOVE TAGESDATUM TO ZP-TOUT of TXILOG70
*     ELSE
*         MOVE TAGESDATUM TO ZP-AOUT of TXILOG70
*     END-IF
**
         MOVE TAGESDATUM TO ZP-TOUT of TXILOG70
*G.02.00 - Ende

         MOVE H-ZP-IN        TO ZP-TIN  of TXILOG70
     .
 G100-99.
     EXIT.

*G.02.05 - Anfang

******************************************************************
* Einstellen Daten in TXILOG70-AUT Buffer
******************************************************************
 G102-PUT-TXILOG70-AUT SECTION.
 G102-00.

     MOVE W-TERMNR (7:2) TO PNR       of TXILOG70-AUT
     MOVE W-TERMNR       TO TERMNR    of TXILOG70-AUT
     MOVE W-TRACENR-37   TO TRACENR   of TXILOG70-AUT
     MOVE 200            TO ISONTYP   of TXILOG70-AUT
     MOVE W-TRACENR      TO TRACENR-S OF TXILOG70-AUT
     .

 G102-99.
     EXIT.

*G.02.05 - Ende

*G.02.00 - Anfang
******************************************************************
* Einstellen Daten in TXNLOG70-TS Antwort - Buffer
******************************************************************
 G105-PUT-TXNLOG70-TS-ANTWORT SECTION.
 G105-00.
     MOVE IMSG-NDATEN    TO VAL of ANTWORT of TXNLOG70-TS
     MOVE IMSG-DATLEN    TO LEN of ANTWORT of TXNLOG70-TS
     .
 G102-99.
     EXIT.
*G.02.00 - Ende

******************************************************************
* Einstellen Daten in TXNLOG70-TS Buffer
******************************************************************
 G110-PUT-TXNLOG70-TS SECTION.
 G110-00.
     MOVE W-TERMNR (7:2) TO PNR            of TXNLOG70-TS
     MOVE W-TERMNR       TO TERMNR         of TXNLOG70-TS
     MOVE W-TRACENR      TO TRACENR        of TXNLOG70-TS
     MOVE W-NTYPE        TO ISONTYP        of TXNLOG70-TS
     MOVE "T"            TO KZ-MSG         of TXNLOG70-TS
     MOVE 1              TO ISO-VERF       of TXNLOG70-TS
     MOVE W-MDNR         TO MDNR           of TXNLOG70-TS
     MOVE W-TSNR         TO TSNR           of TXNLOG70-TS
     MOVE K-MODUL        TO LOG-SRV        of TXNLOG70-TS
     MOVE TS-HEADER      TO FREHEADER      of TXNLOG70-TS
     MOVE TS-NDATEN      TO VAL of ANFRAGE of TXNLOG70-TS
     MOVE TS-DATLEN      TO LEN of ANFRAGE of TXNLOG70-TS
     .
 G110-99.
     EXIT.

*G.02.00 - Anfang
******************************************************************
* Einstellen Daten in UMSWEAT Buffer
******************************************************************
 G125-PUT-UMSWEAT SECTION.
 G125-00.
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
     MOVE "S"                    TO KZ-BEARB  of UMSWEAT
     MOVE W-BELEGNR              TO BELEGNR   of UMSWEAT
     MOVE W-ABWKZ                TO ABWKZ     of UMSWEAT

**  ---> Aufruf Modul IUMSw07 (Zugriff zum UMSIFSF-Server)
     MOVE UMSWEAT    TO WUMS-UMSATZ
     MOVE K-MODUL    TO WUMS-ABSENDER
     SET WUMS-TAB-UW TO TRUE

*G.02.04 - Anfang
*    SET WUMS-CMD-UB TO TRUE
**
** Stornierung gegen Null, dann Umsweat löschen
*    IF BETRAG OF UMSWEAT = ZEROS
**
     IF C18-BETRAG = ZEROS
*G.02.04 - Ende
        SET WUMS-CMD-DB TO TRUE
     ELSE
        SET WUMS-CMD-UB TO TRUE
     END-IF

     PERFORM M180-CALL-IUMSW07
     .
 G125-99.
     EXIT.
*G.02.00 - Ende

******************************************************************
* Select ueber IUMSW07 auf UMSWEAT der Voraut.
*G.02.33 - neu fuer Select
******************************************************************
 G126-SELECT-UMSWEAT SECTION.
 G126-00.
     MOVE W-TERMNR (7:2)         TO PNR       of UMSWEAT
     MOVE W-TERMNR               TO TERMNR    of UMSWEAT
     MOVE W-TRACENR-37           TO TRACENR   of UMSWEAT
     MOVE MDNR      of TXILOG70  TO MDNR      of UMSWEAT
     MOVE TSNR      of TXILOG70  TO TSNR      of UMSWEAT
     MOVE W-BELEGNR              TO BELEGNR   of UMSWEAT

**  ---> Aufruf Modul IUMSw07 (Zugriff zum UMSWEAT-Server)
     MOVE UMSWEAT    TO WUMS-UMSATZ
     MOVE K-MODUL    TO WUMS-ABSENDER
     SET WUMS-TAB-UW TO TRUE
     SET WUMS-CMD-SB TO TRUE

     PERFORM M180-CALL-IUMSW07
     .
 G126-99.
     EXIT.

******************************************************************
* Einstellen Daten in CRDUSEDN-Buffer
******************************************************************
 G130-PUT-CRDUSEDN SECTION.
 G130-00.

*G.02.19 - Anfang
     IF   W-KANR-LEN > ZEROS
     AND  W-KANR     > SPACES

          MOVE W-KANR (W-KANR-LEN:1) TO SDB-PNR
          MOVE W-KANR         TO SDB-KANR
          MOVE "S"            TO SDB-AKZ
          MOVE W-TERMNR       TO SDB-TERMNR
          MOVE W-TRACENR      TO SDB-TRACENR
          MOVE W-AC           TO SDB-AC
          MOVE BETRAG of TXILOG70 TO SDB-BETRAG
          MOVE W-MDNR         TO SDB-MDNR
          MOVE W-TSNR         TO SDB-TSNR
     END-IF
*G.02.19 - Ende
     .
 G130-99.
     EXIT.

******************************************************************
* Felder zusaetzlich in Nachricht einbauen
******************************************************************
 L100-ADD-BMP SECTION.
 L100-00.
     SET W207-ADD-BMP TO TRUE
     PERFORM M130-CALL-WISO207-W207

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
     PERFORM M130-CALL-WISO207-W207

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

**  ---> Nachricht übertragen
     MOVE W207-ISOSTRING TO IMSG-NDATEN
     MOVE W207-COBDATEN  TO IMSG-COBDATEN
     MOVE W207-ISOLEN    TO IMSG-SENDLEN
                            IMSG-DATLEN

**  ---> nur fürs testen
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

*G.02.26 - Anfang
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
         WHEN TABL-OK
              CONTINUE
         WHEN TABL-ABL
              STRING ">>> "
                      TABL-AC
                     " zum Terminal gesendet (TERMABL) <<<"
                DELIMITED BY SIZE
                INTO DATEN-BUFFER1
              END-STRING
              PERFORM Z002-PROGERR
         WHEN OTHER
              MOVE 1201       TO ERROR-NR OF GEN-ERROR
              MOVE TABL-RCODE TO D-NUM4
              STRING "SYSABL1@"
                      D-NUM4
                     "@"
                      W-TERMNR
                     "@"
                DELIMITED BY SIZE
                INTO  DATEN-BUFFER1
              END-STRING
              PERFORM Z002-PROGERR
     END-EVALUATE
     .
 M120-99.
     EXIT.

*G.02.26 - Ende

******************************************************************
* Aufruf Modul WISO207
******************************************************************
 M130-CALL-WISO207-W207 SECTION.
 M130-00.
     CALL "WISO207" USING W207-WISO207C
     .
 M130-99.
     EXIT.

******************************************************************
* Aufruf Modul WISO207
******************************************************************
 M135-CALL-WISO207-WATS SECTION.
 M135-00.
     CALL "WISO207" USING WATS-WISO207C
     .
 M135-99.
     EXIT.

******************************************************************
* Aufruf Modul WSY7066
******************************************************************
 M140-CALL-WSY7066 SECTION.
 M140-00.
     CALL "WSY7066" USING W66-WSY7066C
                          INTERN-MESSAGE
     IF  W66-RCODE NOT = ZERO
         MOVE 97 TO W-AC
     END-IF
     .
 M140-99.
     EXIT.

******************************************************************
* Aufruf Modul WSYS930
******************************************************************
 M150-CALL-WSYS930 SECTION.
 M150-00.
     MOVE ZERO TO ROUT-CC
     CALL "WSYS930" USING ROUT-SATZ
     IF  ROUT-NOT-OK
         IF  ROUT-CMD-ID
             MOVE 02 TO W-AC
             MOVE 1201 TO ERROR-NR OF GEN-ERROR
             STRING "WSYS930@nin KANR2ID: "
                    ROUT-MERKMAL
                    "@"
                         delimited by size
               INTO DATEN-BUFFER1
             END-STRING
             PERFORM Z002-PROGERR
             EXIT SECTION
         END-IF
         IF  ROUT-CMD-AS
             EVALUATE TRUE
*G.02.16         wenn AS-Sperre - trotzdem ASYNC70-Satz erzeugen
                 WHEN ROUT-ASERR CONTINUE
                 WHEN OTHER      MOVE 91 TO W-AC
                                 MOVE 1201 TO ERROR-NR OF GEN-ERROR
                                 MOVE ROUT-KZ TO D-NUM20
                                 MOVE ROUT-CARDID TO D-NUM21
                                 STRING "WSYS930@kein TUEAS: "
                                        D-NUM20 "/" D-NUM21 "@"
                                             delimited by size
                                   INTO DATEN-BUFFER1
                                 END-STRING
                                 PERFORM Z002-PROGERR
             END-EVALUATE
             EXIT SECTION
         END-IF
     END-IF
     .
 M150-99.
     EXIT.

******************************************************************
* Aufruf Modul WSYS971
******************************************************************
 M160-CALL-WSYS971 SECTION.
 M160-00.
     CALL "WSYS971" USING CHK-WSYS971C

*G.02.29 - Anfang
**  ---> nun das Ergebnis überprüfen
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
*                        SET ENDE TO TRUE
**
                         MOVE 30 TO W-AC
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
*                        SET ENDE TO TRUE
*
                         MOVE 30 TO W-AC

**      ---> BMP nicht numerisch  ==> AC 30
         WHEN 1000 THRU 1999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR of GEN-ERROR
                         STRING "BMP nicht num./Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 30 TO W-AC

**      ---> BMP fehlt  ==> AC 30
         WHEN 4000 THRU 4999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR of GEN-ERROR
                         STRING "BMP fehlt/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 30 TO W-AC

**      ---> BMP zuviel  ==> AC 30
         WHEN 5000 THRU 5999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR of GEN-ERROR
                         STRING "BMP zuviel/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 30 TO W-AC

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
*                        SET ENDE TO TRUE
**
                         MOVE 30 TO W-AC
     END-EVALUATE

*G.02.28 - Ende

**  ---> Fehlermeldung vervollständigen, wird nun in Z002-PROGERR gemacht
     IF  W-AC > ZERO or ENDE
*         STRING  "WEAT-Term-Nr./Trace-Nr.: "
*                 IMSG-CF (IMSG-TPTR(41):IMSG-TLEN(41))
*                 "/"
*                 IMSG-CF (IMSG-TPTR(11):IMSG-TLEN(11))
*                     delimited by size
*           INTO  DATEN-BUFFER3
*         END-STRING
         PERFORM Z002-PROGERR
     END-IF
     .
 M160-99.
     EXIT.

******************************************************************
* Aufruf Modul SYSAWKZ
******************************************************************
 M170-CALL-SYSAWKZ SECTION.
 M170-00.
     CALL "SYSAWKZ" USING WKZ-SYSWKZ0C
     IF  WKZ-ERR
         MOVE 30   TO W-AC
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         MOVE WKZ-RCODE TO D-NUM4
         STRING  "SYSWKZ0"
                 "@"
                 "RCODE:"
                 "@"
                 D-NUM4
                 "@"
                     DELIMITED BY SIZE
           INTO DATEN-BUFFER1
         END-STRING
         EVALUATE WKZ-RCODE
             WHEN 2      MOVE "WKZ nicht zugelassen (=WKZKURS)"   TO DATEN-BUFFER2
                         MOVE WKZ-WKZ TO D-NUM4
                         MOVE "Ausgangswährung WKZ ="             TO DATEN-BUFFER3
                         MOVE D-NUM4                              TO DATEN-BUFFER3 (22:)

             WHEN OTHER  continue
         END-EVALUATE
         PERFORM Z002-PROGERR
     END-IF
     .
 M170-99.
     EXIT.

*G.02.00 - Anfang
******************************************************************
* Aufruf Modul IUMSW07
******************************************************************
 M180-CALL-IUMSW07 SECTION.
 M180-00.
     CALL "IUMSW07" USING WUMS-WUMSO07C
     IF  WUMS-ERR
*G.02.50 - bei fehlendem Umsatzrecord kein Ende
*         IF  WUMS-RCODE = 100 and ISONTYP of TXILOG70 > 400
         IF  WUMS-RCODE = 100
**          ---> bei Wiederholungsstorno kann der Umsatzeintrag
**          ---> schon abgeräumt sein, also weiter machen KEIN ENDE
             EXIT SECTION
         END-IF
*G.02.50 - Ende
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
*G.02.33 - Fuer Select Statement Daten wieder moven
     ELSE
        MOVE WUMS-UMSATZ TO UMSWEAT
*G.02.33 - Ende 
     END-IF
     .
 M180-99.
     EXIT.

*G.02.00 - Anfang

******************************************************************
* Aufruf Artikel-Mapper
******************************************************************
 P100-PATHSEND SECTION.
 P100-00.
**  ---> Nachrichtenparameter (Server, Länge, Timeout)
     MOVE W-ARTMAP TO PS-SRVCLASS
     MOVE 2048     TO PS-REQLEN
     MOVE 1000     TO PS-TIMEOUT

**  ---> Übrige Parameter
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
******************************************************************
 P910-WTUNHEX SECTION.
 P910-00.
     ENTER TAL "WT^UNHEX" USING P-HEX8 P-HEX16
     .
 P910-99.
     EXIT.

******************************************************************
* feststellen belegte Länge eines Strings
*        Eingabe:    W-BUFFER
*                    W-BUFFER-LEN
*        Ausgabe:    W-BUFFER-AKT
******************************************************************
 P920-STRING-LEN SECTION.
 P920-00.
     MOVE ZERO TO W-BUFFER-AKT
     ENTER TAL "String^Laenge" USING  W-BUFFER
                                      W-BUFFER-LEN
                               GIVING W-BUFFER-AKT
     .
 P920-99.
     EXIT.

******************************************************************
* Aufruf COBOL-Utility: GETPARAMTEXT
*
*              Eingabe: stup-portion (parametername)
*              Ausgabe: stup-result  (-1:NOK, >=0:OK länge von text)
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
**                  ---> Länge     ist vorhanden in STUP-RESULT

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
         MOVE "Programm-Abbruch (PFCFAD7S)"   TO DATEN-BUFFER2
         PERFORM Z002-PROGERR
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     ENTER TAL "WT^ANCNAME" USING FEHL
                                  ANCNAME
                                  PAIRINFO
     IF  FEHL not = ZERO
         MOVE "Ancestor Pathway nicht ermittelbar" TO DATEN-BUFFER1
         MOVE "Programm-Abbruch (PFCFAD7S)"        TO DATEN-BUFFER2
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
*** => weitere Verarbeitung hier einfügen
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
* Select auf Tabelle MDNR2AS
******************************************************************
 S110-SELECT-MDNR2AS SECTION.
 S110-00.
     EXEC SQL
         SELECT   ROUTKZ, CARDID, MDNR, AIID, TRACENR, DATUM
           INTO   :ROUTKZ of MDNR2AS
                 ,:CARDID of MDNR2AS
                 ,:MDNR of MDNR2AS
                 ,:AIID of MDNR2AS
                 ,:TRACENR of MDNR2AS
                 ,:DATUM of MDNR2AS
                     TYPE AS DATETIME YEAR TO DAY
           FROM  =MDNR2AS
          WHERE   ROUTKZ = :ROUTKZ of MDNR2AS
            AND  (CARDID = :CARDID of MDNR2AS or CARDID = 0)
            AND  (MDNR   = :MDNR   of MDNR2AS or MDNR = 0)
            FOR  REPEATABLE ACCESS
             IN  EXCLUSIVE MODE
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET MDNR2AS-OK TO TRUE
         WHEN OTHER  MOVE 91   TO W-AC
                     MOVE 2001 TO ERROR-NR of GEN-ERROR
                     MOVE CARDID of MDNR2AS TO D-NUM20
                     MOVE ROUTKZ of MDNR2AS TO D-NUM21
                     STRING "MDNR2AS@"
                            MDNR   OF MDNR2AS "/"
                            D-NUM20 "/" D-NUM21 "@"
                                 delimited by size
                       INTO DATEN-BUFFER1
                     END-STRING
                     PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S110-99.
     EXIT.

******************************************************************
* Update auf MDNR2AS
******************************************************************
 S120-UPDATE-MDNR2AS  SECTION.
 S120-00.
     EXEC SQL
         UPDATE  =MDNR2AS
            SET   TRACENR = :TRACENR of MDNR2AS
                 ,DATUM   = CURRENT YEAR TO DAY
          WHERE  MDNR, CARDID, ROUTKZ
                 =  :MDNR   of MDNR2AS
                   ,:CARDID of MDNR2AS
                   ,:ROUTKZ of MDNR2AS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET MDNR2AS-OK  TO TRUE
         WHEN OTHER  SET MDNR2AS-NOK TO TRUE
                     MOVE 91   TO W-AC
                     MOVE 2003 TO ERROR-NR of GEN-ERROR
                     MOVE CARDID of MDNR2AS TO D-NUM20
                     MOVE ROUTKZ of MDNR2AS TO D-NUM21
                     STRING "MDNR2AS@"
                            MDNR   of MDNR2AS "/"
                            D-NUM20 "/" D-NUM21 "@"
                                 delimited by size
                       INTO DATEN-BUFFER1
                     END-STRING
                     PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S120-99.
     EXIT.

******************************************************************
* Select auf Tabelle TSKART40
******************************************************************
 S150-SELECT-TSKART40 SECTION.
 S150-00.
     EXEC SQL
         SELECT    VUNR, ROUTKZ
           INTO   :VUNR of TSKART40
                 ,:ROUTKZ of TSKART40
           FROM  =TSKART40
          WHERE  MDNR, TSNR, CARDSYS, CARDID
                 =  :MDNR    of TSKART40
                   ,:TSNR    of TSKART40
                   ,:CARDSYS of TSKART40
                   ,:CARDID  of TSKART40
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET TSKART40-OK  TO TRUE
         WHEN OTHER      SET TSKART40-NOK TO TRUE
                         MOVE 2001 TO ERROR-NR of GEN-ERROR
                         STRING "TSKART40@"
                                "CSYS/CID: "
                                CARDSYS of TSKART40 "/"
                                CARDID  of TSKART40 "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 04  TO W-AC
                         MOVE "*" TO IMSG-TRACETERMID
                         PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S150-99.
     EXIT.

******************************************************************
* Select auf Tabelle STATION
******************************************************************
 S160-SELECT-STATION SECTION.
 S160-00.
     EXEC SQL
         SELECT   ORT, NAME
           INTO   :ORT of STATION
                 ,:NAME of STATION
           FROM  =STATION
          WHERE  MDNR, TSNR
                 =    :MDNR of STATION
                     ,:TSNR of STATION
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET STATION-OK  TO TRUE
         WHEN OTHER  SET ENDE        TO TRUE
                     SET STATION-NOK TO TRUE
                     MOVE 2001 TO ERROR-NR of GEN-ERROR
                     STRING  "STATION@"
                             "MDNR/TSNR: "
                             MDNR of STATION "/"
                             TSNR of STATION "@"
                                 delimited by size
                       INTO  DATEN-BUFFER1
                     END-STRING
                     STRING  "TERMNR/TRACENR: "
                             W-TERMNR
                             W-TRACENR
                                 delimited by size
                       INTO  DATEN-BUFFER2
                     END-STRING
                     MOVE "Transaktion wird nicht beantwortet" TO DATEN-BUFFER3
                     MOVE "*" TO IMSG-TRACETERMID
                     PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S160-99.
     EXIT.

******************************************************************
* Insert auf Tabelle ASYNC70
******************************************************************
 S170-ASYNC70-INSERT SECTION.
 S170-00.

    EXEC SQL
        INSERT
          INTO  =ASYNC70
                ( PNR,       TERMNR,  TRACENR,  ISONTYP, MDNR,
                  TSNR,      VKZ,     CARDID,   ROUTKZ,  TRACENR_AS,
                  KZ_BEARB,  ANZ_REP, KEY_NAME, ZPINS,   ZPUPD,
                  FREHEADER, ANFRAGE, ANTWORT
                )
        VALUES  (
                 :PNR            OF ASYNC70
                ,:TERMNR         OF ASYNC70
                ,:TRACENR        OF ASYNC70
                ,:ISONTYP        OF ASYNC70
                ,:MDNR           OF ASYNC70
                ,:TSNR           OF ASYNC70
                ,:VKZ            OF ASYNC70
                ,:CARDID         OF ASYNC70
                ,:ROUTKZ         OF ASYNC70
                ,:TRACENR-AS     OF ASYNC70
                ,:KZ-BEARB       OF ASYNC70
                ,:ANZ-REP        OF ASYNC70
                ,:KEY-NAME       OF ASYNC70
                ,current  YEAR TO FRACTION(2)
                ,current  YEAR TO FRACTION(2)
                ,:FREHEADER      OF ASYNC70
                ,:ANFRAGE        OF ASYNC70
                ,:ANTWORT        OF ASYNC70
                  )
    END-EXEC
    EVALUATE SQLCODE OF SQLCA
        WHEN ZERO  SET ASYNC70-OK  TO TRUE
        WHEN OTHER SET ASYNC70-NOK TO TRUE
                   SET ENDE TO TRUE
                   MOVE 2005 TO ERROR-NR OF GEN-ERROR
                   STRING  "ASYNC70@"
                           PNR     of ASYNC70 "/"
                           TERMNR  of ASYNC70 "/"
                           TRACENR of ASYNC70 "/"
                           ISONTYP of ASYNC70
                   DELIMITED BY SIZE
                   INTO DATEN-BUFFER1
                   END-STRING
                   PERFORM Z002-PROGERR
    END-EVALUATE
     .
 S170-99.
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
                 , VUNR, ZP_VERKAUF, ZP_TIN
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
*                 ,:H-ZP-IN
                 ,:ZP-TIN of TXILOG70
                     TYPE AS DATETIME YEAR TO FRACTION(2)
*                 ,:H-ZP-OUT
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

*G.02.05 - Anfang

******************************************************************
* Update: TXILOG70-AUT.TRACENR-S
******************************************************************
 S185-UPDATE-TXILOG70-AUT  SECTION.
 S185-00.
     EXEC SQL
         UPDATE  =TXILOG70
            SET  TRACENR_S = :TRACENR-S OF TXILOG70-AUT
          WHERE  PNR       = :PNR       of TXILOG70-AUT
            AND  TERMNR    = :TERMNR    of TXILOG70-AUT
            AND  TRACENR   = :TRACENR   of TXILOG70-AUT
            AND  ISONTYP   = :ISONTYP   of TXILOG70-AUT
     END-EXEC

     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET TXILOG70-OK-AUT  TO TRUE
         WHEN OTHER  SET TXILOG70-NOK-AUT TO TRUE
                     SET ENDE             TO TRUE
                     MOVE 2003 TO ERROR-NR OF GEN-ERROR
                     STRING  "TXILOG70-AUT@"
                             W-MDNR "/"
                             W-TSNR "/"
                             TERMNR of TXILOG70-AUT
                     delimited by size
                       INTO DATEN-BUFFER1
                     END-STRING
                     PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S185-99.
     EXIT.

*G.02.05 - Ende

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
* Select auf Tabelle TXILOG70 für zugehörige Autorisierung
******************************************************************
 S200-SELECT-TXILOG70-AUT SECTION.
 S200-00.
     EXEC SQL
         SELECT    PNR            ,
                   TERMNR         ,
                   TRACENR        ,
                   ISONTYP        ,
                   KANR           ,
*G.02.31 - Anfang
                   BETRAG_ART     ,
*G.02.31 - Ende
                   CARDID         ,
                   TRACENR_AS     ,
                   GENNR          ,
                   ABL_JJMM       ,
                   BETRAG_AUTOR   ,
                   ERFASSUNGS_ART ,
                   TRANS_ART      ,
                   ZP_VERKAUF     ,
                   AF_BMP07       ,
                   AC_AS          ,
                   AC_TERM
           INTO   :PNR            OF TXILOG70-AUT
                 ,:TERMNR         OF TXILOG70-AUT
                 ,:TRACENR        OF TXILOG70-AUT
                 ,:ISONTYP        OF TXILOG70-AUT
                 ,:KANR           OF TXILOG70-AUT
*G.02.31 - Anfang
                 ,:BETRAG-ART     OF TXILOG70-AUT
*G.02.31 - Ende
                 ,:CARDID         OF TXILOG70-AUT
                 ,:TRACENR-AS     OF TXILOG70-AUT
                 ,:GENNR          OF TXILOG70-AUT
                 ,:ABL-JJMM       OF TXILOG70-AUT
                 ,:BETRAG-AUTOR   OF TXILOG70-AUT
                 ,:ERFASSUNGS-ART OF TXILOG70-AUT
*G.02.22 - Trans-Art wird benoetigt
                 ,:TRANS-ART      OF TXILOG70-AUT
*G.02.22 - Ende
                 ,:ZP-VERKAUF     OF TXILOG70-AUT
                 ,:AF-BMP07       OF TXILOG70-AUT
                 ,:AC-AS          OF TXILOG70-AUT
                 ,:AC-TERM        OF TXILOG70-AUT
           FROM  =TXILOG70
          WHERE  PNR, TERMNR, TRACENR, ISONTYP
                 =    :PNR     OF TXILOG70-AUT
                     ,:TERMNR  OF TXILOG70-AUT
                     ,:TRACENR OF TXILOG70-AUT
                     ,:ISONTYP OF TXILOG70-AUT
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET TXILOG70-OK  TO TRUE
         WHEN 100        SET TXILOG70-NOK TO TRUE
                         MOVE 21 TO W-AC
         WHEN OTHER      SET PRG-ABBRUCH TO TRUE
     END-EVALUATE
     .
 S200-99.
     EXIT.

******************************************************************
* Select auf Tabelle TXNLOG70 für zugehörige Autorisierung
******************************************************************
 S210-SELECT-TXNLOG70-ATS SECTION.
 S210-00.
     EXEC SQL
         SELECT    PNR, TERMNR, TRACENR, ISONTYP, KZ_MSG, ISO_VERF
                 , MDNR, TSNR, LOG_SRV, ZPINS, FREHEADER, ANFRAGE
                 , ANTWORT
           INTO   :PNR of TXNLOG70-ATS
                 ,:TERMNR of TXNLOG70-ATS
                 ,:TRACENR of TXNLOG70-ATS
                 ,:ISONTYP of TXNLOG70-ATS
                 ,:KZ-MSG of TXNLOG70-ATS
                 ,:ISO-VERF of TXNLOG70-ATS
                 ,:MDNR of TXNLOG70-ATS
                 ,:TSNR of TXNLOG70-ATS
                 ,:LOG-SRV of TXNLOG70-ATS
                 ,:ZPINS of TXNLOG70-ATS
                     TYPE AS DATETIME YEAR TO FRACTION(2)
                 ,:FREHEADER of TXNLOG70-ATS
                 ,:ANFRAGE of TXNLOG70-ATS
                 ,:ANTWORT of TXNLOG70-ATS
           FROM  =TXNLOG70
          WHERE  PNR, TERMNR, TRACENR, ISONTYP, KZ_MSG
                 =    :PNR of TXNLOG70-ATS
                     ,:TERMNR of TXNLOG70-ATS
                     ,:TRACENR of TXNLOG70-ATS
                     ,:ISONTYP of TXNLOG70-ATS
                     ,:KZ-MSG of TXNLOG70-ATS
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET TXNLOG70-OK  TO TRUE
         WHEN 100        SET TXNLOG70-NOK TO TRUE
         WHEN OTHER      SET PRG-ABBRUCH TO TRUE
     END-EVALUATE
     .
 S210-99.
     EXIT.

******************************************************************
* Select auf Tabelle TXILOG70 für zugehörige Autorisierung
*G.02.33 - neu
******************************************************************
 S300-SELECT-TXILOG70-WDH SECTION.
 S300-00.
     EXEC SQL
       SELECT  PNR            ,
               TERMNR         ,
               TRACENR        ,
               ISONTYP        ,
               AC_TERM
         INTO :PNR            of TXILOG70-WDH ,
              :TERMNR         of TXILOG70-WDH ,
              :TRACENR        of TXILOG70-WDH ,
              :ISONTYP        of TXILOG70-WDH ,
              :AC-TERM        of TXILOG70-WDH
           FROM  =TXILOG70
          WHERE  PNR, TERMNR, TRACENR, ISONTYP
                 =    :PNR     of TXILOG70-WDH
                     ,:TERMNR  of TXILOG70-WDH
                     ,:TRACENR of TXILOG70-WDH
                     ,:ISONTYP of TXILOG70-WDH
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET TXILOG70-OK   TO TRUE
                         SET WDH-VORHANDEN TO TRUE
         WHEN 100        SET TXILOG70-NOK  TO TRUE
                         SET WDH-FIRST     TO TRUE
         WHEN OTHER      SET PRG-ABBRUCH TO TRUE
     END-EVALUATE
     .
 S300-99.
     EXIT.

******************************************************************
* Select auf Tabelle TXNLOG70 für frueheren Teilsto
*G.02.33 - neu
******************************************************************
 S310-SELECT-TXNLOG70-WDH SECTION.
 S310-00.
     EXEC SQL
         SELECT    PNR, TERMNR, TRACENR, ISONTYP, KZ_MSG, ISO_VERF
                 , FREHEADER, ANTWORT
           INTO   :PNR of TXNLOG70-WDH
                 ,:TERMNR of TXNLOG70-WDH
                 ,:TRACENR of TXNLOG70-WDH
                 ,:ISONTYP of TXNLOG70-WDH
                 ,:KZ-MSG of TXNLOG70-WDH
                 ,:ISO-VERF of TXNLOG70-WDH
                 ,:FREHEADER of TXNLOG70-WDH
                 ,:ANTWORT of TXNLOG70-WDH
           FROM  =TXNLOG70
          WHERE  PNR, TERMNR, TRACENR, ISONTYP, KZ_MSG
                 =    :PNR of TXNLOG70-WDH
                     ,:TERMNR of TXNLOG70-WDH
                     ,:TRACENR of TXNLOG70-WDH
                     ,:ISONTYP of TXNLOG70-WDH
                     ,:KZ-MSG of TXNLOG70-WDH
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET TXNLOG70-OK  TO TRUE
         WHEN 100        SET TXNLOG70-NOK TO TRUE
         WHEN OTHER      SET PRG-ABBRUCH TO TRUE
     END-EVALUATE
     .
 S310-99.
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

*kl20180405 - G.02.43 - wg. Cardid ZERO zurueck zur klassischen
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
*     übernehmen kompletten 1. Key
     MOVE    S-SEARCH-KEY       TO S2-SEARCH-KEY
*     CARDID ersezten (ggf. auch für andere Werte möglich)
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
*kl20180405 - G.02.43 - Ende
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
                                delimited by
                                 ALL SPACE or "@"
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

*    C4-ANZ: Anzahl belegter Teilstrings

     IF  C4-ANZ = ZERO
**      ---> kein Inhalt
         EXIT SECTION
     END-IF

     MOVE SPACES TO W-BUFFER
     MOVE 1 TO C4-I2
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   C4-I1 > C4-ANZ

         COMPUTE C4-LEN = W-COUNT(C4-I1) - 1

         EVALUATE W-TEILSTRING (C4-I1) (1:1)

             WHEN "A"    MOVE W-TEILSTRING (C4-I1) (2:C4-LEN) TO D-NUM4
                         MOVE D-NUM4 TO C4-I3
**                   ---> hier muss ggf. ein anderer Buffer gewählt werden
                         MOVE IMSG-CF(IMSG-TPTR(C4-I3):IMSG-TLEN(C4-I3))
                             TO W-BUFFER (C4-I2:IMSG-TLEN(C4-I3))
                         COMPUTE C4-I2 = C4-I2 + IMSG-TLEN(C4-I3)

             WHEN "F"    MOVE W-TEILSTRING(C4-I1) (2:C4-LEN)
                             TO W-BUFFER (C4-I2:C4-LEN)
                         ADD C4-LEN TO C4-I2

             WHEN "H"    MOVE W-TEILSTRING(C4-I1) (2:C4-LEN)
                             TO W-BUFFER (C4-I2:C4-LEN)
                         MOVE W-BUFFER (C4-I2:C4-LEN) TO WTHEXS-SRC
                         MOVE C4-LEN                  TO WTHEXS-SRC-LEN
                         PERFORM V400-WT-HEX-STRING
                         MOVE WTHEXS-DST (1:WTHEXS-DST-LEN) TO W-BUFFER (C4-I2:)
                         ADD WTHEXS-DST-LEN TO C4-I2

             WHEN "T"    MOVE W-TEILSTRING(C4-I1) (2:C4-LEN) TO D-NUM4
                         MOVE D-NUM4 TO C4-I3
**                   ---> hier muss ggf. ein anderer Buffer gewählt werden
                         MOVE IMSG-CF(IMSG-TPTR(C4-I3):IMSG-TLEN(C4-I3))
                             TO W-BUFFER (C4-I2:IMSG-TLEN(C4-I3))
                         COMPUTE C4-I2 = C4-I2 + IMSG-TLEN(C4-I3)

             WHEN space  continue

             WHEN OTHER  SET ENDE TO TRUE
                         MOVE W-ROUTKZ TO D-NUM4
                         STRING  "Unbekannte Verarbeitungsregeln "
                                 "für Rout-KZ = "
                                 D-NUM4
                                     delimited by size
                           INTO  DATEN-BUFFER1
                         END-STRING
                         PERFORM Z002-PROGERR
                         EXIT SECTION

         END-EVALUATE

     END-PERFORM

     COMPUTE W-BUFFER-LEN = C4-I2 - 1
     .
 U400-99.
     EXIT.

******************************************************************
* hexen String
*                        Eingabe:    WTHEXS-SRC
*                                    WTHEXS-SRC-LEN
*                        Ausgabe:    WTHEXS-DST
*                                    WTHEXS-DST-LEN
******************************************************************
 V400-WT-HEX-STRING SECTION.
 V400-00.
     MOVE SPACES TO WTHEXS-DST
     ENTER TAL "WT^HEX^STRING" USING WTHEXS-SRC WTHEXS-SRC-LEN
                                     WTHEXS-DST WTHEXS-DST-LEN
     .
 V400-99.
     EXIT.




******************************************************************
* SQL-Fehlerbehandlung
******************************************************************
 Z001-SQLERROR SECTION.
 Z001-00.

**  ---> holen Daten für Fehlertabelle
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
**  ---> Angaben für Terminal- und Trace-Nummer in BUFFER5 einstellen
     STRING  "WEAT-Term-Nr./Trace-Nr.: "
             W-TERMNR "/" W-TRACENR
                 delimited by size
       INTO  DATEN-BUFFER5
     END-STRING

**  ---> holen Daten für Fehlertabelle
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
