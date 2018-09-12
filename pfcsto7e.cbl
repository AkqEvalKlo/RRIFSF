?CONSULT $SYSTEM.SYSTEM.COBOLEX0
?SEARCH  $SYSTEM.SYSTEM.COBOLLIB
?SEARCH  =TALLIB
?SEARCH  =ASC2EBC
?SEARCH  =EBC2ASC
?SEARCH  =IUMSW07
?SEARCH  =SDBCDU5
?SEARCH  =SYSAWKZ
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

*G.00.19 - Anfang
?SEARCH  =WCSI060
?SEARCH  =WEUR055
?SEARCH  =WEUR056
*G.00.19 - Ende

*G.01.07 - Modul für Zusammenstellung ZP-VERKAUF
?SEARCH =ZPVERK
*G.01.07 - ende


?NOLMAP, SYMBOLS, INSPECT
?SAVE ALL
?SAVEABEND
?LINES 66
?CHECK 3
?SQL


 IDENTIFICATION DIVISION.

 PROGRAM-ID. PFCSTO7 .

 DATE-COMPILED.


****************************************************************
* Letzte Aenderung :: 2018-09-11
* Letzte Version   :: G.02.01
* Kurzbeschreibung :: Dieses Programm setzt Flottenkarten-
* Kurzbeschreibung :: Stornierungsanfragen vom Terminal-Protokoll
* Kurzbeschreibung :: auf AS-IFSF-Protokoll um. Bearbeitet werden
* Kurzbeschreibung :: nur Terminalnachrichten von Typ 400, die
* Kurzbeschreibung :: auf AS-Nachrichten vom Typ 1420 umgesetzt
* Kurzbeschreibung :: werden.
* Package          :: ICC
* Auftrag          :: R7-376
*
* Aenderungen:
*
*----------------------------------------------------------------*
* Vers. | Datum    | von | Kommentar                             *
*-------|----------|-----|---------------------------------------*
*G.02.01|2018-09-11| kus | R7-376:
*       |          |     | - Umstellung von festem ROUTKZ auf AS-Verf
*-------|----------|-----|-------------------------------------------*
*G.01.12|2018-08-03| kus | R7-365/DKVCHIP-8:
*       |          |     | - neues KZ-VERF fuer Chip 
*-------|----------|-----|---------------------------------------*
*G.01.11|2018-05-28| kl  | F1ICC-114:
*       |          |     | - Branchänderung KUS vom 25.05.2018
*       |          |     |   (BMP56) nachgezogen
*-------|----------|-----|---------------------------------------*
*G.01.10|2018-05-25| kl  | RRIFSF-4:
*       |          |     | - Neues FK-AS: Road Runner (RK=25)
*       |          |     |   (Vereinbarung: Identisch WEAT AS 2)
*-------|----------|-----|----------------------------------------*
*G.01.09|20180518  | kl  | Neukompilierung wg. Korrektur ZPVERKM
*       |          |     | (ZP-VERKAUF Modul)
*-------|----------|-----|---------------------------------------*
*G.01.08|2018-04-30| kus | F1ICC-105:
*       |          |     | - AS BMP 12 fuellen mit ZP vom Terminal
*       |          |     |   dafuer ZP-VERKAUF verwenden (ist Kombi
*       |          |     |   aus Terminal BMP 12/13)
*-------|----------|-----|---------------------------------------*
*G.01.07|2018-04-11| SK  | Jira R7-306: Modulaufruf f. zp_verkauf
*       |          |     | Berechnung
*-------|----------|-----|-------------------------------------------*
*G.01.06|2018-04-05| kl  | R7-272:
*       |          |     | Optimierung Zugriff / Laden FCPARAM
*       |          |     | (CARDID = 0 oder X - Vorrang bei X)
*-------|----------|-----|---------------------------------------*
*G.01.05|2018-03-16| kl  | Bei Chip-Transaktionen Kartenart = 211
*       |          |     | setzen; BASIS: G.01.03
*       |          |     | (DKVCHIP-21)
*-------|----------|-----|---------------------------------------*
*G.01.04|2018-03-16| kl  | VERWORFEN: Falsche Implementierung
*       |          |     |            R7-269
*-------|----------|-----|--- -----------------------------------*
*G.01.03|2018-02-01| hkn | POST70 entfernt, da nicht benutzt     *
*-------|----------|-----|--- -----------------------------------*
*G.01.02|2018-01-22| hkn | IQ-5 Neu: Stiglechner mit Routkz = 24 *
*-------|----------|-----|--- -----------------------------------*
*G.01.01|2018-01-09| kl  | Initialierung T-MAX in B000 optimiert *
*-------|----------|-----|---------------------------------------*
*G.01.00|2018-01-05| kl  | - Speichertabelle fuer FCPARAM        *
*       |          |     |   vergroessert                        *
*       |          |     | - Versionierung auf G.01.00, da in    *
*       |          |     |   mehreren Iterationen produktiv      *
*-------|----------|-----|---------------------------------------*
*-------|----------|-----|---------------------------------------*
*G.00.38|2018-01-04| hkn | R7-Version aus X-Version kopiert
*-------|----------|-----|---------------------------------------*
*G.00.37|2017-09-11| hkn | Erkennung INDOOR/OUTDOOR: Mit
*       |          |     | TXILOG70.Betrag_Art = "M" zusätzlich
*       |          |     | zur Prüfung auf BMP 25
*-------|----------|-----|---------------------------------------*
*G.00.36|2017-06-28| hkn | Stornierung auf nicht existierende
*       |          |     | Autorisierung: Umsatzfehler in VERRLOG
*       |          |     | schreiben und Antwort an das TS
*-------|----------|-----|---------------------------------------*
*G.00.35|2017-06-13| kus | BMP 56 fuer AS angepasst, sodass auch
*       |          |     | Vorautorisierungen (1100) storniert
*       |          |     | werden können
*-------|----------|-----|---------------------------------------*
*G.00.35|2017-06-07| kus | Eurowag: BMP 41 senden
*-------|----------|-----|---------------------------------------*
*G.00.34|2017-06-02| das | Verkürztes BMP59 erst mam nur shell
*-------|----------|-----|---------------------------------------*
*G.00.33|2017-05-03| kus | DKV-Chip: Nachrichten koennen jetzt
*       |          |     | Chipdaten (BMP 55) enthalten
*-------|----------|-----|---------------------------------------*
*G.00.32|2017-02-02| hkn | FCPARM ersetzt durch FCPARAM
*-------|----------|-----|---------------------------------------*
*G.00.31|2017-01-11| HJO | Erweiterungen für Nachbucher NABUO70S
*       |          |     | eingeführt - Auch Antwort 00 bei AS-
*       |          |     | Sperre ans Terminal
*-------|----------|-----|---------------------------------------*
*G.00.30|2016-12-02| hkn | TND: Verwendung E310-BMP48-DEFAULT neu
*-------|----------|-----|---------------------------------------*
*G.00.29|2016-12-01| hkn | BMP38: In 1420 an div. AS-Systeme
*-------|----------|-----|---------------------------------------*
*G.00.28|2016-11-25| hkn | Nachricht 410:
*       |          |     | Mit  BMP59 -
*       |          |     | Online, wenn AC = 0
*       |          |     | Ohne BMP59 -
*       |          |     | Offline, wenn zugeh. Autorisierung
*       |          |     | mit Genehmigung (telefonisch)
*-------|----------|-----|---------------------------------------*
*G.00.27|2016-11-17| hkn | AS-Nachricht: 1200/1220 abhängig von
*       |          |     | Auto Storno oder Manueller Storno
*-------|----------|-----|---------------------------------------*
*G.00.26|2016-11-10| hkn | Total: Eurotrafic-Karte - Cardid = 19
*       |          |     | AS-Anfrage (1420) mit BMP 53/64
*-------|----------|-----|---------------------------------------*
*G.00.25|2016-11-08| hkn | DUKPT: nur übesetzt wg. Änderung
*-------|----------|-----|---------------------------------------*
*G.00.24|2016-11-08| hkn | DUKPT: nur übesetzt wg. Änderung
*-------|----------|-----|---------------------------------------*
*G.00.23|2016-11-08| hkn | DUKPT: nur übesetzt wg. Änderung
*-------|----------|-----|---------------------------------------*
*G.00.22|2016-11-07| hkn | EUROWAG: Sicherheitsverfahren auch
*       |          |     | wenn kein PAC in TS-Anfrage vorhanden
*-------|----------|-----|---------------------------------------*
*G.00.21|2016-11-07| hkn | DUKPT: nur übesetzt wg. Änderung
*-------|----------|-----|---------------------------------------*
*G.00.20|2016-11-04| hkn | DUKPT: nur übesetzt wg. Änderung
*-------|----------|-----|---------------------------------------*
*G.00.19|2016-10-12| hkn | EUROWAG: PAC Umschlüssen und Bilden
*       |          |     | mit DUKPT-Verfahren
*-------|----------|-----|---------------------------------------*
*G.00.18|2016-09-21| hkn | Neu: LogPay mit Routkz = 23
*-------|----------|-----|---------------------------------------*
*G.06.17|2016-09-08| hkn | Bei FEP-Autorisierung nur Eigenantwort
*       |          |     |(AS-Anfrage entfällt)
*       |          |     |
*       |          |     | Neue Serverklasse: PFCOFF7S o. Routkz
*       |          |     | da  kein AS-Routing
*       |          |     |
*       |          |     | Bei FEP-Verarbeitung ROUTKZ = 00
*       |          |     |
*       |          |     | D310-TOTAL: Keine spezielle
*       |          |     | Bearbeitung BMP 42 - VUNR
*       |          |     |
*-------|----------|-----|---------------------------------------*
*G.00.16|2016-09-15| kl  | KZ-VERF bei Offline auf "k" setzen
*       |          |     | wg. Update auf =UMSWEAT
*-------|----------|-----|---------------------------------------*
*G.00.15|2016-08-17| hkn | D314-BP: Macbildung - W66-DEFAULT
*-------|----------|-----|---------------------------------------*
*G.00.14|2016-08-10| hkn | Neu: Eurowag mit Routkz = 22
*-------|----------|-----|---------------------------------------*
*G.00.13|2016-07-28| hkn | D314-BP - BMP42: Positionierung
*       |          |     | korrigiert
*-------|----------|-----|---------------------------------------*
*       |          |     | G.00.12 korrigiert G.00.08
*G.00.12|2016-07-13| hkn | Unterscheidung Offline Buchung und
*       |          |     | Stornierung
*       |          |     | Steuerung BMP42 in Nachricht 410
*       |          |     | nicht, wenn Offline-Buchung
*-------|----------|-----|---------------------------------------*
*G.00.11|2016-04-07| kl  | = G.00.10 / Dummy wg. Sourcsafe-Crash
*-------|----------|-----|---------------------------------------*
*G.00.10|2016-06-24| hkn |TXILOG70.TRACENR_S der Autorisierung
*       |          |     |aus TXILOG70.TRACENR der Stornierung
*-------|----------|-----|---------------------------------------*
*G.00.09|2016-06-14| hkn |TXILOG70.BETRAG-AUTOR gleich
*       |          |     |angefragter Betrag
*-------|----------|-----|---------------------------------------*
*G.00.08|2016-04-27| hkn | Nachricht 410: Ohne BMP 42
*-------|----------|-----|---------------------------------------*
*G.00.07|2016-03-15| hkn | Wiedereinbau BMP 22: ORLEN, Routkz - 16
*       |          |     | benötigt das BMP 22 - Eingabeart
*-------|----------|-----|---------------------------------------*
*G.00.06|2016-03-04| hkn | Ausbau BMP 22 Eingabeart: Wird zur Zeit
*       |          |     | nicht an AS geschickt und auch nicht
*       |          |     | in der Eigenantwort verwendet
*-------|----------|-----|---------------------------------------*
*G.00.05|2016-02-26| hkn | G090-PUT-ASNYC70:
*       |          |     | ASYNC70.ISONTYP     = 400
*-------|----------|-----|---------------------------------------*
*G.00.04|2016-02-25| hkn | G090-PUT-ASNYC70:
*       |          |     | ASYNC70.ISONTYP     = 1420
*       |          |     | ASYNC70.ANFRAGE.VAL = IMSG-NDATEN
*       |          |     | ASYNC70.ANFRAGE.LEN = IMSG-SENDLEN
*-------|----------|-----|---------------------------------------*
*G.00.03|2016-02-15| hkn | C100-ANFRAGE-CHECK:
*       |          |     | Setzen UMS-ZAHLUNG zusätzlich für
*       |          |     | Abwicklungs-Kenz. = 77
*       |          |     | Auto-Storno: Abwknz:77/78, dann AC = 30
*       |          |     | und Eigenantwort
*-------|----------|-----|---------------------------------------*
*G.00.02|2015-12-04| hkn | a) Verarbeitung UMSWEAT eingefügt
*       |          |     | b) VKZ = FK  Steuerg. Memlog-Selektion
*-------|----------|-----|---------------------------------------*
*G.00.01|2015-11-15| hkn | a) TRX-Anfrage in ASYNC70 eintragen
*       |          |     | b) Eigenantwort an TS
*       |          |     | c) Keine Nachricht an AS
*       |          |     |
*-------|----------|-----|---------------------------------------*
*G.00.00|2015-05-15| BAH | Neuerstellung
*----------------------------------------------------------------*
*
* Programmbeschreibung
* --------------------
*
* Das Programm setzt Flottenkarten-Terminal-Stornierungs-Anfragen
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
* definiert werden (z.B. PFCSTO7S-05 für Avia). Die Keys für das
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
*    -   ANZREPA     Anzahl Versuche der automatischen Stornierung,
*                    wird benötigt in ASYNC70.ANZ-REP
*    -   ANZREPM     Anzahl Versuche der manuellen Stornierung,
*                    wird benötigt in ASYNC70.ANZ-REP
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
* C500-LOGGING
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
* D326-STIGLECHNER
* D900-ROUTING-ETC
* D318-TND
* D900-ROUTING-ETC
* D910-GET-ASTRACENR
*
* E100-FEP-ANTWORT
* E305-01-ARTIKELDATEN
* E310-BMP48-DEFAULT
* E900-PUT-ERRLOG
*
* F100-LOGDATEN-EIGEN
* G090-PUT-ASNYC70
* F920-MAC-BILDEN

*G.00.19 - Anfang
* F950-ASMAC-DUKPT
*G.00.19 - Ende
*
* G100-PUT-TXILOG70

*G.00.10 - Anfang
* G102-PUT-TXILOG70-AUT
*G.00.10 - Ende

* G110-PUT-TXNLOG70-TS
* G130-PUT-CRDUSEDN
* G130-PUT-UMSWEAT
*
* L100-ADD-BMP
* L110-COB2ISO
*
* M100-CALL-SDBCDU5
* M120-CALL-WCAPM92
* M130-CALL-WISO207
* M140-CALL-WSY7066
* M150-CALL-WSYS930
* M160-CALL-WSYS971
* M170-CALL-SYSAWKZ
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
* S110-SELECT-MDNR2AS
* S120-UPDATE-MDNR2AS
* S150-SELECT-TSKART40
* S160-SELECT-STATION
* S170-ASYNC70-INSERT
* S180-INSERT-TXILOG70

*G.00.10 - Anfang
* S185-UPDATE-TXILOG70-AUT
*G.00.10 - Ende

* S190-INSERT-TXNLOG70-TS
* S200-SELECT-TXILOG70-AUT
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
     05      K-MODUL             PIC X(08)          VALUE "PFCSTO7S".

**          ---> Pflichtfelder einer 1420-AS-Nachricht
     05      K-BYTEMAP-A1420      PIC X(64) VALUE
*G.00.29 - Anfang
*    "0111001000110100000000011000000100000000010000011000000000100000".
**
     "0111001000110100000000011000000100000100010000011000000000100000".
*G.00.29 - Anfang
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

     05      ASYNC70-FLAG        PIC 9     VALUE ZERO.
          88 ASYNC70-OK                    VALUE ZERO.
          88 ASYNC70-NOK                   VALUE 1.

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

*G.00.10 - Anfang
     05      TXILOG70-FLAG-AUT   PIC 9       VALUE ZERO.
          88 TXILOG70-OK-AUT                 VALUE ZERO.
          88 TXILOG70-NOK-AUT                VALUE 1.
*G.00.10 - Ende

     05      TXILOG70-FLAG       PIC 9       VALUE ZERO.
          88 TXILOG70-OK                     VALUE ZERO.
          88 TXILOG70-NOK                    VALUE 1.

     05      TXNLOG70-FLAG       PIC 9       VALUE ZERO.
          88 TXNLOG70-OK                     VALUE ZERO.
          88 TXNLOG70-NOK                    VALUE 1.

     05      UMS-FLAG            PIC X     VALUE SPACE.
          88 UMS-ZAHLUNG                   VALUE SPACE.
          88 UMS-GUTSCHRIFT                VALUE HIGH-VALUE.

*G.00.12 - Anfang

     05      BUCHUNGS-FLAG       PIC X     VALUE SPACE.
          88 OFFLINE-BUCHUNG               VALUE SPACE.
          88 ONLINE-BUCHUNG                VALUE HIGH-VALUE.

*G.00.12 - Ende

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

     05      STORNO-FLAG         PIC X     VALUE LOW-VALUE.
          88 STORNO-AUTO                   VALUE LOW-VALUE.
          88 STORNO-MANUELL                VALUE HIGH-VALUE.

     05      STORNO-WFLAG        PIC X     VALUE LOW-VALUE.
          88 STORNO-WERSTE                 VALUE LOW-VALUE.
          88 STORNO-WIEDERHOLUNG           VALUE HIGH-VALUE.

     05      FEP-ANTWORT-FLAG    PIC X     VALUE LOW-VALUE.
          88 NO-FEP-ANTWORT                VALUE LOW-VALUE.
          88 FEP-ANTWORT                   VALUE HIGH-VALUE.

*G.06.17 - Entscheidung, ob FEP oder AS-TX
     05      PRUEF-ORT           PIC 9     VALUE 1.
          88 PRF-AS                        VALUE 1.
          88 PRF-FEP                       VALUE 2.
*G.06.17 - Ende

*--------------------------------------------------------------------*
* weitere Arbeitsfelder
*--------------------------------------------------------------------*
**          ---> unverändert
 01          WORK-FELDER.
     05      W-ROUTKZ            PIC S9(04) COMP.
     05      W-KEYNAME           PIC  X(08).
     05      W-ISOGEN-VERS       PIC  X(02).
     05      W-ANZREPA           PIC S9(04) COMP.
     05      W-ANZREPM           PIC S9(04) COMP.

**          ---> werden bei jeder Tx initiert
 01          WORK-INIT.
     05      W-CARDID            PIC S9(04) COMP.
     05      W-KANR-LEN          PIC S9(04) COMP.
     05      W18-BETRAG          PIC S9(16)V99 COMP.
     05      W-ZP-VERKAUF        PIC S9(18) COMP.
*G.02.01 - AIID 
     05      W-AIID              PIC X(11).
*G.02.01 - Ende
     
     05      W-ACX.
      10     W-AC                PIC 9(02).
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
      10     W-BELEGNR           PIC 9(04).

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
     05      W-BMP55-LEN         PIC S9(04)    COMP VALUE ZEROS.
     05      W-BMP55             PIC X(512).
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
*G.02.01 - Refactoring VERF-ROUTKZ -> VERF-AS
* 01          VERF-ROUTKZ         PIC 9(02).
 01          VERF-AS             PIC 9(02).
*G.02.01 - Ende
          88 VERF-AG                         VALUE 15.
          88 VERF-AV                         VALUE 05.
          88 VERF-BP                         VALUE 14.
          88 VERF-DK                         VALUE 12.
*G.00.14 - Anfang
          88 VERF-EU                         VALUE 22.
*G.00.14 - Ende

*G.00.18 - Anfang
          88 VERF-LO                         VALUE 23.
*G.00.18 - Ende

          88 VERF-NF                         VALUE 99.
          88 VERF-OR                         VALUE 16.
          88 VERF-SH                         VALUE 07.
          88 VERF-TN                         VALUE 18.
          88 VERF-TO                         VALUE 10.
          88 VERF-UT                         VALUE 17.
          
*G.01.02 - Anfang
          88 VERF-IQ                         VALUE 24.
*G.01.02 - Ende

*kl20180525 - G.01.10 - Integration Roadrunner AS
          88 VERF-RR                         VALUE 25.
*kl20180525 - G.01.10 - Ende
          

**          ---> Verfahrensfestlegung für Artikelmapper
**          ---> AG, AV und TN sind gleich (werden wie AG behandelt)
 01          AS-VERF             PIC X(02).
          88 AS-VERF-AG                      VALUE "AG".
          88 AS-VERF-AV                      VALUE "AV".
          88 AS-VERF-BP                      VALUE "BP".
          88 AS-VERF-DK                      VALUE "DK".
*G.00.14 - Anfang
          88 AS-VERF-EU                      VALUE "EU".
*G.00.14 - Ende

*G.00.18 - Anfang
          88 AS-VERF-LO                      VALUE "LO".
*G.00.18 - Ende

          88 AS-VERF-OR                      VALUE "OR".
          88 AS-VERF-SH                      VALUE "SH".
          88 AS-VERF-TN                      VALUE "TN".
          88 AS-VERF-TO                      VALUE "TO".
          88 AS-VERF-UT                      VALUE "UT".

*G.01.02 - Anfang
          88 AS-VERF-IQ                      VALUE "IQ".
*G.01.02 - Ende

*kl20180525 - G.01.10 - Integration Roadrunner AS
          88 AS-VERF-RR                      VALUE "RR".
*kl20180525 - G.01.10 - Ende
                 
          88 AS-VERF-DEFAULT                 VALUE "AG".

**          ---> Parametertabelle für Autorisierungssystem
**          ---> wird im Programmvorlauf geladen, d.h. bei Änderungen
**          ---> muss das Programm (Serverklasse) neu gestartet werden


*kl20180405 - G.01.06 - Sieht gut aus, ist aber hier nicht angebracht
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
*kl20180405 - G.01.06 - Ende

**          ---> zu suchende Werte
 01          S-SEARCH-KEY.
     05      S-ROUTKZ            PIC S9(04) comp.
     05      S-CARDID            PIC S9(04) comp.
     05      S-ISONTYP           PIC S9(04) comp.
     05      S-KZ-MSG            PIC  X(02).
     05      S-BMP               PIC S9(04) comp.
     05      S-LFDNR             PIC S9(04) comp.

*kl20180405 - G.01.06 - Zusätzlicher Searchkey für CARDID = 0
*                       (wird aus S-SEARCH-KEY gefüllt; dann wird
*                        ledglich S2-CARDID mit ZERO überschrieben)
 01          S2-SEARCH-KEY.
     05      S2-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
     05      S2-CARDID            PIC S9(04) COMP VALUE ZEROS.
     05      S2-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
     05      S2-KZ-MSG            PIC  X(02).
     05      S2-BMP               PIC S9(04) COMP VALUE ZEROS.
     05      S2-LFDNR             PIC S9(04) COMP VALUE ZEROS.
*kl20180405 - G.01.06 - Ende

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
*G.02.01 - AIID hier mit speichern
      10     TK-AIID             PIC X(11).
*G.02.01 - Ende

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

**          ---> Schnittstelle zu SYSAWKZ
     COPY    SYSWKZ0C OF "=MSGLIB"
             REPLACING =="*"== BY ==WKZ==.

**          ---> Schnittstelle zu WCAPM92
     COPY    PCAPM01C    OF "=MSGLIB"
             REPLACING =="*"== BY ==PCAP==.

**          ---> Schnittstelle zu WISO207
     COPY    WISO207C OF "=MSGLIB"
             REPLACING =="*"== BY ==W207==.

**          ---> Schnittstelle zu WSY7066
     COPY    WSY7066C OF "=MSGLIB"
             REPLACING =="*"== BY ==W66==.

**          ---> Schnittstelle zu WSYS930
     COPY    WSYS930C OF "=MSGLIB"
             REPLACING =="*"== BY ==ROUT==.

**          ---> Schnittstelle zu WSYS971
     COPY    WSYS971C OF "=MSGLIB"
             REPLACING =="*"== BY ==CHK==.

**          ---> Fuer Umsatz
     COPY    WUMSO07C OF "=MSGLIB"
             REPLACING =="*"== BY ==WUMS==.

*G.00.19 - Anfang
**          ---> Für Boxen-interface
     COPY    WEUR056C OF  "=MSGLIB"
             REPLACING =="*"== BY ==Z==.
*G.00.19 - Ende

*G.01.07
      COPY    ZPVERKAUF-IFC OF "=MSGLIB".
*G.01.07 - ende

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

**  --->  Transaktionslog Nachrichten der Station
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-TS
 END-EXEC

**  --->  Transaktionslog Nachrichten zum/vom AS
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-AS
 END-EXEC

**  ---> Struktur der Tabelle UMSWEAT
 EXEC SQL
    INVOKE =UMSWEAT  AS UMSWEAT
 END-EXEC
 
*G.02.01 - neue Tabelle für AIID
 EXEC SQL
    INVOKE =FCAIID AS FCAIID
 END-EXEC
*G.02.01 - Ende

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
          WHERE   ROUTKZ = :ROUTKZ  OF FCPARAM
*G.00.32 - Anfang
            AND   APPKZ  = "R7"
*G.00.32 - Anfang

*kl20180405 - G.01.06 - wg. Prioriseirung CARDID=X vor CARDID=0
*          ORDER  BY ROUTKZ, CARDID, ISONTYP, BMP, LFDNR
          ORDER  BY ROUTKZ          ASC,
                    CARDID          DESC,
                    ISONTYP         ASC,
                    BMP             ASC,
                    LFDNR           ASC
*kl20180405 - G.01.06 - Ende

         BROWSE  ACCESS
 END-EXEC

**  ---> Cursor auf Tabelle KEYNAMEN
 EXEC SQL
     DECLARE KEYNAMEN_CURS CURSOR FOR
         SELECT   ROUTKZ, CARDID, KEYNAME, ISOGEN, ISOVERS
           FROM  =KEYNAMEN
*G.02.01 - alle laden
*          WHERE   ROUTKZ = :ROUTKZ of KEYNAMEN
         ORDER  BY ROUTKZ, CARDID
*G.02.01 - Ende
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
                         MOVE 1004 TO ERROR-NR OF GEN-ERROR
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

     
*G.02.01 - jetzt neu ueber Parameter AS-VERF
***  ---> holen Parameter AS-ROUTKZ
*     MOVE "AS-ROUTKZ" TO STUP-PORTION
*     PERFORM P950-GETPARAMTEXT
*     IF  PRG-ABBRUCH
*         EXIT SECTION
*     END-IF

**  ---> holen Parameter AS-ROUTKZ 
     MOVE "AS-VERF" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF
     
**  ---> holen Parameter für zuständiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO ROUTKZ OF FCPARAM
*                                       W-ROUTKZ
                                       S-ROUTKZ
***                                    ---> für Artikelmapper
*                                       VERF-ROUTKZ
                                       VERF-AS
*G.02.01 - Ende

**  ---> Anwendung setzen für Artikelmapper
**  ---> (die auf Kommentar gesetzten sind default (AG))
     EVALUATE TRUE

*         WHEN VERF-AG    SET AS-VERF-AG TO TRUE
*         WHEN VERF-AV    SET AS-VERF-AV TO TRUE
         WHEN VERF-BP    SET AS-VERF-BP TO TRUE
         WHEN VERF-DK    SET AS-VERF-DK TO TRUE
*G.00.14 - Anfang
         WHEN VERF-EU    SET AS-VERF-EU TO TRUE
*G.00.14 - Ende

*G.00.18 - Anfang
         WHEN VERF-LO    SET AS-VERF-LO TO TRUE
*G.00.18 - Ende
         WHEN VERF-OR    SET AS-VERF-OR TO TRUE
         WHEN VERF-SH    SET AS-VERF-SH TO TRUE
*         WHEN VERF-TN    SET AS-VERF-TN TO TRUE
         WHEN VERF-TO    SET AS-VERF-TO TO TRUE
         WHEN VERF-UT    SET AS-VERF-UT TO TRUE
*G.01.02 - Anfang
         WHEN VERF-IQ    SET AS-VERF-IQ TO TRUE
*G.01.02 - Ende

*kl20180525 - G.01.10 - Integration Roadrunner AS
         WHEN VERF-RR    SET AS-VERF-RR TO TRUE
*kl20180525 - G.01.10 - Integration Roadrunner AS

         WHEN OTHER      SET AS-VERF-DEFAULT TO TRUE

     END-EVALUATE

**  ---> interne Tabelle initialisieren
*kl20180109 - G.01.01 - Initialisieren mit T-TAB-MAX
*                       statt Fixwert 200/500
     MOVE   T-TAB-MAX       TO T-MAX
*kl20180109 - G.01.01 - Ende
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

         MOVE ROUTKZ        OF FCPARAM TO T-ROUTKZ        (C4-I1)
         MOVE CARDID        OF FCPARAM TO T-CARDID        (C4-I1)
         MOVE ISONTYP       OF FCPARAM TO T-ISONTYP       (C4-I1)
         MOVE KZ-MSG        OF FCPARAM TO T-KZ-MSG        (C4-I1)
         MOVE BMP           OF FCPARAM TO T-BMP           (C4-I1)
         MOVE LFDNR         OF FCPARAM TO T-LFDNR         (C4-I1)
         MOVE KZ-ABWEICHUNG OF FCPARAM TO T-KZ-ABWEICHUNG (C4-I1)

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
*kl20180105 - G.01.00 - Erlaubtes Maximum in T-TAB-MAX
*     IF  C9-COUNT > T-MAX
     IF  C9-COUNT > T-TAB-MAX
*kl20180105 - G.01.00 - Ende
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

*G.02.01 - zusätzlich AIID mit in diese Tabelle laden + alle Eintraege aus KEYNAMEN
**  ---> AS Schlüssel MACKEYA und PACKEYA aus Tabelle =KEYNAMEN einlesen
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> !!!! zunächstmal wird nur der erste eingelesen !!!!
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> ersten Eintrag holen
*     MOVE W-ROUTKZ TO ROUTKZ OF KEYNAMEN
     PERFORM S930-OPEN-KEYNAMEN-CURSOR
     PERFORM S940-FETCH-KEYNAMEN-CURSOR

**  ---> Schleife über alle Einträge für das ROUTKZ
     PERFORM VARYING C4-I1 FROM 1 BY 1
             UNTIL   KEYNAMEN-EOD
             or      KEYNAMEN-NOK
             or      C9-COUNT > TK-TAB-MAX
*             or      c4-i1 > 1

         MOVE ROUTKZ  OF KEYNAMEN TO TK-ROUTKZ  (C4-I1)
         MOVE CARDID  OF KEYNAMEN TO TK-CARDID  (C4-I1)
         MOVE KEYNAME OF KEYNAMEN TO TK-KEYNAME (C4-I1)
         MOVE ISOGEN  OF KEYNAMEN TO TK-ISOGEN  (C4-I1)
         MOVE ISOVERS OF KEYNAMEN TO TK-ISOVERS (C4-I1)
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
         
         PERFORM S960-SELECT-AIID
         MOVE AIID OF FCAIID TO TK-AIID(C4-I1)

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
*G.02.01 - Ende

**  ---> holen Boxen-Monitor
     MOVE "BOXMON" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     MOVE STUP-TEXT(1:STUP-RESULT) TO W66-BOXMON-TEXT

**  ---> holen Prozess- und ANCNAME
     PERFORM P960-GET-PROC-ANCNAME

     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF

**  ---> holen Parameter ANZREPA
     MOVE "ANZREPA" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF

**  ---> holen Parameter für zuständiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO W-ANZREPA

**  ---> holen Parameter ANZREPM
     MOVE "ANZREPM" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         EXIT SECTION
     END-IF

**  ---> holen Parameter für zuständiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO W-ANZREPM

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
*G.02.01 - ROUTKZ aus Drehscheibe holen
     MOVE IMSG-ROUTKZ  TO W-ROUTKZ
*G.02.01 - Ende
     
**  ---> Kontrolle der Anfrage <---
**  ---> ist die Anfrage evtl. OK?
     PERFORM C100-ANFRAGE-CHECK

*G.00.10 - Anfang
*    IF  ENDE
*        EXIT SECTION
*    END-IF
**
     IF  ENDE
     AND W-AC = ZERO
         EXIT SECTION
     END-IF

*G.00.10 - Ende

     IF  W-AC not = ZERO
**      ---> Ablehnung senden: Formatfehler
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

*G.06.17 Anfang - Bei FEP-Verarbeitung entfällt der komplette
*          AS-Anteil; Nur Eigenantwort mit AC = 0 + Logging
     IF PRF-AS
        CONTINUE
     ELSE
**      ---> Antwort senden und TX loggen
        PERFORM E100-FEP-ANTWORT
        PERFORM C500-LOGGING
        EXIT SECTION
     END-IF
*G.06.17 - Ende

**  ---> für alle AS'sen gültige Transaktions Regeln
     PERFORM C200-AS-GENERELL

*G.00.10 - Anfang
*    IF  ENDE
*        EXIT SECTION
*    END-IF
**
     IF  ENDE
     AND W-AC = ZERO
         EXIT SECTION
     END-IF

     IF  W-AC NOT = ZEROS
**      ---> Ablehnung senden:
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

*G.00.10 - Ende


**  ---> spezielle Regeln für AS'sen
     PERFORM C300-AS-SPEZIELL

*G.00.10 - Anfang
*    IF  ENDE
*        EXIT SECTION
*    END-IF
**
     IF  ENDE
     AND W-AC = ZERO
         EXIT SECTION
     END-IF

     IF  W-AC not = ZEROS
**      ---> Ablehnung senden:
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

*G.00.10 - Ende

**  ---> und hier die AS-Nachricht zusammenbauen
     PERFORM C400-BUILD-AS-NACHRICHT

**  ---> Check AC
     IF  ENDE
     AND W-AC = ZERO
         EXIT SECTION
     END-IF

     IF  W-AC NOT = ZEROS
**      ---> Ablehnung senden: 12/96 (s. C200-AS-NACHRICHT)
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

**  ---> Schreiben der Log-Daten
     PERFORM C500-LOGGING

     IF ENDE
     OR ASYNC70-NOK
        MOVE 91 TO W-AC
        PERFORM E100-FEP-ANTWORT
        EXIT SECTION
     ELSE
        MOVE ZEROS TO W-AC
        PERFORM E110-FEP-ANTWORT-AC-NULL
        PERFORM M180-CALL-IUMSW07
        EXIT SECTION
     END-IF

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
                WORK-INIT
                ASYNC70

     MOVE ZERO TO W-AC
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
         MOVE P-HEX8(1:4) TO TERMID OF APPL-SPEC-BUF
     END-IF

**  ---> Trace-Nr. schon malmerken
     IF  IMSG-TBMP(11) = 1 AND IMSG-TPTR(11) > 0 AND IMSG-TLEN(11) > 0
         MOVE IMSG-CF(IMSG-TPTR(11):IMSG-TLEN(11)) TO W-TRACENR
     END-IF

**  ---> weitere Felder merken
     MOVE IMSG-MDNR TO MDNR OF TXILOG70
                       MDNR OF TXNLOG70-TS
                       MDNR OF TXNLOG70-AS
                       W-MDNR

     MOVE IMSG-TSNR TO TSNR OF TXILOG70
                       TSNR OF TXNLOG70-TS
                       TSNR OF TXNLOG70-AS
                       W-TSNR

     MOVE W-TERMNR  TO TERMNR OF TXILOG70
                       TERMNR OF TXNLOG70-TS
                       TERMNR OF TXNLOG70-AS

**  ---> Nachrichten enthalten keinen MAC

**  ---> formale Prüfung der Nachricht
     MOVE ZERO TO W-AC
     SET  CHK-CHECK-ALL TO TRUE
     MOVE IMSG-NTYPE    TO CHK-NTYPE
                           W-NTYPE
     MOVE "X"           TO CHK-NTYPE (4:1)
     MOVE "R7"          TO CHK-ABWKZ(1:2)
     MOVE K-MODUL (2:4) TO CHK-ABWKZ(3:4)
**  ---> soll dann "040X" "R7FCST" sein

**  ---> formale Prüfung durch Modul WSYS971
     PERFORM M160-CALL-WSYS971
     IF  ENDE or W-AC > ZERO
         EXIT SECTION
     END-IF

**  ---> Zahlung oder Gutschrift
     IF  IMSG-CF(IMSG-TPTR(3):2) = "67"
     OR  IMSG-CF(IMSG-TPTR(3):2) = "77"
         SET UMS-ZAHLUNG    TO TRUE
     ELSE
         SET UMS-GUTSCHRIFT TO TRUE
     END-IF

*G.00.12 - Anfang

**  ---> Offline oder Online
     IF  IMSG-CF(IMSG-TPTR(3):2) = "77"
     OR  IMSG-CF(IMSG-TPTR(3):2) = "78"
         SET OFFLINE-BUCHUNG OF BUCHUNGS-FLAG TO TRUE
     ELSE
         SET ONLINE-BUCHUNG  OF BUCHUNGS-FLAG TO TRUE
     END-IF

*G.00.12 - Ende

**  ---> Abwicklungs-KZ merken
     MOVE IMSG-CF(IMSG-TPTR(03):IMSG-TLEN(03)) TO W-ABWKZ
     MOVE IMSG-CF(IMSG-TPTR(03) + 2:4)         TO W-BELEGNR

**  ---> Betrag aufbereiten und merken (nur für Eigenantwort)
     MOVE IMSG-CF(IMSG-TPTR(04):IMSG-TLEN(04)) TO W-BETRAG
     COMPUTE W18-BETRAG = W-BETRAG / 100


*G.01.07 - Anfang
*
**  ---> Verkaufszeitpunkt aufbereiten (20 + BMP13 + BMP12)
*     MOVE IMSG-CF(IMSG-TPTR(12):IMSG-TLEN(12)) TO D-NUM6
*     MOVE IMSG-CF(IMSG-TPTR(13):IMSG-TLEN(13)) TO D-NUM12
*     COMPUTE D-NUM12      = D-NUM12 * 1000000
*     COMPUTE W-ZP-VERKAUF = (10000000000 * TAL-JHJJ OF TAL-TIME-D)
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
      MOVE ZPVERKAUF-TXILOG70                   TO W-ZP-VERKAUF

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
*G.01.07 - Ende


**  ---> feststellen, ob Auto- oder Man.Storno
     MOVE IMSG-CF(IMSG-TPTR(37) + 6:6) TO W-TRACENR-37
     IF  W-TRACENR = W-TRACENR-37
         IF IMSG-CF(IMSG-TPTR(3):2) = "77" or "78"
            MOVE 30 TO W-AC
            EXIT SECTION
         END-IF
         SET STORNO-AUTO    TO TRUE
     ELSE
         SET STORNO-MANUELL TO TRUE
     END-IF

**  ---> prüfen, ob Wiederholungsnachricht
     IF  W-NTYPE > 400
         SET STORNO-WIEDERHOLUNG TO TRUE
         IF  STORNO-MANUELL
**          ---> manuelle Storno-Wiederholungen sind verboten
             MOVE 30 TO W-AC
             MOVE "Manuelles Wiederholungsstorno ist NICHT zulässig"
                 TO DATEN-BUFFER1
             STRING  "TERMNR / TRACENR = "
                     W-TERMNR " / " W-TRACENR
                         delimited by size
               INTO  DATEN-BUFFER2
             END-STRING
             PERFORM Z002-PROGERR
             EXIT SECTION
         END-IF
     END-IF

**  ---> holen Routing etc.
     PERFORM D900-ROUTING-ETC
     IF  W-AC not = ZERO
         EXIT SECTION
     END-IF

**  ---> holen zuständigen Keynamen
*G.06.17 - Nur bei AS-Verarbeitung
     IF PRF-AS
        PERFORM D200-FIX-KEY
**  ---> setzen Keyname für diese Transaktion
        MOVE TK-KEYNAME (C4-I1) TO W-KEYNAME
        MOVE TK-HEXISO  (C4-I1) TO W-ISOGEN-VERS
     END-IF
*G.06.17 - Ende

**  ---> Erfassungsart (BMP22)
     MOVE IMSG-CF(IMSG-TPTR(22) + 1:2) TO W-ERFASSUNGS-ART


*G.00.33 - Chipdaten, muessen vorliegen/fehlen je nach Erfassungsart
     IF W-ERF-CHIP AND IMSG-TBMP(55) = 0
     OR NOT W-ERF-CHIP AND IMSG-TBMP(55) = 1
         MOVE 30 TO W-AC
         MOVE "falsche Kombination BMP 22 und 55"
             TO DATEN-BUFFER1
         STRING  "TERMNR / TRACENR = "
                 W-TERMNR " / " W-TRACENR
                     delimited by size
           INTO  DATEN-BUFFER2
         END-STRING
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF

**  ---> holen Chipdaten, wenn vorhanden
     IF IMSG-TBMP(55) = 1
        MOVE IMSG-TLEN(55)                        TO W-BMP55-LEN
        MOVE IMSG-CF(IMSG-TPTR(55):IMSG-TLEN(55)) TO W-BMP55
     END-IF
*G.00.33 - Ende

**  ---> Währungs-KZ könnte hier gegen Tabelle WKZKURS geprüft werden
     MOVE IMSG-CF (IMSG-TPTR (49) + 1:3) TO W-WKZ
     MOVE W-WKZ TO WKZ-WKZ
     SET WKZ-CMD-WKZ-A TO TRUE
     PERFORM M170-CALL-SYSAWKZ
     IF  WKZ-ERR
         EXIT SECTION
     END-IF

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
     MOVE K-BYTEMAP-A1420 TO W207-TBMP-O (1:64)

**  ---> Werte für Tabelle =FCPARAM
     MOVE 1420 TO S-ISONTYP
     MOVE "AS" TO S-KZ-MSG
     MOVE 1    TO S-LFDNR

     SET W207-IFSF TO TRUE
**  ---> Nachrichtentyp für Nachricht setzen
**  ---> prüfen, ob Wiederholung
     IF  STORNO-WIEDERHOLUNG
         MOVE 1421 TO W207-NTYPE
     ELSE
         MOVE 1420 TO W207-NTYPE
     END-IF

**  ---> schon mal einen Dummy-MAC vorbereiten
     MOVE 1420 TO S-ISONTYP
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

**  ---> BMP  2 - PAN
     MOVE 02                   TO W207-XBMP
     MOVE KANR OF TXILOG70-AUT TO W207-XCOBVAL

**  ---> Länge der KANR bestimmen
     MOVE ZERO TO W-KANR-LEN
     PERFORM VARYING C4-I1 FROM 19 BY -1
             UNTIL   C4-I1 = ZERO

         IF  KANR OF TXILOG70-AUT (C4-I1:1) not = SPACE
             MOVE C4-I1 TO W-KANR-LEN
             EXIT PERFORM
         END-IF

     END-PERFORM

**  ---> wenn Länge = 0: Fehlermeldung und ENDE
     IF  W-KANR-LEN = ZERO
         SET ENDE TO TRUE
         MOVE "Keine KANR in =TXNLOG70" TO DATEN-BUFFER1
         STRING  "TermNr./TraceNr. = "
                 W-TERMNR
                 "/"
                 W-TRACENR
                     delimited by size
           INTO  DATEN-BUFFER2
         END-STRING

     END-IF

     MOVE W-KANR-LEN           TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP  3 - Zahlung/Gutschrift (nur Buffer manipulieren)
     IF  UMS-ZAHLUNG
         MOVE "000000" TO W207-CF(W207-TPTR(3):W207-TLEN(3))
     ELSE
         MOVE "200000" TO W207-CF(W207-TPTR(3):W207-TLEN(3))
     END-IF

**  ---> BMP  4 - Betrag - wird übernommen
**  ---> BMP  7 - Übertragungszeit
     MOVE 07 TO W207-XBMP
     MOVE 10 TO W207-XCOBLEN
     STRING  TAL-MM   OF TAL-TIME-D
             TAL-TT   OF TAL-TIME-D
             TAL-HHMI OF TAL-TIME-D
             TAL-SS   OF TAL-TIME-D
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
*G.01.08 - Anhand ZP-VERKAUF (Kombination TS BMP 12/13) AS BMP 12 fuellen
     COMPUTE D-NUM12 = W-ZP-VERKAUF - 20000000000000
     MOVE D-NUM12 TO W207-XCOBVAL
*     STRING  TAL-JHJJMMTT of TAL-TIME-D (3:6)
*             TAL-HHMI     of TAL-TIME-D
*             TAL-SS       of TAL-TIME-D
*                 delimited by size
*       INTO W207-XCOBVAL
*     END-STRING
*G.01.08 - Ende
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 14 - Ablaufdatum
     MOVE 14     TO W207-XBMP
     MOVE 04     TO W207-XCOBLEN
     MOVE ABL-JJMM OF TXILOG70-AUT TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 24 - Funktionscode
     MOVE 24     TO W207-XBMP
     MOVE 03     TO W207-XCOBLEN
     MOVE "400"  TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 25 - Message Reason
     MOVE 25     TO W207-XBMP
     MOVE 04     TO W207-XCOBLEN
     IF  STORNO-AUTO
         MOVE "4021" TO W207-XCOBVAL
     ELSE
         MOVE "4000" TO W207-XCOBVAL
     END-IF
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.02.01 - AIID aus neuer Tabelle FCAIID (geladen bei Start)
**  ---> BMP 32 - Netzbetreiber Kennung AIID
*     MOVE 32 TO S-BMP
*     MOVE 1  TO S-LFDNR
*     PERFORM U300-SEARCH-TAB
*     IF  PRM-NOT-FOUND
*         PERFORM E900-PUT-ERRLOG
*         SET ENDE TO TRUE
*         EXIT SECTION
*     END-IF
*     PERFORM U400-INTERPRET-ABWEICHUNG
*     MOVE 32           TO W207-XBMP
*     MOVE W-BUFFER-LEN TO W207-XCOBLEN
*     MOVE W-BUFFER     TO W207-XCOBVAL
     
     MOVE 32     TO W207-XBMP
     MOVE W-AIID TO W207-XCOBVAL
     MOVE ZERO TO D-NUM4N
     INSPECT W-AIID TALLYING D-NUM4N
     FOR CHARACTERS BEFORE INITIAL " "
     MOVE D-NUM4N TO W207-XCOBLEN 
*G.02.01 - Ende
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.00.29 - Anfang
*
**  ---> BMP 38 - Autorisierungs-KZ (wird hier zunächst gesetzt, muss
**  --->          ggf. in den spez. Section's wieder ausschalten)
*    IF  GENNR OF TXILOG70-AUT not = SPACE
*        MOVE 38                    TO W207-XBMP
*        MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
*        MOVE 6                     TO W207-XCOBLEN
*        PERFORM L100-ADD-BMP
*        IF  ENDE
*            EXIT SECTION
*        END-IF
*    END-IF
*
*G.00.29 - Anfang

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
*G.00.33 - 42 aus Nachricht, wenn vorhanden
     IF IMSG-TBMP(42) = 1
        MOVE IMSG-CF(IMSG-TPTR(42):IMSG-TLEN(42)) TO W207-XCOBVAL
     ELSE
        MOVE VUNR OF TSKART40 TO W207-XCOBVAL
     END-IF
*G.00.33 - Ende
     MOVE 15               TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

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


*G.00.33 - Chipdaten in AS Anfrage
**  ---> BMP 55 - wenn Chiperfassung
     IF W-ERF-CHIP
        MOVE 55          TO W207-XBMP
        MOVE W-BMP55-LEN TO W207-XCOBLEN
        MOVE W-BMP55     TO W207-XCOBVAL
        PERFORM L100-ADD-BMP
     END-IF
*G.00.33 - Ende

**  ---> BMP 56 - Daten der originalen Transaktion
     MOVE 56     TO W207-XBMP
     MOVE SPACES TO W207-XCOBVAL
     MOVE 1 TO C4-PTR

*kl20180528 - G.01.10 - BMP 12+13(ZP-VERKAUF) aus Autorisierung verwenden
*                       (uebernommen alternative G.01.10 KUS)
*     MOVE TAL-JHJJ of TAL-TIME-D (3:2) TO D-NUM2
*     MOVE AF-BMP07 OF TXILOG70-AUT     TO D-NUM10
     COMPUTE D-NUM12 = ZP-VERKAUF OF TXILOG70-AUT - 20000000000000

*G.00.27 - Anfang
*    STRING  "1200"
**
     IF STORNO-MANUELL
      IF OFFLINE-BUCHUNG
         MOVE 1220 TO D-NUM4M
      ELSE
        MOVE 1200 TO D-NUM4M
      END-IF
     ELSE
*G.00.35 - Autostorno auch auf Voraut (1100) moeglich
**  ---> BETRAG-ART = "M" bedeutet Vorautorisierung
        IF BETRAG-ART OF TXILOG70-AUT = "M"
            MOVE 1100 TO D-NUM4M
        ELSE
            MOVE 1200 TO D-NUM4M
        END-IF
     END-IF
*G.00.35 - Ende

     STRING  D-NUM4M
*G.00.27 - Ende
             TRACENR-AS OF TXILOG70-AUT
*             D-NUM2
*             D-NUM10
             D-NUM12
                 delimited by size
       INTO  W207-XCOBVAL
       WITH  POINTER C4-PTR
     END-STRING
*kl20180528 - G.01.10 - Ende

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

**  ---> nur fürs testen
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
*G.02.01 - VERF-AS jetzt
     EVALUATE VERF-AS
         WHEN 05     PERFORM D305-AVIA
         WHEN 07     PERFORM D307-SHELL
         WHEN 10     PERFORM D310-TOTAL
         WHEN 12     PERFORM D312-DKV
         WHEN 14     PERFORM D314-BP
         WHEN 15     PERFORM D315-ENI
         WHEN 16     PERFORM D316-ORLEN
         WHEN 17     PERFORM D317-UTA
         WHEN 18     PERFORM D318-TND

*G.00.14 - Anfang
         WHEN 22     PERFORM D322-EUROWAG
*G.00.14 - Anfang

*G.00.18 - Anfang
         WHEN 23     PERFORM D323-LOGPAY
*G.00.18 - Anfang

*G.01.02 - Anfang
*kl20180525 - G.01.10 - D[Routkennzeichen]-[AS] !!!
*         WHEN 24     PERFORM D326-STIGLECHNER
         WHEN 24     PERFORM D324-STIGLECHNER
*G.01.02 - Anfang

*kl20180525 - G.01.10 - Integration Roadrunner AS
         WHEN 25     PERFORM D325-ROADRUNNER
*kl20180525 - G.01.10 - Integration Roadrunner AS
         
         WHEN OTHER
                 SET ENDE TO TRUE
*                 MOVE W-ROUTKZ TO D-NUM4
                 MOVE VERF-AS  TO D-NUM4
                 STRING  "Keine speziellen Verarbeitungsregeln "
                         "für Rout-KZ = "
                         D-NUM4
                             delimited by size
                   INTO  DATEN-BUFFER1
                 END-STRING
                 PERFORM Z002-PROGERR
                 EXIT SECTION

     END-EVALUATE
*G.02.01 - Ende
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
* Loggen der Transaktionsdaten in den MEMLOG
******************************************************************
 C500-LOGGING SECTION.
 C500-00.
**  ---> Führungstabelle =TXILOG70
     PERFORM G100-PUT-TXILOG70

**  ---> Anfragenachricht für Tabelle =TXNLOG70 TS-Nachrichten
     PERFORM G110-PUT-TXNLOG70-TS

*G.00.10 - Anfang

**  ---> Eintrag TRACENR_S in TXILOG70 der Autorisierung
     PERFORM G102-PUT-TXILOG70-AUT
     PERFORM S185-UPDATE-TXILOG70-AUT
     IF  ENDE
         EXIT SECTION
     END-IF

*G.00.10 - Ende

**  ---> nun UMSWEAT bedienen
     PERFORM G130-PUT-UMSWEAT
     IF  ENDE
         EXIT SECTION
     END-IF

*G.06.17 - ASYNC70 nur bei AS-Verabeitung
     IF PRF-AS
        PERFORM G090-PUT-ASNYC70
     END-IF
*G.06.17 - Ende

**  ---> holen momentanen Zeitpunkt
     PERFORM U200-TIMESTAMP
     MOVE TAGESDATUM  TO ZP-AOUT OF TXILOG70
     MOVE H-ZP-IN     TO ZP-TIN  OF TXILOG70
     .
 C500-99.
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

*G.02.01 - auch ROUTKZ Vergleich, da nun alle Eintraege geladen
         IF  TK-CARDID (C4-I1) NOT = W-CARDID
         AND TK-ROUTKZ (C4-I1) NOT = W-ROUTKZ
*G.02.01 - Ende
**          ---> nächsten suchen
             EXIT PERFORM CYCLE
         END-IF

**      ---> Schlüssel zur Verfügung stellen
         MOVE TK-HEXKEY (C4-I1) TO W-MACKEYA
         MOVE TK-HEXKEY (C4-I1) TO W-PACKEYA (1:4)
**      ---> Schlüsselgeneration und -version bereitstellen
         MOVE TK-HEXISO (C4-I1) TO W-ISOGEN-VERS
*G.02.01 - AIID speichern
         MOVE TK-AIID (C4-I1)   TO W-AIID
*G.02.01 - Ende
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

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
       MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

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

*G.00.29 - Anfang
**  ---> BMP38 ausschalten
     MOVE ZERO TO W207-TBMP(38)
*G.00.29 - Ende

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT

*G.00.34 - Sonderbehandlung verkürztes BMP 59 für Shell2

*    Laenge aus allgemeinem Teil wieder reduzieren (nur noch 10 Byte:
*    Release + Applikation + AS-Tracenr)
     MOVE   10     TO W207-TLEN(59)

*    W207-CF Terminal-ID / Tracenr / Abwkz mit AS-TRACENR ueberschreiben
     MOVE W-AS-TRACENR    TO W207-CF(W207-TPTR(59) + 4:6)
*G.00.34 - Ende
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

*G.00.29 - Anfang
**  ---> BMP38 ausschalten
     MOVE ZERO TO W207-TBMP(38)
*G.00.29 - Ende

** G.00.06 - Anfang
*** ---> BMP 22 - Eingabeart wird hier überschrieben
**   MOVE 22 TO W207-XBMP
**   MOVE 12 TO W207-XCOBLEN
**   IF  ERFASSUNGS-ART OF TXILOG70-AUT = 01
***     ---> handeingabe
**       MOVE "210101613001" TO W207-XCOBVAL
**   ELSE
**       MOVE "210101B13001" TO W207-XCOBVAL
**   END-IF
**   PERFORM L100-ADD-BMP
**   IF  ENDE
**       EXIT SECTION
**   END-IF
** G.00.06 - Ende

*G.07.17 - Anfang

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
*G.07.17 - Ende

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

*G.00.26 - Anfang
     IF W-CARDID = 19
**BMP53 einschalten
**BMP64 einschalten

        MOVE 1 TO W207-TBMP(53)
        MOVE 1 TO W207-TBMP(64)

     END-IF

*G.00.26 - Ende

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

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
         MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

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
*G.00.15 - Anfang
*    SET W66-TOTAL TO TRUE
**
     SET W66-DEFAULT TO TRUE
*G.00.15 - Ende

** G.00.06 - Anfang
*** ---> BMP 22 - Eingabeart wird hier überschrieben
**   MOVE 22 TO W207-XBMP
**   MOVE 12 TO W207-XCOBLEN
**   IF  ERF-MANUELL
**       MOVE "C11101654144" TO W207-XCOBVAL
**   ELSE
**       MOVE "C11101214144" TO W207-XCOBVAL
**   END-IF
**   PERFORM L100-ADD-BMP
**   IF  ENDE
**       EXIT SECTION
**   END-IF
** G.00.06 - Ende

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
         MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

**  ---> BMP 42 - VUNR wird hier überschrieben
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

*G.00.13 - Anfang
*    MOVE W-TSNR       TO W207-XCOBVAL (W207-XCOBLEN + 9:8)
**
     MOVE W-TSNR       TO W207-XCOBVAL (W207-XCOBLEN + 3:8)

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

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
         MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

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

** G.00.07 - Anfang
** ---> BMP 22 - Eingabeart wird hier überschrieben
   MOVE 22 TO W207-XBMP
   MOVE 12 TO W207-XCOBLEN

   IF  ERFASSUNGS-ART OF TXILOG70-AUT = 01
**   ---> handeingabe
       MOVE "C10101654144" TO W207-XCOBVAL
   ELSE
       MOVE "C10101214144" TO W207-XCOBVAL
   END-IF
   PERFORM L100-ADD-BMP
   IF  ENDE
       EXIT SECTION
   END-IF
** G.00.07 - Ende

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
        MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

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

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
      ELSE
         MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

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

**  ---> BMP38 ausschalten
     MOVE ZERO TO W207-TBMP(38)

*G.00.30 - Anfang
**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
*G.00.30 - Ende

     continue
     .
 D318-99.
     EXIT.

*G.00.14 - Anfang

******************************************************************
* spezielle Behandlung für das Eurowag-AS
*    ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
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

*G.00.35 - BMP 41 senden
     MOVE 1 TO W207-TBMP(41)
*G.00.35 - Ende

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
         MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D322-99.
     EXIT.

*G.00.14 - Ende

*G.00.18 - Anfang

******************************************************************
* spezielle Behandlung für das LogPay-AS
*    ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
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

*G.00.29 - Anfang

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
         MOVE ZERO TO W207-TBMP(38)
     END-IF

*G.00.29 - Ende

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D323-99.
     EXIT.

*G.00.18 - Ende

*G.01.02 - Anfang

******************************************************************
* spezielle Behandlung für das LogPay-AS
* ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
******************************************************************
*kl20180525 - G.01.10 - D[Routkennzeichen]-[AS] !!!         
* D326-STIGLECHNER SECTION.
* D326-00.
 D324-STIGLECHNER SECTION.
 D324-00.
*kl20180525 - G.01.10 - D[Routkennzeichen]-[AS] !!!
 
**  ---> Anwendung für MAC-Bildung setzen
**   SET W66-DEFAULT TO TRUE
     SET W66-DKV     TO TRUE

     MOVE 1 TO W207-TBMP(53)
     MOVE 1 TO W207-TBMP(64)

**  ---> BMP 14 - Ablaufdatum
     MOVE 14   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "0"
             MOVE ZERO TO W207-TBMP-O (14:1)
         END-IF
     END-IF

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
         MOVE ZERO TO W207-TBMP(38)
     END-IF

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
     .
 D324-99.
     EXIT.
*G.01.02 - Ende

*kl20180525 - G.01.10 - Integration Roadrunner AS (voerst lt.
*                       Vereinbarung identsich zu AVIA)
******************************************************************
* spezielle Behandlung für das RoadRunner-AS
*    ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
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

     IF  OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
     AND GENNR           OF TXILOG70-AUT NOT = SPACES
         MOVE 38                    TO W207-XBMP
         MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
         MOVE 6                     TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     ELSE
       MOVE ZERO TO W207-TBMP(38)
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
**  ---> Werte für Zugriff auf Autorisierung belegen
     MOVE W-TERMNR       TO TERMNR  OF TXILOG70-AUT
     MOVE W-TERMNR (7:2) TO PNR     OF TXILOG70-AUT
     MOVE W-TRACENR-37   TO TRACENR OF TXILOG70-AUT
     MOVE 200            TO ISONTYP OF TXILOG70-AUT
     PERFORM S200-SELECT-TXILOG70-AUT
     IF  W-AC not = ZERO
         EXIT SECTION
     END-IF

     MOVE CARDID OF TXILOG70-AUT TO ROUT-CARDID
                                    W-CARDID
                                    S-CARDID
     MOVE KANR   OF TXILOG70-AUT TO W-KANR

**  ---> VUNR / Routkz über TSKART40 holen
     MOVE IMSG-MDNR TO MDNR    OF TSKART40
     MOVE IMSG-TSNR TO TSNR    OF TSKART40

*G.00.37 - Anfang
*    IF  IMSG-TBMP(25) = 1
*        MOVE 2     TO CARDSYS OF TSKART40
*    ELSE
*        MOVE 1     TO CARDSYS OF TSKART40
*    END-IF
**

     IF  IMSG-TBMP(25) = 1
     OR  BETRAG-ART OF TXILOG70-AUT = "M"
         MOVE 2     TO CARDSYS OF TSKART40
     ELSE
         MOVE 1     TO CARDSYS OF TSKART40
     END-IF

*G.00.37 - Ende

     MOVE W-CARDID  TO CARDID  OF TSKART40

     PERFORM S150-SELECT-TSKART40
     IF  W-AC not = ZERO
         EXIT SECTION
     END-IF

*G.06.17 - Entscheidung, ob FEP oder AS-TX
     MOVE AKZ OF TSKART40 TO PRUEF-ORT

*    Falls Pruefort = AS weiter, sonst ist hier fertig
     IF PRF-AS
        CONTINUE
     ELSE
        EXIT SECTION
     END-IF
*G.06.17 - Ende

**  ---> Routinginformationen über Modul WSYS930 holen
     MOVE W-ROUTKZ TO ROUT-KZ
     SET ROUT-CMD-AS TO TRUE
     PERFORM M150-CALL-WSYS930
*
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
     MOVE IMSG-MDNR          TO MDNR   OF MDNR2AS
     MOVE CARDID OF TSKART40 TO CARDID OF MDNR2AS
     MOVE W-ROUTKZ           TO ROUTKZ OF MDNR2AS
     PERFORM S110-SELECT-MDNR2AS

*G.00.10 - Anfang
*     IF  ENDE
*         EXIT SECTION
*     END-IF
**
     IF ENDE
     OR W-AC NOT = ZEROS
        EXIT SECTION
     END-IF
*G.00.10 - Ende

     MOVE AIID OF MDNR2AS TO W-ACQUIRER-ID

*G.00.10 - Anfang
*     IF  W-AC NOT = ZERO
***     ---> Ablehnung senden: AS nicht verfügbar
*         PERFORM E100-FEP-ANTWORT
*         EXIT SECTION
*     END-IF

      IF  W-AC NOT = ZERO
**      ---> Ablehnung senden: AS nicht verfügbar
          EXIT SECTION
      END-IF

*G.00.10 - Ende

**  ---> prüfen auf Überschlag
     IF  TRACENR OF MDNR2AS = 999999
         MOVE 1 TO TRACENR OF MDNR2AS
     ELSE
         ADD  1 TO TRACENR OF MDNR2AS
     END-IF

**  ---> sichern AS-Trace-Nr.
     MOVE TRACENR OF MDNR2AS TO W-AS-TRACENR

**  ---> TraceNr. und Datum updaten
     PERFORM S120-UPDATE-MDNR2AS

*G.00.10 - Anfang
*    IF  W-AC NOT = ZERO
***     ---> Ablehnung senden: AS nicht verfügbar
*        PERFORM E100-FEP-ANTWORT
*        EXIT SECTION
*    END-IF
*G.00.10- Ende
     .
 D910-99.
     EXIT.

******************************************************************
* generieren FEP-Antwort
******************************************************************
 E100-FEP-ANTWORT SECTION.
 E100-00.
     SET FEP-ANTWORT TO TRUE
     MOVE TS-INTERN-MESSAGE TO INTERN-MESSAGE

**  ---> zunaechst Felder ausschalten, die nicht gesendet werden sollen
     MOVE ZERO TO IMSG-TBMP (02)
     MOVE ZERO TO IMSG-TBMP (14)
     MOVE ZERO TO IMSG-TBMP (22)
     MOVE ZERO TO IMSG-TBMP (25)
     MOVE ZERO TO IMSG-TBMP (26)
     MOVE ZERO TO IMSG-TBMP (35)

*G.00.12 - Anfang
     IF OFFLINE-BUCHUNG  OF BUCHUNGS-FLAG
        MOVE ZERO TO IMSG-TBMP (42)
     END-IF
*G.00.12 - Anfang

     MOVE ZERO TO IMSG-TBMP (52)

*G.00.33 - Keine Chipdaten in Antwort
     MOVE ZERO TO IMSG-TBMP (55)
*G.00.33 - Ende
     MOVE ZERO TO IMSG-TBMP (56)
     MOVE ZERO TO IMSG-TBMP (60)
     MOVE ZERO TO IMSG-TBMP (63)

**  ---> im Folgenden noch die Felder BMP 33, 39 hinzufuegen
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

**  ---> BMP39  ACODE
     MOVE 39    TO W207-XBMP
     MOVE W-AC  TO W207-XCOBVAL
     MOVE 2     TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.00.12 - Anfang
     IF OFFLINE-BUCHUNG  OF BUCHUNGS-FLAG
        CONTINUE
     ELSE
*G.00.33 - Wenn 42 in Anfrage, dann nicht in Antwort
        IF TS-TBMP(42) = 1
            CONTINUE
        ELSE
**  ---> BMP42  VUNR
            MOVE 42               TO W207-XBMP
            MOVE VUNR OF TSKART40 TO W207-XCOBVAL
            MOVE 15               TO W207-XCOBLEN
            PERFORM L100-ADD-BMP
            IF  ENDE
                EXIT SECTION
            END-IF
        END-IF
*G.00.33
     END-IF

*G.00.12 - Ende

**  ---> BMP53  Sicherheitsverfahren
**  --->        wird so zurück gegeben wie's gekommen ist

**  ---> Nachricht zusammenstellen
     SET W207-EC TO TRUE
     PERFORM L110-COB2ISO
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> Logdaten für Eigenantwort bereitstellen
     PERFORM F100-LOGDATEN-EIGEN

**  --> FREGAT-Parameter setzen
     SET  IMSG-WRITE-SL    TO TRUE
     MOVE W-FRE-MONNAME    TO IMSG-NEXTSERV
     MOVE MODUL OF MYPROG  TO IMSG-MONNAME
     .
 E100-99.
     EXIT.

******************************************************************
* generieren FEP-Antwort
******************************************************************
 E110-FEP-ANTWORT-AC-NULL SECTION.
 E110-00.
     SET FEP-ANTWORT TO TRUE
     MOVE TS-INTERN-MESSAGE TO INTERN-MESSAGE

**  ---> zunaechst Felder ausschalten, die nicht gesendet werden sollen
     MOVE ZERO TO IMSG-TBMP (02)
     MOVE ZERO TO IMSG-TBMP (14)
     MOVE ZERO TO IMSG-TBMP (22)
     MOVE ZERO TO IMSG-TBMP (25)
     MOVE ZERO TO IMSG-TBMP (26)
     MOVE ZERO TO IMSG-TBMP (35)
     MOVE ZERO TO IMSG-TBMP (37)

*G.00.12 - Anfang

     IF OFFLINE-BUCHUNG  OF BUCHUNGS-FLAG
        MOVE ZERO TO IMSG-TBMP (42)
     END-IF

*G.00.12 - Ende

     MOVE ZERO TO IMSG-TBMP (52)
*G.00.33 - Keine Chipdaten in Antwort
     MOVE ZERO TO IMSG-TBMP (55)
*G.00.33 - Ende
     MOVE ZERO TO IMSG-TBMP (56)
     MOVE ZERO TO IMSG-TBMP (60)
     MOVE ZERO TO IMSG-TBMP (63)

**  ---> im Folgenden noch die Felder BMP 33, 39, 59 hinzufuegen
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

**  ---> BMP39  ACODE
     MOVE 39    TO W207-XBMP
     MOVE W-AC  TO W207-XCOBVAL
     MOVE 2     TO W207-XCOBLEN
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.00.12 - Anfang

     IF OFFLINE-BUCHUNG  OF BUCHUNGS-FLAG
        CONTINUE
     ELSE
*G.00.33 - Wenn 42 in Anfrage, dann nicht in Antwort
        IF TS-TBMP(42) = 1
            CONTINUE
        ELSE
**  ---> BMP42  VUNR
            MOVE 42               TO W207-XBMP
            MOVE VUNR OF TSKART40 TO W207-XCOBVAL
            MOVE 15               TO W207-XCOBLEN
            PERFORM L100-ADD-BMP
            IF  ENDE
                EXIT SECTION
            END-IF
        END-IF
*G.00.33 - Ende
     END-IF

*G.00.12 - Anfang

**  ---> BMP53  Sicherheitsverfahren
**  --->        wird so zurück gegeben wie's gekommen ist

*A.00.28 - Anfang
     IF ONLINE-BUCHUNG  OF BUCHUNGS-FLAG
        IF GENNR OF TXILOG70-AUT NOT = SPACES
           MOVE 59                    TO W207-XBMP
           MOVE 6                     TO W207-XCOBLEN
           MOVE GENNR OF TXILOG70-AUT TO W207-XCOBVAL
           PERFORM L100-ADD-BMP
           IF  ENDE
               EXIT SECTION
           END-IF
        END-IF
     END-IF
*A.00.28 - Ende

**  ---> Nachricht zusammenstellen
     SET W207-EC TO TRUE
     PERFORM L110-COB2ISO
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> Logdaten für Eigenantwort bereitstellen
     PERFORM F100-LOGDATEN-EIGEN

**  --> FREGAT-Parameter setzen
     SET  IMSG-WRITE-SL    TO TRUE
     MOVE W-FRE-MONNAME    TO IMSG-NEXTSERV
     MOVE MODUL OF MYPROG  TO IMSG-MONNAME
     .
 E110-99.
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
     MOVE CARDID OF TSKART40 TO AMP-CARDID

     MOVE AS-VERF    TO AMP-FORMAT
     MOVE ALL SPACES TO AMP-HOST-VAL
     MOVE ZERO       TO AMP-RC
                        AMP-HOST-LEN

**  ---> Pathsend zu WXAMP07S
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
     MOVE 1105 TO ERROR-NR OF GEN-ERROR
     MOVE "=FCPARAM für: @" TO DATEN-BUFFER1
*G.02.01 - VERF-AS jetzt
*     MOVE W-ROUTKZ  TO D-NUM4
     MOVE VERF-AS   TO D-NUM4
*G.02.01 - Ende
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

******************************************************************
* Logdaten bei Eigenantworten bereitstellen
******************************************************************
 F100-LOGDATEN-EIGEN SECTION.
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

**  ---> Anfragenachricht für Tabelle =TXNLOG70
     PERFORM G110-PUT-TXNLOG70-TS
**  ---> und nun schreiben
     PERFORM S190-INSERT-TXNLOG70-TS
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und schliesslich Eintrag in CRDUSEDN erzeugen
     PERFORM G130-PUT-CRDUSEDN
**  ---> und nun schreiben
     PERFORM M100-CALL-SDBCDU5
     .
 F100-99.
     EXIT.

******************************************************************
* Einstellen Daten in ASYN70 Buffer
******************************************************************
 G090-PUT-ASNYC70 SECTION.
 G090-00.
      MOVE W-TERMNR (7:2)  TO        PNR        OF ASYNC70
      MOVE W-TERMNR        TO        TERMNR     OF ASYNC70
      MOVE W-TRACENR       TO        TRACENR    OF ASYNC70
      MOVE 400             TO        ISONTYP    OF ASYNC70
      MOVE W-MDNR          TO        MDNR       OF ASYNC70
      MOVE W-TSNR          TO        TSNR       OF ASYNC70
      MOVE "FK"            TO        VKZ        OF ASYNC70
      MOVE W-AS-TRACENR    TO        TRACENR-AS OF ASYNC70
      MOVE W-CARDID        TO        CARDID     OF ASYNC70
      MOVE W-ROUTKZ        TO        ROUTKZ     OF ASYNC70
      MOVE "A"             TO        KZ-BEARB   OF ASYNC70
      IF STORNO-AUTO
         MOVE W-ANZREPA    TO        ANZ-REP    OF ASYNC70
      ELSE
         MOVE W-ANZREPM    TO        ANZ-REP    OF ASYNC70
      END-IF
      MOVE IMSG-HEADER     TO        FREHEADER  OF ASYNC70
      MOVE IMSG-NDATEN     TO VAL OF ANFRAGE    OF ASYNC70
      MOVE IMSG-SENDLEN    TO LEN OF ANFRAGE    OF ASYNC70
      MOVE SPACES          TO VAL OF ANTWORT    OF ASYNC70
      MOVE ZEROS           TO LEN OF ANTWORT    OF ASYNC70
      MOVE W-KEYNAME       TO        KEY-NAME   OF ASYNC70

      PERFORM S170-ASYNC70-INSERT

     .

 G090-99.
     EXIT.

*G.00.19 - Anfang
******************************************************************
* Erstellen Antwort-MAC
******************************************************************
 F920-MAC-BILDEN SECTION.
 F920-00.
**  ---> MAC-Bildung und Schlüsselwechsel über WSY7066
**  ---> Datenbereich Message-datei external übergibt Daten für WSY7066
*G.06.12 - Anfang
*G.02.01 - VERF-AS jetzt
*   EVALUATE W-ROUTKZ
   EVALUATE VERF-AS
*G.02.01 - Ende
       WHEN 22
            PERFORM F950-ASMAC-DUKPT
       WHEN OTHER
         MOVE 88 TO W66-RCODE OF W66-WSY7066C
         PERFORM M140-CALL-WSY7066
         IF  W66-ERR
             MOVE 30   TO W-AC
             MOVE 1201 TO ERROR-NR OF GEN-ERROR
             MOVE W66-RCODE TO D-NUM4
             STRING  "WSY7066@" delimited by size
                     D-NUM4     delimited by size
                     "@@"       delimited by size
               INTO  DATEN-BUFFER1
             END-STRING
             MOVE "Bei F920-MAC-BILDEN" TO DATEN-BUFFER2
             MOVE "Transaktions-Ende"   TO DATEN-BUFFER3
             PERFORM Z002-PROGERR
             SET ENDE TO TRUE
             EXIT SECTION
         END-IF
   END-EVALUATE

**  --> Datenteil wird in WSY7066 mit MAC versehen
     .
 F920-99.
      EXIT.

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

*G.00.22 - Anfang
      MOVE W-AS-TRACENR    TO Z-AS-TRACENR
*G.00.22 - Ende

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
*G.00.19 - Ende

******************************************************************
* Einstellen Daten in TXILOG70 Buffer
******************************************************************
 G100-PUT-TXILOG70 SECTION.
 G100-00.
     MOVE W-TERMNR (7:2) TO PNR            OF TXILOG70
     MOVE W-TERMNR       TO TERMNR         OF TXILOG70
     MOVE W-TRACENR      TO TRACENR        OF TXILOG70
     MOVE W-NTYPE        TO ISONTYP        OF TXILOG70
     MOVE W-MDNR         TO MDNR           OF TXILOG70
     MOVE W-TSNR         TO TSNR           OF TXILOG70
     MOVE W-AS-TRACENR   TO TRACENR-AS     OF TXILOG70
     MOVE W-TRACENR-37   TO TRACENR-S      OF TXILOG70
     MOVE W-KANR         TO KANR           OF TXILOG70
     MOVE SPACES         TO KZ-E2EE        OF TXILOG70
     MOVE SPACES         TO KEYNAME        OF TXILOG70
     MOVE W18-BETRAG     TO BETRAG         OF TXILOG70

*G.03.02 - Anfang
     MOVE W18-BETRAG     TO BETRAG-AUTOR   OF TXILOG70
*G.03.02 - Ende

     MOVE "F"            TO BETRAG-ART     OF TXILOG70
     MOVE W-CARDID       TO CARDID         OF TXILOG70

*G.06.17 - Routing gibt es nicht bei FEP-Verarbeitung
     IF PRF-AS
        MOVE W-ROUTKZ    TO ROUTKZ         of TXILOG70
     ELSE
        MOVE ZEROES      TO ROUTKZ         of TXILOG70
     END-IF
*G.06.17 - Ende

     MOVE W-LTGIND       TO LTGIND         OF TXILOG70
     MOVE 740000         TO ASID           OF TXILOG70
     MOVE 9999           TO AC-AS          OF TXILOG70
     MOVE W-AC           TO AC-TERM        OF TXILOG70
     MOVE WKZ-WKZ-A      TO WKZ            OF TXILOG70
     MOVE 70             TO LOGPROT        OF TXILOG70
     MOVE "S"            TO KZ-BEARB       OF TXILOG70

*G.01.12 - neue KZ-VERF fuer Chip
*G.00.16 - Anfang - Sonst fuktioniert =UMSWEAT bei man. Storno nicht
     IF OFFLINE-BUCHUNG  OF BUCHUNGS-FLAG
        IF W-ERF-CHIP
            MOVE "q"         TO KZ-VERF        OF TXILOG70
        ELSE
            MOVE "k"         TO KZ-VERF        OF TXILOG70
        END-IF
     ELSE
        IF W-ERF-CHIP
            MOVE "r"         TO KZ-VERF        OF TXILOG70
        ELSE
            MOVE "f"         TO KZ-VERF        OF TXILOG70
        END-IF
     END-IF
*G.00-16 - Ende
*G.01.12 - Ende

     IF  STORNO-AUTO
         MOVE "A"        TO KZ-UMSATZ      OF TXILOG70
     ELSE
         MOVE "M"        TO KZ-UMSATZ      OF TXILOG70
     END-IF
     MOVE W-ABL          TO ABL-JJMM       OF TXILOG70
     MOVE W-ACQUIRER-ID  TO ACQUIRER-ID    OF TXILOG70
     MOVE W-ERFASSUNGS-ART TO ERFASSUNGS-ART OF TXILOG70
*G.00.33 - Wenn Chip Erfassung -> EMV Daten loggen und Trans-Art aendern
     IF W-ERF-CHIP
        MOVE W-BMP55-LEN TO LEN OF EMV-DATEN OF TXILOG70
        MOVE W-BMP55     TO VAL OF EMV-DATEN OF TXILOG70
     END-IF
*G.00.33 - Ende

*kl20180316 - G.01.05 - Unterscheidung zwischen Chip und Spur2
*     MOVE 221            TO KARTEN-ART     of TXILOG70
     IF W-ERF-CHIP
*       Kartenart = Chip ohne Cashback
        MOVE   211       TO KARTEN-ART     of TXILOG70
     ELSE
*       Kartenart = Spur2 Magnet
        MOVE   221       TO KARTEN-ART     of TXILOG70
     END-IF
*kl20180316 - G.01.05 - Ende

*G.00.33 - Auswertung Erfassungsart fuer TRANS-ART
*  --> Koennte prinzipiell auch aus Anfrage gesetzt werden
*     MOVE "2E"           TO TRANS-ART      OF TXILOG70
     IF OFFLINE-BUCHUNG OF BUCHUNGS-FLAG
         EVALUATE W-ERFASSUNGS-ART
            WHEN 01 MOVE "MO"       TO TRANS-ART      of TXILOG70
            WHEN 02 MOVE "2O"       TO TRANS-ART      of TXILOG70
            WHEN 05 MOVE "IC"       TO TRANS-ART      of TXILOG70
            WHEN OTHER CONTINUE
         END-EVALUATE
     ELSE
         EVALUATE W-ERFASSUNGS-ART
            WHEN 01 MOVE "ME"       TO TRANS-ART      of TXILOG70
            WHEN 02 MOVE "2E"       TO TRANS-ART      of TXILOG70
            WHEN 05 MOVE "OC"       TO TRANS-ART      of TXILOG70
            WHEN OTHER CONTINUE
         END-EVALUATE
     END-IF
*G.00.33 - Ende

     MOVE SPACES         TO TRANS-TYP      OF TXILOG70
     MOVE 5541           TO BRANCHEN-KZ    OF TXILOG70

*G.00.33 - Wenn VUNR in Anfrage, diese speichern
     IF TS-TBMP(42) = 1
        MOVE TS-CF(TS-TPTR(42):TS-TLEN(42)) TO VUNR OF TXILOG70
     ELSE
        MOVE VUNR OF TSKART40 TO VUNR OF TXILOG70
     END-IF
*G.00.33 - Ende


*G.00.33 - CVM Result aus Anfrage
     MOVE CVM-RESULT OF TXILOG70-AUT TO CVM-RESULT OF TXILOG70
*G.00.33 - Ende

     MOVE W-ZP-VERKAUF   TO ZP-VERKAUF     OF TXILOG70

**  ---> holen momentanen Zeitpunkt
     PERFORM U200-TIMESTAMP
     IF  FEP-ANTWORT
         MOVE TAGESDATUM TO ZP-TOUT OF TXILOG70
     ELSE
         MOVE TAGESDATUM TO ZP-AOUT OF TXILOG70
     END-IF
     MOVE H-ZP-IN        TO ZP-TIN  OF TXILOG70
     .
 G100-99.
     EXIT.

*G.00.10 - Anfang

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

*G.00.10 - Ende

******************************************************************
* Einstellen Daten in TXNLOG70-TS Buffer
******************************************************************
 G110-PUT-TXNLOG70-TS SECTION.
 G110-00.
     MOVE W-TERMNR (7:2) TO PNR            OF TXNLOG70-TS
     MOVE W-TERMNR       TO TERMNR         OF TXNLOG70-TS
     MOVE W-TRACENR      TO TRACENR        OF TXNLOG70-TS
     MOVE W-NTYPE        TO ISONTYP        OF TXNLOG70-TS
     MOVE "T"            TO KZ-MSG         OF TXNLOG70-TS
     MOVE 1              TO ISO-VERF       OF TXNLOG70-TS
     MOVE W-MDNR         TO MDNR           OF TXNLOG70-TS
     MOVE W-TSNR         TO TSNR           OF TXNLOG70-TS
     MOVE K-MODUL        TO LOG-SRV        OF TXNLOG70-TS
     MOVE TS-HEADER      TO FREHEADER      OF TXNLOG70-TS
     MOVE TS-NDATEN      TO VAL OF ANFRAGE OF TXNLOG70-TS
     MOVE TS-DATLEN      TO LEN OF ANFRAGE OF TXNLOG70-TS
     MOVE IMSG-NDATEN    TO VAL OF ANTWORT OF TXNLOG70-TS
     MOVE IMSG-DATLEN    TO LEN OF ANTWORT OF TXNLOG70-TS
     .
 G110-99.
     EXIT.

******************************************************************
* Einstellen Daten in UMSWEAT Buffer
******************************************************************
 G130-PUT-UMSWEAT SECTION.
 G130-00.
     MOVE PNR       OF TXILOG70  TO PNR       OF UMSWEAT
     MOVE TERMNR    OF TXILOG70  TO TERMNR    OF UMSWEAT
     MOVE TRACENR   OF TXILOG70  TO TRACENR   OF UMSWEAT
     MOVE MDNR      OF TXILOG70  TO MDNR      OF UMSWEAT
     MOVE TSNR      OF TXILOG70  TO TSNR      OF UMSWEAT
     MOVE TRACENR-S OF TXILOG70  TO TRACENR-S OF UMSWEAT
     MOVE CARDID    OF TXILOG70  TO CARDID    OF UMSWEAT

*G.00.09 - Anfang

*    IF  STORNO-MANUELL
*        COMPUTE BETRAG of UMSWEAT = BETRAG of TXILOG70 * -1
*    ELSE
*        MOVE BETRAG of TXILOG70 TO BETRAG    of UMSWEAT
*    END-IF
**

     IF  STORNO-MANUELL
         COMPUTE BETRAG of UMSWEAT = BETRAG-AUTOR of TXILOG70 * -1
     ELSE
         MOVE BETRAG-AUTOR  of TXILOG70 TO BETRAG of UMSWEAT
     END-IF

*G.00.09 - Ende

     MOVE WKZ       OF TXILOG70  TO WKZ       OF UMSWEAT
     MOVE KZ-VERF   OF TXILOG70  TO KZ-VERF   OF UMSWEAT
     MOVE "S"                    TO KZ-BEARB  OF UMSWEAT
     MOVE W-BELEGNR              TO BELEGNR   OF UMSWEAT
     MOVE W-ABWKZ                TO ABWKZ     OF UMSWEAT

**  ---> Aufruf Modul IUMSw07 (Zugriff zum UMSIFSF-Server)
     MOVE UMSWEAT    TO WUMS-UMSATZ
     MOVE K-MODUL    TO WUMS-ABSENDER

     SET WUMS-TAB-UW TO TRUE
     IF  STORNO-MANUELL
         SET WUMS-CMD-I  TO TRUE
     ELSE
         SET WUMS-CMD-DT TO TRUE
     END-IF
     .
 G130-99.
     EXIT.

******************************************************************
* Einstellen Daten in CRDUSEDN-Buffer
******************************************************************
 G130-PUT-CRDUSEDN SECTION.
 G130-00.
     MOVe W-TERMNR (7:2) TO SDB-PNR
     MOVE W-KANR         TO SDB-KANR
     MOVE "S"            TO SDB-AKZ
     MOVE W-TERMNR       TO SDB-TERMNR
     MOVE W-TRACENR      TO SDB-TRACENR
     MOVE W-AC           TO SDB-AC
     MOVE BETRAG OF TXILOG70 TO SDB-BETRAG
     MOVE W-MDNR         TO SDB-MDNR
     MOVE W-TSNR         TO SDB-TSNR
     .
 G130-99.
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
         MOVE 1201 TO ERROR-NR OF GEN-ERROR
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
         MOVE 1201 TO ERROR-NR OF GEN-ERROR
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
*                WHEN ROUT-ASERR MOVE 91 TO W-AC
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

**  ---> nun das Ergebnis überprüfen
     EVALUATE CHK-RCODE
**      ---> OK
         WHEN ZERO       CONTINUE

**      ---> Fehler bei BMP  ==> ENDE
         WHEN 0001 THRU 0128
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR OF GEN-ERROR
                         STRING "BMP/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         SET ENDE TO TRUE

**      ---> andere Fehler (z.B. CMD-Error)  ==> ENDE
         WHEN 0129 THRU 0999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 1201 TO ERROR-NR OF GEN-ERROR
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
                         MOVE 2201 TO ERROR-NR OF GEN-ERROR
                         STRING "BMP nicht num./Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 30 TO W-AC

**      ---> BMP fehlt  ==> AC 30
         WHEN 4000 THRU 4999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR OF GEN-ERROR
                         STRING "BMP fehlt/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 30 TO W-AC

**      ---> BMP zuviel  ==> AC 30
         WHEN 5000 THRU 5999
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 2201 TO ERROR-NR OF GEN-ERROR
                         STRING "BMP zuviel/Retcode: " D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         MOVE 30 TO W-AC

**      ---> sonstige ?  ==> ENDE
         WHEN OTHER
                         MOVE CHK-RCODE TO D-NUM4
                         MOVE 1201 TO ERROR-NR OF GEN-ERROR
                         STRING "WSYS971 (Feldcheck)/"
                                "@"
                                D-NUM4
                                "@"
                                     delimited by size
                           INTO DATEN-BUFFER1
                         END-STRING
                         SET ENDE TO TRUE
     END-EVALUATE

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
         MOVE 1201 TO ERROR-NR OF GEN-ERROR
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
*G.00.36 - Anfang
*        SET ENDE TO TRUE
*G.00.36 - Ende
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
             MOVE 1210 TO ERROR-NR OF GEN-ERROR
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
             MOVE 1210 TO ERROR-NR OF GEN-ERROR
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
         MOVE "Programm-Abbruch (PFCSTO7S)"   TO DATEN-BUFFER2
         PERFORM Z002-PROGERR
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     ENTER TAL "WT^ANCNAME" USING FEHL
                                  ANCNAME
                                  PAIRINFO
     IF  FEHL not = ZERO
         MOVE "Ancestor Pathway nicht ermittelbar" TO DATEN-BUFFER1
         MOVE "Programm-Abbruch (PFCSTO7S)"        TO DATEN-BUFFER2
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
           INTO   :ROUTKZ OF KEYNAMEN
                 ,:CARDID OF KEYNAMEN
                 ,:ASNAME OF KEYNAMEN
                 ,:KEYNAME OF KEYNAMEN
                 ,:ISOGEN OF KEYNAMEN
                 ,:ISOVERS OF KEYNAMEN
                 ,:OPTIONEN OF KEYNAMEN
           FROM  =KEYNAMEN
          WHERE  ROUTKZ, CARDID
                 =    :ROUTKZ OF KEYNAMEN
                     ,:CARDID OF KEYNAMEN
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET KEYNAMEN-OK  TO TRUE
         WHEN OTHER      SET ENDE         TO TRUE
                         SET KEYNAMEN-NOK TO TRUE
                         MOVE 91   TO W-AC
                         MOVE 2001 TO ERROR-NR OF GEN-ERROR
                         MOVE ROUTKZ OF KEYNAMEN TO D-NUM20
                         MOVE CARDID OF KEYNAMEN TO D-NUM21
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
           INTO   :ROUTKZ OF MDNR2AS
                 ,:CARDID OF MDNR2AS
                 ,:MDNR OF MDNR2AS
                 ,:AIID OF MDNR2AS
                 ,:TRACENR OF MDNR2AS
                 ,:DATUM OF MDNR2AS
                     TYPE AS DATETIME YEAR TO DAY
           FROM  =MDNR2AS
          WHERE   ROUTKZ = :ROUTKZ OF MDNR2AS
            AND  (CARDID = :CARDID OF MDNR2AS or CARDID = 0)
            AND  (MDNR   = :MDNR   OF MDNR2AS or MDNR = 0)
            FOR  REPEATABLE ACCESS
             IN  EXCLUSIVE MODE
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET MDNR2AS-OK TO TRUE
         WHEN OTHER  MOVE 91   TO W-AC
                     MOVE 2001 TO ERROR-NR OF GEN-ERROR
                     MOVE CARDID OF MDNR2AS TO D-NUM20
                     MOVE ROUTKZ OF MDNR2AS TO D-NUM21
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
            SET   TRACENR = :TRACENR OF MDNR2AS
                 ,DATUM   = CURRENT YEAR TO DAY
          WHERE  MDNR, CARDID, ROUTKZ
                 =  :MDNR   OF MDNR2AS
                   ,:CARDID OF MDNR2AS
                   ,:ROUTKZ OF MDNR2AS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET MDNR2AS-OK  TO TRUE
         WHEN OTHER  SET MDNR2AS-NOK TO TRUE
                     MOVE 91   TO W-AC
                     MOVE 2003 TO ERROR-NR OF GEN-ERROR
                     MOVE CARDID OF MDNR2AS TO D-NUM20
                     MOVE ROUTKZ OF MDNR2AS TO D-NUM21
                     STRING "MDNR2AS@"
                            MDNR   OF MDNR2AS "/"
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
*G.06.17 - AKZ wird auch benötig für Entscheidung FEP/AS
*        SELECT    VUNR, ROUTKZ
**
         SELECT    VUNR, ROUTKZ, AKZ
           INTO   :VUNR OF TSKART40
                 ,:ROUTKZ OF TSKART40
                 ,:AKZ of TSKART40
*G.06.17 - Ende

           FROM  =TSKART40
          WHERE  MDNR, TSNR, CARDSYS, CARDID
                 =  :MDNR    OF TSKART40
                   ,:TSNR    OF TSKART40
                   ,:CARDSYS OF TSKART40
                   ,:CARDID  OF TSKART40
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET TSKART40-OK  TO TRUE
         WHEN OTHER      SET TSKART40-NOK TO TRUE
                         MOVE 2001 TO ERROR-NR OF GEN-ERROR
                         STRING "TSKART40@"
                                "CSYS/CID: "
                                CARDSYS OF TSKART40 "/"
                                CARDID  OF TSKART40 "@"
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
           INTO   :ORT OF STATION
                 ,:NAME OF STATION
           FROM  =STATION
          WHERE  MDNR, TSNR
                 =    :MDNR OF STATION
                     ,:TSNR OF STATION
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET STATION-OK  TO TRUE
         WHEN OTHER  SET ENDE        TO TRUE
                     SET STATION-NOK TO TRUE
                     MOVE 2001 TO ERROR-NR OF GEN-ERROR
                     STRING  "STATION@"
                             "MDNR/TSNR: "
                             MDNR OF STATION "/"
                             TSNR OF STATION "@"
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
                           PNR     OF ASYNC70 "/"
                           TERMNR  OF ASYNC70 "/"
                           TRACENR OF ASYNC70 "/"
                           ISONTYP OF ASYNC70
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
                  :PNR OF TXILOG70
                 ,:TERMNR OF TXILOG70
                 ,:TRACENR OF TXILOG70
                 ,:ISONTYP OF TXILOG70
                 ,:MDNR OF TXILOG70
                 ,:TSNR OF TXILOG70
                 ,:TRACENR-AS OF TXILOG70
                 ,:TRACENR-S OF TXILOG70
                 ,:BATCHNR OF TXILOG70
                 ,:KANR OF TXILOG70
                 ,:KZ-E2EE OF TXILOG70
                 ,:KEYNAME OF TXILOG70
                 ,:BETRAG OF TXILOG70
                 ,:BETRAG-AUTOR OF TXILOG70
                 ,:BETRAG-CASHBACK OF TXILOG70
                 ,:BETRAG-ART OF TXILOG70
                 ,:CARDID OF TXILOG70
                 ,:ROUTKZ OF TXILOG70
                 ,:LTGIND OF TXILOG70
                 ,:ASID OF TXILOG70
                 ,:AC-AS OF TXILOG70
                 ,:AC-TERM OF TXILOG70
                 ,:GENNR OF TXILOG70
                 ,:WKZ OF TXILOG70
                 ,:LOGPROT OF TXILOG70
                 ,:KZ-BEARB OF TXILOG70
                 ,:KZ-VERF OF TXILOG70
                 ,:KZ-UMSATZ OF TXILOG70
                 ,:ABL-JJMM OF TXILOG70
                 ,:ACQUIRER-ID OF TXILOG70
                 ,:ERFASSUNGS-ART OF TXILOG70
                 ,:KARTEN-ART OF TXILOG70
                 ,:KARTENFOLGE OF TXILOG70
                 ,:POS-DATEN OF TXILOG70
                 ,:TRANS-ART OF TXILOG70
                 ,:TRANS-TYP OF TXILOG70
                 ,:CVM-RESULT OF TXILOG70
                 ,:BRANCHEN-KZ OF TXILOG70
                 ,:HAENDLERNAME OF TXILOG70
                 ,:PROJEKT-ABH-DATEN OF TXILOG70
                 ,:VUNR OF TXILOG70
                 ,:ZP-VERKAUF OF TXILOG70
*                 ,:H-ZP-IN
                 ,:ZP-TIN OF TXILOG70
                     TYPE AS DATETIME YEAR TO FRACTION(2)
*                 ,:H-ZP-OUT
                 ,:ZP-TOUT OF TXILOG70
                     TYPE AS DATETIME YEAR TO FRACTION(2)
                 ,:AA-BMP38 OF TXILOG70
                 ,:AF-BMP07 OF TXILOG70
                 ,:ARTIKEL OF TXILOG70
                 ,:EMV-DATEN OF TXILOG70
                 )
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET TXILOG70-OK  TO TRUE
         WHEN OTHER  SET TXILOG70-NOK TO TRUE
                     SET ENDE TO TRUE
                     MOVE 2005 TO ERROR-NR OF GEN-ERROR
                     STRING  "TXILOG70@"
                             PNR     OF TXILOG70 "/"
                             TERMNR  OF TXILOG70 "/"
                             TRACENR OF TXILOG70 "/"
                             ISONTYP OF TXILOG70
                                 DELIMITED BY SIZE
                       INTO DATEN-BUFFER1
                     END-STRING
                     PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S180-99.
     EXIT.

*G.00.10 - Anfang

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

*G.00.10 - Ende

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
                  :PNR OF TXNLOG70-TS
                 ,:TERMNR OF TXNLOG70-TS
                 ,:TRACENR OF TXNLOG70-TS
                 ,:ISONTYP OF TXNLOG70-TS
                 ,:KZ-MSG OF TXNLOG70-TS
                 ,:ISO-VERF OF TXNLOG70-TS
                 ,:MDNR OF TXNLOG70-TS
                 ,:TSNR OF TXNLOG70-TS
                 ,:LOG-SRV OF TXNLOG70-TS
                 ,:FREHEADER OF TXNLOG70-TS
                 ,:ANFRAGE OF TXNLOG70-TS
                 ,:ANTWORT OF TXNLOG70-TS
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
*G.00.35 - BETRAG-ART auch selecten, ist "M" fuer Voraut(1100)
     EXEC SQL
       SELECT  PNR            ,
               TERMNR         ,
               TRACENR        ,
               ISONTYP        ,
               KANR           ,
               CARDID         ,
               TRACENR_AS     ,
               GENNR          ,
               ABL_JJMM       ,
               ERFASSUNGS_ART ,
               CVM_RESULT     ,
               ZP_VERKAUF     ,
               AF_BMP07       ,
               AC_AS          ,
               AC_TERM        ,
               BETRAG_ART
         INTO :PNR            OF TXILOG70-AUT ,
              :TERMNR         OF TXILOG70-AUT ,
              :TRACENR        OF TXILOG70-AUT ,
              :ISONTYP        OF TXILOG70-AUT ,
              :KANR           OF TXILOG70-AUT ,
              :CARDID         OF TXILOG70-AUT ,
              :TRACENR-AS     OF TXILOG70-AUT ,
              :GENNR          OF TXILOG70-AUT ,
              :ABL-JJMM       OF TXILOG70-AUT ,
              :ERFASSUNGS-ART OF TXILOG70-AUT ,
*G.00.33 - CVM Result aus Anfrage
              :CVM-RESULT     OF TXILOG70-AUT ,
*G.00.33 - Ende
              :ZP-VERKAUF     OF TXILOG70-AUT ,
              :AF-BMP07       OF TXILOG70-AUT ,
              :AC-AS          OF TXILOG70-AUT ,
              :AC-TERM        OF TXILOG70-AUT ,
              :BETRAG-ART     OF TXILOG70-AUT
*G.00.35 - Ende
         FROM =TXILOG70
        WHERE  PNR, TERMNR, TRACENR, ISONTYP
            =  :PNR     OF TXILOG70-AUT
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
          INTO  :ROUTKZ OF FCPARAM
               ,:CARDID OF FCPARAM
               ,:ISONTYP OF FCPARAM
               ,:KZ-MSG OF FCPARAM
               ,:BMP OF FCPARAM
               ,:LFDNR OF FCPARAM
               ,:KZ-ABWEICHUNG OF FCPARAM
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
          INTO  :ROUTKZ OF KEYNAMEN
               ,:CARDID OF KEYNAMEN
               ,:KEYNAME OF KEYNAMEN
               ,:ISOGEN OF KEYNAMEN
               ,:ISOVERS OF KEYNAMEN
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
* Select auf neue AIID-Tabelle
*G.02.01 - neu
******************************************************************
 S960-SELECT-AIID SECTION.
 S960-00.

     EXEC SQL
         SELECT AIID
         INTO  :AIID     OF FCAIID
         FROM  =FCAIID
         WHERE ROUTKZ, CARDID =
              :ROUTKZ    OF KEYNAMEN
             ,:CARDID    OF KEYNAMEN
         BROWSE ACCESS
     END-EXEC
     
     
     IF SQLCODE OF SQLCA = 100
         EXEC SQL
             SELECT AIID
             INTO  :AIID     OF FCAIID
             FROM  =FCAIID
             WHERE ROUTKZ, CARDID =
                  :ROUTKZ    OF KEYNAMEN
                 ,0
             BROWSE ACCESS
         END-EXEC
     END-IF
     
     IF NOT SQLCODE OF SQLCA = ZERO
         MOVE  SQLCODE OF SQLCA    TO D-NUM4
         STRING "Fehler bei LESEN FCAIID: ", D-NUM4
         DELIMITED BY SIZE INTO DATEN-BUFFER1
         MOVE ROUTKZ OF KEYNAMEN TO D-NUM4-ROUTKZ
         MOVE CARDID OF KEYNAMEN TO D-NUM4-CARDID
         STRING "ROUTKZ= ",
                 D-NUM4-ROUTKZ,
                 " / CARDID= ",
                 D-NUM4-CARDID,
                 " oder 0"
         DELIMITED BY SIZE INTO DATEN-BUFFER2
         PERFORM Z002-PROGERR
     END-IF

     .
 S960-99.
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
*                    PRM-NOT-FOUND - false OF true
******************************************************************
 U300-SEARCH-TAB SECTION.
 U300-00.

*kl20180405 - G.01.06 - wg. Cardid ZERO zurueck zur klassischen
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
*kl20180405 - G.01.06 - Ende
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
*G.02.01 - VERF-AS jetzt
*                         MOVE W-ROUTKZ TO D-NUM4
                         MOVE VERF-AS  TO D-NUM4
*G.02.01 - Ende
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
