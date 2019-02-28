?CONSULT $SYSTEM.SYSTEM.COBOLEX0
?SEARCH  $SYSTEM.SYSTEM.COBOLLIB
?SEARCH  =TALLIB
?SEARCH  =ASC2EBC
?SEARCH  =EBC2ASC
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
*kl20160323 - G.01.03 - Pruefmodul BIN-Sperre
?SEARCH  =PFCBNS7
*kl20160323 - G.01.03 - Ende

*G.01.18 - Anfang
?SEARCH  =WCSI060
?SEARCH  =WEUR055
?SEARCH  =WEUR056
*G.01.18 - Ende

*G.06.38 - Fuer TAG Handling
?SEARCH  =WISO300
?SEARCH  =WISO310
?SEARCH  =WISO400
*G.06.38 - Ende

*kl20170929 - G.06.42 - Fuer Bearbeitung FREHEADER
?SEARCH =WFREH07
*kl20170929 - G.06.42 - Ende

*G.06.52 - Modul für Zusammenstellung ZP-VERKAUF
?SEARCH =ZPVERK
*G.06.52 - ende


?NOLMAP, SYMBOLS, INSPECT
?SAVE ALL
?SAVEABEND
?LINES 66
?CHECK 3
?SQL


 IDENTIFICATION DIVISION.

 PROGRAM-ID. PFCPRE7 .

 DATE-COMPILED.


**************************************************************
* Letzte Aenderung :: 2019-02-11
* Letzte Version   :: G.07.05
* Kurzbeschreibung :: Dieses Programm setzt Flottenkarten-
* Kurzbeschreibung :: Autorisierungsanfragen vom Terminal-Protok.
* Kurzbeschreibung :: auf AS-IFSF-Protokoll um. Bearbeitet werden
* Kurzbeschreibung :: nur Terminalnachrichten von Typ 200, die
* Kurzbeschreibung :: auf AS-Nachrichten vom Typ 1200 umgesetzt
* Kurzbeschreibung :: werden.
* Package          :: ICC
* Auftrag          :: R7-376
*
* Aenderungen
*
*---------------------------------------------------------------------------*
* Vers. | Datum    | von | Kommentar                                        *
*-------|----------|-----|--------------------------------------------------*
*G.07.05|2019-02-11| kus | R7-471:
*       |          |     | - Neu Kompilierung für Modul PFCBNS7
*-------|----------|-----|--------------------------------------------------*
*G.07.04|2018-12-17| das | R7-436:
*       |          |     | - Keine VUNR Sonderlocke für BP
*-------|----------|-----|--------------------------------------------------*
*G.07.03|2018-12-11| das | R7-430:
*       |          |     | - Keine VUNR Sonderlocke für ENI
*-------|----------|-----|--------------------------------------------------*
*G.07.02|2018-10-05| kus | DKVCHIP-23:
*       |          |     | - Erfassungsart 7 kontaktlos wie Chip
*       |          |     | RRIFSF-13:
*       |          |     | - KM-Stand + Fahrzeugnummer an RR-AS
*       |          |     | DKVCHIP-26:
*       |          |     | - IFSF 22.1 konfigurierbar über FCPARAM
*-------|----------|-----|-------------------------------------------------*
*G.07.01|2018-09-11| kus | R7-376:
*       |          |     | - Umstellung von festem ROUTKZ auf AS-Verf
*-------|----------|-----|-------------------------------------------------*
*G.06.56|2018-08-03| kus | R7-365/DKVCHIP-4:
*       |          |     | - neues KZ-VERF fuer Chip
*-------|----------|-----|--------------------------------------------------*
*G.06.55|20180524  | kus | RRIFSF-3:
*       |          |     | - Umsetzung Roadrunner (Routkz = 25)
*-------|----------|-----|--------------------------------------------------*
*G.06.54|20180518  | kl  | Neukompilierung wg. Korrektur ZPVERKM
*       |          |     | (ZP-VERKAUF Modul)
*-------|----------|-----|--------------------------------------------------*
*G.06.53|2018-04-30| kus | F1ICC-104:
*       |          |     | - AS BMP 12 fuellen mit ZP vom Terminal
*       |          |     |   dafuer ZP-VERKAUF verwenden (ist Kombi
*       |          |     |   aus Terminal BMP 12/13)
*-------|----------|-----|-------------------------------------------*
*G.06.52|2018-04-11| sk  | Jira R7-304: Modulaufruf f. zp_verkauf Berechnung*
*-------|----------|-----|-------------------------------------------*
*G.06.51|2018-04-05| kl  | R7-272:
*       |          |     | Optimierung Zugriff / Laden FCPARAM
*       |          |     | (CARDID = 0 oder X - Vorrang bei X)
*-------|----------|-----|-------------------------------------------*
*G.06.50|2018-03-16| kl  | Bei Chip-Transaktionen Kartenart = 211
*       |          |     | setzen; BASIS: G.06.48
*       |          |     | (DKVCHIP-21)
*-------|----------|-----|-------------------------------------------*
*G.06.49|2018-03-16| kl  | VERWORFEN: Falsche Implementierung R7-269
*-------|----------|-----|--------------------------------------------*
*G.06.48|2018-01-23| hkn | Neu: Stiglechner mit Routkz = 24
*-------|----------|-----|--- ----------------------------------------*
*G.06.47|2018-01-09| kl  | Initialierung T-MAX in B000 optimiert      *
*-------|----------|-----|--------------------------------------------*
*G.06.46|2018-01-05| kl  | Speichertabelle fuer FCPARAM vergroessert  *
*-------|----------|-----|--------------------------------------------*
*G.03.45|2017-01-04| hkn | R7-Version aus X-Version kopiert
*-------|----------|-----|---------------------------------------*
*G.03.44|2017-11-17| kl  | TXILOG70.KZ-E2EE jetzt nicht mehr fix;
*       |          |     | aus INT-SCHNITTSTELLE-C.reserve1(1:2)
*       |          |     | JIRA: R7-238
*       |2017-11-21| hkn | POST70-Zugriff mit Prüfung auf
*       |          |     | MDNR/TSNR
*-------|----------|-----|---------------------------------------*
*G.06.43|2017-10-10| hkn |Rücknahme G.06.40/G.03.03(Nur Kommentar)
*-------|----------|-----|---------------------------------------*
*G.06.42|2017-09-29| kl  |- MEMLOG-Switch-Ready (WFREH07)
*       |          |     |
*       |          | hkn |  Erfassung-CHIP: BMP14 und BMP 35
*       |          |     |  müssen vorkommen
*-------|----------|-----|---------------------------------------*
*G.06.41|2017-09-07| hkn |AS-Nachricht: BMP 14 auch für CHIP-
*       |          |     |Verarbeitung
*       |          |     |Zurück geändert, da falsch
*-------|----------|-----|---------------------------------------*
*G.06.40|2017-08-31| hkn | Prüfung Bmp55 auf Tag 5F34 - Karten-
*G.03.03|          |     | folgenr. und BMP23
*-------|----------|-----|---------------------------------------*
*G.06.39|2016-05-30| das | Verkürztes BMP59 erst mal nur Shell
*-------|----------|-----|---------------------------------------*
*G.06.38|2017-05-19| kus | - Anpassung TXILOG70 Felder fuer Chip
*-------|----------|-----|---------------------------------------*
*G.06.37|2016-05-08| hkn | VUNR: Kann mit der Eingangsnachricht
*       |          |     | geschickt werden.
*-------|----------|-----|---------------------------------------*
*G.06.36|2016-05-05| hkn | Anpassung: Memlog-Zugriff über BMP59
*-------|----------|-----|---------------------------------------*
*G.06.35|2016-05-05| hkn | Anpassungen AC-Werte
*-------|----------|-----|---------------------------------------*
*G.06.34|2016-04-28| hkn |DKVCHIP: Anpassungen BMP55
*-------|----------|-----|---------------------------------------*
*G.06.33|2016-04-06| hkn |POST70: Update pro TRX
*-------|----------|-----|---------------------------------------*
*G.06.32|2016-03-15| hkn |Mac: Neu gebunden wg. Modul-Änderung
*-------|----------|-----|---------------------------------------*
*G.06.31|2017-02-14| hkn |BMP 41 - TerminalNr generell an alle
*       |          |     |Autorisierungs-System (AS) schicken
*-------|----------|-----|---------------------------------------*
*G.06.30|2017-02-13| hkn |Total: BMP 26 - Branchencode konstant
*       |          |     |mit Wert - 5541
*-------|----------|-----|---------------------------------------*
*G.06.29|2017-02-02| hkn | FCPARM ersetzt durch FCPARAM
*-------|----------|-----|---------------------------------------*
*G.06.28|2017-01-09| hkn | TND: BMP41 an AS (1100)
*-------|----------|-----|---------------------------------------*
*G.06.27|2016-12-29| HJO | Neu gebunden wg. Modul-Austausch
*       |          |     | WSY7066 - TND/R integriert
*-------|----------|-----|---------------------------------------*
*G.06.26|2016-12-01| kl  | Neu gebunden wg. grundsätzlicher Ablauf
*       |          |     | änderungen im Modul PFCBNS7 (BIN-Sperre)
*-------|----------|-----|---------------------------------------*
*G.06.25|2016-11-16| hkn | TND: PAC - BMP 52 ohne Prüfziffer,
*       |          |     | somit 12 Stellen, anstatt 13 Stellen
*-------|----------|-----|---------------------------------------*
*G.06.24|2016-11-09| hkn | Dummy-Übersetzung
*-------|----------|-----|---------------------------------------*
*G.06.23|2016-11-09| hkn | EUROWAG: BMP26 mit Wert 5541
*       |          |     | BMP (41) in Message 1100 schicken
*-------|----------|-----|---------------------------------------*
*G.06.22|2016-11-08| hkn | Übersetzung wg. Änderung DUKPT
*-------|----------|-----|---------------------------------------*
*G.06.21|2016-11-07| hkn | EUROWAG: Sicherheitsverfahren auch
*       |          |     | wenn kein PAC in TS-Anfrage vorhanden
*-------|----------|-----|---------------------------------------*
*G.06.20|2016-11-07| hkn | Übersetzung wg. Änderung DUKPT
*-------|----------|-----|---------------------------------------*
*G.06.19|2016-11-02| hkn | Eurowag/Logpay/TND in 1200 - keine
*       |          |     | Geodaten schicken ( BMP 48.41 = 0 )
*-------|----------|-----|---------------------------------------*
*G.06.19|2016-11-02| hkn | Eurowag/Logpay/TND in 1200 - keine
*       |          |     | Geodaten schicken ( BMP 48.41 = 0 )
*-------|----------|-----|---------------------------------------*
*G.06.18|2016-10-13| hkn | EUROWAG: PAC Umschlüssen und Bilden
*       |          |     | mit DUKPT-Verfahren
*-------|----------|-----|---------------------------------------*
*G.01.17|2016-09-21| hkn | Neu: LogPay mit Routkz = 23
*-------|----------|-----|---------------------------------------*
*G.01.16|2016-09-13| hkn | D310-TOTAL: Keine spezielle
*       |          |     | Bearbeitung BMP 42 - VUNR
*-------|----------|-----|---------------------------------------*
*G.01.15|2016-08-17| hkn | D314-BP: Macbildung - W66-DEFAULT
*-------|----------|-----|---------------------------------------*
*G.01.14|2016-08-02| hkn | Neu: Eurowag mit Routkz = 22
*-------|----------|-----|---------------------------------------*
*G.01.13|2016-08-01| hkn | POST70.TERMTYP = SPACES, dann Fehler
*       |          |     | und AC = 45 zurück an das Terminal
*-------|----------|-----|---------------------------------------*
*G.01.12|2016-07-27| hkn | D314-BP - BMP42: Positionierung korrig.
*       |          |     |           BMP48: Default-Verarbeitung
*-------|----------|-----|---------------------------------------*
*G.01.11|2016-07-26| hkn | Rücknahme G.01.10
*-------|----------|-----|---------------------------------------*
*G.01.10|2016-07-25| hkn | D305-AVIA: GEO-Daten (48.41) hexen,
*       |          |     | da diese in diesem Formant weiter
*       |          |     | verarbeteit werden (siehe ASAVA0E)
*-------|----------|-----|---------------------------------------*
*G.01.09|2016.07.05| hkn | AS-Kürzel nach AMP-FORMAT
*-------|----------|-----|---------------------------------------*
*G.01.08|2016.07.04| hkn | Dummy-Version wegen Versionsproblem
*-------|----------|-----|---------------------------------------*
*G.01.07|2016.06.16| hkn | Prüfung auf BMP63 ohne Artikel
*-------|----------|-----|---------------------------------------*
*G.01.06|2016.06.15| hkn |G100-PUT-TXILOG70: Enthält die TS-Nachr.
*       |          |     |ein Pac (BMP52) dann den Wert "2 " in
*       |          |     |TXILOG70.CVM-RESULT einstellen
*-------|----------|-----|---------------------------------------*
*G.01.05|2016.05.19| hkn |E100-FEP-ANTWORT: Prüfung MAC-Bilden
*       |          |     |verbessert
*-------|----------|-----|---------------------------------------*
*G.01.04|2016.05.18| hkn |Leer-Vesion wg. Rückbau SIP-Verarb.
*-------|----------|-----|---------------------------------------*
*G.01.03|2016.03.23| kl  |Erweiterung um konfigurierbare BIN-
*       |          |     |Sperre (aktivierbar ja Mandant/Cardid)
*-------|----------|-----|---------------------------------------*
*G.01.02|2016.03.14| hkn | Korrektur: K-BYTEMAP-A1100 BMP63
*       |          |     | zurück gesetzt
*-------|----------|-----|---------------------------------------*
*G.01.01|2016.03.11| hkn | Korrektur: Fehler in K-BYTEMAP-A1100
*       |          |     | BMP63 nicht gesetzt, also keine Prüfung
*       |          |     | durch den Artikelmapper für BMP 63
*-------|----------|-----|---------------------------------------*
*G.01.00|2016.03.10| hkn | Korrekturen wg. fehlender Artikel in
*       |          |     | Reation Terminalmessage zu =ARTMAP
*       |          |     | und Rückgabe AC = 45 ans Terminal
*-------|----------|-----|---------------------------------------*
*G.00.00|2015-05-27| BAH | Neuerstellung
*----------------------------------------------------------------*
*
* Programmbeschreibung
* --------------------
*
* Das Programm setzt Flottenkarten-Terminal-Vor-Autorisierungs-Anfragen
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
*    -   ROADRUNNER
*
* Für jedes AS muss eine eigene Serverklasse mit diesem Programm
* definiert werden (z.B. PFCPRE7S-05 für Avia). Die Keys für das
* jeweilige AS werden in der Tabelle =KEYNAMEN erwartet.
*
* Das Programm erwartet die folgenden Parameter:
*
*    -   AS-ROUTKZ   Routkennzeichen für das relevante AS
*    -   MACKEYT     MAC-Schlüssel für Terminaltransaktionen
*    -   PACKEYT     PAC-Schlüssel für Terminaltransaktionen
*    -   BOXMON      Festlegung des Boxenservers
*    -   ARTMAP      Festlegung des zuständigen Artikel-Mappers
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
* D324-STIGLECHNER
* D325-ROADRUNNER
* D900-GET-CARDID
* D910-GET-ASTRACENR
* D950-EMV-VERARBEITUNG
*
* E100-FEP-ANTWORT
* E305-01-ARTIKELDATEN
* E310-BMP48-DEFAULT
* E900-PUT-ERRLOG
*
* F100-LOGDATEN-EIGEN
* F910-MAC-PRUEFEN
* F920-MAC-BILDEN
* F930-PAC-UMSCHLUESSELN

*G.01.18 - Anfang
* F940-PAC-NACH-DUKPT
* F950-ASMAC-DUKPT
*G.01.18 - Ende

*
* G100-PUT-TXILOG70
* G110-PUT-TXNLOG70-TS
* G115-PUT-TXNLOG70-TS-ANF-ANT
* G120-PUT-TXNLOG70-AS
* G130-PUT-CRDUSEDN
*
* L100-ADD-BMP
* L110-COB2ISO
*
* M100-CALL-SDBCDU5
* M110-CALL-SYSMK7I
* M120-CALL-WCAPM92
* M130-CALL-WISO207
* M140-CALL-WSY7066
* M150-CALL-WSYS930
* M160-CALL-WSYS971
* M170-CALL-SYSAWKZ
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
* S130-SELECT-POST70
* S140-UPDATE-POST70
* S150-SELECT-TSKART40
* S160-SELECT-STATION
* S170-SELECT-STATIONA
* S180-INSERT-TXILOG70
* S190-INSERT-TXNLOG70-TS
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
     05      C4-ANZ              PIC S9(04) COMP VALUE ZEROS.
     05      C4-COUNT            PIC S9(04) COMP VALUE ZEROS.
     05      C4-I1               PIC S9(04) COMP VALUE ZEROS.
     05      C4-I2               PIC S9(04) COMP VALUE ZEROS.
     05      C4-I3               PIC S9(04) COMP VALUE ZEROS.
     05      C4-I4               PIC S9(04) COMP VALUE ZEROS.
     05      C4-LEN              PIC S9(04) COMP VALUE ZEROS.
     05      C4-PTR              PIC S9(04) COMP VALUE ZEROS.

     05      C4-X.
      10                         PIC X value low-value.
      10     C4-X2               PIC X.
     05      C4-NUM redefines C4-X
                                 PIC S9(04) COMP.

     05      C9-ANZ              PIC S9(09) COMP VALUE ZEROS.
     05      C9-COUNT            PIC S9(09) COMP VALUE ZEROS.

     05      C18-VAL             PIC S9(18) COMP VALUE ZEROS.

     05      REPLY-LAENGE        PIC  9(04) COMP VALUE ZEROS.

*--------------------------------------------------------------------*
* Display-Felder: Präfix D
*--------------------------------------------------------------------*
 01          DISPLAY-FELDER.
     05      D-NUM1              PIC  9 VALUE ZEROS.
     05      D-NUM2              PIC  9(02) VALUE ZEROS.
     05      D-NUM20             PIC  9(02) VALUE ZEROS.
     05      D-NUM21             PIC  9(02) VALUE ZEROS.
     05      D-NUM3              PIC  9(03) VALUE ZEROS.
     05      D-NUM4              PIC -9(04) VALUE ZEROS.
     05      D-NUM4M             PIC  9(04) VALUE ZEROS.
     05      D-NUM4N             PIC  9(04) VALUE ZEROS.
     05      D-NUM4OV            PIC  9(04) VALUE ZEROS.
     05      D-NUM6              PIC  9(06) VALUE ZEROS.
     05      D-NUM61             PIC  9(06) VALUE ZEROS.
     05      D-NUM9              PIC  9(09) VALUE ZEROS.
     05      D-NUM12             PIC  9(12) VALUE ZEROS.
     05      D-NUM18             PIC  9(18) VALUE ZEROS.

*--------------------------------------------------------------------*
* Felder mit konstantem Inhalt: Präfix K
*--------------------------------------------------------------------*
 01          KONSTANTE-FELDER.
     05      K-MODUL             PIC X(08)          VALUE "PFCPRE7S".

**          ---> Pflichtfelder einer 1100-AS-Nachricht
*G.06.31 - Anfang
*    05      K-BYTEMAP-A1100      PIC X(64) VALUE
*    "0011001000110000000001010100000100100000010000011000000000100000".
**
     05      K-BYTEMAP-A1100      PIC X(64) VALUE
     "0011001000110000000001010100000100100000110000011000000000100000".
*G.06.31 - Ende
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

     05      MSG-STATUS          PIC 9       VALUE ZEROS.
          88 MSG-OK                          VALUE ZEROS.
          88 MSG-EOF                         VALUE 1.

     05      PRG-STATUS          PIC 9       VALUE ZEROS.
          88 PRG-OK                          VALUE ZEROS.
          88 PRG-NOK                         VALUE 1 THRU 9.
          88 PRG-ENDE                        VALUE 1.
          88 PRG-ABBRUCH                     VALUE 2.

     05      ENDE-FLAG           PIC 9       VALUE ZEROS.
          88 ENDE-OFF                        VALUE ZEROS.
          88 ENDE                            VALUE 1.

     05      FCPARAM-FLAG         PIC 9       VALUE ZEROS.
          88 FCPARAM-OK                       VALUE ZEROS.
          88 FCPARAM-EOD                      VALUE 1.
          88 FCPARAM-NOK                      VALUE 9.

     05      KEYNAMEN-FLAG       PIC 9       VALUE ZEROS.
          88 KEYNAMEN-OK                     VALUE ZEROS.
          88 KEYNAMEN-EOD                    VALUE 1.
          88 KEYNAMEN-NOK                    VALUE 9.

     05      MDNR2AS-FLAG        PIC 9       VALUE ZEROS.
          88 MDNR2AS-OK                      VALUE ZEROS.
          88 MDNR2AS-NOK                     VALUE 1.

     05      POST70-FLAG         PIC 9       VALUE ZEROS.
          88 POST70-OK                       VALUE ZEROS.
          88 POST70-NOK                      VALUE 1.

     05      TSKART40-FLAG       PIC 9       VALUE ZEROS.
          88 TSKART40-OK                     VALUE ZEROS.
          88 TSKART40-NOK                    VALUE 1.

     05      STATION-FLAG        PIC 9       VALUE ZEROS.
          88 STATION-OK                      VALUE ZEROS.
          88 STATION-NOK                     VALUE 1.

     05      STATIONA-FLAG       PIC 9       VALUE ZEROS.
          88 STATIONA-OK                     VALUE ZEROS.
          88 STATIONA-NOK                    VALUE 1.

     05      TXILOG70-FLAG       PIC 9       VALUE ZEROS.
          88 TXILOG70-OK                     VALUE ZEROS.
          88 TXILOG70-NOK                    VALUE 1.

     05      TXNLOG70-FLAG       PIC 9       VALUE ZEROS.
          88 TXNLOG70-OK                     VALUE ZEROS.
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
*G.06.34 - Erfassungsart fuer Chip
          88 ERF-CHIP                      VALUE "3".
*G.06.34 - Ende

     05      PAC-FLAG            PIC X     VALUE LOW-VALUE.
          88 PAC-YES                       VALUE LOW-VALUE.
          88 PAC-NO                        VALUE HIGH-VALUE.

     05      MAC-FLAG            PIC X     VALUE LOW-VALUE.
          88 MAC-YES                       VALUE LOW-VALUE.
          88 MAC-NO                        VALUE HIGH-VALUE.

     05      GEODATA-FLAG        PIC X     VALUE LOW-VALUE.
          88 GEODATA-NO                    VALUE LOW-VALUE.
          88 GEODATA-YES                   VALUE HIGH-VALUE.

     05      KFZ-FLAG            PIC X     VALUE LOW-VALUE.
          88 KFZ-NO                        VALUE LOW-VALUE.
          88 KFZ-YES                       VALUE HIGH-VALUE.

     05      FEP-ANTWORT-FLAG    PIC X     VALUE LOW-VALUE.
          88 NO-FEP-ANTWORT                VALUE LOW-VALUE.
          88 FEP-ANTWORT                   VALUE HIGH-VALUE.

     05      BSP-FLAG            PIC 9     VALUE ZERO.
          88 BS-OFF                        VALUE ZERO.
          88 BS-ON                         VALUE 1.
          88 BS-ERR                        VALUE 9.

*--------------------------------------------------------------------*
* weitere Arbeitsfelder
*--------------------------------------------------------------------*
**          ---> unverändert
 01          WORK-FELDER.
     05      W-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
     05      W-KEYNAME           PIC  X(08).
     05      W-ISOGEN-VERS       PIC  X(02).

**          ---> werden bei jeder Tx initiert
 01          WORK-INIT.
     05      W-CARDID            PIC S9(04)    COMP VALUE ZEROS.
     05      W-KANR-LEN          PIC S9(04)    COMP VALUE ZEROS.
*G.06.34 - Anfang
     05      W-BMP55-LEN         PIC S9(04)    COMP VALUE ZEROS.
*G.06.34 - Ende
     05      W18-BETRAG          PIC S9(16)V99 COMP VALUE ZEROS.
     05      W-BMP07             PIC 9(10)          VALUE ZEROS.
     05      W-ZP-VERKAUF        PIC S9(18)    COMP VALUE ZEROS.
*G.07.01 - AIID
     05      W-AIID              PIC X(11).
*G.07.01 - Ende

     05      W-ACX.
      10     W-AC                PIC 9(02) VALUE ZEROS.
     05      W-ACQUIRER-ID       PIC X(06).
     05      W-AS-TRACENR        PIC 9(06) VALUE ZEROS.
     05      W-ABL               PIC 9(04) VALUE ZEROS.
     05      W-MDNR              PIC 9(08) VALUE ZEROS.
     05      W-TSNR              PIC 9(08) VALUE ZEROS.

     05      W-MEMLOG-KEY.
      10     W-TERMNR            PIC 9(08) VALUE ZEROS.
      10     W-TRACENR           PIC 9(06) VALUE ZEROS.
      10     W-NTYPE             PIC 9(04) VALUE ZEROS.
      10     W-ABWKZ             PIC 9(06) VALUE ZEROS.

     05      W-LTGIND            PIC 9(04) VALUE ZEROS.
     05      W-BETRAG            PIC 9(18) VALUE ZEROS.
     05      W-ERFASSUNGS-ART    PIC 9(02) VALUE ZEROS.
          88 W-ERF-MANUELL                   value 01.
          88 W-ERF-MAGNET                    value 02.
          88 W-ERF-CHIP                      value 05.
          88 W-ERF-KONTAKTLOS                value 07 91.

     05      W-TRANS-ART         PIC X(02).
     05      W-KANR              PIC X(19).
*G.06.34 - Anfang
     05      W-BMP55             PIC X(512).
*G.06.34 - Ende
     05      W-WKZ               PIC 9(03) VALUE ZEROS.
     05      W-SPUR2             PIC X(40).
     05      W-BUFFER            PIC X(128).
     05      W-BUFFER-LEN        PIC S9(04) comp VALUE ZEROS.
     05      W-BUFFER-AKT        PIC S9(04) comp VALUE ZEROS.

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
     05      INLINE-SERVICE      PIC S9(04) COMP VALUE ZEROS.
          88 USE-WXAMP                           VALUE 20.

 01          GEO-BUFFER.
     05      GEO-KZ-BREITE       PIC X.
     05      GEO-BREITE-8        PIC 9(08) VALUE ZEROS.
     05                          PIC X  VALUE SPACE.
     05      GEO-KZ-LAENGE       PIC X.
     05      GEO-LAENGE-9        PIC 9(09) VALUE ZEROS.
 01          GEO-FAKTOR          PIC S9(18) COMP VALUE 1000000.

**          ---> Bereiche für IFSF-BMP48
 01          W-BYTEMAP-48        PIC X(64) VALUE
     "0000000000000000000000000000000000000000000000000000000000000000".
 01          W-BITMAP            PIC X(08)  VALUE LOW-VALUES.
 01          W-BMP48-VAL         PIC X(128) VALUE SPACES.
 01          W-48-LEN            PIC 9(03)  VALUE ZEROS.

**  ---> Zwischenfelder fuer Fregat
 01          W-FRE.
     05      W-FRE-TERMID        PIC X(16).
     05      W-FRE-MONNAME       PIC X(16).
     05      W-FRE-DATLEN        PIC S9(04) COMP VALUE ZEROS.

*kl20170929 - G.06.42 - Hilfsbuffer fuer FREHEADER-Modul
 01 W-FH-BUFFER.
      05 FH-STRING PIC X(256).
*        Aktuelle Länge = 88; Mehr für Zukunftssicherheit
      05 FH-STRLEN PIC S9(04) COMP.
*kl20170929 - G.06.42 - Ende

*--------------------------------------------------------------------*
* Datm-Uhrzeitfelder (für TAL-Routine)
*--------------------------------------------------------------------*
 01          TAL-TIME.
     05      TAL-JHJJMMTT.
      10     TAL-JHJJ            PIC S9(04) COMP VALUE ZEROS.
      10     TAL-MM              PIC S9(04) COMP VALUE ZEROS.
      10     TAL-TT              PIC S9(04) COMP VALUE ZEROS.
     05      TAL-HHMI.
      10     TAL-HH              PIC S9(04) COMP VALUE ZEROS.
      10     TAL-MI              PIC S9(04) COMP VALUE ZEROS.
     05      TAL-SS              PIC S9(04) COMP VALUE ZEROS.
     05      TAL-HS              PIC S9(04) COMP VALUE ZEROS.
     05      TAL-MS              PIC S9(04) COMP VALUE ZEROS.

 01          TAL-TIME-D.
     05      TAL-JHJJMMTT.
        10   TAL-JHJJ            PIC  9(04) VALUE ZEROS.
        10   TAL-MM              PIC  9(02) VALUE ZEROS.
        10   TAL-TT              PIC  9(02) VALUE ZEROS.
     05      TAL-HHMI.
        10   TAL-HH              PIC  9(02) VALUE ZEROS.
        10   TAL-MI              PIC  9(02) VALUE ZEROS.
     05      TAL-SS              PIC  9(02) VALUE ZEROS.
     05      TAL-HS              PIC  9(02) VALUE ZEROS.
     05      TAL-MS              PIC  9(02) VALUE ZEROS.
 01          TAL-TIME-N REDEFINES TAL-TIME-D.
     05      TAL-TIME-N16        PIC  9(16).
     05      TAL-TIME-REST       PIC  9(02).

 01          TAL-JUL-DAY         PIC S9(09) COMP VALUE ZEROS.

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
 01          P-CC-LEN            PIC S9(04) COMP VALUE ZEROS.
 01          P-CC-IN             PIC X(64).
 01          P-CC-OUT            PIC X(64).

**          ---> für WT^HEX und WT^UNHEX Routinen
 01          P-HEX8              PIC X(08).
 01          P-HEX16             PIC X(16).

**          ---> für COBOL-Utilities GET-/PUT-STARTUPTEXT
**          --->                     GET-/PUT-PARAMTEXT
 01          STUP-PARAMETER.
     05      STUP-RESULT         PIC S9(04) COMP VALUE ZEROS.
     05      STUP-CPLIST         PIC  9(09) COMP VALUE ZEROS.
     05      STUP-PORTION        PIC  X(30) VALUE "STRING".
     05      STUP-TEXT           PIC X(128).

**          ---> Holen des eigenen Pathwaysystems
 01          PAIRINFO            PIC S9(04) COMP VALUE ZEROS.
 01          FEHL                PIC S9(04) COMP VALUE ZEROS.

**          ---> Holen des Prozessnamens
 01          PNAME               PIC  X(47).
 01          PNAMELEN            PIC S9(04) COMP VALUE ZEROS.
 01          PROC-INFO           PIC  X(18) VALUE SPACE.


 EXTENDED-STORAGE SECTION.

*--------------------------------------------------------------------*
* weitere Buffer
*--------------------------------------------------------------------*
**          ---> Mapping ROUTKZ <-> APPL_KZ.IFSFAC
**          --->
**          ---> hier muss ggf. bei weiteren AS'sen erweitert werden
*G.07.01 - Refactoring fuer AS-Verfahren
 01          VERF-AS            PIC 9(02) VALUE ZEROS.
*G.07.01 - Ende
          88 VERF-AG                         VALUE 15.
          88 VERF-AV                         VALUE 05.
          88 VERF-BP                         VALUE 14.
          88 VERF-DK                         VALUE 12.
*G.01.14 - Anfang
          88 VERF-EU                         VALUE 22.
*G.01.14 - Ende

*G.06.48 - Anfang
          88 VERF-IQ                         VALUE 24.
*G.06.48 - Ende

*G.01.17 - Anfang
          88 VERF-LO                         VALUE 23.
*G.01.17 - Ende
          88 VERF-NF                         VALUE 99.
          88 VERF-OR                         VALUE 16.
          88 VERF-SH                         VALUE 07.
          88 VERF-TN                         VALUE 18.
          88 VERF-TO                         VALUE 10.
          88 VERF-UT                         VALUE 17.
*G.06.55 - Verfahren Roadrunner neu
          88 VERF-RR                         VALUE 25.
*G.06.55 - Ende

**          ---> Verfahrensfestlegung für Artikelmapper
**          ---> AG, AV und TN sind gleich (werden wie AG behandelt)
 01          AS-VERF             PIC X(02).
          88 AS-VERF-AG                      VALUE "AG".
          88 AS-VERF-AV                      VALUE "AV".
          88 AS-VERF-BP                      VALUE "BP".
          88 AS-VERF-DK                      VALUE "DK".
*G.01.14 - Anfang
          88 AS-VERF-EU                      VALUE "EU".
*G.01.14 - Ende

*G.06.48 - Anfang
          88 AS-VERF-IQ                      VALUE "IQ".
*G.06.48 - Ende

*G.01.17 - Anfang
          88 AS-VERF-LO                      VALUE "LO".
*G.01.17 - Ende
          88 AS-VERF-OR                      VALUE "OR".
          88 AS-VERF-SH                      VALUE "SH".
          88 AS-VERF-TN                      VALUE "TN".
          88 AS-VERF-TO                      VALUE "TO".
          88 AS-VERF-UT                      VALUE "UT".
*G.06.55 - Verfahren Roadrunner neu
          88 AS-VERF-RR                      VALUE "RR".
*G.06.55 - Ende

          88 AS-VERF-DEFAULT                 VALUE "AG".

**          ---> Buffer für Log-Prozess
 01          MEMLOG-SATZ.
     05      MEM-TXILOG70        PIC X(3300).
     05      MEM-TXNLOG70-TS     PIC X(3300).
     05      MEM-TXNLOG70-AS     PIC X(3300).

**          ---> Parametertabelle für Autorisierungssystem
**          ---> wird im Programmvorlauf geladen, d.h. bei Änderungen
**          ---> muss das Programm (Serverklasse) neu gestartet werden


*kl20180405 - G.06.51 - Sieht gut aus, ist aber hier nicht angebracht
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
*kl20180405 - G.06.51. - Ende

**          ---> zu suchende Werte
 01          S-SEARCH-KEY.
     05      S-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
     05      S-CARDID            PIC S9(04) COMP VALUE ZEROS.
     05      S-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
     05      S-KZ-MSG            PIC  X(02).
     05      S-BMP               PIC S9(04) COMP VALUE ZEROS.
     05      S-LFDNR             PIC S9(04) COMP VALUE ZEROS.

*kl20180405 - G.06.51 - Zusätzlicher Searchkey für CARDID = 0
*                       (wird aus S-SEARCH-KEY gefüllt; dann wird
*                        ledglich S2-CARDID mit ZERO überschrieben)
 01          S2-SEARCH-KEY.
     05      S2-ROUTKZ            PIC S9(04) COMP VALUE ZEROS.
     05      S2-CARDID            PIC S9(04) COMP VALUE ZEROS.
     05      S2-ISONTYP           PIC S9(04) COMP VALUE ZEROS.
     05      S2-KZ-MSG            PIC  X(02).
     05      S2-BMP               PIC S9(04) COMP VALUE ZEROS.
     05      S2-LFDNR             PIC S9(04) COMP VALUE ZEROS.
*kl20180405 - G.06.51 - Ende

*G.07.01 - Tabelle auf 150 vergroessert
**          ---> AS-Keytabelle
 01          TK-KEYNAMEN.
     05      TK-KEYNAMEN-TABELLE occurs 150.
      10     TK-ROUTKZ           PIC S9(04) VALUE ZEROS.
      10     TK-CARDID           PIC S9(04) VALUE ZEROS.
      10     TK-KEYNAME          PIC X(08).
      10     TK-ISOGEN           PIC X(02).
      10     TK-ISOVERS          PIC X(02).
      10     TK-HEXKEY           PIC X(04).
      10     TK-HEXISO           PIC X(02).
*G.07.01 - AIID hier mit speichern
      10     TK-AIID             PIC X(11).

 01          TK-MAX              PIC S9(04) COMP VALUE ZEROS.
 01          TK-TAB-MAX          PIC S9(04) COMP VALUE 150.
*G.07.01 - Ende

**          --->
 01          W-TEILSTRING-TABELLE.
     05      W-TEILSTRING-TAB    occurs 10.
      10     W-TEILSTRING        PIC X(64).

 01          W-DELIM-TABELLE.
     05      W-DELIM-TAB         occurs 10.
      10     W-DELIM             PIC X(01).

 01          W-COUNT-TABELLE.
     05      W-COUNT-TAB         occurs 10.
      10     W-COUNT             PIC S9(04) COMP VALUE ZEROS.

**          ---> zum Metadaten dieses Prozesses
 01          MY-META.
   05        MYPROG.
    10       SYSTEM              PIC X(08).
    10       VOL                 PIC X(08).
    10       SUBVOL              PIC X(08).
    10       MODUL               PIC X(08).
   05        PROC-INFO           PIC X(18).
   05        ANCNAME             PIC X(10).
   05        MY-SRV-ID           PIC S9(04) COMP VALUE ZEROS.
   05        MY-SRV-CLASS        PIC X(16)   VALUE " ".

**          ---> fuer Pathsend (serverclass_send_)
 01          PS-PATHNAME         PIC X(15).
 01          PS-PATHNAMELEN      PIC S9(04) COMP VALUE 15.
 01          PS-SRVCLASS         PIC X(15).
 01          PS-SRVCLASSLEN      PIC S9(04) COMP VALUE 15.
 01          PS-REQLEN           NATIVE-2.
 01          PS-MAXREPLEN        NATIVE-2.
 01          PS-AKTREPLEN        NATIVE-2.
 01          PS-TIMEOUT          PIC S9(04) COMP VALUE ZEROS.
 01          PS-ERROR            PIC S9(04) COMP VALUE ZEROS.

*           ---> fuer Pathsend (serverclass_send_info)
 01          SSI-ERROR           PIC S9(04) COMP VALUE ZEROS.
 01          SSI-PSERROR         PIC S9(04) COMP VALUE ZEROS.
 01          SSI-FSERROR         PIC S9(04) COMP VALUE ZEROS.

**          ---> für TAL-Modul WT^HEX^STRING
 01          WTHEXS.
     05      WTHEXS-SRC          PIC X(4096).
     05      WTHEXS-SRC-LEN      PIC S9(04) COMP VALUE ZEROS.
     05      WTHEXS-DST          PIC X(4096).
     05      WTHEXS-DST-LEN      PIC S9(04) COMP VALUE ZEROS.

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

*G.06.38 - Tag Handling
**          ----> fuer Tag Handling
     COPY    WISO400C OF "=MSGLIB"
             REPLACING =="*"== BY ==TF==.
*G.06.38 - Ende

**          ---> Schnittstelle zu SYSMK7I
     COPY    SYSML7IC    OF "=MSGLIB"
             REPLACING =="*"== BY ==MEM==.

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

**          ---> Schnittstelle zum Identifikationsmodul WSYS980
     COPY    WSYS980C   OF "=MSGLIB"
             REPLACING =="*"== BY ==ID==.

*kl20160323 - G.01.03 - Fuer Pruefung BIN-Sperre
**          ---> Schnittstelle zu SYSAWKZ
     COPY    PFCBNS7C OF "=MSGLIB"
             REPLACING =="*"== BY ==BS==.
*kl20160323 - G.01.03 - Ende

*G.01.18 - Anfang
**          ---> Für Boxen-interface
     COPY    WEUR056C OF  "=MSGLIB"
             REPLACING =="*"== BY ==Z==.
*G.01.18 - Ende

*G.06.40 - Anfang
**          ---> fuer Zwischenschichtmodul WISO400 zum BER-TLV / KAAI-LTV
     COPY    WISO400C    OF "=MSGLIB"
             REPLACING =="*"== BY ==W400==.
*G.06.40 - Ende

*G.06.52
      COPY    ZPVERKAUF-IFC OF "=MSGLIB".
*G.06.52 - ende

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
     05      H-SYSKEY            PIC S9(18) COMP VALUE ZEROS.
     05      H-ZP-IN             PIC X(22).
     05      H-ZP-OUT            PIC X(22).

******************************************************************
* Im Folgenden mit dem INVOKE-Befehl die Tabellenstruktur-
* definitonen der benötigten Tabellen einfügen
******************************************************************
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

**  ---> Struktur der Tabelle POST70
 EXEC SQL
    INVOKE =POST70   AS POST70
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

**  --->  Transaktionslog Nachrichten der Station
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-TS
 END-EXEC

**  --->  Transaktionslog Nachrichten zum/vom AS
 EXEC SQL
    INVOKE =TXNLOG70 AS TXNLOG70-AS
 END-EXEC

*kl20160323 - G.01.03 - Fuer Kontrolle,
*                       ob Prüfung auf BIN-Sperre erforderlich
**  --->  Binsperrenkonfiguration
 EXEC SQL
    INVOKE =BINSPCFG AS BINSPCFG
 END-EXEC
*kl20160323 - G.01.03 - Ende

*G.07.01 - neue Tabelle für AIID
 EXEC SQL
    INVOKE =FCAIID AS FCAIID
 END-EXEC
*G.07.01 - Ende

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
*G06.29 - Anfang
            AND   APPKZ = "R7"
*G06.29 - Anfang

*kl20180405 - G.06.51 - wg. Prioriseirung CARDID=X vor CARDID=0
*          ORDER  BY ROUTKZ, CARDID, ISONTYP, BMP, LFDNR
          ORDER  BY ROUTKZ          ASC,
                    CARDID          DESC,
                    ISONTYP         ASC,
                    BMP             ASC,
                    LFDNR           ASC
*kl20180405 - G.06.51 - Ende

         BROWSE  ACCESS
 END-EXEC

**  ---> Cursor auf Tabelle KEYNAMEN
 EXEC SQL
     DECLARE KEYNAMEN_CURS CURSOR FOR
         SELECT   ROUTKZ, CARDID, KEYNAME, ISOGEN, ISOVERS
           FROM  =KEYNAMEN
*G.07.01 - alle laden
*          WHERE   ROUTKZ = :ROUTKZ of KEYNAMEN
         ORDER  BY ROUTKZ, CARDID
*G.07.01 - Ende
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

*G.07.01 - neuen Parameter AS-VERF laden und nicht mehr AS-ROUTKZ
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


**  ---> holen Parameter für zuständiges AS
     MOVE STUP-TEXT (1:STUP-RESULT) TO VERF-AS
                                       ROUTKZ of FCPARAM
*                                       W-ROUTKZ
                                       S-ROUTKZ
***                                    ---> für Artikelmapper
*                                       VERF-ROUTKZ
*G.07.01 - Ende

**  ---> Anwendung setzen für Artikelmapper
**  ---> (die auf Kommentar gesetzten sind default (AG))
     EVALUATE TRUE

*        WHEN VERF-AG    SET AS-VERF-AG TO TRUE
*        WHEN VERF-AV    SET AS-VERF-AV TO TRUE
         WHEN VERF-BP    SET AS-VERF-BP TO TRUE
         WHEN VERF-DK    SET AS-VERF-DK TO TRUE
         WHEN VERF-OR    SET AS-VERF-OR TO TRUE
         WHEN VERF-SH    SET AS-VERF-SH TO TRUE
*        WHEN VERF-TN    SET AS-VERF-TN TO TRUE
         WHEN VERF-TO    SET AS-VERF-TO TO TRUE
         WHEN VERF-UT    SET AS-VERF-UT TO TRUE
         WHEN VERF-LO    SET AS-VERF-LO TO TRUE
         WHEN VERF-EU    SET AS-VERF-EU TO TRUE
*G.07.02 - auch spezielles Verfahren fuer Roadrunner
         WHEN VERF-RR    SET AS-VERF-RR TO TRUE
*G.07.02 - Ende

         WHEN OTHER      SET AS-VERF-DEFAULT TO TRUE

     END-EVALUATE

**  ---> interne Tabelle initialisieren
*kl20180109 - G.06.47 - Initialisieren mit T-TAB-MAX
*                       statt Fixwert 200/500
     MOVE   T-TAB-MAX       TO T-MAX
*kl20180109 - G.06.47 - Ende
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

*kl20180105 - G.06.46 - erlaubtes Maximum in T-TAB-MAX
*     IF  C9-COUNT > T-MAX
     IF  C9-COUNT > T-TAB-MAX
*kl20180105 - G.06.46 - Ende
         MOVE "Programm interne Tabelle zu klein" TO  DATEN-BUFFER1
         MOVE "mehr als 200 Einträge in =FCPARAM " TO  DATEN-BUFFER2
         MOVE ROUTKZ OF FCPARAM TO D-NUM4
         STRING  "für ROUTKZ = "     delimited by size
                 D-NUM4              delimited by size
           INTO  DATEN-BUFFER3
         END-STRING
         MOVE "Programm-Abbruch" TO  DATEN-BUFFER4
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

*G.07.01 - zusätzlich AIID mit in diese Tabelle laden + alle Eintraege aus KEYNAMEN
**  ---> AS Schlüssel MACKEYA und PACKEYA aus Tabelle =KEYNAMEN einlesen
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> !!!! zunächstmal wird nur der erste eingelesen !!!!
**  ---> !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**  ---> ersten Eintrag holen
*     MOVE W-ROUTKZ TO ROUTKZ of KEYNAMEN
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
*G.07.01 - Ende

**  ---> Holen Terminal-Schlüsselnamen
**     > Ungepackten MAC(Terminal)-SchlüsselID holen
     MOVE "MACKEYT" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     MOVE STUP-TEXT(1:STUP-RESULT) TO P-HEX16
     PERFORM P900-WTHEX
     MOVE P-HEX8 TO W-MACKEYT

**     > Ungepackten PAC(Terminal)-SchluesselID holen
     MOVE "PACKEYT" TO STUP-PORTION
     PERFORM P950-GETPARAMTEXT
     IF  PRG-ABBRUCH
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     MOVE STUP-TEXT(1:STUP-RESULT) TO P-HEX16
     PERFORM P900-WTHEX
     MOVE P-HEX8 TO W-PACKEYT

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
*G.07.01 - ROUTKZ von Drehscheibe + Laden Schluessel
     MOVE IMSG-ROUTKZ  TO W-ROUTKZ
*G.07.01 - Ende

**  ---> Kontrolle der Anfrage <---
**  ---> ist die Anfrage evtl. OK?
     PERFORM C100-ANFRAGE-CHECK

** G.01.00 - Anfang
**   IF  ENDE
**       EXIT SECTION
**   END-IF
****
     IF  ENDE
     AND W-AC = ZEROS
         EXIT SECTION
     END-IF
     IF  W-AC NOT = ZERO
**---> Ablehnung senden: Verschiedene Gründe
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF
** G.01.00 - Anfang

**  ---> holen Erweiterungsdate der Station (hier für: GEODATEN)
     PERFORM S170-SELECT-STATIONA

** G.01.00 - Anfang
     IF  ENDE
     AND W-AC = ZEROS
         EXIT SECTION
     END-IF
** G.01.00 - Ende

**  ---> für alle AS'sen gültige Transaktions Regeln
     PERFORM C200-AS-GENERELL

** G.01.00 - Anfang
     IF  ENDE
     AND W-AC = ZEROS
         EXIT SECTION
     END-IF

     IF  W-AC NOT = ZERO
**      ---> Ablehnung senden: Verschiedene Gründe
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF

** G.01.00 - Ende

**  ---> spezielle Regeln für AS'sen
     PERFORM C300-AS-SPEZIELL

** G.01.00 - Anfang
     IF  ENDE
     AND W-AC = ZEROS
         EXIT SECTION
     END-IF

     IF  W-AC NOT = ZEROS
**---> Ablehnung senden: Verschiedene Gründe
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF
** G.01.00 - Ende

**  ---> und hier die AS-Nachricht zusammenbauen
     PERFORM C400-BUILD-AS-NACHRICHT

** G.01.00 - Anfang
**  ---> Check AC
     IF  ENDE
     AND W-AC = ZEROS
         EXIT SECTION
     END-IF

     IF  W-AC NOT = ZEROS
**      ---> Ablehnung senden: 12/96 (s. C200-AS-NACHRICHT)
         PERFORM E100-FEP-ANTWORT
         EXIT SECTION
     END-IF
** G.00.01 - Ende

**  ---> Schreiben der Log-Daten
     PERFORM C500-LOGGING

**  ---> AC 91, wenn MEMLOG nicht OK
     IF  FEP-ANTWORT
**      ---> Ablehnung senden: 91
         MOVE 91 TO W-AC
         PERFORM E100-FEP-ANTWORT
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

**  ---> erstmal termid für ERRLOG aufbereiten
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

**  ---> weitere Felder merken
     MOVE IMSG-MDNR TO MDNR of TXILOG70
                       MDNR of TXNLOG70-TS
                       MDNR of TXNLOG70-AS
                       MDNR of POST70
                       MDNR of STATION
                       MDNR of STATIONA
                       W-MDNR

     MOVE IMSG-TSNR TO TSNR of TXILOG70
                       TSNR of TXNLOG70-TS
                       TSNR of TXNLOG70-AS
                       TSNR of POST70
                       TSNR of STATION
                       TSNR of STATIONA
                       W-TSNR

     MOVE W-TERMNR  TO TERMNR of TXILOG70
                       TERMNR of TXNLOG70-TS
                       TERMNR of TXNLOG70-AS
                       TERMNR of POST70

*G.01.13 - Anfang

     MOVE W-TERMNR (7:2) TO PNR OF POST70
     PERFORM S130-SELECT-POST70

     IF POST70-OK

        IF TERMTYP OF POST70 = SPACES
           MOVE "Terminal-Typ in =POST70 nicht gesetzt:"
             TO DATEN-BUFFER1
           MOVE TERMNR OF POST70 TO DATEN-BUFFER (40:)
           MOVE 45 TO W-AC
           PERFORM Z002-PROGERR
           EXIT SECTION
        END-IF
     ELSE
        EXIT SECTION
     END-IF

*G.01.13 - Ende

**  ---> zunächst mal den MAC prüfen, sofern vorhanden
     SET MAC-NO TO TRUE
     IF  IMSG-TBMP(64) = 1
         SET MAC-YES TO TRUE
         SET W66-EC  TO TRUE
         PERFORM F910-MAC-PRUEFEN
         IF  W-AC > ZERO
             SET ENDE TO TRUE
             EXIT SECTION
         END-IF
     END-IF

**  ---> Station zum Terminal lesen
     PERFORM S160-SELECT-STATION
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> formale Prüfung der Nachricht
     MOVE ZERO TO W-AC
     SET  CHK-CHECK-ALL TO TRUE
     MOVE IMSG-NTYPE    TO CHK-NTYPE
                           W-NTYPE
     MOVE "R7"          TO CHK-ABWKZ(1:2)
     MOVE K-MODUL (2:4) TO CHK-ABWKZ(3:4)
**  ---> soll dann "0200" "R7FCPR" sein

**  ---> formale Prüfung durch Modul WSYS971
     PERFORM M160-CALL-WSYS971
     IF  ENDE or W-AC > ZERO
         EXIT SECTION
     END-IF

**  ---> Zahlung oder Gutschrift
     IF  IMSG-CF(IMSG-TPTR(3):2) = "67"
         SET UMS-ZAHLUNG    TO TRUE
     ELSE
         SET UMS-GUTSCHRIFT TO TRUE
     END-IF

**  ---> Abwicklungs-KZ merken
     MOVE IMSG-CF(IMSG-TPTR(03):IMSG-TLEN(03)) TO W-ABWKZ

**  ---> Betrag aufbereiten und merken (nur für Eigenantwort)
     MOVE IMSG-CF(IMSG-TPTR(04):IMSG-TLEN(04)) TO W-BETRAG
     COMPUTE W18-BETRAG = W-BETRAG / 100


*G.06.52 - Anfang
*
*--> ZP-VERKAUF - Berechnung jetzt über Modul ZPVERK
*
**  ---> Verkaufszeitpunkt aufbereiten (20 + BMP13 + BMP12)
*     MOVE IMSG-CF(IMSG-TPTR(12):IMSG-TLEN(12)) TO D-NUM6
*     MOVE IMSG-CF(IMSG-TPTR(13):IMSG-TLEN(13)) TO D-NUM12
*     COMPUTE D-NUM12      = D-NUM12 * 1000000
*     COMPUTE W-ZP-VERKAUF = (10000000000 * TAL-JHJJ of TAL-TIME-D)
*                          + D-NUM12 + D-NUM6

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
*G.06.52 - Ende

**  ---> holen Cardid
     PERFORM D900-GET-CARDID
     IF  W-AC NOT = ZERO
         EXIT SECTION
     END-IF

**  ---> holen zuständigen Keynamen
     PERFORM D200-FIX-KEY
**  ---> setzen Keyname für diese Transaktion
     MOVE TK-KEYNAME (C4-I1) TO W-KEYNAME
     MOVE TK-HEXISO (C4-I1)  TO W-ISOGEN-VERS

**  ---> Spur 2 auseinander nehmen
     MOVE ZERO TO C4-ANZ
     MOVE 1    TO C4-PTR
     MOVE SPACES     TO W-DELIM-TABELLE
     MOVE LOW-VALUES TO W-COUNT-TABELLE

     SET ERF-SPUR2 TO TRUE
     MOVE IMSG-CF(IMSG-TPTR(35):IMSG-TLEN(35)) TO W-SPUR2
     UNSTRING W-SPUR2
                 delimited by "D" or "="
         INTO W-TEILSTRING (1)  delimiter in W-DELIM (1)
                                count     in W-COUNT (1)
              W-TEILSTRING (2)  delimiter in W-DELIM (2)
                                count     in W-COUNT (2)
         with pointer C4-PTR
         tallying in  C4-ANZ
     END-UNSTRING
     MOVE W-TEILSTRING (1) TO W-KANR
     MOVE W-COUNT (1)      TO W-KANR-LEN
     IF  C4-ANZ > 1
         MOVE W-TEILSTRING (2) (1:4) TO W-ABL
     ELSE
         MOVE ZEROES                 TO W-ABL
     END-IF

**  ---> Erfassungsart (BMP22)
     MOVE IMSG-CF(IMSG-TPTR(22) + 1:2) TO W-ERFASSUNGS-ART
     IF  W-ERFASSUNGS-ART = "01"
         SET ERF-ERROR TO TRUE
         MOVE 30  TO W-AC
         MOVE 2201 TO ERROR-NR of GEN-ERROR
         MOVE "BMP22 zeigt man. Eingabe@" TO DATEN-BUFFER1
         MOVE "bei Automaten nicht korrekt" TO DATEN-BUFFER2
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF

**G.06.34 - Pruefung, ob Chipdaten, bei BMP35
     IF IMSG-TBMP(55) = 1
        SET ERF-CHIP TO TRUE
        MOVE IMSG-TLEN(55) TO W-BMP55-LEN
        MOVE IMSG-CF(IMSG-TPTR(55):IMSG-TLEN(55)) TO W-BMP55
     END-IF
**G.06.34 - Ende

*G.06.34 - Anfang
     IF      (W-ERFASSUNGS-ART = "02"
     AND NOT  ERF-SPUR2)
     OR      (W-ERFASSUNGS-ART = "05"
     AND NOT  ERF-CHIP)
         SET ERF-ERROR TO TRUE
         MOVE 30 TO W-AC
         MOVE 2201 TO ERROR-NR of GEN-ERROR
         MOVE "Kombination aus:@"
           TO DATEN-BUFFER1
         MOVE "BMP 22 und BMP 35, 55 nicht korrekt"
           TO DATEN-BUFFER2
         PERFORM Z002-PROGERR
         EXIT SECTION
     END-IF
*G.06.34 - Ende

*kl20160323 - G.01.03 - BIN-Sperre
**  ---> Behandlung BIN-Sperre
     SET BS-OFF      TO TRUE
     PERFORM S200-SELECT-BINSPCFG
     IF BS-ON
        PERFORM C101-BIN-SPERRE
        IF  W-AC NOT = ZERO
            EXIT SECTION
        END-IF
     END-IF
*kl20160323 - G.01.03 - Ende

**  ---> Währungs-KZ könnte hier gegen Tabelle WKZKURS geprüft werden
     MOVE IMSG-CF (IMSG-TPTR (49) + 1:3) TO W-WKZ
     MOVE W-WKZ TO WKZ-WKZ
     SET WKZ-CMD-WKZ-A TO TRUE
     PERFORM M170-CALL-SYSAWKZ
     IF  WKZ-ERR
         EXIT SECTION
     END-IF

**  ---> Artikeldaten müssen vorhanden sein
     IF  IMSG-TBMP(63) = 1
         MOVE IMSG-CF(IMSG-TPTR(63) + 32:1)  TO P-HEX8
         PERFORM P910-WTUNHEX
*G.01.07 - Anfang - BMP63 ohne Artikel
*        IF  P-HEX16(1:2) = "00"
**
         IF  IMSG-TLEN(63) = 33
*G.01.07 - Ende

             MOVE 30   TO W-AC
             MOVE 2201 TO ERROR-NR of GEN-ERROR
             MOVE "Autorisierung OHNE Artikel@" TO DATEN-BUFFER1
             PERFORM Z002-PROGERR
             EXIT SECTION
         END-IF
     END-IF

** G.01.01 - Anfang

** BMP 63 - Artikel über Artikel-Mapper
** Die Artikel werden nur geprüft und nicht an den AS geschickt, da
** die IFSF-1100 kein BMP 63 enthält.
**
** Die Prüfung ist jedoch notwendig, damit vom Terminal nur die
** erlaubten Artkel geschickt werden, um Vorort keine falschen
** Erwartungen zu wecken

     IF  IMSG-TBMP(63) = 1
         PERFORM E305-01-ARTIKELDATEN
         IF  not AMP-OK
**---> Fehler vom Artikel-Mapper
             MOVE AMP-RC TO D-NUM4
             STRING  "RC aus Artikelmapping: "
                     D-NUM4
                         delimited by size
               INTO  DATEN-BUFFER1
             END-STRING
             PERFORM Z002-PROGERR
             IF  AMP-RC = 100
**---> Ablehnung senden: Ungültige Transaktion
                 MOVE 45 TO W-AC
             ELSE
**---> Ablehnung senden: Verarbeitung z.Zt. nicht möglich
                 MOVE 96 TO W-AC
             END-IF
             EXIT SECTION
         END-IF

      MOVE ZERO TO W-48-LEN
      SET KFZ-NO TO TRUE
      IF  AMP-BMP48-FLAG = 1
          SET KFZ-YES TO TRUE
          MOVE AMP-HOST-VAL(1:3)            TO W-48-LEN
          MOVE AMP-HOST-VAL(1:W-48-LEN + 3) TO W-BMP48-VAL
          ADD  3                            TO W-48-LEN
      END-IF

     END-IF

** G.01.01 - Ende

**  ---> online PIN erforderlich? Dann BMP 52,53,57,64 Pflicht
**  ---> dafür nachsehen, ob online PIN Pflicht ist
     SET PAC-NO TO TRUE
     MOVE 100  TO S-ISONTYP
     MOVE "TS" TO S-KZ-MSG
     MOVE 52   TO S-BMP
     MOVE 1    TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         IF  T-KZ-ABWEICHUNG (T-AKT-IND) = "1"
             SET PAC-YES TO TRUE
             IF  not (IMSG-TBMP(52) = 1 and IMSG-TBMP(53) = 1 and
                      IMSG-TBMP(57) = 1 and IMSG-TBMP(64) = 1)
                 MOVE 30   TO W-AC
                 MOVE 2201 TO ERROR-NR of GEN-ERROR
                 MOVE "Online PIN erforderlich@" TO DATEN-BUFFER1
                 MOVE "Tx ohne BMP52,53,57 oder 64"   TO DATEN-BUFFER2
                 PERFORM Z002-PROGERR
                 EXIT SECTION
             END-IF
         END-IF
     END-IF

**  ---> wenn in Anfrage PAC (52) auch 53, 57 und 64 Pflicht
     IF  IMSG-TBMP(52) = 1
         SET PAC-YES TO TRUE
         IF  not (IMSG-TBMP(53) = 1 and
                  IMSG-TBMP(57) = 1 and IMSG-TBMP(64) = 1)
             MOVE 30   TO W-AC
             MOVE 2201 TO ERROR-NR of GEN-ERROR
             MOVE "Online PIN gesendet@" TO DATEN-BUFFER1
             MOVE "jedoch ohne BMP53,57 oder 64"   TO DATEN-BUFFER2
             PERFORM Z002-PROGERR
             EXIT SECTION
         END-IF
     END-IF

     IF IMSG-TBMP(55) = 1
        IF  IMSG-TPTR(55) > 0
        AND IMSG-TLEN(55) > 0
*           Uebernehmen Chipdaten (gueltiges BMP 55)
            PERFORM D950-EMV-VERARBEITUNG
            IF  W-AC = ZERO
            AND ENDE-OFF
               CONTINUE
            ELSE
               EXIT SECTION
            END-IF
        ELSE
*           Ungueltiges BMP 55
            INITIALIZE GEN-ERROR
            MOVE "Ungueltiges (PTR/LEN) BMP 55 in Nachricht"
                  TO DATEN-BUFFER1
*           Keine Pruefung auf vorhanden, da interne Arbeitsfelder
            STRING "Bei TERMNR: ",
                    W-TERMNR,
                    " / TRACENR ",
                    W-TRACENR
            DELIMITED BY SIZE INTO DATEN-BUFFER2
*           Speziell hier: BMP 55 ausschalten fuer Antwort
            MOVE ZERO TO IMSG-TBMP(55)
            MOVE 30   TO W-AC
            EXIT SECTION
        END-IF
    END-IF

*G.06.40 - Ende

     .
 C100-99.
      EXIT.

*kl20160323 - G.01.03 - Durchführen Prüfung BIN-Sperre
******************************************************************
* BIN-Sperre fur MD/CARDID gesetzt -> Pruefen
******************************************************************
 C101-BIN-SPERRE SECTION.
 C101-00.

** ---> Erstmal IFC initialisieren und Dummy-AC, AC=000 ist ok!
     INITIALIZE BS-PFCBNS7C

**  --> Besetzen Prueffelder
     MOVE    MDNR    OF TSKART40      TO BS-MDNR
     MOVE    TSNR    OF TSKART40      TO BS-TSNR
     MOVE    CARDSYS OF TSKART40      TO BS-CARDSYS
     MOVE    CARDID  OF TSKART40      TO BS-CARDID
     MOVE    W-KANR                   TO BS-PAN
     MOVE    W-KANR-LEN               TO BS-PANLEN

**  --> Erster Modulaufruf -> Pruefen BIN-Sperre fuer spez. TS
     PERFORM M180-CALL-PFCBNS7

     EVALUATE BS-AC

*       Zero = OK, wenn aber spez. TS weitermachen mit TSNR = 0 (s.u.)
        WHEN ZERO      IF BS-TSNR = ZERO
                          EXIT SECTION
                       ELSE
                          CONTINUE
                       END-IF

*       BIN gesperrt -> Ablehnung
        WHEN 004       MOVE 04 TO W-AC
                       EXIT SECTION

*       Sonstiger Modulfehler -> Dummy-AC steht noch, neuer Versuch mit TSNR=0
        WHEN OTHER     INITIALIZE GEN-ERROR
                       STRING "Pruefung BIN-Sperre fehlgeschlagen - ",
                       BS-AC
                       DELIMITED BY SIZE INTO DATEN-BUFFER1
                       STRING BS-MDNR,
                       " / ",
                       BS-TSNR,
                       " / ",
                       BS-CARDSYS,
                       " / ",
                       BS-CARDID
                       DELIMITED BY SIZE INTO DATEN-BUFFER2
                       STRING "KANR=",
                              BS-PAN
                       DELIMITED BY SIZE INTO DATEN-BUFFER3
                       PERFORM Z002-PROGERR
*                      Noch ein Versuch mit TSNR = ZERO, daher geht's
*                      hier erst mal weiter

     END-EVALUATE

**  --> Zweiter Modulaufruf -> Pruefen BIN-Sperre fuer Mandanten

*    Jetzt fuer TSNR = 0 pruefen (=Mandantenkonfuguration)
     MOVE ZERO TO BS-TSNR
     PERFORM M180-CALL-PFCBNS7

*    Ergebnis?
     EVALUATE BS-AC

        WHEN ZERO       CONTINUE
        WHEN 004        MOVE  04      TO W-AC
        WHEN OTHER      INITIALIZE GEN-ERROR
                        STRING "Pruefung BIN-Sperre fehlgeschlagen - ",
                        BS-AC
                        DELIMITED BY SIZE INTO DATEN-BUFFER1
                        STRING BS-MDNR,
                        " / ",
                        BS-TSNR,
                        " / ",
                        BS-CARDSYS,
                        " / ",
                        BS-CARDID
                        DELIMITED BY SIZE INTO DATEN-BUFFER2
                        STRING "KANR=",
                                BS-PAN
                        DELIMITED BY SIZE INTO DATEN-BUFFER3
                        PERFORM Z002-PROGERR

     END-EVALUATE
     .
 C101-99.
     EXIT.
*kl20160323 - G.01.03 - Ende

******************************************************************
* AS-Anfrageteile, die für alle gleich sind
******************************************************************
 C200-AS-GENERELL SECTION.
 C200-00.
**  ---> im Folgenden noch die Felder BMP 33, 39, 42 hinzufuegen
     MOVE IMSG-COBDATEN TO W207-COBDATEN

**  ---> zunächst die BitMap auf die Pflichtfelder setzen
     MOVE LOW-VALUE       TO W207-TBMP-O
     MOVE K-BYTEMAP-A1100 TO W207-TBMP-O (1:64)

**  ---> Werte für Tabelle =FCPARAM
     MOVE 1100 TO S-ISONTYP
     MOVE "AS" TO S-KZ-MSG

     SET W207-IFSF TO TRUE
**  ---> Nachrichtentyp für Nachricht setzen
     MOVE 1100 TO W207-NTYPE

**  ---> schon mal einen Dummy-MAC vorbereiten
     MOVE 1100 TO S-ISONTYP
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
     STRING  TAL-MM   of TAL-TIME-D
             TAL-TT   of TAL-TIME-D
             TAL-HHMI of TAL-TIME-D
             TAL-SS   of TAL-TIME-D
                 delimited by size
       INTO  W207-XCOBVAL
     END-STRING
**  ---> und den Wert für evtl. Storni sichern
     MOVE W207-XCOBVAL (1:W207-XCOBLEN) TO W-BMP07
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
*G.06.53 - Anhand ZP-VERKAUF (Kombination TS BMP 12/13) AS BMP 12 fuellen
     COMPUTE D-NUM12 = W-ZP-VERKAUF - 20000000000000
     MOVE D-NUM12 TO W207-XCOBVAL
*     STRING  TAL-JHJJMMTT of TAL-TIME-D (3:6)
*             TAL-HHMI     of TAL-TIME-D
*             TAL-SS       of TAL-TIME-D
*                 delimited by size
*       INTO W207-XCOBVAL
*     END-STRING
*G.06.53 - Ende
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 14 - Verfalldatum
     IF  ERF-MANUELL
         MOVE 14 TO W207-XBMP
         MOVE 4  TO W207-XCOBLEN
         MOVE W-ABL TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

**  ---> BMP 22 - Eingabeart
     MOVE 22 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN
*G.07.02 - Start von BMP 22 in FCPARAM definiert
*     MOVE "B10201" TO W207-XCOBVAL
*  ---> BMP 22 - Servicecode
     MOVE 22 TO S-BMP
     MOVE 1  TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-NOT-FOUND
         PERFORM E900-PUT-ERRLOG
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF
     PERFORM U400-INTERPRET-ABWEICHUNG
     MOVE W-BUFFER     TO W207-XCOBVAL
*G.07.02 - Ende
     EVALUATE TRUE
         WHEN W-ERF-MANUELL      MOVE "6" TO W207-XCOBVAL (7:1)
         WHEN W-ERF-MAGNET       MOVE "2" TO W207-XCOBVAL (7:1)
         WHEN W-ERF-CHIP         MOVE "5" TO W207-XCOBVAL (7:1)
         WHEN W-ERF-KONTAKTLOS   MOVE "A" TO W207-XCOBVAL (7:1)
         WHEN OTHER              MOVE "3" TO W207-XCOBVAL (7:1)
     END-EVALUATE
     EVALUATE TRUE
         WHEN PAC-YES            MOVE "1" TO W207-XCOBVAL (8:1)
         WHEN PAC-NO             MOVE "1" TO W207-XCOBVAL (8:1)
         WHEN OTHER              MOVE "6" TO W207-XCOBVAL (8:1)
     END-EVALUATE
     MOVE "4144" TO W207-XCOBVAL (9:4)
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 24 - Funktionscode
     MOVE 24     TO W207-XBMP
     MOVE 03     TO W207-XCOBLEN
     MOVE "101"  TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 26 - Branchencode
     MOVE 26     TO W207-XBMP
     MOVE 04     TO W207-XCOBLEN
     MOVE "5542" TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.07.01 - AIID aus neuer Tabelle FCAIID (geladen bei Start)
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
*G.07.01 - Ende
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> BMP 35 - Spur 2
     MOVE 35            TO W207-XBMP
     MOVE W207-TLEN(35) TO W207-XCOBLEN
     INSPECT W207-CF(W207-TPTR(35):W207-TLEN(35)) CONVERTING "D"
                                                          TO "="
     MOVE W207-CF(W207-TPTR(35):W207-TLEN(35)) TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

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
*G.06.37 - Anfang
     IF IMSG-TBMP(42) = 1
        MOVE IMSG-CF(IMSG-TPTR(42):IMSG-TLEN(42)) TO W207-XCOBVAL
     ELSE
        MOVE VUNR of TSKART40 TO W207-XCOBVAL
     END-IF
*G.06.37 - Anfang

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

**  ---> BMP 52 - PAK (muss umgeschlüsselt werden)
     IF  PAC-YES
         MOVE 52 TO W207-XBMP
         MOVE 08 TO W207-XCOBLEN
*         PERFORM D200-FIX-KEY
         IF  ENDE
             EXIT SECTION
         END-IF

*G.01.18 - Anfang
*G.07.01 - AS-VERF hier verwenden
*         EVALUATE W-ROUTKZ
         EVALUATE VERF-AS
*G.07.01 - Ende
             WHEN 22
                  PERFORM F940-PAC-NACH-DUKPT
             WHEN OTHER
                  PERFORM F930-PAC-UMSCHLUESSELN
         END-EVALUATE
*G.01.18 - Ende

         IF  ENDE
             EXIT SECTION
         END-IF
**      ---> PAC ist original Terminal-Nachricht (IMSG-..) umgeschlüsselt
**      ---> also in W207-Buffer übertragen
         MOVE IMSG-CF(IMSG-TPTR(52):IMSG-TLEN(52)) TO W207-XCOBVAL
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

**  ---> BMP 53 - wenn AS-MAC/PAC
     IF  PAC-YES or MAC-YES
*G.01.18 - Anfang
*G.07.01 - AS-VERF hier verwenden
*        EVALUATE W-ROUTKZ
         EVALUATE VERF-AS
*G.07.01 - Ende
           WHEN 22
                MOVE LOW-VALUE TO W207-XCOBVAL
                IF IMSG-TBMP(57) = 1
                   MOVE IMSG-CF(IMSG-TPTR(57):10 )
                     TO W207-XCOBVAL
                END-IF
                MOVE 10 TO W207-XCOBLEN
           WHEN OTHER
            MOVE W-ISOGEN-VERS TO W207-XCOBVAL
            MOVE 2 TO W207-XCOBLEN
            MOVE LOW-VALUE TO W207-XCOBVAL (W207-XCOBLEN + 1:)
            ADD 16 TO W207-XCOBLEN
            IF  IMSG-TBMP(57) = 1
                 MOVE IMSG-CF(IMSG-TPTR(57) + 2:16)
                   TO W207-XCOBVAL (W207-XCOBLEN + 1:)
            END-IF
            ADD 16 TO W207-XCOBLEN
**      --->  nachsehen, ob noch Suffix gesendet werden soll
            MOVE 53 TO S-BMP
            MOVE 1  TO S-LFDNR
            PERFORM U300-SEARCH-TAB
            IF PRM-FOUND
               PERFORM U400-INTERPRET-ABWEICHUNG
               MOVE W-BUFFER    TO W207-XCOBVAL (W207-XCOBLEN + 1:)
               ADD W-BUFFER-LEN TO W207-XCOBLEN
            END-IF
**      --->  und nun in Nachricht
            MOVE 53 TO W207-XBMP
            PERFORM L100-ADD-BMP
            IF ENDE
               EXIT SECTION
            END-IF
         END-EVALUATE
*G.01.18 - Ende
     END-IF

*G.06.34 - BMP 55 in AS ANFRAGE
     IF ERF-CHIP
        MOVE 1 TO W207-TBMP-O (55:1)
        MOVE 55          TO W207-XBMP
        MOVE W-BMP55-LEN TO W207-XCOBLEN
        MOVE W-BMP55     TO W207-XCOBVAL
        PERFORM L100-ADD-BMP
        IF ENDE
            EXIT SECTION
        END-IF
     END-IF
*G.06.34 - Ende

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

** G.01.01 - Anfang

** BMP 63 - Artikel über Artikel-Mapper
** Die Artikel werden nur geprüft und nicht an den AS geschickt, da
** die IFSF-1100 kein BMP 63 enthält.
**
** Die Prüfung ist jedoch notwendig, damit vom Terminal nur die erlaubten
** Artkel geschickt werden, um Vorort keine falschen Erwartungen zu wecken
**
**   IF  W207-TBMP(63) = 1
**       PERFORM E305-01-ARTIKELDATEN
**       IF  not AMP-OK
**          ---> Fehler vom Artikel-Mapper
**           MOVE AMP-RC TO D-NUM4
**           STRING  "RC aus Artikelmapping: "
**                   D-NUM4
**                       delimited by size
**             INTO  DATEN-BUFFER1
**           END-STRING
**           PERFORM Z002-PROGERR
**           IF  AMP-RC = 100
**              ---> Ablehnung senden: Ungültige Transaktion
**               MOVE 45 TO W-AC
**           ELSE
**              ---> Ablehnung senden: Verarbeitung z.Zt. nicht möglich
**               MOVE 96 TO W-AC
**           END-IF
**           EXIT SECTION
**       END-IF
****
**Artikel-Mapper OK - siehe C100-ANFRAGE-CHECK
**Der Artikelmapper bringt ggf. zunächst Fahrer-/Fahrzeug-
**daten (AMP-BMP48-FLAG = 1), die müssen für BMP48 aufbereitet
**werden(Kfz-Daten für BMP48 werden für Avia nicht gebraucht)
**
**   MOVE ZERO TO W-48-LEN
**   SET KFZ-NO TO TRUE
**   IF  AMP-BMP48-FLAG = 1
**       SET KFZ-YES TO TRUE
**       MOVE AMP-HOST-VAL(1:3)            TO W-48-LEN
**       MOVE AMP-HOST-VAL(1:W-48-LEN + 3) TO W-BMP48-VAL
**       ADD  3                            TO W-48-LEN
**   END-IF
**      ---> hier würden Artikel eingestellt
*         MOVE 63           TO W207-XBMP
*         COMPUTE W-BUFFER-LEN = AMP-HOST-LEN - W-48-LEN
*         MOVE W-BUFFER-LEN TO W207-XCOBLEN
*         MOVE AMP-HOST-VAL(W-48-LEN + 1:W-BUFFER-LEN) TO W207-XCOBVAL
*         PERFORM L100-ADD-BMP
*         IF  ENDE
*             EXIT SECTION
*         END-IF
*   END-IF

** G.01.01 - Ende

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
*G.07.01 - VERF-AS verwenden hier
*     EVALUATE W-ROUTKZ
     EVALUATE VERF-AS
*G.07.01 - Ende
         WHEN 05     PERFORM D305-AVIA
         WHEN 07     PERFORM D307-SHELL
         WHEN 10     PERFORM D310-TOTAL
         WHEN 12     PERFORM D312-DKV
         WHEN 14     PERFORM D314-BP
         WHEN 15     PERFORM D315-ENI
         WHEN 16     PERFORM D316-ORLEN
         WHEN 17     PERFORM D317-UTA
         WHEN 18     PERFORM D318-TND

*G.01.14 - Anfang
         WHEN 22     PERFORM D322-EUROWAG
*G.01.14 - Ende

*G.01.17 - Anfang
         WHEN 23     PERFORM D323-LOGPAY
*G.01.17 - Ende

*G.06.48 - Anfang
         WHEN 24     PERFORM D324-STIGLECHNER
*G.06.48 - Ende

*G.06.55 - Roadrunner AS neu
         WHEN 25     PERFORM D325-ROADRUNNER
*G.06.55 - Ende

         WHEN OTHER
                 SET ENDE TO TRUE
*G.07.01 - jetzt VERF-AS verwenden
*                 MOVE W-ROUTKZ TO D-NUM4
                 MOVE VERF-AS TO D-NUM4
*G.07.01 - Ende
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

*G.00.19 - Anfang
*G.07.01 - VERF-AS jetzt
*         EVALUATE W-ROUTKZ
         EVALUATE VERF-AS
*G.07.01 - Ende
             WHEN 22
                  PERFORM F950-ASMAC-DUKPT
             WHEN OTHER
              MOVE ALL LOW-VALUES    TO W66-AKEY-NAME
              MOVE W-MACKEYA         TO W66-AKEY-NAME (1:4)
              SET  W66-MAC-BILDEN-AS TO TRUE
              SET  W66-IFSF          TO TRUE
              MOVE W-TERMNR          TO IMSG-TERMNR
              PERFORM F920-MAC-BILDEN
          END-EVALUATE
*G.00.19 - Ende
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

**  ---> Anfragenachricht für Tabelle =TXNLOG70 AS-Nachrichten
     PERFORM G120-PUT-TXNLOG70-AS

**  ---> ggf. anpassen - Serverspezifischer Timeout
*        (bei -1 wird der Verfahrensdefault hier fuer "B" verwendet)
     MOVE -1 TO MEM-APP-TIMEOUT

**  ---> Autorisierung: Log-Eintrag in MEMLOG
     SET MEM-INSERT TO TRUE
     MOVE "B"       TO MEM-VERFKZ

     MOVE SPACES    TO MEM-LOGKEY

*G.06.36 - Anfang
**  ---> Logkey setzen: Terminal-Nr., Trace-Nr., NType, AbwKz
*    STRING  W-TERMNR
*            W-TRACENR
*            W-NTYPE
*            W-ABWKZ
*                delimited by size
*      INTO  MEM-LOGKEY
*    END-STRING
**
    MOVE   IMSG-CF(IMSG-TPTR(59):IMSG-TLEN(59)) TO MEM-LOGKEY
*G.06.36 - Ende

     MOVE IMSG-NEXTSERV TO MEM-ASNAME

**  ---> holen momentanen Zeitpunkt
     PERFORM U200-TIMESTAMP
     MOVE TAGESDATUM  TO ZP-AOUT OF TXILOG70
     MOVE H-ZP-IN     TO ZP-TIN  OF TXILOG70
*
     MOVE TXILOG70    TO MEM-TXILOG70
     MOVE TXNLOG70-TS TO MEM-TXNLOG70-TS
     MOVE TXNLOG70-AS TO MEM-TXNLOG70-AS

     if  trace-on
         move "Test # - C500-LOGGING - MEMLOG-Logkey:" to daten-buffer1
         move mem-logkey                               to daten-buffer2
         string  "AS-Name: "
                 mem-asname
                     delimited by size
           into  daten-buffer3
         end-string
         perform z002-progerr
     end-if

**  ---> Aufruf MEMLOG
     PERFORM M110-CALL-SYSMK7I
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

*G.07.01 - auch ROUTKZ Vergleich, da nun alle Eintraege geladen
         IF  TK-CARDID (C4-I1) NOT = W-CARDID
         OR  TK-ROUTKZ (C4-I1) NOT = W-ROUTKZ
*G.07.01 - Ende
**          ---> nächsten suchen
             EXIT PERFORM CYCLE
         END-IF

**      ---> Schlüssel zur Verfügung stellen
         MOVE TK-HEXKEY (C4-I1) TO W-MACKEYA
         MOVE TK-HEXKEY (C4-I1) TO W-PACKEYA (1:4)
**      ---> Schlüsselgeneration und -version bereitstellen
         MOVE TK-HEXISO (C4-I1) TO W-ISOGEN-VERS
*G.07.01 - AIID speichern
         MOVE TK-AIID (C4-I1)   TO W-AIID
*G.07.01 - Ende
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

**  ---> BMP 48 - geht erst nach Artikel-Mapper (fummelt aus BMP63 ggf.
**  --->          noch Fahrerdaten, die einzustellen sind (48.8))
**  --->          !!! bei Avia allerdings nicht !!!
**  ---> zunächst die BitMap erstellen
     MOVE ALL ZEROES TO W-BYTEMAP-48
     MOVE "1"        TO W-BYTEMAP-48(4:1)
     MOVE "1"        TO W-BYTEMAP-48(14:1)
     IF  GEODATA-YES
         MOVE "1"    TO W-BYTEMAP-48(41:1)
     END-IF
     MOVE  LOW-VALUE TO W-BITMAP
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE 8        TO W-BUFFER-LEN
     MOVE W-BITMAP TO W-BUFFER

**  +++> und jetzt die Subfelder 4, 14 Fixwerte und ggf. 41
     MOVE "000000000103" TO W-BUFFER (W-BUFFER-LEN + 1:)
     ADD 12 TO W-BUFFER-LEN

     IF  GEODATA-YES
         MOVE GEO-BUFFER TO W-BUFFER (W-BUFFER-LEN + 1:)
         ADD 20 TO W-BUFFER-LEN
     END-IF

**  +++> jetzt in die Nachricht einbauen
     MOVE 48           TO W207-XBMP
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-BUFFER     TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
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

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT

*G.06.39 - Sonderbehandlung verkürztes BMP 59 für Shell2

*    Laenge aus allgemeinem Teil wieder reduzieren (nur noch 10 Byte:
*    Release + Applikation + AS-Tracenr)
     MOVE   10     TO W207-TLEN(59)

*    W207-CF Terminal-ID / Tracenr / Abwkz mit AS-TRACENR ueberschreiben
     MOVE W-AS-TRACENR    TO W207-CF(W207-TPTR(59) + 4:6)
*G.06.39 - Ende


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
     IF  ERF-MANUELL
         MOVE "210201613001" TO W207-XCOBVAL
     ELSE
         MOVE "210201B13001" TO W207-XCOBVAL
     END-IF
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

*G.06.30 - Anfang

**  ---> BMP 26 - Branchencode
     MOVE 26     TO W207-XBMP
     MOVE 04     TO W207-XCOBLEN
     MOVE "5541" TO W207-XCOBVAL
     PERFORM L100-ADD-BMP

     IF  ENDE
         EXIT SECTION
     END-IF

*G.06.30 - Anfang

*G.01.16 - Anfang
*
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
*G.01.16 - Ende

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT
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

**  ---> und BMP43 aufbereiten (Name, Ort)
     MOVE 43 TO W207-XBMP
     MOVE 1  TO C4-PTR
     MOVE SPACES TO W207-XCOBVAL
     STRING  NAME of STATION
                 delimited by space
             ", "
                 delimited by size
             ORT of STATION
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
*G.01.15 - Anfang
*    SET W66-TOTAL TO TRUE
**
     SET W66-DEFAULT TO TRUE
*G.01.15 - Ende

**  ---> BMP 22 - Eingabeart wird hier überschrieben
     MOVE 22 TO W207-XBMP
     MOVE 12 TO W207-XCOBLEN
     IF  ERF-MANUELL
         MOVE "C11201654144" TO W207-XCOBVAL
     ELSE
         MOVE "C11201214144" TO W207-XCOBVAL
     END-IF
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF

* G.07.04 VURN jetzt aus TSKART40
**  ---> BMP 42 - VUNR wird hier überschrieben
*     MOVE 42 TO S-BMP
*     MOVE 1  TO S-LFDNR
*     PERFORM U300-SEARCH-TAB*
*     IF  PRM-NOT-FOUND
*         PERFORM E900-PUT-ERRLOG
*         SET ENDE TO TRUE
*         EXIT SECTION
*     END-IF
*
*     PERFORM U400-INTERPRET-ABWEICHUNG
*     MOVE 42           TO W207-XBMP
*     MOVE W-BUFFER     TO W207-XCOBVAL
*     MOVE W-BUFFER-LEN TO W207-XCOBLEN
*     MOVE W-MDNR (7:2) TO W207-XCOBVAL (W207-XCOBLEN + 1:2)
*
*     ADD 10 TO W207-XCOBLEN
*     PERFORM L100-ADD-BMP
*     IF  ENDE
*         EXIT SECTION
*     END-IF
*G.07.04  ENDE

*G.01.12 - Anfang
**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT

*G.01.12 - Ende

     continue
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

*G.07.03
**  ---> BMP 42 muss hier nochmal überarbeitet werden
*    MOVE "3280"  TO W-BUFFER
*     MOVE 4      TO W-BUFFER-LEN

**  ---> und nun der helle Wahnsinn !!!  lt. Kay für erfolgreiche Tests
*     IF  W-MDNR = 99 and W-TSNR = 1
*         MOVE 1154 TO W-KONV-TSNR
*     ELSE
*         MOVE W-TSNR TO W-KONV-TSNR
*     END-IF

**  ---> führende Nullen entfernen
*     MOVE ZERO TO C4-PTR
*     INSPECT W-KONV-TSNR-STR TALLYING C4-PTR for LEADING SPACES
*     MOVE W-KONV-TSNR-STR (C4-PTR + 1:8 - C4-PTR)
*         TO W-BUFFER (W-BUFFER-LEN + 1:8 - C4-PTR)
*     COMPUTE W-BUFFER-LEN = W-BUFFER-LEN + 8 - C4-PTR

*     MOVE 42           TO W207-XBMP
*     MOVE W-BUFFER     TO W207-XCOBVAL
*     MOVE W-BUFFER-LEN TO W207-XCOBLEN
*     PERFORM L100-ADD-BMP
*     IF  ENDE
*         EXIT SECTION
*     END-IF
*G.07.03 ENDE

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
     IF  PAC-YES
         MOVE "C11201214144" TO W207-XCOBVAL
     ELSE
         MOVE "C11201212144" TO W207-XCOBVAL
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
*     SET W66-DEFAULT TO TRUE
     SET W66-TND TO TRUE

*G.06.23 - Anfang
**  ---> BMP 41 - TERMNR - wird an AS geschickt

*G.06.31 - Anfang
*G.06.28 - Anfang
*    MOVE 1 TO W207-TBMP-O(41:1)
*
*    MOVE 41           TO W207-XBMP
*    MOVE 8            TO W207-XCOBLEN
*    MOVE W-TERMNR     TO W207-XCOBVAL
*    PERFORM L100-ADD-BMP
*    IF  ENDE
*        EXIT SECTION
*    END-IF
*G.06.28 - Ende
*G.06.31 - Ende

**  ---> BMP 48 - geht erst nach Artikel-Mapper (fummelt aus BMP63 ggf.
**  --->          noch Fahrerdaten, die einzustellen sind (48.8))
**  --->          !!! bei TND/Avia allerdings nicht !!!
**  ---> zunächst die BitMap erstellen
     MOVE ALL ZEROES TO W-BYTEMAP-48
     MOVE "1"        TO W-BYTEMAP-48(4:1)
     MOVE "1"        TO W-BYTEMAP-48(14:1)
*G.06.18 - Anfang
**   IF  GEODATA-YES
**       MOVE "1"    TO W-BYTEMAP-48(41:1)
**   END-IF
*G.06.18 - Ende

     MOVE  LOW-VALUE TO W-BITMAP
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE 8        TO W-BUFFER-LEN
     MOVE W-BITMAP TO W-BUFFER

**  +++> und jetzt die Subfelder 4, 14 Fixwerte und ggf. 41
     MOVE "000000000103" TO W-BUFFER (W-BUFFER-LEN + 1:)
     ADD 12 TO W-BUFFER-LEN

*G.06.18 - Anfang
**   IF  GEODATA-YES
**       MOVE GEO-BUFFER TO W-BUFFER (W-BUFFER-LEN + 1:)
**       ADD 20 TO W-BUFFER-LEN
**   END-IF
*G.06.18 - Ende

**  +++> jetzt in die Nachricht einbauen
     MOVE 48           TO W207-XBMP
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-BUFFER     TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
     .
 D318-99.
     EXIT.

*G.01.14 - Anfang

******************************************************************
* spezielle Behandlung für das Eurowag-AS
* ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
******************************************************************

 D322-EUROWAG SECTION.
 D322-00.

** Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

*G.06.23 - Anfang
**  ---> BMP 41 - TERMNR - wird an AS geschickt

     MOVE 1 TO W207-TBMP-O(41:1)

**  ---> BMP 26 - Branchencode
     MOVE 26     TO W207-XBMP
     MOVE 04     TO W207-XCOBLEN
     MOVE "5541" TO W207-XCOBVAL
     PERFORM L100-ADD-BMP

     IF  ENDE
         EXIT SECTION
     END-IF

*G.06.23 - Ende

** BMP 48 - geht erst nach Artikel-Mapper (fummelt aus BMP63 ggf.
** noch Fahrerdaten, die einzustellen sind (48.8))
** !!! bei Avia allerdings nicht !!!
** zunächst die BitMap erstellen

     MOVE ALL ZEROES TO W-BYTEMAP-48
     MOVE "1"        TO W-BYTEMAP-48(4:1)
     MOVE "1"        TO W-BYTEMAP-48(14:1)

*G.06.18 - Anfang
**   IF  GEODATA-YES
**       MOVE "1"    TO W-BYTEMAP-48(41:1)
**   END-IF
*G.06.18 - Ende

     MOVE  LOW-VALUE TO W-BITMAP
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE 8        TO W-BUFFER-LEN
     MOVE W-BITMAP TO W-BUFFER

** Jetzt die Subfelder 4, 14 Fixwerte und ggf. 41
     MOVE "000000000103" TO W-BUFFER (W-BUFFER-LEN + 1:)
     ADD 12 TO W-BUFFER-LEN

*G.06.18 - Anfang
**   IF  GEODATA-YES
**       MOVE GEO-BUFFER TO W-BUFFER (W-BUFFER-LEN + 1:)
**       ADD 20 TO W-BUFFER-LEN
**   END-IF
*G.06.18 - Ende

** Jetzt in die Nachricht einbauen
     MOVE 48           TO W207-XBMP
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-BUFFER     TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
     .

 D322-99.
     EXIT.

*G.01.14 - Ende

*G.01.17 - Anfang

******************************************************************
* spezielle Behandlung für das LogPay-AS
* ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
******************************************************************

 D323-LOGPAY SECTION.
 D323-00.

** Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

** BMP 48 - geht erst nach Artikel-Mapper (fummelt aus BMP63 ggf.
** noch Fahrerdaten, die einzustellen sind (48.8))
** !!! bei Avia allerdings nicht !!!
** zunächst die BitMap erstellen

     MOVE ALL ZEROES TO W-BYTEMAP-48
     MOVE "1"        TO W-BYTEMAP-48(4:1)
     MOVE "1"        TO W-BYTEMAP-48(14:1)

*G.06.18 - Anfang
**   IF  GEODATA-YES
**       MOVE "1"    TO W-BYTEMAP-48(41:1)
**   END-IF
*G.06.18 - Ende

     MOVE  LOW-VALUE TO W-BITMAP
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE 8        TO W-BUFFER-LEN
     MOVE W-BITMAP TO W-BUFFER

** Jetzt die Subfelder 4, 14 Fixwerte und ggf. 41
     MOVE "000000000103" TO W-BUFFER (W-BUFFER-LEN + 1:)
     ADD 12 TO W-BUFFER-LEN

*G.06.18 - Anfang
**   IF  GEODATA-YES
**       MOVE GEO-BUFFER TO W-BUFFER (W-BUFFER-LEN + 1:)
**       ADD 20 TO W-BUFFER-LEN
**   END-IF
*G.06.18 - Ende

** Jetzt in die Nachricht einbauen
     MOVE 48           TO W207-XBMP
     MOVE W-BUFFER-LEN TO W207-XCOBLEN
     MOVE W-BUFFER     TO W207-XCOBVAL
     PERFORM L100-ADD-BMP
     IF  ENDE
         EXIT SECTION
     END-IF
     .

 D323-99.
     EXIT.

*G.01.17 - Ende

*G.06.48 - Anfang

******************************************************************
* spezielle Behandlung für das Stiglechner-AS
* ggf. mit Hilfe der Parameter aus Tabelle =FCPARAM
******************************************************************

 D324-STIGLECHNER SECTION.
 D324-00.

** Anwendung für MAC-Bildung setzen
**   SET W66-DEFAULT TO TRUE
     SET W66-DKV TO TRUE

**  ---> und BMP48 aufbereiten
     PERFORM E310-BMP48-DEFAULT

     IF  ENDE
         EXIT SECTION
     END-IF
     .

 D324-99.
     EXIT.

*G.06.48 - Ende

******************************************************************
* spezielle Behandlung für das Roadrunner-AS
*G.06.55 - neu
******************************************************************
 D325-ROADRUNNER SECTION.
 D325-00.
**  ---> Anwendung für MAC-Bildung setzen
     SET W66-DEFAULT TO TRUE

*G.07.02 - BMP 48-8 jetzt auch, Default Section kann verwendet werden
     PERFORM E310-BMP48-DEFAULT

***  ---> BMP 48 - geht erst nach Artikel-Mapper (fummelt aus BMP63 ggf.
***  --->          noch Fahrerdaten, die einzustellen sind (48.8))
***  --->          !!! bei Roadrunner allerdings nicht !!!
***  ---> zunächst die BitMap erstellen
*     MOVE ALL ZEROES TO W-BYTEMAP-48
*     MOVE "1"        TO W-BYTEMAP-48(4:1)
*     MOVE "1"        TO W-BYTEMAP-48(14:1)
*     IF  GEODATA-YES
*         MOVE "1"    TO W-BYTEMAP-48(41:1)
*     END-IF
*     MOVE  LOW-VALUE TO W-BITMAP
*     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
*     MOVE 8        TO W-BUFFER-LEN
*     MOVE W-BITMAP TO W-BUFFER
*
***  +++> und jetzt die Subfelder 4, 14 Fixwerte und ggf. 41
*     MOVE "000000000103" TO W-BUFFER (W-BUFFER-LEN + 1:)
*     ADD 12 TO W-BUFFER-LEN
*
*     IF  GEODATA-YES
*         MOVE GEO-BUFFER TO W-BUFFER (W-BUFFER-LEN + 1:)
*         ADD 20 TO W-BUFFER-LEN
*     END-IF
*
***  +++> jetzt in die Nachricht einbauen
*     MOVE 48           TO W207-XBMP
*     MOVE W-BUFFER-LEN TO W207-XCOBLEN
*     MOVE W-BUFFER     TO W207-XCOBVAL
*     PERFORM L100-ADD-BMP
*     IF  ENDE
*         EXIT SECTION
*     END-IF
*G.07.02 - Ende
     .
 D325-99.
     EXIT.

******************************************************************
* bestimmen CARDID
******************************************************************
 D900-GET-CARDID SECTION.
 D900-00.
**  ---> CARDID über Modul WSYS930 holen, dafür
**  ---> erste 9 Stellen der Karten-Nr. bereitstellen
     IF  IMSG-TBMP(2) = 1
         MOVE IMSG-CF(IMSG-TPTR(2):9)  TO ROUT-MERKMAL
     ELSE
         MOVE IMSG-CF(IMSG-TPTR(35):9) TO ROUT-MERKMAL
     END-IF
     SET ROUT-CMD-ID TO TRUE
     PERFORM M150-CALL-WSYS930
     IF  ENDE
         EXIT SECTION
     END-IF
     MOVE ROUT-CARDID TO W-CARDID
                         S-CARDID

**  ---> VUNR / Routkz über TSKART40 holen
     MOVE IMSG-MDNR TO MDNR    of TSKART40
     MOVE IMSG-TSNR TO TSNR    of TSKART40
     IF  IMSG-TBMP(25) = 1
         MOVE 2     TO CARDSYS OF TSKART40
     ELSE
         MOVE 1     TO CARDSYS OF TSKART40
     END-IF
     MOVE W-CARDID  TO CARDID  of TSKART40

     PERFORM S150-SELECT-TSKART40
     IF  W-AC not = ZERO
         EXIT SECTION
     END-IF

**  ---> Routinginformationen über Modul WSYS930 holen
     MOVE W-ROUTKZ TO ROUT-KZ
     SET ROUT-CMD-AS TO TRUE
     PERFORM M150-CALL-WSYS930
     IF  ROUT-NOT-OK
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

*G.06.40 - Anfang
******************************************************************
* Verabreitung EMV-Daten (Uebernahme TXILOG70)
******************************************************************
 D950-EMV-VERARBEITUNG SECTION.
 D950-00.

*    Pruefen BMP 22 auf Gueltigkeit (Pos 7 = 05 = ICC)
*G.07.02 - Kontaktlos Chip auch beachten
     IF W-ERF-CHIP OR W-ERF-KONTAKTLOS
*G.07.02 - Ende
        IF IMSG-TBMP(14) = 1
           CONTINUE
        ELSE
**      ---> Fehler: weder BMP2/14 noch BMP35 vorhanden
          SET ERF-ERROR TO TRUE
          MOVE 30  TO W-AC
          STRING "BMP 14 fehlt bei Chip-TX: ",
                  W-ERFASSUNGS-ART
          DELIMITED BY SIZE INTO DATEN-BUFFER1
          PERFORM Z002-PROGERR
          EXIT SECTION
        END-IF
     ELSE
        INITIALIZE GEN-ERROR
        STRING "Falsche Erfassungsart (22) bei Chip-TX: ",
               W-ERFASSUNGS-ART
        DELIMITED BY SIZE INTO DATEN-BUFFER1
        STRING "Bei TERMNR: ",
                W-TERMNR,
                " / TRACENR ",
                W-TRACENR
        DELIMITED BY SIZE INTO DATEN-BUFFER2
        PERFORM Z002-PROGERR
        MOVE 30 TO W-AC
        EXIT SECTION
     END-IF

*    Uebernehmen komplette Chipdaten nach TXILOG70
     MOVE IMSG-CF(IMSG-TPTR(55):IMSG-TLEN(55))
       TO VAL OF EMV-DATEN OF TXILOG70
     MOVE IMSG-TLEN(55)
       TO LEN OF EMV-DATEN OF TXILOG70

*---> Extrahieren CVM-Result

*    Default "2" - online PIN  wenn kein  9F34 da ist
     MOVE "2"   TO CVM-RESULT
*    Erstmal annehmen, dass 9F34 nicht vorhanden
     SET W400-NOTFOUND   TO TRUE

*    Versuch, 9F34 ueber WISO400 zu holen
     SET W400-LOOK4TAGXP TO TRUE
     MOVE VAL OF EMV-DATEN OF TXILOG70 TO W400-BER-TLV-STRING
     MOVE LEN OF EMV-DATEN OF TXILOG70 TO W400-BER-TLV-LEN
     MOVE "9F34"                       TO W400-SEARCH-TAG
     CALL "WISO400" USING W400-WISO400C
*
     EVALUATE TRUE

       WHEN W400-OK
            MOVE W400-KAAI-LTV-STRING (2:1)
              TO CVM-RESULT OF TXILOG70

       WHEN W400-NOTFOUND
            CONTINUE

       WHEN OTHER
            INITIALIZE GEN-ERROR
            MOVE W400-RCODE TO D-NUM4
            MOVE 2201       TO ERROR-NR OF GEN-ERROR
            STRING "BMP55-Err (LOOK4TAGXP): "
                    D-NUM4
                   "@"
            DELIMITED BY SIZE INTO DATEN-BUFFER1
            STRING  "Term-Nr./Trace-Nr.: "
                     W-TERMNR
                    "/"
                     W-TRACENR
            DELIMITED BY SIZE INTO  DATEN-BUFFER2
            PERFORM Z002-PROGERR
            MOVE 30 TO W-AC
            EXIT SECTION

     END-EVALUATE

*G.06.43 - Anfang
*G.03.03 - Anfang
*    Prüfung auf TAG 5F34 (Kartenfolgenummer)
*    Erstmal annehmen, dass 5F34 nicht vorhanden
*
*
*    SET W400-NOTFOUND TO TRUE
*
*Versuch, 5F34 ueber WISO400 zu holen
*    SET W400-LOOK4TAGXP TO TRUE
*    MOVE VAL OF EMV-DATEN OF TXILOG70 TO W400-BER-TLV-STRING
*    MOVE LEN OF EMV-DATEN OF TXILOG70 TO W400-BER-TLV-LEN
*    MOVE "5F34"                       TO W400-SEARCH-TAG
*
*    CALL "WISO400" USING W400-WISO400C
*
*    EVALUATE TRUE
*
*     WHEN W400-OK
*
*          IF  IMSG-TBMP(23) = 1
*          AND IMSG-TPTR(23) > ZEROS
*          AND IMSG-TLEN(23) > ZEROS
*              CONTINUE
*          ELSE
*              INITIALIZE GEN-ERROR
*              STRING "TAG 5F34 in BMP55, BMP23 fehlt in der Anfrage",
*              DELIMITED BY SIZE INTO DATEN-BUFFER1
*              STRING "Bei TERMNR: ",
*                      W-TERMNR,
*                     " / TRACENR ",
*                      W-TRACENR
*              DELIMITED BY SIZE INTO DATEN-BUFFER2
*              PERFORM Z002-PROGERR
*              MOVE 30 TO W-AC
*              EXIT SECTION
*          END-IF
*
*     WHEN W400-NOTFOUND
*
*          IF  IMSG-TBMP(23) = 1
*          AND IMSG-TPTR(23) > ZEROS
*          AND IMSG-TLEN(23) > ZEROS
*              INITIALIZE GEN-ERROR
*              STRING "BMP23 in Anfrage, aber TAG 5F34 fehlt in BMP55",
*              DELIMITED BY SIZE INTO DATEN-BUFFER1
*              STRING "Bei TERMNR: ",
*                      W-TERMNR,
*                     " / TRACENR ",
*                      W-TRACENR
*              DELIMITED BY SIZE INTO DATEN-BUFFER2
*              PERFORM Z002-PROGERR
*              MOVE 30 TO W-AC
*              EXIT SECTION
*          END-IF
*
*     WHEN OTHER
*          INITIALIZE GEN-ERROR
*          MOVE W400-RCODE TO D-NUM4
*          MOVE 2201 TO ERROR-NR OF GEN-ERROR
*          STRING "BMP55-Err (LOOK4TAGXP): "
*                  D-NUM4
*                 "@"
*          DELIMITED BY SIZE INTO DATEN-BUFFER1
*
*          STRING  "Term-Nr./Trace-Nr.: "
*                   W-TERMNR
*                  "/"
*                   W-TRACENR
*          DELIMITED BY SIZE INTO  DATEN-BUFFER2
*          PERFORM Z002-PROGERR
*          MOVE 30 TO W-AC
*          EXIT SECTION
*
*     END-EVALUATE
*G.03.03 - Ende
*G.06.43 - Ende
     .

 D950-99.
     EXIT.

*G.06.40 - Ende

******************************************************************
* generieren FEP-Antwort
******************************************************************
 E100-FEP-ANTWORT SECTION.
 E100-00.
     SET FEP-ANTWORT TO TRUE

**  ---> zunaechst Felder ausschalten, die nicht gesendet werden sollen
     MOVE ZERO TO IMSG-TBMP (02)
     MOVE ZERO TO IMSG-TBMP (14)
     MOVE ZERO TO IMSG-TBMP (22)
     MOVE ZERO TO IMSG-TBMP (25)
     MOVE ZERO TO IMSG-TBMP (26)
     MOVE ZERO TO IMSG-TBMP (35)
     MOVE ZERO TO IMSG-TBMP (52)

*G.06.34 - BMP 55 auch ausknipsen
     MOVE ZERO TO W207-TBMP (55)
*G.06.34 - Ende

     MOVE ZERO TO IMSG-TBMP (56)
     MOVE ZERO TO IMSG-TBMP (60)
     MOVE ZERO TO IMSG-TBMP (63)

**  ---> im Folgenden noch die Felder BMP 33, 39, 42 hinzufuegen
     MOVE IMSG-COBDATEN TO W207-COBDATEN
     SET W207-EC        TO TRUE

**  ---> Nachrichtentype setzen
     MOVE 0210 TO W207-NTYPE

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

*G.06.37 - Anfang
     IF IMSG-TBMP(42) = 1
        MOVE ZERO TO IMSG-TBMP (42)
     ELSE
**  ---> BMP42  VUNR
        MOVE 42               TO W207-XBMP
        MOVE VUNR of TSKART40 TO W207-XCOBVAL
        MOVE 15               TO W207-XCOBLEN
        PERFORM L100-ADD-BMP
        IF ENDE
           EXIT SECTION
        END-IF
     END-IF
*G.06.37 - Ende

**  ---> BMP53  Sicherheitsverfahren
**  --->        wird so zurück gegeben wie's gekommen ist

**  ---> BMP64  dummy-MAC vorbereiten

*G.01.05 - Anfang Mac-Yes wird auch über FCPARAM gesetzt
*    IF  MAC-YES
**
     IF  IMSG-TBMP(64) = 1
*G.01.05 - Ende

         MOVE 64         TO W207-XBMP
         MOVE LOW-VALUES TO W207-XCOBVAL
         MOVE 8          TO W207-XCOBLEN
         PERFORM L100-ADD-BMP
         IF  ENDE
             EXIT SECTION
         END-IF
     END-IF

**  ---> Nachricht zusammenstellen
     SET W207-EC TO TRUE
     PERFORM L110-COB2ISO
     IF  ENDE
         EXIT SECTION
     END-IF

**  ---> und echten MAC bilden und einstellen
*G.01.05 - Anfang Mac-Yes wird auch über FCPARAM gesetzt
*    IF  MAC-YES
**
     IF  IMSG-TBMP(64) = 1
*G.01.05 - Ende
         MOVE ALL LOW-VALUES    TO W66-TKEY-NAME
         MOVE W-MACKEYT         TO W66-TKEY-NAME (1:4)
         SET  W66-MAC-BILDEN-TS TO TRUE
         SET  W66-EC            TO TRUE
         MOVE W-TERMNR          TO IMSG-TERMNR
         PERFORM F920-MAC-BILDEN
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

*G.06.55 - Evaluate ist ueberfluessig, AS-VERF wird verwendet
**G.01.09 - Anfang
*     EVALUATE W-ROUTKZ
*
*         WHEN 05  MOVE "AV" TO AMP-FORMAT
*         WHEN 07  MOVE "SH" TO AMP-FORMAT
*         WHEN 10  MOVE "TO" TO AMP-FORMAT
*         WHEN 12  MOVE "DK" TO AMP-FORMAT
*         WHEN 14  MOVE "BP" TO AMP-FORMAT
*         WHEN 15  MOVE "AG" TO AMP-FORMAT
*         WHEN 16  MOVE "OR" TO AMP-FORMAT
*         WHEN 17  MOVE "UT" TO AMP-FORMAT
*         WHEN 18  MOVE "TN" TO AMP-FORMAT
*
**G.01.14 - Anfang
*         WHEN 22  MOVE "EU" TO AMP-FORMAT
**G.01.14 - Ende
*
**G.01.17 - Anfang
*         WHEN 23  MOVE "LO" TO AMP-FORMAT
**G.01.17 - Ende
*
**G.06.48 - Anfang
*         WHEN 24  MOVE "IQ" TO AMP-FORMAT
**G.06.48 - Ende
*
**G.06.55 - Roadrunner neu
*         WHEN 25  MOVE "RR" TO AMP-FORMAT
**G.06.55 - Ende
*         WHEN OTHER
*              CONTINUE
*     END-EVALUATE
**G.01.09 - Ende
*G.06.55 - Ende

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
     MOVE SPACES     TO W207-XCOBVAL
     MOVE 8          TO W207-XCOBLEN

**  +++> jetzt das Subfeld 3 oder auch nicht
     MOVE 48 TO S-BMP
     MOVE 3  TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         PERFORM U400-INTERPRET-ABWEICHUNG
         MOVE W-BUFFER (1:W-BUFFER-LEN) TO W207-XCOBVAL (W207-XCOBLEN + 1:)
         ADD W-BUFFER-LEN TO W207-XCOBLEN
         MOVE "1"         TO W-BYTEMAP-48(3:1)
     END-IF

**  +++> und jetzt das Subfeld 4 (Fixwert)
     MOVE "1"          TO W-BYTEMAP-48(4:1)
     MOVE "0000000001" TO W207-XCOBVAL (W207-XCOBLEN + 1:)
     ADD 10 TO W207-XCOBLEN

**  +++> und jetzt das Subfeld 8 (Kfz-Daten)
     IF  KFZ-YES AND W-48-LEN > ZERO
         MOVE W-BMP48-VAL (1:W-48-LEN) TO W207-XCOBVAL (W207-XCOBLEN + 1:)
         ADD W-48-LEN TO W207-XCOBLEN
         MOVE "1"     TO W-BYTEMAP-48(8:1)
     END-IF

**  +++> und jetzt das Subfeld 14, ggf. aus =FCPARAM
     MOVE "1" TO W-BYTEMAP-48(14:1)
     MOVE 48 TO S-BMP
     MOVE 14 TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         PERFORM U400-INTERPRET-ABWEICHUNG
         MOVE W-BUFFER (1:W-BUFFER-LEN) TO W207-XCOBVAL (W207-XCOBLEN + 1:)
         ADD W-BUFFER-LEN TO W207-XCOBLEN
     ELSE
         MOVE "33" TO W-BUFFER (W-BUFFER-LEN + 1:)
         ADD 2 TO W207-XCOBLEN
     END-IF

**  +++> und jetzt das Subfeld 38, ggf. aus =FCPARAM
     MOVE 48 TO S-BMP
     MOVE 38 TO S-LFDNR
     PERFORM U300-SEARCH-TAB
     IF  PRM-FOUND
         PERFORM U400-INTERPRET-ABWEICHUNG
         MOVE W-BUFFER (1:W-BUFFER-LEN) TO W207-XCOBVAL (W207-XCOBLEN + 1:)
         ADD W-BUFFER-LEN TO W207-XCOBLEN
         MOVE "1"         TO W-BYTEMAP-48(38:1)
     END-IF

**  +++>  nun endlich die Bitmap einfügen
     MOVE  LOW-VALUE TO W-BITMAP
     ENTER TAL "WT^BY2BI" USING W-BITMAP W-BYTEMAP-48
     MOVE W-BITMAP TO W207-XCOBVAL (1:8)

**  +++> jetzt in die Nachricht einbauen
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
*G.07.01 - VERF-AS jetzt
*     MOVE W-ROUTKZ  TO D-NUM4
     MOVE VERF-AS  TO D-NUM4
*G.07.01 - Ende
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
* Logdaten bei Eigenantworten bereitstellen an das Terminal
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

** G.01.00 - Anfang
**   PERFORM G110-PUT-TXNLOG70-TS
****
     PERFORM G115-PUT-TXNLOG70-TS-ANF-ANT
** G.01.00 - Ende

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
* MAC prüfen (ggf. ist W-AC gesetzt)
******************************************************************
 F910-MAC-PRUEFEN SECTION.
 F910-00.
**  ---> MAC ist ggf. schon in der Drehscheibe (WXCRD7) geprüft
     EVALUATE IMSG-MDNR-HOST

         WHEN 10     EXIT SECTION
         WHEN 97     MOVE 97 TO W-AC
                     MOVE "MAC-Prüfung aus WXCRD7S meldet MAC falsch"
                         TO DATEN-BUFFER1
                     MOVE "AC 97 gesetzt" TO DATEN-BUFFER2
                     PERFORM Z002-PROGERR
                     EXIT SECTION

     END-EVALUATE

**  ---> keine Vorprüfung des MAC also hier prüfen
     SET W66-MAC-PRUEFEN-TS TO TRUE
     MOVE 88                TO W66-RCODE of W66-WSY7066C
     SET  W66-DEFAULT       TO TRUE
     MOVE ALL LOW-VALUES    TO W66-TKEY-NAME
     MOVE W-MACKEYT         TO W66-TKEY-NAME (1:4)

     PERFORM M140-CALL-WSY7066
     .
 F910-99.
     EXIT.

******************************************************************
* Erstellen Antwort-MAC
******************************************************************
 F920-MAC-BILDEN SECTION.
 F920-00.
**  ---> MAC-Bildung und Schlüsselwechsel über WSY7066
**  ---> Datenbereich Message-datei external übergibt Daten für WSY7066
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

**  --> Datenteil wird in WSY7066 mit MAC versehen
     .
 F920-99.
      EXIT.

******************************************************************
* PAC umschluesseln
******************************************************************
 F930-PAC-UMSCHLUESSELN SECTION.
 F930-00.
     SET W66-PAC-UMSCHL  TO TRUE

*G.06.25 - Anfang
**  ---> Anwendungs-KZ setzen
*G.07.01 - VERF-AS jetzt
*      EVALUATE W-ROUTKZ
      EVALUATE VERF-AS
*G.07.01 - Ende
          WHEN 18
               SET W66-TND     TO TRUE
          WHEN OTHER
               SET W66-DEFAULT TO TRUE
      END-EVALUATE
*G.06.25 - Ende

     MOVE 88             TO W66-RCODE of W66-WSY7066C
     MOVE ALL LOW-VALUES TO W66-TKEY-NAME
     MOVE ALL LOW-VALUES TO W66-AKEY-NAME
     MOVE W-PACKEYT      TO W66-TKEY-NAME (1:4)
     MOVE W-PACKEYA      TO W66-AKEY-NAME (1:4)
     PERFORM M140-CALL-WSY7066
     IF  W66-ERR
         MOVE 1201 TO ERROR-NR of GEN-ERROR
         MOVE W66-RCODE TO D-NUM4
         STRING  "WSY7066@"          delimited by size
                 D-NUM4              delimited by size
                 "@@"                delimited by size
           INTO  DATEN-BUFFER1
         END-STRING
         MOVE "Bei F930-PAC-UMSCHLUESSELN" TO DATEN-BUFFER2
         MOVE "Transaktions-Ende"          TO DATEN-BUFFER3
         PERFORM Z002-PROGERR
         SET ENDE TO TRUE
         EXIT SECTION
     END-IF
     .
 F930-99.
   EXIT.

*G.01.18 - Anfang
******************************************************************
* PAC umschluesseln mit DUKPT-Verfahren
******************************************************************
 F940-PAC-NACH-DUKPT SECTION.
 F940-00.
*
      SET Z-PAC-UMSCHL     TO TRUE

      MOVE 88              TO Z-RCODE OF Z-WEUR056C
      MOVE ALL LOW-VALUES  TO Z-TKEY-NAME
                              Z-AKEY-NAME
      MOVE W-PACKEYT       TO Z-TKEY-NAME (1:4)
      MOVE W-PACKEYA       TO Z-AKEY-NAME (1:4)
      MOVE W-AS-TRACENR    TO Z-AS-TRACENR

      CALL "WEUR056"  USING Z-WEUR056C,
                            INTERN-MESSAGE

      IF Z-ERR
         MOVE 1201      TO ERROR-NR of GEN-ERROR
         MOVE   Z-RCODE TO D-NUM4
         STRING "Returncode aus WEUR056: ", D-NUM4
         DELIMITED BY SIZE                 INTO DATEN-BUFFER1
         MOVE "Bei  F940-PAC-NACH-DUKPT"     TO DATEN-BUFFER2
         MOVE "Transaktions-Ende"            TO DATEN-BUFFER3

         PERFORM Z002-PROGERR
         SET ENDE TO TRUE
      END-IF

*     Ein neuer PAC wird in BMP 52 zurückgeliefert im AS-Format
*     ISO-0 (IFSF-Standard)

*     es wird ein BMP 57 mit dem DUKPT-AS-KEY zurückgeliefert
*     Der wird in 20 Bytes ASCII in die INTERN-MESSAGE
*     eingestellt.

*     Für die AS-Nachricht muss Inhalt von BMP 57 auf
*     BMP 53 übetragen werden und bmp 57 ggfs neu
*     besetzt werden bzw. das TMP (57) = 0 gesetzt
*     werden.

*
   .
 F940-99.
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

*G.06.21 - Anfang
      MOVE W-AS-TRACENR    TO Z-AS-TRACENR
*G.06.21 - Ende

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

*G.01.18 - Ende

******************************************************************
* Einstellen Daten in TXILOG70 Buffer
******************************************************************
 G100-PUT-TXILOG70 SECTION.
 G100-00.
     MOVE W-TERMNR (7:2) TO PNR            of TXILOG70
     MOVE W-TERMNR       TO TERMNR         of TXILOG70
     MOVE W-TRACENR      TO TRACENR        of TXILOG70
     MOVE 200            TO ISONTYP        of TXILOG70
     MOVE W-MDNR         TO MDNR           of TXILOG70
     MOVE W-TSNR         TO TSNR           of TXILOG70
     MOVE W-AS-TRACENR   TO TRACENR-AS     of TXILOG70
     MOVE W-KANR         TO KANR           of TXILOG70
*kl20171117 - G.03.44 - Kommt jetzt in INT-SCHNITTSTELLE-C
*                       von der Drehscheibe
*       MOVE "V2"           TO KZ-E2EE        of TXILOG70
     MOVE IMSG-RESERVE1(1:2) TO KZ-E2EE    of TXILOG70
*kl20171117 - G.03.44 - Ende

     MOVE SPACES         TO KEYNAME        of TXILOG70
     MOVE W18-BETRAG     TO BETRAG         of TXILOG70
     MOVE "M"            TO BETRAG-ART     of TXILOG70
     MOVE W-CARDID       TO CARDID         of TXILOG70
     MOVE W-ROUTKZ       TO ROUTKZ         of TXILOG70
     MOVE W-LTGIND       TO LTGIND         of TXILOG70
     MOVE 740000         TO ASID           of TXILOG70
     MOVE 9999           TO AC-AS          of TXILOG70
     MOVE W-AC           TO AC-TERM        of TXILOG70
     MOVE WKZ-WKZ-A      TO WKZ            of TXILOG70
     MOVE 70             TO LOGPROT        of TXILOG70
     IF  FEP-ANTWORT
         MOVE "L"        TO KZ-BEARB       of TXILOG70
     ELSE
         MOVE "A"        TO KZ-BEARB       of TXILOG70
     END-IF
*G.06.56 - neues KZ-VERF Chip
*     MOVE "f"            TO KZ-VERF        of TXILOG70
     IF  UMS-ZAHLUNG
         MOVE "Z"        TO KZ-UMSATZ      of TXILOG70
     ELSE
         MOVE "G"        TO KZ-UMSATZ      of TXILOG70
     END-IF
     MOVE W-ABL          TO ABL-JJMM       of TXILOG70
     MOVE W-ACQUIRER-ID  TO ACQUIRER-ID    of TXILOG70
     MOVE W-ERFASSUNGS-ART TO ERFASSUNGS-ART of TXILOG70

*kl20180316 - G.06.50 - Unterscheidung zwischen Chip und Spur2
*     MOVE 221            TO KARTEN-ART     of TXILOG70
*G.07.02 - Chip Kontaktlos auch beachten
     IF W-ERF-CHIP OR W-ERF-KONTAKTLOS
*G.07.02 - Ende
*       Kartenart = Chip ohne Cashback
        MOVE   211       TO KARTEN-ART     of TXILOG70
        MOVE   "r"       TO KZ-VERF        of TXILOG70
     ELSE
*       Kartenart = Spur2 Magnet
        MOVE   221       TO KARTEN-ART     of TXILOG70
        MOVE   "f"       TO KZ-VERF        of TXILOG70
     END-IF
*kl20180316 - G.06.50 - Ende
*G.06.56 - Ende



*     IF  W-ERFASSUNGS-ART = 01
*         MOVE "ME"       TO TRANS-ART      of TXILOG70
*     ELSE
*         MOVE "2E"       TO TRANS-ART      of TXILOG70
*     END-IF
*G.06.38 - TRANS-ART anpassen fuer Chip
     EVALUATE W-ERFASSUNGS-ART
        WHEN 01 MOVE "ME"       TO TRANS-ART      of TXILOG70
        WHEN 02 MOVE "2E"       TO TRANS-ART      of TXILOG70
        WHEN 05 MOVE "OC"       TO TRANS-ART      of TXILOG70
        WHEN OTHER CONTINUE
     END-EVALUATE
*G.06.38 - Ende

     IF  PAC-YES
*G.01.06 - Anfang
         MOVE "2 "       TO CVM-RESULT     of TXILOG70
*G.01.06 - Ende
         MOVE "MP"       TO TRANS-TYP      of TXILOG70
     ELSE
         MOVE "OP"       TO TRANS-TYP      of TXILOG70
     END-IF
     MOVE 5542           TO BRANCHEN-KZ    of TXILOG70

*G.06.37 - Anfang
     IF IMSG-TBMP(42) = 1
        MOVE IMSG-CF(IMSG-TPTR(42):IMSG-TLEN(42))
          TO VUNR of TXILOG70
     ELSE
        MOVE VUNR of TSKART40 TO VUNR      of TXILOG70
     END-IF
*G.06.37 - Anfang

     MOVE W-ZP-VERKAUF   TO ZP-VERKAUF     of TXILOG70
     MOVE W-BMP07        TO AF-BMP07       of TXILOG70

*G.06.34 - wenn Chiperfassung, EMV Daten setzen und CVM Result
     IF ERF-CHIP
        MOVE W-BMP55-LEN TO LEN OF EMV-DATEN OF TXILOG70
        MOVE W-BMP55     TO VAL OF EMV-DATEN OF TXILOG70

        PERFORM N100-GET-TAG9F34
     END-IF
*G.06.34

     MOVE TS-TLEN(63)    TO LEN of ARTIKEL of TXILOG70
     MOVE TS-CF(TS-TPTR(63):TS-TLEN(63))
                         TO VAL of ARTIKEL of TXILOG70


**  ---> holen momentanen Zeitpunkt
     PERFORM U200-TIMESTAMP
     IF  FEP-ANTWORT
         MOVE TAGESDATUM TO ZP-TOUT of TXILOG70
     ELSE
         MOVE TAGESDATUM TO ZP-AOUT of TXILOG70
     END-IF
     MOVE H-ZP-IN        TO ZP-TIN  of TXILOG70
     .
 G100-99.
     EXIT.

******************************************************************
* Einstellen Daten in TXNLOG70-TS Buffer
******************************************************************
 G110-PUT-TXNLOG70-TS SECTION.
 G110-00.
     MOVE W-TERMNR (7:2) TO PNR            of TXNLOG70-TS
     MOVE W-TERMNR       TO TERMNR         of TXNLOG70-TS
     MOVE W-TRACENR      TO TRACENR        of TXNLOG70-TS
     MOVE 200            TO ISONTYP        of TXNLOG70-TS
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

******************************************************************
* Einstellen Daten in TXNLOG70-TS Buffer für die Eigenantwort
******************************************************************
 G115-PUT-TXNLOG70-TS-ANF-ANT SECTION.
 G115-00.
     MOVE W-TERMNR (7:2) TO PNR            of TXNLOG70-TS
     MOVE W-TERMNR       TO TERMNR         of TXNLOG70-TS
     MOVE W-TRACENR      TO TRACENR        of TXNLOG70-TS
     MOVE 200            TO ISONTYP        of TXNLOG70-TS
     MOVE "T"            TO KZ-MSG         of TXNLOG70-TS
     MOVE 1              TO ISO-VERF       of TXNLOG70-TS
     MOVE W-MDNR         TO MDNR           of TXNLOG70-TS
     MOVE W-TSNR         TO TSNR           of TXNLOG70-TS
     MOVE K-MODUL        TO LOG-SRV        of TXNLOG70-TS
     MOVE TS-HEADER      TO FREHEADER      of TXNLOG70-TS
     MOVE TS-NDATEN      TO VAL of ANFRAGE of TXNLOG70-TS
     MOVE TS-DATLEN      TO LEN of ANFRAGE of TXNLOG70-TS
     MOVE IMSG-NDATEN    TO VAL of ANTWORT of TXNLOG70-TS
     MOVE IMSG-DATLEN    TO LEN of ANTWORT of TXNLOG70-TS
     .
 G115-99.
     EXIT.

******************************************************************
* Einstellen Daten in TXNLOG70-AS Buffer
******************************************************************
 G120-PUT-TXNLOG70-AS SECTION.
 G120-00.
     MOVE W-TERMNR (7:2) TO PNR            of TXNLOG70-AS
     MOVE W-TERMNR       TO TERMNR         of TXNLOG70-AS
     MOVE W-TRACENR      TO TRACENR        of TXNLOG70-AS
     MOVE 200            TO ISONTYP        of TXNLOG70-AS
     MOVE "A"            TO KZ-MSG         of TXNLOG70-AS
     MOVE 5              TO ISO-VERF       of TXNLOG70-AS
     MOVE W-MDNR         TO MDNR           of TXNLOG70-AS
     MOVE W-TSNR         TO TSNR           of TXNLOG70-AS
     MOVE K-MODUL        TO LOG-SRV        of TXNLOG70-AS

*kl20170929 - G.06.42 - Fregat-Header bearbeiten MEMLOG-Switch
      CALL "WFREH07" USING W-FH-BUFFER.
      MOVE FH-STRING(1:FH-STRLEN) TO FREHEADER of TXNLOG70-AS .
*    Wird nicht mehr gebraucht, wird im Modul gesetzt
*    (funktioniert, da IMSG-... EXTERNAL ist!)
*     MOVE IMSG-HEADER    TO FREHEADER      of TXNLOG70-AS
*kl20170929 - G.06.42 - Ende

     MOVE IMSG-NDATEN    TO VAL of ANFRAGE of TXNLOG70-AS
     MOVE IMSG-DATLEN    TO LEN of ANFRAGE of TXNLOG70-AS
     .
 G120-99.
     EXIT.

******************************************************************
* Einstellen Daten in CRDUSEDN-Buffer
******************************************************************
 G130-PUT-CRDUSEDN SECTION.
 G130-00.
     MOVe W-TERMNR (7:2) TO SDB-PNR
     MOVE W-KANR         TO SDB-KANR
     MOVE "L"            TO SDB-AKZ
     MOVE W-TERMNR       TO SDB-TERMNR
     MOVE W-TRACENR      TO SDB-TRACENR
     MOVE W-AC           TO SDB-AC
     MOVE BETRAG of TXILOG70 TO SDB-BETRAG
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
         PERFORM Z002-PROGERR
     END-IF
     .
 M110-99.
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
                 WHEN ROUT-ASERR MOVE 91 TO W-AC
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
                         SET ENDE TO TRUE
     END-EVALUATE

**  ---> Fehlermeldung vervollständigen, wird nun in Z002-PROGERR gemacht
     IF  W-AC > ZERO or ENDE
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
*kl20160321 -  G.02.01 - Pruefen, ob BIN gesperrt
*                        (ersetzt Vorserver WTPNL06)
******************************************************************
* Aufruf Modul PFCBNS7
******************************************************************
 M180-CALL-PFCBNS7 SECTION.
 M180-00.

** --> Setzen DUMMY-AC, da AC=0 "Ok"
     MOVE 999           TO BS-AC
** --> Aufruf Modul
     CALL "PFCBNS7"    USING BS-PFCBNS7C

** --> Auswertung Ergebnis Nach Aufruf dieser Section
     .
 M180-99.
     EXIT.
*kl20160323 - G.01.03 - Ende


******************************************************************
* Auslesen BMP55-TAG 9F34 fuer CVM-RESULT OF TXILOG70
******************************************************************
*G.06.38 - Neu fuer TAG 9F34 aus BMP 55 holen
 N100-GET-TAG9F34 SECTION.
 N100-00.

**        ---> Uebernehmen fuer Tagfummler
     MOVE W-BMP55       TO TF-BER-TLV-STRING
     MOVE W-BMP55-LEN   TO TF-BER-TLV-LEN

**        ---> CVM-RESULT  suchen
*         soll eigentlich in TAG 9F34 kommen
     MOVE "9F34"         TO TF-SEARCH-TAG
     SET TF-LOOK4TAGXP   TO TRUE
     PERFORM N101-SEARCH-TAG
     IF ENDE
        EXIT SECTION
     ELSE IF TF-NOTFOUND
             MOVE "  "                       TO CVM-RESULT OF TXILOG70
             MOVE "TAG 9F34 nicht gefunden"  TO DATEN-BUFFER1
             MOVE "Verarbeitung geht weiter" TO DATEN-BUFFER2
             PERFORM Z002-PROGERR
          ELSE
             MOVE TF-KAAI-LTV-STRING(2:1) TO CVM-RESULT OF TXILOG70
             EXIT SECTION
          END-IF
     END-IF
     .
 N100-99.
     EXIT.
*G.06.38 - Ende

******************************************************************
* Holen Tagwert ueber WISO400
******************************************************************
*G.06.38 - Neu fuer Tag Suche in BMP 55
 N101-SEARCH-TAG SECTION.
 N101-00.
     SET TF-OK TO TRUE
     CALL "WISO400"    USING TF-WISO400C
     IF TF-OK OR TF-NOTFOUND
        EXIT SECTION
     END-IF
     .
 N101-99.
     EXIT.
*G.06.38 - Ende

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
         MOVE "Programm-Abbruch (PFCPRE7S)"   TO DATEN-BUFFER2
         PERFORM Z002-PROGERR
         SET PRG-ABBRUCH TO TRUE
         EXIT SECTION
     END-IF

     ENTER TAL "WT^ANCNAME" USING FEHL
                                  ANCNAME
                                  PAIRINFO
     IF  FEHL not = ZERO
         MOVE "Ancestor Pathway nicht ermittelbar" TO DATEN-BUFFER1
         MOVE "Programm-Abbruch (PFCPRE7S)"        TO DATEN-BUFFER2
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
* Select auf Tabelle POST70
******************************************************************
 S130-SELECT-POST70 SECTION.
 S130-00.
     EXEC SQL
         SELECT    TERMNR,
                   TERMTYP,
                   SI,
                   MDNR,
                   TSNR
           INTO   :TERMNR  OF POST70
                 ,:TERMTYP OF POST70
                 ,:SI      OF POST70
                 ,:MDNR    OF POST70
                 ,:TSNR    OF POST70
           FROM  =POST70
*G.03.44 - Anfang
          WHERE     PNR = :PNR of POST70
            AND  TERMNR = :TERMNR of POST70
*G.03.44 - Ende
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO
*G.03.44 - Anfang
              SET POST70-OK   TO TRUE
*G.03.44 - Ende
         WHEN OTHER
              SET POST70-NOK  TO TRUE
              SET ENDE TO TRUE
              STRING "Terminal "
                      IMSG-CF(IMSG-TPTR(41):IMSG-TLEN(41))
                     " nicht in =POSTSTD !!!"
                delimited by size
                INTO  DATEN-BUFFER1
              END-STRING
              STRING  "Trace-Nr. "
                       IMSG-CF(IMSG-TPTR(11):IMSG-TLEN(11))
                delimited by size
                INTO  DATEN-BUFFER2
              END-STRING
              PERFORM Z002-PROGERR
     END-EVALUATE

*G.03.44 - Anfang

     IF MDNR OF POST70 NOT = IMSG-MDNR
        STRING "Falsche MDNR in  POST70: ",
          MDNR OF POST70
        DELIMITED BY SIZE INTO DATEN-BUFFER1
        STRING "Bei TERMNR: ",
          TERMNR OF POST70
        DELIMITED BY SIZE INTO DATEN-BUFFER2
        STRING "MandantNr der TS-Anfrage: " ,
          IMSG-MDNR
        DELIMITED BY SIZE INTO DATEN-BUFFER3
        PERFORM Z002-PROGERR
        SET POST70-NOK  TO TRUE
        MOVE 45 TO W-AC
        EXIT SECTION
     END-IF

     IF TSNR OF POST70 NOT = IMSG-TSNR
        STRING "Falsche TSNR in  POST70: ",
          TSNR OF POST70
        DELIMITED BY SIZE INTO DATEN-BUFFER1
        STRING "Bei TERMNR: ",
          TERMNR OF POST70
        DELIMITED BY SIZE INTO DATEN-BUFFER2
        STRING "TSNR der TS-Anfrage: " ,
          IMSG-TSNR
        DELIMITED BY SIZE INTO DATEN-BUFFER3
        PERFORM Z002-PROGERR
        SET POST70-NOK  TO TRUE
        MOVE 45 TO W-AC
        EXIT SECTION
     END-IF

     PERFORM S140-UPDATE-POST70

*G.03.44 - Ende
     .
 S130-99.
     EXIT.

******************************************************************
* Update auf POST70
******************************************************************
 S140-UPDATE-POST70  SECTION.
 S140-00.
     EXEC SQL
         UPDATE  =POST70
            SET  DATUM_LAST = CURRENT  YEAR TO DAY
*G.03.44 - Anfang
          WHERE     PNR = :PNR    of POST70
            AND  TERMNR = :TERMNR of POST70
*G.03.44 - Anfang
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO   SET POST70-OK   TO TRUE
         WHEN OTHER  SET POST70-NOK  TO TRUE
                     SET ENDE TO TRUE
                     MOVE 2003 TO ERROR-NR OF GEN-ERROR
                     STRING  "POST70@"
                             MDNR   of POST70 "/"
                             TSNR   of POST70 "/"
                             TERMNR of POST70
                                 delimited by size
                       INTO DATEN-BUFFER1
                     END-STRING
                     PERFORM Z002-PROGERR
     END-EVALUATE
     .
 S140-99.
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
* Select auf Tabelle STATIONA
******************************************************************
 S170-SELECT-STATIONA SECTION.
 S170-00.
     EXEC SQL
         SELECT    KZ_LAENGE, LAENGE
                 , KZ_BREITE, BREITE
           INTO   :KZ-LAENGE of STATIONA
                 ,:LAENGE of STATIONA
                 ,:KZ-BREITE of STATIONA
                 ,:BREITE of STATIONA
           FROM  =STATIONA
          WHERE  MDNR, TSNR
                 =    :MDNR of STATIONA
                     ,:TSNR of STATIONA
         BROWSE  ACCESS
     END-EXEC
     EVALUATE SQLCODE OF SQLCA
         WHEN ZERO       SET STATIONA-OK
                             GEODATA-YES  TO TRUE
         WHEN 100        SET STATIONA-NOK
                             GEODATA-NO   TO TRUE
                         EXIT SECTION
         WHEN OTHER      SET ENDE
                             GEODATA-NO   TO TRUE
                         EXIT SECTION
     END-EVALUATE

**  ---> Daten zur Verfügung stellen
     MOVE KZ-BREITE of STATIONA TO GEO-KZ-BREITE
     MOVE KZ-LAENGE of STATIONA TO GEO-KZ-LAENGE

     COMPUTE GEO-BREITE-8
           = BREITE of STATIONA
           * GEO-FAKTOR
     ON SIZE ERROR
        MOVE ZEROS TO GEO-BREITE-8
     END-COMPUTE

     COMPUTE GEO-LAENGE-9
           = LAENGE of STATIONA
           * GEO-FAKTOR
     ON SIZE ERROR
        MOVE ZEROS TO GEO-LAENGE-9
     END-COMPUTE

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
*kl20160323 - G.01.03 - Soll BIN-Sperre geprueft werden?
******************************************************************
* Select BIN-Sperrenkonfiguration
******************************************************************
 S200-SELECT-BINSPCFG SECTION.
 S200-00.

     EXEC SQL
         SELECT MDNR
         INTO  :MDNR     OF BINSPCFG
         FROM  =BINSPCFG
         WHERE MDNR, CARDID =
*              Felder können aus TSKART40 verwendet werden,
*              wurden in D900-GET-CARDID gesetzt
              :MDNR      OF TSKART40
             ,:CARDID    OF TSKART40
         BROWSE ACCESS
     END-EXEC

     EVALUATE SQLCODE OF SQLCA

        WHEN   ZERO      SET BS-ON   TO TRUE
        WHEN   100       SET BS-OFF  TO TRUE
        WHEN   OTHER     SET BS-ERR  TO TRUE
                         INITIALIZE GEN-ERROR
                         MOVE  SQLCODE OF SQLCA    TO D-NUM4
                         STRING "Fehler bei LESEN BINSPCFG: ", D-NUM4
                         DELIMITED BY SIZE INTO DATEN-BUFFER1
                         STRING "MDNR= ",
                                 MDNR OF BINSPCFG,
                                 " / CARDID= ",
                                 CARDID OF BINSPCFG
                         DELIMITED BY SIZE INTO DATEN-BUFFER2
                         MOVE " -- Keine Prüfung auf BIN-Sperre -- "
                              TO DATEN-BUFFER3
                         PERFORM Z002-PROGERR

     END-EVALUATE
     .
 S200-99.
     EXIT.
*kl20160323 - G.01.03 - Ende

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
* Select auf neue AIID-Tabelle
*G.07.01 - neu
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

     EVALUATE SQLCODE OF SQLCA
        WHEN 0      CONTINUE
** ---> Dummy AIID, fuer RoutKZ, die nicht gepflegt sind
        WHEN 100    MOVE "000000" TO AIID OF FCAIID
        WHEN OTHER  MOVE  SQLCODE OF SQLCA    TO D-NUM4
                    STRING "Fehler bei LESEN FCAIID: ", D-NUM4
                    DELIMITED BY SIZE INTO DATEN-BUFFER1
                    MOVE ROUTKZ OF KEYNAMEN TO D-NUM4M
                    MOVE CARDID OF KEYNAMEN TO D-NUM4N
                    STRING "ROUTKZ= ",
                            D-NUM4M,
                            " / CARDID= ",
                            D-NUM4N,
                            " oder 0"
                    DELIMITED BY SIZE INTO DATEN-BUFFER2
                    PERFORM Z002-PROGERR
     END-EVALUATE

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
*                    PRM-NOT-FOUND - false of true
******************************************************************
 U300-SEARCH-TAB SECTION.
 U300-00.

*kl20180405 - G.06.51 - wg. Cardid ZERO zurueck zur klassischen
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
*kl20180405 - G.06.51 - Ende
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
*G.07.01 - VERF-AS jetzt
*                         MOVE W-ROUTKZ TO D-NUM4
                         MOVE VERF-AS TO D-NUM4
*G.07.01 - Ende
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
*G.07.01 - Fehlermeldung erweitert
     MOVE W-CARDID TO D-NUM2
     MOVE W-ROUTKZ TO D-NUM4
     STRING  "TermNr/TraceNr/RoutKZ/Verf/Cardid: "
             W-TERMNR "/" W-TRACENR "/" D-NUM4 "/"
             VERF-AS  "/" D-NUM2
                 delimited by size
       INTO  DATEN-BUFFER5
     END-STRING
*G.07.01 - Ende

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
