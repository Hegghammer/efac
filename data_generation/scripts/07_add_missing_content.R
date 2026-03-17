# ----------------------------------------
# Patch known OCR content omissions
#
# Inserts or replaces specific remark segments that
# were missed during OCR/parsing and reindexes rows.
# ----------------------------------------

library(tidyverse)
library(fs)
library(glue)

df <- read_csv("out/remarks.csv")

# Add missing content ----------------------------------------

# function to insert rows
insert_row_at <- function(df, row, n) {
  # Split the data frame
  df_part1 <- df[1:(n - 1), ]
  df_part2 <- df[n:nrow(df), ]
  # Combine the parts with the new row
  df <- rbind(df_part1, row, df_part2)
  return(df)
}

# 1983_1122: replace index 11 + insert Formannen ----------------------------------------
# check; should start "Det er vel ingen"
df$remark[df$filename == "1983_1122.pdf" & df$index == 11]

replacement <- "Det er vel ingen overraskelse at vi helst hadde sett at man hadde stemt for forslaget. Men når det er sagt, vil jeg jo si at jeg opplever det at man nå skal avholde seg, som et stort og viktig skritt i riktig retning. Jeg vil jo for sikkerhets skyld også ha sagt at jeg ikke uten videre med det som utenriksministeren nå har sagt, vil avskrive meg muligheten for å få markere vårt prinsipale standpunkt i Stortinget i en eller annen sammenheng."

df$remark[df$filename == "1983_1122.pdf" & df$index == 11] <- replacement

pos <- which(df$remark == replacement) + 1

insert <- data.frame(
  filename = "1983_1122.pdf",
  date = "1983-11-22",
  index = NA,
  speaker_verbatim = "FORMANNEN",
  remark = "Eg har satt opp meg sjølv - eg skal heller ikkje tala lenge. Men eg er også tilfreds med denne utviklinga. Vi hadde ein debatt om dette i fjor, og eg ymta vel då om at eg godt kunne tenkt meg at ein også dả kunne ha avstått, utan at eg ville gjera noko anna enn å nemna det som eit råd i fjor. men eg er glad for at dette er Regjeringa sin politikk i år. Eg vil høyra om det er andre som vil be om ordet, eller om utanriksministeren har noko å seia i tillegg til slutt. Ingen har spørsmål i tillegg til det som er framkome, og vi er da ferdige med sak nr. 1."
)

df <- insert_row_at(df, insert, pos)

# 1976_0211: replace index 1 + insert Frydenlund ----------------------------------------
# check; should start "Da utenrikskomiteens valgte sekretær"
df$remark[df$filename == "1976_0211.pdf" & df$index == 1]

replacement <- "Da utenrikskomiteens valgte sekretær, Otto Lyng, er sykepermittert, har komiteen konstituert Paul Thyness som settesekretær. Han vil også fungere som sekretær i den utvidede. Møtet er innkalt etter anmodning fra utenriksministeren for å behandle de to sakene som er oppført på den skriftlige dagsorden. Jeg gir da straks ordet til utenriksministeren til sak nr. 1. "
df$remark[df$filename == "1976_0211.pdf" & df$index == 1] <- replacement

pos <- which(df$remark == replacement) + 1

insert <- data.frame(
  filename = "1976_0211.pdf",
  date = "1976-02-11",
  index = NA,
  speaker_verbatim = "UTENRIKSMINISTER KNUT FRYDENLUND",
  remark = "La meg først kort skissere problemstillingen. Styret i Det internasjonale energibyrå vedtok i slutten av januar et langtidsprogram. Dette langtidsprogrammet har Norge ikke tiltrått. Vi kan, om vi ønsker det, tiltre på et senere tidspunkt, og medlemslandene i IEA har gjennom formannen i styret uttrykt håp og ønske om at Norge i nær framtid vil kunne tiltre programmet. Hva er så bakgrunnen for at vi ikke fant å kunne tiltre programmet på det nåværende tidspunkt? Da må vi se på dette langtidsprogrammet, som består av en rekke elementer, og de som i denne sammenheng er viktige for oss og for vår vurdering, er bestemmelsene om minsteprisordning og bestemmelsene om ikke-diskriminerende behandling. Hele langtidsprogrammet er en pakke. Minsteprisordningen er klart i Norges interesse. Minsteprisen er lagt på 7 dollar pr. fat, men foreløpig er denne minsteprisordningen såpass løst utformet at vi ikke har noen garanti for hvordan den vil virke i praksis. Men i prinsippet er den altså klart i Norges interesse. På den annen side har man kapitlet i langtidsprogrammet hvor det gis uttrykk for ønskeligheten av at konsumentlandenes borgere gis nasjonal behandling når det gjelder utnyttelse av energi i produsentland. Man skal ifølge dette kapittel gjøre sitt ytterste for ikke å innføre nye diskriminerende lover og bestemmelser. Dette kapittel rammer ikke allerede eksisterende lover, og det rammer ikke landenes rett til å ha kontroll og full eiendomsrett over sine naturressurser. Kapitlet er heller ikke juridisk bindende, men vil legge et moralsk press på deltakerland, bl.a. gjennom periodiske eksaminasjoner, så selv om kapitlet er uforpliktende utformet, reiser kapitlet spørsmål som vi må vurdere nærmere før vi kan ta endelig standpunkt. Det vi står overfor i denne saken, er tre alternativer: Vi kan la være å slutte oss til i det hele tatt. Dette vil være en belastning i forhold til medlemslandene i IEA, men også en ulempe for oss selv, fordi det er mange elementer i dette langtidsprogrammet som er i vår interesse. Vi kan slutte oss til programmet som det er, med den risiko at vi da går inn på ting som vi i dag ikke har den hele oversikt over. Det tredje alternativet er at vi bruker tid på å vurdere virkningene av dette langtidsprogrammet for Norge og norsk oljepolitikk og undersøker mulighetene for å ta eventuelle forbehold der hvor det kan være nødvendig, der hvor langtidsprogrammet kan skape problemer for utformingen av norsk oljepolitikk. Denne sak er behandlet av Regjeringen, som i prinsippet inntar en positiv holdning til spørsmålet om norsk tiltredelse til langtidsprogrammet, men anbefaler at vi vurderer nærmere spørsmålene om virkningene for Norge, først og fremst av bestemmelsene om ikke-diskriminering, for så eventuelt å komme med forslag overfor IEA om forbehold som Norge vil be om å få inn. Vi har, skulle jeg anta, en 5 måneders tid på denne operasjon. I juli vil vi møte spørsmålet igjen for alvor, og vi vil også møte det i mellomtiden. Jeg skal til Belgia i dag, og formannen i styret, som er belgier, vil da ta opp dette spørsmålet med meg. Jeg er interessert i å kunne meddele ham disse foreløpige betraktninger fra vår side."
)

df <- insert_row_at(df, insert, pos)

# 1976_0107: replace index 7 + insert Gustavsen ----------------------------------------
# check; should start "Jeg må si at det er foruroligende"

df$remark[df$filename == "1976_0107.pdf" & df$index == 7]

replacement <- "Jeg må si at det er foruroligende opplysninger vi får her. Selv om man bare kan anføre fire sikre krenkelser i tidsrommet 1969 - 1975, antyder man et nokså stort antall mulige krenkelser, og det er meget foruroligende. Ut fra det vil jeg si prinsipielt at reglene bør skjerpes. Jeg er enig med Thyness i at det er vanskelig for oss å ha noen sikker formening om i hvilken grad reglene er skjerpet. Jeg forstår at i indre lukket farvann har man klart skjerpet dem, idet det her ikke skal gis noe varsel. Det spørsmål som da står åpent, er hvorvidt man skal ha samme regler innenfor 4-milsgrensen og først utenfor den skal operere med de vanlige varsel. Det er et spørsmål som Regjeringen får vurdere på nytt. I alle tilfelle er det åpenbart at vi må ha skjerpede regler i forhold til de vi nå har. Det ble sagt av forsvarsministeren at man anså det ikke nødvendig med en ny kgl. resolusjon. Hvis jeg har oppfattet det riktig, er den kgl. resolusjon av 19. Januar 1951 generell med hensyn til hvordan man skal forholde seg for å tvinge båtene opp osv.; regelverket er altså ikke gitt ved den kgl. resolusjon. Den kgl. resolusjon er følgelig slik formet at regelverket utmerket godt kan endres uten at det blir gitt en ny kgl. resolusjon. På den annen side forekommer det meg at hvis man går til vesentlige skjerpelser av reglene, vil det ha sin betydning om det ble gitt en ny kgl. resolusjon, slik at det også i den ble gitt klarere uttrykk for at man vil holde en mer skjerpet linje."

df$remark[df$filename == "1976_0107.pdf" & df$index == 7] <- replacement

pos <- which(df$remark == replacement) + 1

insert <- data.frame(
  filename = "1976_0107.pdf",
  date = "1976-01-07",
  index = NA,
  speaker_verbatim = "FINN GUSTAVSEN",
  remark = "Jeg er enig med Thyness i at fremmede ubåter iallfall inne i fjorder definitivt er en fiendtlig handling og bør betraktes som det. Og da er jeg i tvil om riktigheten av at det skal innhentes tillatelse fra høyere myndigheter før man reagerer. Jeg vil gjerne vite hvordan man får tillatelse til å ha fremmede ubåter i norske fjorder, hvorvidt det er behov for at de befinner seg i neddykket tilstand idet de kommer inn på norsk sjøterritorium og beveger seg på norsk territorium, og hvilket omfang dette har at ubåter fra andre land har tillatelse til å befinne seg i neddykket tilstand på norsk sjøterritorium. Det synes jeg er nokså fundamentalt, for å si det rett ut. Hvis det er slik at en fartøysjef og lokale militære myndigheter skal innhente tillatelse for å gjøre det som vel vanligvis anses for å være det eneste riktige når det begås en fiendtlig handling, en militær inntrenging på norsk område, foreligger det jo så avgjort den mulighet som jeg tror unnskyld - at man vil benytte seg av, at man undersøker i visse land om det kunne være båter fra disse landene. Og da vil også mistanken være der om at det kan komme til å bli forskjellsbehandling. I likhet med Thyness igjen hadde jeg på forhånd det inntrykk at svenskenes regler er strengere, og at de er offentlig kjent, og jeg heller foreløpig til at reglene bør offentliggjøres temmelig klart og slik at alle makter - vennligeller fiendtligsinnet - har helt klar beskjed om norske reaksjoner på dette. Jeg vil bare gjenta at jeg iallfall når det gjelder ubåter inne i norske fjor der stiller meg skeptisk til at en militær ansvarshavende skal være nødt til å gå helt til forsvarsministeren som da må undersøke nærmere før han kan reagere, og da vil ubåten ha alle muligheter til å komme seg avgårde, særlig hvis man skal undersøke hvor den kommer fra."
)

df <- insert_row_at(df, insert, pos)

# 1976_0107: replace index 12 + insert Fostervoll ----------------------------------------

# check; should start "Men i dag varsles"

df$remark[df$filename == "1976_0107.pdf" & df$index == 12]

replacement <- "Men i dag varsles det to ganger, ikke sant?"

df$remark[df$filename == "1976_0107.pdf" & df$index == 12] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1976_0107.pdf",
  date = "1976-01-07",
  index = NA,
  speaker_verbatim = "STATSRÅD ALV JAKOB FOSTERVOLL",
  remark = "Ja. Når det gjelder indre lukket farvann, er det der skjedd en betydelig skjerpelse ved at man går direkte til angrep med tilgjengelige våpen, unntatt de mest drastiske våpen, altså torpedo, som har til hensikt å senke ubåten. La det vare helt klart at det er bare ved bruk av torpedo at sannsynligheten er meget, meget høy for at ubåten blir senket, og vi finner det ikke tilrådelig å gå til et så drastisk skritt uten at den politiske ledelse har hatt høve til å vurdere den politiske situasjon som eksisterer i øyeblikket. Korvald kommenterte resolusjonen av 1951, og det er vår oppfatning at resolusjonen av 1951 hjemler adgang til å endre regelverket. Det står ikke detaljert i resolusjonen og det kan selvsagt vare en smakssak hvorvidt en skal lage en ny kongelig resolusjon. Vi har altså ikke funnet det nødvendig i denne omgang, fordi den som ligger der, hjemler de tiltakene som vi nå setter i verk. Jeg går ut fra at jeg med dette har besvart en del av Gustavsens spørsmål. Når det gjelder spørsmålet om offentliggørelse, er det slik å forstå at vi akter å offentliggøre hovedinnholdet i skjerpelsen, men vi akter ikke å offentliggjøre reglene detaljert, fordi ethvert regelverk kan omgås, og skulle det legges detaljert fram, vil en ubåtsjef kunne operere på våre premisser så å si, på en slik måte at han kan spille vår marine ut. Det er uhyggelig vanskelig å finne disse ubåtene og å holde dem, og det er årsaken til at vi vegrer oss mot en offentliggørelse av det detaljerte regelverk, fordi vi da spiller en del gode kort i hendene på ubåtsjefen som han ikke bør ha når han opererer i norsk farvann. Det er ikke vår hensikt ved denne konsultasjon på politisk nivå å undersøke i noen grad hvor ubåten kommer fra. Det er for å vurdere den situasjon som oppstår ved et angrep med stor sannsynlighet for senkning, at reglene pålegger å konsultere den politiske ledelse. Det er klart at regelverket gjelder uansett nasjonalitet for de ubåter som krenker norsk territorium. Når det gjelder ytre farvann og hr. Steenbergs mening om at også ubåtene i ytre farvann bør oppbringes, er det forutsatt at vi har sikker identitet på ubåten når den utvises. Er det tvil om dette, bør den bringes opp. Jeg vet ikke om jeg har fått med meg det meste av det som det ble spurt om?"
)

df <- insert_row_at(df, insert, pos)

# 1976_0513: insert short Berit As turn ----------------------------------------

pos <- which(df$filename == "1976_0513.pdf" & df$index == 12 & df$speaker_verbatim == "UTENRIKSMINISTER KNUT FRYDENLUND") + 1
pos

insert <- data.frame(
  filename = "1976_0513.pdf",
  date = "1976-05-13",
  index = NA,
  speaker_verbatim = "BERIT ÅS",
  remark = "Takk."
)

df <- insert_row_at(df, insert, pos)

# 1976_0120: replace index 4 + insert Furre ----------------------------------------

# check; should start "Jeg synes totalinntrykket"
df$remark[df$filename == "1976_0120.pdf" & df$index == 4]

replacement <- "Jeg synes totalinntrykket av det bilde utenriksministeren tegnet, ikke er helt hyggelig. Særlig saken lillejulaften tyder på at russerne har valgt å forsøke å sette oss overfor fullbyrdede kjensgjerninger, i strid med inngåtte avtaler og muligens for a prøve alvoret i Norge på bakgrunn av de uttalelser som falt i den debatten vi hadde om Svalbard for nylig. Slik sett var den måten de gjorde det på lillejulaften, veldig godt utspekulert. Jeg har allerede lagt merke til at opinionen i Norge forstår ingenting av at ikke russerne skal få ha koner med, og det er jo rimelig. Og slik sett har det vært godt utspekulert psykologisk det hele. Det jeg gjerne vil spørre om, er om den instruks sysselmannen har, er helt klar, og om man fulgte den instruksen. Jeg la merke til at utenriksministeren sa at sysselmannen protesterte, men at de flyttet inn likevel. Jeg vet ikke hvordan instruksen er på det punkt, men det forekommer meg at fra norsk synspunkt hadde det enkleste vært at man hadde anvendt den makt man har som politi, og sagt: Beklager, men jeg har ikke anledning til å la dere komme i land på Svalbard flyplass og flytte inn i strid med kontrakten! Dette er litt uklart for meg; det er derfor jeg stiller spørsmålet."

df$remark[df$filename == "1976_0120.pdf" & df$index == 4] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1976_0120.pdf",
  date = "1976-01-20",
  index = NA,
  speaker_verbatim = "BERGE FURRE",
  remark = "Eg synest det måtte vera imeleg å møta kvar av desse sakene på ein sakleg og praktisk måte på grunnlag av traktaten og på grunnlag av rimeleg vett og forstand, ut frå ønsket om eit godt granneforhold til Sovjet, og at ein ikkje skal sjå press og trugsmål der det 1kkje er nødvendig å sjå det. Vitskapeleg aktivitet kan 1kkje vera så farleg. Det er sjølvsagt at ein må finna ut kva slag vitskapeleg ekspedisjon det er tale om, men det er i høg grad rimeleg at det blir drive vitskapeleg aktivitet i polområda med utgangspunkt på Svalbard. Ein må sjå å finna ut kva slag ekspedisjon det er tale om, og ein må krevja at i den grad ekspedisjonen nyttar norske lokalitetar, må han halda seg innanfor det omfang dei gjev rom for. Men det kan 1kkje vera rett å leggja opp ein politikk som legg vanskar i vegen for seriøs forskning. Når det gjeld Iufthamnsjefens problem med teneste utover traktaten, er det vanskeleg å vurda kva som ligg bak der. Det er vel også rimeleg at slike spørsmål blir løyste med velvilje innanfor traktaten på rimeleg måte så langt råd er. Og fisk bør dei vel få lov til å eta, dei som andre. Det eg er noko bekymra for her, er desse konene med dei fire dobbeltsengene. Det er nokså utspekulert, og det er ei lei sak for Norge dette. Eg må seia at dersom denne saka 1kkje blir løyst på ein rimeleg måte, ser eg 1kkje med glede fram til karikaturteikningane i internasjonal presse. Eg må seia at det var ønskjeleg om ein kunne finna ei minneleg ordning der, og at ein valde ei anna sak som prøvesak for den norske vilje til å stå fast på sine rettar på Svalbard. Denne saka er 1kkje god å prosedera, iallfall 1kkje når ein står overfor ein internasjonal opinion. Det er kanskje endå vanskelegare enn å stå overfor den norske opinionen. Utanriksministeren nemnde i parentes at ein skulle ha plass til to større fly der, som kan drive elektronisk etterretning frå Svalbard. Eg kunne ha hug til å spørja: Er 1kkje dette problematisk i forhold til traktaten? Det er vel her snakk om militær aktivitet, eller noko i nærleiken av militær aktivitet?"
)

df <- insert_row_at(df, insert, pos)


# 1976_0120: replace index 15 + insert Bratteli ----------------------------------------

# check; should start "Jeg er enig med formannen"
df$remark[df$filename == "1976_0120.pdf" & df$index == 15]

replacement <- "Jeg er enig med formannen i at vi må se det hele i et sammenhengende mønster. Men det er en annen sak jeg hadde lyst til å stille et spørsmål om, som kanskje hører med i det samme bilde. Da vi var på Svalbard i sommer, fikk jeg inntrykk av en utbredt misnøye innen de norske myndigheter også med den såkalte postgangsaken, hvor russerne efter manges mening hadde gått lenger enn de hadde rettigheter til når det gjaldt sin egen postordning. Jeg vil gjerne spørre utenriksministeren om dette spørsmålet nu er løst, eller om det fortsatt er et stridsspørsmål mellom norske og sovjetiske myndigheter."

df$remark[df$filename == "1976_0120.pdf" & df$index == 15] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1976_0120.pdf",
  date = "1976-01-20",
  index = NA,
  speaker_verbatim = "TRYGVE BRATTELI",
  remark = 'Så vidt jeg skjønner, etter det som opplyses, dreier dette seg om spesialbygde hybelleiligheter til et begrenset antall mennesker som skal kunne bo på flyplassen. Antall russere på flyplassen er kjernen i dette og ikke spørsmålet om menn eller kvinner, ektefeller, papirløse ekteskap eller hva det nå måtte være. Og spørsmålet om hvilket antall mennesker russerne skulle ha på flyplassen, var hele tiden et av de vanskeligste under forhandlingene om deres rettigheter der. Dette antallet ble da begrenset til 5-6, som så vidt jeg skjønner, hvis det er så at de kommer med ektefeller alle sammen, nå blir doblet. Dette var altså et konkret forhandlingspunkt under forhandlingene om russernes rettigheter i forbindelse med flyplassen, ikke i forbindelse med Svalbard generelt. De kan ta med seg så mange ektefeller de vil til Svalbard, men dette dreier seg om hvor mange som så å si skal kunne oppholde seg på flyplassen. Likevel er jeg helt enig i at en sak som akkurat denne med ektefellene blir ikke helt lett å forklare, selv om vi ved en voldgiftsdom vinner den rent juridisk. Noe helt annet er - og nå skal jeg fatte meg i korthet; vi kan kanskje komme tilbake og gå inn på dette i en annen sammenheng: Min oppfatning etter behandlingen av Svalbardsaken er at vi i Norge bør gjøre oss klar over visse hovedpunkter av betydning for suverenitetsutøvelsen som vi skal være vaktsomme med. Men vi skal ikke drive med pirk i et område som Svalbard. Jeg tror det siste er en forutsetning for det første. Det annet jeg gjerne vil si, er at vi er ikke interessert i å la oss vikle inn i et dårlig forhold til Sovjetsamveldet. Vi er tvert imot interessert i å ha et rimelig åpent forhold til Sovjetsamveldet generelt sett, på grunn av mange forhold. Derfor skal en også være varsom med å la seg provosere - og jeg holdt på å si at ikke minst hvis visse skritt er forsøksvise provokasjoner, skal en være veldig varsom med å la seg provosere; da gjelder det fremfor alt å holde hodet kaldt. Men hovedpunktet for meg ved håndteringen av saker av denne art er at en foretar en realistisk vurdering av hva som virkelig er hovedpunktet, det viktige spørsmålet i forbindelse med suverenitetsutøvelsen, og at en så lar alt pirk fare. Jeg nevner også i denne sammenheng, selv om det ikke har noe med denne sak å gjøre, at jeg liker ikke at de har hatt sin ambassadør hjemme i det meste av den tiden han har vært utnevnt her. I virkeligheten var han uvanlig lenge hjemme også i fjor sommer. Og når det gjelder denne siste situasjonen, er det kommet relativt uventet at han nå har vært kalt hjem igjen og er der borte. Jeg bare stiller spørsmålet - kaster det ut i luften - om vi kanskje burde overveie fra norsk side nå å prøve å få luftet ut, hvis det er noe under utvikling her, og om vi kanskje burde friske opp igjen den i flere år hvilende innbydelse til utenriksminister Gromyko. Det er mye lettere å snakke "på toppen" om slikt som dette enn det er å snakke med mange av de andre.'
)

df <- insert_row_at(df, insert, pos)


# 1980_0430: replace index 9 + insert Jakobsen ----------------------------------------
# check; should start "Selv om fiskeriministeren"
df$remark[df$filename == "1980_0430.pdf" & df$index == 9]

replacement <- "Selv om fiskeriministeren avstod fra noen innledende kommentarer, har jeg allikevel lyst til å spørre ham om han vil spå litt om Fiskarlagets reaksjon på det som nå ble skissert som opplegg fra norsk side. Hvilke reaksjoner kan vi forvente? På hvilke punkter kan de komme? Og i det hele tatt: Kan han si litt om holdningene i Norges Fiskarlag nå til det som hittil er skjedd under forhandlingene."

df$remark[df$filename == "1980_0430.pdf" & df$index == 9] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1980_0430.pdf",
  date = "1980-04-30",
  index = NA,
  speaker_verbatim = "JOHAN J. JAKOBSEN",
  remark = "Bare noen få kommentarer. Jeg vil gjerne støtte opp under den tanke at en her med alle krefter bør satse på en forhandlingsløsning. Selv om det er vanskelige forhandlinger, og det er vanskelige avveininger mellom det ideelle og det mulige, bør vi fortsatt se på en ensidig soneopprettelse som en nødløsning, som bare kan komme på tale i tilfelle alle andre utveier ikke viser seg å vare mulige. Jeg går også ut fra at det i en forhandlingsløsning ikke vil innga noe som kan ha form av avgivelse av suverenitet, at en da heller, som en langt bedre løsning, viser imøtekommenhet når det gjelder å gi Island muligheter for fiske innenfor sonen. Utover det har jeg ikke noen kommentarer: Jeg forstår det slik at opplegget vil være at en satser på en forhandlingsløsning, og at en ensidig soneopprettelse uten at det skjer i forståelse med Island, vil vare en nødløsning som først kan komme i betraktning i aller siste fase."
)

df <- insert_row_at(df, insert, pos)

# 1982_1123: replace index 19 only ----------------------------------------

# check; should start "Det var en bemerkning til"
df$remark[df$filename == "1982_1123.pdf" & df$index == 19]

replacement <- "Det var en bemerkning til komiteen. Jakobsen får ordet."

df$remark[df$filename == "1982_1123.pdf" & df$index == 19] <- replacement

# 1982_0127: replace index 13 + insert Stray ----------------------------------------

# check; should start "Er det flere som har spørsmål"
df$remark[df$filename == "1982_0127.pdf" & df$index == 13]

replacement <- "Er det flere som har spørsmål til utenriksministeren før jeg gir ham ordet? - Det ser ikke ut til å være tilfellet. Utenriksministeren har ordet."

df$remark[df$filename == "1982_0127.pdf" & df$index == 13] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1982_0127.pdf",
  date = "1982-01-27",
  index = NA,
  speaker_verbatim = "UTENRIKSMINISTER SVENN STRAY",
  remark = 'Takk for det. Det som ble sagt fra Jakobsens side, at vi må reagere like sterkt på brudd på menneskerettighetene uansett hvor de foregår, er det vel i og for seg bred enighet om, tror jeg. Jeg vil ikke dermed ha sagt at vi alltid gjør det, eller at det alltid er like lett å etterleve det. En skal være klar over at de mest alvorlige brudd på menneskerettighetene i dag foregår formodentlig verken i Polen eller i Tyrkia, men i Iran. Vi får alle sammen gå i oss selv og si at vi reagerer svært lite på det. Fra norsk side har vi foretatt en del fremstøt for å forsøke å få samlet en gruppe land som kan gå til noe så beskjedent som å få brakt dette inn for FNs menneskerettighetskommisjon. Selv det viser seg vanskelig. Jeg vil bare nevne det. Vi ser lite reaksjoner på det i våre media. Men det er ingen tvil om at det er der menneskerettighetene i dag krenkes på den mest fundamentale måte. Når det gjelder reaksjonene fra norsk side overfor Tyrkia i forhold til overfor Polen, er vel saken den at vi hittil har reagert sterkere overfor Tyrkia enn overfor Polen. På et tidligere tidspunkt innstilte vi all økonomisk bistand til Tyrkia. Det har vi altså gjort i langt mindre omfang overfor Polen, noe jeg også skal komme tilbake til i neste sak. Det ble stilt spørsmål om når Europarådets behandling vil være ferdig. Forholdet er at det vi har hatt i tankene når vi har sagt at vi vil avvente behandlingen i Europarådet, er den behandling som i disse dager pågår i parlamentarikerforsamlingen i Strasbourg, og som vel er avsluttet i løpet av uken. Vi vil da formodentlig i løpet av neste uke - jeg går i hvert fall ut fra at det ikke blir mye senere enn det - få både rapportene og referatene fra debatten der nede. Når det gjelder de kontakter vi har hatt med deøvige nordiske land, og det som ble sagt i nyhetene i dag, kan jeg si at jeg også hørte disse nyhetene. Jeg oppfattet det som ble sagt, som et litt klosset uttrykk for at man mente å si at de nordiske delegater i Strasbourg nå hadde bestemt seg for å stemme for dette. Det var bare en litt klosset måte å formulere det på. Uten at det har noen forbindelse med det som ble sagt i nyhetene i dag, har det vært kontakt mellom den danske utenriksminister og meg om denne saken, og vi var begge enige om at før vi gikk til videre konsultasjoner oss imellom eller også med andre land, var det riktig å avvente det som nå pågår i Strasbourg. Hvis vi imidlertid skal gå til det skritt å bringe saken inn for menneskerettighetskommisjonen, tror jeg det er nokså maktpåliggende at det da er en gruppe land som går sammen om det. Som jeg nevnte i min redegjørelse, er det da tanken at vi konfererer om dette, og at vi konsulterer både Danmark og Sverige. Jeg syns det ville være naturlig å konferere også med Nederland, som har vært meget opptatt av dette spørsmålet, og likeledes med Vest-Tyskland. Jeg har for så vidt allerede nevnt dette for den vesttyske utenriksminister ved en tidligere anledning, og vi ble enige om at vi skulle holde kontakt om saken. Så meget om det. Så til dette spørsmålet om hva fremtredende representanter for den demokratiske opposisjon i Tyrkia har gitt uttrykk for. Jeg må her ta det forbehold at jeg ikke personlig har snakket med mange, og naturligvis har de nordmenn som rapporterer til oss om dette, heller ikke snakket med alle. Der er det selvfølgelig en naturlig begrensning, og noe annet vil aldri være mulig. Men samtlige av de rapporter som er kommet inn til oss fra fremtredende medlemmer av den demokratiske opposisjon, går ut på at man har frarådet at man stelte seg slik at det ble brudd mellom Tyrkia og Europarådet. De har ikke sagt at man ikke skulle gå til noe tiltak, men de har frarådet at man stelte seg slik at det ble brudd mellom Europarådet og Tyrkia. Det tror alle ville være uheldig både for muligheten til å påvirke utviklingen og for stemningen i det hele hos de nåværende makthavere i Tyrkia. Man mener da at Tyrkia ikke skal ekskluderes eller suspenderes fra Europarådet. Heller ikke ønsker man at det skal tas skritt som fører til at Tyrkia selv trekker seg ut. Det er hovedsynspunktene hos alle som vi har vært i kontakt med. Vi kan i grunnen likevel beklage at vi holder dette møtet mens Europarådets delegasjon er i Strasbourg. Vi gjorde jo spesielle foranstaltninger for at vi skulle få med to stortingsrepresentanter i den delegasjonen som drog til Tyrkia, og de har da førstehånds kjennskap til dette og kunne ha bidratt til å orientere oss om det. La meg imidlertid også få lov til å si når det gjelder spørsmålet om hva den demokratiske opposisjon i Tyrkia mener om hvorvidt det ville være riktig å inntale Tyrkia for menneskerettighetskommisjonen, at da er meningene delte. Dere kunne kanskje få et godt inntrykk av dette hvis jeg rett og slett leste opp den rapport vi har fått fra vår ambassade i Ankara om en samtale med en fremtredende tyrkisk menneskerettighetsforkjemper og sosialistisk orientert politiker, nemlig professor Mumtaz Soysal. Jeg tror jeg vil gjøre det. Han er nemlig, som dere vil høre, for at man skal innbringe saken for menneskerettighetskommisjonen, men jeg tror det kan være av interesse at jeg leser dette, så får man se hvor vanskelige avveiningene her er. Jeg tror det er bedre enn lange foredrag fra min side. Og jeg leser fra rapporten: "Professor Mumtaz Soysal vil være kjent som en av Tyrkias fremste menneskerettighetsforkjempere og sosialister. Soysal satt i fengsel under militærregimet i 1971. Han har siden hatt fremtredende tillitsverv i Amnesty International. Som grunnlovsekspert var han hovedarkitekten bak den tyrkiske grunnloven av 1961. Han ble senere rådgiver for statsminister Ecevit. Blant tyrkiske demokratiske sosialister er det delte meninger om hvorvidt det vil være klokt å anlegge sak mot Tyrkia for menneskerettighetskommisjonen. Mange frykter at militærregimet da vil trekke seg ut av Europarådet, og at man dermed åpner for en utvikling som de demokratiske krefter i Tyrkia vil være lite tjent med. Prof. Soysal er imidlertid blant dem som mener at en sak for menneskerettighetskommisjonen vil være ønskelig. Dette syn ga han bl.a. nylig uttrykk for i sin faste spalte i avisen "Milliyet"" - det er en uavhengig/liberal avis. "Ambassaden hadde 24.1. en lengre samtale med prof. Soysal. Soysal sa en sak for kommisjonen antagelig ville ha størst betydning for menneskerettighetenes stilling i Tyrkia på sikt. Han regnet med at en evt. sak ville bli langvarig (Tyrkia godkjenner som kjent ikke domstolen) og i hovedsak falle sammen med gjeninnføringen av demokratiet. Dermed ville den virke som pressmiddel i denne viktige perioden. Samtidig ville en sak også skape mer publisitet om menneskerettighetene og dermed skape holdninger på sikt. Det største usikkerhetsmomentet lå i hva de militære ville gjøre dersom Tyrkia ble stevnet. En tyrkisk utmeldelse fra Europarådet ville være et alvorlig tilbakeslag. Soysal fryktet bl.a. at latente antieuropeiske og pro-islamske følelser i så fall ville komme til overflaten og bli utnyttet. Dersom det ble aktuelt å reise sak, besto derfor problemet i å "sukre pillen" tilstrekkelig til at generalene kunne svelge den uten å overreagere. Problemet var at de militære anså seg selv for å ha reddet landet og kunne ikke skjønne at de fortjente "straff" for en slik handling. For å minimalisere sjansene for en tyrkisk utmelding mente professor Soysal derfor at man burde være nøye med fremgangsmåten ved en evt. stevning. Han pekte i denne forbindelse bl.a. på flg. forhold: - En anbefaling om sak i resolusjons form for Europarådet ville være betenkelig fordi det ville fremstå for de militære som en bastant europeisk avvisning av Tyrkia. - Det beste ville være om kun ett eller to land (som Tyrkia anså seg å ha gode forbindelser med) gikk til sak, og at beslutningen ble presentert som et kompromiss og ønske om å la en upartisk, uavhengig institusjon se på de fremsatte anklager om brudd. - Forut for offentliggjørelse av en beslutning burde man gi seg god tid til å bearbeide og forberede tyrkiske myndigheter slik at man ikke risikerte overreaksjon. - Den økonomiske bistand burde fortsette. Dersom man i tillegg til stevning fikk en rekke andre sanksjoner ville sannsynligheten for utmeldelse stige. Soysal oppsummerte med å si at han trodde en kommisjonsbehandling ville tjene en god hensikt, men at det var viktig at saken ble håndtert på en måte som forhindret at antidemokratiske strømninger fikk spillerom." Dette er da en fremstående tyrkisk demokratisk sosialist, som altså er tilhenger av - som dere hører at man går til det skritt å bringe saken inn for menneskerettighetskommisjonen, men som likevel mener at dette i tilfelle bør skje innenfor den ramme som her er nevnt, noe det slett ikke vil være lett å få gjennomført. Jeg nevner dette, ikke for at det vil være helt avgjørende verken i den ene eller den annen retning, men bare for å vise hvor vanskelige avveiningsspørsmål man her står overfor.'
)

df <- insert_row_at(df, insert, pos)

# 1982_0127: replace index 16 + insert Stray ----------------------------------------

# check; should start "Før jeg gir ordet til"
df$remark[df$filename == "1982_0127.pdf" & df$index == 16]

replacement <- "Før jeg gir ordet til utenriksministeren vil jeg gjerne gi uttrykk for min støtte til den tanke som Nordli gav uttrykk for. Jeg deler det syn at prinsipielt ligger det alvorlige i den ting at en dødsdom avsies. Det stiller også spørsmålet: Har det vært drøftet noe hva man vil gjøre vis-à-vis en annen organisasjon, nemlig ILO? Det har jo skjedd overgrep ikke bare overfor fagforeningslederne i Tyrkia, men også overfor fagforeningsledere i andre land. Jeg minner om at Lech Walesa deltok i ILO-konferansen i fjor."

df$remark[df$filename == "1982_0127.pdf" & df$index == 16] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1982_0127.pdf",
  date = "1982-01-27",
  index = NA,
  speaker_verbatim = "UTENRIKSMINISTER SVENN STRAY",
  remark = 'Jeg er selvfølgelig helt enig i det som Nordli sier, at vi naturligvis prinsipielt tar avstand fra at det skulle vare noe straffbart å drive fagforeningsvirksomhet eller politisk aktivitet av annen art. Det er ganske klart. Jeg vil også gå et skritt videre og si som så at det vel formodentlig er temmelig klart ut fra alle de rapporter som vi har fått fra ulike hold, at det nåværende tyrkiske regimet i hvert fall foreløpig har hatt støtte av det store flertall av den tyrkiske befolkning. Til tross for dette må vi prinsipielt ta avstand fra styret der. Det er ikke i og for seg noen legitimasjon for et autoritært diktatur at det støttes av et flertall av befolkningen, så prinsipielt er det helt klart at vi må ta avstand fra styret der. Når jeg nevnte disse momentene omkring rettssaken, var det fordi det i hvert fall i diskusjonen i avisene har fremkommet det synspunkt at vi fra Norges side burde anvende et meget hurtig tempo i vår behandling av denne saken. Ellers risikerte vi at noen av de 52 tillitsmennene som står for domstolene, ble henrettet. Jeg mente det var riktig å si at det er det meget liten sannsynlighet for. For det første har, som jeg nevnte, Soysal uttalt at det er meget liten sannsynslighet for at denne rettssaken vil ende med noen dødsdommer. Skulle det mot formodning ende med det, er det enda mindre sannsynlig at de vil bli eksekvert. Det er en lang og omstendelig prosess som forestår om denne rettssaken for en eller fleres vedkommende skulle ende med dødsstraff. Vi følger rettssaken meget nøye. Når det gjelder tidsmomentet, nevnte jeg at rettssaken ikke er kommet så langt at de er ferdige med å lese opp tiltalebeslutningen. Det er det de holder på med, og de regner ikke med å bli ferdige med det før om én måneds tid. Det er ingen som regner med at det faller dom i saken før etter utløpet av det året vi nå er inne i. Dette nevner jeg for ytterligere å understreke at det av hensyn til livet for disse 52 ikke er noen grunn til at Regjeringen eller vi i det hele tatt i Norge skal fôle oss under noe tidspress. Det har vert anfort som et selvstendig moment, men er etter vår beste vurdering et moment som ikke er riktig.'
)

df <- insert_row_at(df, insert, pos)




# 1982_0127: replace index 20 + insert Formannen ----------------------------------------

# check; should start "Det er selvfølgelig litt vanskelig"
df$remark[df$filename == "1982_0127.pdf" & df$index == 20]

replacement <- "Det er selvfølgelig litt vanskelig å uttale seg i akkurat denne konkrete saken fordi det har lett for å bli et lokalpolitisk spørsmål midt oppe i utenrikspolitikken, men grunnen til at jeg bad om ordet, er at jeg kjenner Fosendalen Bergverk temmelig godt ettersom jeg har vært styreformann i selskapet i fem år, og jeg kan bekrefte det utenriksministeren sier, at østhandelen for Fosendalen Bergverk gjennom de siste 30 år har vært helt avgjørende for Bergverkets eksistens. Det er ikke noen ny handelsforbindelse. Det er Polen og Finland som har vært Bergverkets avtakere. Handelsforbindelsen har gjort at et bergverk som ellers er helt marginalt, og som ikke hadde kunnet eksistere, har kunnet eksistere. Jeg kan ikke se at sanksjoner som skulle ramme jernmalmleveranser til Polen fra Fosendalen, skulle ha noen som helst virkning for Polen. Saken er at Polen før gang på gang har blitt forsøkt presset vekk fra - det kan jeg si i denne forsamling - å kjøpe i Norge og til å kjøpe i Sovjet, men polakkene har greidd å holde seg til den norske leveranse. Det skyldes kvalitet, men de har vært utsatt for et meget sterkt press, et press som for øvrig Finland ikke greidde å motstå når det gjaldt leveransen dit. Uten at jeg nå uttaler meg på partiets vegne, for det synes jeg ikke det er rimelig at jeg gjør i en sak hvor jeg er så sterkt involvert, vil jeg si at jeg personlig gjerne tilrår at denne garantien gis."

df$remark[df$filename == "1982_0127.pdf" & df$index == 20] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1982_0127.pdf",
  date = "1982-01-27",
  index = NA,
  speaker_verbatim = "FORMANNEN",
  remark = 'Jeg har tegnet meg selv. Jeg har ikke noe å bemerke til den konklusjon som utenriksministeren trakk, og som nå ble backet opp av representanten Hansen, som har langt bedre innsikt i denne saken enn jeg har. Men jeg synes å orindre at utenriksministeren ved en tidligere anledning da kreditter til Polen var oppe, og det gjaldt en statlig garanti for en slik kreditt, gav uttrykk for at garantier i en slik sammenheng egentlig var det samme som direkte tilskudd, fordi det i den økonomiske situasjon Polen nå hadde, ikke var store muligheter for å få noe igjen av dette. Gjelder det også i dette tilfellet?'
)

df <- insert_row_at(df, insert, pos)

# 1980_1204: insert Formannen transition ----------------------------------------

pos <- which(df$filename == "1980_1204.pdf" & df$speaker_verbatim == "UTENRIKSMINISTER KNUT FRYDENLUND" & grepl("Når det gjelder denne sak", df$remark)) + 1
pos

insert <- data.frame(
  filename = "1980_1204.pdf",
  date = "1980-12-04",
  index = NA,
  speaker_verbatim = "FORMANNEN",
  remark = 'Da gir jeg ordet fritt.'
)

df <- insert_row_at(df, insert, pos)

# 1982_0204: replace index 8 + insert Formannen ----------------------------------------

# check; should start "Er det ikke såpass uklarhet"
df$remark[df$filename == "1982_0204.pdf" & df$index == 8]

replacement <- "Er det ikke såpass uklarhet på dette punkt at man bør ta en kritisk gjennomgåing av det, slik at man kan få det hele klart både når det gjelder på hvilken måte man skal gradere innkallingen, og hvordan man etterpå skal bli kvitt den?"

df$remark[df$filename == "1982_0204.pdf" & df$index == 8] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1982_0204.pdf",
  date = "1982-02-04",
  index = NA,
  speaker_verbatim = "FORMANNEN",
  remark = 'Når det gjelder selve innkallingen, tror jeg ikke det er tvil om hva forretningsordenen sier. Jeg kan godt finne det fram. Når det gjelder makulering eller innlevering av de graderte dokumenter, er det også helt greie regler, men jeg har dem ikke i hodet og kan ikke sitere dem nå. Hvis andre husker bedre enn jeg, er jeg takknemlig for hjelp. Etter min oppfatning er de reglene tilfredsstillende, men vi kan godt få en redegjørelse for det senere.'
)

df <- insert_row_at(df, insert, pos)

# 1982_0204: replace index 17 + insert Formannen ----------------------------------------

# check; should start "Det er vel egentlig ikke noen"
df$remark[df$filename == "1982_0204.pdf" & df$index == 17]

replacement <- "Det er vel egentlig ikke noen særlig uenighet om det siste som Gro Harlem Brundtland nå nevnte, nemlig at utgangspunktet må være å ta vare på og slå ring om det som er vunnet når det gjelder avspenningspolitikken i Europa. En kan jo diskutere hvilken strategi og hvilke virkemidler som fremmer det som kan betraktes som et felles mål. I den forbindelse nevnte utenriksministeren både START-forhandlingene og - selvsagt - KSSEprosessen, som kanskje er de deler av avspenningsprosessen som vil bli sterkest berort av en forverring i det internasjonale klima. Men det pågår også andre forhandlinger som svært mange av oss er opptatt av, nemlig de som for tiden er i gang i Genève mellom USA og Sovjet. Mitt spørsmål er da følgende: Hvordan vurderer utenriksministeren mulighetene for at Amerika under en eventuell forverring av klimaet, f.eks. en forverring av situasjonen i Polen, kan komme til å gå til det drastiske skritt å bryte Genève-forhandlingene? Kan det på den annen side tenkes at Sovjetunionen under det de vil føle som et sterkt press fra vestlig side, på et gitt punkt vil kunne gjøre det samme?"

df$remark[df$filename == "1982_0204.pdf" & df$index == 17] <- replacement

pos <- which(df$remark == replacement) + 1
pos

insert <- data.frame(
  filename = "1982_0204.pdf",
  date = "1982-02-04",
  index = NA,
  speaker_verbatim = "FORMANNEN",
  remark = 'Utenriksministerens hovedkonklusjon var at man ikke kunne la det som er hendt i Polen, gå uberørt hen når man åpner denne KSSE-konferansen igjen, og jeg for min del vil gjerne gi min tilslutning til det. I et eller annet internasjonalt forum må man gi en reaksjon på begivenhetene i Polen, og i forlengelsen av det Jakobsen nå var inne på, vil jeg si det slik at man bør få gjort det noenlunde klart - og det er vel kanskje allerede gjort - at en reaksjon i Madrid faktisk også innebærer og forutsetter at man ikke vil la en tilsvarende reaksjon komme i Genève, men at man tvert imot legger vekt på å få fram at Genève-forhandlingene fra vestlig side vil bli lagt opp til å gå som forutsatt. Slik som forholdene har utviklet seg, mener jeg det er nødvendig at man får en reaksjon i Madrid, og det er vel slik konferansen er lagt opp nå. Når det gjelder spørsmålet om hvor lenge forhandlingene kan fortsette, og hva man der kan oppnå, er det mye som taler for det som USA er inne på, nemlig at man etter den forste utladning med innlegg fra utenriksministerene bør ta en pause. Jeg tror, som Harlem Brundtland var inne på, at de saker som vi behandler, kan tilsi det, men på den annen side synes jeg det må vare riktig at det ikke er vi - og jeg mener da de vestlige land - som bryter forhandlingene, men at vi avventer reaksjon, og at man i så tilfelle får initiativet til en avbrytelse av møtet fra den annen side.'
)

df <- insert_row_at(df, insert, pos)

# 1982_0204: insert Stray confirmation ----------------------------------------

pos <- which(
  df$filename == "1982_0204.pdf" &
    df$speaker_verbatim == "FORMANNEN" &
    grepl("Før jeg gir ordet til", df$remark)
) + 1
pos

insert <- data.frame(
  filename = "1982_0204.pdf",
  date = "1982-12-04",
  index = NA,
  speaker_verbatim = "UTENRIKSMINISTER SVENN STRAY",
  remark = 'Det er riktig.'
)

df <- insert_row_at(df, insert, pos)

# Fix image-OCR edge cases ----------------------------------------

txts <- dir_ls("data/txt")
# length(txts)
txt_content <- map_chr(txts, read_file)
txts_w_img <- txts[grep("\\!\\[img\\-", txt_content)]
# length(txts_w_img) # 16

# Manual review shows instances 2-16 are all cases of 
# blacked out parts getting captured as images. These
# require no further action. 
# However, instance 1 is a case of an entire page og text
# getting parsed as an image. 
# A re-run of Mistral OCR on that page reproduces the error,
# so we process it with Google Document AI instead

# Get the page as PDF and store it
pdf <- str_replace_all(txts_w_img[1], "txt", "pdf")
# dir_create("temp/misc")
# path <- "temp/misc/extrapage.pdf"
# pdf_subset(pdf, pages = 2, output = path)

# OCR and save
# text <- get_text(dai_sync(path))
# write(text, "temp/misc/extrapage.txt")

# Parse text and build insert
text <- read_file("temp/misc/extrapage.txt")

speakers <- unlist(str_extract_all(text, "\\b[A-Z ]{8,}\\b"))

remark1 <- str_match(text, glue("(?s){speakers[1]}: (.*?){speakers[2]}"))[, 2] |>
  str_replace("-\n\\(SA\\)\n", "") |>
  trimws()

remark2 <- str_extract(text, glue("(?s)(?<={speakers[2]}: ).*")) |>
  str_squish() |>
  str_replace_all("- ", "") |>
  str_replace_all("ö", "ø") 

insert1 <- data.frame(
  filename = basename(pdf),
  date = "1976-05-13",
  index = NA,
  speaker_verbatim = speakers[1],
  remark = remark1
)

insert2 <- data.frame(
  filename = basename(pdf),
  date = "1976-05-13",
  index = NA,
  speaker_verbatim = speakers[2],
  remark = remark2
)

insert <- rbind(insert1, insert2)

# Insert in df
pos <- which(
  df$filename == basename(pdf) &
    df$speaker_verbatim == "FORMANNEN" &
    grepl("Jeg takker utenriksministeren", df$remark)
)

df <- insert_row_at(df, insert, pos)

# Fix indices
df <- df |>
  group_by(filename) |>
  mutate(index = row_number()) |>
  ungroup()

# Write
write_csv(df, "out/remarks.csv")
