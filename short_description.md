[home](/README.md)

---

# Kurzbeschreibung


Unsere Idee ist es, eine Füllstandüberwachung für die Industrie zu prototypen,
die automatisch eine Benachrichtigung versendet.

Die Füllstandsanzeige kann zum Beispiel von einem Unternehmen genutzt werden,
welches Wasserspender für andere Unternehmen aufstellt. Damit nicht immer ein
Mitarbeiter des Wasserspenderunternehmens nachgucken muss, ob noch genügend
Wasser im Wasserspender vorhanden ist, wird dies von einer Mechanik übernommen.
Dadurch müssen nur die leeren Wasserspender angefahren werden, was Zeit und
Kosten spart. Zudem kann so schneller auf das Leerwerden eines Wasserspenders
reagiert werden.

Bei dem Sensor für die Füllstandsmessung haben wir uns für eine Wägezelle
entschieden, da diese einfach kalibriert und ausgelesen werden kann. Die
Wägezelle misst das Gewicht des Wasserbehälters, und kann so den genauen
Füllstand ermitteln. Der Sensor ist über einen Analog-Digital-Wandler mit einem
Raspberry Pi verbunden. Dieser überwacht den gemessenen Füllstand, und kann über
ein internetfähiges Gerät konfiguriert, und ausgelesen werden. Dieses kann auch
direkt bei Erreichen eines niedriegen Füllstandes benachrichtigt werden. So kann
der Mitarbeiter des Wasserspenderunternehmens den Spender sofort wieder
auffüllen.
