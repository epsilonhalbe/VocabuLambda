module FreqTable where

import VocabularyData
import Control.Arrow ((&&&))
import qualified Data.Map as M

freqMap :: VocMap
freqMap = M.fromList $ map (_frq &&& id) freqTable

freqTable :: [Word]
freqTable =
  [ Word { _frq=1
         , _fra="le"
         , _eng="the; him, her, it, them"
         , _deu="der,die,das; er,sie,es"
         , _uses=[Determiner [], Pronoun []]
         , _phrase="Vive la politique, vive l'amour."
         , _sentence="Long live politics, long live love."
         , _satz="Lang lebe die Politik, lang lebe die Liebe."
         }
  , Word { _frq=2
         , _fra="de"
         , _eng="of, from, some, any"
         , _deu="von, aus, etwas von"
         , _uses=[Determiner [], Preposition []]
         , _phrase="Il ne rêve que d'argent et de plaisirs."
         , _sentence="He only dreams of money and pleasure."
         , _satz="Er träumt nur von Geld und Vergnügen."
         }
  , Word { _frq=3
         , _fra="un"
         , _eng="a, an, one"
         , _deu="ein/eine"
         , _uses=[Adjective [], Determiner [], Noun [Masculine], Pronoun []]
         , _phrase="Je me suis cassé un ongle."
         , _sentence="I broke one of my fingernails."
         , _satz="Ich habe mir einen Fingernagel abgebrochen."
         }
  , Word { _frq=4
         , _fra="à"
         , _eng="to, at, in"
         , _deu="zu, an, in"
         , _uses=[Preposition []]
         , _phrase="Ils restent à l'école le plus longtemps possible."
         , _sentence="They remain at school as long as possible."
         , _satz="Sie bleiben so lang wie möglich in der Schule."
         }
  , Word { _frq=5
         , _fra="être"
         , _eng="to be; being"
         , _deu="sein"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Tout le monde veut être beau."
         , _sentence="Everybody wants to be beautiful."
         , _satz="Jedermann will schön sein."
         }
  , Word { _frq=6
         , _fra="et"
         , _eng="and"
         , _deu="und"
         , _uses=[Conjunction []]
         , _phrase="Et les larmes se remirent à couler."
         , _sentence="And the tears started flowing again."
         , _satz="Und die Tränen begannen zu fließen."
         }
  , Word { _frq=7
         , _fra="en"
         , _eng="in, by"
         , _deu="in, bei"
         , _uses=[Adverb [],Preposition [],Pronoun []]
         , _phrase="Je suis retournée en Espagne en septembre."
         , _sentence="I returned to Spain in September."
         , _satz="Ich kehrte nach Spanien zurück im September"
         }
  , Word { _frq=8
         , _fra="avoir"
         , _eng="to have"
         , _deu="haben"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="On était six donc tu peux pas avoir une conversation."
         , _sentence="There were six of us so you can't have a conversation."
         , _satz="Es waren sechs von uns, daher konntest du keine Unterhaltung führen."
         }
  , Word { _frq=9
         , _fra="que"
         , _eng="that, which, who, whom"
         , _deu="dass, der/die, welcher/welche"
         , _uses=[Adverb [],Conjunction [],Pronoun []]
         , _phrase="C'est un soldat. Mais que fait-il ici?"
         , _sentence="It's a soldier. but what's he doing here?"
         , _satz="Das ist ein Soldat. Aber was macht er hier"
         }
  , Word { _frq=10
         , _fra="pour"
         , _eng="for, in order to"
         , _deu="für, um zu, damit, fürs, mit"
         , _uses=[Preposition []]
         , _phrase="Elle jouait pour gagner."
         , _sentence="She played to win."
         , _satz="Sie spielt fürs Gewinnen."
         }
  , Word { _frq=11
         , _fra="dans"
         , _eng="in, into, from"
         , _deu="in, hinein, von"
         , _uses=[Preposition []]
         , _phrase="Je reviendrai dans dix minutes."
         , _sentence="I will return in 10 minutes."
         , _satz="Ich kehre in zehn Minuten zurück."
         }
  , Word { _frq=12
         , _fra="ce"
         , _eng="this, that"
         , _deu="dieser/diese/dieses, jener/jene/jenes"
         , _uses=[Determiner [],Pronoun []]
         , _phrase="Je ne déteste pas cet homme."
         , _sentence="I do not detest this man."
         , _satz="Ich verabscheue diesen Mann nicht."
         }
  , Word { _frq=13
         , _fra="il"
         , _eng="he, it"
         , _deu="er/es"
         , _uses=[Pronoun []]
         , _phrase="Allez voir s'il est blessé."
         , _sentence="Go see if he is injured."
         , _satz="Geh, schau ob er verletzt ist."
         }
  , Word { _frq=14
         , _fra="qui"
         , _eng="who, whom"
         , _deu="wem/wen"
         , _uses=[Pronoun []]
         , _phrase="Je ne sais pas à qui m'adresser."
         , _sentence="I don't know who to talk to."
         , _satz="Ich weiß nicht an wen ich mich wenden soll."
         }
  , Word { _frq=15
         , _fra="ne"
         , _eng="not"
         , _deu="nicht"
         , _uses=[Adverb []]
         , _phrase="Nous ne faisons pas du très bon travail."
         , _sentence="We are not doing very good work."
         , _satz="Wir machen keine sehr gute Arbeit."
         }
  , Word { _frq=16
         , _fra="sur"
         , _eng="on, upon"
         , _deu="auf,über,unter"
         , _uses=[Adjective [],Preposition []]
         , _phrase="T'avais une chance sur un million."
         , _sentence="You had one chance in a million."
         , _satz="Du hattest eine Gelegenheit unter einer Million."
         }
  , Word { _frq=17
         , _fra="se"
         , _eng="oneself, himself, herself, itself, themselves"
         , _deu="sich, selbst"
         , _uses=[Pronoun []]
         , _phrase="Avec ce traité, le Japon se rapproche des Etats-Unis."
         , _sentence="With this treaty, Japan brings itself closer to the U.S."
         , _satz="Mit diesem Waffenstillstand, näherten sich die Japaner den USA (wieder) an."
         }
  , Word { _frq=18
         , _fra="pas"
         , _eng="not, n't; footstep"
         , _deu="nicht - Verneinepartikel"
         , _uses=[Adverb [],Noun [Masculine, NoDistinctPlural]]
         , _phrase="Non, ne touchez pas!"
         , _sentence="No, don't touch it!"
         , _satz="Nein, Ihr berührt das nicht."
         }
  , Word { _frq=19
         , _fra="plus"
         , _eng="more, no more"
         , _deu="mehr, nicht mehr, Komperativpartikel"
         , _uses=[Adverb []]
         , _phrase="Il est considérablement plus jeune que moi."
         , _sentence="He's considerably younger than I am."
         , _satz="Er ist bedeutend jünger als ich"
         }
  , Word { _frq=20
         , _fra="pouvoir"
         , _eng="can, to be able to"
         , _deu="können"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Tu peux jouer de la guitare électrique."
         , _sentence="You can play electric guitar."
         , _satz="Du kannst E-Gitarre spielen."
         }
  , Word { _frq=21
         , _fra="par"
         , _eng="by"
         , _deu="durch, per"
         , _uses=[Preposition []]
         , _phrase="Il s'y trouvait par hasard."
         , _sentence="He found himself there by accident."
         , _satz="Er fand sich dort per Zufall."
         }
  , Word { _frq=22
         , _fra="je"
         , _eng="I"
         , _deu="ich"
         , _uses=[Pronoun []]
         , _phrase="Je suis contente de vous revoir."
         , _sentence="I am happy to see you again."
         , _satz="Ich bin froh euch wiederzusehen."
         }
  , Word { _frq=23
         , _fra="avec"
         , _eng="with"
         , _deu="mit"
         , _uses=[Preposition []]
         , _phrase="Vous voulez aller au ciné avec moi?"
         , _sentence="Do you want to go to a movie with me?"
         , _satz="Wollt ihr mit mir ins Kino gehen."
         }
  , Word { _frq=24
         , _fra="tout"
         , _eng="all, very"
         , _deu="ganz, alle, alles, sehr"
         , _uses=[Adverb [],Determiner [],NounOrAdjective [],Pronoun []]
         , _phrase="Comme vous voyez, tout est propre."
         , _sentence="As you see, everything is clean."
         , _satz="Wie sie sehen, alles ist sauber."
         }
  , Word { _frq=25
         , _fra="faire"
         , _eng="to do, make"
         , _deu="machen, tun"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Qu'est-ce qu'il fait?"
         , _sentence="What's he doing?"
         , _satz="Was machst du? (eigentlich: Was ist es das du machst?)"
         }
  , Word { _frq=26
         , _fra="son"
         , _eng="his, her, its; sound; bran"
         , _deu="sein/ihr; Ton; Kleie(z.B. Weizenkleie)"
         , _uses=[Determiner [],Noun [Masculine]]
         , _phrase="Un ami ingénieur du son m'aide pour les arrangements."
         , _sentence="A sound engineer friend of mine helped me with the arrangements."
         , _satz="Ein befreundeter Toningenieur half mir mit den Arrangements"
         }
  , Word { _frq=27
         , _fra="mettre"
         , _eng="to put, place"
         , _deu="setzen, stellen, legen"
         , _uses=[Verb []]
         , _phrase="Je peux me mettre à votre table?"
         , _sentence="May I sit at your table?"
         , _satz="Kann ich mich an euren Tisch setzen?"
         }
  , Word { _frq=28
         , _fra="autre"
         , _eng="other"
         , _deu="anderer, andere, anderes, weiteres"
         , _uses=[Determiner [],NounOrAdjective [NoDistinctFeminine],Pronoun []]
         , _phrase="Il y a un autre problème."
         , _sentence="There's another problem."
         , _satz="Es gibt ein weiteres Problem."
         }
  , Word { _frq=29
         , _fra="on"
         , _eng="one, we"
         , _deu="wir, man"
         , _uses=[Pronoun []]
         , _phrase="On tire et on pose les questions ensuite."
         , _sentence="We shoot first and ask questions later."
         , _satz="Wir schießen und (erst) dann stellen wir Fragen."
         }
  , Word { _frq=30
         , _fra="mais"
         , _eng="but"
         , _deu="aber"
         , _uses=[Adverb [],Conjunction [],Interjection []]
         , _phrase="Je ne suis pas riche, mais je connais la vérité."
         , _sentence="I'm not rich, but I know the truth."
         , _satz="Ich bin nicht reich, aber ich kenne die Wahrheit."
         }
  , Word { _frq=31
         , _fra="nous"
         , _eng="we, us"
         , _deu="wir, uns"
         , _uses=[Pronoun []]
         , _phrase="Nous devons nous défendre nous-mêmes."
         , _sentence="We must defend ourselves."
         , _satz="Wir müssen/sollen uns (selber) verteidigen."
         }
  , Word { _frq=32
         , _fra="comme"
         , _eng="like, as"
         , _deu="wie"
         , _uses=[Adverb [],Conjunction []]
         , _phrase="Tony et moi, on est comme des frères."
         , _sentence="Tony and I, we're like brothers."
         , _satz="Tony und ich, wir sind wie Brüder."
         }
  , Word { _frq=33
         , _fra="ou"
         , _eng="or"
         , _deu="oder"
         , _uses=[Conjunction []]
         , _phrase="Il en reste du café ou pas?"
         , _sentence="Is there some coffee left or not?"
         , _satz="TODO"
         }
  , Word { _frq=34
         , _fra="si"
         , _eng="if, whether"
         , _deu="wenn, ob, so"
         , _uses=[Adverb [],Conjunction [],Noun [Masculine, Invariable]]
         , _phrase="Aujourd'hui, notre économie va si mal."
         , _sentence="Today our economy is going so poorly."
         , _satz="Heutezutage geht unsere Wirtschaft so schlecht."
         }
  , Word { _frq=35
         , _fra="leur"
         , _eng="them, their, theirs"
         , _deu="ihr/ihres"
         , _uses=[Determiner [],Adjective [NoDistinctFeminine],Pronoun []]
         , _phrase="L'énergie solaire assurait leur survie."
         , _sentence="Solar energy assured their survival."
         , _satz="Die Solarenergie sicherte ihr überleben."
         }
  , Word { _frq=36
         , _fra="y"
         , _eng="there"
         , _deu="dort, dorthin"
         , _uses=[Adverb [],Noun [Masculine, Invariable],Pronoun []]
         , _phrase="C'est certain qu'on va y aller."
         , _sentence="It's for certain that we'll be going there."
         , _satz="Es ist sicher dass wir dorthin gehen werden."
         }
  , Word { _frq=37
         , _fra="dire"
         , _eng="to say"
         , _deu="sagen"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Je décrochais le téléphone sans rien dire à personne."
         , _sentence="I picked up the telephone receiver without saying anything to anyone."
         , _satz="Ich hob den Telefonhörer ab ohne etwas mit jemand zu sprechen."
         }
  , Word { _frq=38
         , _fra="elle"
         , _eng="she, her"
         , _deu="sie, ihr"
         , _uses=[Pronoun []]
         , _phrase="J'étais fou amoureux. Elle m'aimait bien."
         , _sentence="I was head-over-heels in love. She loved me a lot."
         , _satz="Ich war wahnsinnig verliebt. Sie liebte mich sehr."
         }
  , Word { _frq=39
         , _fra="devoir"
         , _eng="to have to, owe; duty"
         , _deu="müssen, sollen, schulden; Aufgabe"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Je dois travailler sans la moindre entrave."
         , _sentence="I must work without the least bit of hindrance ."
         , _satz="Ich muss arbeiten ohne die geringste Beeinträchtigung."
         }
  , Word { _frq=40
         , _fra="avant"
         , _eng="before"
         , _deu="vorwärts, vor, voran"
         , _uses=[Adjective [Invariable],Adverb [],Noun [Masculine],Preposition []]
         , _phrase="Tu vas te pencher en avant."
         , _sentence="You're going to lean forward."
         , _satz="Du wirst dich nach vorne bücken."
         }
  , Word { _frq=41
         , _fra="deux"
         , _eng="two"
         , _deu="zwei"
         , _uses=[Determiner [],Noun [Masculine, Invariable]]
         , _phrase="A il prend le train deux fois par semaine pour affaires."
         , _sentence="He takes the train on business twice per week."
         , _satz="Er nimmt den Zug für Geschäftsangelegenheiten zwei Mal pro Woche."
         }
  , Word { _frq=42
         , _fra="même"
         , _eng="same, even, self"
         , _deu="gar,ja, selbst, gleiche, selbe"
         , _uses=[Adjective [NoDistinctFeminine],Adverb [],Pronoun []]
         , _phrase="Ils ne s'excusent même pas."
         , _sentence="They don't even excuse themselves."
         , _satz="Sie entschuldigen sich nicht (einmal)."
         }
  , Word { _frq=43
         , _fra="prendre"
         , _eng="to take"
         , _deu="nehmen"
         , _uses=[Verb []]
         , _phrase="Elle lui prit la main."
         , _sentence="She took him by the hand."
         , _satz="Sie nimmt ihn bei der Hand."
         }
  , Word { _frq=44
         , _fra="aussi"
         , _eng="too, also, as"
         , _deu="auch, ebenfalls, daher, darum"
         , _uses=[Adverb [],Conjunction []]
         , _phrase="Je rêvais aussi de beaucoup voyager."
         , _sentence="I also dreamed of traveling a lot."
         , _satz="Ich träumte auch von häufigem reisen."
         }
  , Word { _frq=45
         , _fra="celui"
         , _eng="that, the one, he, him"
         , _deu="dieser, jener, er, ihm, ihn"
         , _uses=[Pronoun []]
         , _phrase="Tu es celui que je respecte le plus."
         , _sentence="You're the one I respect the most."
         , _satz="Du bist der, den ich am meisten achte."
         }
  , Word { _frq=46
         , _fra="donner"
         , _eng="to give"
         , _deu="geben"
         , _uses=[Verb []]
         , _phrase="J'aurais donné ma vie pour lui."
         , _sentence="I would have given my life for him."
         , _satz="Ich hätte mein Leben für ihn gegeben."
         }
  , Word { _frq=47
         , _fra="bien"
         , _eng="well"
         , _deu="gut/sehr"
         , _uses=[Adverb [],Noun [Masculine], Adjective [Invariable]]
         , _phrase="Tout va bien maintenant."
         , _sentence="Everything's going well now."
         , _satz="Alles geht gerade gut."
         }
  , Word { _frq=48
         , _fra="où"
         , _eng="where"
         , _deu="wo"
         , _uses=[Adverb [],Pronoun []]
         , _phrase="On ne dit pas où il vit."
         , _sentence="They aren't saying where he lives."
         , _satz="Sie sagen nicht wo er lebt."
         }
  , Word { _frq=49
         , _fra="fois"
         , _eng="time, times"
         , _deu="Zeit, Zeiten, Mal"
         , _uses=[Noun [Feminine, NoDistinctPlural]]
         , _phrase="Lève la main une fois."
         , _sentence="Raise your hand one time."
         , _satz="Hebe deine Hand ein Mal"
         }
  , Word { _frq=50
         , _fra="vous"
         , _eng="you"
         , _deu="ihr, Ihr"
         , _uses=[Pronoun []]
         , _phrase="Ici, vous avez une personnalité publique."
         , _sentence="Here, you are a public personality."
         , _satz="Hier, Ihr habt eine öffentliche Persönlichkeit."
         }
  , Word { _frq=51
         , _fra="encore"
         , _eng="again, yet"
         , _deu="nochmal, schon wieder"
         , _uses=[Adverb []]
         , _phrase="Tu as encore menti à ta femme."
         , _sentence="You lied once again to your wife."
         , _satz="Du hast schon wieder deine Frau belogen."
         }
  , Word { _frq=52
         , _fra="nouveau"
         , _eng="new"
         , _deu="neu"
         , _uses=[Adjective [],Noun [Masculine]]
         , _phrase="Il a construit une nouvelle vie ici."
         , _sentence="He made a new life for himself here."
         , _satz="Er hat (sich) hier ein neues Leben aufgebaut."
         }
  , Word { _frq=53
         , _fra="aller"
         , _eng="to go"
         , _deu="gehen"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Tu devrais aller te coucher, tu as l'air vanné."
         , _sentence="You should go to bed, you look wiped out."
         , _satz="Du solltest dich hinlegen, du hast ein hundemüdes Aussehen."
         }
  , Word { _frq=54
         , _fra="cela"
         , _eng="that, it"
         , _deu="es, dieser, diese, dieses"
         , _uses=[Pronoun []]
         , _phrase="Cela demande de l'intégrité et du courage."
         , _sentence="That requires integrity and courage."
         , _satz="Es verlangt Anstand und Mut."
         }
  , Word { _frq=55
         , _fra="entre"
         , _eng="between"
         , _deu="zwischen"
         , _uses=[Preposition []]
         , _phrase="Je marche entre les maisons."
         , _sentence="I'm walking between the houses."
         , _satz="Ich marschiere zwischen den Häusern."
         }
  , Word { _frq=56
         , _fra="premier"
         , _eng="first"
         , _deu="erster, erste, erstes"
         , _uses=[Determiner [],NounOrAdjective []]
         , _phrase="Est-elle la première épouse, la deuxième?"
         , _sentence="Is she the first wife, the second one?"
         , _satz="Ist sie die erste Gemahlin, die Zweite?"
         }
  , Word { _frq=57
         , _fra="vouloir"
         , _eng="to want"
         , _deu="wollen"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Tu veux faire ton chemin. C'est bien."
         , _sentence="You want to achieve something. That's OK."
         , _satz="Du willst es zu etwas bringen. Das ist OK."
         }
  , Word { _frq=58
         , _fra="déjà"
         , _eng="already"
         , _deu="schon"
         , _uses=[Adverb []]
         , _phrase="Les rues étaient déjà pleines de monde."
         , _sentence="The streets were already full of people."
         , _satz="Die Straßen waren schon voller Leute."
         }
  , Word { _frq=59
         , _fra="grand"
         , _eng="great, big, tall"
         , _deu="großartig, groß"
         , _uses=[Adverb [],NounOrAdjective []]
         , _phrase="Tu es plus grand que je pensais."
         , _sentence="You're taller than I thought."
         , _satz="Du bist größer als ich dachte."
         }
  , Word { _frq=60
         , _fra="mon"
         , _eng="my"
         , _deu="mein"
         , _uses=[Determiner []]
         , _phrase="T'aurais pu rencontrer mon copain."
         , _sentence="You could've met my buddy."
         , _satz="Du könntest mir begegnet sein mein Freund"
         }
  , Word { _frq=61
         , _fra="me"
         , _eng="me, to me, myself"
         , _deu="mich, mein"
         , _uses=[Pronoun []]
         , _phrase="Reviens me voir dans cinq ou six ans."
         , _sentence="Come back to see me in five or six years."
         , _satz="Komm zurück mich zu sehen in fünf oder sechs Stunden."
         }
  , Word { _frq=62
         , _fra="moins"
         , _eng="less"
         , _deu="weniger"
         , _uses=[Adjective [Invariable, NoDistinctPlural],Adverb [],Noun [Masculine, NoDistinctPlural],Preposition []]
         , _phrase="Il avait moins d'excuses encore que ses complices."
         , _sentence="He had even less excuses than his accomplices did."
         , _satz="Er hatte sogar noch weniger Ausreden als seine Kameraden."
         }
  , Word { _frq=63
         , _fra="aucun"
         , _eng="none, either, neither, not any"
         , _deu="nicht, nichts, kein, weder … noch, entweder … oder"
         , _uses=[Determiner [],Adjective [],Pronoun []]
         , _phrase="Trop d'argent et aucun goût."
         , _sentence="Too much money and no taste."
         , _satz="Zu viel Geld und keinen Geschmack."
         }
  , Word { _frq=64
         , _fra="lui"
         , _eng="him, her"
         , _deu="er, ihr"
         , _uses=[Pronoun []]
         , _phrase="Mais j'ai confiance qu'en lui."
         , _sentence="But I have confidence in him."
         , _satz="Aber ich hatte zuversicht in ihn."
         }
  , Word { _frq=65
         , _fra="temps"
         , _eng="time"
         , _deu="Zeit"
         , _uses=[Noun [Masculine, NoDistinctPlural]]
         , _phrase="Sur le plan spirituel, le temps n'existe pas."
         , _sentence="On the spiritual level, time does not exist."
         , _satz="In der spirituellen Ebene, existiert die Zeit nicht."
         }
  , Word { _frq=66
         , _fra="très"
         , _eng="very"
         , _deu="sehr"
         , _uses=[Adverb []]
         , _phrase="J'ai été bonne? Très, très bonne."
         , _sentence="Was I good? Very, very good."
         , _satz="War ich gut? Sehr, sehr gut."
         }
  , Word { _frq=67
         , _fra="savoir"
         , _eng="to know"
         , _deu="wissen, können"
         , _uses=[Noun [Masculine],Verb []]
         , _phrase="Je ne savais plus quoi dire."
         , _sentence="I didn't know what to say any more."
         , _satz="Ich wusste nicht mehr was ich sagen soll."
         }
  , Word { _frq=68
         , _fra="falloir"
         , _eng="to take, require, need"
         , _deu="nehmen, brauchen"
         , _uses=[Verb []]
         , _phrase="Il ne faut pas être raciste, point."
         , _sentence="There's no need to be a racist, at all."
         , _satz="Man muss nicht Rassist sein, Punktum."
         }
  , Word { _frq=69
         , _fra="voir"
         , _eng="to see"
         , _deu="sehen"
         , _uses=[Verb []]
         , _phrase="Je tenais seulement à te voir pour te dire … bonne chance."
         , _sentence="I just wanted to see you to say … good luck."
         , _satz="Ich wollte dich sehen um dir viel Glück zu wünschen."
         }
  , Word { _frq=70
         , _fra="quelque"
         , _eng="some"
         , _deu="etwas, wenige, ein paar"
         , _uses=[Adverb [],Adjective [],Determiner []]
         , _phrase="Il restera pour quelques mois."
         , _sentence="He will stay for a few months."
         , _satz="Er wird für ein paar Monate bleiben."
         }
  , Word { _frq=71
         , _fra="sans"
         , _eng="without"
         , _deu="ohne"
         , _uses=[Preposition []]
         , _phrase="Je fais plus jeune sans maquillage."
         , _sentence="I look younger without makeup."
         , _satz="Ich sehe jünger aus ohne Makeup."
         }
  , Word { _frq=72
         , _fra="raison"
         , _eng="reason"
         , _deu="Vernunft"
         , _uses=[Noun [Feminine]]
         , _phrase="Elle a raison, tu sais."
         , _sentence="She's right, you know."
         , _satz="Sie ist vernünftig, weißt du."
         }
  , Word { _frq=73
         , _fra="notre"
         , _eng="our"
         , _deu="unser"
         , _uses=[Determiner []]
         , _phrase="Notre ville a le sens de la communauté."
         , _sentence="Our city has a sense of community."
         , _satz="Unser Dorf hat einen Sinn für Gemeinschaft."
         }
  , Word { _frq=74
         , _fra="dont"
         , _eng="whose, of which"
         , _deu="wessen, von wem"
         , _uses=[Pronoun []]
         , _phrase="On a tout ce dont tu rêvais en ville."
         , _sentence="Everything you dreamed about is available in town."
         , _satz="Man hat alles das von dem du träumst in der Stadt.\n"++
                 "Alles von dem du träumst ist in der Stadt verfügbar."
         }
  , Word { _frq=75
         , _fra="non"
         , _eng="no, not"
         , _deu="nein, nicht"
         , _uses=[Adverb [],Noun [Masculine, Invariable]]
         , _phrase="Non, je ne peux pas parler maintenant."
         , _sentence="No, I can't talk now."
         , _satz="Nein, ich kann jetzt nicht sprechen."
         }
  , Word { _frq=76
         , _fra="an"
         , _eng="year"
         , _deu="Jahr"
         , _uses=[Noun [Masculine]]
         , _phrase="J'avais un an à la mort de mon père."
         , _sentence="I was one year old when my father died."
         , _satz="Ich hatte ein Jahr beim Tod meines Vaters.\n"++
                 "Ich war ein Jahr alt beim Tod meines Vaters."
         }
  , Word { _frq=77
         , _fra="monde"
         , _eng="world, people"
         , _deu="Welt, Leute"
         , _uses=[Noun [Masculine]]
         , _phrase="Notre ultime but est de reconstruire le monde."
         , _sentence="Our ultimate goal is to remake the world."
         , _satz="Unser ultimatives Ziel ist die Welt wieder aufzubauen."
         }
  , Word { _frq=78
         , _fra="jour"
         , _eng="day"
         , _deu="Tag"
         , _uses=[Noun [Masculine]]
         , _phrase="Un jour, je retrouverais mes vrais parents."
         , _sentence="One day, I would find my real parents."
         , _satz="Eines Tages, werde ich meine echten Eltern finden. "
         }
  , Word { _frq=79
         , _fra="monsieur"
         , _eng="mister, sir, gentleman"
         , _deu="Herr/Mein Herr"
         , _uses=[Noun [Masculine]]
         , _phrase="Comment allez-vous, monsieur?"
         , _sentence="How are you, sir?"
         , _satz="Wie geht es Ihnen, mein Herr?"
         }
  , Word { _frq=80
         , _fra="demander"
         , _eng="to ask for"
         , _deu="verlangen, bitten"
         , _uses=[Verb []]
         , _phrase="J'ai un service à vous demander."
         , _sentence="I would like to ask you to do something for me."
         , _satz="Ich habe einen Dienst von euch zu verlangen.\n"++
                 "Ich würde euch bitten etwas für mich zu tun."
         }
  , Word { _frq=81
         , _fra="alors"
         , _eng="then, so"
         , _deu="weiter, weiters, dann"
         , _uses=[Adverb []]
         , _phrase="Et alors? Vous revenez quand?"
         , _sentence="So? When are you coming back?"
         , _satz="Und? Wann kommt ihr zurück?"
         }
  , Word { _frq=82
         , _fra="après"
         , _eng="after"
         , _deu="nach"
         , _uses=[Adverb [],Preposition []]
         , _phrase="Il a succombé à ses blessures peu après."
         , _sentence="He shortly thereafter succumbed to his wounds."
         , _satz="Kurz darauf er erlag seinen Wunden."
         }
  , Word { _frq=83
         , _fra="trouver"
         , _eng="to find"
         , _deu="finden"
         , _uses=[Verb []]
         , _phrase="J'ai trouvé du sang. Sur le siège avant."
         , _sentence="I found some blood. On the front seat."
         , _satz="Ich fand etwas Blut. Auf dem Vordersitz."
         }
  , Word { _frq=84
         , _fra="personne"
         , _eng="person, people, anybody, anyone, nobody"
         , _deu="Person"
         , _uses=[Noun [Feminine],Pronoun []]
         , _phrase="Il ne parlait à personne en particulier."
         , _sentence="He was speaking to nobody in particular."
         , _satz="Er sprach nicht mit einer bestimmten Person."
         }
  , Word { _frq=85
         , _fra="rendre"
         , _eng="to render, return, yield, give up"
         , _deu="(se rendre à)sich begeben nach, (rendre qc. à qn.) jemandem etwas übergeben "
         , _uses=[Verb []]
         , _phrase="Je m'y rendis à 4 heures du matin."
         , _sentence="I went there at 4 o'clock in the morning."
         , _satz="Ich begab mich um 4 Uhr morgens dorthin."
         }
  , Word { _frq=86
         , _fra="part"
         , _eng="share"
         , _deu="Teil, Anteil"
         , _uses=[Noun [Feminine]]
         , _phrase="Je voudrais vous faire part de quelques données."
         , _sentence="I want to share some data with you."
         , _satz="Ich würde Daten mit euch teilen."
         }
  , Word { _frq=87
         , _fra="dernier"
         , _eng="last"
         , _deu=""
         , _uses=[NounOrAdjective []]
         , _phrase="C'est le dernier endroit où vous auriez dû vous rencontrer."
         , _sentence="That's the last place you should have met."
         , _satz=""
         }
  , Word { _frq=88
         , _fra="venir"
         , _eng="to come"
         , _deu=""
         , _uses=[Verb []]
         , _phrase="Venez rencontrer les autres."
         , _sentence="Come meet the others."
         , _satz=""
         }
  , Word { _frq=89
         , _fra="pendant"
         , _eng="during; pendant"
         , _deu=""
         , _uses=[Adjective [],Preposition [],Noun [Masculine]]
         , _phrase="Où t'étais pendant tout ce temps?"
         , _sentence="Where were you all of this time?"
         , _satz=""
         }
  , Word { _frq=90
         , _fra="passer"
         , _eng="to pass"
         , _deu=""
         , _uses=[Verb []]
         , _phrase="Nous sommes passées devant la maison de ma grand-mère."
         , _sentence="We went past the front of my grandmother's place."
         , _satz=""
         }
  , Word { _frq=91
         , _fra="peu"
         , _eng="little"
         , _deu=""
         , _uses=[Adverb []]
         , _phrase="Un secret se dévoile peu à peu."
         , _sentence="A secret unfolds little by little."
         , _satz=""
         }
  , Word { _frq=92
         , _fra="lequel"
         , _eng="who, whom, which"
         , _deu=""
         , _uses=[Pronoun []]
         , _phrase="Il y a bien des points sur lesquels je voudrais faire des commentaires."
         , _sentence="There are many points on which I would like to comment."
         , _satz=""
         }
  , Word { _frq=93
         , _fra="suite"
         , _eng="result, follow-up, rest"
         , _deu=""
         , _uses=[Noun [Feminine]]
         , _phrase="Deux, trois et ainsi de suite."
         , _sentence="Two, three, and so on."
         , _satz=""
         }
  , Word { _frq=94
         , _fra="bon"
         , _eng="good"
         , _deu=""
         , _uses=[Adjective [],Adverb [],Interjection [],Noun [Masculine]]
         , _phrase="Ce n'est pas le bon moment."
         , _sentence="It's not a good time."
         , _satz=""
         }
  , Word { _frq=95
         , _fra="comprendre"
         , _eng="to understand"
         , _deu=""
         , _uses=[Verb []]
         , _phrase="Je comprends que vous soyez fâché."
         , _sentence="I understand that you're upset."
         , _satz=""
         }
  , Word { _frq=96
         , _fra="depuis"
         , _eng="since, for"
         , _deu=""
         , _uses=[Adverb [],Preposition []]
         , _phrase="Je le connais depuis le lycée."
         , _sentence="I have known him since high school."
         , _satz=""
         }
  , Word { _frq=97
         , _fra="point"
         , _eng="point; at all"
         , _deu=""
         , _uses=[Adverb [],Noun [Masculine]]
         , _phrase="Je suis d'accord avec ce point de vue."
         , _sentence="I'm in agreement with that point of view."
         , _satz=""
         }
  , Word { _frq=98
         , _fra="ainsi"
         , _eng="thus"
         , _deu=""
         , _uses=[Adverb []]
         , _phrase="Qu'est-ce qui le pousse à agir ainsi?"
         , _sentence="What drives him to act that way?"
         , _satz=""
         }
  , Word { _frq=99
         , _fra="heure"
         , _eng="hour"
         , _deu=""
         , _uses=[Noun [Feminine]]
         , _phrase="A quelle heure sont-ils partis?"
         , _sentence="At what time did they leave?"
         , _satz=""
         }
  , Word { _frq=100
         , _fra="rester"
         , _eng="to stay"
         , _deu=""
         , _uses=[Verb []]
         , _phrase="Le temps qu'il te reste, trouve une solution."
         , _sentence="In the remaining time, find a solution."
         , _satz=""
         }]
